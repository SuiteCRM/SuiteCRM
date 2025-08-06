const WebSocket = require('ws');
const { exec } = require('child_process');
const fs = require('fs').promises;
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Logger configuration
const logger = winston.createLogger({
    level: 'info',
    format: winston.format.json(),
    transports: [
        new winston.transports.File({ filename: 'websocket.log' }),
        new winston.transports.Console({
            format: winston.format.simple()
        })
    ]
});

// WebSocket server configuration
const wss = new WebSocket.Server({ 
    port: 8080,
    perMessageDeflate: false
});

// Active connections and streams
const connections = new Map();
const activeStreams = new Map();

// COBOL transaction stream handler
class TransactionStreamHandler {
    constructor(connectionId, ws, filters) {
        this.connectionId = connectionId;
        this.ws = ws;
        this.filters = filters;
        this.streamId = uuidv4();
        this.isActive = true;
        this.inputFile = `stream-input-${this.streamId}.dat`;
        this.outputFile = `stream-output-${this.streamId}.dat`;
        this.cobolProcess = null;
    }
    
    async start() {
        try {
            // Prepare COBOL input
            const streamRequest = this.formatStreamRequest();
            await fs.writeFile(this.inputFile, streamRequest);
            
            // Start COBOL streaming process
            this.cobolProcess = exec(
                `./transaction-stream < ${this.inputFile}`,
                { maxBuffer: 10 * 1024 * 1024 } // 10MB buffer
            );
            
            // Handle COBOL output
            this.cobolProcess.stdout.on('data', (data) => {
                this.processStreamData(data);
            });
            
            this.cobolProcess.stderr.on('data', (data) => {
                logger.error(`COBOL stream error: ${data}`);
            });
            
            this.cobolProcess.on('close', (code) => {
                logger.info(`COBOL stream closed with code ${code}`);
                this.cleanup();
            });
            
            // Send initial connection message
            this.sendMessage({
                type: 'stream_started',
                streamId: this.streamId,
                filters: this.filters
            });
            
            logger.info(`Transaction stream started: ${this.streamId}`);
            
        } catch (error) {
            logger.error(`Failed to start stream: ${error.message}`);
            this.sendError('Failed to start transaction stream');
        }
    }
    
    formatStreamRequest() {
        const {
            streamType = 'LIVE-FEED',
            accountFilter = '',
            dateFrom = '00000000',
            dateTo = '99999999',
            transTypeFilter = '',
            amountMin = 0,
            amountMax = 999999999.99,
            maxRecords = 100,
            streamMode = 'REALTIME'
        } = this.filters;
        
        let request = '';
        request += streamType.padEnd(20);
        request += accountFilter.padEnd(20);
        request += dateFrom.padEnd(8);
        request += dateTo.padEnd(8);
        request += transTypeFilter.padEnd(20);
        request += this.formatAmount(amountMin, 12);
        request += this.formatAmount(amountMax, 12);
        request += maxRecords.toString().padStart(4, '0');
        request += streamMode.padEnd(10);
        
        return request + '\n';
    }
    
    formatAmount(value, totalDigits) {
        const intValue = Math.round(value * 100);
        return intValue.toString().padStart(totalDigits, '0');
    }
    
    processStreamData(data) {
        try {
            const lines = data.toString().split('\n').filter(line => line.trim());
            
            for (const line of lines) {
                if (line.startsWith('{')) {
                    try {
                        const transaction = JSON.parse(line);
                        this.sendTransaction(transaction);
                    } catch (parseError) {
                        // Not JSON, might be partial data
                    }
                }
            }
        } catch (error) {
            logger.error(`Error processing stream data: ${error.message}`);
        }
    }
    
    sendTransaction(transaction) {
        if (this.isActive && this.ws.readyState === WebSocket.OPEN) {
            this.sendMessage({
                type: 'transaction',
                data: transaction,
                timestamp: new Date().toISOString()
            });
        }
    }
    
    sendMessage(message) {
        if (this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify(message));
        }
    }
    
    sendError(error) {
        this.sendMessage({
            type: 'error',
            error: error,
            streamId: this.streamId
        });
    }
    
    stop() {
        this.isActive = false;
        
        if (this.cobolProcess) {
            this.cobolProcess.kill();
        }
        
        this.cleanup();
        
        logger.info(`Transaction stream stopped: ${this.streamId}`);
    }
    
    async cleanup() {
        try {
            await fs.unlink(this.inputFile).catch(() => {});
            await fs.unlink(this.outputFile).catch(() => {});
        } catch (error) {
            // Ignore cleanup errors
        }
    }
}

// Handle WebSocket connections
wss.on('connection', (ws, req) => {
    const connectionId = uuidv4();
    const clientIp = req.socket.remoteAddress;
    
    logger.info(`New WebSocket connection: ${connectionId} from ${clientIp}`);
    
    // Store connection
    connections.set(connectionId, {
        ws: ws,
        ip: clientIp,
        connected: new Date(),
        authenticated: false,
        userId: null
    });
    
    // Send welcome message
    ws.send(JSON.stringify({
        type: 'connection',
        connectionId: connectionId,
        message: 'Connected to COBOL Transaction Stream'
    }));
    
    // Handle messages
    ws.on('message', async (message) => {
        try {
            const data = JSON.parse(message);
            await handleMessage(connectionId, data);
        } catch (error) {
            logger.error(`Invalid message from ${connectionId}: ${error.message}`);
            ws.send(JSON.stringify({
                type: 'error',
                error: 'Invalid message format'
            }));
        }
    });
    
    // Handle disconnection
    ws.on('close', () => {
        logger.info(`WebSocket disconnected: ${connectionId}`);
        
        // Stop any active streams
        const stream = activeStreams.get(connectionId);
        if (stream) {
            stream.stop();
            activeStreams.delete(connectionId);
        }
        
        connections.delete(connectionId);
    });
    
    // Handle errors
    ws.on('error', (error) => {
        logger.error(`WebSocket error for ${connectionId}: ${error.message}`);
    });
});

// Message handler
async function handleMessage(connectionId, data) {
    const connection = connections.get(connectionId);
    if (!connection) return;
    
    const { ws } = connection;
    
    switch (data.type) {
        case 'authenticate':
            await handleAuthentication(connectionId, data);
            break;
            
        case 'start_stream':
            await handleStartStream(connectionId, data);
            break;
            
        case 'stop_stream':
            handleStopStream(connectionId);
            break;
            
        case 'update_filters':
            await handleUpdateFilters(connectionId, data);
            break;
            
        case 'search':
            await handleSearch(connectionId, data);
            break;
            
        case 'ping':
            ws.send(JSON.stringify({ type: 'pong' }));
            break;
            
        default:
            ws.send(JSON.stringify({
                type: 'error',
                error: 'Unknown message type'
            }));
    }
}

// Authentication handler
async function handleAuthentication(connectionId, data) {
    const connection = connections.get(connectionId);
    if (!connection) return;
    
    const { token } = data;
    
    // Validate token with COBOL auth service
    // For now, we'll simulate this
    if (token && token.length > 10) {
        connection.authenticated = true;
        connection.userId = data.userId || 'authenticated_user';
        
        connection.ws.send(JSON.stringify({
            type: 'authenticated',
            message: 'Authentication successful'
        }));
        
        logger.info(`Authenticated connection: ${connectionId}`);
    } else {
        connection.ws.send(JSON.stringify({
            type: 'error',
            error: 'Authentication failed'
        }));
    }
}

// Start stream handler
async function handleStartStream(connectionId, data) {
    const connection = connections.get(connectionId);
    if (!connection) return;
    
    if (!connection.authenticated) {
        connection.ws.send(JSON.stringify({
            type: 'error',
            error: 'Not authenticated'
        }));
        return;
    }
    
    // Stop any existing stream
    const existingStream = activeStreams.get(connectionId);
    if (existingStream) {
        existingStream.stop();
    }
    
    // Create new stream
    const stream = new TransactionStreamHandler(
        connectionId,
        connection.ws,
        data.filters || {}
    );
    
    activeStreams.set(connectionId, stream);
    await stream.start();
}

// Stop stream handler
function handleStopStream(connectionId) {
    const stream = activeStreams.get(connectionId);
    if (stream) {
        stream.stop();
        activeStreams.delete(connectionId);
        
        const connection = connections.get(connectionId);
        if (connection) {
            connection.ws.send(JSON.stringify({
                type: 'stream_stopped',
                message: 'Transaction stream stopped'
            }));
        }
    }
}

// Update filters handler
async function handleUpdateFilters(connectionId, data) {
    const connection = connections.get(connectionId);
    if (!connection || !connection.authenticated) return;
    
    // Restart stream with new filters
    handleStopStream(connectionId);
    await handleStartStream(connectionId, data);
}

// Search handler
async function handleSearch(connectionId, data) {
    const connection = connections.get(connectionId);
    if (!connection || !connection.authenticated) return;
    
    const searchId = uuidv4();
    const inputFile = `search-input-${searchId}.dat`;
    const outputFile = `search-output-${searchId}.dat`;
    
    try {
        // Prepare search request
        const searchRequest = formatSearchRequest({
            ...data.filters,
            streamType: 'SEARCH'
        });
        
        await fs.writeFile(inputFile, searchRequest);
        
        // Execute search
        exec(`./transaction-stream < ${inputFile} > ${outputFile}`, async (error) => {
            if (error) {
                connection.ws.send(JSON.stringify({
                    type: 'search_error',
                    error: 'Search failed'
                }));
                return;
            }
            
            try {
                const results = await fs.readFile(outputFile, 'utf8');
                const transactions = parseSearchResults(results);
                
                connection.ws.send(JSON.stringify({
                    type: 'search_results',
                    searchId: searchId,
                    results: transactions
                }));
                
            } catch (readError) {
                connection.ws.send(JSON.stringify({
                    type: 'search_error',
                    error: 'Failed to read results'
                }));
            }
            
            // Cleanup
            await fs.unlink(inputFile).catch(() => {});
            await fs.unlink(outputFile).catch(() => {});
        });
        
    } catch (error) {
        logger.error(`Search error: ${error.message}`);
        connection.ws.send(JSON.stringify({
            type: 'search_error',
            error: 'Search failed'
        }));
    }
}

// Format search request
function formatSearchRequest(filters) {
    const handler = new TransactionStreamHandler(null, null, filters);
    return handler.formatStreamRequest();
}

// Parse search results
function parseSearchResults(output) {
    try {
        const lines = output.split('\n').filter(line => line.trim());
        const lastLine = lines[lines.length - 1];
        
        if (lastLine && lastLine.startsWith('{')) {
            const result = JSON.parse(lastLine);
            return result.transactions || [];
        }
    } catch (error) {
        logger.error(`Failed to parse search results: ${error.message}`);
    }
    
    return [];
}

// Compile COBOL transaction stream program
exec('cobc -x -o transaction-stream transaction-stream.cob', (error) => {
    if (error) {
        logger.error('Failed to compile transaction-stream COBOL program:', error);
        process.exit(1);
    }
    logger.info('Transaction stream COBOL program compiled successfully');
});

// Start WebSocket server
logger.info('WebSocket server started on port 8080');
logger.info('Ready for transaction stream connections');

// Graceful shutdown
process.on('SIGTERM', () => {
    logger.info('Shutting down WebSocket server...');
    
    // Close all streams
    activeStreams.forEach(stream => stream.stop());
    
    // Close all connections
    connections.forEach(conn => conn.ws.close());
    
    wss.close(() => {
        logger.info('WebSocket server closed');
        process.exit(0);
    });
});