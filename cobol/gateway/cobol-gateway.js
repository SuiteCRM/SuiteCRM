/**
 * COBOL Gateway Server
 * Enables native COBOL device communication with SuiteCRM
 */

const express = require('express');
const WebSocket = require('ws');
const { spawn } = require('child_process');
const cors = require('cors');
const bodyParser = require('body-parser');
const path = require('path');
const fs = require('fs');

const app = express();
const PORT = process.env.COBOL_PORT || 8080;
const WS_PORT = process.env.COBOL_WS_PORT || 8081;

// Middleware
app.use(cors());
app.use(bodyParser.text({ type: 'text/plain' }));
app.use(bodyParser.json());

// WebSocket server for real-time communication
const wss = new WebSocket.Server({ port: WS_PORT });

// Connected clients
const clients = new Map();

// COBOL program paths
const COBOL_PROGRAMS = {
    'VALIDATE-CARD': path.join(__dirname, '../programs/CREDITCARD'),
    'PROCESS-PAYMENT': path.join(__dirname, '../programs/PAYMENT'),
    'CALCULATE-MORTGAGE': path.join(__dirname, '../programs/MORTGAGE'),
    'MAINFRAME-SYNC': path.join(__dirname, '../programs/MAINFRAME')
};

// Initialize COBOL runtime
function initializeCOBOL() {
    console.log('Initializing COBOL runtime...');
    
    // Compile COBOL programs if needed
    Object.entries(COBOL_PROGRAMS).forEach(([operation, programPath]) => {
        const cobFile = programPath + '.cob';
        const exeFile = programPath;
        
        if (fs.existsSync(cobFile) && !fs.existsSync(exeFile)) {
            console.log(`Compiling ${cobFile}...`);
            const compile = spawn('cobc', ['-x', '-o', exeFile, cobFile]);
            
            compile.on('close', (code) => {
                if (code === 0) {
                    console.log(`✓ Compiled ${operation}`);
                } else {
                    console.error(`✗ Failed to compile ${operation}`);
                }
            });
        }
    });
}

// Execute COBOL program
function executeCOBOL(operation, data) {
    return new Promise((resolve, reject) => {
        const program = COBOL_PROGRAMS[operation];
        
        // Check if we should use mock service
        const useMock = process.env.USE_MOCK_COBOL === 'true' || !program || !fs.existsSync(program);
        
        if (useMock) {
            // Use mock service as fallback
            console.log(`Using mock COBOL service for ${operation}`);
            const mockUrl = `http://localhost:8090/cobol/${operation}`;
            
            const http = require('http');
            const postData = data;
            
            const options = {
                hostname: 'localhost',
                port: 8090,
                path: `/cobol/${operation}`,
                method: 'POST',
                headers: {
                    'Content-Type': 'text/plain',
                    'Content-Length': Buffer.byteLength(postData)
                }
            };
            
            const req = http.request(options, (res) => {
                let output = '';
                
                res.on('data', (chunk) => {
                    output += chunk;
                });
                
                res.on('end', () => {
                    if (res.statusCode === 200) {
                        resolve(output);
                    } else {
                        reject(new Error(`Mock service returned ${res.statusCode}`));
                    }
                });
            });
            
            req.on('error', (e) => {
                // If mock service fails, try actual COBOL
                if (program && fs.existsSync(program)) {
                    runActualCOBOL();
                } else {
                    reject(new Error(`COBOL program not found and mock service unavailable: ${operation}`));
                }
            });
            
            req.write(postData);
            req.end();
            return;
        }
        
        // Run actual COBOL program
        runActualCOBOL();
        
        function runActualCOBOL() {
            const cobol = spawn(program, [], {
                env: { ...process.env, COBOL_DATA: data }
            });
            
            let output = '';
            let error = '';
            
            cobol.stdout.on('data', (data) => {
                output += data.toString();
            });
            
            cobol.stderr.on('data', (data) => {
                error += data.toString();
            });
            
            cobol.on('close', (code) => {
                if (code === 0) {
                    resolve(output.trim());
                } else {
                    reject(new Error(error || `COBOL program exited with code ${code}`));
                }
            });
            
            // Send input data to COBOL program
            cobol.stdin.write(data);
            cobol.stdin.end();
        }
    });
}

// REST API Endpoints

// Health check
app.get('/health', (req, res) => {
    res.json({
        status: 'online',
        version: '1.0.0',
        cobol_runtime: 'GnuCOBOL 3.1',
        websocket_clients: clients.size,
        uptime: process.uptime()
    });
});

// COBOL operation endpoint
app.post('/cobol/:operation', async (req, res) => {
    const operation = req.params.operation.toUpperCase();
    const data = req.body;
    
    console.log(`Processing ${operation} request`);
    
    try {
        // Notify WebSocket clients
        broadcastToClients({
            type: 'operation_start',
            operation: operation,
            timestamp: new Date().toISOString()
        });
        
        // Execute COBOL program
        const result = await executeCOBOL(operation, data);
        
        // Parse and send response
        res.send(result);
        
        // Notify WebSocket clients
        broadcastToClients({
            type: 'operation_complete',
            operation: operation,
            result: result,
            timestamp: new Date().toISOString()
        });
        
    } catch (error) {
        console.error(`Error in ${operation}:`, error);
        res.status(500).json({
            error: error.message,
            operation: operation
        });
        
        broadcastToClients({
            type: 'operation_error',
            operation: operation,
            error: error.message,
            timestamp: new Date().toISOString()
        });
    }
});

// Batch processing endpoint
app.post('/cobol/batch', async (req, res) => {
    const transactions = req.body.transactions || [];
    const results = [];
    
    console.log(`Processing batch of ${transactions.length} transactions`);
    
    for (const transaction of transactions) {
        try {
            const result = await executeCOBOL('PROCESS-PAYMENT', 
                formatTransactionForCOBOL(transaction));
            results.push({ success: true, result });
        } catch (error) {
            results.push({ success: false, error: error.message });
        }
    }
    
    res.json({
        processed: transactions.length,
        successful: results.filter(r => r.success).length,
        failed: results.filter(r => !r.success).length,
        results: results
    });
});

// Device communication endpoints

// Card reader endpoint
app.post('/device/card-reader', async (req, res) => {
    const { device_id, card_data } = req.body;
    
    console.log(`Card reader ${device_id} data received`);
    
    try {
        // Process through COBOL
        const result = await executeCOBOL('VALIDATE-CARD', card_data);
        
        // Send to connected WebSocket clients
        broadcastToClients({
            type: 'card_read',
            device_id: device_id,
            result: result,
            timestamp: new Date().toISOString()
        });
        
        res.json({ status: 'processed', result });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// Mainframe sync endpoint
app.post('/mainframe/sync', async (req, res) => {
    const { sync_type, data } = req.body;
    
    console.log(`Mainframe sync: ${sync_type}`);
    
    try {
        const result = await executeCOBOL('MAINFRAME-SYNC', 
            JSON.stringify({ sync_type, data }));
        
        res.json({ status: 'synced', result });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

// WebSocket handling
wss.on('connection', (ws, req) => {
    const clientId = generateClientId();
    const clientInfo = {
        id: clientId,
        ip: req.socket.remoteAddress,
        connectedAt: new Date(),
        ws: ws
    };
    
    clients.set(clientId, clientInfo);
    console.log(`WebSocket client connected: ${clientId}`);
    
    // Send welcome message
    ws.send(JSON.stringify({
        type: 'connected',
        clientId: clientId,
        message: 'Connected to COBOL Gateway'
    }));
    
    // Handle messages
    ws.on('message', async (message) => {
        try {
            const data = JSON.parse(message);
            
            switch (data.action) {
                case 'validate_payment':
                    await handlePaymentValidation(clientId, data.data);
                    break;
                    
                case 'subscribe':
                    handleSubscription(clientId, data.property_id);
                    break;
                    
                case 'device_register':
                    handleDeviceRegistration(clientId, data.device_info);
                    break;
                    
                default:
                    ws.send(JSON.stringify({
                        type: 'error',
                        message: 'Unknown action'
                    }));
            }
        } catch (error) {
            console.error('WebSocket message error:', error);
            ws.send(JSON.stringify({
                type: 'error',
                message: error.message
            }));
        }
    });
    
    // Handle disconnect
    ws.on('close', () => {
        clients.delete(clientId);
        console.log(`WebSocket client disconnected: ${clientId}`);
    });
});

// WebSocket helper functions
function generateClientId() {
    return 'client_' + Date.now() + '_' + Math.random().toString(36).substr(2, 9);
}

function broadcastToClients(data) {
    const message = JSON.stringify(data);
    
    clients.forEach((client) => {
        if (client.ws.readyState === WebSocket.OPEN) {
            client.ws.send(message);
        }
    });
}

async function handlePaymentValidation(clientId, paymentData) {
    const client = clients.get(clientId);
    if (!client) return;
    
    try {
        // Send processing status
        client.ws.send(JSON.stringify({
            type: 'payment_update',
            status: 'processing'
        }));
        
        // Format for COBOL
        const cobolData = formatPaymentForCOBOL(paymentData);
        
        // Execute COBOL validation
        const result = await executeCOBOL('VALIDATE-CARD', cobolData);
        
        // Parse COBOL response
        const response = parseCOBOLResponse(result);
        
        // Send result
        client.ws.send(JSON.stringify({
            type: 'validation_result',
            ...response
        }));
        
    } catch (error) {
        client.ws.send(JSON.stringify({
            type: 'validation_result',
            status: 'ERROR',
            message: error.message
        }));
    }
}

function handleSubscription(clientId, propertyId) {
    const client = clients.get(clientId);
    if (!client) return;
    
    client.subscribedProperty = propertyId;
    
    client.ws.send(JSON.stringify({
        type: 'subscribed',
        property_id: propertyId
    }));
}

function handleDeviceRegistration(clientId, deviceInfo) {
    const client = clients.get(clientId);
    if (!client) return;
    
    client.deviceInfo = deviceInfo;
    
    console.log(`Device registered: ${deviceInfo.type} - ${deviceInfo.id}`);
    
    client.ws.send(JSON.stringify({
        type: 'device_registered',
        device_id: deviceInfo.id
    }));
}

// Helper functions
function formatPaymentForCOBOL(data) {
    // Format according to COBOL expectations
    const formatted = [
        data.number.padEnd(19, ' '),
        data.expiry.replace('/', ''),
        data.cvv.padStart(3, '0'),
        Math.floor(data.amount * 100).toString().padStart(10, '0'),
        'EARNEST'.padEnd(10, ' '),
        data.property_id.padEnd(20, ' '),
        new Date().toISOString().replace(/[-:T.]/g, '').substr(0, 14)
    ].join('');
    
    return formatted;
}

function formatTransactionForCOBOL(transaction) {
    return [
        transaction.card_number.padEnd(19, ' '),
        transaction.expiry_date,
        transaction.cvv.padStart(3, '0'),
        Math.floor(transaction.amount * 100).toString().padStart(10, '0'),
        transaction.type.padEnd(10, ' '),
        transaction.reference.padEnd(20, ' '),
        new Date().toISOString().replace(/[-:T.]/g, '').substr(0, 14)
    ].join('');
}

function parseCOBOLResponse(response) {
    // Parse fixed-width COBOL response
    return {
        status_code: response.substr(0, 2),
        status: mapCOBOLStatus(response.substr(0, 2)),
        transaction_id: response.substr(2, 20).trim(),
        auth_code: response.substr(22, 6).trim(),
        message: response.substr(28, 50).trim(),
        available_balance: parseInt(response.substr(78, 10)) / 100,
        card_type: response.substr(88, 10).trim(),
        risk_score: parseInt(response.substr(98, 3))
    };
}

function mapCOBOLStatus(code) {
    const statusMap = {
        '00': 'APPROVED',
        '01': 'DECLINED',
        '02': 'INSUFFICIENT_FUNDS',
        '03': 'INVALID_CARD',
        '04': 'EXPIRED_CARD',
        '05': 'INVALID_CVV',
        '06': 'FRAUD_SUSPECTED',
        '07': 'SYSTEM_ERROR',
        '99': 'UNKNOWN_ERROR'
    };
    
    return statusMap[code] || 'UNKNOWN';
}

// Start mock service if enabled
if (process.env.USE_MOCK_COBOL === 'true') {
    const mockService = spawn('node', [path.join(__dirname, 'mock-cobol-service.js')], {
        env: { ...process.env, MOCK_PORT: '8090' },
        detached: false
    });
    
    mockService.stdout.on('data', (data) => {
        console.log(`Mock Service: ${data}`);
    });
    
    mockService.stderr.on('data', (data) => {
        console.error(`Mock Service Error: ${data}`);
    });
    
    console.log('Started mock COBOL service on port 8090');
}

// Start server
initializeCOBOL();

app.listen(PORT, () => {
    console.log(`COBOL Gateway Server running on port ${PORT}`);
    console.log(`WebSocket server running on port ${WS_PORT}`);
    if (process.env.USE_MOCK_COBOL === 'true') {
        console.log('Using mock COBOL service for demo');
    }
});

// Graceful shutdown
process.on('SIGTERM', () => {
    console.log('Shutting down COBOL Gateway...');
    
    // Close WebSocket connections
    clients.forEach((client) => {
        client.ws.close();
    });
    
    wss.close(() => {
        console.log('WebSocket server closed');
    });
    
    process.exit(0);
});