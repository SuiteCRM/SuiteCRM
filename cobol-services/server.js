const express = require('express');
const bodyParser = require('body-parser');
const cors = require('cors');
const { exec } = require('child_process');
const fs = require('fs').promises;
const path = require('path');
const { v4: uuidv4 } = require('uuid');
const winston = require('winston');
const rateLimit = require('express-rate-limit');

const app = express();
const PORT = process.env.PORT || 3000;

// Logger configuration
const logger = winston.createLogger({
    level: 'info',
    format: winston.format.json(),
    transports: [
        new winston.transports.File({ filename: 'error.log', level: 'error' }),
        new winston.transports.File({ filename: 'combined.log' }),
        new winston.transports.Console({
            format: winston.format.simple()
        })
    ]
});

// Rate limiting
const limiter = rateLimit({
    windowMs: 1 * 60 * 1000, // 1 minute
    max: 100 // limit each IP to 100 requests per minute
});

// Middleware
app.use(cors());
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));
app.use(limiter);

// Health check endpoint
app.get('/health', (req, res) => {
    res.json({
        status: 'healthy',
        service: 'COBOL Financial Services',
        timestamp: new Date().toISOString()
    });
});

// Main calculation endpoint
app.post('/calculate', async (req, res) => {
    const requestId = uuidv4();
    const inputFile = `calc-input-${requestId}.dat`;
    const outputFile = `calc-output-${requestId}.dat`;
    
    try {
        logger.info(`Processing calculation request ${requestId}`, req.body);
        
        // Validate request
        const { type, principal, rate, term, lineItems } = req.body;
        
        if (!type) {
            return res.status(400).json({
                error: 'Calculation type is required',
                requestId
            });
        }
        
        // Prepare COBOL input file
        const inputData = await prepareCobolInput(req.body);
        await fs.writeFile(inputFile, inputData);
        
        // Execute COBOL program
        const startTime = Date.now();
        
        exec(`./financial-calc < ${inputFile} > ${outputFile}`, async (error, stdout, stderr) => {
            try {
                if (error) {
                    logger.error(`COBOL execution error for ${requestId}:`, error);
                    throw new Error('COBOL calculation failed');
                }
                
                // Read and parse output
                const output = await fs.readFile(outputFile, 'utf8');
                const result = parseCobolOutput(output);
                
                // Add metadata
                result.requestId = requestId;
                result.processingTime = `${Date.now() - startTime}ms`;
                result.precision = 'COBOL COMP-3 (packed decimal)';
                
                logger.info(`Calculation completed for ${requestId}`);
                res.json(result);
                
                // Cleanup files
                await cleanup(inputFile, outputFile);
                
            } catch (parseError) {
                logger.error(`Output parsing error for ${requestId}:`, parseError);
                res.status(500).json({
                    error: 'Failed to parse calculation result',
                    requestId
                });
            }
        });
        
    } catch (err) {
        logger.error(`Request processing error for ${requestId}:`, err);
        res.status(500).json({
            error: 'Internal server error',
            message: err.message,
            requestId
        });
        
        // Cleanup on error
        await cleanup(inputFile, outputFile).catch(() => {});
    }
});

// Batch calculation endpoint
app.post('/batch', async (req, res) => {
    const batchId = uuidv4();
    
    try {
        const { calculations } = req.body;
        
        if (!Array.isArray(calculations)) {
            return res.status(400).json({
                error: 'Calculations array is required'
            });
        }
        
        const results = [];
        
        for (const calc of calculations) {
            const result = await processCalculation(calc);
            results.push(result);
        }
        
        res.json({
            batchId,
            results,
            summary: {
                total: results.length,
                successful: results.filter(r => r.status === 'success').length,
                failed: results.filter(r => r.status === 'error').length
            }
        });
        
    } catch (err) {
        logger.error(`Batch processing error for ${batchId}:`, err);
        res.status(500).json({
            error: 'Batch processing failed',
            batchId
        });
    }
});

// Helper function to prepare COBOL input
async function prepareCobolInput(data) {
    const {
        type = '',
        principal = 0,
        rate = 0,
        term = 0,
        compoundFreq = 0,
        lineItems = []
    } = data;
    
    // Format fields for COBOL fixed-width format
    let input = '';
    input += type.padEnd(20);
    input += formatNumber(principal, 12);
    input += formatNumber(rate, 9, 6);
    input += formatNumber(term, 4);
    input += formatNumber(compoundFreq, 3);
    
    // Add line items (max 50)
    for (let i = 0; i < 50; i++) {
        if (i < lineItems.length) {
            const item = lineItems[i];
            input += formatNumber(item.quantity || 0, 7, 2);
            input += formatNumber(item.price || 0, 9, 2);
            input += formatNumber(item.discount || 0, 5, 2);
            input += formatNumber(item.taxRate || 0, 5, 3);
        } else {
            // Pad with zeros
            input += '0'.repeat(26);
        }
    }
    
    return input + '\n';
}

// Helper function to format numbers for COBOL
function formatNumber(value, totalDigits, decimalPlaces = 2) {
    const multiplier = Math.pow(10, decimalPlaces);
    const intValue = Math.round(value * multiplier);
    return intValue.toString().padStart(totalDigits, '0');
}

// Helper function to parse COBOL output
function parseCobolOutput(output) {
    try {
        // COBOL outputs JSON-like format
        const lines = output.trim().split('\n');
        const lastLine = lines[lines.length - 1];
        
        // Basic JSON parsing with error handling
        const result = JSON.parse(lastLine);
        
        // Convert string numbers to actual numbers
        if (result.result) result.result = parseFloat(result.result.replace(/[$,]/g, ''));
        if (result.payment) result.payment = parseFloat(result.payment.replace(/[$,]/g, ''));
        if (result.interest) result.interest = parseFloat(result.interest.replace(/[$,]/g, ''));
        if (result.total) result.total = parseFloat(result.total.replace(/[$,]/g, ''));
        
        return result;
    } catch (err) {
        throw new Error('Invalid COBOL output format');
    }
}

// Helper function for single calculation processing
async function processCalculation(calcData) {
    const requestId = uuidv4();
    const inputFile = `calc-input-${requestId}.dat`;
    const outputFile = `calc-output-${requestId}.dat`;
    
    try {
        const inputData = await prepareCobolInput(calcData);
        await fs.writeFile(inputFile, inputData);
        
        return new Promise((resolve) => {
            exec(`./financial-calc < ${inputFile} > ${outputFile}`, async (error) => {
                try {
                    if (error) {
                        throw new Error('COBOL calculation failed');
                    }
                    
                    const output = await fs.readFile(outputFile, 'utf8');
                    const result = parseCobolOutput(output);
                    
                    await cleanup(inputFile, outputFile);
                    resolve(result);
                    
                } catch (err) {
                    await cleanup(inputFile, outputFile).catch(() => {});
                    resolve({
                        status: 'error',
                        error: err.message
                    });
                }
            });
        });
        
    } catch (err) {
        return {
            status: 'error',
            error: err.message
        };
    }
}

// Cleanup temporary files
async function cleanup(...files) {
    for (const file of files) {
        try {
            await fs.unlink(file);
        } catch (err) {
            // Ignore cleanup errors
        }
    }
}

// Compile COBOL on startup
exec('cobc -x -o financial-calc financial-calc.cob', (error, stdout, stderr) => {
    if (error) {
        logger.error('Failed to compile COBOL program:', error);
        process.exit(1);
    }
    logger.info('COBOL program compiled successfully');
});

// Authentication endpoints
app.post('/auth/login', async (req, res) => {
    const authId = uuidv4();
    const inputFile = `auth-input-${authId}.dat`;
    const outputFile = `auth-output-${authId}.dat`;
    
    try {
        const { username, password, domain } = req.body;
        
        if (!username || !password) {
            return res.status(400).json({
                error: 'Username and password are required',
                authId
            });
        }
        
        // Prepare COBOL input
        const authData = formatAuthInput({
            type: 'LOGIN',
            username,
            password,
            domain: domain || 'MAINFRAME',
            clientIp: req.ip,
            sessionId: authId
        });
        
        await fs.writeFile(inputFile, authData);
        
        // Execute COBOL auth program
        exec(`./legacy-auth < ${inputFile} > ${outputFile}`, async (error) => {
            try {
                if (error) {
                    throw new Error('Authentication service error');
                }
                
                const output = await fs.readFile(outputFile, 'utf8');
                const result = JSON.parse(output.trim());
                
                res.json(result);
                await cleanup(inputFile, outputFile);
                
            } catch (err) {
                res.status(500).json({
                    status: 'error',
                    message: 'Authentication failed',
                    authId
                });
            }
        });
        
    } catch (err) {
        logger.error(`Auth error for ${authId}:`, err);
        res.status(500).json({
            status: 'error',
            message: err.message,
            authId
        });
    }
});

app.post('/auth/validate', async (req, res) => {
    const authId = uuidv4();
    const inputFile = `auth-input-${authId}.dat`;
    const outputFile = `auth-output-${authId}.dat`;
    
    try {
        const { token } = req.body;
        
        if (!token) {
            return res.status(400).json({
                error: 'Token is required'
            });
        }
        
        const authData = formatAuthInput({
            type: 'VALIDATE-TOKEN',
            token,
            clientIp: req.ip
        });
        
        await fs.writeFile(inputFile, authData);
        
        exec(`./legacy-auth < ${inputFile} > ${outputFile}`, async (error) => {
            try {
                const output = await fs.readFile(outputFile, 'utf8');
                const result = JSON.parse(output.trim());
                
                res.json(result);
                await cleanup(inputFile, outputFile);
                
            } catch (err) {
                res.status(401).json({
                    status: 'error',
                    message: 'Invalid token'
                });
            }
        });
        
    } catch (err) {
        res.status(500).json({
            status: 'error',
            message: err.message
        });
    }
});

app.post('/auth/logout', async (req, res) => {
    const { token } = req.body;
    
    if (!token) {
        return res.status(400).json({
            error: 'Token is required'
        });
    }
    
    // Process logout through COBOL
    const result = await processAuthRequest({
        type: 'LOGOUT',
        token
    });
    
    res.json(result);
});

// Helper function to format auth input
function formatAuthInput(data) {
    const {
        type = '',
        username = '',
        password = '',
        token = '',
        domain = '',
        clientIp = '',
        sessionId = ''
    } = data;
    
    let input = '';
    input += type.padEnd(20);
    input += username.padEnd(30);
    input += password.padEnd(50);
    input += token.padEnd(64);
    input += domain.padEnd(30);
    input += clientIp.padEnd(15);
    input += sessionId.padEnd(36);
    
    return input + '\n';
}

// Helper for auth requests
async function processAuthRequest(authData) {
    const authId = uuidv4();
    const inputFile = `auth-input-${authId}.dat`;
    const outputFile = `auth-output-${authId}.dat`;
    
    try {
        const input = formatAuthInput(authData);
        await fs.writeFile(inputFile, input);
        
        return new Promise((resolve) => {
            exec(`./legacy-auth < ${inputFile} > ${outputFile}`, async (error) => {
                try {
                    const output = await fs.readFile(outputFile, 'utf8');
                    const result = JSON.parse(output.trim());
                    await cleanup(inputFile, outputFile);
                    resolve(result);
                } catch (err) {
                    resolve({
                        status: 'error',
                        message: 'Processing failed'
                    });
                }
            });
        });
    } catch (err) {
        return {
            status: 'error',
            message: err.message
        };
    }
}

// Compile COBOL auth program
exec('cobc -x -o legacy-auth legacy-auth.cob', (error, stdout, stderr) => {
    if (error) {
        logger.error('Failed to compile legacy-auth COBOL program:', error);
    } else {
        logger.info('Legacy auth COBOL program compiled successfully');
    }
});

// Start server
app.listen(PORT, () => {
    logger.info(`COBOL Financial Services running on port ${PORT}`);
    logger.info('Available endpoints:');
    logger.info('  GET  /health - Health check');
    logger.info('  POST /calculate - Single calculation');
    logger.info('  POST /batch - Batch calculations');
    logger.info('  POST /auth/login - User login');
    logger.info('  POST /auth/validate - Validate token');
    logger.info('  POST /auth/logout - User logout');
});