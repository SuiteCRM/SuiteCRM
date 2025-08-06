/**
 * Mock COBOL Service
 * Provides simulated COBOL responses for demo purposes
 */

const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');

const app = express();
const PORT = process.env.MOCK_PORT || 8090;

// Middleware
app.use(cors());
app.use(bodyParser.text({ type: 'text/plain' }));
app.use(bodyParser.json());

// Simulated card database
const validCards = {
    '4111111111111111': { type: 'VISA', limit: 10000, balance: 2000 },
    '5555555555554444': { type: 'MASTERCARD', limit: 15000, balance: 5000 },
    '378282246310005': { type: 'AMEX', limit: 25000, balance: 10000 },
    '6011111111111117': { type: 'DISCOVER', limit: 8000, balance: 3000 }
};

// Mock COBOL endpoints
app.post('/cobol/VALIDATE-CARD', (req, res) => {
    const data = req.body;
    console.log('Mock VALIDATE-CARD called with:', data.substr(0, 50) + '...');
    
    // Parse card number from fixed-width format
    const cardNumber = data.substr(0, 19).trim();
    const amount = parseInt(data.substr(29, 10)) / 100;
    
    // Check if card is valid
    if (validCards[cardNumber]) {
        const card = validCards[cardNumber];
        const available = card.limit - card.balance;
        
        if (amount <= available) {
            // Approved response
            const response = 
                '00' +                                    // Status code
                'TRN' + Date.now().toString().substr(-17) + '   ' + // Transaction ID (20 chars)
                Date.now().toString().substr(-6) +       // Auth code (6 chars)
                'Payment approved successfully'.padEnd(50, ' ') + // Message (50 chars)
                String(available * 100).padStart(10, '0') + // Available balance (10 chars)
                card.type.padEnd(10, ' ') +              // Card type (10 chars)
                '150';                                    // Risk score (3 chars)
            
            res.send(response);
        } else {
            // Insufficient funds
            const response = 
                '02' +
                'TRN' + Date.now().toString().substr(-17) + '   ' +
                '000000' +
                'Insufficient funds'.padEnd(50, ' ') +
                String(available * 100).padStart(10, '0') +
                card.type.padEnd(10, ' ') +
                '250';
            
            res.send(response);
        }
    } else {
        // Invalid card
        const response = 
            '03' +
            '00000000000000000000' +
            '000000' +
            'Invalid card number'.padEnd(50, ' ') +
            '0000000000' +
            'UNKNOWN   ' +
            '999';
        
        res.send(response);
    }
});

app.post('/cobol/PROCESS-PAYMENT', (req, res) => {
    const data = req.body;
    console.log('Mock PROCESS-PAYMENT called');
    
    // Parse payment data (pipe-delimited)
    const parts = data.split('|');
    const type = parts[0];
    const amount = parseFloat(parts[1]);
    const method = parts[2];
    
    // Calculate fee based on method
    let fee = 0;
    if (method === 'CARD') {
        fee = amount * 0.0295;
    } else if (method === 'ACH') {
        fee = 0.50;
    } else if (method === 'WIRE') {
        fee = 25.00;
    }
    
    const netAmount = amount - fee;
    const paymentId = 'PAY' + Date.now();
    
    // Return pipe-delimited response
    const response = [
        '00',                                    // Status
        paymentId,                               // Payment ID
        'Payment processed successfully',        // Message
        netAmount.toFixed(2),                   // Net amount
        fee.toFixed(2)                          // Fee
    ].join('|');
    
    res.send(response);
});

app.post('/cobol/CALCULATE-MORTGAGE', (req, res) => {
    console.log('Mock CALCULATE-MORTGAGE called');
    
    // Simple mortgage calculation
    const response = [
        '00',                    // Status
        'MORT' + Date.now(),     // Mortgage ID
        '2150.00',              // Monthly payment
        '774000.00',            // Total payment
        '424000.00',            // Total interest
        'Mortgage calculated'    // Message
    ].join('|');
    
    res.send(response);
});

app.post('/cobol/MAINFRAME-SYNC', (req, res) => {
    console.log('Mock MAINFRAME-SYNC called');
    
    const response = JSON.stringify({
        status: 'SUCCESS',
        records_synced: 42,
        timestamp: new Date().toISOString(),
        message: 'Mainframe sync completed'
    });
    
    res.send(response);
});

// Health check
app.get('/health', (req, res) => {
    res.json({
        status: 'online',
        service: 'Mock COBOL Service',
        message: 'This is a simulated COBOL service for demo purposes'
    });
});

app.listen(PORT, () => {
    console.log(`Mock COBOL Service running on port ${PORT}`);
    console.log('This service simulates COBOL responses for demo purposes');
});