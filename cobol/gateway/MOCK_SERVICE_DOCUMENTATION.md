# Mock COBOL Service Documentation

## Service Overview
The Mock COBOL Service simulates mainframe COBOL responses for demo purposes. It runs on port 8090 and provides endpoints for payment processing, card validation, mortgage calculations, and mainframe synchronization.

## Available Endpoints

### 1. Health Check
- **URL**: `GET /health`
- **Response**: JSON with service status
- **Example Response**:
```json
{
  "status": "online",
  "service": "Mock COBOL Service",
  "message": "This is a simulated COBOL service for demo purposes"
}
```

### 2. Card Validation
- **URL**: `POST /cobol/VALIDATE-CARD`
- **Content-Type**: `text/plain`
- **Request Format**: Fixed-width format
  - Card Number: positions 0-18 (19 characters)
  - Transaction Type: positions 19-28 (10 characters)
  - Amount: positions 29-38 (10 characters, in cents)
  - Date: positions 39-46 (8 characters, YYYYMMDD)
- **Response Format**: Fixed-width format
  - Status Code: positions 0-1
  - Transaction ID: positions 2-21
  - Auth Code: positions 22-27
  - Message: positions 28-77
  - Available Balance: positions 78-87
  - Card Type: positions 88-97
  - Risk Score: positions 98-100

### 3. Payment Processing
- **URL**: `POST /cobol/PROCESS-PAYMENT`
- **Content-Type**: `text/plain`
- **Request Format**: Pipe-delimited
  - `TYPE|AMOUNT|METHOD`
  - Types: SALE, REFUND
  - Methods: CARD, ACH, WIRE
- **Response Format**: Pipe-delimited
  - `STATUS|PAYMENT_ID|MESSAGE|NET_AMOUNT|FEE`
- **Fee Structure**:
  - CARD: 2.95% of amount
  - ACH: $0.50 flat fee
  - WIRE: $25.00 flat fee

### 4. Mortgage Calculation
- **URL**: `POST /cobol/CALCULATE-MORTGAGE`
- **Content-Type**: `text/plain`
- **Request Format**: Pipe-delimited
  - `PRINCIPAL|YEARS|RATE`
- **Response Format**: Pipe-delimited
  - `STATUS|MORTGAGE_ID|MONTHLY_PAYMENT|TOTAL_PAYMENT|TOTAL_INTEREST|MESSAGE`

### 5. Mainframe Sync
- **URL**: `POST /cobol/MAINFRAME-SYNC`
- **Content-Type**: `text/plain`
- **Request**: Any text data
- **Response**: JSON with sync status

## Test Credit Card Numbers

The following test credit card numbers are accepted by the mock service:

| Card Number        | Type       | Credit Limit | Current Balance | Available Credit |
|-------------------|------------|--------------|-----------------|------------------|
| 4111111111111111  | VISA       | $10,000      | $2,000          | $8,000          |
| 5555555555554444  | MASTERCARD | $15,000      | $5,000          | $10,000         |
| 378282246310005   | AMEX       | $25,000      | $10,000         | $15,000         |
| 6011111111111117  | DISCOVER   | $8,000       | $3,000          | $5,000          |

## Response Status Codes

- **00**: Success/Approved
- **02**: Insufficient funds
- **03**: Invalid card number

## Example Test Commands

### Test Card Validation (Approved)
```bash
curl -X POST http://localhost:8090/cobol/VALIDATE-CARD \
  -H "Content-Type: text/plain" \
  -d "4111111111111111   PAYMENT    0000002500     20240726"
```

### Test Card Validation (Insufficient Funds)
```bash
curl -X POST http://localhost:8090/cobol/VALIDATE-CARD \
  -H "Content-Type: text/plain" \
  -d "4111111111111111   PAYMENT    0001000000     20240726"
```

### Test Payment Processing
```bash
curl -X POST http://localhost:8090/cobol/PROCESS-PAYMENT \
  -H "Content-Type: text/plain" \
  -d "SALE|1500.00|CARD"
```

### Test Mortgage Calculation
```bash
curl -X POST http://localhost:8090/cobol/CALCULATE-MORTGAGE \
  -H "Content-Type: text/plain" \
  -d "350000|30|4.5"
```

### Test Mainframe Sync
```bash
curl -X POST http://localhost:8090/cobol/MAINFRAME-SYNC \
  -H "Content-Type: text/plain" \
  -d "SYNC_REQUEST"
```

## Starting the Service

If the service is not running, start it with:
```bash
cd /Users/jfuginay/Documents/dev/SuiteCRM-fork/cobol/gateway
node mock-cobol-service.js
```

The service will start on port 8090 by default. You can change the port by setting the MOCK_PORT environment variable:
```bash
MOCK_PORT=8091 node mock-cobol-service.js
```