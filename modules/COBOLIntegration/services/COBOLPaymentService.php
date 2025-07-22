<?php
/**
 * COBOL Payment Service Integration
 * Bridges SuiteCRM with COBOL credit card processing
 */

class COBOLPaymentService
{
    private $cobolHost;
    private $cobolPort;
    private $cobolTimeout;
    private $websocketUrl;
    
    public function __construct()
    {
        global $sugar_config;
        
        $this->cobolHost = $sugar_config['cobol_host'] ?? 'localhost';
        $this->cobolPort = $sugar_config['cobol_port'] ?? 8080;
        $this->cobolTimeout = $sugar_config['cobol_timeout'] ?? 30;
        $this->websocketUrl = $sugar_config['cobol_websocket'] ?? 'ws://localhost:8081';
    }
    
    /**
     * Validate credit card using COBOL service
     */
    public function validateCard($cardData)
    {
        // Format data for COBOL
        $cobolData = $this->formatForCOBOL($cardData);
        
        // Send to COBOL service
        $response = $this->sendToCOBOL('VALIDATE-CARD', $cobolData);
        
        // Parse COBOL response
        return $this->parseCOBOLResponse($response);
    }
    
    /**
     * Process payment transaction
     */
    public function processPayment($transactionData)
    {
        // Include COBOL transaction ID if exists
        if (!empty($transactionData['cobol_transaction_id'])) {
            $cobolData = $this->formatForCOBOL($transactionData);
            $response = $this->sendToCOBOL('PROCESS-PAYMENT', $cobolData);
            return $this->parseCOBOLResponse($response);
        }
        
        return array('status' => 'ERROR', 'message' => 'No COBOL transaction ID');
    }
    
    /**
     * Format data for COBOL consumption
     */
    private function formatForCOBOL($data)
    {
        // COBOL expects fixed-width fields
        $formatted = '';
        
        // Card number (19 chars, right padded)
        $formatted .= str_pad($data['card_number'], 19, ' ', STR_PAD_RIGHT);
        
        // Expiry date (MMYY format, 4 chars)
        $formatted .= sprintf('%02d%02d', $data['expiry_month'], $data['expiry_year'] % 100);
        
        // CVV (3 chars, left padded with zeros)
        $formatted .= str_pad($data['cvv'], 3, '0', STR_PAD_LEFT);
        
        // Amount (10 digits, 2 decimal places, no decimal point)
        $amount = intval($data['amount'] * 100);
        $formatted .= str_pad($amount, 10, '0', STR_PAD_LEFT);
        
        // Transaction type (10 chars)
        $formatted .= str_pad(substr($data['transaction_type'], 0, 10), 10, ' ', STR_PAD_RIGHT);
        
        // Property ID (20 chars)
        $formatted .= str_pad($data['property_id'], 20, ' ', STR_PAD_RIGHT);
        
        // Timestamp (14 chars - YYYYMMDDHHmmss)
        $formatted .= date('YmdHis');
        
        return $formatted;
    }
    
    /**
     * Send data to COBOL service
     */
    private function sendToCOBOL($operation, $data)
    {
        $url = "http://{$this->cobolHost}:{$this->cobolPort}/cobol/{$operation}";
        
        $ch = curl_init($url);
        curl_setopt($ch, CURLOPT_POST, 1);
        curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_TIMEOUT, $this->cobolTimeout);
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: text/plain',
            'X-COBOL-Operation: ' . $operation
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode !== 200) {
            throw new Exception("COBOL service error: HTTP $httpCode");
        }
        
        return $response;
    }
    
    /**
     * Parse COBOL response
     */
    private function parseCOBOLResponse($response)
    {
        // COBOL returns fixed-width response
        $result = array();
        
        // Status code (2 chars)
        $result['status_code'] = substr($response, 0, 2);
        $result['status'] = $this->mapStatusCode($result['status_code']);
        
        // Transaction ID (20 chars)
        $result['transaction_id'] = trim(substr($response, 2, 20));
        
        // Authorization code (6 chars)
        $result['auth_code'] = trim(substr($response, 22, 6));
        
        // Response message (50 chars)
        $result['message'] = trim(substr($response, 28, 50));
        
        // Available balance (10 digits)
        $balance = substr($response, 78, 10);
        $result['available_balance'] = intval($balance) / 100;
        
        // Card type (10 chars)
        $result['card_type'] = trim(substr($response, 88, 10));
        
        // Risk score (3 digits)
        $result['risk_score'] = intval(substr($response, 98, 3));
        
        return $result;
    }
    
    /**
     * Map COBOL status codes to human-readable status
     */
    private function mapStatusCode($code)
    {
        $statusMap = array(
            '00' => 'APPROVED',
            '01' => 'DECLINED',
            '02' => 'INSUFFICIENT_FUNDS',
            '03' => 'INVALID_CARD',
            '04' => 'EXPIRED_CARD',
            '05' => 'INVALID_CVV',
            '06' => 'FRAUD_SUSPECTED',
            '07' => 'SYSTEM_ERROR',
            '99' => 'UNKNOWN_ERROR'
        );
        
        return $statusMap[$code] ?? 'UNKNOWN';
    }
    
    /**
     * Establish WebSocket connection for real-time updates
     */
    public function connectWebSocket($propertyId)
    {
        $script = <<<JS
        <script>
        (function() {
            var ws = new WebSocket('{$this->websocketUrl}');
            
            ws.onopen = function() {
                console.log('COBOL WebSocket connected');
                ws.send(JSON.stringify({
                    action: 'subscribe',
                    property_id: '{$propertyId}'
                }));
            };
            
            ws.onmessage = function(event) {
                var data = JSON.parse(event.data);
                
                if (data.type === 'payment_update') {
                    // Update UI with payment status
                    updatePaymentStatus(data);
                } else if (data.type === 'validation_result') {
                    // Show validation result
                    showValidationResult(data);
                }
            };
            
            ws.onerror = function(error) {
                console.error('COBOL WebSocket error:', error);
            };
            
            // Make WebSocket available globally
            window.cobolWebSocket = ws;
        })();
        </script>
JS;
        
        return $script;
    }
    
    /**
     * Get COBOL system status
     */
    public function getSystemStatus()
    {
        try {
            $response = $this->sendToCOBOL('STATUS', 'PING');
            $status = $this->parseCOBOLResponse($response);
            
            return array(
                'online' => true,
                'response_time' => $status['response_time'] ?? 'N/A',
                'active_connections' => $status['active_connections'] ?? 0,
                'version' => $status['version'] ?? 'Unknown'
            );
        } catch (Exception $e) {
            return array(
                'online' => false,
                'error' => $e->getMessage()
            );
        }
    }
    
    /**
     * Process batch transactions (for nightly processing)
     */
    public function processBatch($transactions)
    {
        $batchId = uniqid('BATCH_');
        $results = array();
        
        foreach ($transactions as $transaction) {
            $transaction['batch_id'] = $batchId;
            $result = $this->processPayment($transaction);
            $results[] = $result;
        }
        
        // Send batch completion signal to COBOL
        $this->sendToCOBOL('BATCH-COMPLETE', $batchId);
        
        return array(
            'batch_id' => $batchId,
            'total' => count($transactions),
            'successful' => count(array_filter($results, function($r) { return $r['status'] === 'APPROVED'; })),
            'failed' => count(array_filter($results, function($r) { return $r['status'] !== 'APPROVED'; })),
            'results' => $results
        );
    }
}