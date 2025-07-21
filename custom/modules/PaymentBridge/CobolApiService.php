<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * COBOL API Service for SuiteCRM Payment Bridge
 * Handles all communication with legacy COBOL payment systems
 */
class CobolApiService
{
    private $apiBase;
    private $timeout = 30;
    private $headers = array();
    
    public function __construct()
    {
        global $sugar_config;
        
        // Get API configuration from SuiteCRM config
        $this->apiBase = isset($sugar_config['cobol_api_url']) 
            ? $sugar_config['cobol_api_url'] 
            : 'http://localhost:3000/api';
            
        $this->headers = array(
            'Content-Type: application/json',
            'Accept: application/json'
        );
        
        // Add API key if configured
        if (isset($sugar_config['cobol_api_key'])) {
            $this->headers[] = 'X-API-Key: ' . $sugar_config['cobol_api_key'];
        }
    }
    
    /**
     * Validate credit card number
     */
    public function validateCard($cardNumber)
    {
        $endpoint = '/validate';
        $data = array('cardNumber' => $cardNumber);
        
        $result = $this->callApi('POST', $endpoint, $data);
        
        // Log validation attempt
        $this->logTransaction('card_validation', $cardNumber, $result);
        
        return $result;
    }
    
    /**
     * Calculate interest on balance
     */
    public function calculateInterest($cardNumber, $customBalance = null)
    {
        $endpoint = '/calculate-interest';
        $data = array('cardNumber' => $cardNumber);
        
        if ($customBalance !== null) {
            $data['customBalance'] = floatval($customBalance);
        }
        
        $result = $this->callApi('POST', $endpoint, $data);
        
        // Log calculation
        $this->logTransaction('interest_calculation', $cardNumber, $result);
        
        return $result;
    }
    
    /**
     * Generate statement
     */
    public function generateStatement($cardNumber, $format = 'json')
    {
        $endpoint = '/generate-statement';
        $data = array(
            'cardNumber' => $cardNumber,
            'format' => $format
        );
        
        $result = $this->callApi('POST', $endpoint, $data);
        
        // Log statement generation
        $this->logTransaction('statement_generation', $cardNumber, $result);
        
        return $result;
    }
    
    /**
     * Get all cards for account
     */
    public function getCards()
    {
        $endpoint = '/cards';
        return $this->callApi('GET', $endpoint);
    }
    
    /**
     * Check COBOL system health
     */
    public function getSystemHealth()
    {
        $endpoint = '/health';
        
        $startTime = microtime(true);
        $result = $this->callApi('GET', $endpoint);
        $responseTime = round((microtime(true) - $startTime) * 1000);
        
        // Store health metrics
        $this->storeHealthMetrics($result, $responseTime);
        
        return array(
            'status' => isset($result['status']) ? $result['status'] : 'error',
            'responseTime' => $responseTime,
            'timestamp' => date('Y-m-d H:i:s'),
            'details' => $result
        );
    }
    
    /**
     * Create payment plan
     */
    public function createPaymentPlan($cardNumber, $amount, $months)
    {
        // Calculate payment plan details
        $interestRates = array(
            3 => 0.02,  // 2% for 3 months
            6 => 0.05,  // 5% for 6 months
            12 => 0.10  // 10% for 12 months
        );
        
        $interestRate = isset($interestRates[$months]) ? $interestRates[$months] : 0.10;
        $totalAmount = $amount * (1 + $interestRate);
        $monthlyPayment = $totalAmount / $months;
        
        return array(
            'planId' => create_guid(),
            'months' => $months,
            'monthlyPayment' => round($monthlyPayment, 2),
            'totalAmount' => round($totalAmount, 2),
            'interestRate' => $interestRate * 100,
            'startDate' => date('Y-m-d'),
            'endDate' => date('Y-m-d', strtotime("+{$months} months"))
        );
    }
    
    /**
     * Make API call
     */
    private function callApi($method, $endpoint, $data = null)
    {
        $url = $this->apiBase . $endpoint;
        
        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_TIMEOUT, $this->timeout);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $this->headers);
        
        if ($method === 'POST') {
            curl_setopt($ch, CURLOPT_POST, true);
            if ($data) {
                curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($data));
            }
        }
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        $error = curl_error($ch);
        curl_close($ch);
        
        if ($error) {
            $GLOBALS['log']->error("COBOL API Error: " . $error);
            throw new Exception("API Connection Error: " . $error);
        }
        
        if ($httpCode >= 400) {
            $GLOBALS['log']->error("COBOL API HTTP Error: " . $httpCode);
            throw new Exception("API HTTP Error: " . $httpCode);
        }
        
        $result = json_decode($response, true);
        if (json_last_error() !== JSON_ERROR_NONE) {
            $GLOBALS['log']->error("COBOL API JSON Error: " . json_last_error_msg());
            throw new Exception("Invalid API Response");
        }
        
        return $result;
    }
    
    /**
     * Log transaction for audit trail
     */
    private function logTransaction($type, $reference, $result)
    {
        global $current_user, $db;
        
        $id = create_guid();
        $now = date('Y-m-d H:i:s');
        $status = isset($result['error']) ? 'error' : 'success';
        $response = $db->quote(json_encode($result));
        
        $sql = "INSERT INTO payment_history 
                (id, date_entered, date_modified, transaction_type, 
                 reference_number, status, cobol_response)
                VALUES 
                ('$id', '$now', '$now', '$type', 
                 '$reference', '$status', '$response')";
        
        $db->query($sql);
    }
    
    /**
     * Store system health metrics
     */
    private function storeHealthMetrics($health, $responseTime)
    {
        global $db;
        
        $id = create_guid();
        $now = date('Y-m-d H:i:s');
        $status = isset($health['status']) ? $health['status'] : 'error';
        
        $sql = "INSERT INTO cobol_health_metrics 
                (id, date_entered, response_time_ms, status)
                VALUES 
                ('$id', '$now', $responseTime, '$status')";
        
        $db->query($sql);
    }
    
    /**
     * Get average response time for last hour
     */
    public function getAverageResponseTime()
    {
        global $db;
        
        $oneHourAgo = date('Y-m-d H:i:s', strtotime('-1 hour'));
        
        $sql = "SELECT AVG(response_time_ms) as avg_time 
                FROM cobol_health_metrics 
                WHERE date_entered > '$oneHourAgo'";
        
        $result = $db->query($sql);
        $row = $db->fetchByAssoc($result);
        
        return $row ? round($row['avg_time']) : 0;
    }
    
    /**
     * Get system availability percentage
     */
    public function getSystemAvailability()
    {
        global $db;
        
        $oneDayAgo = date('Y-m-d H:i:s', strtotime('-24 hours'));
        
        $sql = "SELECT 
                COUNT(*) as total,
                SUM(CASE WHEN status = 'healthy' THEN 1 ELSE 0 END) as healthy
                FROM cobol_health_metrics 
                WHERE date_entered > '$oneDayAgo'";
        
        $result = $db->query($sql);
        $row = $db->fetchByAssoc($result);
        
        if ($row && $row['total'] > 0) {
            return round(($row['healthy'] / $row['total']) * 100, 2);
        }
        
        return 0;
    }
}