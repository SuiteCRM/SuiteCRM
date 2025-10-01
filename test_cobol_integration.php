#!/usr/bin/env php
<?php
/**
 * COBOL Integration Test Script
 * Tests all COBOL payment processing components
 */

echo "=== COBOL Credit Card Processing Integration Test ===\n\n";

// Test configuration
$tests = array(
    'gateway_health' => true,
    'websocket' => true,
    'card_validation' => true,
    'payment_processing' => true,
    'suitecrm_integration' => true
);

$results = array();

// 1. Test Gateway Health
if ($tests['gateway_health']) {
    echo "1. Testing COBOL Gateway Health...\n";
    // Try mock service first
    $healthUrl = 'http://localhost:8090/health';
    $health = @file_get_contents($healthUrl);
    
    if (!$health) {
        // Try actual gateway
        $healthUrl = 'http://localhost:8080/health';
        $health = @file_get_contents($healthUrl);
    }
    
    if ($health) {
        $healthData = json_decode($health, true);
        if ($healthData['status'] === 'online') {
            $results['gateway_health'] = '✅ PASS - Gateway is online';
            echo "   ✅ Gateway Status: " . $healthData['status'] . "\n";
            echo "   ✅ COBOL Runtime: " . $healthData['cobol_runtime'] . "\n";
            echo "   ✅ Version: " . $healthData['version'] . "\n";
        } else {
            $results['gateway_health'] = '❌ FAIL - Gateway offline';
        }
    } else {
        $results['gateway_health'] = '❌ FAIL - Cannot connect to gateway';
    }
    echo "\n";
}

// 2. Test WebSocket Connection
if ($tests['websocket']) {
    echo "2. Testing WebSocket Connection...\n";
    $wsPort = 8081;
    $socket = @fsockopen('localhost', $wsPort, $errno, $errstr, 5);
    
    if ($socket) {
        $results['websocket'] = '✅ PASS - WebSocket port is open';
        echo "   ✅ WebSocket port $wsPort is accessible\n";
        fclose($socket);
    } else {
        $results['websocket'] = '❌ FAIL - WebSocket port not accessible';
        echo "   ❌ WebSocket connection failed: $errstr\n";
    }
    echo "\n";
}

// 3. Test Card Validation
if ($tests['card_validation']) {
    echo "3. Testing Credit Card Validation...\n";
    
    // Test valid card
    $validCard = array(
        'number' => '4111111111111111',
        'expiry' => '12/25',
        'cvv' => '123',
        'amount' => 5000.00,
        'property_id' => 'TEST_PROP_001'
    );
    
    echo "   Testing valid card (4111111111111111)...\n";
    $response = testCardValidation($validCard);
    
    if ($response && isset($response['status'])) {
        if ($response['status'] === 'APPROVED' || $response['status_code'] === '00') {
            $results['card_validation_valid'] = '✅ PASS - Valid card approved';
            echo "   ✅ Valid card approved\n";
        } else {
            $results['card_validation_valid'] = '❌ FAIL - Valid card rejected: ' . $response['message'];
            echo "   ❌ Valid card rejected: " . $response['message'] . "\n";
        }
    } else {
        $results['card_validation_valid'] = '❌ FAIL - No response from COBOL';
        echo "   ❌ No response from COBOL service\n";
    }
    
    // Test invalid card
    $invalidCard = array(
        'number' => '4111111111111112', // Invalid Luhn
        'expiry' => '12/25',
        'cvv' => '123',
        'amount' => 5000.00,
        'property_id' => 'TEST_PROP_001'
    );
    
    echo "   Testing invalid card (4111111111111112)...\n";
    $response = testCardValidation($invalidCard);
    
    if ($response && isset($response['status'])) {
        if ($response['status'] !== 'APPROVED' && $response['status_code'] !== '00') {
            $results['card_validation_invalid'] = '✅ PASS - Invalid card rejected';
            echo "   ✅ Invalid card correctly rejected\n";
        } else {
            $results['card_validation_invalid'] = '❌ FAIL - Invalid card was approved';
            echo "   ❌ Invalid card was incorrectly approved\n";
        }
    } else {
        $results['card_validation_invalid'] = '❌ FAIL - No response from COBOL';
        echo "   ❌ No response from COBOL service\n";
    }
    echo "\n";
}

// 4. Test Payment Processing
if ($tests['payment_processing']) {
    echo "4. Testing Payment Processing...\n";
    
    $paymentData = array(
        'type' => 'EARNEST',
        'amount' => 5000.00,
        'method' => 'CARD',
        'account' => 'ACC_TEST_001',
        'reference' => 'PROP_TEST_001'
    );
    
    $formatted = implode('|', array(
        $paymentData['type'],
        $paymentData['amount'],
        $paymentData['method'],
        $paymentData['account'],
        $paymentData['reference']
    ));
    
    // Try mock service on port 8090
    $ch = curl_init('http://localhost:8090/cobol/PROCESS-PAYMENT');
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_POSTFIELDS, $formatted);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: text/plain'));
    
    $response = curl_exec($ch);
    $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);
    
    if ($httpCode === 200 && $response) {
        $results['payment_processing'] = '✅ PASS - Payment processed';
        echo "   ✅ Payment processing endpoint accessible\n";
    } else {
        $results['payment_processing'] = '❌ FAIL - Payment processing failed';
        echo "   ❌ Payment processing failed (HTTP $httpCode)\n";
    }
    echo "\n";
}

// 5. Test SuiteCRM Integration
if ($tests['suitecrm_integration']) {
    echo "5. Testing SuiteCRM Integration...\n";
    
    // Check if COBOLPaymentService exists
    $serviceFile = __DIR__ . '/modules/COBOLIntegration/services/COBOLPaymentService.php';
    if (file_exists($serviceFile)) {
        $results['suitecrm_service'] = '✅ PASS - COBOLPaymentService exists';
        echo "   ✅ COBOLPaymentService.php found\n";
    } else {
        $results['suitecrm_service'] = '❌ FAIL - COBOLPaymentService missing';
        echo "   ❌ COBOLPaymentService.php not found\n";
    }
    
    // Check payment form in mobile view
    $mobileViewFile = __DIR__ . '/modules/RE_Properties/views/view.mobile.php';
    if (file_exists($mobileViewFile)) {
        $content = file_get_contents($mobileViewFile);
        if (strpos($content, 'paymentForm') !== false && strpos($content, 'processPayment') !== false) {
            $results['payment_form'] = '✅ PASS - Payment form implemented';
            echo "   ✅ Payment form found in mobile view\n";
        } else {
            $results['payment_form'] = '❌ FAIL - Payment form not found';
            echo "   ❌ Payment form missing from mobile view\n";
        }
    } else {
        $results['payment_form'] = '❌ FAIL - Mobile view file missing';
        echo "   ❌ Mobile view file not found\n";
    }
    
    // Check ProcessPayment action
    $actionFile = __DIR__ . '/modules/RE_Properties/action_process_payment.php';
    if (file_exists($actionFile)) {
        $results['process_action'] = '✅ PASS - ProcessPayment action exists';
        echo "   ✅ ProcessPayment action found\n";
    } else {
        $results['process_action'] = '❌ FAIL - ProcessPayment action missing';
        echo "   ❌ ProcessPayment action not found\n";
    }
    echo "\n";
}

// Summary
echo "=== Test Summary ===\n\n";
$passed = 0;
$failed = 0;

foreach ($results as $test => $result) {
    echo str_pad($test, 30) . ": " . $result . "\n";
    if (strpos($result, '✅') !== false) $passed++;
    else $failed++;
}

echo "\nTotal Tests: " . count($results) . "\n";
echo "Passed: $passed\n";
echo "Failed: $failed\n";

// Risk Assessment
echo "\n=== Risk Assessment ===\n";
if ($failed === 0) {
    echo "Risk Level: LOW ✅\n";
    echo "All tests passed. System is ready for demo.\n";
} elseif ($failed <= 2) {
    echo "Risk Level: MEDIUM ⚠️\n";
    echo "Some non-critical tests failed. Demo possible with workarounds.\n";
} else {
    echo "Risk Level: HIGH ❌\n";
    echo "Critical components are not working. Demo not recommended.\n";
}

// Helper function
function testCardValidation($cardData) {
    $formatted = str_pad($cardData['number'], 19, ' ', STR_PAD_RIGHT);
    $formatted .= str_replace('/', '', $cardData['expiry']);
    $formatted .= str_pad($cardData['cvv'], 3, '0', STR_PAD_LEFT);
    $formatted .= str_pad(intval($cardData['amount'] * 100), 10, '0', STR_PAD_LEFT);
    $formatted .= str_pad('EARNEST', 10, ' ', STR_PAD_RIGHT);
    $formatted .= str_pad($cardData['property_id'], 20, ' ', STR_PAD_RIGHT);
    $formatted .= date('YmdHis');
    
    // Try mock service on port 8090
    $ch = curl_init('http://localhost:8090/cobol/VALIDATE-CARD');
    curl_setopt($ch, CURLOPT_POST, 1);
    curl_setopt($ch, CURLOPT_POSTFIELDS, $formatted);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: text/plain'));
    
    $response = curl_exec($ch);
    $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    curl_close($ch);
    
    if ($httpCode === 200 && $response) {
        // Try to parse as JSON first
        $json = json_decode($response, true);
        if ($json) return $json;
        
        // Parse fixed-width COBOL response
        return array(
            'status_code' => substr($response, 0, 2),
            'status' => mapStatus(substr($response, 0, 2)),
            'transaction_id' => trim(substr($response, 2, 20)),
            'auth_code' => trim(substr($response, 22, 6)),
            'message' => trim(substr($response, 28, 50))
        );
    }
    
    return null;
}

function mapStatus($code) {
    $map = array(
        '00' => 'APPROVED',
        '01' => 'DECLINED',
        '03' => 'INVALID_CARD',
        '07' => 'SYSTEM_ERROR'
    );
    return $map[$code] ?? 'UNKNOWN';
}