<?php
/**
 * Process Payment Action for RE_Properties
 * Handles COBOL payment processing via AJAX
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('modules/COBOLIntegration/services/COBOLPaymentService.php');

class RE_PropertiesActionProcessPayment extends SugarController
{
    public function action_ProcessPayment()
    {
        global $current_user;
        
        // Check permissions
        if (!$current_user->id) {
            $this->returnError('Authentication required');
            return;
        }
        
        // Get payment data
        $cardNumber = $_POST['number'] ?? '';
        $expiry = $_POST['expiry'] ?? '';
        $cvv = $_POST['cvv'] ?? '';
        $propertyId = $_POST['property_id'] ?? '';
        $amount = $_POST['amount'] ?? 0;
        
        // Basic validation
        if (empty($cardNumber) || empty($expiry) || empty($cvv) || empty($propertyId)) {
            $this->returnError('Missing required payment information');
            return;
        }
        
        // Parse expiry date
        $expiryParts = explode('/', $expiry);
        if (count($expiryParts) !== 2) {
            $this->returnError('Invalid expiry date format');
            return;
        }
        
        try {
            // Initialize COBOL payment service
            $cobolService = new COBOLPaymentService();
            
            // Prepare payment data
            $paymentData = array(
                'card_number' => $cardNumber,
                'expiry_month' => intval($expiryParts[0]),
                'expiry_year' => intval($expiryParts[1]),
                'cvv' => $cvv,
                'amount' => floatval($amount),
                'transaction_type' => 'EARNEST',
                'property_id' => $propertyId
            );
            
            // Validate card first
            $validationResult = $cobolService->validateCard($paymentData);
            
            if ($validationResult['status'] === 'APPROVED') {
                // Process the payment
                $paymentResult = $cobolService->processPayment($paymentData);
                
                if ($paymentResult['status'] === 'APPROVED') {
                    // Update property record
                    $property = BeanFactory::getBean('RE_Properties', $propertyId);
                    if ($property && $property->id) {
                        $property->earnest_money_status = 'paid';
                        $property->earnest_money_date = date('Y-m-d');
                        $property->earnest_transaction_id = $paymentResult['transaction_id'];
                        $property->save();
                    }
                    
                    // Return success response
                    $this->returnSuccess($paymentResult);
                } else {
                    $this->returnError($paymentResult['message'], $paymentResult);
                }
            } else {
                $this->returnError($validationResult['message'], $validationResult);
            }
            
        } catch (Exception $e) {
            $GLOBALS['log']->error('COBOL Payment Processing Error: ' . $e->getMessage());
            $this->returnError('Payment processing failed: ' . $e->getMessage());
        }
    }
    
    private function returnSuccess($data)
    {
        ob_clean();
        header('Content-Type: application/json');
        echo json_encode(array_merge(array('success' => true), $data));
        exit;
    }
    
    private function returnError($message, $additionalData = array())
    {
        ob_clean();
        header('Content-Type: application/json');
        echo json_encode(array_merge(
            array(
                'success' => false,
                'status' => 'ERROR',
                'message' => $message
            ),
            $additionalData
        ));
        exit;
    }
}