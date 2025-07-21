<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/Controller/SugarController.php');

class PaymentBridgeController extends SugarController
{
    /**
     * Action to display card validation form
     */
    public function action_ValidateCard()
    {
        $this->view = 'validatecard';
    }
    
    /**
     * AJAX action to validate card
     */
    public function action_AjaxValidateCard()
    {
        global $current_user;
        
        $cardNumber = $_REQUEST['card_number'] ?? '';
        $accountId = $_REQUEST['account_id'] ?? '';
        
        if (empty($cardNumber)) {
            echo json_encode(array('success' => false, 'error' => 'Card number required'));
            sugar_die();
        }
        
        // Create payment bridge record
        require_once('custom/modules/PaymentBridge/PaymentBridge.php');
        $paymentBridge = new PaymentBridge();
        
        if (!empty($accountId)) {
            $paymentBridge->account_id = $accountId;
        }
        
        try {
            $result = $paymentBridge->validateCard($cardNumber);
            $paymentBridge->name = 'Card Validation - ' . date('Y-m-d H:i:s');
            $paymentBridge->assigned_user_id = $current_user->id;
            $paymentBridge->save();
            
            echo json_encode(array(
                'success' => true,
                'data' => $result,
                'record_id' => $paymentBridge->id
            ));
        } catch (Exception $e) {
            echo json_encode(array(
                'success' => false,
                'error' => $e->getMessage()
            ));
        }
        
        sugar_die();
    }
    
    /**
     * Interest calculator action
     */
    public function action_InterestCalculator()
    {
        $this->view = 'interestcalculator';
    }
    
    /**
     * AJAX action to calculate interest
     */
    public function action_AjaxCalculateInterest()
    {
        $cardNumber = $_REQUEST['card_number'] ?? '';
        $balance = $_REQUEST['balance'] ?? null;
        $recordId = $_REQUEST['record_id'] ?? '';
        
        require_once('custom/modules/PaymentBridge/PaymentBridge.php');
        
        if (!empty($recordId)) {
            $paymentBridge = BeanFactory::getBean('PaymentBridge', $recordId);
        } else {
            $paymentBridge = new PaymentBridge();
            $paymentBridge->card_number_masked = $cardNumber;
        }
        
        try {
            $result = $paymentBridge->calculateInterest($balance);
            $paymentBridge->save();
            
            echo json_encode(array(
                'success' => true,
                'data' => $result,
                'record_id' => $paymentBridge->id
            ));
        } catch (Exception $e) {
            echo json_encode(array(
                'success' => false,
                'error' => $e->getMessage()
            ));
        }
        
        sugar_die();
    }
    
    /**
     * Statement generation action
     */
    public function action_GenerateStatement()
    {
        $recordId = $_REQUEST['record'] ?? '';
        $format = $_REQUEST['format'] ?? 'json';
        
        if (empty($recordId)) {
            SugarApplication::appendErrorMessage('No payment record specified');
            SugarApplication::redirect('index.php?module=PaymentBridge');
        }
        
        $paymentBridge = BeanFactory::getBean('PaymentBridge', $recordId);
        
        try {
            $statement = $paymentBridge->generateStatement($format);
            
            if ($format === 'json') {
                header('Content-Type: application/json');
                echo json_encode($statement);
            } else {
                // Handle PDF or other formats
                header('Content-Type: application/pdf');
                header('Content-Disposition: attachment; filename="statement.pdf"');
                echo $statement;
            }
            
            sugar_die();
        } catch (Exception $e) {
            SugarApplication::appendErrorMessage('Statement generation failed: ' . $e->getMessage());
            SugarApplication::redirect('index.php?module=PaymentBridge&action=DetailView&record=' . $recordId);
        }
    }
    
    /**
     * System health dashboard
     */
    public function action_SystemHealth()
    {
        $this->view = 'systemhealth';
    }
    
    /**
     * AJAX action to get system health
     */
    public function action_AjaxSystemHealth()
    {
        require_once('custom/modules/PaymentBridge/CobolApiService.php');
        $apiService = new CobolApiService();
        
        try {
            $health = $apiService->getSystemHealth();
            $avgResponseTime = $apiService->getAverageResponseTime();
            $availability = $apiService->getSystemAvailability();
            
            echo json_encode(array(
                'success' => true,
                'data' => array(
                    'health' => $health,
                    'avgResponseTime' => $avgResponseTime,
                    'availability' => $availability
                )
            ));
        } catch (Exception $e) {
            echo json_encode(array(
                'success' => false,
                'error' => $e->getMessage()
            ));
        }
        
        sugar_die();
    }
    
    /**
     * Payment dashboard
     */
    public function action_PaymentDashboard()
    {
        $this->view = 'paymentdashboard';
    }
    
    /**
     * Create payment plan
     */
    public function action_CreatePaymentPlan()
    {
        $recordId = $_REQUEST['record_id'] ?? '';
        $months = $_REQUEST['months'] ?? 6;
        $amount = $_REQUEST['amount'] ?? 0;
        
        require_once('custom/modules/PaymentBridge/PaymentBridge.php');
        $paymentBridge = BeanFactory::getBean('PaymentBridge', $recordId);
        
        require_once('custom/modules/PaymentBridge/CobolApiService.php');
        $apiService = new CobolApiService();
        
        try {
            $plan = $apiService->createPaymentPlan(
                $paymentBridge->card_number_masked,
                $amount,
                $months
            );
            
            // Save payment plan
            global $db, $current_user;
            $planId = create_guid();
            $now = date('Y-m-d H:i:s');
            
            $sql = "INSERT INTO payment_plans 
                    (id, date_entered, date_modified, created_by,
                     payment_bridge_id, account_id, total_amount,
                     installment_count, monthly_payment, interest_rate,
                     start_date, end_date, status, remaining_balance)
                    VALUES 
                    ('$planId', '$now', '$now', '{$current_user->id}',
                     '{$paymentBridge->id}', '{$paymentBridge->account_id}', {$plan['totalAmount']},
                     {$plan['months']}, {$plan['monthlyPayment']}, {$plan['interestRate']},
                     '{$plan['startDate']}', '{$plan['endDate']}', 'Active', {$plan['totalAmount']})";
            
            $db->query($sql);
            
            echo json_encode(array(
                'success' => true,
                'data' => $plan,
                'plan_id' => $planId
            ));
        } catch (Exception $e) {
            echo json_encode(array(
                'success' => false,
                'error' => $e->getMessage()
            ));
        }
        
        sugar_die();
    }
}