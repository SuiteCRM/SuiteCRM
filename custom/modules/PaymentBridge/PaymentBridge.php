<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/SugarObjects/templates/basic/Basic.php');

/**
 * PaymentBridge module for integrating COBOL payment systems with SuiteCRM
 * Designed for insurance and financial services companies
 */
class PaymentBridge extends Basic
{
    public $module_dir = 'PaymentBridge';
    public $object_name = 'PaymentBridge';
    public $table_name = 'payment_bridge';
    public $new_schema = true;
    public $module_name = 'PaymentBridge';
    
    // Payment specific fields
    public $card_number_masked;
    public $validation_status;
    public $credit_limit;
    public $current_balance;
    public $apr;
    public $last_statement_date;
    public $payment_gateway_status;
    public $cobol_system_health;
    public $last_validation_date;
    public $interest_charge;
    public $minimum_payment;
    public $payment_plan_status;
    
    public $account_id;
    public $account_name;
    
    public function __construct()
    {
        parent::__construct();
        $this->setupCustomFields();
    }
    
    /**
     * Validate credit card using COBOL API
     */
    public function validateCard($cardNumber)
    {
        require_once('custom/modules/PaymentBridge/CobolApiService.php');
        $apiService = new CobolApiService();
        
        try {
            $result = $apiService->validateCard($cardNumber);
            
            // Store validation result
            $this->card_number_masked = $result['cardNumber'];
            $this->validation_status = $result['valid'] ? 'Valid' : 'Invalid';
            $this->last_validation_date = date('Y-m-d H:i:s');
            
            return $result;
        } catch (Exception $e) {
            $GLOBALS['log']->error("PaymentBridge: Card validation failed - " . $e->getMessage());
            return array('valid' => false, 'error' => $e->getMessage());
        }
    }
    
    /**
     * Calculate interest based on current balance
     */
    public function calculateInterest($balance = null)
    {
        require_once('custom/modules/PaymentBridge/CobolApiService.php');
        $apiService = new CobolApiService();
        
        if ($balance === null) {
            $balance = $this->current_balance;
        }
        
        try {
            $result = $apiService->calculateInterest($this->card_number_masked, $balance);
            
            // Store calculation results
            $this->interest_charge = $result['interestCharge'];
            $this->apr = $result['apr'];
            $this->current_balance = $result['newBalance'];
            
            return $result;
        } catch (Exception $e) {
            $GLOBALS['log']->error("PaymentBridge: Interest calculation failed - " . $e->getMessage());
            return array('error' => $e->getMessage());
        }
    }
    
    /**
     * Generate statement for account
     */
    public function generateStatement($format = 'json')
    {
        require_once('custom/modules/PaymentBridge/CobolApiService.php');
        $apiService = new CobolApiService();
        
        try {
            $result = $apiService->generateStatement($this->card_number_masked, $format);
            
            // Update last statement date
            $this->last_statement_date = date('Y-m-d');
            $this->save();
            
            return $result;
        } catch (Exception $e) {
            $GLOBALS['log']->error("PaymentBridge: Statement generation failed - " . $e->getMessage());
            return array('error' => $e->getMessage());
        }
    }
    
    /**
     * Check COBOL system health
     */
    public function checkSystemHealth()
    {
        require_once('custom/modules/PaymentBridge/CobolApiService.php');
        $apiService = new CobolApiService();
        
        try {
            $health = $apiService->getSystemHealth();
            $this->cobol_system_health = json_encode($health);
            return $health;
        } catch (Exception $e) {
            $GLOBALS['log']->error("PaymentBridge: System health check failed - " . $e->getMessage());
            return array('status' => 'error', 'message' => $e->getMessage());
        }
    }
    
    /**
     * Get payment plan options
     */
    public function getPaymentPlanOptions($amount)
    {
        $plans = array();
        
        // 3-month plan
        $plans[] = array(
            'months' => 3,
            'monthlyPayment' => round($amount / 3 * 1.02, 2), // 2% fee
            'totalCost' => round($amount * 1.02, 2),
            'interestRate' => 2
        );
        
        // 6-month plan
        $plans[] = array(
            'months' => 6,
            'monthlyPayment' => round($amount / 6 * 1.05, 2), // 5% fee
            'totalCost' => round($amount * 1.05, 2),
            'interestRate' => 5
        );
        
        // 12-month plan
        $plans[] = array(
            'months' => 12,
            'monthlyPayment' => round($amount / 12 * 1.10, 2), // 10% fee
            'totalCost' => round($amount * 1.10, 2),
            'interestRate' => 10
        );
        
        return $plans;
    }
    
    /**
     * Setup custom fields for module
     */
    private function setupCustomFields()
    {
        $this->field_name_map['card_number_masked'] = array(
            'name' => 'card_number_masked',
            'vname' => 'LBL_CARD_NUMBER_MASKED',
            'type' => 'varchar',
            'len' => 20,
            'required' => false,
            'audited' => true
        );
        
        $this->field_name_map['credit_limit'] = array(
            'name' => 'credit_limit',
            'vname' => 'LBL_CREDIT_LIMIT',
            'type' => 'currency',
            'required' => false,
            'audited' => true
        );
        
        $this->field_name_map['current_balance'] = array(
            'name' => 'current_balance',
            'vname' => 'LBL_CURRENT_BALANCE',
            'type' => 'currency',
            'required' => false,
            'audited' => true
        );
        
        $this->field_name_map['apr'] = array(
            'name' => 'apr',
            'vname' => 'LBL_APR',
            'type' => 'decimal',
            'len' => '5,2',
            'required' => false
        );
        
        $this->field_name_map['validation_status'] = array(
            'name' => 'validation_status',
            'vname' => 'LBL_VALIDATION_STATUS',
            'type' => 'enum',
            'options' => 'payment_validation_status_list',
            'required' => false
        );
    }
}