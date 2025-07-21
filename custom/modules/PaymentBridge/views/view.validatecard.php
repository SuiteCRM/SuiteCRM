<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/View/SugarView.php');

class PaymentBridgeViewValidateCard extends SugarView
{
    public function display()
    {
        global $mod_strings, $app_strings;
        
        // Get account if passed
        $accountId = $_REQUEST['account_id'] ?? '';
        $accountName = '';
        
        if (!empty($accountId)) {
            $account = BeanFactory::getBean('Accounts', $accountId);
            if ($account) {
                $accountName = $account->name;
            }
        }
        
        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        $this->ss->assign('ACCOUNT_ID', $accountId);
        $this->ss->assign('ACCOUNT_NAME', $accountName);
        
        echo $this->ss->fetch('custom/modules/PaymentBridge/tpls/ValidateCard.tpl');
    }
}