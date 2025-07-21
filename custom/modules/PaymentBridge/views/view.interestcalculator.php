<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/View/SugarView.php');

class PaymentBridgeViewInterestCalculator extends SugarView
{
    public function display()
    {
        global $mod_strings, $app_strings, $db;
        
        // Get recent payment records for dropdown
        $sql = "SELECT id, name, card_number_masked, current_balance, apr 
                FROM payment_bridge 
                WHERE deleted = 0 
                ORDER BY date_modified DESC 
                LIMIT 20";
        
        $result = $db->query($sql);
        $recentCards = array();
        
        while ($row = $db->fetchByAssoc($result)) {
            $recentCards[] = $row;
        }
        
        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        $this->ss->assign('RECENT_CARDS', $recentCards);
        
        echo $this->ss->fetch('custom/modules/PaymentBridge/tpls/InterestCalculator.tpl');
    }
}