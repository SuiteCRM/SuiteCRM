<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/generic/SugarWidgets/SugarWidgetSubPanelTopButton.php');

#[\AllowDynamicProperties]
class SugarWidgetSubPanelManageAcceptancesButton extends SugarWidgetSubPanelTopButton
{
    public function display($defines, $additionalFormFields = null, $nonbutton = false)
    {
        global $mod_strings;
        
        $button  = "<form id='ManageAcceptancesForm' name='ManageAcceptancesForm' method='post' action=''>";
        // $button .= "<input id='custom_hidden_5' type='hidden' name='custom_hidden_5' value=''/>";
        $button .= "<input id='Manage_Acceptances' class='button' type='button' name='Manage_Acceptances' onclick='manage_acceptances();' value='".$mod_strings['LBL_MANAGE_ACCEPTANCES']."'/>\n</form>";
        return $button;
    }
}
