<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */



#[\AllowDynamicProperties]
class EmailManViewList extends ViewList
{
    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;
        
        if (!is_admin($current_user) && !is_admin_for_module($current_user, 'Campaigns')) {
            sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
        }
        
        $this->lv = new ListViewSmarty();
        $this->lv->export = false;
        $this->lv->quickViewLinks = false;
    }
    
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;
        
        return array(
           "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
           translate('LBL_MASS_EMAIL_MANAGER_TITLE', 'Administration'),
           );
    }
    
    
    public function listViewPrepare()
    {
        $this->options['show_title'] = false;
        parent::listViewPrepare();
        echo $this->getModuleTitle(false);
    }
    /**
     * @see ViewList::listViewProcess()
     */
    public function listViewProcess()
    {
        parent::listViewProcess();
        
        global $app_strings;
        
        echo "<form action=\"index.php\" method=\"post\" name=\"EmailManDelivery\" id=\"form\">
			<table width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" class='actionsContainer'>
				<tr><td style=\"padding-bottom: 2px;\">
                        <input type=\"hidden\" name=\"module\" value=\"EmailMan\">
                        <input type=\"hidden\" name=\"action\">
                        <input type=\"hidden\" name=\"return_module\">
                        <input type=\"hidden\" name=\"return_action\">
                        <input type=\"hidden\" name=\"manual\" value=\"true\">
                        <input	title=\"".$app_strings['LBL_CAMPAIGNS_SEND_QUEUED']."\" 
                                accessKey=\"".$app_strings['LBL_SAVE_BUTTON_KEY']."\" class=\"button\" 
                                onclick=\"this.form.return_module.value='EmailMan'; this.form.return_action.value='index'; this.form.action.value='EmailManDelivery'\" 
                                type=\"submit\" name=\"Send\" value=\"".$app_strings['LBL_CAMPAIGNS_SEND_QUEUED']."\">
            </td></tr></table></form>";
    }
}
