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




require_once('modules/Campaigns/utils.php');

if (!empty($_REQUEST['remove'])) {
    clean_string($_REQUEST['remove'], "STANDARD");
}
if (!empty($_REQUEST['from'])) {
    clean_string($_REQUEST['from'], "STANDARD");
}

if (!empty($_REQUEST['identifier'])) {
    global $beanFiles, $beanList, $current_user;

    //user is most likely not defined, retrieve admin user so that team queries are bypassed
    if (empty($current_user) || empty($current_user->id)) {
        $current_user = BeanFactory::newBean('Users');
        $current_user->retrieve('1');
    }
    
    $keys=log_campaign_activity($_REQUEST['identifier'], 'removed');
    global $current_language;
    $mod_strings = return_module_language($current_language, 'Campaigns');

    
    if (!empty($keys) && $keys['target_type'] == 'Users') {
        //Users cannot opt out of receiving emails, print out warning message.
        echo $mod_strings['LBL_USERS_CANNOT_OPTOUT'];
    } elseif (!empty($keys) && isset($keys['campaign_id']) && !empty($keys['campaign_id'])) {
        //we need to unsubscribe the user from this particular campaign
        $beantype = $beanList[$keys['target_type']];
        require_once($beanFiles[$beantype]);
        $focus = new $beantype();
        $focus->retrieve($keys['target_id']);
        unsubscribe($keys['campaign_id'], $focus);
    } elseif (!empty($keys)) {
        $id = $keys['target_id'];
        $module = trim($keys['target_type']);
        $class = $beanList[$module];
        require_once($beanFiles[$class]);
        $mod = new $class();
        $db = DBManagerFactory::getInstance();

        $id = $db->quote($id);

        //no opt out for users.
        if (preg_match('/^[0-9A-Za-z\-]*$/', (string) $id) && $module != 'Users') {
            //record this activity in the campaing log table..
            $query = "UPDATE email_addresses SET email_addresses.opt_out = 1 WHERE EXISTS(SELECT 1 FROM email_addr_bean_rel ear WHERE ear.bean_id = '$id' AND ear.deleted=0 AND email_addresses.id = ear.email_address_id)";
            $status=$db->query($query);
            if ($status) {
                echo "*";
            }
        }
    }
    //Print Confirmation Message.
    echo $mod_strings['LBL_ELECTED_TO_OPTOUT'];
}
sugar_cleanup();
