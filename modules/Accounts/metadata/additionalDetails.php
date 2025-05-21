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

 


function additionalDetailsAccount($fields)
{
    static $mod_strings;
    global $app_strings;
    if (empty($mod_strings)) {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'Accounts');
    }
    $overlib_string = '';
    
    if (!empty($fields['BILLING_ADDRESS_STREET']) || !empty($fields['BILLING_ADDRESS_CITY']) ||
        !empty($fields['BILLING_ADDRESS_STATE']) || !empty($fields['BILLING_ADDRESS_POSTALCODE']) ||
        !empty($fields['BILLING_ADDRESS_COUNTRY'])) {
        $overlib_string .= '<b>' . $mod_strings['LBL_BILLING_ADDRESS'] . '</b><br>';
    }
    if (!empty($fields['BILLING_ADDRESS_STREET'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_STREET'] . '<br>';
    }
    if (!empty($fields['BILLING_ADDRESS_STREET_2'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_STREET_2'] . '<br>';
    }
    if (!empty($fields['BILLING_ADDRESS_STREET_3'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_STREET_3'] . '<br>';
    }
    if (!empty($fields['BILLING_ADDRESS_STREET_4'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_STREET_4'] . '<br>';
    }
    if (!empty($fields['BILLING_ADDRESS_CITY'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_CITY'] . ', ';
    }
    if (!empty($fields['BILLING_ADDRESS_STATE'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_STATE'] . ' ';
    }
    if (!empty($fields['BILLING_ADDRESS_POSTALCODE'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_POSTALCODE'] . ' ';
    }
    if (!empty($fields['BILLING_ADDRESS_COUNTRY'])) {
        $overlib_string .= $fields['BILLING_ADDRESS_COUNTRY'] . '<br>';
    }
    
    if (strlen($overlib_string) > 0 && !(strrpos($overlib_string, '<br>') == strlen($overlib_string) - 4)) {
        $overlib_string .= '<br>';
    }
    
    if (!empty($fields['PHONE_FAX'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_FAX'] . '</b> <span class="phone">' . $fields['PHONE_FAX'] . '</span><br>';
    }
    if (!empty($fields['PHONE_ALTERNATE'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_OTHER_PHONE'] . '</b> <span class="phone">' . $fields['PHONE_ALTERNATE'] . '</span><br>';
    }
    if (!empty($fields['WEBSITE'])) {
        $overlib_string .= '<a target=_blank href='. $fields['WEBSITE'] . '>' . $fields['WEBSITE'] . '</a><br>';
    }
    if (!empty($fields['INDUSTRY'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_INDUSTRY'] . '</b> ' . $fields['INDUSTRY'] . '<br>';
    }
    if (!empty($fields['DESCRIPTION'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_DESCRIPTION'] . '</b> ' . substr((string) $fields['DESCRIPTION'], 0, 300);
        if (strlen((string) $fields['DESCRIPTION']) > 300) {
            $overlib_string .= '...';
        }
    }

    return array('fieldToAddTo' => 'NAME',
                 'string' => $overlib_string,
                 'editLink' => "index.php?action=EditView&module=Accounts&return_module=Accounts&record={$fields['ID']}",
                 'viewLink' => "index.php?action=DetailView&module=Accounts&return_module=Accounts&record={$fields['ID']}");
}
