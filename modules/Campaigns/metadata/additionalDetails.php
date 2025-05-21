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


function additionalDetailsCampaign($fields)
{
    static $mod_strings;
    if (empty($mod_strings)) {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'Campaigns');
    }
        
    $overlib_string = '';
    
    if (!empty($fields['START_DATE'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_CAMPAIGN_START_DATE'] . '</b> ' . $fields['START_DATE'] . '<br>';
    }

    if (!empty($fields['TRACKER_TEXT'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_TRACKER_TEXT'] . '</b> ' . $fields['TRACKER_TEXT'] . '<br>';
    }
    if (!empty($fields['REFER_URL'])) {
        $overlib_string .= '<a target=_blank href='. $fields['REFER_URL'] . '>' . $fields['REFER_URL'] . '</a><br>';
    }
    
    if (!empty($fields['OBJECTIVE'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_CAMPAIGN_OBJECTIVE'] . '</b> ' . substr((string) $fields['OBJECTIVE'], 0, 300);
        if (strlen((string) $fields['OBJECTIVE']) > 300) {
            $overlib_string .= '...';
        }
        $overlib_string .= '<br>';
    }
    if (!empty($fields['CONTENT'])) {
        $overlib_string .= '<b>'. $mod_strings['LBL_CAMPAIGN_CONTENT'] . '</b> ' . substr((string) $fields['CONTENT'], 0, 300);
        if (strlen((string) $fields['CONTENT']) > 300) {
            $overlib_string .= '...';
        }
    }
    
    return array('fieldToAddTo' => 'NAME',
                 'string' => $overlib_string,
                 'editLink' => "index.php?action=EditView&module=Campaigns&return_module=Campaigns&record={$fields['ID']}",
                 'viewLink' => "index.php?action=DetailView&module=Campaigns&return_module=Campaigns&record={$fields['ID']}");
}
