<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once "include/utils/additional_details.php";

function additionalDetailsstic_Journal($fields, ?SugarBean $bean = null, $params = array())
{

    if (file_exists('custom/modules/' . $bean->module_name . '/metadata/customAdditionalDetails.php')) {
        $additionalDetailsFile = 'custom/modules/' . $bean->module_name . '/metadata/customAdditionalDetails.php';
        require_once($additionalDetailsFile);
        
        $mod_strings = return_module_language($current_language, $bean->module_name);
        return customAdditionalDetails::customAdditionalDetailsstic_Journal($fields, $bean, $mod_strings);
    
    } else {
        global $current_language, $app_list_strings;

        // Get the list to show the content
        $fields['TYPE_TASK'] = $app_list_strings['stic_journal_types_list']['task'];
        $fields['TYPE_INFRINGEMENT'] = $app_list_strings['stic_journal_types_list']['infringement'];
        $fields['TYPE_EDUCATIONAL_MEASURE'] = $app_list_strings['stic_journal_types_list']['educational_measure'];

        $mod_strings = return_module_language($current_language, $bean->module_name);
        return additional_details($fields, $bean, $mod_strings);
    }    
}
