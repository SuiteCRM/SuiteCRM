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

function additionalDetailsTracker($fields, ?SugarBean $bean = null, $params = array())
{
    global $app_list_strings;
    // Virtual field to take the internal value of module_name
    $fields['MODULE_CODE'] = array_search($bean->module_name, $app_list_strings['moduleList']);

    // If the item_id is not empty, create the url with the item_id and the module_name
    if (!empty($fields['ITEM_ID'])) {
        $fields['LINK_URL'] = 'index.php?action=DetailView&module=' . $fields['MODULE_CODE']. '&record=' . $fields['ITEM_ID'];
        $fields['LINK_TEXT'] = $fields['ITEM_SUMMARY'];

        // Remove the url when the record is deleted
        if(array_search($bean->action, $app_list_strings['trackers_actions_list']) == "deleted"){
            $fields['LINK_URL'] = '';
        }

    // Create the url with the listview showing only the action
    } else if(array_search($bean->action, $app_list_strings['trackers_actions_list']) == "listview") {
        $fields['LINK_URL'] = 'index.php?' . $fields['ITEM_SUMMARY'];
        $fields['LINK_TEXT'] = $fields['ACTION'];
    // Otherwise, show the action
    } else {
        $fields['LINK_URL'] = '';
    }

    // Reassign module_name to call Trackers and not the tracked module
    $bean->module_name = $bean->module_dir;

    if (file_exists('custom/modules/' . $bean->module_name . '/metadata/customAdditionalDetails.php')) {
        $additionalDetailsFile = 'custom/modules/' . $bean->module_name . '/metadata/customAdditionalDetails.php';
        require_once($additionalDetailsFile);
        
        $mod_strings = return_module_language($current_language, $bean->module_name);

        return customAdditionalDetails::additionalDetailsTracker($fields, $bean, $mod_strings);

    } else {
        global $current_language;

        $mod_strings = return_module_language($current_language, $bean->module_name);

        return additional_details($fields, $bean, $mod_strings);
    }    
}
