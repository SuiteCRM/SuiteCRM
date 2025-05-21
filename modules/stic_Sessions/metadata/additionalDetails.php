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


function additionalDetailsstic_Sessions($fields, ?SugarBean $bean = null, $params = array())
{
    if (file_exists('custom/modules/' . $bean->module_name . '/metadata/customAdditionalDetails.php')) {
        $additionalDetailsFile = 'custom/modules/' . $bean->module_name . '/metadata/customAdditionalDetails.php';
        require_once($additionalDetailsFile);
        
        $mod_strings = return_module_language($current_language, $bean->module_name);
        return customAdditionalDetails::customAdditionalDetailsstic_Sessions($fields, $bean, $mod_strings);

    } else {
        global $current_language;
        include_once 'SticInclude/Utils.php';
        $relatedBean = SticUtils::getRelatedBeanObject($bean, 'stic_sessions_stic_events');
        $centerName = $relatedBean->stic_centers_stic_events_name;
        $centerId = $relatedBean->stic_centers_stic_eventsstic_centers_ida;
        $fields['EVENT_CENTER_NAME'] = $centerName;
        $fields['EVENT_CENTER_ID'] = $centerId;
        // if ($bean->load_relationship('stic_sessions_stic_events')) {
        //     $resourcesBeans = $bean->stic_sessions_stic_events->getBeans();
        //     $fields['RESOURCE_COUNT'] = count($resourcesBeans);
        //     $fields['SHOW_BUTTONS'] = true;
        //     if (!$fields['RESOURCE_NAME'] || !$fields['RESOURCE_ID']) {
        //         $fields['SHOW_BUTTONS'] = false;
        //         $fields['RESOURCES_LIST'] = array();
        //         foreach ($resourcesBeans as $resourceBean) {
        //             $fields['RESOURCES_LIST'][] = array('name' => $resourceBean->name, 'id' => $resourceBean->id);
        //         }
        //     }
        // }
        $mod_strings = return_module_language($current_language, $bean->module_name);
        $mod_strings['LBL_EVENT_CENTER'] = return_module_language($current_language, $relatedBean->module_name)['LBL_STIC_CENTERS_STIC_EVENTS_FROM_STIC_CENTERS_TITLE'];
        return additional_details($fields, $bean, $mod_strings);
    }    
}
