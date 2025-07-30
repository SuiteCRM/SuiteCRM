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
global $mod_strings, $app_strings, $sugar_config;

require_once 'modules/MySettings/TabController.php';

$tabs = new TabController();
$displayTabs = $tabs->get_system_tabs();

$current_action = isset($_REQUEST['action']) ? $_REQUEST['action'] : '';

if (ACLController::checkAccess('stic_Resources', 'edit', true)) {
    $module_menu[] = array("index.php?module=stic_Resources&action=EditView&return_module=stic_Resources&return_action=DetailView", $mod_strings['LNK_NEW_RECORD'], "Add", 'stic_Resources');
}

if ($current_action != 'listplaces') {

    if (ACLController::checkAccess('stic_Resources', 'list', true)) {
        $module_menu[] = array("index.php?module=stic_Resources&action=index&return_module=stic_Resources&return_action=DetailView", $mod_strings['LNK_LIST'], "View", 'stic_Resources');
    }
    
    if (in_array('stic_Places', $displayTabs)) {
        if (ACLController::checkAccess('stic_Resources', 'list', true)) {
            $module_menu[] = array("index.php?module=stic_Resources&action=listplaces", $mod_strings['LNK_LIST2'], "View", 'stic_Resources');
        }
    }

    if (ACLController::checkAccess('stic_Resources', 'import', true)) {
        $module_menu[] = array("index.php?module=Import&action=Step1&import_module=stic_Resources&return_module=stic_Resources&return_action=index", $app_strings['LBL_IMPORT'], "Import", 'stic_Resources');
    }
    
    if (ACLController::checkAccess('stic_Bookings_Calendar', 'list', true)) {
        $module_menu[] = array("index.php?module=stic_Bookings_Calendar&action=index&return_module=stic_Bookings&return_action=index", translate('LBL_ACTION_VIEW_BOOKINGS_CALENDAR', 'stic_Bookings_Calendar'), "Schedule");
    }
} 
else {
    
    if (ACLController::checkAccess('stic_Resources', 'list', true)) {
        $module_menu[] = array("index.php?module=stic_Resources&action=listplaces", $mod_strings['LNK_LIST2'], "View", 'stic_Resources');
    }

    if (ACLController::checkAccess('stic_Resources', 'list', true)) {
        $module_menu[] = array("index.php?module=stic_Resources&action=index&return_module=stic_Resources&return_action=DetailView", $mod_strings['LNK_LIST'], "View", 'stic_Resources');
    }

    if (ACLController::checkAccess('stic_Resources', 'import', true)) {
        $module_menu[] = array("index.php?module=Import&action=Step1&import_module=stic_Resources&return_module=stic_Resources&return_action=index", $app_strings['LBL_IMPORT'], "Import", 'stic_Resources');
    }
    
    if (ACLController::checkAccess('stic_Bookings_Places_Calendar', 'list', true)) {
        $module_menu[] = array("index.php?module=stic_Bookings_Places_Calendar&action=index&return_module=stic_Bookings&return_action=index", translate('LBL_ACTION_VIEW_BOOKINGS_PLACES_CALENDAR', 'stic_Bookings_Places_Calendar'), "Schedule");
    }

}
