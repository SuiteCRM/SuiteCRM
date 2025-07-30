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

if (ACLController::checkAccess('stic_Bookings', 'edit', true)) {
    $module_menu[] = array("index.php?module=stic_Bookings&action=EditView&return_module=stic_Bookings&return_action=DetailView", $mod_strings['LNK_NEW_RECORD'], "Add", 'stic_Bookings');
}

if (ACLController::checkAccess('stic_Bookings', 'list', true)) {
    $module_menu[] = array("index.php?module=stic_Bookings&action=index&return_module=stic_Bookings&return_action=DetailView", $mod_strings['LNK_LIST'], "View", 'stic_Bookings');
}

if (ACLController::checkAccess('stic_Bookings', 'import', true)) {
    $module_menu[] = array("index.php?module=Import&action=Step1&import_module=stic_Bookings&return_module=stic_Bookings&return_action=index", $app_strings['LBL_IMPORT'], "Import", 'stic_Bookings');
}

if (ACLController::checkAccess('stic_Bookings_Calendar', 'list', true)) {
    $module_menu[] = array("index.php?module=stic_Bookings_Calendar&action=index&return_module=stic_Bookings&return_action=index", translate('LBL_ACTION_VIEW_BOOKINGS_CALENDAR', 'stic_Bookings_Calendar'), "Schedule");
}

if (ACLController::checkAccess('stic_Bookings_Places_Calendar', 'list', true)) {
    $module_menu[] = array("index.php?module=stic_Bookings_Places_Calendar&action=index&return_module=stic_Bookings&return_action=index", translate('LBL_ACTION_VIEW_BOOKINGS_PLACES_CALENDAR', 'stic_Bookings_Places_Calendar'), "Schedule");
}

