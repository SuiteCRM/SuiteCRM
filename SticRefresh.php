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
/**
 * SticUpdate.php
 *
 * Main update script for SinergiaCRM. Handles database repairs, cache clearing,
 * and pre/post update script execution.
 *
 * Reference: https://github.com/SinergiaTIC/SinergiaCRM/blob/6dfadf98d6f5e5375a65f72fad7379dcf0e5d247/ModuleInstall/ModuleInstaller.php#L127-L141
 */

if (!defined('sugarEntry')) {
    define('sugarEntry', true);
}

require_once 'include/entryPoint.php';
require_once 'SticInclude/Utils.php';

// Initialize global variables
global $sugar_config, $current_user, $db;
$errors = [];
$infos = [];

// Check if maintenance mode is enabled
if (!(isset($sugar_config['stic_maintenance_mode_enabled']) && filter_var($sugar_config['stic_maintenance_mode_enabled'], FILTER_VALIDATE_BOOLEAN))) {
    http_response_code(503);
    $errors[] = "stic_maintenance_mode_enabled is not enabled in configuration. Exiting.";
    SticUtils::outputJsonResponse('error', $errors, $infos);
    exit;
}

require_once('ModuleInstall/ModuleInstaller.php');

// Create system user with admin capabilities to execute repairs
$current_user = new User();
$current_user->getSystemUser();

$mi = new ModuleInstaller();
$mi->silent = true;

global $moduleList, $current_language;

return_application_language($current_language);

foreach ($moduleList as $module) {
    LanguageManager::clearLanguageCache($module);
    return_module_language($current_language, $module);
}


// Cleanup application resources
sugar_cleanup(false);

// Ensure database disconnection
// Some jobs call sugar_cleanup() multiple times, which is not allowed.
// Since job results may be written to DB after completion, we disconnect here.
if (class_exists('DBManagerFactory')) {
    $db = DBManagerFactory::getInstance();
    $db->disconnect();
}

// Output results
if (count($errors)) {
    SticUtils::outputJsonResponse('error', $errors, $infos);
    exit;
}

SticUtils::outputJsonResponse('success', [], $infos);
exit;
