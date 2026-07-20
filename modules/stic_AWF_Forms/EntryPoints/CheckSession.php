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

/**
 * EntryPoint: stic_AWF_checkSession
 * This entry point is called from the form when it is loaded with CheckSessionAction, 
 * to verify that the user session is still valid and has access to the modules related to the form. 
 * It returns a JSON with the result of the validation.
 */

// Set CORS headers to allow requests from the form, which might be in a different domain
$origin = isset($_SERVER['HTTP_ORIGIN']) ? $_SERVER['HTTP_ORIGIN'] : '';
if (!empty($origin)) {
    header("Access-Control-Allow-Origin: {$origin}");
    header('Access-Control-Allow-Credentials: true'); // Allow cookies to be sent
}
header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
header('Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With');
// Handle preflight OPTIONS request
if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    exit(0);
}

if (ob_get_level()) {
    ob_end_clean();
}
header('Content-Type: application/json');

global $current_user;
// If SuiteCRM does not load the session and user (auth is false), we try to load it manually using the authenticated_user_id from the session (if available)
if (empty($current_user) || empty($current_user->id)) {
    if (isset($_SESSION['authenticated_user_id'])) {
        // Load user bean to validate session and permissions
        $current_user = BeanFactory::getBean('Users', $_SESSION['authenticated_user_id']);
    }
}

$response = [
    'is_logged' => false,
    'user_id' => null,
    'user_name' => null,
    'full_name' => null,
    'permissions' => false,
    'failed_modules' => [],
    'error' => ''
];

// Validate session
if (!empty($current_user) && !empty($current_user->id)) {
    $response['is_logged'] = true;
    $response['user_id'] = $current_user->id;
    $response['user_name'] = $current_user->user_name;
    $response['full_name'] = $current_user->full_name;

    // Validate Form ID
    $formId = $_REQUEST['id'] ?? '';
    if (empty($formId)) {
        $response['error'] = 'No Form ID provided';
        echo json_encode($response);
        exit;
    }

    // Load Form Bean
    $bean = BeanFactory::getBean('stic_AWF_Forms', $formId);
    if (!$bean || empty($bean->id)) {
        $response['error'] = 'Form not found';
        echo json_encode($response);
        exit;
    }

    // Look for modules associated with the form and verify permissions
    $response['permissions'] = true;
    $jsonConfig = $bean->configuration;
    $jsonConfig = html_entity_decode($jsonConfig, ENT_QUOTES, 'UTF-8');
    $configData = json_decode($jsonConfig, true);
    if ($configData && isset($configData['data_blocks'])) {
        foreach ($configData['data_blocks'] as $block) {
            $module = $block['module'] ?? '';
            if (!empty($module)) {
                // Check Access 'edit'
                if (!ACLController::checkAccess($module, 'edit', true)) {
                    $response['permissions'] = false;
                    $moduleName = translate($module);
                    if (!in_array($moduleName, $response['failed_modules'])) {
                        $response['failed_modules'][] = $moduleName;
                    }
                }
            }
        }
    }    
}

echo json_encode($response);
exit;
