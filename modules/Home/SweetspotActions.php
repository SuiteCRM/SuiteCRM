<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2026 SalesAgility Ltd.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/SuiteSweetspot/actions.php';

global $current_user;
global $sugar_config;

header('Content-Type: application/json; charset=UTF-8');

if (isset($sugar_config['suite_sweetspot_enabled']) && !$sugar_config['suite_sweetspot_enabled']) {
    echo json_encode(array('actions' => array()));
    sugar_cleanup(true);
}

if (empty($current_user) || empty($current_user->id)) {
    http_response_code(403);
    echo json_encode(array(
        'error' => 'not_authenticated',
        'message' => 'User must be authenticated.',
    ));
    sugar_cleanup(true);
}

$actions = suite_sweetspot_get_actions_for_user($current_user);

echo json_encode(array(
    'actions' => array_values($actions),
));

sugar_cleanup(true);
