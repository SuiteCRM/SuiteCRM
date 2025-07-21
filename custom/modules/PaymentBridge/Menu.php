<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $mod_strings, $app_strings, $sugar_config;

if (ACLController::checkAccess('PaymentBridge', 'edit', true)) {
    $module_menu[] = array('index.php?module=PaymentBridge&action=EditView&return_module=PaymentBridge&return_action=DetailView', $mod_strings['LNK_NEW_PAYMENT_BRIDGE'], 'Add', 'PaymentBridge');
}

if (ACLController::checkAccess('PaymentBridge', 'list', true)) {
    $module_menu[] = array('index.php?module=PaymentBridge&action=index&return_module=PaymentBridge&return_action=DetailView', $mod_strings['LNK_PAYMENT_BRIDGE_LIST'], 'List', 'PaymentBridge');
}

if (ACLController::checkAccess('PaymentBridge', 'import', true)) {
    $module_menu[] = array('index.php?module=Import&action=Step1&import_module=PaymentBridge&return_module=PaymentBridge&return_action=index', $mod_strings['LNK_IMPORT_PAYMENT_BRIDGE'], 'Import', 'PaymentBridge');
}

// Custom menu items for payment features
if (ACLController::checkAccess('PaymentBridge', 'view', true)) {
    $module_menu[] = array('index.php?module=PaymentBridge&action=ValidateCard', $mod_strings['LNK_VALIDATE_CARD'], 'Validate', 'PaymentBridge');
    $module_menu[] = array('index.php?module=PaymentBridge&action=InterestCalculator', $mod_strings['LNK_INTEREST_CALCULATOR'], 'Calculate', 'PaymentBridge');
    $module_menu[] = array('index.php?module=PaymentBridge&action=SystemHealth', $mod_strings['LNK_SYSTEM_HEALTH'], 'Health', 'PaymentBridge');
    $module_menu[] = array('index.php?module=PaymentBridge&action=PaymentDashboard', $mod_strings['LNK_PAYMENT_DASHBOARD'], 'Dashboard', 'PaymentBridge');
}