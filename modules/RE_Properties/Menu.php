<?php
/**
 * Real Estate Properties Menu
 * Defines module menu items including mobile-specific actions
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $mod_strings, $app_strings, $sugar_config;

// Check if user has access to module
if (ACLController::checkAccess('RE_Properties', 'edit', true)) {
    $module_menu[] = array(
        "index.php?module=RE_Properties&action=EditView&return_module=RE_Properties&return_action=DetailView",
        $mod_strings['LNK_NEW_RECORD'] ?? 'Create Property',
        "CreateRE_Properties",
        'RE_Properties'
    );
}

// Mobile Quick Capture
if (ACLController::checkAccess('RE_Properties', 'edit', true)) {
    $module_menu[] = array(
        "index.php?module=RE_Properties&action=quick_capture",
        $mod_strings['LNK_QUICK_CAPTURE'] ?? 'Quick Capture',
        "QuickCapture",
        'RE_Properties'
    );
}

// List View
if (ACLController::checkAccess('RE_Properties', 'list', true)) {
    $module_menu[] = array(
        "index.php?module=RE_Properties&action=index&return_module=RE_Properties&return_action=DetailView",
        $mod_strings['LNK_LIST'] ?? 'View Properties',
        "RE_Properties",
        'RE_Properties'
    );
}

// Mobile View
$module_menu[] = array(
    "index.php?module=RE_Properties&action=Mobile",
    $mod_strings['LNK_MOBILE_VIEW'] ?? 'Mobile View',
    "MobileView",
    'RE_Properties'
);

// Import Properties
if (ACLController::checkAccess('RE_Properties', 'import', true)) {
    $module_menu[] = array(
        "index.php?module=Import&action=Step1&import_module=RE_Properties&return_module=RE_Properties&return_action=index",
        $mod_strings['LNK_IMPORT_RE_PROPERTIES'] ?? 'Import Properties',
        "Import",
        'RE_Properties'
    );
}

// Reports
$module_menu[] = array(
    "index.php?module=RE_Properties&action=Reports",
    $mod_strings['LNK_REPORTS'] ?? 'Property Reports',
    "Reports",
    'RE_Properties'
);

// COBOL Payment Dashboard
if (ACLController::checkAccess('RE_Properties', 'view', true)) {
    $module_menu[] = array(
        "index.php?module=RE_Properties&action=PaymentDashboard",
        $mod_strings['LNK_PAYMENT_DASHBOARD'] ?? 'Payment Dashboard',
        "PaymentDashboard",
        'RE_Properties'
    );
}

// Mobile API Endpoints (for documentation)
if (is_admin($GLOBALS['current_user'])) {
    $module_menu[] = array(
        "index.php?module=RE_Properties&action=APIDocs",
        $mod_strings['LNK_API_DOCS'] ?? 'Mobile API Docs',
        "APIDocs",
        'RE_Properties'
    );
}