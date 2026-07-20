<?php
/**
 * STIC#1021 - Repair JJWG Maps Fields
 *
 * This script adds the jjwg_maps custom fields (lat, lng, address, geocode_status)
 * to the 8 modules that support maps functionality in instances that were created
 * before the GoogleMaps suite_install was implemented.
 *
 * Modules: Accounts, Cases, Contacts, Leads, Meetings, Opportunities, Project, Prospects
 */

global $current_user, $db;

// Load admin user
$current_user = new User();
$current_user->getSystemUser();

// Include the GoogleMaps installer
require_once 'install/suite_install/GoogleMaps.php';

$allFields = getCustomFields();

// Filter out fields that already exist in fields_meta_data
$missingFields = array();
foreach ($allFields as $id => $field) {
    $result = $db->query("SELECT id FROM fields_meta_data WHERE custom_module = '{$field['module']}' AND name = '{$field['name']}' AND deleted = 0");
    if (!$db->fetchByAssoc($result)) {
        $missingFields[$id] = $field;
    }
}

if (!empty($missingFields)) {
    require_once('ModuleInstall/ModuleInstaller.php');
    $ModuleInstaller = new ModuleInstaller();
    $ModuleInstaller->install_custom_fields($missingFields);
    echo "Installed " . count($missingFields) . " missing JJWG Maps fields.\n";
} else {
    echo "All JJWG Maps fields already exist.\n";
}

installJJWHooks();
echo "JJWG Maps logic hooks ensured.\n";