<?php
/**
 * Mobile Real Estate Theme Definition
 * Optimized for real estate agents on the go
 */

$themedef = array(
    'name' => 'MobileRealEstate',
    'description' => 'Mobile-optimized theme for real estate professionals',
    'version' => array(
        'regex_matches' => array('.*'),
    ),
    'group_tabs' => true,
    'config_options' => array(
        'display_tabs' => array(
            'vname' => 'LBL_DISPLAY_TABS',
            'type' => 'bool',
            'default' => true,
        ),
    ),
    'mobile' => true,
    'pwa_enabled' => true,
);

// Mobile-specific configurations
$themedef['mobile_config'] = array(
    'touch_optimized' => true,
    'gesture_support' => true,
    'offline_mode' => true,
    'responsive_breakpoints' => array(
        'phone' => 480,
        'tablet' => 768,
        'desktop' => 1024,
    ),
);

// Real Estate specific features
$themedef['realestate_features'] = array(
    'property_quick_capture' => true,
    'location_services' => true,
    'camera_integration' => true,
    'voice_notes' => true,
    'qr_scanner' => true,
    'cobol_integration' => true,
);