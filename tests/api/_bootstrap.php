<?php
/* bootstrap composer's autoloader */
chdir(__DIR__.'/../../');
require_once __DIR__.'/../../vendor/autoload.php';
global $sugar_config, $db;
require_once __DIR__ .'/../../include/utils.php';
require_once __DIR__ .'/../../include/modules.php';
require_once __DIR__ .'/../../include/entryPoint.php';
//Oddly entry point loads app_strings but not app_list_strings, manually do this here.
$GLOBALS['current_language'] = 'en_us';
$GLOBALS['app_list_strings'] = return_app_list_strings_language($GLOBALS['current_language']);

/* VERY BAD :-( - but for now at least tests are running */
$GLOBALS['sugar_config']['resource_management']['default_limit'] = 999999;
