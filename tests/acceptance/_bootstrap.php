<?php
// Here you can initialize variables that will be available to your tests

/* bootstrap composer's autoloader */
chdir(__DIR__.'/../../');
require_once __DIR__ . '/../../vendor/autoload.php';
global $sugar_config, $db;

require_once __DIR__ . '/../../include/database/DBManagerFactory.php';

require_once __DIR__ . '/../../include/utils.php';
require_once __DIR__ .'/../../include/modules.php';
require_once __DIR__ .'/../../include/entryPoint.php';
$db = DBManagerFactory::getInstance();
