<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

include_once __DIR__ . '/../../../vendor/autoload.php';

// Prevent errors from being echoed out to the client
// We MUST use the exceptions instead to pass the errors object
// back to the client
ini_set('error_reporting', ~E_ALL & ~E_NOTICE & ~E_STRICT & ~E_DEPRECATED);

chdir(__DIR__.'/../../../');



include_once __DIR__ . '/../../../include/utils/array_utils.php';
include_once __DIR__ . '/../../../include/SugarObjects/SugarConfig.php';
include_once __DIR__ . '/../../../include/SugarLogger/SugarLogger.php';
include_once __DIR__ . '/../../../include/SugarLogger/LoggerManager.php';

SuiteCRM\ErrorMessage::log('Calling this area of API is depricated. Use http://[SuiteCRM_instance]/Api/V8... ', 'deprecated');

require_once __DIR__.'/../../../include/entryPoint.php';
global $sugar_config;
global $version;
global $container;

preg_match("/\/api\/(.*?)\//", (string) $_SERVER['REQUEST_URI'], $matches);

$GLOBALS['app_list_strings'] = return_app_list_strings_language($GLOBALS['current_language']);

$_SERVER['REQUEST_URI'] = $_SERVER['PHP_SELF'];

$version = 8;

require_once __DIR__.'/containers.php';

$app = new \Slim\App($container);
$paths = new \SuiteCRM\Utility\Paths();


// Load Core Routes
$routeFiles = (array) glob($paths->getLibraryPath() . '/API/v8/route/*.php');
foreach ($routeFiles as $routeFile) {
    require $routeFile;
}

// Load Custom Routes
$customRouteFiles = (array) glob($paths->getCustomLibraryPath() . '/API/v8/route/*.php');
foreach ($customRouteFiles as $routeFile) {
    require $routeFile;
}

// Load callables
$callableFiles = (array) glob($paths->getLibraryPath().'/API/v8/callable/*.php');
foreach ($callableFiles as $callableFile) {
    require $callableFile;
}

$customCallableFiles = (array) glob($paths->getCustomLibraryPath().'/API/v8/callable/*.php');
foreach ($customCallableFiles as $callableFile) {
    require $callableFile;
}

$app->run();
