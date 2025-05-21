<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

/**
 * @param array $arguments
 * @return string
 */
function build_argument_string($arguments = [])
{
    if (!is_array($arguments)) {
        return '';
    }

    $argument_string = '';
    $count = 0;
    foreach ($arguments as $arg) {
        if ($count !== 0) {
            //If current directory or parent directory is specified, substitute with full path
            if ($arg === '.') {
                $arg = getcwd();
            } elseif ($arg === '..') {
                $dir = getcwd();
                $arg = substr($dir, 0, strrpos($dir, DIRECTORY_SEPARATOR));
            }
            $argument_string .= ' ' . escapeshellarg((string) $arg);
        }
        $count++;
    }

    return $argument_string;
}

//Bug 52872. Dies if the request does not come from CLI.
$sapi_type = PHP_SAPI;
if (strpos($sapi_type, 'cli') !== 0) {
    die('This is command-line only script');
}
//End of #52872

$php_path = '';
$run_dce_upgrade = false;
if (isset($argv[3]) && is_dir($argv[3]) && file_exists($argv[3] . '/ini_setup.php')) {
    //this is a dce call, set the dce flag
    chdir($argv[3]);
    $run_dce_upgrade = true;
    //set the php path if found
    if (is_file($argv[7] . 'dce_config.php')) {
        include $argv[7] . 'dce_config.php';
        $php_path = $dce_config['client_php_path'] . '/';
    }
}

$php_file = $argv[0];
$p_info = pathinfo($php_file);
$php_dir = (isset($p_info['dirname']) && $p_info['dirname'] !== '.') ? $p_info['dirname'] . '/' : '';

$step1 = $php_path . "php -f {$php_dir}silentUpgrade_step1.php " . build_argument_string($argv);
passthru($step1, $output);
if ($output !== 0) {
    echo "***************         step1 failed         ***************: $output\n";
}
$has_error = $output !== 0;

if (!$has_error) {
    if ($run_dce_upgrade) {
        $step2 = $php_path . "php -f {$php_dir}silentUpgrade_dce_step1.php " . build_argument_string($argv);
        passthru($step2, $output);
    } else {
        $step2 = "php -f {$php_dir}silentUpgrade_step2.php " . build_argument_string($argv);
        passthru($step2, $output);
    }
}

if ($run_dce_upgrade) {
    $has_error = $output !== 0;
    if (!$has_error) {
        $step3 = $php_path . "php -f {$php_dir}silentUpgrade_dce_step2.php " . build_argument_string($argv);
        passthru($step3, $output);
    }
}

if ($output !== 0) {
    echo "***************         silentupgrade failed         ***************: $output\n";
}
exit($output);
