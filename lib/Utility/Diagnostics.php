<?php

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2020 SalesAgility Ltd.
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

namespace SuiteCRM\Utility;


class Diagnostics
{
    public function checkToken() {

        global $sugar_config;

        if (!isset($_REQUEST['callerToken']) ||
            !isset($sugar_config['cache_dir']) ||
            strlen($_REQUEST['callerToken']) != 64) {
            return false;
        }
        $tokenFilename = preg_replace("/[^a-z0-9]+/", "", $_REQUEST['callerToken']); // discard any unexpected characters
        $tokenFilename = $sugar_config['cache_dir'] . $tokenFilename . '.tmp';

        $timeStamp = filemtime($tokenFilename);
        if (($timeStamp === false) || // token file doesn't exist
            (time() - $timeStamp > 30)) { // token file validity is expired
            return false;
        }

        return true;
    }

    public function QueryWebServerFromCLI()
    {
        global $sugar_config;

        if (!isset($sugar_config['cache_dir'])) {
            return '';
        }

        //Generate a random token and save it as a filename in a short-lived file:
        $token = bin2hex(openssl_random_pseudo_bytes(32));
        $tokenFilename = $sugar_config['cache_dir'] . $token .'.tmp';
        $handle = sugar_fopen($tokenFilename, "w");

        // from this point on, once the token file is created, we want to ensure we always delete it in the end, no matter what
        try {
            fclose($handle);

            $post_data = [
                'callerToken' => $token,
            ];
            // always make the call as localhost, but get any port number from the site_url, if present
            $url = preg_replace('/(.*):\/\/(.*?)((?::[0-9]+)|\/)(.*)/', '$1://localhost$3$4', $sugar_config['site_url']);
            $url .= '/index.php?entryPoint=queryWebServerFromCLI';
            $handler = curl_init($url);
            $options = [
                CURLOPT_URL => $url,
                CURLOPT_POST => true,
                CURLOPT_POSTFIELDS => $post_data,
                CURLOPT_RETURNTRANSFER => true,
                CURLOPT_FAILONERROR => true,
                CURLOPT_FOLLOWLOCATION => true,
                // Tip: to debug the entry-point called with curl, you can add:
                // CURLOPT_COOKIE => 'XDEBUG_SESSION=PHPSTORM',
            ];

            curl_setopt_array($handler, $options);
            $ret = curl_exec($handler);
            if (curl_errno($handler)) {
                $ret = curl_error($handler);
            }
            curl_close($handler);
        } finally {
            unlink($tokenFilename);
        }

        $retArray = json_decode($ret, true);
        return ($retArray ===  null) ? '' : $retArray;
    }

    public function is_exec_available() {
        static $available;

        if (!isset($available)) {
            $available = true;
            if (ini_get('safe_mode')) {
                $available = false;
            } else {
                $d = ini_get('disable_functions');
                $s = ini_get('suhosin.executor.func.blacklist');
                if ("$d$s") {
                    $array = preg_split('/,\s*/', "$d,$s");
                    if (in_array('exec', $array)) {
                        $available = false;
                    }
                }
            }
        }

        return $available;
    }

    public function buildQuickReport($calledFromEntrypoint = false)
    {
        require_once "include/utils.php";

        global $sugar_config;
        $quickReport = Array();
        $section = Array();
        //$ret = $this->QueryWebServerFromCLI();
        //print_r($ret);

        // SuiteCRM section:
        $section['Base Path'] = getcwd();
        $section['Operating System'] = PHP_OS;
        $section['Web server user name'] = getRunningUser();
        $section['SuiteCRM Version'] = $sugar_config['suitecrm_version'];
        $section['SuiteCRM Log path'] = ($sugar_config['log_dir'] === '.' ? getcwd() : $sugar_config['log_dir']). '/' . $sugar_config['log_file'];
        $section['Site URL'] = $sugar_config['site_url'];
        $section['Host Name'] = $sugar_config['host_name'];

        $quickReport['SuiteCRM'] = $section;

        // Next section gets stored as Web server or CLI, according to where we're being called from:
        unset($section);
        $section['PHP Version'] = PHP_VERSION;
        $section['PHP SAPI name'] = php_sapi_name();
        $section['PHP Ini path'] = php_ini_loaded_file();
        $section['PHP Binary path'] = PHP_BINDIR;
        $section['PHP Error log'] = ini_get('error_log');
        $section['PHP Session save path'] = ini_get('session.save_path');
        $section['PHP Memory Limit'] = ini_get('memory_limit');
        $section['PHP Max execution time'] = ini_get('max_execution_time');
        $section['PHP Post max size'] = ini_get('post_max_size');
        $section['PHP Upload max filesize'] = ini_get('upload_max_filesize');
        $section['PHP Timezone'] = ini_get('date.timezone');
        $extensions = get_loaded_extensions();
        natcasesort($extensions);
        $section['PHP Extensions'] = implode(', ', $extensions);

        if (php_sapi_name() === 'cli') {
            $quickReport['PHP Command-line (CLI)'] = $section;
        } else {
            $quickReport['PHP Web Server'] = $section;

            // next, we try a fallible method to get CLI info when the server allows it:
            unset($section);
            if (!$calledFromEntrypoint) { // if coming from Robo through entry-point, skip this, we have direct access to CLI in caller
                if ($this->is_exec_available() &&
                    ('123' === exec("php -r 'echo 123;' 2>&1"))) { // this catches other failures, like php binary not in PATH
                    $section['PHP Version'] = exec("php -r 'echo PHP_VERSION;' 2>&1");
                    $section['PHP SAPI name'] = exec("php -r 'echo php_sapi_name();' 2>&1");
                    $section['PHP Ini path'] = exec("php -r 'echo php_ini_loaded_file();' 2>&1");
                    $section['PHP Binary path'] = exec("php -r 'echo PHP_BINDIR;' 2>&1");
                    $section['PHP Error log'] = exec("php -r 'echo ini_get(\"error_log\");' 2>&1");
                    $section['PHP Session save path'] = exec("php -r 'echo ini_get(\"session.save_path\");' 2>&1");
                    $section['PHP Memory Limit'] = exec("php -r 'echo ini_get(\"memory_limit\");' 2>&1");
                    $section['PHP Max execution time'] = exec("php -r 'echo ini_get(\"max_execution_time\");' 2>&1");
                    $section['PHP Post max size'] = exec("php -r 'echo ini_get(\"post_max_size\");' 2>&1");
                    $section['PHP Upload max filesize'] = exec("php -r 'echo ini_get(\"upload_max_filesize\");' 2>&1");
                    $section['PHP Timezone'] = exec("php -r 'echo ini_get(\"date.timezone\");' 2>&1");
                    $section['PHP Extensions'] = exec("php -r '\$extensions = get_loaded_extensions(); natcasesort(\$extensions); echo implode(\", \", \$extensions);;' 2>&1");
                } else {
                    $section['Not available'] = "Couldn't scan for CLI settings from Web server: check that 'exec' function is enabled " .
                        'in your server, and that php executable is set in PATH environment variable.' .
                        "<br/>You can try running 'php -i' from a command-line to get this information.";
                }
                $quickReport['PHP Command-line (CLI)'] = $section;
            }
        }

        // Database section:
        $db = $sugar_config['dbconfig'];
        if (isset($db)) {
            unset($section);
            $section['DB Type'] = $db['db_type'];
            $section['DB Host'] = $db['db_host_name'] . (($db['db_port'] !== '') ? ':'.$db['db_port'] : '');
            $section['DB Name'] = $db['db_name'];
            $section['DB Version'] = $GLOBALS['db']->database->server_info;

            $quickReport['Database'] = $section;
        }
        return $quickReport;
    }

}
