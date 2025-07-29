<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class SticRemoteLogLogicHooks
{
    private $scriptDuration = -1;
    private $startLogTime = -1;

    public function server_round_trip($event, $arguments)
    {
        // Calculate execution time
        $this->startLogTime = microtime(true);
        $this->scriptDuration = $this->startLogTime - $_SERVER['REQUEST_TIME_FLOAT'];

        global $sugar_config;
        if (
            isset($sugar_config['stic_remote_monitor_enabled']) && $sugar_config['stic_remote_monitor_enabled']
            && isset($sugar_config['stic_remote_monitor_url']) && $sugar_config['stic_remote_monitor_url']
            && (!isset($sugar_config['stic_remote_monitor_duration_threshold']) 
                || (isset($sugar_config['stic_remote_monitor_duration_threshold']) && $sugar_config['stic_remote_monitor_duration_threshold'] > (microtime(true) - $_SERVER['REQUEST_TIME_FLOAT'])))
            && (!isset($sugar_config['stic_remote_monitor_memory_threshold']) 
                || (isset($sugar_config['stic_remote_monitor_memory_threshold']) && $sugar_config['stic_remote_monitor_memory_threshold'] > memory_get_peak_usage()))
            && (!isset($_REQUEST['module']) || (isset($_REQUEST['module']) 
                && $_REQUEST['module'] != 'Alerts' && $_REQUEST['module'] != 'stic_Time_Tracker') && $_REQUEST['module'] != 'Favorites')
        ) {
            $this->sticShutdownHandler();
        }
    }

    /**
     * Handles logging to SuiteCRM and Loki, even when no errors occur.
     */
    protected function sticShutdownHandler() {
        global $current_user, $sugar_config;
        $appName = 'SinergiaCRM';
        
        $site_url = $sugar_config['site_url'] ?? '';
        // Clean site URL (removing http/https)
        // $instanceClean = preg_replace('/^https?:\/\//', '', $site_url);

        $hostname = $sugar_config['host_name'] ?? 'unknown';
        $full_url = (isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] === 'on' ? 'https' : 'http') . '://' . $_SERVER['HTTP_HOST'] . $_SERVER['REQUEST_URI'];

        // Get last error, if any and extract details
        $errorInfo = self::getErrorInfo($appName);
        if ($errorInfo['HasError']) {
            // Log to SuiteCRM built-in logger
            $GLOBALS['log']->{$errorInfo['SuitecrmLevel']}($errorInfo['LogMessage']);
        }

        // Get info about call
        $request_uri = $_SERVER['REQUEST_URI'] ?? '';
        $queryString = $_SERVER['QUERY_STRING'] ?? '';
        $entryPoint = 'N/A';
        $module = 'N/A';
        if (str_starts_with($request_uri, '/index.php?entryPoint=')) {
            $action = 'EntryPoint';
            $entryPointWithParams = substr($request_uri, strlen('/index.php?entryPoint='));
            $entryPontParts = explode('&', $entryPointWithParams, 2);
            $entryPoint = $entryPontParts[0];
        } else if(str_starts_with($request_uri, '/SticMonitor.php')) {
            $action = 'SticMonitor';
        } else {
            $module = $_REQUEST['module'] ?? 'N/A';
            $action = $_REQUEST['action'] ?? 'N/A';
        }


        if (!class_exists(\Itspire\MonologLoki\Handler\LokiHandler::class)) {
            require_once 'vendor/autoload.php';
            require_once 'SticInclude/vendor/monolog-loki/src/main/php/Handler/LokiHandler.php';
            require_once 'SticInclude/vendor/monolog-loki/src/main/php/Formatter/LokiFormatter.php';
        }

        $logger = new \Monolog\Logger('loki-no-failure', [
            new \Monolog\Handler\WhatFailureGroupHandler([
                new \Itspire\MonologLoki\Handler\LokiHandler([
                    'entrypoint' => $sugar_config['stic_remote_monitor_url'],
                    'context' => [],
                    'labels' => [
                        'app' => $appName,
                        'app_version' => $sugar_config['sinergiacrm_version'] ?? 'unknown', 
                        'environment' => $sugar_config['stic_environment'] ?? 'production',
                        'host_name' => $hostname,
                        'error_type' => $errorInfo['ErrorNumber'],
                        'detected_level' => $errorInfo['MonologLevel'],

                        'http_method' => $_SERVER['REQUEST_METHOD'],
                        'http_status_code' => http_response_code(),
                        'is_ajax_request' => (!empty($_SERVER['HTTP_X_REQUESTED_WITH']) && strtolower($_SERVER['HTTP_X_REQUESTED_WITH']) == 'xmlhttprequest'),

                        'crm_module' => $module,
                        'crm_action' => $action,
                        'crm_entrypoint' => $entryPoint,

                        'user_admin' => is_admin($current_user),
                    ],
                    'curl_options' => [
                        CURLOPT_CONNECTTIMEOUT_MS => 10,
                        CURLOPT_TIMEOUT_MS => 20,
                    ]
                ])
            ])
        ]);

        // Send execution data and error (if any) to Loki
        $logger->{$errorInfo['MonologLevel']}($errorInfo['LogMessage'], [
            'crm_record' => $_REQUEST['record'] ?? 'N/A',
            
            'error_message' => $errorInfo['ErrorMessage'],
            'error_file_full' => $errorInfo['ErrorFileFull'],
            'error_file' => $errorInfo['ErrorFile'],
            'error_line' => $errorInfo['ErrorLine'],

            'memory_usage_bytes' => memory_get_usage(),
            'memory_peak_usage_bytes' => memory_get_peak_usage(),

            'php_pid' => getmypid(),
            'php_session_id' => session_id(),

            'time_duration_log' => round(microtime(true) - $this->startLogTime, 4),
            'time_duration_script' => round($this->scriptDuration, 4),
            'time_sent_log' => microtime(true),
            'time_start' => $_SERVER['REQUEST_TIME_FLOAT'],

            'url_string' => $queryString, 
            'url_full' => $full_url,
            'url_site' => $site_url,
            'url_request_uri' => $request_uri,

            'user_id' => $current_user->id ?? 'unknown',
            'user_name' => $current_user->user_name ?? 'unknown',
            'user_agent' => $_SERVER['HTTP_USER_AGENT'] ?? 'unknown',
        ]);

    }

    private static function getErrorInfo(string $appName) : array {
        // Get last error, if any
        $error = error_get_last();
        $errno = $error['type'] ?? null;
        $result = [
            'HasError' => false,
            'ErrorNumber' => $errno,
            'ErrorName' => '',
            'ErrorDesc' => '',
            'ErrorFileFull' => $error['file'] ?? null,
            'ErrorFile' => str_replace(dirname(__DIR__, 1). '/', '', $error['file'] ?? ''),
            'ErrorMessage' => $error['message'] ?? '',
            'ErrorLine' => $error['line'] ?? '',
            'ErrorType' => 'Info',
            'SuitecrmLevel' => 'info',
            'MonologLevel' => 'info',
            'LogMessage' => 'Script executed successfully.',
        ];
        if ($error !== null) {
            $result['HasError'] = true;

            switch ($errno) {
                case E_ERROR: // Fatal run-time errors (1)
                    $result['ErrorName'] = "E_ERROR";
                    $result['ErrorDesc'] = "Fatal run-time error";
                    $result['SuitecrmLevel'] = 'fatal';
                    $result['MonologLevel'] = 'critical';
                    break;

                case E_WARNING: // Run-time warnings (2)
                    $result['ErrorName'] = "E_WARNING";
                    $result['ErrorDesc'] = "Run-time warning";
                    $result['SuitecrmLevel'] = 'warn';
                    $result['MonologLevel'] = 'warning';
                    break;

                case E_PARSE: // Compile-time parse errors (4)
                    $result['ErrorName'] = "E_PARSE";
                    $result['ErrorDesc'] = "Compile-time parse error";
                    $result['SuitecrmLevel'] = 'fatal';
                    $result['MonologLevel'] = 'critical';
                    break;

                case E_NOTICE: // Run-time notices (8)
                    $result['ErrorName'] = "E_NOTICE";
                    $result['ErrorDesc'] = "Run-time notice";
                    $result['SuitecrmLevel'] = 'info';
                    $result['MonologLevel'] = 'notice';
                    break;

                case E_CORE_ERROR: // Fatal errors that occur during PHP's initial startup (16)
                    $result['ErrorName'] = "E_CORE_ERROR";
                    $result['ErrorDesc'] = "Fatal error that occur during PHP's initial startup";
                    $result['SuitecrmLevel'] = 'fatal';
                    $result['MonologLevel'] = 'critical';
                    break;

                case E_CORE_WARNING: // Warning that occur during PHP's initial startup (32)
                    $result['ErrorName'] = "E_CORE_WARNING";
                    $result['ErrorDesc'] = "Warning that occur during PHP's initial startup";
                    $result['SuitecrmLevel'] = 'warn';
                    $result['MonologLevel'] = 'warning';
                    break;
                
                case E_COMPILE_ERROR: // Fatal compile-time error (64)
                    $result['ErrorName'] = "E_COMPILE_ERROR";
                    $result['ErrorDesc'] = "Fatal compile-time error";
                    $result['SuitecrmLevel'] = 'fatal';
                    $result['MonologLevel'] = 'critical';
                    break;

                case E_COMPILE_WARNING: // Compile-time warning (128)
                    $result['ErrorName'] = "E_COMPILE_WARNING";
                    $result['ErrorDesc'] = "Compile-time warning";
                    $result['SuitecrmLevel'] = 'warn';
                    $result['MonologLevel'] = 'warning';
                    break;

                case E_DEPRECATED: // Run-time deprecation notice (8192)
                    $result['ErrorName'] = "E_DEPRECATED";
                    $result['ErrorDesc'] = "Run-time deprecation notice";
                    $result['SuitecrmLevel'] = 'warn';
                    $result['MonologLevel'] = 'warning';
                    break;

                case E_USER_ERROR: // User-generated error message (256)
                    $result['ErrorName'] = "E_USER_ERROR";
                    $result['ErrorDesc'] = "User-generated error message";
                    $result['SuitecrmLevel'] = 'error';
                    $result['MonologLevel'] = 'error';
                    break;

                case E_USER_WARNING: // User-generated warning message (512)
                    $result['ErrorName'] = "E_USER_WARNING";
                    $result['ErrorDesc'] = "User-generated warning message";
                    $result['SuitecrmLevel'] = 'warn';
                    $result['MonologLevel'] = 'warning';
                    break;

                case E_USER_NOTICE: // User-generated notice message (1824)
                    $result['ErrorName'] = "E_USER_NOTICE";
                    $result['ErrorDesc'] = "User-generated notice message";
                    $result['SuitecrmLevel'] = 'info';
                    $result['MonologLevel'] = 'notice';
                    break;

                case E_USER_DEPRECATED: // User-generated deprecation message (16384)
                    $result['ErrorName'] = "E_USER_DEPRECATED";
                    $result['ErrorDesc'] = "User-generated deprecation message";
                    $result['SuitecrmLevel'] = 'warn';
                    $result['MonologLevel'] = 'warning';
                    break;

                case E_RECOVERABLE_ERROR: // Legacy engine "exceptions" which correspond to catchable fatal error (4096)
                    $result['ErrorName'] = "E_RECOVERABLE_ERROR";
                    $result['ErrorDesc'] = "Legacy engine 'exceptions' which correspond to catchable fatal error";
                    $result['SuitecrmLevel'] = 'fatal';
                    $result['MonologLevel'] = 'critical';
                    break;

                default:
                    $result['ErrorName'] = "Unknown";
                    $result['ErrorDesc'] = "Unknown error";
                    $result['SuitecrmLevel'] = 'error';
                    $result['MonologLevel'] = 'error';
                    break;
            }
            $result['ErrorType'] = ucfirst($result['MonologLevel']);
            $result['LogMessage'] = "[{$appName} {$result['ErrorType']} {$result['ErrorName']} ({$result['ErrorDesc']}:{$result['ErrorNumber']})] ". 
                                    "{$result['ErrorMessage']} in {$result['ErrorFile']}:{$result['ErrorLine']}";
        }        
        return $result;
    }

}


// Register shutdown function to log execution and errors
// register_shutdown_function('suitecrmShutdownHandler');
