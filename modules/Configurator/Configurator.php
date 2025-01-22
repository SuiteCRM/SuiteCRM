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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class Configurator
{
    /** @var array */
    public $config = '';
    public $override = '';
    public $allow_undefined = [
        'stack_trace_errors',
        'export_delimiter',
        'use_real_names',
        'developerMode',
        'default_module_favicon',
        'authenticationClass',
        'SAML_loginurl',
        'SAML_logouturl',
        'SAML_X509Cert',
        'dashlet_auto_refresh_min',
        'show_download_tab',
        'enable_action_menu',
        'enable_line_editing_list',
        'enable_line_editing_detail',
        'hide_subpanels',
        'stackTrace'
    ];
    public $errors = array('main' => '');
    public $logger = null;
    public $previous_sugar_override_config_array = array();
    public $useAuthenticationClass = false;

    /**
     * @var array
     */
    protected $keysToIgnoreLoadedOverrideFile = [];

    protected $error = null;

    public function __construct()
    {
        $this->loadConfig();
    }

    public function loadConfig()
    {
        $this->logger = LoggerManager::getLogger();
        global $sugar_config;
        $this->config = $sugar_config;
    }

    public function populateFromPost()
    {
        $sugarConfig = SugarConfig::getInstance();

        $this->checkLoggerFileName();

        foreach ($_POST as $key => $value) {
            if ($key === "logger_file_ext" || $key === 'logger_file_name') {
                if ($value === '') {
                    $GLOBALS['log']->security("Log file extension can't be blank.");
                    continue;
                }
            }

            if (isset($this->config[$key]) || in_array($key, $this->allow_undefined)) {
                if (strcmp((string)$value, 'true') == 0) {
                    $value = true;
                }
                if (strcmp((string)$value, 'false') == 0) {
                    $value = false;
                }
                $this->config[$key] = $value;
            } else {
                $v = $sugarConfig->get(str_replace('_', '.', $key));
                if ($v !== null) {
                    setDeepArrayValue($this->config, $key, $value);
                }
            }
        }
    }

    public function checkLoggerFileName()
    {

        $logFileName =  '';
        if (!empty($_POST['logger_file_name'])) {
            $logFileName = $_POST['logger_file_name'];
        }

        $logFileExt = '';
        if (!empty($_POST['logger_file_ext'])) {
            $logFileExt = $_POST['logger_file_ext'];
        }

        $logFileExt = $this->prependDot($logFileExt);

        if (!$this->hasValidExtension('logger_file_ext', $logFileExt)) {
            $_POST['logger_file_ext'] = 'log';
            $logFileExt = $this->prependDot('log');
            LoggerManager::getLogger()->security("Setting logger_file_ext to '.log'.");
        }

        $fullName = $logFileName . $logFileExt;
        $_POST['logger_file_name'] = $logFileName;
        $_POST['logger_file_ext'] = $logFileExt;
        $valid = true;

        if (!hasValidFileName('logger_file_name', $logFileName) ||
            !$this->hasValidExtension('logger_file_name', $logFileName)
        ) {
            LoggerManager::getLogger()->security("Setting logger_file_name to ''.");
            $_POST['logger_file_name'] = '';
            $valid = false;
        }

        if (!$valid) {
            return;
        }

        if (!hasValidFileName('logger_full_name', $fullName) ||
            !$this->hasValidExtension('logger_full_name', $fullName)
        ) {
            LoggerManager::getLogger()->security("Setting logger_file_name and  logger_file_ext to ''.");
            $_POST['logger_file_name'] = '';
            $_POST['logger_file_ext'] = '';
        }
    }

    /**
     * Trim value
     * @param string $value
     * @return string
     */
    public function trimValue($value)
    {
        return preg_replace('/.*\.([^\.]+)$/', '\1', $value);
    }

    /**
     * Prepend dot
     * @param string $value
     * @return string
     */
    public function prependDot($value)
    {

        if (empty($value)) {
            return $value;
        }

        if ($value[0] === '.') {
            return $value;
        }

        return '.' . $value;
    }

    /**
     * Check if has valid extension
     * @param string $fieldName
     * @param string $value
     * @return bool
     */
    public function hasValidExtension(string $fieldName, string $value): bool
    {

        if ($value === '.' || empty($value)) {
            LoggerManager::getLogger()->security("Invalid ext  $fieldName : '$value'.");

            return false;
        }

        $defaults = get_sugar_config_defaults() ?? [];
        $badExtDefaults = $defaults['upload_badext'] ?? [];
        $badExtensions = array_merge($badExtDefaults, $this->config['upload_badext'] ?? []) ?? [];

        $badExt = array_map('strtolower', $badExtensions);

        $parts = explode('.', $value);

        if (empty($parts)) {
            LoggerManager::getLogger()->security("Invalid ext  $fieldName : '$value'.");

            return false;
        }

        $ext = array_pop($parts);

        if (in_array(strtolower($this->trimValue($ext)), $badExt, true)) {
            LoggerManager::getLogger()->security("Invalid $fieldName: '$value'.");

            return false;
        }

        return true;
    }

    public function handleOverride($fromParseLoggerSettings = false)
    {
        global $sugar_config, $sugar_version;
        $sc = SugarConfig::getInstance();
        $overrideArray = $this->readOverride();
        $this->previous_sugar_override_config_array = $overrideArray;
        $diffArray = deepArrayDiff($this->config, $sugar_config);
        $overrideArray = sugarArrayMergeRecursive($overrideArray, $diffArray);

        foreach ($this->keysToIgnoreLoadedOverrideFile as $key => $val) {
            $overrideArray[$key] = $val;
        }

        // To remember checkbox state
        if (!$this->useAuthenticationClass && !$fromParseLoggerSettings) {
            if (isset($overrideArray['authenticationClass']) &&
                $overrideArray['authenticationClass'] == 'SAMLAuthenticate') {
                unset($overrideArray['authenticationClass']);
            }
        }

        $overideString = "<?php\n/***CONFIGURATOR***/\n";

        sugar_cache_put('sugar_config', $this->config);
        $GLOBALS['sugar_config'] = $this->config;

        //print_r($overrideArray);
        //Bug#53013: Clean the tpl cache if action menu style has been changed.
        if (isset($overrideArray['enable_action_menu']) &&
                (!isset($this->previous_sugar_override_config_array['enable_action_menu']) ||
                $overrideArray['enable_action_menu'] != $this->previous_sugar_override_config_array['enable_action_menu'])
        ) {
            require_once('modules/Administration/QuickRepairAndRebuild.php');
            $repair = new RepairAndClear;
            $repair->module_list = array();
            $repair->clearTpls();
        }

        foreach ($overrideArray as $key => $val) {
            if (in_array($key, $this->allow_undefined) || isset($sugar_config[$key])) {
                if (is_string($val) && strcmp($val, 'true') == 0) {
                    $val = true;
                    $this->config[$key] = $val;
                }
                if (is_string($val) && strcmp($val, 'false') == 0) {
                    $val = false;
                    $this->config[$key] = false;
                }
            }
            $overideString .= override_value_to_string_recursive2('sugar_config', $key, $val);
        }
        $overideString .= '/***CONFIGURATOR***/';

        $this->saveOverride($overideString);
        if (isset($this->config['logger']['level']) && $this->logger) {
            $this->logger->setLevel($this->config['logger']['level']);
        }
    }

    /**
     * @param mixed $key
     * @param mixed $value
     */
    public function addKeyToIgnoreOverride($key, $value)
    {
        $this->keysToIgnoreLoadedOverrideFile[$key] = $value;
    }

    //bug #27947 , if previous $sugar_config['stack_trace_errors'] is true and now we disable it , we should clear all the cache.
    public function clearCache()
    {
        global $sugar_config, $sugar_version;
        $currentConfigArray = $this->readOverride();
        foreach ($currentConfigArray as $key => $val) {
            if (in_array($key, $this->allow_undefined) || isset($sugar_config[$key])) {
                if (empty($val)) {
                    if (!empty($this->previous_sugar_override_config_array['stack_trace_errors']) && $key == 'stack_trace_errors') {
                        require_once('include/TemplateHandler/TemplateHandler.php');
                        TemplateHandler::clearAll();
                        return;
                    }
                }
            }
        }
    }

    public function saveConfig()
    {
        if ($this->saveImages() === false) {
            return false;
        }

        $this->populateFromPost();
        $this->handleOverride();
        $this->clearCache();
    }

    public function readOverride()
    {
        $sugar_config = array();
        if (file_exists('config_override.php')) {
            if (!is_readable('config_override.php')) {
                $GLOBALS['log']->fatal("Unable to read the config_override.php file. Check the file permissions");
            } else {
                include('config_override.php');
            }
        }
        return $sugar_config;
    }

    public function saveOverride($override)
    {
        require_once('install/install_utils.php');
        if (!file_exists('config_override.php')) {
            touch('config_override.php');
        }
        if (!(make_writable('config_override.php')) || !(is_writable('config_override.php'))) {
            $GLOBALS['log']->fatal("Unable to write to the config_override.php file. Check the file permissions");
            return;
        }
        sugar_file_put_contents('config_override.php', $override);
    }

    public function overrideClearDuplicates($array_name, $key)
    {
        if (!empty($this->override)) {
            $pattern = '/.*CONFIGURATOR[^\$]*\$' . $array_name . '\[\'' . $key . '\'\][\ ]*=[\ ]*[^;]*;\n/';
            $this->override = preg_replace($pattern, '', (string) $this->override);
        } else {
            $this->override = "<?php\n\n?>";
        }
    }

    public function replaceOverride($array_name, $key, $value)
    {
        $GLOBALS[$array_name][$key] = $value;
        $this->overrideClearDuplicates($array_name, $key);
        $new_entry = '/***CONFIGURATOR***/' . override_value_to_string($array_name, $key, $value);
        $this->override = str_replace('?>', "$new_entry\n?>", (string) $this->override);
    }

    public function restoreConfig()
    {
        $this->readOverride();
        $this->overrideClearDuplicates('sugar_config', '[a-zA-Z0-9\_]+');
        $this->saveOverride();
        ob_clean();
        header('Location: index.php?action=EditView&module=Configurator');
    }

    public function saveImages()
    {
        if (!empty($_POST['company_logo'])) {
            if ($this->saveCompanyLogo("upload://" . $_POST['company_logo']) === false) {
                return false;
            }
        }
    }

    public function checkTempImage($path)
    {
        if (!verify_uploaded_image($path)) {
            $error = translate('LBL_ALERT_TYPE_IMAGE');
            $GLOBALS['log']->fatal("A user ({$GLOBALS['current_user']->id}) attempted to use an invalid file for the logo - {$path}");
            $this->error = $error;
            return false;
        }

        return $path;
    }

    public function getError()
    {
        $e = $this->error;
        $this->error = null;
        return $e;
    }

    /**
     * Saves the company logo to the custom directory for the default theme, so all themes can use it
     *
     * @param string $path path to the image to set as the company logo image
     */
    public function saveCompanyLogo($path)
    {
        $path = $this->checkTempImage($path);
        if ($path === false) {
            return false;
        }

        mkdir_recursive('custom/' . SugarThemeRegistry::current()->getDefaultImagePath(), true);
        copy($path, 'custom/' . SugarThemeRegistry::current()->getDefaultImagePath() . '/company_logo.png');
        sugar_cache_clear('company_logo_attributes');
        SugarThemeRegistry::clearAllCaches();
    }

    /**
     * @params : none
     * @return : An array of logger configuration properties including log size, file extensions etc. See SugarLogger for more details.
     * Parses the old logger settings from the log4php.properties files.
     *
     */
    public function parseLoggerSettings()
    {
        if (!function_exists('setDeepArrayValue')) {
            require('include/utils/array_utils.php');
        }
        if (file_exists('log4php.properties')) {
            $fileContent = file_get_contents('log4php.properties');
            $old_props = explode('\n', $fileContent);
            $new_props = array();
            $key_names = array();
            foreach ($old_props as $value) {
                if (!empty($value) && !preg_match("/^\/\//", $value)) {
                    $temp = explode("=", $value);
                    $property = isset($temp[1]) ? $temp[1] : array();
                    if (preg_match("/log4php.appender.A2.MaxFileSize=/", $value)) {
                        setDeepArrayValue($this->config, 'logger_file_maxSize', rtrim($property));
                    } elseif (preg_match("/log4php.appender.A2.File=/", $value)) {
                        $ext = preg_split("/\./", $property);
                        if (preg_match("/^\./", $property)) { //begins with .
                            setDeepArrayValue($this->config, 'logger_file_ext', isset($ext[2]) ? '.' . rtrim($ext[2]) : '.log');
                            setDeepArrayValue($this->config, 'logger_file_name', rtrim("." . $ext[1]));
                        } else {
                            setDeepArrayValue($this->config, 'logger_file_ext', isset($ext[1]) ? '.' . rtrim($ext[1]) : '.log');
                            setDeepArrayValue($this->config, 'logger_file_name', rtrim($ext[0]));
                        }
                    } elseif (preg_match("/log4php.appender.A2.layout.DateFormat=/", $value)) {
                        setDeepArrayValue($this->config, 'logger_file_dateFormat', trim(rtrim($property), '""'));
                    } elseif (preg_match("/log4php.rootLogger=/", $value)) {
                        $property = explode(",", $property);
                        setDeepArrayValue($this->config, 'logger_level', rtrim($property[0]));
                    }
                }
            }
            setDeepArrayValue($this->config, 'logger_file_maxLogs', 10);
            setDeepArrayValue($this->config, 'logger_file_suffix', "%m_%Y");
            $this->handleOverride();
            unlink('log4php.properties');
            $GLOBALS['sugar_config'] = $this->config; //load the rest of the sugar_config settings.
            require_once('include/SugarLogger/SugarLogger.php');
            //$logger = new SugarLogger(); //this will create the log file.
        }

        if (!isset($this->config['logger']) || empty($this->config['logger'])) {
            $this->config['logger'] = array(
                'file' => array(
                    'ext' => '.log',
                    'name' => 'sugarcrm',
                    'dateFormat' => '%c',
                    'maxSize' => '10MB',
                    'maxLogs' => 10,
                    'suffix' => ''), // bug51583, change default suffix to blank for backwards comptability
                'level' => 'fatal');
        }
        $this->handleOverride(true);
    }

    /**
     * @return bool
     */
    public function isConfirmOptInEnabled()
    {
        $confirmOptInEnabled =
            $this->getConfirmOptInEnumValue() === SugarEmailAddress::COI_STAT_CONFIRMED_OPT_IN;

        if (!$confirmOptInEnabled) {
            $this->logger->warn('Confirm Opt in is disabled in email settings');
        }

        return $confirmOptInEnabled;
    }

    /**
     * @return bool
     */
    public function isOptInEnabled()
    {
        $confirmOptInEnabled =
            $this->getConfirmOptInEnumValue() === SugarEmailAddress::COI_STAT_OPT_IN;

        if (!$confirmOptInEnabled) {
            $this->logger->warn('Confirm Opt in is disabled in email settings');
        }

        return $confirmOptInEnabled;
    }

    /**
     * @return null|string
     */
    public function getConfirmOptInTemplateId()
    {
        /** @var null|string $confirmOptInTemplateId */
        $confirmOptInTemplateId = $this->config['email_confirm_opt_in_email_template_id'];
        if (empty($confirmOptInTemplateId)) {
            $confirmOptInTemplateId =
                isset($this->config['system_email_templates']['confirm_opt_in_template_id']) ?
                    $this->config['system_email_templates']['confirm_opt_in_template_id'] : null;
        }

        if (!$confirmOptInTemplateId) {
            $this->logger->warn('Confirm Opt template is not set');
        }

        return $confirmOptInTemplateId;
    }

    /**
     * returns Confirm Opt In Enum Value from configuration
     *
     * @return string
     */
    public function getConfirmOptInEnumValue()
    {
        // TODO: use this function everywhere to make the code more clear also this variable 'email_enable_confirm_opt_in' is enum but assuming a bool -> should change this config variable name
        $ret = isset($this->config['email_enable_confirm_opt_in']) ? $this->config['email_enable_confirm_opt_in'] : SugarEmailAddress::COI_STAT_DISABLED;
        return $ret;
    }
}
