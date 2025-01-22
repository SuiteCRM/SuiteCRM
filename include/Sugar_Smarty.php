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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

if (!defined('SUGAR_SMARTY_DIR')) {
    define('SUGAR_SMARTY_DIR', sugar_cached('smarty/'));
}

/**
 * Smarty wrapper for Sugar
 * @api
 */
#[\AllowDynamicProperties]
class Sugar_Smarty extends Smarty
{
    public $_filepaths_cache = [];
    protected $_compile_id;
    /**
     * Sugar_Smarty constructor.
     */
    public function __construct()
    {
        $plugins_dir = [];
        parent::__construct();
        if (!file_exists(SUGAR_SMARTY_DIR)) {
            mkdir_recursive(SUGAR_SMARTY_DIR, true);
        }
        if (!file_exists(SUGAR_SMARTY_DIR . 'templates_c')) {
            mkdir_recursive(SUGAR_SMARTY_DIR . 'templates_c', true);
        }
        if (!file_exists(SUGAR_SMARTY_DIR . 'configs')) {
            mkdir_recursive(SUGAR_SMARTY_DIR . 'configs', true);
        }
        if (!file_exists(SUGAR_SMARTY_DIR . 'cache')) {
            mkdir_recursive(SUGAR_SMARTY_DIR . 'cache', true);
        }

        $this->template_dir = '.';
        $this->compile_dir = SUGAR_SMARTY_DIR . 'templates_c';
        $this->config_dir = SUGAR_SMARTY_DIR . 'configs';
        $this->cache_dir = SUGAR_SMARTY_DIR . 'cache';
        //$this->request_use_auto_globals = true; // to disable Smarty from using long arrays

        //TODO fix literals
        $this->auto_literal = false;

        if (file_exists('custom/include/Smarty/plugins')) {
            $plugins_dir[] = 'custom/include/Smarty/plugins';
        }
        $plugins_dir[] = 'include/Smarty/plugins';
        $this->plugins_dir = $plugins_dir;
        $this->muteUndefinedOrNullWarnings();

        $this->assign("VERSION_MARK", getVersionedPath(''));
    }



    /**
     * Override default _unlink method call to fix Bug 53010
     *
     * @param string $resource
     * @param integer $exp_time
     * @return boolean
     */
    public function _unlink($resource, $exp_time = null)
    {
        if (file_exists($resource)) {
            return parent::_unlink($resource, $exp_time);
        }

        // file wasn't found, so it must be gone.
        return true;
    }

    /**
     * executes & returns or displays the template results
     *
     * @param null $template
     * @param string|null $cache_id
     * @param string|null $compile_id
     * @param null $parent
     * @return string
     * @throws SmartyException
     * @global array $app_list_strings
     * @global array $app_strings
     * @global array $mod_strings
     * @global array $sugar_config
     */
    public function fetch($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        global $app_list_strings;
        global $app_strings;
        global $mod_strings;
        global $sugar_config;
        
        $template = $this->loadTemplatePath($template);

        $this->assign('APP_LIST_STRINGS', $app_list_strings);
        $this->assign('APP', $app_strings);
        $this->assign('MOD', $mod_strings);
        $this->assign('APP_CONFIG', $sugar_config);
        $errorLevelStored = 0;

        if (!empty($sugar_config['developerMode'])) {
            $level = $sugar_config['smarty_error_level'] ?? 0;
            $errorLevelStored = error_reporting();
            error_reporting($level);
        }
        $fetch = parent::fetch($template, $cache_id, $compile_id, $parent);

        if (!empty($sugar_config['developerMode'])) {
            error_reporting($errorLevelStored);
        }

        return $fetch;
    }

    public function display($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        $template = $this->loadTemplatePath($template);
        
        parent::display($template, $cache_id, $compile_id, $parent);
    }

    public function get_template_vars($name = null)
    {
        return $this->getTemplateVars($name);
    }

    /**
     * load a filter of specified type and name
     *
     * @param string $type filter type
     * @param string $name filter name
     *
     * @throws SmartyException
     */
    public function load_filter($type, $name)
    {
        $this->loadFilter($type, $name);
    }

    function _get_compile_path($resource_name)
    {
        return $this->_get_auto_filename($this->compile_dir, $resource_name,
                $this->_compile_id) . '.php';
    }

    function _get_auto_filename($auto_base, $auto_source = null, $auto_id = null)
    {
        $_compile_dir_sep =  $this->use_sub_dirs ? DIRECTORY_SEPARATOR : '^';
        $_return = $auto_base . DIRECTORY_SEPARATOR;

        if(isset($auto_id)) {
            // make auto_id safe for directory names
            $auto_id = str_replace('%7C',$_compile_dir_sep,(urlencode($auto_id)));
            // split into separate directories
            $_return .= $auto_id . $_compile_dir_sep;
        }

        if(isset($auto_source)) {
            // make source name safe for filename
            $_filename = urlencode(basename($auto_source));
            $_crc32 = sprintf('%08X', crc32($auto_source));
            // prepend %% to avoid name conflicts with
            // with $params['auto_id'] names
            $_crc32 = substr($_crc32, 0, 2) . $_compile_dir_sep .
                substr($_crc32, 0, 3) . $_compile_dir_sep . $_crc32;
            $_return .= '%%' . $_crc32 . '%%' . $_filename;
        }

        return $_return;
    }

    function _get_plugin_filepath($type, $name)
    {
        $_params = array('type' => $type, 'name' => $name);
        return $this->smarty_core_assemble_plugin_filepath($_params, $this);
    }

    function smarty_core_assemble_plugin_filepath($params, &$smarty)
    {
        $_plugin_filename = $params['type'] . '.' . $params['name'] . '.php';
        if (isset($smarty->_filepaths_cache[$_plugin_filename])) {
            return $smarty->_filepaths_cache[$_plugin_filename];
        }
        $_return = false;

        foreach ((array)$smarty->plugins_dir as $_plugin_dir) {

            $_plugin_filepath = $_plugin_dir . DIRECTORY_SEPARATOR . $_plugin_filename;

            // see if path is relative
            if (!preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $_plugin_dir)) {
                $_relative_paths[] = $_plugin_dir;
                // relative path, see if it is in the SMARTY_DIR
                if (is_readable(SMARTY_DIR . $_plugin_filepath)) {
                    $_return = SMARTY_DIR . $_plugin_filepath;
                    break;
                }
            }
            // try relative to cwd (or absolute)
            if (is_readable($_plugin_filepath)) {
                $_return = $_plugin_filepath;
                break;
            }
        }

//        if($_return === false) {
//            // still not found, try PHP include_path
//            if(isset($_relative_paths)) {
//                foreach ((array)$_relative_paths as $_plugin_dir) {
//
//                    $_plugin_filepath = $_plugin_dir . DIRECTORY_SEPARATOR . $_plugin_filename;
//
//                    $_params = array('file_path' => $_plugin_filepath);
//                    require_once(SMARTY_DIR. 'core.get_include_path.php');
//                    if(smarty_core_get_include_path($_params, $smarty)) {
//                        $_return = $_params['new_file_path'];
//                        break;
//                    }
//                }
//            }
//        }
        $smarty->_filepaths_cache[$_plugin_filename] = $_return;
        return $_return;
    }

    /**
     * clears compiled version of specified template resource,
     * or all compiled template files if one is not specified.
     * This function is for advanced use only, not normally needed.
     *
     * @param string $tpl_file
     * @param string $compile_id
     * @param string $exp_time
     *
     * @return boolean results of {@link smarty_core_rm_auto()}
     */
    public function clear_compiled_tpl($tpl_file = null, $compile_id = null, $exp_time = null)
    {
        return $this->clearCompiledTemplate($tpl_file, $compile_id, $exp_time);
    }

    /**
     * Log smarty error out to default log location
     * @param string $error_msg
     * @param integer $error_type
     */
    public function trigger_error($error_msg, $error_type = E_USER_WARNING)
    {
        $error_msg = htmlentities($error_msg);

        switch ($error_type) {
            case E_USER_NOTICE:
            case E_USER_ERROR:
                $GLOBALS['log']->error('Smarty: ' . $error_msg);
                break;
            case E_USER_WARNING:
                $GLOBALS['log']->warn('Smarty: ' . $error_msg);
                break;
            case E_USER_DEPRECATED:
                $GLOBALS['log']->debug('Smarty: ' . $error_msg);
                break;
            default:
                $GLOBALS['log']->fatal('Smarty: ' . $error_type . ' ' . $error_msg);
                break;
        }
    }

    /**
     * Try and fetch the tpl from the theme folder
     * if the tpl exists in the theme folder then set the resource_name to the tpl in the theme folder.
     * otherwise fall back to the default tpl
     * @param $template
     * @return string
     */
    public function loadTemplatePath($template)
    {
        $current_theme = SugarThemeRegistry::current();
        $theme_directory = (string)$current_theme;
        if (strpos($template, "themes" . DIRECTORY_SEPARATOR . $theme_directory) === false) {
            $test_path = SUGAR_PATH . DIRECTORY_SEPARATOR . "themes" . DIRECTORY_SEPARATOR . $theme_directory . DIRECTORY_SEPARATOR . $template;
            if (file_exists($test_path)) {
                $template = "themes" . DIRECTORY_SEPARATOR . $theme_directory . DIRECTORY_SEPARATOR . $template;
            }
        }

        return get_custom_file_if_exists($template);
    }
}
