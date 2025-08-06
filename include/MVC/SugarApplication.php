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

/*
 * Created on Mar 21, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
require_once('include/MVC/Controller/ControllerFactory.php');
require_once('include/MVC/View/ViewFactory.php');

/**
 * SugarCRM application
 * @api
 */
class SugarApplication
{
    public $controller = null;
    public $headerDisplayed = false;
    public $default_module = 'Home';
    public $default_action = 'index';

    public function __construct()
    {
    }



    /**
     * Perform execution of the application. This method is called from index2.php
     */
    public function execute()
    {
        global $sugar_config;
        if (!empty($sugar_config['default_module'])) {
            $this->default_module = $sugar_config['default_module'];
        }
        $module = $this->default_module;
        if (!empty($_REQUEST['module'])) {
            $module = $_REQUEST['module'];
        }
        insert_charset_header();
        $this->setupPrint();
        $this->controller = ControllerFactory::getController($module);
        // If the entry point is defined to not need auth, then don't authenticate.
        if (empty($_REQUEST['entryPoint']) || $this->controller->checkEntryPointRequiresAuth($_REQUEST['entryPoint'])) {
            $this->loadUser();
            $this->ACLFilter();
            $this->preProcess();
            $this->controller->preProcess();
            $this->checkHTTPReferer();
        }

        SugarThemeRegistry::buildRegistry();
        $this->loadLanguages();
        $this->loadDisplaySettings();
        $this->loadGlobals();
        $this->setupResourceManagement($module);
        $this->controller->execute();
        sugar_cleanup();
    }

    /**
     * Load the authenticated user. If there is not an authenticated user then redirect to login screen.
     */
    public function loadUser()
    {
        global $authController, $sugar_config;
        // Double check the server's unique key is in the session.  Make sure this is not an attempt to hijack a session
        $user_unique_key = (isset($_SESSION['unique_key'])) ? $_SESSION['unique_key'] : '';
        $server_unique_key = (isset($sugar_config['unique_key'])) ? $sugar_config['unique_key'] : '';
        $allowed_actions = (!empty($this->controller->allowed_actions)) ? $this->controller->allowed_actions : $allowed_actions = array('Authenticate', 'Login', 'LoggedOut');

        $authController = new AuthenticationController();

        if (($user_unique_key != $server_unique_key) && (!in_array($this->controller->action, $allowed_actions)) &&
                (!isset($_SESSION['login_error']))) {
            session_destroy();

            if (!empty($this->controller->action)) {
                if (strtolower($this->controller->action) == 'delete') {
                    $this->controller->action = 'DetailView';
                } elseif (strtolower($this->controller->action) == 'save') {
                    $this->controller->action = 'EditView';
                } elseif (strtolower($this->controller->action) == 'quickcreate') {
                    $this->controller->action = 'index';
                    $this->controller->module = 'home';
                } elseif (isset($_REQUEST['massupdate']) || isset($_GET['massupdate']) || isset($_POST['massupdate'])) {
                    $this->controller->action = 'index';
                } elseif ($this->isModifyAction()) {
                    $this->controller->action = 'index';
                } elseif ($this->controller->action == $this->default_action && $this->controller->module == $this->default_module) {
                    $this->controller->action = '';
                    $this->controller->module = '';
                } elseif (strtolower($this->controller->module) == 'alerts' && strtolower($this->controller->action) == 'get') {
                    echo 'lost password';
                    if (preg_match('/\bentryPoint=Changenewpassword\b/', $_SERVER['HTTP_REFERER'])) {
                        echo ' - change new password';
                    }
                    exit();
                }
            }

            $authController->authController->redirectToLogin($this);
        }

        $GLOBALS['current_user'] = BeanFactory::newBean('Users');

        $isLogicActionCall = $this->controller->module === 'Users' && in_array($this->controller->action, $allowed_actions);
        if (isset($_SESSION['authenticated_user_id'])) {
            // set in modules/Users/Authenticate.php
            if (!$authController->sessionAuthenticate()) {
                // if the object we get back is null for some reason, this will break - like user prefs are corrupted
                $GLOBALS['log']->fatal('User retrieval for ID: (' . $_SESSION['authenticated_user_id'] . ') does not exist in database or retrieval failed catastrophically.  Calling session_destroy() and sending user to Login page.');
                session_destroy();
                SugarApplication::redirect('index.php?action=Login&module=Users');
                die();
            }//fi
        } elseif (!$isLogicActionCall || !empty($_REQUEST['entryPoint'])) {
            session_destroy();
            SugarApplication::redirect('index.php?action=Login&module=Users');
            die();
        }
        $GLOBALS['log']->debug('Current user is: ' . $GLOBALS['current_user']->user_name);

        //set cookies
        if (isset($_SESSION['authenticated_user_id'])) {
            $GLOBALS['log']->debug("setting cookie ck_login_id_20 to " . $_SESSION['authenticated_user_id']);
            self::setCookie('ck_login_id_20', $_SESSION['authenticated_user_id'], time() + 86400 * 90);
        }
        if (isset($_SESSION['authenticated_user_theme'])) {
            $GLOBALS['log']->debug("setting cookie ck_login_theme_20 to " . $_SESSION['authenticated_user_theme']);
            self::setCookie('ck_login_theme_20', $_SESSION['authenticated_user_theme'], time() + 86400 * 90);
        }
        if (isset($_SESSION['authenticated_user_theme_color'])) {
            $GLOBALS['log']->debug("setting cookie ck_login_theme_color_20 to " . $_SESSION['authenticated_user_theme_color']);
            self::setCookie('ck_login_theme_color_20', $_SESSION['authenticated_user_theme_color'], time() + 86400 * 90);
        }
        if (isset($_SESSION['authenticated_user_theme_font'])) {
            $GLOBALS['log']->debug("setting cookie ck_login_theme_font_20 to " . $_SESSION['authenticated_user_theme_font']);
            self::setCookie('ck_login_theme_font_20', $_SESSION['authenticated_user_theme_font'], time() + 86400 * 90);
        }
        if (isset($_SESSION['authenticated_user_language'])) {
            $GLOBALS['log']->debug("setting cookie ck_login_language_20 to " . $_SESSION['authenticated_user_language']);
            self::setCookie('ck_login_language_20', $_SESSION['authenticated_user_language'], time() + 86400 * 90);
        }
        //check if user can access
    }

    public function ACLFilter()
    {
        ACLController :: filterModuleList($GLOBALS['moduleList']);
    }

    /**
     * setupResourceManagement
     * This function initialize the ResourceManager and calls the setup method
     * on the ResourceManager instance.
     *
     */
    public function setupResourceManagement($module)
    {
        require_once('include/resource/ResourceManager.php');
        $resourceManager = ResourceManager::getInstance();
        $resourceManager->setup($module);
    }

    public function setupPrint()
    {
        $GLOBALS['request_string'] = '';

        // merge _GET and _POST, but keep the results local
        // this handles the issues where values come in one way or the other
        // without affecting the main super globals
        $merged = array_merge($_GET, $_POST);
        foreach ($merged as $key => $val) {
            if (is_array($val)) {
                foreach ($val as $k => $v) {
                    //If an array, then skip the urlencoding. This should be handled with stringify instead.
                    if (is_array($v)) {
                        continue;
                    }

                    $GLOBALS['request_string'] .= urlencode($key) . '[' . $k . ']=' . urlencode($v) . '&';
                }
            } else {
                $GLOBALS['request_string'] .= urlencode($key) . '=' . urlencode($val) . '&';
            }
        }
        $GLOBALS['request_string'] .= 'print=true';
    }

    public function preProcess()
    {
        $config = new Administration;
        $config->retrieveSettings();
        if (!empty($_SESSION['authenticated_user_id'])) {
            if (isset($_SESSION['hasExpiredPassword']) && $_SESSION['hasExpiredPassword'] == '1') {
                if ($this->controller->action != 'Save' && $this->controller->action != 'Logout') {
                    $this->controller->module = 'Users';
                    $this->controller->action = 'ChangePassword';
                    $record = $GLOBALS['current_user']->id;
                } else {
                    $this->handleOfflineClient();
                }
            } else {
                $ut = $GLOBALS['current_user']->getPreference('ut');
                if (empty($ut) && $this->controller->action != 'AdminWizard' && $this->controller->action != 'EmailUIAjax' && $this->controller->action != 'Wizard' && $this->controller->action != 'SaveAdminWizard' && $this->controller->action != 'SaveUserWizard' && $this->controller->action != 'SaveTimezone' && $this->controller->action != 'Logout') {
                    $this->controller->module = 'Users';
                    $this->controller->action = 'SetTimezone';
                    $record = $GLOBALS['current_user']->id;
                } else {
                    if ($this->controller->action != 'AdminWizard' && $this->controller->action != 'EmailUIAjax' && $this->controller->action != 'Wizard' && $this->controller->action != 'SaveAdminWizard' && $this->controller->action != 'SaveUserWizard') {
                        $this->handleOfflineClient();
                    }
                }
            }
        }
        $this->handleAccessControl();
    }

    public function handleOfflineClient()
    {
        if (isset($GLOBALS['sugar_config']['disc_client']) && $GLOBALS['sugar_config']['disc_client']) {
            if (isset($_REQUEST['action']) && $_REQUEST['action'] != 'SaveTimezone') {
                if (!file_exists('modules/Sync/file_config.php')) {
                    if ($_REQUEST['action'] != 'InitialSync' && $_REQUEST['action'] != 'Logout' &&
                            ($_REQUEST['action'] != 'Popup' && $_REQUEST['module'] != 'Sync')) {
                        //echo $_REQUEST['action'];
                        //die();
                        $this->controller->module = 'Sync';
                        $this->controller->action = 'InitialSync';
                    }
                } else {
                    require_once('modules/Sync/file_config.php');
                    if (isset($file_sync_info['is_first_sync']) && $file_sync_info['is_first_sync']) {
                        if ($_REQUEST['action'] != 'InitialSync' && $_REQUEST['action'] != 'Logout' &&
                                ($_REQUEST['action'] != 'Popup' && $_REQUEST['module'] != 'Sync')) {
                            $this->controller->module = 'Sync';
                            $this->controller->action = 'InitialSync';
                        }
                    }
                }
            }
            global $moduleList, $sugar_config, $sync_modules;
            require_once('modules/Sync/SyncController.php');
            $GLOBALS['current_user']->is_admin = '0'; //No admins for disc client
        }
    }

    /**
     * Handles everything related to authorization.
     */
    public function handleAccessControl()
    {
        if ($GLOBALS['current_user']->isDeveloperForAnyModule()) {
            return;
        }
        if (!empty($_REQUEST['action']) && $_REQUEST['action'] == "RetrieveEmail") {
            return;
        }
        if (!is_admin($GLOBALS['current_user']) && !empty($GLOBALS['adminOnlyList'][$this->controller->module]) && !empty($GLOBALS['adminOnlyList'][$this->controller->module]['all']) && (empty($GLOBALS['adminOnlyList'][$this->controller->module][$this->controller->action]) || $GLOBALS['adminOnlyList'][$this->controller->module][$this->controller->action] != 'allow')) {
            $this->controller->hasAccess = false;
            return;
        }

        // Bug 20916 - Special case for check ACL access rights for Subpanel QuickCreates
        if (isset($_POST['action']) && $_POST['action'] == 'SubpanelCreates') {
            $actual_module = $_POST['target_module'];
            if (!empty($GLOBALS['modListHeader']) && !in_array($actual_module, $GLOBALS['modListHeader'])) {
                $this->controller->hasAccess = false;
            }
            return;
        }


        if (!empty($GLOBALS['current_user']) && empty($GLOBALS['modListHeader'])) {
            $GLOBALS['modListHeader'] = query_module_access_list($GLOBALS['current_user']);
        }

        if (in_array($this->controller->module, $GLOBALS['modInvisList']) &&
                ((in_array('Activities', $GLOBALS['moduleList']) &&
                in_array('Calendar', $GLOBALS['moduleList'])) &&
                in_array($this->controller->module, $GLOBALS['modInvisListActivities']))
        ) {
            $this->controller->hasAccess = false;
            return;
        }
    }

    /**
     * Load only bare minimum of language that can be done before user init and MVC stuff
     */
    public static function preLoadLanguages()
    {
        $GLOBALS['current_language'] = get_current_language();
        $GLOBALS['log']->debug('current_language is: ' . $GLOBALS['current_language']);
        //set module and application string arrays based upon selected language
        $GLOBALS['app_strings'] = return_application_language($GLOBALS['current_language']);
    }

    /**
     * Load application wide languages as well as module based languages so they are accessible
     * from the module.
     */
    public function loadLanguages()
    {
        $GLOBALS['current_language'] = get_current_language();
        $GLOBALS['log']->debug('current_language is: ' . $GLOBALS['current_language']);
        //set module and application string arrays based upon selected language
        $GLOBALS['app_strings'] = return_application_language($GLOBALS['current_language']);
        if (empty($GLOBALS['current_user']->id)) {
            $GLOBALS['app_strings']['NTC_WELCOME'] = '';
        }
        if (!empty($GLOBALS['system_config']->settings['system_name'])) {
            $GLOBALS['app_strings']['LBL_BROWSER_TITLE'] = $GLOBALS['system_config']->settings['system_name'];
        }
        $GLOBALS['app_list_strings'] = return_app_list_strings_language($GLOBALS['current_language']);
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], $this->controller->module);
    }

    /**
     * checkDatabaseVersion
     * Check the db version sugar_version.php and compare to what the version is stored in the config table.
     * Ensure that both are the same.
     */
    public function checkDatabaseVersion($dieOnFailure = true)
    {
        $row_count = sugar_cache_retrieve('checkDatabaseVersion_row_count');
        $sugarDbVersion = $GLOBALS['sugar_db_version'];
        $db = DBManagerFactory::getInstance();
        if (empty($row_count)) {
            $version_query = "SELECT count(*) as the_count FROM config WHERE category='info' AND name='sugar_version' AND " .
                    $db->convert('value', 'text2char') . " = " .
                    $db->quoted($sugarDbVersion);

            $result = $db->query($version_query);
            $row = $db->fetchByAssoc($result);
            $row_count = $row['the_count'];
            sugar_cache_put('checkDatabaseVersion_row_count', $row_count);
        }

        if ($row_count == 0 && empty($GLOBALS['sugar_config']['disc_client'])) {
            if ($dieOnFailure) {
                $replacementStrings = array(
                    0 => $GLOBALS['sugar_version'],
                    1 => $GLOBALS['sugar_db_version'],
                );
                sugar_die(string_format($GLOBALS['app_strings']['ERR_DB_VERSION'], $replacementStrings));
            } else {
                return false;
            }
        }

        return true;
    }

    /**
     * Load the themes/images.
     */
    public function loadDisplaySettings()
    {
        global $theme;

        // load the user's default theme
        $theme = $GLOBALS['current_user']->getPreference('user_theme');

        if (is_null($theme)) {
            $theme = $GLOBALS['sugar_config']['default_theme'];
            if (!empty($_SESSION['authenticated_user_theme'])) {
                $theme = $_SESSION['authenticated_user_theme'];
            } else {
                if (!empty($_COOKIE['sugar_user_theme'])) {
                    $theme = $_COOKIE['sugar_user_theme'];
                }
            }

            if (isset($_SESSION['authenticated_user_theme']) && $_SESSION['authenticated_user_theme'] != '') {
                $_SESSION['theme_changed'] = false;
            }
        }

        $available_themes = SugarThemeRegistry::availableThemes();
        if (!isset($available_themes[$theme])) {
            $theme = $GLOBALS['sugar_config']['default_theme'];
        }

        if (!is_null($theme) && !headers_sent()) {
            self::setCookie('sugar_user_theme', $theme, time() + 31536000, null, null, isSSL(), true); // expires in a year
        }

        SugarThemeRegistry::set($theme);
        require_once('include/utils/layout_utils.php');
        $GLOBALS['image_path'] = SugarThemeRegistry::current()->getImagePath() . '/';
        if (defined('TEMPLATE_URL')) {
            $GLOBALS['image_path'] = TEMPLATE_URL . '/' . $GLOBALS['image_path'];
        }

        if (isset($GLOBALS['current_user'])) {
            $GLOBALS['gridline'] = (int) ($GLOBALS['current_user']->getPreference('gridline') == 'on');
            $GLOBALS['current_user']->setPreference('user_theme', $theme, 0, 'global');
        }
    }

    public function loadLicense()
    {
        loadLicense();
        global $user_unique_key, $server_unique_key;
        $user_unique_key = (isset($_SESSION['unique_key'])) ? $_SESSION['unique_key'] : '';
        $server_unique_key = (isset($sugar_config['unique_key'])) ? $sugar_config['unique_key'] : '';
    }

    public function loadGlobals()
    {
        global $currentModule;
        $currentModule = $this->controller->module;
        if ($this->controller->module == $this->default_module) {
            $_REQUEST['module'] = $this->controller->module;
            if (empty($_REQUEST['action'])) {
                $_REQUEST['action'] = $this->default_action;
            }
        }
    }

    /**
     * Actions that modify data in this controller's instance and thus require referrers
     * @var array
     */
    protected $modifyActions = array();

    /**
     * Actions that always modify data and thus require referrers
     * save* and delete* hardcoded as modified
     * @var array
     */
    private $globalModifyActions = array(
        'massupdate', 'configuredashlet', 'import', 'importvcardsave', 'inlinefieldsave',
        'wlsave', 'quicksave'
    );

    /**
     * Modules that modify data and thus require referrers for all actions
     */
    private $modifyModules = array(
        'Administration' => true,
        'UpgradeWizard' => true,
        'Configurator' => true,
        'Studio' => true,
        'ModuleBuilder' => true,
        'Emails' => true,
        'DCETemplates' => true,
        'DCEInstances' => true,
        'DCEActions' => true,
        'Trackers' => array('trackersettings'),
        'SugarFavorites' => array('tag'),
        'Import' => array('last', 'undo'),
        'Users' => array('changepassword', "generatepassword"),
    );

    protected function isModifyAction()
    {
        $action = strtolower($this->controller->action);
        if (substr($action, 0, 4) == "save" || substr($action, 0, 6) == "delete") {
            return true;
        }
        if (isset($this->modifyModules[$this->controller->module])) {
            if ($this->modifyModules[$this->controller->module] === true) {
                return true;
            }
            if (in_array($this->controller->action, $this->modifyModules[$this->controller->module])) {
                return true;
            }
        }
        if (in_array($this->controller->action, $this->globalModifyActions)) {
            return true;
        }
        if (in_array($this->controller->action, $this->modifyActions)) {
            return true;
        }
        return false;
    }

    /**
     * The list of the actions excepted from referer checks by default
     * @var array
     */
    protected $whiteListActions = array('index', 'ListView', 'DetailView', 'EditView', 'oauth', 'authorize', 'Authenticate', 'Login', 'SupportPortal');

    /**
     *
     * Checks a request to ensure the request is coming from a valid source or it is for one of the white listed actions
     */
    protected function checkHTTPReferer($dieIfInvalid = true)
    {
        global $sugar_config;
        if (!empty($sugar_config['http_referer']['actions'])) {
            $this->whiteListActions = array_merge($sugar_config['http_referer']['actions'], $this->whiteListActions);
        }

        $strong = empty($sugar_config['http_referer']['weak']);

        // Bug 39691 - Make sure localhost and 127.0.0.1 are always valid HTTP referers
        $whiteListReferers = array('127.0.0.1', 'localhost');
        if (!empty($_SERVER['SERVER_ADDR'])) {
            $whiteListReferers[] = $_SERVER['SERVER_ADDR'];
        }
        if (!empty($sugar_config['http_referer']['list'])) {
            $whiteListReferers = array_merge($whiteListReferers, $sugar_config['http_referer']['list']);
        }

        if ($strong && empty($_SERVER['HTTP_REFERER']) && !in_array($this->controller->action, $this->whiteListActions) && $this->isModifyAction()) {
            $http_host = explode(':', $_SERVER['HTTP_HOST']);
            $whiteListActions = $this->whiteListActions;
            $whiteListActions[] = $this->controller->action;
            $whiteListString = "'" . implode("', '", $whiteListActions) . "'";
            if ($dieIfInvalid) {
                header("Cache-Control: no-cache, must-revalidate");
                $ss = new Sugar_Smarty;
                $ss->assign('host', securexss($http_host[0]));
                $ss->assign('action', $this->controller->action);
                $ss->assign('whiteListString', $whiteListString);
                $ss->display('include/MVC/View/tpls/xsrf.tpl');
                sugar_cleanup(true);
            }
            return false;
        } else {
            if (!empty($_SERVER['HTTP_REFERER']) && !empty($_SERVER['SERVER_NAME'])) {
                $http_ref = parse_url($_SERVER['HTTP_REFERER']);
                if ($http_ref['host'] !== $_SERVER['SERVER_NAME'] && !in_array($this->controller->action, $this->whiteListActions) &&
                    (empty($whiteListReferers) || !in_array($http_ref['host'], $whiteListReferers))) {
                    if ($dieIfInvalid) {
                        header("Cache-Control: no-cache, must-revalidate");
                        $whiteListActions = $this->whiteListActions;
                        $whiteListActions[] = $this->controller->action;
                        $whiteListString = "'" . implode("', '", $whiteListActions) . "'";

                        $ss = new Sugar_Smarty;
                        $ss->assign('host', securexss($http_ref['host']));
                        $ss->assign('action', $this->controller->action);
                        $ss->assign('whiteListString', $whiteListString);
                        $ss->display('include/MVC/View/tpls/xsrf.tpl');
                        sugar_cleanup(true);
                    }
                    return false;
                }
            }
        }
        return true;
    }

    public function startSession()
    {
        $sessionIdCookie = isset($_COOKIE['PHPSESSID']) ? $_COOKIE['PHPSESSID'] : null;
        if (isset($_REQUEST['MSID'])) {
            session_id($_REQUEST['MSID']);
            session_start();
            if (!isset($_SESSION['user_id'])) {
                if (isset($_COOKIE['PHPSESSID'])) {
                    self::setCookie('PHPSESSID', '', time() - 42000, '/');
                }
                sugar_cleanup(false);
                session_destroy();
                exit('Not a valid entry method');
            }
        } else {
            if (can_start_session()) {
                session_start();
            }
        }

        //set the default module to either Home or specified default
        $default_module = !empty($GLOBALS['sugar_config']['default_module']) ? $GLOBALS['sugar_config']['default_module'] : 'Home';

        //set session expired message if login module and action are set to a non login default
        //AND session id in cookie is set but super global session array is empty
        if (isset($_REQUEST['login_module']) && isset($_REQUEST['login_action']) && !($_REQUEST['login_module'] == $default_module && $_REQUEST['login_action'] == 'index')) {
            if (!is_null($sessionIdCookie) && empty($_SESSION)) {
                self::setCookie('loginErrorMessage', 'LBL_SESSION_EXPIRED', time() + 30, '/');
            }
        }


        LogicHook::initialize()->call_custom_logic('', 'after_session_start');
    }

    public function endSession()
    {
        session_destroy();
    }
    
    /**
     * Redirect to another URL
     *
     * @access	public
     * @param	string	$url	The URL to redirect to
     */
    public static function redirect(
        $url
    ) {
        /*
         * If the headers have been sent, then we cannot send an additional location header
         * so we will output a javascript redirect statement.
         */
        
        if (!empty($_REQUEST['ajax_load'])) {
            ob_get_clean();
            $ajax_ret = array(
                'content' => "<script>SUGAR.ajaxUI.loadContent('$url');</script>\n",
                'menu' => array(
                    'module' => $_REQUEST['module'],
                    'label' => translate($_REQUEST['module']),
                ),
            );
            $json = getJSONobj();
            echo $json->encode($ajax_ret);
        } else {
            if (headers_sent()) {
                echo "<script>SUGAR.ajaxUI.loadContent('$url');</script>\n";
            } else {
                //@ob_end_clean(); // clear output buffer
                session_write_close();
                header('HTTP/1.1 301 Moved Permanently');
                header("Location: " . $url);
            }
        }
        if (!defined('SUITE_PHPUNIT_RUNNER')) {
            exit();
        }
    }

    /**
     * classic redirect to another URL, but check first that URL start with "Location:"...
     * @param $header_URL
     */
    public static function headerRedirect($header_URL)
    {
        if (preg_match('/\s*Location:\s*(.*)$/', $header_URL, $matches)) {
            $href = $matches[1];
            SugarApplication::redirect($href);
        } else {
            header($header_URL);
        }
    }

    /**
     * Storing messages into session
     *
     * @access	public
     * @param string $message
     */
    public static function appendErrorMessage($message)
    {
        self::appendMessage('user_error_message', $message);
    }

    /**
     * picking up the messages from the session and clearing session storage array
     * @return array messages
     */
    public static function getErrorMessages()
    {
        $messages = self::getMessages('user_error_message');
        return $messages;
    }

    /**
     * Storing messages into session
     *
     * @access	public
     * @param string $message
     */
    public static function appendSuccessMessage($message)
    {
        self::appendMessage('user_success_message', $message);
    }

    /**
     * picking up the messages from the session and clearing session storage array
     * @return array messages
     */
    public static function getSuccessMessages()
    {
        $messages = self::getMessages('user_success_message');
        return $messages;
    }

    /**
     * Storing messages into session
     * @param string $message
     */
    protected static function appendMessage($type, $message)
    {
        self::validateMessageType($type);

        if (empty($_SESSION[$type]) || !is_array($_SESSION[$type])) {
            $_SESSION[$type] = array();
        }
        if (!in_array($message, $_SESSION[$type])) {
            $_SESSION[$type][] = $message;
        }
    }

    /**
     * picking up the messages from the session and clearing session storage array
     * @return array messages
     */
    protected static function getMessages($type)
    {
        self::validateMessageType($type);

        if (isset($_SESSION[$type]) && is_array($_SESSION[$type])) {
            $msgs = $_SESSION[$type];
            unset($_SESSION[$type]);
            return $msgs;
        } else {
            return array();
        }
    }

    /**
     *
     * @param string $type possible message types: ['user_error_message', 'user_success_message']
     * @throws Exception message type should be valid
     */
    protected static function validateMessageType($type)
    {
        if (!in_array($type, array('user_error_message', 'user_success_message'))) {
            throw new Exception('Incorrect application message type: ' . $type);
        }
    }

    /**
     * Wrapper for the PHP setcookie() function, to handle cases where headers have
     * already been sent
     * @param $name
     * @param $value
     * @param int $expire
     * @param null $path
     * @param null $domain
     * @param bool $secure
     * @param bool $httponly
     */
    public static function setCookie(
        $name,
        $value,
        $expire = 0,
        $path = null,
        $domain = null,
        $secure = false,
        $httponly = true
    ) {
        if (isSSL()) {
            $secure = true;
        }
        if ($domain === null) {
            if (isset($_SERVER["HTTP_HOST"])) {
                $domain = preg_replace('/(\:\d+)$/', '', $_SERVER["HTTP_HOST"]); // Fix #9898 Invalid cookie domain when using non-standard HTTP Port.
            } else {
                $domain = 'localhost';
            }
        }

        $defaultCookiePath = ini_get('session.cookie_path');
        if ($path === null) {
            if(empty($defaultCookiePath)) {
                $path = '/';
            } else {
                $path = $defaultCookiePath;
            }
        }

        if (!headers_sent()) {
            setcookie($name, $value, $expire, $path, $domain, $secure, $httponly);
        }

        $_COOKIE[$name] = $value;
    }

    protected $redirectVars = array('module', 'action', 'record', 'token', 'oauth_token', 'mobile');

    /**
     * Create string to attach to login URL with vars to preserve post-login
     * @return string URL part with login vars
     */
    public function createLoginVars()
    {
        $ret = array();
        foreach ($this->redirectVars as $var) {
            if (!empty($this->controller->$var)) {
                $ret["login_" . $var] = $this->controller->$var;
                continue;
            }
            if (!empty($_REQUEST[$var])) {
                $ret["login_" . $var] = $_REQUEST[$var];
            }
        }
        if (isset($_REQUEST['mobile'])) {
            $ret['mobile'] = $_REQUEST['mobile'];
        }
        if (isset($_REQUEST['no_saml'])) {
            $ret['no_saml'] = $_REQUEST['no_saml'];
        }
        if (empty($ret)) {
            return '';
        }
        return "&" . http_build_query($ret);
    }

    /**
     * Get the list of vars passed with login form
     * @param bool $add_empty Add empty vars to the result?
     * @return array List of vars passed with login
     */
    public function getLoginVars($add_empty = true)
    {
        $ret = array();
        foreach ($this->redirectVars as $var) {
            if (!empty($_REQUEST['login_' . $var]) || $add_empty) {
                $ret["login_" . $var] = isset($_REQUEST['login_' . $var]) ? $_REQUEST['login_' . $var] : '';
            }
        }
        return $ret;
    }

    /**
     * Get URL to redirect after the login
     * @return string the URL to redirect to
     */
    public function getLoginRedirect()
    {
        $vars = array();
        foreach ($this->redirectVars as $var) {
            if (!empty($_REQUEST['login_' . $var])) {
                $vars[$var] = $_REQUEST['login_' . $var];
            }
        }

        if (isset($_REQUEST['mobile'])) {
            $vars['mobile'] = $_REQUEST['mobile'];
        }
        if (empty($vars)) {
            return "index.php?module=Home&action=index";
        } else {
            return "index.php?" . http_build_query($vars);
        }
    }
}
