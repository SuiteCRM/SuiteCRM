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


require_once('modules/Users/authentication/SugarAuthenticate/SugarAuthenticate.php');

/**
 * This class handles OAuth authentication for users.
 */
#[\AllowDynamicProperties]
class OAuthAuthenticate extends SugarAuthenticate
{
    public $userAuthenticateClass = 'OAuthAuthenticateUser';
    public $authenticationDir = 'OAuthAuthenticate';
    private $provider = '';
    private $utilsClass = null;

    /**
     * Constructor for the OAuthAuthenticate class.
     * 
     * @param string $provider The OAuth provider name (e.g., 'Google', 'Microsoft').
     */
    public function __construct($provider = '')
    {
        $this->provider = $provider;
        $this->setUtilsClass($provider);
        parent::__construct();

    }

    /**
     * Sets the utility class for the specified OAuth provider.
     * 
     * @param string $provider The OAuth provider name.
     */
    protected function setUtilsClass($provider)
    {
        $utilsClass = $provider . 'Utils';
        $this->utilsClass = $utilsClass;
        if (file_exists('custom/modules/Users/authentication/OAuthAuthenticate/Providers/'.$provider.'/'.$utilsClass.'.php')) {
            require_once('custom/modules/Users/authentication/OAuthAuthenticate/Providers/'.$provider.'/'.$utilsClass.'.php');
        } else if (file_exists('modules/Users/authentication/OAuthAuthenticate/Providers/'.$provider.'/'.$utilsClass.'.php')) {
            require_once('modules/Users/authentication/OAuthAuthenticate/Providers/'.$provider.'/'.$utilsClass.'.php');
        } else {
            $this->utilsClass = '';
        }
    }

    /**
     * Determines whether to show the basic login form alongside OAuth options.
     * 
     * @return bool True if the basic login form should be shown, false otherwise.
     */
    public static function getShowBasicLoginForm()
    {
        global $sugar_config;
        if (isset($sugar_config['authentication_oauth_show_basic']) && $sugar_config['authentication_oauth_show_basic'] == true) {
            return true;
        }
        return false;

    }

    /**
     * Generates the admin template for OAuth settings.
     * 
     * @param object $ss The Smarty template engine instance.
     * @return string The rendered admin template HTML.
     */
    public static function getOAuthAdminTemplate($ss)
    {
        global $current_language, $sugar_config;

        $templateFile = get_custom_file_if_exists('modules/Users/authentication/OAuthAuthenticate/OAuthAdminTemplate.tpl');

        if (file_exists((string)$templateFile)) {
            $GLOBALS['log']->info("Found template: {$templateFile}\n");
            $ss->assign('OAUTH_LANG', return_module_language($current_language, 'Users'));
            $ss->assign('AUTHENTICATION_CLASS', $sugar_config['authenticationClass'] ?? 'SugarAuthenticate');
            return $ss->fetch($templateFile);
        } else {
            $GLOBALS['log']->info("WARNING: Template file not found at {$templateFile}\n");
        }
    }

    /**
     * Gets the login parameters for the current OAuth provider.
     * 
     * @return array The login parameters.
     */
    public function getLoginParams()
    {
        $utilsClass = $this->utilsClass;
        if ($utilsClass) {
            return $utilsClass::getLoginParams($this->getSettingsProvider());
        }
        return [];
    }

    /**
     * Retrieves the settings for the current OAuth provider from the configuration.
     * 
     * @return array The settings for the OAuth provider.
     */
    protected function getSettingsProvider() {
        global $sugar_config;
        $provider = $this->provider;
        if ($sugar_config['authenticationOauthProviders'] ?? false) {
            $oauthProviders = $sugar_config['authenticationOauthProviders'];
            if (isset($oauthProviders[$provider]) && isset($oauthProviders[$provider]['enabled']) && ($oauthProviders[$provider]['enabled'] == true || $oauthProviders[$provider]['enabled'] == 'on')) {
                return $sugar_config['authenticationOauthProviders'][$provider];
            }
        }
        return [];
    }

    /**
     * Generates the admin template for the current OAuth provider.
     * 
     * @param object $ss The Smarty template engine instance.
     * @return string The rendered admin template HTML.
     */
    public function getAdminTemplate(&$ss) {
        global $current_language;
        $provider = $this->provider;

        $templateFile = get_custom_file_if_exists('modules/Users/authentication/OAuthAuthenticate/Providers/' . $provider . '/' .$provider.'AdminTemplate.tpl');

        if (file_exists((string)$templateFile)) {
            $GLOBALS['log']->info("Found template: {$templateFile}\n");
            $ss->assign('OAUTH_LANG', return_module_language($current_language, 'Users'));
            $ss->assign('OAUTH_CONFIG', $this->utilsClass::getAdminParams($this->getSettingsProvider()));
            return $ss->fetch($templateFile);
        } else {
            $GLOBALS['log']->info("WARNING: Template file not found at {$templateFile}\n");
        }
    }

    /**
     * Generates the login template for the current OAuth provider.
     * 
     * @param object $ss The Smarty template engine instance.
     * @return string The rendered login template HTML.
     */
    public function getLoginTemplate(&$ss)
    {
        global $current_language;
        $provider = $this->provider;
        $loginTemplate = get_custom_file_if_exists('modules/Users/authentication/OAuthAuthenticate/Providers/'.$provider.'/'.$provider.'LoginTemplate.tpl');
        if (file_exists((string) $loginTemplate)) {
            $ss->assign('OAUTH_PARAMS', json_encode($this->getLoginParams()));
            $ss->assign('OAUTH_LANG', return_module_language($current_language, 'Users'));
            return $ss->fetch($loginTemplate);
        } else {
            return '';
        }
    }

    /**
     * Authenticates a user using OAuth or falls back to standard authentication.
     * This method is 
     * 
     * @param string $username The username.
     * @param string $password The password.
     * @param bool $fallback Whether to allow fallback to standard authentication.
     * @param array $PARAMS Additional parameters.
     * @return bool True if authentication is successful, false otherwise.
     */
    public function loginAuthenticate($username, $password, $fallback=false, $PARAMS = array())
    {
        global $mod_strings;
        unset($_SESSION['login_error']);
        unset($_REQUEST['login_token']);

        if (isset($_REQUEST['oauth_provider']) && !empty($_REQUEST['oauth_provider'])) {
            $this->provider = $_REQUEST['oauth_provider'];

            $this->__construct($this->provider);
            $payload = $this->utilsClass::verifyToken();

            if (is_array($payload) && isset($payload['email']) && !empty($payload['email'])) {
                $email = $payload['email'];

                $userBean = BeanFactory::newBean('Users');
	            $userBean->retrieve_by_email_address($payload['email']);

                if(empty($userBean->id)) {
                    $_SESSION['login_error'] = $mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_1'] . $email . $mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_2'];
                    return false;
                }
                if($userBean->status == 'Inactive') {
                    $_SESSION['login_error'] = $mod_strings['LBL_OAUTH_AUTH_ERR_INACTIVE_USER'];
                    return false;
                }
                if($userBean->portal_only == '1' || $userBean->is_group == '1') {
                    $_SESSION['login_error'] = $mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_USER_TYPE'];
                    return false;
                }
                $this->userAuthenticate->loadUserOnSession($userBean->id);

	            return $this->postLoginAuthenticate();

            } else {
                $_SESSION['login_error'] = $mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_TOKEN'];
                return false;
            }
        } else {
           return parent::loginAuthenticate($username, $password, $fallback, $PARAMS);
        }
    }
}
