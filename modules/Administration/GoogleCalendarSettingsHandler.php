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
class GoogleCalendarSettingsHandler extends BaseHandler
{
    /**
     *
     * @var Configurator
     */
    protected $configurator = null;

    /**
     *
     * @var javascript
     */
    protected $js = null;

    /**
     *
     * @var User
     */
    protected $currentUser = null;

    /**
     * Setup Object
     *
     * @param string       $tpl_path
     * @param User         $current_user
     * @param array        $request
     * @param array        $mod_strings
     * @param Configurator $config
     * @param Sugar_Smarty $sugar_smarty
     * @param javascript   $js
     */
    public function __construct($tpl_path, User $current_user, $request, $mod_strings, Configurator $config, Sugar_Smarty $sugar_smarty, javascript $js)
    {
        // Get parent
        parent::__construct($sugar_smarty, $request, $mod_strings);

        $this->currentUser  = $current_user;
        $this->tplPath      = $tpl_path;
        $this->js           = $js;
        $this->configurator = $config;

        $this->checkUserIsAdmin();

        $this->doActions();
        $this->handleDisplay();
    }

    /**
     * Check the current user is admin
     *
     * @return void
     */
    protected function checkUserIsAdmin()
    {
        // Check current user is admin
        if (!is_admin($this->currentUser)) {
            $this->protectedDie("Unauthorized access to administration.");
        }
    }

    /**
     * Deal with do actions
     *
     * @return void
     */
    protected function doActions()
    {
        if (isset($this->request['do']) && $this->request['do'] == 'save') {
            $this->configurator->config['google_auth_json'] = !empty($this->request['google_auth_json']);
            $this->configurator->saveConfig();
            $this->redirect('index.php?module=Administration&action=index');
            $this->protectedExit();
        }
    }

    /**
     * This function handles displaying the template
     *
     * @return void
     */
    public function handleDisplay()
    {
        global $app_strings;
        $this->ss->assign('PAGE_TITLE', $this->getPageTitle());
        $this->ss->assign('APP', $app_strings);
        $this->ss->assign('MOD', $this->modStrings);

        $this->getJavascript();
        $this->getGoogleCalendarAuthState();

        $this->ss->display($this->tplPath);
    }

    /**
     * Get the page title
     *
     * @return string
     */
    protected function getPageTitle()
    {
        return getClassicModuleTitle(
            "Administration",
            array(
                "<a href='index.php?module=Administration&action=index'>" . translate('LBL_MODULE_NAME', 'Administration') . "</a>",
                $this->modStrings['LBL_GOOGLE_AUTH_TITLE'],
            ),
            false
        );
    }

    /**
     * Get the google calendar authentication state
     *
     * @return void
     */
    protected function getGoogleCalendarAuthState()
    {
        // Get the config
        $this->getConfig();

        // Check for Google Sync JSON
        $json = base64_decode($this->configurator->config['google_auth_json']);
        $gcConfig = json_decode($json);

        $googleJsonConfState = array(
            'status' => 'UNCONFIGURED',
            'color'  => 'black'
        );

        if ($gcConfig) {
            $googleJsonConfState = array(
                'status' => 'CONFIGURED',
                'color'  => 'green'
            );
        }

        $this->ss->assign('GOOGLE_JSON_CONF', $googleJsonConfState);
    }

    /**
     * Get the config
     *
     * @return void
     */
    protected function getConfig()
    {
        if (!array_key_exists('google_auth_json', $this->configurator->config)) {
            $this->configurator->config['google_auth_json'] = false;
        }

        $this->ss->assign('config', $this->configurator->config['google_auth_json']);
    }

    /**
     * Get the javascript
     *
     * @return void
     */
    protected function getJavascript()
    {
        $this->js->setFormName('ConfigureSettings');
        $js = $this->js->getScript();
        $this->ss->assign('JAVASCRIPT', $js);
    }
}
