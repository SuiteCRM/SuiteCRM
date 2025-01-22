<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2023 SalesAgility Ltd.
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

require_once('modules/Users/UserViewHelper.php');

#[\AllowDynamicProperties]
class UsersViewDetail extends ViewDetail
{
    public function __construct()
    {
        parent::__construct();
    }




    public function preDisplay()
    {
        global $current_user, $app_strings, $sugar_config;

        if (!isset($this->bean->id)) {
            // No reason to set everything up just to have it fail in the display() call
            return;
        }
        /**
         *  taken from parent::preDisplay by change the template file.
         */
        $metadataFile = $this->getMetaDataFile();
        $this->dv = new DetailView2();
        $this->dv->ss =& $this->ss;
        $this->dv->setup(
            $this->module,
            $this->bean,
            $metadataFile,
            get_custom_file_if_exists('modules/Users/tpls/DetailView.tpl')
        );
        /****/
        $viewHelper = new UserViewHelper($this->ss, $this->bean, 'DetailView');
        $viewHelper->setupAdditionalFields();

        $errors = "";
        $msgGood = false;
        if (isset($_REQUEST['pwd_set']) && $_REQUEST['pwd_set'] != 0) {
            if ($_REQUEST['pwd_set'] == '4') {
                require_once('modules/Users/password_utils.php');
                $errors .= canSendPassword();
            } else {
                $errors .= translate('LBL_NEW_USER_PASSWORD_3' . $_REQUEST['pwd_set'], 'Users');
                $msgGood = true;
            }
        } else {
            //IF BEAN USER IS LOCKOUT
            if ($this->bean->getPreference('lockout') == '1') {
                $errors .= translate('ERR_USER_IS_LOCKED_OUT', 'Users');
            }
        }
        $this->ss->assign("ERRORS", $errors);
        $this->ss->assign(
            "ERROR_MESSAGE",
            $msgGood ? translate('LBL_PASSWORD_SENT', 'Users') : translate('LBL_CANNOT_SEND_PASSWORD', 'Users')
        );
        $buttons = array();

        $this->ss->assign("CURRENT_USER_ID", $GLOBALS['current_user']->id);

        if ((
            is_admin($current_user) || $_REQUEST['record'] == $current_user->id
            )
            && !empty($sugar_config['default_user_name'])
            && $sugar_config['default_user_name'] == $this->bean->user_name
            && isset($sugar_config['lock_default_user_name'])
            && $sugar_config['lock_default_user_name']) {
            $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' =>"<input id='edit_button' accessKey='".$app_strings['LBL_EDIT_BUTTON_KEY']."' name='Edit' title='".$app_strings['LBL_EDIT_BUTTON_TITLE']."' value='".$app_strings['LBL_EDIT_BUTTON_LABEL']."' onclick=\"this.form.return_module.value='Users'; this.form.return_action.value='DetailView'; this.form.return_id.value='".'{$fields.id.value}'."'; this.form.action.value='EditView'\" type='submit' value='" . $app_strings['LBL_EDIT_BUTTON_LABEL'] .  "'>");
        } elseif (is_admin($current_user)|| ($GLOBALS['current_user']->isAdminForModule('Users')&& !$this->bean->is_admin)
            || $_REQUEST['record'] == $current_user->id) {
            $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' => "<input title='".$app_strings['LBL_EDIT_BUTTON_TITLE']."' accessKey='".$app_strings['LBL_EDIT_BUTTON_KEY']."' name='Edit' id='edit_button' value='".$app_strings['LBL_EDIT_BUTTON_LABEL']."' onclick=\"this.form.return_module.value='Users'; this.form.return_action.value='DetailView'; this.form.return_id.value='".'{$fields.id.value}'."'; this.form.action.value='EditView'\" type='submit' value='" . $app_strings['LBL_EDIT_BUTTON_LABEL'] .  "'>");
            if ((
                is_admin($current_user)|| $GLOBALS['current_user']->isAdminForModule('Users')
            )) {
                if (!$current_user->is_group) {
                    $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' => "<input id='duplicate_button' title='" . $app_strings['LBL_DUPLICATE_BUTTON_TITLE'] . "' accessKey='" . $app_strings['LBL_DUPLICATE_BUTTON_KEY'] . "' class='button' onclick=\"this.form.return_module.value='Users'; this.form.return_action.value='DetailView'; this.form.isDuplicate.value=true; this.form.action.value='EditView'\" type='submit' name='Duplicate' value='" . $app_strings['LBL_DUPLICATE_BUTTON_LABEL'] . "'>");

                    $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' => "<input name='delete' id='delete_button' title='" . $app_strings['LBL_DELETE_BUTTON_LABEL'] . "' type='button' class='button' onclick='confirmDelete();' value='" . $app_strings['LBL_DELETE_BUTTON_LABEL'] . "'>");

                    if (!$this->bean->portal_only && !$this->bean->is_group && !$this->bean->external_auth_only
                        && isset($sugar_config['passwordsetting']['SystemGeneratedPasswordON']) && $sugar_config['passwordsetting']['SystemGeneratedPasswordON']) {
                        $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' => '<input title="'.translate('LBL_GENERATE_PASSWORD_BUTTON_TITLE', 'Users').'" class="button" LANGUAGE=javascript onclick="generatepwd(\'{$fields.id.value}\');" type="button" name="password" value="'.translate('LBL_GENERATE_PASSWORD_BUTTON_LABEL', 'Users').'">"');
                    }
                }
            }
        }


        $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' => '<input title="'.translate('LBL_RESET_PREFERENCES', 'Users').'" class="button" LANGUAGE=javascript onclick="if(confirm(\''.translate('LBL_RESET_PREFERENCES_WARNING_USER', 'Users').'\')) window.location=\'index.php?module=Users&action=resetPreferences&reset_preferences=true&record={$fields.id.value}\';" type="button" name="password" value="'.translate('LBL_RESET_PREFERENCES', 'Users').'">"');
        $this->dv->defs['templateMeta']['form']['buttons'][] = array('customCode' => '<input title="'.translate('LBL_RESET_HOMEPAGE', 'Users').'" class="button" LANGUAGE=javascript onclick="if(confirm(\''.translate('LBL_RESET_HOMEPAGE_WARNING', 'Users').'\')) window.location=\'index.php?module=Users&action=DetailView&reset_homepage=true&record={$fields.id.value}\';" type="button" name="password" value="'.translate('LBL_RESET_HOMEPAGE', 'Users').'">"');

        $show_roles = (!($this->bean->is_group == '1' || $this->bean->portal_only == '1'));
        if (is_admin($this->bean)) {
            $show_roles = false;
        }

        $this->ss->assign('SHOW_ROLES', $show_roles);
        //Mark whether or not the user is a group or portal user
        $this->ss->assign(
            'IS_GROUP_OR_PORTAL',
            ($this->bean->is_group == '1' || $this->bean->portal_only == '1') ? true : false
        );
        if ($show_roles) {
            ob_start();

            require_once('modules/ACLRoles/DetailUserAccess.php');


            $role_html = ob_get_contents();
            ob_end_clean();
            $this->ss->assign('ROLE_HTML', $role_html);
        }
    }

    public function getMetaDataFile()
    {
        $userType = 'Regular';
        if ($this->bean->is_group == 1) {
            $userType = 'Group';
        }

        if ($userType != 'Regular') {
            $oldType = $this->type;
            $this->type = $oldType . 'group';
        }
        $metadataFile = parent::getMetaDataFile();
        if ($userType != 'Regular') {
            $this->type = $oldType;
        }

        return $metadataFile;
    }

    public function display()
    {
        if ($this->bean->portal_only == 1 || $this->bean->is_group == 1) {
            $this->options['show_subpanels'] = false;
            $this->dv->formName = 'DetailViewGroup';
            $this->dv->view = 'DetailViewGroup';
        }

        //handle request to reset the homepage
        if (isset($_REQUEST['reset_homepage'])) {
            $this->bean->resetPreferences('Home');
            global $current_user;
            if ($this->bean->id == $current_user->id) {
                $_COOKIE[$current_user->id . '_activePage'] = '0';
                SugarApplication::setCookie($current_user->id . '_activePage', '0', 3000, null, null, isSSL(), true);
            }
        }

        if (empty($this->bean->id)) {
            sugar_die($GLOBALS['app_strings']['ERROR_NO_RECORD']);
        }
        $this->dv->process();
        echo $this->dv->display();
    }


    /**
     * getHelpText
     *
     * This is a protected function that returns the help text portion.  It is called from getModuleTitle.
     * We override the function from SugarView.php to make sure the create link only appears if the current user
     * meets the valid criteria.
     *
     * @param $module String the formatted module name
     * @return $theTitle String the HTML for the help text
     */
    protected function getHelpText($module)
    {
        $theTitle = '';

        if ($GLOBALS['current_user']->isAdminForModule('Users')
        ) {
            $createImageURL = SugarThemeRegistry::current()->getImageURL('create-record.gif');
            $url = ajaxLink("index.php?module=$module&action=EditView&return_module=$module&return_action=DetailView");
            $theTitle = <<<EOHTML
&nbsp;
<img src='{$createImageURL}' alt='{$GLOBALS['app_strings']['LNK_CREATE']}'>
<a href="{$url}" class="utilsLink">
{$GLOBALS['app_strings']['LNK_CREATE']}
</a>
EOHTML;
        }

        return $theTitle;
    }
}
