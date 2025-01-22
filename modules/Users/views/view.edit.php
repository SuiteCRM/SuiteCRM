<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


require_once('modules/Users/UserViewHelper.php');


#[\AllowDynamicProperties]
class UsersViewEdit extends ViewEdit
{
    public $useForSubpanel = true;
    public function __construct()
    {
        parent::__construct();
    }




    public function preDisplay()
    {
        $this->fieldHelper = new UserViewHelper($this->ss, $this->bean, 'EditView');
        $this->fieldHelper->setupAdditionalFields();

        parent::preDisplay();
    }

    public function getMetaDataFile()
    {
        $userType = 'Regular';
        if ($this->fieldHelper->usertype == 'GROUP') {
            $userType = 'Group';
        }

        if ($userType != 'Regular') {
            $oldType = $this->type;
            $this->type = $oldType.'group';
        }
        $metadataFile = parent::getMetaDataFile();
        if ($userType != 'Regular') {
            $this->type = $oldType;
        }

        return $metadataFile;
    }

    public function display()
    {
        global $current_user, $app_list_strings, $mod_strings;


        //lets set the return values
        if (isset($_REQUEST['return_module'])) {
            $this->ss->assign('RETURN_MODULE', $_REQUEST['return_module']);
        }

        $this->ss->assign('IS_ADMIN', $current_user->is_admin ? true : false);

        //make sure we can populate user type dropdown.  This usually gets populated in predisplay unless this is a quickeditform
        if (!isset($this->fieldHelper)) {
            $this->fieldHelper = new UserViewHelper($this->ss, $this->bean, 'EditView');
            $this->fieldHelper->setupAdditionalFields();
        }

        if (isset($_REQUEST['isDuplicate']) && $_REQUEST['isDuplicate'] == 'true') {
            $this->ss->assign('RETURN_MODULE', $_REQUEST['return_module']);
            $this->ss->assign('RETURN_ACTION', $_REQUEST['return_action']);
            $this->ss->assign('RETURN_ID', $_REQUEST['record']);
            $this->bean->id = "";
            $this->bean->user_name = "";
            $this->ss->assign('ID', '');
        } else {
            if (isset($_REQUEST['return_module'])) {
                $this->ss->assign('RETURN_MODULE', $_REQUEST['return_module']);
            } else {
                $this->ss->assign('RETURN_MODULE', $this->bean->module_dir);
            }

            $return_id = isset($_REQUEST['return_id'])?$_REQUEST['return_id']:$this->bean->id;
            if (isset($return_id)) {
                $return_action = isset($_REQUEST['return_action'])?$_REQUEST['return_action']:'DetailView';
                $this->ss->assign('RETURN_ID', $return_id);
                $this->ss->assign('RETURN_ACTION', $return_action);
            }
        }


        ///////////////////////////////////////////////////////////////////////////////
        ////	REDIRECTS FROM COMPOSE EMAIL SCREEN
        if (isset($_REQUEST['type']) && (isset($_REQUEST['return_module']) && $_REQUEST['return_module'] == 'Emails')) {
            $this->ss->assign('REDIRECT_EMAILS_TYPE', $_REQUEST['type']);
        }
        ////	END REDIRECTS FROM COMPOSE EMAIL SCREEN
        ///////////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////////
        ////	NEW USER CREATION ONLY
        if (empty($this->bean->id)) {
            $this->ss->assign('SHOW_ADMIN_CHECKBOX', 'height="30"');
            $this->ss->assign('NEW_USER', '1');
        } else {
            $this->ss->assign('NEW_USER', '0');
            $this->ss->assign('NEW_USER_TYPE', 'DISABLED');
        }

        ////	END NEW USER CREATION ONLY
        ///////////////////////////////////////////////////////////////////////////////


        // FIXME: Translate error prefix
        if (isset($_REQUEST['error_string'])) {
            LoggerManager::getLogger()->warn('Using error string in request is deprecated: ' . $_REQUEST[
                'error_string']);
            $this->ss->assign('ERROR_STRING', '<span class="error">Error: ' . $mod_strings['LBL_ERROR'] . '</span>');
        }
        if (isset($_REQUEST['error_password'])) {
            LoggerManager::getLogger()->warn('Using password error in request is deprecated: ' . $_REQUEST[
                'error_password']);
            $this->ss->assign('ERROR_PASSWORD', '<span id="error_pwd" class="error">Error: ' . $mod_strings['LBL_ERROR'
                ] . '</span>');
        }




        // Build viewable versions of a few fields for non-admins
        if (!empty($this->bean->id)) {
            if (!empty($this->bean->status)) {
                $this->ss->assign('STATUS_READONLY', $app_list_strings['user_status_dom'][$this->bean->status]);
            }
            if (!empty($this->bean->employee_status)) {
                $this->ss->assign('EMPLOYEE_STATUS_READONLY', $app_list_strings['employee_status_dom'][$this->bean->employee_status]);
            }
            if (!empty($this->bean->reports_to_id)) {
                $reportsToUser = get_assigned_user_name($this->bean->reports_to_id);
                $reportsToUserField = "<input type='text' name='reports_to_name' id='reports_to_name' value='{$reportsToUser}' disabled>\n";
                $reportsToUserField .= "<input type='hidden' name='reports_to_id' id='reports_to_id' value='{$this->bean->reports_to_id}'>";
                $this->ss->assign('REPORTS_TO_READONLY', $reportsToUserField);
            }
            if (!empty($this->bean->title)) {
                $this->ss->assign('TITLE_READONLY', $this->bean->title);
            }
            if (!empty($this->bean->department)) {
                $this->ss->assign('DEPT_READONLY', $this->bean->department);
            }
        }

        $processSpecial = false;
        $processFormName = '';
        if (isset($this->fieldHelper->usertype) && (
            $this->fieldHelper->usertype == 'GROUP'
            )) {
            $this->ev->formName = 'EditViewGroup';

            $processSpecial = true;
            $processFormName = 'EditViewGroup';
        }

        //Bug#51609 Replace {php} code block in EditViewHeader.tpl
        $action_button = array();
        $APP = $this->ss->get_template_vars('APP');
        $PWDSETTINGS = $this->ss->get_template_vars('PWDSETTINGS');
        $REGEX = $this->ss->get_template_vars('REGEX');
        $CHOOSER_SCRIPT = $this->ss->get_template_vars('CHOOSER_SCRIPT');
        $REASSIGN_JS = $this->ss->get_template_vars('REASSIGN_JS');
        $RETURN_ACTION = $this->ss->get_template_vars('RETURN_ACTION');
        $RETURN_MODULE = $this->ss->get_template_vars('RETURN_MODULE');
        $RETURN_ID = $this->ss->get_template_vars('RETURN_ID');

        $minpwdlength = !empty($PWDSETTINGS['minpwdlength']) ? $PWDSETTINGS['minpwdlength'] : '';
        $maxpwdlength =  !empty($PWDSETTINGS['maxpwdlength']) ? $PWDSETTINGS['maxpwdlength'] : '';
        $action_button_header = [];
        $action_button_header[] = <<<EOD
                    <input type="button" id="SAVE_HEADER" title="{$APP['LBL_SAVE_BUTTON_TITLE']}" accessKey="{$APP['LBL_SAVE_BUTTON_KEY']}"
                          class="button primary" onclick="var _form = $('#EditView')[0]; if (!set_password(_form,newrules('{$minpwdlength}','{$maxpwdlength}','{$REGEX}'))) return false; if (!Admin_check()) return false; _form.action.value='Save'; {$CHOOSER_SCRIPT} {$REASSIGN_JS} if(verify_data(EditView)) _form.submit();"
                          name="button" value="{$APP['LBL_SAVE_BUTTON_LABEL']}">
EOD
        ;
        $action_button_header[] = <<<EOD
                    <script>
                       $('#EditView').submit(function(){
                                            var theForm =$('#EditView');
                                           if (!set_password(theForm[0],newrules('{$minpwdlength}','{$maxpwdlength}','{$REGEX}'))){
                                             return false;
                                           }
                                           if (!Admin_check()){
                                                return false;
                                            }
                                             $('#EditView input[name=action]').val('save');
                                            return true;
                        });
                     </script>
EOD
        ;
        $action_button_header[] = <<<EOD
                    <input	title="{$APP['LBL_CANCEL_BUTTON_TITLE']}" id="CANCEL_HEADER" accessKey="{$APP['LBL_CANCEL_BUTTON_KEY']}"
                              class="button" onclick="var _form = $('#EditView')[0]; _form.action.value='{$RETURN_ACTION}'; _form.module.value='{$RETURN_MODULE}'; _form.record.value='{$RETURN_ID}'; _form.submit()"
                              type="button" name="button" value="{$APP['LBL_CANCEL_BUTTON_LABEL']}">
EOD
        ;
        $action_button_header = array_merge($action_button_header, $this->ss->get_template_vars('BUTTONS_HEADER'));
        $this->ss->assign('ACTION_BUTTON_HEADER', $action_button_header);

        $action_button_footer = [];
        $action_button_footer[] = <<<EOD
                    <input type="button" id="SAVE_FOOTER" title="{$APP['LBL_SAVE_BUTTON_TITLE']}" accessKey="{$APP['LBL_SAVE_BUTTON_KEY']}"
                          class="button primary" onclick="var _form = $('#EditView')[0]; if (!set_password(_form,newrules('{$minpwdlength}','{$maxpwdlength}','{$REGEX}'))) return false; if (!Admin_check()) return false; _form.action.value='Save'; {$CHOOSER_SCRIPT} {$REASSIGN_JS} if(verify_data(EditView)) _form.submit();"
                          name="button" value="{$APP['LBL_SAVE_BUTTON_LABEL']}">
EOD
        ;
        $action_button_footer[] = <<<EOD
                    <input	title="{$APP['LBL_CANCEL_BUTTON_TITLE']}" id="CANCEL_FOOTER" accessKey="{$APP['LBL_CANCEL_BUTTON_KEY']}"
                              class="button" onclick="var _form = $('#EditView')[0]; _form.action.value='{$RETURN_ACTION}'; _form.module.value='{$RETURN_MODULE}'; _form.record.value='{$RETURN_ID}'; _form.submit()"
                              type="button" name="button" value="{$APP['LBL_CANCEL_BUTTON_LABEL']}">
EOD
        ;
        $action_button_footer = array_merge($action_button_footer, $this->ss->get_template_vars('BUTTONS_FOOTER'));
        $this->ss->assign('ACTION_BUTTON_FOOTER', $action_button_footer);

        //if the request object has 'scrolltocal' set, then we are coming here from the tour window box and need to set flag to true
        // so that footer.tpl fires off script to scroll to calendar section
        if (!empty($_REQUEST['scrollToCal'])) {
            $this->ss->assign('scroll_to_cal', true);
        }



        $this->ss->assign('SUBTHEMES', $this->bean->getSubThemes());
        $this->ss->assign('SUBTHEME', $this->bean->getSubTheme());


        require_once('modules/Emails/EmailUI.php');
        $efocus = BeanFactory::newBean('Emails');
        $efocus->email2init();
        //$efocus->et->preflightUser($current_user);
        $out = $efocus->et->displayEmailFrame('modules/Users/_baseEmail.tpl');
        echo $out;
        echo "<script>var composePackage = null;</script>";

        $this->ev->process($processSpecial, $processFormName);

        echo $this->ev->display($this->showTitle);
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
