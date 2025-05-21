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

require_once __DIR__ . '/../../include/OutboundEmail/OutboundEmail.php';
require_once __DIR__ . '/../../modules/UserPreferences/UserPreference.php';

#[\AllowDynamicProperties]
class UsersController extends SugarController
{
    /**
     * bug 48170
     * Action resetPreferences gets fired when user clicks on  'Reset User Preferences' button
     * This action is set in UserViewHelper.php
     */
    protected function action_resetPreferences()
    {
        if ($_REQUEST['record'] == $GLOBALS['current_user']->id || ($GLOBALS['current_user']->isAdminForModule('Users'))) {
            $u = BeanFactory::newBean('Users');
            $u->retrieve($_REQUEST['record']);
            $u->resetPreferences();
            if ($u->id == $GLOBALS['current_user']->id) {
                SugarApplication::redirect('index.php');
            } else {
                SugarApplication::redirect("index.php?module=Users&record=" . $_REQUEST['record'] . "&action=DetailView"); //bug 48170]
            }
        }
    }

    private function currentUserEqualsRecordUser() {
        return $_REQUEST['record'] === $GLOBALS['current_user']->id;
    }

    protected function action_delete()
    {
        global $app_strings, $mod_strings;

        if (!$this->currentUserEqualsRecordUser() && (
            $GLOBALS['current_user']->isAdminForModule('Users')
            )
        ) {
            $user = BeanFactory::newBean('Users');
            $user->retrieve($_REQUEST['record']);
            $user->status = 'Inactive';
            $user->employee_status = 'Terminated';
            $user->save();
            $user->mark_deleted($user->id);
            $GLOBALS['log']->info("User id: {$GLOBALS['current_user']->id} deleted user record: {$_REQUEST['record']}");

            $eapm = loadBean('EAPM');
            $eapm->delete_user_accounts($_REQUEST['record']);
            $GLOBALS['log']->info("Removing user's External Accounts");

            SugarApplication::redirect("index.php?module=Users&action=index");
        } else {
            if ($this->currentUserEqualsRecordUser()) {
                sugar_die($mod_strings['ERR_DELETE_USER']);
            } else {
                sugar_die($app_strings['ERR_NOT_ADMIN']);
            }
        }
    }

    protected function action_wizard()
    {
        $this->view = 'wizard';
    }

    protected function action_saveuserwizard()
    {
        global $current_user, $sugar_config;

        // set all of these default parameters since the Users save action will undo the defaults otherwise
        $_POST['record'] = $current_user->id;
        $_POST['is_admin'] = ($current_user->is_admin ? 'on' : '');
        $_POST['use_real_names'] = true;
        $_POST['reminder_checked'] = '1';
        $_POST['email_reminder_checked'] = '1';
        $_POST['reminder_time'] = 1800;
        $_POST['email_reminder_time'] = 3600;
        $_POST['mailmerge_on'] = 'on';
        $_POST['receive_notifications'] = $current_user->receive_notifications;
        $_POST['user_theme'] = (string)SugarThemeRegistry::getDefault();

        // Will pull in the users details from first page of the wizard
        if (!empty($_POST['first_name'])) {
            $current_user->first_name = ($_POST['first_name']);
        }
        if (!empty($_POST['last_name'])) {
            $current_user->last_name = ($_POST['last_name']);
        }
        if (!empty($_POST['email1'])) {
            $current_user->email1 = ($_POST['email1']);
        }
        if (!empty($_POST['phone_work'])) {
            $current_user->phone_work = ($_POST['phone_work']);
        }
        if (!empty($_POST['phone_mobile'])) {
            $current_user->phone_mobile = ($_POST['phone_mobile']);
        }
        if (!empty($_POST['messenger_type'])) {
            $current_user->messenger_type = ($_POST['messenger_type']);
        }
        if (!empty($_POST['messenger_id'])) {
            $current_user->messenger_id = ($_POST['messenger_id']);
        }
        if (!empty($_POST['address_street'])) {
            $current_user->address_street = ($_POST['address_street']);
        }
        if (!empty($_POST['address_city'])) {
            $current_user->address_city = ($_POST['address_city']);
        }
        if (!empty($_POST['address_state'])) {
            $current_user->address_state = ($_POST['address_state']);
        }
        if (!empty($_POST['address_postalcode'])) {
            $current_user->address_postalcode = ($_POST['address_postalcode']);
        }
        if (!empty($_POST['address_country'])) {
            $current_user->address_country = ($_POST['address_country']);
        }

        // Saves User Details ONLY
        $current_user->save();


        // Will pull in the users Preferences from second page of the wizard
        if (!empty($_POST['timezone'])) {
            $current_user->setPreference('timezone', $_POST['timezone'],
                0, 'global');
        }
        if (!empty($_POST['dateformat'])) {
            $current_user->setPreference('dateformat', $_POST['dateformat'],
                0, 'global');
        }
        if (!empty($_POST['timeformat'])) {
            $current_user->setPreference('timeformat', $_POST['timeformat'],
                0, 'global');
        }
        if (!empty($_POST['currency'])) {
            $current_user->setPreference('currency', $_POST['currency'],
                0, 'global');
        }
        if (!empty($_POST['default_currency_significant_digits'])) {
            $current_user->setPreference('default_currency_significant_digits',
                $_POST['default_currency_significant_digits'], 0, 'global');
        }
        if (!empty($_POST['dec_sep'])) {
            $current_user->setPreference('dec_sep', $_POST['dec_sep'],
                0, 'global');
        }
        if (!empty($_POST['num_grp_sep'])) {
            $current_user->setPreference('num_grp_sep', $_POST['num_grp_sep'],
                0, 'global');
        }
        if (!empty($_POST['default_locale_name_format'])) {
            $current_user->setPreference('default_locale_name_format',
                $_POST['default_locale_name_format'], 0, 'global');
        }

        $current_user->setPreference('language', $_POST['user_language'], 0, 'global');
        $_SESSION['authenticated_user_language'] = $_POST['user_language'];

        $next = $_POST['whatnext'] ?? '';

        $base = 'index.php?action=index&module=Home';
        $nextActions = [
            'users' => 'index.php?action=index&module=Users' ,
            'finish' =>  'index.php?action=index&module=Home',
            'settings' => 'index.php?action=index&module=Administration',
            'studio' => 'index.php?action=index&module=ModuleBuilder?type=studio',
            'import' => 'index.php?module=Import&action=step1&import_module=Administration',
        ];

        $returnUrl = $nextActions[$next] ?? $base;

        // redirect to home
        SugarApplication::redirect($returnUrl);

    }

    protected function action_saveftsmodules()
    {
        $this->view = 'fts';
        $GLOBALS['current_user']->setPreference('fts_disabled_modules', $_REQUEST['disabled_modules']);
    }


    protected function action_editview()
    {
        $this->view = 'edit';
        if (!(is_admin($GLOBALS['current_user']) || $_REQUEST['record'] == $GLOBALS['current_user']->id)) {
            SugarApplication::redirect("index.php?module=Home&action=index");
        }
    }

    protected function action_detailview()
    {
        $this->view = 'detail';
        if (!(is_admin($GLOBALS['current_user']) || $_REQUEST['record'] == $GLOBALS['current_user']->id)) {
            SugarApplication::redirect("index.php?module=Home&action=index");
        }
    }
}
