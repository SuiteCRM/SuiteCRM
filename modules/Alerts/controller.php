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

#[\AllowDynamicProperties]
class AlertsController extends SugarController
{
    public function action_get()
    {
        global $current_user, $app_strings;
        $bean = BeanFactory::getBean('Alerts');

        $this->view_object_map['Flash'] = '';
        $this->view_object_map['Results'] = $bean->get_full_list("alerts.date_entered", "alerts.assigned_user_id = '".$current_user->id."' AND is_read != '1'");
        if ($this->view_object_map['Results'] == '') {
            $this->view_object_map['Flash'] =$app_strings['LBL_NOTIFICATIONS_NONE'];
        }
        $this->view = 'default';
    }

    public function action_add()
    {
        global $current_user;
        $name = null;
        $description = null;

        $assigned_user_id = $current_user->id;
        $is_read = 0;
        $url_redirect = null;
        $target_module = null;
        $reminder_id = '';
        $type = 'info';


        if (isset($_POST['name'])) {
            $name = $_POST['name'];
        }
        if (isset($_POST['description'])) {
            $description = $_POST['description'];
        }
        if (isset($_POST['is_read'])) {
            $is_read = $_POST['is_read'];
        }
        if (isset($_POST['url_redirect'])) {
            $url_redirect = $_POST['url_redirect'];
        } else {
            $url_redirect = null;
        }

        if ($url_redirect == null) {
            $url_redirect = 'index.php?fakeid='. uniqid('fake_', true);
        }

        if (isset($_POST['target_module'])) {
            $target_module = $_POST['target_module'];
        }
        if (isset($_POST['type'])) {
            $type = $_POST['type'];
        }
        if (isset($_POST['reminder_id'])) {
            $reminder_id = $_POST['reminder_id'];
        }

        $shouldShowReminderPopup = false;

        if (isset($_POST) && $reminder_id) {
            $bean = BeanFactory::getBean('Alerts');
            $result = $bean->get_full_list(
                "",
                "alerts.assigned_user_id = '" . $current_user->id . "' AND reminder_id = '" . $bean->db->quote($reminder_id) . "'"
            );
            if (empty($result)) {
                $bean = BeanFactory::newBean('Alerts');
                $bean->name = $name;
                $bean->description = $description;
                $bean->url_redirect = $url_redirect;
                $bean->target_module = $target_module;
                $bean->is_read = $is_read;
                $bean->assigned_user_id = $assigned_user_id;
                $bean->type = $type;
                $bean->reminder_id = $reminder_id;
                $bean->save();

                $shouldShowReminderPopup = true;
            }
        }

        $this->view_object_map['Flash'] = '';
        $this->view_object_map['Result'] = '';
        $this->view = 'ajax';

        echo json_encode(['result' => (int)$shouldShowReminderPopup], true);
    }

    public function action_markAsRead()
    {
        $bean = BeanFactory::getBean('Alerts', $_GET['record']);
        $bean->is_read = 1;
        $bean->save();

        $this->view = 'json';
    }

    public function action_redirect()
    {
        $bean = BeanFactory::getBean('Alerts', $_GET['record']);
        $redirect_url = $bean->url_redirect;
        $bean->is_read = 1;
        $bean->save();

        if ($redirect_url) {
            SugarApplication::redirect($redirect_url);
        }

        if (!empty($_SERVER['HTTP_REFERER'])) {
            SugarApplication::redirect($_SERVER['HTTP_REFERER']);
        }

        SugarApplication::redirect('index.php');
    }
}
