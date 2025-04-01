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

require_once("modules/Calendar/CalendarUtils.php");

#[\AllowDynamicProperties]
class CalendarController extends SugarController
{

    /**
     * Bean that is being handled by the Calendar's current action.
     * @var SugarBean $currentBean
     */
    protected $currentBean = null;


    /**
     * Action SaveActivity
     */
    protected function action_saveactivity()
    {
        $this->view = 'json';

        if (!$this->retrieveCurrentBean('Save')) {
            return;
        }

        $module = $this->currentBean->module_dir;
        $bean = $this->currentBean;

        if (empty($_REQUEST['edit_all_recurrences'])) {
            $repeat_fields = array('type', 'interval', 'count', 'until', 'dow', 'parent_id');
            foreach ($repeat_fields as $suffix) {
                unset($_POST['repeat_' . $suffix]);
            }
        } elseif (!empty($_REQUEST['repeat_type']) && !empty($_REQUEST['date_start'])) {
            $params = array(
                    'type' => $_REQUEST['repeat_type'],
                    'interval' => $_REQUEST['repeat_interval'],
                    'count' => $_REQUEST['repeat_count'],
                    'until' => $_REQUEST['repeat_until'],
                    'dow' => $_REQUEST['repeat_dow'],
            );

            $repeatArr = CalendarUtils::build_repeat_sequence($_REQUEST['date_start'], $params);
            $limit = SugarConfig::getInstance()->get('calendar.max_repeat_count', 1000);

            if (count($repeatArr) > ($limit - 1)) {
                ob_clean();
                $jsonData = array(
                    'access' => 'yes',
                    'limit_error' => 'true',
                    'limit' => $limit,
                );
                $this->view_object_map['jsonData'] = $jsonData;
                return;
            }
        }



        $path = "modules/{$bean->module_dir}/{$bean->object_name}FormBase.php";
        if (!file_exists($path)) {
            $GLOBALS['log']->fatal("File {$bean->object_name}FormBase.php doesn't exist");
            sugar_cleanup(true);
        }

        require_once($path);

        $FBObjectName = "{$bean->object_name}FormBase";

        if (!class_exists($FBObjectName)) {
            $GLOBALS['log']->fatal("Class {$bean->object_name}FormBase doesn't exist");
            sugar_cleanup(true);
        }

        $formBase = new $FBObjectName();
        $bean = $formBase->handleSave('', false, false);
        unset($_REQUEST['send_invites'], $_POST['send_invites']); // prevent invites sending for recurring activities

        if ($record = $bean->id) {
            if ($module == "Meetings" || $module == "Calls") {
                if (!empty($_REQUEST['edit_all_recurrences'])) {
                    CalendarUtils::markRepeatDeleted($bean);
                }
                if (isset($repeatArr) && is_array($repeatArr) && count($repeatArr) > 0) {
                    $repeatCreated = CalendarUtils::save_repeat_activities($bean, $repeatArr);
                }
            }

            $bean->retrieve($record);
            $jsonData = CalendarUtils::get_sendback_array($bean);

            if (isset($repeatCreated) && is_array($repeatCreated)) {
                $jsonData = array_merge($jsonData, array('repeat' => $repeatCreated));
            }

            if (!empty($_REQUEST['edit_all_recurrences'])) {
                $jsonData['edit_all_recurrences'] = 'true';
            }
            if (!empty($jsonData['duration_hours']) && $jsonData['duration_hours'] %24 === 0) {
                $jsonData['allDay'] = 'true';
            }
        } else {
            $jsonData = array(
                'access' => 'no',
            );
        }

        $this->view_object_map['jsonData'] = $jsonData;
    }


    /**
     * Action QuickEdit
     */
    protected function action_quickedit()
    {
        $this->view = 'quickedit';

        if (!$this->retrieveCurrentBean('Detail')) {
            return;
        }

        $this->view_object_map['currentModule'] = $this->currentBean->module_dir;
        $this->view_object_map['currentBean'] = $this->currentBean;
    }

    protected function action_getUser()
    {
        $bean = BeanFactory::getBean("Users", $_REQUEST['record']);
        echo json_encode(array("user_name" => $bean->user_name, "full_name" =>  $bean->full_name));
        die();
    }
    /**
     * Action Reschedule
     * Used for drag & drop
     */
    protected function action_reschedule()
    {
        $this->view = 'json';

        $commit = true;

        if (!$this->retrieveCurrentBean('Save')) {
            return;
        }

        $_REQUEST['parent_name'] = $this->currentBean->parent_name;

        $dateField = "date_start";
        if ($this->currentBean->module_dir == "Tasks") {
            $dateField = "date_due";
        }
        if ($_REQUEST['allDay'] == true) {
            $endDateField = "date_end";
            list($tmp, $time) = explode(" ", $this->currentBean->$endDateField);
            list($date, $tmp) = explode(" ", $_REQUEST['enddatetime']);
            $_REQUEST[ $endDateField ] = $date . " " . $tmp;
            $_POST[$endDateField] = $_REQUEST[ $endDateField ];
        }

        if (!empty($_REQUEST['calendar_style']) && $_REQUEST['calendar_style'] == "basic") {
            list($tmp, $time) = explode(" ", $this->currentBean->$dateField);
            list($date, $tmp) = explode(" ", $_REQUEST['datetime']);
            $_POST['datetime'] = $date . " " . $tmp;
        }
        $_POST[$dateField] = $_REQUEST['datetime'];

        if ($this->currentBean->module_dir == "Tasks" && !empty($this->currentBean->date_start)) {
            if ($GLOBALS['timedate']->fromUser($_POST['date_due'])->ts < $GLOBALS['timedate']->fromUser($this->currentBean->date_start)->ts) {
                $this->view_object_map['jsonData'] = array(
                    'access' => 'no',
                    'errorMessage' => $GLOBALS['mod_strings']['LBL_DATE_END_ERROR'],
                );
                $commit = false;
            }
        }

        if ($commit) {
            require_once('include/formbase.php');
            $this->currentBean = populateFromPost("", $this->currentBean);
            $this->currentBean->save();
            $this->currentBean->retrieve($_REQUEST['record']);

            $this->view_object_map['jsonData'] = CalendarUtils::get_sendback_array($this->currentBean);
        }
    }

    /**
     * Action Remove
     */
    protected function action_remove()
    {
        $this->view = 'json';

        if (!$this->retrieveCurrentBean('Delete')) {
            return;
        }

        if ($this->currentBean->module_dir == "Meetings" || $this->currentBean->module_dir == "Calls") {
            if (!empty($_REQUEST['remove_all_recurrences']) && $_REQUEST['remove_all_recurrences']) {
                CalendarUtils::markRepeatDeleted($this->currentBean);
            }
        }

        $this->currentBean->mark_deleted($_REQUEST['record']);

        $this->view_object_map['jsonData'] = array(
            'access' => 'yes',
        );
    }

    /**
     * Action Resize
     * Used for drag & drop resizing
     */
    protected function action_resize()
    {
        $this->view = 'json';

        if (!$this->retrieveCurrentBean('Save')) {
            return;
        }

        require_once('include/formbase.php');
        $this->currentBean = populateFromPost("", $this->currentBean);
        $this->currentBean->save();

        $this->view_object_map['jsonData'] = array(
            'access' => 'yes',
        );
    }


    /**
     * Retrieves current activity bean and checks access to action
     *
     * @param string $actionToCheck
     * @return bool Result of check
     */
    protected function retrieveCurrentBean($actionToCheck = false)
    {
        $module = $_REQUEST['current_module'];
        $record = null;
        if (!empty($_REQUEST['record'])) {
            $record = $_REQUEST['record'];
        }

        require_once("data/BeanFactory.php");
        $this->currentBean = BeanFactory::getBean($module, $record);

        if (!empty($actionToCheck)) {
            if (!$this->currentBean->ACLAccess($actionToCheck)) {
                $this->view = 'json';
                $jsonData = array(
                    'access' => 'no',
                );
                $this->view_object_map['jsonData'] = $jsonData;
                return false;
            }
        }

        return true;
    }

    protected function action_getActivities()
    {
        $this->view = 'json';

        if (!ACLController::checkAccess('Calendar', 'list', true)) {
            ACLController::displayNoAccess(true);
        }

        require_once('modules/Calendar/Calendar.php');
        $cal = new Calendar($_REQUEST['view']);

        if (in_array($cal->view, array('day', 'week', 'month'))) {
            $cal->add_activities($GLOBALS['current_user']);
        } elseif ($cal->view == 'shared') {
            $cal->init_shared();
            $sharedUser = BeanFactory::newBean('Users');
            foreach ($cal->shared_ids as $member) {
                $sharedUser->retrieve($member);
                $cal->add_activities($sharedUser);
            }
        }
        $cal->load_activities();
        $this->view_object_map['jsonData'] = $cal->items;
    }
}
