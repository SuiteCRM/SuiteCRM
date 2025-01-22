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

require_once('include/MVC/View/SugarView.php');

#[\AllowDynamicProperties]
class CalendarViewSaveSettings extends SugarView
{
    public function CalendarViewSettings()
    {
        parent::__construct();
    }

    public function process()
    {
        $this->display();
    }

    public function display()
    {
        global $current_user;

        $dayStartMeridiem = $_REQUEST['day_start_meridiem'] ?? '';
        $dayEndMeridiem = $_REQUEST['day_end_meridiem'] ?? '';

        $db_start = $this->to_db_time($_REQUEST['day_start_hours'], $_REQUEST['day_start_minutes'], $dayStartMeridiem);
        $db_end = $this->to_db_time($_REQUEST['day_end_hours'], $_REQUEST['day_end_minutes'], $dayEndMeridiem);

        $current_user->setPreference('day_start_time', $db_start, 0, 'global', $current_user);
        $current_user->setPreference('day_end_time', $db_end, 0, 'global', $current_user);

        $current_user->setPreference('CalendarActivities', base64_encode(serialize($_POST['activity'])));

        $current_user->setPreference('calendar_display_timeslots', $_REQUEST['display_timeslots'], 0, 'global', $current_user);
        $current_user->setPreference('show_tasks', $_REQUEST['show_tasks'], 0, 'global', $current_user);
        $current_user->setPreference('show_calls', $_REQUEST['show_calls'], 0, 'global', $current_user);
        $current_user->setPreference('show_completed', $_REQUEST['show_completed'], 0, 'global', $current_user);
        $current_user->setPreference('calendar_display_shared_separate', $_REQUEST['shared_calendar_separate'], 0, 'global', $current_user);

        if (isset($_REQUEST['day']) && !empty($_REQUEST['day'])) {
            header("Location: index.php?module=Calendar&action=index&view=".$_REQUEST['view']."&hour=0&day=".$_REQUEST['day']."&month=".$_REQUEST['month']."&year=".$_REQUEST['year']);
        } else {
            header("Location: index.php?module=Calendar&action=index");
        }
    }

    private function to_db_time($hours, $minutes, $mer)
    {
        $hours = (int)$hours;
        $minutes = (int)$minutes;
        $mer = strtolower($mer);
        if (!empty($mer)) {
            if (($mer) == 'am') {
                if ($hours == 12) {
                    $hours = $hours - 12;
                }
            }
            if (($mer) == 'pm') {
                if ($hours != 12) {
                    $hours = $hours + 12;
                }
            }
        }
        if ($hours < 10) {
            $hours = "0".$hours;
        }
        if ($minutes < 10) {
            $minutes = "0".$minutes;
        }
        return $hours . ":". $minutes;
    }
}
