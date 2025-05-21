<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

require_once 'modules/Calendar/CalendarDisplay.php';

class CustomCalendarDisplay extends CalendarDisplay
{
    /**
     * colors of items on calendar
     */
    public $activity_colors = array(
        'Meetings' => array(
            'border' => '87719C',
            'body' => '6B5171',
            'text' => 'E5E5E5',
        ),
        'Calls' => array(
            'border' => '487166',
            'body' => '72B3A1',
            'text' => 'E5E5E5',
        ),
        'Tasks' => array(
            'border' => '515A71',
            'body' => '707C9C',
            'text' => 'E5E5E5',
        ),
        'Project' => array(
            'border' => '699DC9',
            'body' => '557FA3',
            'text' => 'E5E5E5',
        ),
        'ProjectTask' => array(
            'border' => '83C489',
            'body' => '659769',
            'text' => 'E5E5E5',
        ),
        'stic_Sessions' => array(
            'border' => 'C29B8A',
            'body' => '7D6459',
            'text' => 'E5E5E5',
        ),
        'stic_FollowUps' => array(
            'border' => 'A29B8E',
            'body' => 'AD645E',
            'text' => 'E5E5EE',
        ),
        // STIC-Custom 20240222 MHP - Adding colors to Work Calendar items
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/114
        'stic_Work_Calendar' => array(
            'border_working' => '0A2407',
            'body_working' => '1C6114',
            'text_working' => 'E5E5EE',
            'border_noworking' => '170000',
            'body_noworking' => 'D60000',
            'text_noworking' => 'E5E5EE',        
        ),
        // END STIC-Custom      
    );

    // STIC-Custom 20240222 MHP - Overriding to add the stic_Work_Calendar properties
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/114
    public function checkActivity($activity = "")
    {
        global $current_user;
        if (empty($activity)) {
            $activity = $this->activity_colors;
        }
        $newActivities = unserialize(base64_decode((string)$current_user->getPreference("CalendarActivities")));
        if ($newActivities) {
            $activity = array_merge($activity, $newActivities);
        }
        foreach ($activity as $key => $activityItem) {
            if (isset($GLOBALS['app_list_strings']['moduleList'][ $key ]) && !empty($GLOBALS['app_list_strings']['moduleList'][ $key ]) && !empty($this->cal->activityList[ $key ])) {
                if ($key != 'stic_Work_Calendar') {
                    $activity[ $key ]['label'] = $GLOBALS['app_list_strings']['moduleList'][ $key ];
                } else {
                    $activity[ $key ]['label_working'] = translate('LBL_SETTINGS_STIC_WORK_CALENDAR_WORKING','Calendar');
                    $activity[ $key ]['label_noworking'] = translate('LBL_SETTINGS_STIC_WORK_CALENDAR_NOWORKING','Calendar');
                }
            } else {
                unset($activity[ $key ]);
            }
        }
        if (isset($activity) && !empty($activity)) {
            $this->activity_colors = $activity;
        }
        if (!empty($this->cal->activityList)) {
            foreach ($this->cal->activityList as $key=>$value) {
                if (isset($GLOBALS['beanList'][$key]) && !empty($GLOBALS['beanList'][$key]) && !isset($this->activity_colors[ $key ])) {
                    $this->activity_colors[ $key ] = $GLOBALS['sugar_config']['CalendarColors'][$key];
                    $activity[ $key ] = $GLOBALS['sugar_config']['CalendarColors'][$key];
                }
            }
        }

        return $activity;
    }
    // END STIC-Custom

    // Overriding the array to add Shared Day option
    public function get_date_info($view, $date_time)
    {
        $str = "";

        global $current_user;
        $dateFormat = $current_user->getUserDateTimePreferences();

        if ($view == 'month' || $view == 'sharedMonth') {
            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $date_time->year;
                        break;
                    case "m":
                        $str .= " " . $date_time->get_month_name();
                        break;
                }
            }
        } elseif ($view == 'agendaWeek' || $view == 'sharedWeek') {
            $first_day = $date_time;

            $first_day = CalendarUtils::get_first_day_of_week($date_time);
            $last_day = $first_day->get("+6 days");

            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $first_day->year;
                        break;
                    case "m":
                        $str .= " " . $first_day->get_month_name();
                        break;
                    case "d":
                        $str .= " " . $first_day->get_day();
                        break;
                }
            }
            $str .= " - ";
            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $last_day->year;
                        break;
                    case "m":
                        $str .= " " . $last_day->get_month_name();
                        break;
                    case "d":
                        $str .= " " . $last_day->get_day();
                        break;
                }
            }
        // STIC 20210430 AAM - Add Shared day 
        // STIC#263
        // } elseif ($view == 'agendaDay') {
        } elseif ($view == 'agendaDay' || $view == 'sharedDay') {
            $str .= $date_time->get_day_of_week() . " ";

            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $date_time->year;
                        break;
                    case "m":
                        $str .= " " . $date_time->get_month_name();
                        break;
                    case "d":
                        $str .= " " . $date_time->get_day();
                        break;
                }
            }
        } elseif ($view == 'mobile') {
            $str .= $date_time->get_day_of_week() . " ";

            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $date_time->year;
                        break;
                    case "m":
                        $str .= " " . $date_time->get_month_name();
                        break;
                    case "d":
                        $str .= " " . $date_time->get_day();
                        break;
                }
            }
        } elseif ($view == 'year') {
            $str .= $date_time->year;
        } else {
            //could be a custom view.
            $first_day = $date_time;

            $first_day = CalendarUtils::get_first_day_of_week($date_time);
            $last_day = $first_day->get("+6 days");

            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $first_day->year;
                        break;
                    case "m":
                        $str .= " " . $first_day->get_month_name();
                        break;
                    case "d":
                        $str .= " " . $first_day->get_day();
                        break;
                }
            }
            $str .= " - ";
            for ($i = 0; $i < strlen($dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " " . $last_day->year;
                        break;
                    case "m":
                        $str .= " " . $last_day->get_month_name();
                        break;
                    case "d":
                        $str .= " " . $last_day->get_day();
                        break;
                }
            }
        }
        return $str;
    }

    /**
     * We override this function to include parameters related to the filters function. Including the filters value and the filters TPL.
     *
     * @param SugarSmarty $ss
     * @return void
     */
    protected function load_settings_template(&$ss)
    {
        global $current_user, $app_list_strings, $dictionary;
        parent::load_settings_template($ss);
        $sticSessionsColor = $current_user->getPreference('calendar_stic_sessions_color');
        $sticSessionsActivityType = $current_user->getPreference('calendar_stic_sessions_activity_type');
        $sticSessionsSticEventsType = $current_user->getPreference('calendar_stic_sessions_stic_events_type');
        $sticSessionsSticEventId = $current_user->getPreference('calendar_stic_sessions_stic_events_id');
        $sticSessionsSticCenterId = $current_user->getPreference('calendar_stic_sessions_stic_centers_id');
        $sticSessionsResponsibleId = $current_user->getPreference('calendar_stic_sessions_responsible_id');
        $sticSessionsContactId = $current_user->getPreference('calendar_stic_sessions_contacts_id');
        $sticSessionsProjectId = $current_user->getPreference('calendar_stic_sessions_projects_id');
        $sticFollowUpsColor = $current_user->getPreference('calendar_stic_followups_color');
        $sticFollowUpsType = $current_user->getPreference('calendar_stic_followups_type');
        $sticFollowUpsContactId = $current_user->getPreference('calendar_stic_followups_contacts_id');
        $sticFollowUpsProjectId = $current_user->getPreference('calendar_stic_followups_projects_id');
        $sticWorkCalendarType = $current_user->getPreference('calendar_stic_work_calendar_type');
        $sticWorkCalendarUsersDepartament = $current_user->getPreference('calendar_stic_work_calendar_assigned_user_department');

        $sticSessionsColorOptions = get_select_options_with_id($app_list_strings[$dictionary['stic_Sessions']['fields']['color']['options']], $sticSessionsColor);
        $ss->assign('stic_sessions_color', $sticSessionsColorOptions);

        $sticSessionsActivityTypeOptions = get_select_options_with_id($app_list_strings['stic_sessions_activity_types_list'], $sticSessionsActivityType);
        $ss->assign('stic_sessions_activity_type', $sticSessionsActivityTypeOptions);

        $sticSessionsSticEventsTypeOptions = get_select_options_with_id($app_list_strings['stic_events_types_list'], $sticSessionsSticEventsType);
        $ss->assign('stic_sessions_stic_events_type', $sticSessionsSticEventsTypeOptions);

        if ($sticSessionsSticEventId) {
            $eventBean = BeanFactory::getBean('stic_Events', $sticSessionsSticEventId);
            $ss->assign('stic_sessions_stic_events_name', $eventBean->name);
            $ss->assign('stic_sessions_stic_events_id', $sticSessionsSticEventId);
        }
        if ($sticSessionsSticCenterId) {
            $centerBean = BeanFactory::getBean('stic_Centers', $sticSessionsSticCenterId);
            $ss->assign('stic_sessions_stic_centers_name', $centerBean->name);
            $ss->assign('stic_sessions_stic_centers_id', $sticSessionsSticCenterId);
        }
        if ($sticSessionsResponsibleId) {
            $contactBean = BeanFactory::getBean('Contacts', $sticSessionsResponsibleId);
            $ss->assign('stic_sessions_responsible_name', $contactBean->name);
            $ss->assign('stic_sessions_responsible_id', $sticSessionsResponsibleId);
        }
        if ($sticSessionsContactId) {
            $contactBean = BeanFactory::getBean('Contacts', $sticSessionsContactId);
            $ss->assign('stic_sessions_contacts_name', $contactBean->full_name);
            $ss->assign('stic_sessions_contacts_id', $sticSessionsContactId);
        }
        if ($sticSessionsProjectId) {
            $projectBean = BeanFactory::getBean('Project', $sticSessionsProjectId);
            $ss->assign('stic_sessions_projects_name', $projectBean->name);
            $ss->assign('stic_sessions_projects_id', $sticSessionsProjectId);
        }

        $sticFollowUpsColorOptions = get_select_options_with_id($app_list_strings[$dictionary['stic_FollowUps']['fields']['color']['options']], $sticFollowUpsColor);
        $ss->assign('stic_followups_color', $sticFollowUpsColorOptions);

        $sticFollowUpsTypeOptions = get_select_options_with_id($app_list_strings['stic_followups_types_list'], $sticFollowUpsType);
        $ss->assign('stic_followups_type', $sticFollowUpsTypeOptions);

        if ($sticFollowUpsContactId) {
            $contactBean = BeanFactory::getBean('Contacts', $sticFollowUpsContactId);
            $ss->assign('stic_followups_contacts_name', $contactBean->full_name);
            $ss->assign('stic_followups_contacts_id', $sticFollowUpsContactId);
        }
        if ($sticFollowUpsProjectId) {
            $projectBean = BeanFactory::getBean('Project', $sticFollowUpsProjectId);
            $ss->assign('stic_followups_projects_name', $projectBean->name);
            $ss->assign('stic_followups_projects_id', $sticFollowUpsProjectId);
        }
        $sticWorkCalendarTypeOptions = get_select_options_with_id($app_list_strings['stic_work_calendar_types_list'], $sticWorkCalendarType);
        $ss->assign('stic_work_calendar_type', $sticWorkCalendarTypeOptions);

        $ss->assign('stic_work_calendar_assigned_user_department', $sticWorkCalendarUsersDepartament);

        if (
            $sticSessionsSticEventsType || $sticSessionsSticEventId || $sticSessionsSticCenterId || $sticSessionsResponsibleId || $sticSessionsContactId || $sticSessionsProjectId ||
            $sticSessionsColor || $sticSessionsActivityType || $sticFollowUpsColor || $sticFollowUpsContactId || $sticFollowUpsProjectId || $sticFollowUpsType ||
            $sticWorkCalendarType || $sticWorkCalendarUsersDepartament
        ) {
            $ss->assign('applied_filters', true);
        }
        $filters = get_custom_file_if_exists("modules/Calendar/tpls/filters.tpl");
        $ss->assign("filters", $filters);

        // STIC-Custom 20240222 MHP - Adding Work Calendar record in Calendar
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/114
        $show_work_calendar = $GLOBALS['current_user']->getPreference('show_work_calendar');
        $show_work_calendar = $show_work_calendar ?: false;
        $ss->assign('show_work_calendar', $show_work_calendar);
        // END STIC-Custom
    }
}
