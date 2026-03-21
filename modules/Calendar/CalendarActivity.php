<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2024 SalesAgility Ltd.
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

require_once 'include/utils/activity_utils.php';

#[\AllowDynamicProperties]
class CalendarActivity
{
    public $sugar_bean;
    public $start_time;
    public $end_time;

    /**
     * CalendarActivity constructor.
     * @param $args
     */
    public function __construct($args)
    {
        // if we've passed in an array, then this is a free/busy slot
        // and does not have a sugarbean associated to it
        global $timedate;

        if (is_array($args)) {
            $this->start_time = clone $args[0];
            $this->end_time = clone $args[1];
            $this->sugar_bean = null;
            $timedate->tzGMT($this->start_time);
            $timedate->tzGMT($this->end_time);

            return;
        }

        // else do regular constructor..

        $sugar_bean = $args;
        $this->sugar_bean = $sugar_bean;


        if ($sugar_bean->object_name === 'Task') {
            if (!empty($this->sugar_bean->date_start)) {
                $this->start_time = $timedate->fromUser($this->sugar_bean->date_start);
            } else {
                $this->start_time = $timedate->fromUser($this->sugar_bean->date_due);
            }
            if (empty($this->start_time)) {
                return;
            }
            $this->end_time = $timedate->fromUser($this->sugar_bean->date_due);
        } else {
            $this->start_time = $timedate->fromUser($this->sugar_bean->date_start);
            if (empty($this->start_time)) {
                return;
            }
            $hours = $this->sugar_bean->duration_hours;
            if (empty($hours)) {
                $hours = 0;
            }
            $mins = $this->sugar_bean->duration_minutes;
            if (empty($mins)) {
                $mins = 0;
            }
            $this->end_time = $this->start_time->get("+$hours hours $mins minutes");
        }
        // Convert it back to database time so we can properly manage it for getting the proper start and end dates
        $timedate->tzGMT($this->start_time);
        $timedate->tzGMT($this->end_time);
    }

    /**
     * Get where clause for fetching entries from DB for within two dates timespan
     * @param string $table_name t
     * @param string $rel_table table for accept status, not used in Tasks
     * @param SugarDateTime $start_ts_obj start date
     * @param SugarDateTime $end_ts_obj end date
     * @param string $field_name date field in table
     * @param string $field_end_date
     * @return string
     */
    public static function get_occurs_within_where_clause(
        $table_name,
        $rel_table,
        $start_ts_obj,
        $end_ts_obj,
        $field_name = 'date_start',
        $field_end_date = 'date_end'
    ) {
        return self::getOccursWhereClauseGeneral(
            $table_name,
            $rel_table,
            $start_ts_obj,
            $end_ts_obj,
            $field_name,
            $field_end_date,
            array('CalendarActivity', 'within')
        );
    }

    /**
     * Get where clause for fetching entries from DB for until certain date timespan
     * @param string $table_name t
     * @param string $rel_table table for accept status, not used in Tasks
     * @param SugarDateTime $start_ts_obj start date
     * @param SugarDateTime $end_ts_obj end date
     * @param string $field_name date field in table
     * @param string $field_end_date
     * @return string
     */
    public static function get_occurs_until_where_clause(
        $table_name,
        $rel_table,
        $start_ts_obj,
        $end_ts_obj,
        $field_name = 'date_start',
        $field_end_date = 'date_end'
    ) {
        return self::getOccursWhereClauseGeneral(
            $table_name,
            $rel_table,
            $start_ts_obj,
            $end_ts_obj,
            $field_name,
            $field_end_date,
            array('CalendarActivity', 'until')
        );
    }

    /**
     * @param $user_focus
     * @param $start_date_time
     * @param $end_date_time
     * @return array
     */
    public static function get_freebusy_activities($user_focus, $start_date_time, $end_date_time)
    {
        $act_list = array();
        $vcal_focus = BeanFactory::newBean('vCals');
        $vcal_str = $vcal_focus->get_vcal_freebusy($user_focus);

        $lines = explode("\n", $vcal_str);
        $utc = new DateTimeZone('UTC');
        foreach ($lines as $line) {
            if (preg_match('/^FREEBUSY.*?:([^\/]+)\/([^\/]+)/i', $line, $matches)) {
                $dates_arr = array(
                    SugarDateTime::createFromFormat(vCal::UTC_FORMAT, $matches[1], $utc),
                    SugarDateTime::createFromFormat(vCal::UTC_FORMAT, $matches[2], $utc)
                );
                $act_list[] = new CalendarActivity($dates_arr);
            }
        }

        return $act_list;
    }

    /**
     * Get array of activities
     * @param array $activities
     * @param string $user_id
     * @param boolean $show_tasks
     * @param SugarDateTime $view_start_time start date
     * @param SugarDateTime $view_end_time end date
     * @param string $view view; not used for now, left for compatibility
     * @param boolean $show_calls
     * @param boolean $show_completed use to allow filtering completed events
     * @return array
     */
    public static function get_activities(
        $activities,
        $user_id,
        $show_tasks,
        $view_start_time,
        $view_end_time,
        $view,
        $show_calls = true,
        $show_completed = true
    ) {
        global $current_user;
        $act_list = array();
        $seen_ids = array();

        $complete = [];
        if (!$show_completed) {
            $complete['Calls'] = " AND calls.status = 'Planned' ";
            $complete['Meetings'] = " AND meetings.status = 'Planned' ";
            $complete['Tasks'] = " AND tasks.status != 'Completed' ";
        }

        foreach ($activities as $key => $activity) {
            if ($key === 'Tasks' && !$show_tasks ||
                $key === 'Calls' && !$show_calls) {
                continue;
            }

            if (ACLController::checkAccess($key, 'list', true)) {
                /* END - SECURITY GROUPS */
                $bean = BeanFactory::newBean($key);

                if ($current_user->id === $user_id) {
                    $bean->disable_row_level_security = true;
                }

                switch ($key) {
                    case 'Meetings':
                    case 'Calls':
                        $where = self::get_occurs_until_where_clause(
                            $bean->table_name,
                            isset($bean->rel_users_table) ? $bean->rel_users_table : null,
                            $view_start_time,
                            $view_end_time,
                            $activity['start'],
                            $activity['end']
                        );
                        break;
                    default:
                        $where = self::get_occurs_within_where_clause(
                            $bean->table_name,
                            isset($bean->rel_users_table) ? $bean->rel_users_table : null,
                            $view_start_time,
                            $view_end_time,
                            $activity['start'],
                            $activity['end']
                        );
                        break;
                }

                if (!empty($complete[$key])) {
                    $where .= $complete[$key];
                }

                $focus_list = build_related_list_by_user_id($bean, $user_id, $where);
                //require_once 'modules/SecurityGroups/SecurityGroup.php';
                foreach ($focus_list as $focusBean) {
                    if (isset($seen_ids[$focusBean->id])) {
                        continue;
                    }
                    /* TODO update currently unused functionality, disabled as expensive
                    $in_group = SecurityGroup::groupHasAccess($key, $focusBean->id, 'list');
                    $show_as_busy = !ACLController::checkAccess(
                        $key,
                        'list',
                        $current_user->id === $user_id,
                        'module',
                        $in_group
                    );
                    $focusBean->show_as_busy = $show_as_busy;*/

                    $seen_ids[$focusBean->id] = 1;
                    $act = new CalendarActivity($focusBean);

                    if (!empty($act)) {
                        $act_list[] = $act;
                    }
                }
            }
        }

        return $act_list;
    }

    /**
     * Get where clause for fetching entries from DB (is used by certain get_occurs.. methods)
     * @param string $table_name t
     * @param string $rel_table table for accept status, not used in Tasks
     * @param SugarDateTime $start_ts_obj start date
     * @param SugarDateTime $end_ts_obj end date
     * @param string $field_name date field in table
     * @param $field_end_date
     * @param array $callback callback function to generete specific SQL query-part
     * @return string
     */
    protected static function getOccursWhereClauseGeneral(
        $table_name,
        $rel_table,
        $start_ts_obj,
        $end_ts_obj,
        $field_name,
        $field_end_date,
        $callback
    ) {
        $start = clone $start_ts_obj;
        $end = clone $end_ts_obj;

        $field_date = $table_name . '.' . $field_name;
        $field_end_date = $table_name . '.' . $field_end_date;
        $start_day = DBManagerFactory::getInstance()->convert("'{$start->asDb()}'", 'datetime');
        $end_day = DBManagerFactory::getInstance()->convert("'{$end->asDb()}'", 'datetime');

        $where = '(';
        $where .= call_user_func($callback, $field_date, $field_end_date, $start_day, $end_day);

        if (!empty($rel_table)) {
            $where .= " AND $rel_table.accept_status != 'decline'";
        }

        $where .= ')';

        return $where;
    }

    /**
     * Helper-method to generate within two dates sql clause
     * @param $field_date string table_name.field_name to compare
     * @param $field_date_end
     * @param $start_day string period start date
     * @param $end_day string period end date
     * @return string
     */
    protected static function within($field_date, $field_date_end, $start_day, $end_day)
    {
        return "$field_date >= $start_day AND $field_date < $end_day";
    }

    /**
     * Helper-method to generate until some date sql clause
     * @param $field_date string table_name.field_name to compare
     * @param $field_date_end
     * @param $start_day string period start date
     * @param $end_day string period end date
     * @return string
     */
    protected static function until($field_date, $field_date_end, $start_day, $end_day)
    {
        return "($field_date >= $start_day AND $field_date < $end_day) OR ($field_date < $start_day AND $field_date_end > $start_day)";
    }
}
