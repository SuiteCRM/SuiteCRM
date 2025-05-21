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
class stic_AttendancesUtils
{

    /**
     * createAttendances Creates attendances according to the parameters provided
     *
     * @param String $date String in Y-m-d format '2019-05-05' Optional, default null (apply current_date)
     * @param String $registrationId Optional, default null
     * @param String $sessionId Optional, default null
     * @return void
     */
    public static function createAttendances($date = null, $registrationId = null, $sessionId = null)
    {

        if ($date > date('Y-m-d')) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  $date is future... no attendances will be created.');
            return;
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Creating attendances with these params: date:' . $date . '| registration_id:' . $registrationId . '| session_id:' . $sessionId);
        // STIC TIP: run task in navigator with:
        // window.location.href="/index.php?module=stic_Attendances&action=createAttendances&return_module=stic_Attendances&return_action=index"
        // window.location.href="/index.php?module=stic_Attendances&action=createAttendancesRange&return_module=stic_Attendances&return_action=index&start=2019-01-15&end=2019-01-15&session_id=b10a46ce-9dbc-fa97-2355-5ca399094453"

        global $timedate;
        if ($date == null) {
            $date = date('Y-m-d');
        }

        // Add registration filter if needed
        $registrationIdWhere = '';
        if (!empty($registrationId)) {
            $registrationIdWhere = " AND i.id='$registrationId' ";
        }

        // Add session filter if needed
        $sessionIdWhere = '';
        if (!empty($sessionId)) {
            $sessionIdWhere = " AND s.id='$sessionId' ";
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Creating attendances for date ' . $date);

        $db = DBManagerFactory::getInstance();

        /* The following query returns a recordset containing the attendances that should be created
        in the given $date and works as follows:

        1) The main SELECT/WHERE clauses return the attendances that should exist in a given $date,
        taking into account existing relationships between main event and its sessions and registrations.
        Registrations must have 'confirmed' or 'participating' status and registration date prior or equal to the given $date.

        2) The first query column (idattendance) returns, by means of a subquery, the id of the attendance if it exists.
        If it does not exist, it returns NULL.

        3) The final condition "HAVING idattendance is null" allows the result of the global query to only include
        those attendances that should exist in the given $date but still don't, ie, that must be created. It is important to
        notice that this query can be run under any circumstances, whereas it has been executed before for a certain date or not.

        4) The $registrationIdWhere and $sessionIdWhere variables, if present, are concatenated in the WHERE section
        to add more specific filters.
         */

        $queryAttendancesToCreate =
            "SELECT DISTINCT
                (select a.id
                    from stic_attendances a
                join stic_attendances_stic_sessions_c ase on ase.stic_attendances_stic_sessionsstic_attendances_idb=a.id
                join stic_attendances_stic_registrations_c ai on ai.stic_attendances_stic_registrationsstic_attendances_idb =a.id
                    where
                    ase.stic_attendances_stic_sessionsstic_sessions_ida=s.id and
                    a.deleted=0 and
                    ase.deleted=0 and
                    ai.stic_attendances_stic_registrationsstic_registrations_ida=ei.stic_registrations_stic_eventsstic_registrations_idb
                    limit 1) idattendance,
                s.id session_id,
                i.id registration_id,
                i.disabled_weekdays,
                s.name session_name,
                s.duration duration,
                s.start_date,
                s.end_date,
                s.duration,
                i.registration_date,
                i.name registration_name,
                s.assigned_user_id session_assigned_user_id
            FROM  stic_sessions s
                JOIN stic_sessions_stic_events_c se ON s.id = se.stic_sessions_stic_eventsstic_sessions_idb
                JOIN stic_registrations_stic_events_c ei ON ei.stic_registrations_stic_eventsstic_events_ida = se.stic_sessions_stic_eventsstic_events_ida
                JOIN stic_registrations i ON i.id=ei.stic_registrations_stic_eventsstic_registrations_idb
            WHERE
                DATE(s.start_date) ='$date'
                AND s.deleted=0
                AND se.deleted=0
                AND ei.deleted=0
                AND i.deleted=0
                AND (date(i.registration_date) <='$date' || isnull(i.registration_date))
                $registrationIdWhere -- include the filter for registration if exist
                $sessionIdWhere -- include the filter for session if exist
                AND i.status IN('confirmed','participates')
            HAVING idattendance is null;
            ";

        $result = $db->query($queryAttendancesToCreate, true);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  num_rows = ' . print_r($result->num_rows, true));

        if (!$result) {
            $GLOBALS['log']->error(__METHOD__ . ' Error SELECT query attendances: ' . $queryAttendancesToCreate);
            return false;
        }

        while ($row = $db->fetchByAssoc($result)) {

            // Avoid attendance creation if $date weekday appears in registration disabled_weekdays string
            $currentWeekday = date('w', strtotime($row['start_date']));
            if (strpos($row['disabled_weekdays'], $currentWeekday) !== false) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Attendance creation for session: ' . $row['session_id'] . ' and registration: ' . $row['registration_id'] . ' is ommited because ' . ($row['weekday'] ?? null) . ' is in registration disabled_weekdays');
                continue;
            }

            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Creating attendance: ' . ($row['attendance_name'] ?? 'undefined'). ' for session: ' . $row['session_id'] . ' and registration: ' . $row['registration_id']);
            $attendance = BeanFactory::newBean('stic_Attendances');

            // Set basic data
            $attendance->assigned_user_id = $row['session_assigned_user_id'];
            $attendance->name = $row['registration_name'] . ' | ' . $timedate->to_display_date_time($row['start_date'], true, true) . 'h';
            $attendance->start_date = $row['start_date'];

            // Prepare duration if it is null
            if (is_null($row['duration'])) {
                $row['duration'] = 0;
            }

            // Set duration with the proper decimal symbol
            $attendance->duration = formatDecimalInConfigSettings($row['duration'], true);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Attendance Duration = ' . $attendance->duration);

            // Set a virtual attribute in order to know this is an automatically generated attendance
            // when we reach before_save LH code
            $attendance->automaticCreate = 1;

            // Add relationships
            $attendance->stic_attendances_stic_sessionsstic_sessions_ida = $row['session_id'];
            $attendance->stic_attendances_stic_registrationsstic_registrations_ida = $row['registration_id'];

            $attendance->save();

        }
        return true;
    }

    /**
     * Recalculates and saves in database, using SQL, the total hours and the attendance percentage of a registration
     *
     * @param String $registrationId
     * @return void
     */
    public static function setRegistrationTotalHoursAndPercentage($registrationId)
    {
        $GLOBALS['log']->debug(__METHOD__ . "Calculating total hours and percentage for registration $registrationId ");
        if (empty($registrationId)) {
            $GLOBALS['log']->error(__METHOD__ . " | The function has been called without the $registrationId parameter");
            return;
        }

        $db = DBManagerFactory::getInstance();
        $queryRegistrationTotalHoursAndPercentage =
            "UPDATE stic_registrations as main
            LEFT JOIN
                    (
                        SELECT
                        round(sum(a.duration)/e.total_hours*100,2) attendances_percentage,
                        sum(a.duration) attendances_total_hours
                    FROM stic_registrations i
                        JOIN stic_attendances_stic_registrations_c ai on ai.stic_attendances_stic_registrationsstic_registrations_ida=i.id
                        JOIN stic_attendances a on ai.stic_attendances_stic_registrationsstic_attendances_idb=a.id
                        JOIN stic_registrations_stic_events_c ei on ei.stic_registrations_stic_eventsstic_registrations_idb = i.id
                        JOIN stic_events e on e.id=ei.stic_registrations_stic_eventsstic_events_ida
                    WHERE
                        i.id='$registrationId'
                        AND i.deleted = 0
                        AND ai.deleted=0
                        AND ei.deleted=0
                        AND a.deleted=0
                        AND e.deleted=0
                        AND a.`status`in ('yes','partial')
                        group by i.id, e.id
                    ) as sub on main.id='$registrationId'
            SET main.attended_hours = ifnull(sub.attendances_total_hours,0),main.attendance_percentage=ifnull(sub.attendances_percentage,0)
            WHERE main.id='$registrationId'
        ";

        $result = $db->query($queryRegistrationTotalHoursAndPercentage);
        $GLOBALS['log']->debug(__METHOD__ . " Result: " . print_r($result, true));

        return $result;
    }

    /**
     * Update attendances data when related session changes.
     * Beans are used in order to trigger other functions that will update registrations.
     *
     * @param Object $sessionBean Session Bean
     * @return void
     */
    public static function inheritDataFromSession($sessionBean)
    {
        $sessionBean->load_relationship('stic_attendances_stic_sessions');
        foreach ($sessionBean->stic_attendances_stic_sessions->getBeans() as $attendanceBean) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  ' . $attendanceBean->name);

            // Set attendance's duration if session's duration has changed
            if ($sessionBean->duration != $sessionBean->fetched_row['duration']) {
                $attendanceBean->duration = formatDecimalInConfigSettings($sessionBean->duration, true);
            }

            // Set attendance's start_date if session's start_date has changed
            if ($sessionBean->start_date != $sessionBean->fetched_row['start_date']) {
                $attendanceBean->start_date = $sessionBean->start_date;
            }

            // Empty attendance's name for later regeneration if session's name has changed
            if ($sessionBean->name != $sessionBean->fetched_row['name']) {
                $attendanceBean->name = '';
            }

            $attendanceBean->save();
        }

    }
}
