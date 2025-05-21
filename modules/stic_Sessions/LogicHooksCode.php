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

class stic_SessionsLogicHooks
{

    public function before_save(&$bean, $event, $arguments)
    {

        global $current_user;

        // Session dates management
        $format = 'Y-m-d H:i:s';
        if ((is_null($bean->start_date) || ($bean->start_date == '')) &&
            (is_null($bean->end_date) || ($bean->end_date == ''))) {
            // If start and end dates were not set then start = now and end = now + 1h
            $bean->start_date = gmdate($format);
            $endDate = DateTime::createFromFormat($format, $bean->start_date);
            $endDate->modify("+1 hours");
            $bean->end_date = $endDate->format($format);
            // Empty name to be rebuilt later
            $bean->name = null;
        } else if (is_null($bean->start_date) || ($bean->start_date == '')) {
            // If start date was not set then set start = end - 1h
            $endDate = DateTime::createFromFormat($format, $bean->end_date);
            $startDate = DateTime::createFromFormat($format, $bean->end_date);
            $startDate->modify('-1 hours');
            $bean->start_date = $startDate->format($format);
            // Empty name to be rebuilt later
            $bean->name = null;
        } else if (is_null($bean->end_date) || ($bean->end_date == '')) {
            // If end date was not set then set end = start + 1h
            $startDate = DateTime::createFromFormat($format, $bean->start_date);
            $endDate = DateTime::createFromFormat($format, $bean->start_date);
            $endDate->modify('+1 hours');
            $bean->end_date = $endDate->format($format);
        } else {
            $startDate = DateTime::createFromFormat($format, $bean->start_date);
            $endDate = DateTime::createFromFormat($format, $bean->end_date);

            // If end date is previous to start date then set end = start + 1h
            if ($endDate <= $startDate) {
                $endDate = DateTime::createFromFormat($format, $bean->start_date);
                $endDate->modify('+1 hours');
                $bean->end_date = $endDate->format($format);
            }
        }

        // Set session duration
        $startTime = strtotime($bean->start_date);
        $endTime = strtotime($bean->end_date);
        $duration = $endTime - $startTime;
        // Casting the result into float, otherwise a string is returned and may give wrong values in workflows
        $bean->duration = (float) number_format($duration / 3600, 2);

        // If start_date has changed set weekday
        if (empty($bean->fetched_row) || $bean->start_date != $bean->fetched_row['start_date']) {
            $bean->weekday = date('w', strtotime($bean->start_date));
        }

        // If name is empty or start_date has changed, set the name
        if (empty($bean->name) || $bean->start_date != ($bean->fetched_row['start_date']??'')) {
            require_once 'modules/stic_Sessions/Utils.php';
            stic_SessionsUtils::setSessionName($bean);
        }
    }

    public function after_save(&$bean, $event, $arguments)
    {
        include_once 'modules/stic_Events/Utils.php';

        // Create attendances in case of new session
        if (empty($bean->fetched_row['id'])) {
            require_once 'modules/stic_Attendances/Utils.php';
            $startDateString = substr($bean->start_date, 0, 10);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  ' . $startDateString . '  >>>>  ' . $bean->end_date . ' >>>  ' . $bean->id);
            stic_AttendancesUtils::createAttendances($startDateString, null, $bean->id);

            // Recalculate event total hours (in periodic session creation will use notUpdateRelatedEvent flag
            // in order to boost performance by only recalculating in the last created session)
            if (!isset($_SESSION['notUpdateRelatedEvent'])) {
                $event_id = $bean->db->getOne("select se.stic_sessions_stic_eventsstic_events_ida FROM stic_sessions_stic_events_c se WHERE se.stic_sessions_stic_eventsstic_sessions_idb='" . $bean->id . "' AND deleted=0");
                stic_EventsUtils::setEventTotalHours($event_id);
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Setting total hours to the event related to the session ' . $bean->id);
            }
        } else {
            // If start or end dates have changed...
            if ($bean->start_date != $bean->fetched_row['start_date'] || $bean->end_date != $bean->fetched_row['end_date']) {
                // Update attendances
                include_once 'modules/stic_Attendances/Utils.php';
                stic_AttendancesUtils::inheritDataFromSession($bean);
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Applying changes to the attendances related to the session ' . $bean->id);

                // Recalculate event total hours
                $event_id = $bean->db->getOne("select se.stic_sessions_stic_eventsstic_events_ida FROM stic_sessions_stic_events_c se WHERE se.stic_sessions_stic_eventsstic_sessions_idb='" . $bean->id . "' AND deleted=0");
                stic_EventsUtils::setEventTotalHours($event_id);
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Setting total hours to the event related to the session ' . $bean->id);

            // If session name has changed...
            } elseif ($bean->name != $bean->fetched_row['name']) {
                // Update attendances
                include_once 'modules/stic_Attendances/Utils.php';
                stic_AttendancesUtils::inheritDataFromSession($bean);
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Applying changes to the attendances related to the session ' . $bean->id);
            }

        }

    }

    // Manage relationship events
    public function manage_relationships(&$bean, $event, $arguments)
    {
        if ($arguments['related_module'] == 'stic_Events') {
            require_once 'modules/stic_Events/Utils.php';
            $event_id = $arguments['related_id'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . $event . ' | ' . $arguments['related_module'] . '--' . $arguments['related_id']);
            switch ($event) {
                case 'after_relationship_delete':
                    // Recalculate event total hours
                    stic_EventsUtils::setEventTotalHours($event_id);
                    break;
                case 'after_relationship_add':
                    // Recalculate event total hours
                    stic_EventsUtils::setEventTotalHours($event_id);
                    // Create attendances if neccesary
                    require_once 'modules/stic_Attendances/Utils.php';
                    $startDateString = substr($bean->start_date, 0, 10);
                    stic_AttendancesUtils::createAttendances($startDateString, null, $bean->id);
                    break;
                default:
                    break;
            }
        }
    }

}
