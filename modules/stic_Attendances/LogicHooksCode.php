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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class stic_AttendancesLogicHooks
{

    public function before_save(&$bean, $event, $arguments)
    {
        // Exit function in case of automatically generated attendances as
        // all actions performed below are already done in createAttendances function.
        // This way we save time, specially when creating a large number of attendances.
        if ($bean->automaticCreate??false) {
            return;
        }

        // If name, start date or duration are not set then get the related session
        // in order to inheritate its data
        if (empty($bean->name) || empty($bean->start_date) || empty($bean->duration)) {
            if (is_string($bean->stic_attendances_stic_sessionsstic_sessions_ida)) {
                $sessionId = $bean->stic_attendances_stic_sessionsstic_sessions_ida;
            } else {
                if (!empty($bean->stic_attendances_stic_sessionsstic_sessions_ida)) {
                    $bean->stic_attendances_stic_sessionsstic_sessions_ida->load();
                    $sessionId = key($bean->stic_attendances_stic_sessionsstic_sessions_ida->rows);
                }
            }
            $sessionBean = BeanFactory::getBean('stic_Sessions', $sessionId);
        }

        global $timedate, $current_user;

        // Set start date if it is empty and there is a related session
        // (If there is no related session then the $sessionBean object will be set up as a new Session record.
        // Its start date will be filled with current date and attendance will inherite that value
        // which would be nonsense.)
        if (empty($bean->start_date) && !empty($sessionId)) {
            $sessionDate = $timedate->fromUser($sessionBean->start_date, $current_user);
            $sessionDate = $timedate->asDb($sessionDate);
            $bean->start_date = $sessionDate;
        }

        // Set duration if it is empty and status is empty too.
        // If status is not empty we'll keep the duration value set by the user, whatever it is
        if (empty($bean->duration) && empty($bean->status)) {
            $bean->duration = (float)(empty($sessionBean->duration) ? 0 : $sessionBean->duration);
        }

        // Set name if it is empty
        if (empty($bean->name)) {
            // Fix Fatal Error when stic_attendances_stic_registrationsstic_registrations_ida is null - https://github.com/SinergiaTIC/SinergiaCRM/pull/315
            if ($bean->stic_attendances_stic_registrationsstic_registrations_ida !== null) {
                if (is_string($bean->stic_attendances_stic_registrationsstic_registrations_ida)) {
                    $registration_id = $bean->stic_attendances_stic_registrationsstic_registrations_ida;
                } else {
                    $bean->stic_attendances_stic_registrationsstic_registrations_ida->load();
                    $registration_id = key($bean->stic_attendances_stic_registrationsstic_registrations_ida->rows);
                }
                $registrationBean = BeanFactory::getBean('stic_Registrations', $registration_id);
            } else {
                // If there is no registration, use one with name "Unkmown"
                $registrationBean = new stic_Registrations();
                $registrationBean->name = 'Unknown - Unknown';
            }
            
            // Attendance's name includes registration's and session's names, both of them potentially including event's name.
            // Let's check it in order to avoid repeating event's name in attendance's name.
            $registrationNameEnd = trim(explode('-', $registrationBean->name)[1]);
            $sessionNameStart = trim(explode('|', $sessionBean->name)[0]);
            if ($registrationNameEnd == $sessionNameStart) {
                // If event's name exists in both session's and registration's names, let's exclude it from session's name.
                $cleanSessionName = str_replace($sessionNameStart, '', $sessionBean->name);
                $cleanSessionName = trim(str_replace('|', '', $cleanSessionName));
            } else {
                $cleanSessionName = $sessionBean->name;
            }

            $bean->name = $registrationBean->name . ' | ' . $cleanSessionName;
        }
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  ' . $bean->start_date . '   ' . $bean->duration);

    }

    public function after_save(&$bean, $event, $arguments)
    {

        // Set registration data in case of attendance update with duration or status change
        if (!empty($bean->fetched_row['id']) || in_array($bean->status, array('yes', 'partial')) == true) {
            if (empty($bean->fetched_row) || $bean->duration != $bean->fetched_row['duration'] || $bean->status != $bean->fetched_row['status']) {
                require_once 'modules/stic_Attendances/Utils.php';
                if ($registrationId = $this->getRegistrationIdFromAttendanceId($bean->id)) {
                    if ($registration = BeanFactory::getBean('stic_Registrations', $registrationId)) {
                        stic_AttendancesUtils::setRegistrationTotalHoursAndPercentage($registration->id);
                        // Related with STIC#744 
                        $registration->retrieve();
                    } else {
                        $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": A registration bean with the following ID doesn't exist: ". $registrationId);
                    }
                } else {
                    $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": There isn't a related registration for the attendance ID: ". $bean->id);
                }
            }
        }

        // Set session counters in case of attendance creation or status change
        if (empty($bean->fetched_row) || $bean->status != $bean->fetched_row['status'] || empty($bean->fetched_row['id'])) {
            require_once 'modules/stic_Sessions/Utils.php';
            if ($sessionId = $this->getSessionIdFromAttendanceId($bean->id)) {
                if ($session = BeanFactory::getBean('stic_Sessions', $sessionId)) {
                    stic_SessionsUtils::setSessionAttendancesCounters($session->id);
                    // Related with STIC#744 
                    $session->retrieve();
                } else {
                    $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": A session bean with the following ID doesn't exist: ". $sessionId);
                }
            } else {
                $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": There isn't a related session for the attendance ID: ". $bean->id);
            }
        }
    }

    // Manage relationship events
    public function manage_relationship(&$bean, $event, $arguments)
    {
        if ($arguments['related_module'] == 'stic_Registrations') {
            require_once 'modules/stic_Attendances/Utils.php';
            switch ($event) {
                case 'after_relationship_delete':
                case 'after_relationship_add':     
                    if ($registrationId = $arguments['related_id']) {
                        if ($registration = BeanFactory::getBean($arguments['related_module'], $registrationId)) {               
                            stic_AttendancesUtils::setRegistrationTotalHoursAndPercentage($registration->id);
                            // Related with STIC#744 
                            $registration->retrieve();
                        } else {
                            $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": A registration bean with the following ID doesn't exist: ". $registrationId);
                        }
                    } else {
                        $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": There isn't a related registration for the attendance ID: ". $bean->id);
                    }
                    break;
                default:
                    break;
            }
        } elseif ($arguments['related_module'] == 'stic_Sessions') {
            require_once 'modules/stic_Sessions/Utils.php';
            switch ($event) {
                case 'after_relationship_delete':
                case 'after_relationship_add':   
                    if ($sessionId = $arguments['related_id']) {
                        if ($session = BeanFactory::getBean($arguments['related_module'], $sessionId)) {
                            $GLOBALS['log']->debug(__METHOD__ . ' ' . $event . ' | ' . $arguments['related_module'] . '--' . $arguments['related_id']);                 
                            stic_SessionsUtils::setSessionAttendancesCounters($session->id);
                            // Related with STIC#744                     
                            $session->retrieve();
                        } else {
                            $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": A session bean with the following ID doesn't exist: ". $sessionId);
                        }
                    } else {
                        $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": There isn't a related session for the attendance ID: ". $bean->id);
                    }
                    break;

                default:
                    break;
            }
        }
    }

    /**
     * Get the id of a session using id of an attendances
     *
     * @param String $attendanceId
     * @return String
     */
    public function getSessionIdFromAttendanceId($attendanceId)
    {
        $db = DBManagerFactory::getInstance();
        $sql = 'select rel.stic_attendances_stic_sessionsstic_sessions_ida from stic_attendances_stic_sessions_c rel where deleted = 0 AND rel.stic_attendances_stic_sessionsstic_attendances_idb="' . $attendanceId . '"';
        return $db->getOne($sql);
    }

    /**
     * Get an registration id from the id of a related attendance
     *
     * @param String $attendanceId
     * @return String
     */
    public function getRegistrationIdFromAttendanceId($attendanceId)
    {
        $db = DBManagerFactory::getInstance();
        $sql = 'select rel.stic_attendances_stic_registrationsstic_registrations_ida from stic_attendances_stic_registrations_c rel where deleted = 0 AND rel.stic_attendances_stic_registrationsstic_attendances_idb="' . $attendanceId . '"';
        return $db->getOne($sql);
    }

}
