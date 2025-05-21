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
class stic_SessionsUtils
{
    /**
     * Saves the value of the total_attendances and validated_attendances fields of the session by means of the state of the related attendances
     *
     * @param String $sessionId
     * @return void
     */
    public static function setSessionAttendancesCounters($sessionId)
    {
        $GLOBALS['log']->debug(__METHOD__ . "Calculating total and validated attendances for  $sessionId");

        $db = DBManagerFactory::getInstance();
        $sqlToUpdateCounters =
            "UPDATE stic_sessions
                SET
                    total_attendances=
                        (SELECT COUNT(*)
                        FROM stic_attendances a
                        JOIN stic_attendances_stic_sessions_c rel ON rel.stic_attendances_stic_sessionsstic_attendances_idb=a.id
                        WHERE rel.stic_attendances_stic_sessionsstic_sessions_ida ='$sessionId' AND a.deleted=0 AND rel.deleted=0),
                    validated_attendances =
                        (SELECT COUNT(*)
                        FROM stic_attendances a
                        JOIN stic_attendances_stic_sessions_c rel ON rel.stic_attendances_stic_sessionsstic_attendances_idb=a.id
                        WHERE rel.stic_attendances_stic_sessionsstic_sessions_ida ='$sessionId' AND a.deleted=0 AND rel.deleted =0 AND a.`status` !='')
                WHERE id='$sessionId';";
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "QUERY: $sqlToUpdateCounters");

        return $db->query($sqlToUpdateCounters);

    }

    /**
     * Sets the session name 
     *
     * @param Object $sessionBean
     * @return void
     */
    public static function setSessionName($sessionBean)
    {
        global $current_user;
        // Session name must be set if it is empty. If it is not, then only should be overwritten 
        // if its value was not customized by the user.
       
        // Build the session name that would have been set in case of no user customization
        include_once 'SticInclude/Utils.php';
        $eventBean = SticUtils::getRelatedBeanObject($sessionBean, 'stic_sessions_stic_events');
        $eventName = $eventBean->name;
        $timeDate = new TimeDate();
        // As start date might have changed in the current record edition, let's use the value stored in database
        $sessionOriginalStartDate = '';
        if(!empty($sessionBean->fetched_row)){
            $sessionOriginalStartDate = $timeDate->to_display_date_time($sessionBean->fetched_row['start_date'], true, true, $current_user);
        }
        $sessionOriginalName = "{$eventName} | {$sessionOriginalStartDate}h";

        // If session name is empty or it has not been customized, (re)built it
        if ($sessionOriginalName == $sessionBean->name || empty($sessionBean->name)) {
            $sessionCurrentStartDate = $timeDate->to_display_date_time($sessionBean->start_date, true, true, $current_user);
            $sessionBean->name = "{$eventName} | {$sessionCurrentStartDate}h";
        }
    }
}
