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

require_once __DIR__ . '/../../modules/Users/User.php';
require_once __DIR__ . '/../../modules/Meetings/Meeting.php';

/**
 * Implements Google Calendar Syncing
 *
 * @license https://raw.githubusercontent.com/salesagility/SuiteCRM/master/LICENSE.txt
 * GNU Affero General Public License version 3
 * @author Benjamin Long <ben@offsite.guru>
 */

#[\AllowDynamicProperties]
class GoogleSyncHelper
{
    /**
     * Helper method for GoogleSync::pushPullSkip.
     *
     * When given a single calendar object, determine its type and return an action.
     * At least one of the params is required.
     *
     * @param Meeting|null $meeting (optional) Meeting Bean
     * @param \Google\Service\Calendar\Event|null $event (optional) Google\Service\Calendar\Event Object
     *
     * @return string push, pull, skip, or false on error
     */
    public function singleEventAction(Meeting $meeting = null, Google\Service\Calendar\Event $event = null)
    {
        if (empty($meeting) && empty($event)) {
            return false;
        }
        if (empty($meeting) && $event->status !== 'cancelled' && $event->getStart()->getDateTime() !== null) { // We only pull if the Google Event is not deleted/cancelled and not an all day event.
            return "pull";
        } elseif (empty($event) && $meeting->deleted == '0') {
            return "push";
        }
        return "skip";
    }

    /**
     * Helper method for GoogleSync::pushPullSkip.
     *
     * Takes two calendar events, and extracts their last modified and sync times.
     *
     * @param Meeting $meeting Meeting Bean
     * @param \Google\Service\Calendar\Event $event Google\Service\Calendar\Event Object
     *
     * @return array key/value array with [sModified, $gModified, lastsync] keys
     */
    public function getTimeStrings(Meeting $meeting, Google\Service\Calendar\Event $event)
    {
        $timeArray = array();

        // Get the last modified time from google event
        $timeArray['gModified'] = strtotime($event->getUpdated());

        // Get last modified of SuiteCRM event
        $date = !empty($meeting->fetched_row['date_modified']) ? $meeting->fetched_row['date_modified']. ' UTC' : 'now';
        $timeArray['sModified'] = strtotime($date); // SuiteCRM stores the timedate as UTC in the DB

        // Get the last sync time of SuiteCRM event
        $timeArray['lastSync'] = 0;
        if (isset($meeting->fetched_row['gsync_lastsync'])) {
            $timeArray['lastSync'] = $meeting->fetched_row['gsync_lastsync'];
        }

        return $timeArray;
    }

    /**
     * Helper method for GoogleSync::pushPullSkip.
     *
     * Takes two calendar events and the timeArray from getTimeStrings, and returns a push/pull[_delete] string.
     *
     * @param Meeting $meeting Meeting Bean
     * @param \Google\Service\Calendar\Event $event Google\Service\Calendar\Event Object
     * @param array timeArray from getTimeStrings
     *
     * @return string 'push(_delete)', 'pull(_delete)'
     */
    public function getNewestMeetingResponse(Meeting $meeting, Google\Service\Calendar\Event $event, array $timeArray)
    {
        if ($timeArray['gModified'] > $timeArray['sModified']) {
            if ($event->status == 'cancelled') {
                // if the remote event is deleted, delete it here
                return "pull_delete";
            }
            return "pull";
        }
        if ($meeting->deleted == '1') {
            return "push_delete";
        }
        return "push";
    }

    /**
    * Helper method for GoogleSync::pushPullSkip.
    *
    * Takes two calendar events and the timeArray from getTimeStrings, and returns bool (should we skip this record).
    *
    * @param Meeting $meeting Meeting Bean
    * @param \Google\Service\Calendar\Event $event Google\Service\Calendar\Event Object
    * @param array $timeArray from getTimeStrings
    * @param array $syncedList from GoogleSyncBase Class
    *
    * @return bool should we skip this record
    */
    public function isSkippable(Meeting $meeting, Google\Service\Calendar\Event $event, array $timeArray, array $syncedList)
    {
        $ret = false;

        // Check if we already sync'ed this event on this run
        if (in_array($meeting->id, $syncedList, true)) {
            $ret = true;
        }

        // Check if event is deleted on both ends
        if ($meeting->deleted == '1' && $event->status == 'cancelled') {
            $ret = true;
        }

        // Event has not been modified since last sync... skip
        if ($timeArray['gModified'] <= $timeArray['lastSync'] && $timeArray['sModified'] <= $timeArray['lastSync']) {
            $ret = true;
        }

        return $ret;
    }

    /**
     * Helper Method for GoogleSyncBase::updateSuitecrmMeetingEvent
     *
     * Creates reminders for event from google event reminders
     *
     * @param array $overrides Google Calendar Event Reminders (See Class Google\Service\Calendar\EventReminders)
     * @param string $meeting Meeting Bean
     *
     * @return array|bool Nested array of unsaved reminders and reminder_invitees, false on Failure
     */
    public function createSuitecrmReminders(array $overrides, Meeting $meeting)
    {
        $reminders = array();
        $invitees = array();

        foreach ($overrides as $override) {
            if ($override->getMethod() == 'popup') {
                $sReminder = BeanFactory::getBean('Reminders');
                if (!$sReminder) {
                    throw new Exception('Unable to get Reminder bean.');
                }
                $sReminder->popup = '1';
                $sReminder->timer_popup = $override->getMinutes() * 60;
                $sReminder->related_event_module = $meeting->module_name;
                $sReminder->related_event_module_id = $meeting->id;
                $sReminder->new_with_id = true;
                $sReminder->id = create_guid();
                $reminders[] = $sReminder;

                $reminderInvitee = BeanFactory::getBean('Reminders_Invitees');
                if (!$reminderInvitee) {
                    throw new Exception('Unable to get Reminders_Invitees bean.');
                }
                $reminderInvitee->reminder_id = $sReminder->id;
                $reminderInvitee->related_invitee_module = 'Users';
                $reminderInvitee->related_invitee_module_id = $meeting->assigned_user_id;
                $reminderInvitee->new_with_id = true;
                $reminderInvitee->id = create_guid();
                $invitees[] = $reminderInvitee;
            }
        }
        $ret = array($reminders, $invitees);
        return $ret;
    }

    /**
     * Helper Method for GoogleSyncBase::setUsersGoogleCalendar
     *
     * Wipe the Google Sync data (gsync_id and gsync_lastsync fields) from the users SuiteCRM records
     *
     * @param string $assigned_user_id The user who's events need to be fixed.
     *
     * @return bool True on success, False on failure
     */
    public function wipeLocalSyncData($assigned_user_id)
    {
        $db = DBManagerFactory::getInstance();
        $query = "UPDATE meetings SET gsync_id = NULL, gsync_lastsync = NULL WHERE assigned_user_id = {$db->quoted($assigned_user_id)}";
        $db->query($query);
    }
}
