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
class stic_RegistrationsUtils {

/**
 * Calculate the number of entries by type in the event.
 * It is executed after the events after_save, after_relationship_delete and after_relationship_add of the event.
 * In the event that the threshold of 80% or 100% of those registered has been exceeded, notification is sent to the administrator
 *
 * @param Object $registrationBean
 * @return void
 */
    public static function recalculateTotalAttendees($registrationBean, $eventId = false) {

        // We get directly the id of the linked event if we have not received it as a function parameter
        if (empty($eventId)) {
            $query =
                "SELECT
                    e.id
                FROM
                    stic_events e
                JOIN stic_registrations_stic_events_c ei on
                    ei.stic_registrations_stic_eventsstic_events_ida = e.id
                WHERE
                    ei.stic_registrations_stic_eventsstic_registrations_idb = '{$registrationBean->id}'
                    AND e.deleted = 0
                    AND ei.deleted = 0";

            $eventId = $registrationBean->db->getOne($query);
        }

        // This avoid create empty events if no $eventId
        if (empty($eventId)) {
            return false;
        }

        // We retrieve the event bean
        $eventBean = BeanFactory::getBean('stic_Events', $eventId);

        // We save the current number of confirmed
        $confirmed = $eventBean->status_confirmed;

        // We calculate those registrations by state.
        $eventBean->status_confirmed = 0;
        $eventBean->status_not_invited = 0;
        $eventBean->status_took_part = 0;
        $eventBean->status_rejected = 0;
        $eventBean->status_invited = 0;
        $eventBean->status_didnt_take_part = 0;
        $eventBean->status_maybe = 0;
        $eventBean->status_drop_out = 0;

        $sql =
            "SELECT
                sum(attendees) as total ,
                sr.status
            FROM
                stic_registrations sr
            INNER JOIN stic_registrations_stic_events_c srse ON
                sr.id = srse.stic_registrations_stic_eventsstic_registrations_idb
                AND srse.deleted = 0
            WHERE
                srse.stic_registrations_stic_eventsstic_events_ida = '{$eventId}'
                AND sr.deleted = 0
                AND srse.deleted=0
            GROUP BY
                sr.status";

        global $db;
        $result = $db->query($sql);

        while ($row = $db->fetchByAssoc($result)) {
            switch ($row['status']) {
            case 'confirmed':
                $eventBean->status_confirmed = $row['total'];
                break;
            case 'uninvited':
                $eventBean->status_not_invited = $row['total'];
                break;
            case 'participates':
                $eventBean->status_took_part = $row['total'];
                break;
            case 'rejected':
                $eventBean->status_rejected = $row['total'];
                break;
            case 'invited':
                $eventBean->status_invited = $row['total'];
                break;
            case 'not_participate':
                $eventBean->status_didnt_take_part = $row['total'];
                break;
            case 'maybe':
                $eventBean->status_maybe = $row['total'];
                break;
            case 'dropped':
                $eventBean->status_drop_out = $row['total'];
                break;
            }
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Updating event [{$eventBean->name}]: status_confirmed [{$eventBean->status_confirmed}] status_not_invited [{$eventBean->status_not_invited}] status_took_part [{$eventBean->status_took_part}] status_rejected [{$eventBean->status_rejected}] status_invited [{$eventBean->status_invited}] status_didnt_take_part [{$eventBean->status_didnt_take_part}] status_maybe [{$eventBean->status_maybe}] status_drop_out [{$eventBean->status_drop_out}] ...");

        $eventBean->save(false);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':' . "Previous Confirmed [{$confirmed}], current confirmed [{$eventBean->status_confirmed}].");

        // If we have a maximum number of registered and it has changed, we calculate if we are surpassing it or not
        if (!empty($eventBean->max_attendees) && $confirmed != $eventBean->status_confirmed) {
            $currentPercentage = ($eventBean->status_confirmed / $eventBean->max_attendees) * 100;
            $previousPercentage = ($confirmed / $eventBean->max_attendees) * 100;
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":Previous percentage [{$previousPercentage}] current percentage [{$currentPercentage}]");

            // If we go from a previous percentage lower than 80 to a percentage between 80 and 100, we send a notification of 80%
            if ($previousPercentage < 80 && $currentPercentage >= 80 && $currentPercentage < 100) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Sending 80% notification');
                self::sendNotificationMail($eventBean, '80');
                // If we go from a previous percentage lower than 100 to a percentage equal to or greater than 100, we send a 100% notification
            } else if ($previousPercentage < 100 && $currentPercentage >= 100) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Sending 100% notification');
                self::sendNotificationMail($eventBean, '100');
            }
        }
    }

    /**
     * Send the notification email
     * @param $eventBean: Bean of the event associated with the notification
     * @param $notificationType: Indicates the type of notification to be sent: '100' or '80'
     **/

    public static function sendNotificationMail($eventBean, $notificationType) {

        global $current_user, $mod_strings, $sugar_config;

        // We recover the data of the user responsible for the event
        $user = null;
        if (!empty($eventBean->assigned_user_id)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Recovering user data [{$eventBean->assigned_user_id}] assigned to the event [{$eventBean->id} - {$eventBean->name}]...");
            $user = BeanFactory::getBean('Users', $eventBean->assigned_user_id);
        }

        // If it was not possible to recover, the current user is used
        if (empty($user)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Unable to retrieve the user ID assigned to the event [{$eventBean->id} - {$eventBean->name}]. Notification will be sent to the current user [{$current_user->id}].");
            $user = $current_user;
        }

        // We retrieve the email address of the user to whom the notification will be sent and prepare the message
        $address = null;
        if (!$user->emailAddress || !($address = $user->emailAddress->getPrimaryAddress($user))) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": The user [{$user->id}] has no associated email address, so the notification cannot be sent.");
        } else {

            // We call the Sugar mailer to proceed to send the message
            require_once 'include/SugarPHPMailer.php';
            $mail = new SugarPHPMailer();

            // We create the message to send
            require_once 'modules/Emails/Email.php';
            $emailObj = new Email();
            $defaults = $emailObj->getSystemDefaultEmail();

            $mail->From = $defaults['email'];
            $mail->FromName = $defaults['name'];

            $mail->ClearAllRecipients();
            $mail->ClearReplyTos();
            $mail->AddAddress($address, $user->first_name . ' ' . $user->last_name);

            $label = '';
            switch ($notificationType) {
            case '100':
                $label = translate('LBL_EMAIL_REACHED_100', 'stic_Registrations');
                break;
            case '80':
                $label = translate('LBL_EMAIL_REACHED_80', 'stic_Registrations');
                break;
            }
            $mail->Subject = $label . ' ' . $eventBean->name;

            $url = rtrim($sugar_config['site_url'], '/')."/index.php?module={$eventBean->module_name}&action=DetailView&record={$eventBean->id}";
            $mail->Body = from_html($label . "<a href=\"{$url}\">{$eventBean->name}</a>.");

            $mail->IsHTML(true);

            $mail->setMailerForSystem();
            $mail->prepForOutbound();

            // Send the message
            if (!$mail->Send()) {
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error sending notification email.");
            }
        }
    }

    /**
     * Try create attendances (if neccesary) for new registration. This method is uswed from registration module logic hooks. If registration date * is empty, use the first session date from the related event sessions. The end date is always current date.
     *
     * @param Object $registrationBean Registration bean
     * @return void
     */
    public static function createAttendancesForNewRegistration($registrationBean) {

        // Try create attendances for the new related registration
        global $timedate;
        require_once 'modules/stic_Attendances/Utils.php';

        // If registration_date is empty, use first session date
        if (empty($registrationBean->registration_date)) {
            $queryGetMinSessionDate =
                "SELECT
                    MIN(start_date)
                FROM
                stic_registrations_stic_events_c re
                JOIN stic_sessions_stic_events_c se ON
                    re.stic_registrations_stic_eventsstic_events_ida = se.stic_sessions_stic_eventsstic_events_ida
                JOIN stic_sessions s ON
                    se.stic_sessions_stic_eventsstic_sessions_idb = s.id
                WHERE
                    re.stic_registrations_stic_eventsstic_registrations_idb  = '{$registrationBean->id}'
                    AND re.deleted = 0
                    AND se.deleted = 0
                    AND s.deleted = 0";
            $startDay = substr($registrationBean->db->getOne($queryGetMinSessionDate, true), 0, 10);
        } else {
            $startDay = substr($registrationBean->registration_date, 0, 10);
        }
        $endDay = date('Y-m-d');

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  ' . $startDay . '|' . $endDay);
        $start = new DateTime($startDay);
        $end = new DateTime($endDay);

        $numDays = $start->diff($end)->days;
        $dayToCreate = $start->format('Y-m-d');
        $a = 0;
        while ($a <= $numDays) {
            stic_AttendancesUtils::createAttendances($dayToCreate, $registrationBean->id, null);
            $dayToCreate = $start->add(new DateInterval('P1D'))->format('Y-m-d');
            $a++;
        }

    }
}
