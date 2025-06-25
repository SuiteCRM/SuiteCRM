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
class stic_EventsUtils
{

    /**
     * This function creates periodic sessions for a certain event, based on the parameters received via $_REQUEST
     * and defined in the periodic session creation wizard (modules/stic_Events/SessionWizard.tpl)
     *
     * @return void
     */
    public static function createPeriodicSessions()
    {
        // Disable Advanced Open Discovery to avoid slowing down the writing of the records affected by this function.
        global $sugar_config;
        $aodConfig = $sugar_config['aod']['enable_aod'];
        $sugar_config['aod']['enable_aod'] = false;

        $startTime = microtime(true);

        // TIP: the action can be run from the web browser using this url:
        // http://<CRM domain>/index.php?module=stic_Events&action=createPeriodicSessions&return_module=stic_Events&return_action=index&repeat_type=Daily&repeat_interval=1&repeat_count=3&repeat_until=&repeat_start_day=02/04/2019&repeat_final_day=02/04/2019&repeat_start_hour=09&repeat_start_minute=0&repeat_final_hour=10&repeat_final_minute=1&event_id=<id_evento>"

        global $db, $current_user, $timedate;

        // Get the data from the smarty template
        $user = $current_user->id;
        $type = $_REQUEST['repeat_type'];
        $interval = $_REQUEST['repeat_interval'];
        $count = $_REQUEST['repeat_count'] ?? null;
        $until = $_REQUEST['repeat_until']??null;
        $startDay = $_REQUEST['repeat_start_day'];
        $finalDay = $_REQUEST['repeat_final_day'];
        $startHour = $_REQUEST['repeat_start_hour'];
        $startMinute = $_REQUEST['repeat_start_minute'];
        $finalHour = $_REQUEST['repeat_final_hour'];
        $finalMinute = $_REQUEST['repeat_final_minute'];

        // Get absolute values of 'minutes' and set values
        // for $finalDay and $finalHour if necessary

        // Set minute interval as defined in $sugar_config
        $m = 0;
        $minutesInterval = $sugar_config['stic_datetime_combo_minute_interval'] ?: 15;
        $repeatMinuts1 = array('00');
        do {
            $m = $m + $minutesInterval;
            $repeatMinuts1[] = str_pad($m, 2, '0', STR_PAD_LEFT);
        } while ($m < (60 - $minutesInterval));

        $startMinute = $repeatMinuts1[$startMinute];
        $finalMinute = $repeatMinuts1[$finalMinute];
        if ($finalDay == '') {$finalDay = $startDay;}
        if ($finalHour == '00') {$finalHour = $startHour + 1;}
        if ($finalHour < $startHour and $finalDay == $startDay) {$finalHour = $startHour + 1;}

        // Take the dates collected in the smarty template and set their values
        // in order to calculate the duration of the session
        $until = str_replace('/', '-', $until??'');
        $until = date("Y-m-d", strtotime($until));
        $startDay = str_replace('/', '-', $startDay);
        $startDay = date('Y-m-d H:i:s', strtotime($startDay . " + $startHour hours + $startMinute minutes"));
        $finalDay = str_replace('/', '-', $finalDay);
        $finalDay = date('Y-m-d H:i:s', strtotime($finalDay . " + $finalHour hours + $finalMinute minutes"));
        $duration = strtotime($finalDay) - strtotime($startDay);

        $date = [];

        // Depending on the chosen type, perform the right operation
        // (none, daily, weekly, monthly or annual)
        if ($type == '') {
            header("Location: index.php?action=index&module=stic_Events");
        } else {
            // Daily
            if ($type == 'Daily') {
                $firstDay = $startDay;
                if ($count != '' and $count != '0') {
                    for ($i = 0; $i < $count; $i++) {
                        $date[$i] = $firstDay;
                        $firstDay = date('Y-m-d H:i:s', strtotime($firstDay . " + $interval days"));
                    }
                } else if ($until != '') {
                    $first_d = date("Y-m-d", strtotime($firstDay));
                    for ($i = 0; strtotime($first_d) <= strtotime($until); $i++) {
                        $date[$i] = $firstDay;
                        $firstDay = date('Y-m-d H:i:s', strtotime($firstDay . " + $interval days"));
                        $first_d = date("Y-m-d", strtotime($firstDay));
                    }
                }
            }
            // Monthly
            if ($type == 'Monthly') {
                $firstMonth = $startDay;
                if ($count != '' and $count != '0') {
                    for ($i = 0; $i < $count; $i++) {
                        $date[$i] = $firstMonth;
                        $firstMonth = date('Y-m-d H:i:s', strtotime($firstMonth . " + $interval months"));
                    }
                } else if ($until != '') {
                    $firstM = date("Y-m-d", strtotime($firstMonth));
                    for ($i = 0; strtotime($firstM) <= strtotime($until); $i++) {
                        $date[$i] = $firstMonth;
                        $firstMonth = date('Y-m-d H:i:s', strtotime($firstMonth . " + $interval  months"));
                        $firstM = date("Y-m-d", strtotime($firstMonth));
                    }
                }
            }
            // Yearly
            if ($type == 'Yearly') {
                $firstYear = $startDay;
                if ($count != '' and $count != '0') {
                    for ($i = 0; $i < $count; $i++) {
                        $date[$i] = $firstYear;
                        $firstYear = date('Y-m-d H:i:s', strtotime($firstYear . " + $interval years"));
                    }
                } else if ($until != '') {
                    $firstY = date("Y-m-d", strtotime($firstYear));
                    for ($i = 0; strtotime($firstY) <= strtotime($until); $i++) {
                        $date[$i] = $firstYear;
                        $firstYear = date('Y-m-d H:i:s', strtotime($firstYear . " + $interval years"));
                        $firstY = date("Y-m-d", strtotime($firstYear));
                    }
                }
            }
            // Weekly
            if ($type == 'Weekly') {
                // We create the table $dow of the days of the week, fixing the problem that
                // in the smarty template Sunday is in position '0' and not in position '7'
                $times = 0;
                for ($i = 1; $i < 7; $i++) {
                    $dow[$i] = $_REQUEST['repeat_dow_' . $i]??null;
                    if ($dow[$i] == 'on') {
                        $times = $times + 1;
                        $dow[$i] = 1;} else { $dow[$i] = 0;}
                }
                $zero = 0;
                $dow[7] = $_REQUEST['repeat_dow_' . $zero]??null;
                if ($dow[7] == 'on') {
                    $times = $times + 1;
                    $dow[7] = 1;} else { $dow[7] = 0;}

                if ($times > '0') {
                    $days = array('1', '2', '3', '4', '5', '6', '7');
                    $firstWeek = $startDay;
                    $week = 1;
                    $i = 0;
                    if ($count != '' and $count != '0') {
                        while ($i < $count) {
                            if ($week == '1') {
                                $firstDate = $firstWeek;
                                $weekDay = $days[date('w', strtotime($firstDate))];
                                $weekDay2 = $weekDay;
                                if ($weekDay == '1') {$weekDay = '7';} else { $weekDay = $weekDay - 1;}
                                for ($x = $weekDay; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $adder = $x - $weekDay;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($firstDate . " + $adder days"));
                                        $date[$i] = $firstWeek;
                                        $i = $i + 1;
                                        if ($i == $count) {$x = 8;}
                                    }
                                }
                                $week = '2';
                                if ($interval > 1) {$x = $interval - 1;} else { $x = '0';}
                                if ($weekDay2 == '1') {
                                    $weekDay2 = '8';
                                    $x = 0;}
                                $differenceDay = 8 - $weekDay2;
                                $firstWeek = date('Y-m-d H:i:s', strtotime($startDay . " + $x weeks + $differenceDay days"));
                            }

                            if ($week == '2' and $i < $count) {
                                $newWeek = $firstWeek;
                                for ($x = 1; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $addDays = $x;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $addDays days"));
                                        $date[$i] = $firstWeek;
                                        $i = $i + 1;if ($i == $count) {$x = 8;}
                                    }
                                }
                                $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $interval weeks"));
                            }
                        }
                    } else if ($until != '') {
                        $firstWeek = $startDay;
                        $week = 1;
                        $i = 0;

                        while (strtotime($firstWeek) <= strtotime($until)) {
                            if ($week == '1') {
                                $startDay = $timedate->asDb($timedate->fromString($startDay), $current_user);
                                $finalDay = $timedate->asDb($timedate->fromString($finalDay), $current_user);
                                $firstDate = $firstWeek;
                                $weekDay = $days[date('w', strtotime($firstDate))];
                                $weekDay2 = $weekDay;
                                if ($weekDay == '1') {$weekDay = '7';} else { $weekDay = $weekDay - 1;}
                                for ($x = $weekDay; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $adder = $x - $weekDay;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($firstDate . " + $adder days"));
                                        $date[$i] = $firstWeek;
                                        $i = $i + 1;
                                        if (strtotime($firstWeek) == strtotime($until)) {$x = 8;}
                                    }
                                }
                                $week = '2';
                                if ($interval > 1) {$x = $interval - 1;} else { $x = '0';}
                                if ($weekDay2 == '1') {
                                    $weekDay2 = '8';
                                    $x = 0;}
                                $differenceDay = 8 - $weekDay2;
                                $firstWeek = date('Y-m-d H:i:s', strtotime($firstDate . " + $x weeks + $differenceDay days"));
                            }
                            if ($week == '2' and strtotime($firstWeek) <= strtotime($until)) {
                                $newWeek = $firstWeek;
                                for ($x = 1; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $addDays = $x;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $addDays days"));

                                        if ($until >= substr($firstWeek, 0, 10)) {
                                            $date[$i] = $firstWeek;
                                        }

                                        $i = $i + 1;
                                        if (strtotime($firstWeek) == strtotime($until)) {$x = 8;}
                                    }
                                }

                                $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $interval weeks"));
                            }
                        }
                    }
                } else {
                    $firstWeek = $startDay;
                    if ($count != '' and $count != '0') {
                        for ($i = 0; $i < $count; $i++) {
                            $date[$i] = $firstWeek;
                            $firstWeek = date('Y-m-d H:i:s', strtotime($firstWeek . " + $interval weeks"));
                        }
                    } else if ($until != '') {
                        $firstW = date("Y-m-d", strtotime($firstWeek));
                        for ($i = 0; strtotime($firstW) <= strtotime($until); $i++) {
                            $date[$i] = $firstWeek;
                            $firstWeek = date('Y-m-d H:i:s', strtotime($firstWeek . " + $interval weeks"));
                            $firstW = date("Y-m-d", strtotime($firstWeek));
                        }
                    }
                }
            }
        }

        // Get and save other data
        $eventId = $_REQUEST['event_id'];
        $counter = count($date);

        // Boost performance by not updating related event until the last session
        $_SESSION['notUpdateRelatedEvent'] = true;

        // Loop for session creation
        for ($i = 0; $i < $counter; $i++) {

            // If creating the last session let related event to be updated
            if ($counter == $i + 1) {
                unset($_SESSION['notUpdateRelatedEvent']);
            }

            $date[$i] = $timedate->to_db($timedate->to_display_date_time($date[$i], true, false, $current_user));

            if ($finalDay != '') {
                $finalDay = strtotime($date[$i]) + $duration;
                $finalDay = date('Y-m-d H:i:s', $finalDay);
            }
            $sessionBean = BeanFactory::newBean('stic_Sessions');
            if (isset($_REQUEST['session_name']) && $_REQUEST['session_name'] != '') {
                $sessionName = $_REQUEST['session_name'];
                // Check if the variable is present in the string
                if (strpos($sessionName, '{{$counter}}') !== false) {
                    // Substitute the variable with the integer
                    $sessionName = str_replace('{{$counter}}', ($i+1), $sessionName);
                }
                $sessionBean->name = $sessionName;
            }
            $sessionBean->start_date = $date[$i];
            $sessionBean->end_date = $finalDay;
            $sessionBean->stic_sessions_stic_eventsstic_events_ida = $eventId;

            if (isset($_REQUEST['assigned_user_id']) && $_REQUEST['assigned_user_id'] != '') {
                $sessionBean->assigned_user_id = $_REQUEST['assigned_user_id'];
            } else {
                $sessionBean->assigned_user_id = $user;
            }
            
            if (isset($_REQUEST['responsible_id']) && $_REQUEST['responsible_id'] != '') {
                $sessionBean->contact_id_c = $_REQUEST['responsible_id'];
            }

            if (isset($_REQUEST['color']) && $_REQUEST['color'] != '') {
                $sessionBean->color = $_REQUEST['color'];
            } else {
                $eventBean = BeanFactory::getBean('stic_Events', $sessionBean->stic_sessions_stic_eventsstic_events_ida);

                $sessionBean->color = $eventBean->color??null;
            }
            if (isset($_REQUEST['description']) && $_REQUEST['description'] != '') {
                $sessionBean->description = $_REQUEST['description'];
            }

            if (isset($_REQUEST['activity_type'])) {
                $activityType = $_REQUEST['activity_type'];
                if (is_array($activityType)) {
                    $sessionBean->activity_type = encodeMultienumValue($activityType);
                } else {
                    $sessionBean->activity_type = '^'.$activityType.'^';
                }
            }
            $sessionBean->save();
        }
        $endTime = microtime(true);
        $totalTime = $endTime - $startTime;
        $GLOBALS['log']->debug(__METHOD__ . '(' . __LINE__ . ") >> Has been created $i sessions in $totalTime seconds");
        // For total security, unset the control variable anyway
        unset($_SESSION['notUpdateRelatedEvent']);

        // Calculate total Sessions duration for this event
        self::setEventTotalHours($eventId);

        // Reactivamos la configuración previa de Advanced Open Discovery
        $sugar_config['aod']['enable_aod'] = $aodConfig;

        header("Location: index.php?action=DetailView&module=stic_Events&record=$eventId");
    }

    /**
     * It generates an registration to the event that invokes the action for each contact, account or lead present in the selected LPO.
     *
     * This function is invoked from action_addTargetListToEventRegistrations() function in module controller which shows a popup window to select the LPO. The function avoids duplicates
     *
     * @param String $prospectListId Id of prospect list to use
     * @param Object $eventBean Event Bean in which create the registrations
     * @return void
     */
    public static function addTargetListToEventRegistrations($prospectListId, $eventBean)
    {

        global $db, $current_user;

        $prospectList = BeanFactory::getBean('ProspectLists', $prospectListId);
        if ($prospectList === false) {
            SugarApplication::appendErrorMessage('Target list not found');
        }

        // Retrieving the IDs of the contacts that are already in Event
        $contactsIds = array();
        $contactsSql = "SELECT
                    rc.stic_registrations_contactscontacts_ida as id
                FROM
                    stic_registrations_stic_events_c srsec
                INNER JOIN stic_registrations_contacts_c rc ON
                    rc.stic_registrations_contactsstic_registrations_idb = srsec.stic_registrations_stic_eventsstic_registrations_idb
                    AND rc.deleted = 0
                WHERE
                    srsec.deleted = 0
                    AND srsec.stic_registrations_stic_eventsstic_events_ida ='{$eventBean->id}'";

        $contactsSqlResult = $db->query($contactsSql);
        while ($row = $db->fetchByAssoc($contactsSqlResult)) {
            $contactsIds[$row['id']] = $row['id'];
        }

        // Retrieving contacts from Prospect_list
        $prospectList->load_relationship('contacts');
        foreach ($prospectList->contacts->getBeans() as $contact) {
            // We check if the contact is already added in this Event
            if (!in_array($contact->id, $contactsIds)) {
                $registrationBean = new stic_Registrations();
                $registrationBean->name = $contact->name . " - " . $eventBean->name;
                $registrationBean->status = 'uninvited';
                $registrationBean->registration_date = gmdate('Y-m-d H:i:s');
                $registrationBean->stic_registrations_stic_eventsstic_events_ida = $eventBean->id;
                $registrationBean->stic_registrations_contactscontacts_ida = $contact->id;
                $registrationBean->assigned_user_id = $current_user->id;
                $registrationBean->save(false);
            }
        }

        // Retrieving the IDs of the accounts that are already in Event
        $accountsIds = array();
        $accountsSql = "SELECT
                            rc.stic_registrations_accountsaccounts_ida as id
                        FROM
                            stic_registrations_stic_events_c srsec
                        INNER JOIN stic_registrations_accounts_c rc ON
                            rc.stic_registrations_accountsstic_registrations_idb = srsec.stic_registrations_stic_eventsstic_registrations_idb
                            AND rc.deleted = 0
                        WHERE
                            srsec.deleted = 0
                            AND srsec.stic_registrations_stic_eventsstic_events_ida ='{$eventBean->id}'";

        $accountsSqlResult = $db->query($accountsSql);
        while ($row = $db->fetchByAssoc($accountsSqlResult)) {
            $accountsIds[$row['id']] = $row['id'];
        }

        // Retrieving contacts from Prospect_list
        $prospectList->load_relationship('accounts');
        foreach ($prospectList->accounts->getBeans() as $account) {
            // We check if the contact is already added in this Event
            if (!in_array($account->id, $accountsIds)) {
                $registrationBean = new stic_Registrations();
                $registrationBean->name = $account->name . " - " . $eventBean->name;
                $registrationBean->status = 'uninvited';
                $registrationBean->registration_date = gmdate('Y-m-d H:i:s');
                $registrationBean->stic_registrations_stic_eventsstic_events_ida = $eventBean->id;
                $registrationBean->stic_registrations_accountsaccounts_ida = $account->id;
                $registrationBean->assigned_user_id = $current_user->id;
                $registrationBean->save(false);
            }
        }

        // Retrieving the IDs of the accounts that are already in Event
        $leadIds = array();
        $leadsSql = "SELECT
                        rc.stic_registrations_leadsleads_ida as id
                    FROM
                        stic_registrations_stic_events_c srsec
                    INNER JOIN stic_registrations_leads_c rc ON
                        rc.stic_registrations_leadsstic_registrations_idb = srsec.stic_registrations_stic_eventsstic_registrations_idb
                        AND rc.deleted = 0
                    WHERE
                        srsec.deleted = 0
                        AND srsec.stic_registrations_stic_eventsstic_events_ida ='{$eventBean->id}'";

        $leadsSqlResult = $db->query($leadsSql);
        while ($row = $db->fetchByAssoc($leadsSqlResult)) {
            $leadIds[$row['id']] = $row['id'];
        }

        // Retrieving contacts from Prospect_list
        $prospectList->load_relationship('leads');
        foreach ($prospectList->leads->getBeans() as $lead) {

            // We check if the contact is already added in this Event
            if (!in_array($lead->id, $leadIds)) {
                $registrationBean = new stic_Registrations();
                $registrationBean->name = $lead->name . " - " . $eventBean->name;
                $registrationBean->status = 'uninvited';
                $registrationBean->registration_date = gmdate('Y-m-d H:i:s');
                $registrationBean->stic_registrations_stic_eventsstic_events_ida = $eventBean->id;
                $registrationBean->stic_registrations_leadsleads_ida = $lead->id;
                $registrationBean->assigned_user_id = $current_user->id;
                $registrationBean->save(false);
            }
        }

        SugarApplication::redirect("index.php?module={$eventBean->module_dir}&action=DetailView&record={$eventBean->id}");
    }

    /**
     * setEventTotalHours Set the total hours for $eventId by summing its sessions durations
     *
     * @param mixed $eventId String
     * @return void
     */
    public static function setEventTotalHours($eventId)
    {
        // Check that an event_id is provided. If not, exit to avoid creating an empty event when saving
        if (empty($eventId)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . '.Exit without set Event total hours, because event is empty.');
            return;
        }

        // When creating multiple periodical sessions for the event, in order to boost performance,
        // let's use a session var to only update the event when creating the last session.
        if (isset($_SESSION['notUpdateRelatedEvent'])) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Creating Periodic Sessions. Not last session: Total Hours not Updated.');
            return;
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Creating Periodic Sessions - Is last session: Updating Total Hours.');
        }

        include_once 'include/utils.php';
        global $current_user, $sugar_config;

        // Get user config
        if (!empty($current_user->id)) {
            $userDecS_sep = $current_user->getPreference('dec_sep');
            $dec_sep = (empty($userDecS_sep) ? $sugar_config['default_decimal_seperator'] : $userDecS_sep);
        }

        $configGroupNumberSeparator = $sugar_config['default_number_grouping_seperator'];
        if (!empty($current_user->id)) {
            $userGroupNumberSeparator = $current_user->getPreference('num_grp_sep');
            $configGroupNumberSeparator = (empty($userGroupNumberSeparator) ? $sugar_config['default_number_grouping_seperator'] : $userGroupNumberSeparator);
        }

        $eventBean = BeanFactory::getBean('stic_Events', $eventId);

        // Get event total hours by summing its sessions durations
        $query =
            "SELECT
                SUM(s.duration)
            FROM
                stic_sessions s
            JOIN stic_sessions_stic_events_c se ON
                s.id = se.stic_sessions_stic_eventsstic_sessions_idb
            WHERE
                se.stic_sessions_stic_eventsstic_events_ida = '$eventId'
                AND s.deleted = 0
                AND se.deleted = 0";
        $res = $eventBean->db->getOne($query);

        // Set event total hours
        $eventBean->total_hours = str_replace('.', $dec_sep, $res??'');
        $eventBean->save();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Setting total hours for event: ' . $eventBean->name . ". Total Hours=" . $eventBean->total_hours);
    }
}
