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

#[\AllowDynamicProperties]
class stic_Work_Calendar extends Basic
{
    public const ALL_DAY_TYPES = ['vacation', 'holiday', 'personal', 'sick', 'leave'];

    public $new_schema = true;
    public $module_dir = 'stic_Work_Calendar';
    public $object_name = 'stic_Work_Calendar';
    public $table_name = 'stic_work_calendar';
    public $importable = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $SecurityGroups;
    public $type;
    public $start_date;
    public $end_date;
    public $duration;
    public $weekday;

    public function bean_implements($interface)
    {
        switch($interface)
        {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     * Override bean's save function to calculate the valor of the some fields
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = true)
    {
        global $app_list_strings, $current_user, $timedate;
        if ($_REQUEST["action"] && $_REQUEST["action"] != "saveHTMLField") // Inline Edit
        {
            $assignedUser = BeanFactory::getBean('Users', $this->assigned_user_id);
            $typeLabel = $app_list_strings['stic_work_calendar_types_list'][$this->type];
            $startDateInUTC = $timedate->fromDbFormat($this->start_date, TimeDate::DB_DATETIME_FORMAT);
            
            if ($_REQUEST["action"] && $_REQUEST["action"] != "Save") // MassUpdate, API, Import..
            {
                // Disable disable date_format so that $timedate object calculates start and end dates in user format when the action does not come from the user interface
                $GLOBALS['disable_date_format'] = false;
                $startDateInUserFormat = $timedate->asUser($startDateInUTC, $current_user);
            } else {
                $startDateInUserFormat = $timedate->asUser($startDateInUTC, $current_user);
            }

            if (!in_array($this->type, self::ALL_DAY_TYPES)) {
                $endDate = $timedate->fromDbFormat($this->end_date, TimeDate::DB_DATETIME_FORMAT);
                $endDate = $timedate->asUser($endDate, $current_user);                
                $this->name = $assignedUser->name . " - " . $typeLabel . " - " . $startDateInUserFormat . " - " . substr($endDate, -5);
            } else {
                // If it is a new record or the type before the modification was not an all-day one
                if (!isset($this->fetched_row['type']) || !in_array($this->fetched_row['type'], self::ALL_DAY_TYPES)) 
                {
                    $auxStartDate = $startDateInUTC;
                    if ($_REQUEST["action"] != "Save")
                    {
                        // Convert $auxStartDate to UTC
                        $userTZ = $current_user->getPreference('timezone');
                        $auxStartDate->setTimezone(new DateTimeZone($userTZ));

                        // Set the time to 00:00:00 of the start date and time
                        $auxStartDate = date('Y-m-d 00:00:00', strtotime($auxStartDate->format(TimeDate::DB_DATETIME_FORMAT)));

                        // Convert $auxStartDate to UTC
                        $dateInUserTZ = new DateTime($auxStartDate, new DateTimeZone($userTZ));
                        $auxStartDate = $dateInUserTZ->setTimezone(new DateTimeZone('UTC'));
                    }

                    // Update the start date and time, the end date and time after adding one day, and the record name
                    $this->start_date = $timedate->asDb($auxStartDate, $current_user);  
                    $this->end_date = $timedate->asDb($auxStartDate->modify("next day"), $current_user);                         
                } else {
                    $endDate = $timedate->fromDbFormat($this->start_date, TimeDate::DB_DATETIME_FORMAT);
                    $endDate = $endDate->modify("next day");
                    $this->end_date = $timedate->asDb($endDate, $current_user);  
                }
                $this->name = $assignedUser->name . " - " . $typeLabel . " - " . substr($startDateInUserFormat, 0, 10);       
            }

            if ($_REQUEST["action"] != "Save" && $_REQUEST["action"] != "runMassUpdateDates") // MassUpdate, API, Import..
            {
                // Reactivate disable date_format to work with the rest of the date type fields
                $GLOBALS['disable_date_format'] = true;
            }

            // Set duration field
            if (!empty($this->end_date)) {
                $startTime = strtotime($this->start_date);
                $endTime = strtotime($this->end_date);
                $duration = $endTime - $startTime;            
                $this->duration = (float) number_format($duration / 3600, 2);            
            } else {
                $this->duration = 0;
            }      

            // Set weekday field
            if (isset($this->fetched_row['start_date']) && $this->start_date != $this->fetched_row['start_date']) {
                $this->weekday = date('w', strtotime($startDateInUTC));
            }
        }

        // Save the bean
        parent::save($check_notify);
    }

    /**
     * Checks if the given user has an uncancelled calendar record between the previous 24 hours and now, in UTC
     * @param userId User Identificator
     * @return void
     */
    public static function existAtLeastOneRecordFromYesterday($userId)
    {
        global $db;
        $query = "SELECT count(id) as count
                    FROM stic_work_calendar
                  WHERE deleted = 0 
                    AND type != 'canceled'
                    AND start_date BETWEEN DATE_SUB(UTC_TIMESTAMP(), INTERVAL 1 DAY)  AND UTC_TIMESTAMP()
                    AND assigned_user_id = '" . $userId . "';";

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
        $result = $db->query($query);
        $data = $db->fetchByAssoc($result);
        return $data['count'] > 0;
    }
}