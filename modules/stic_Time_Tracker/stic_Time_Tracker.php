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
class stic_Time_Tracker extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Time_Tracker';
    public $object_name = 'stic_Time_Tracker';
    public $table_name = 'stic_time_tracker';
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
    public $start_date;
    public $end_date;
    public $duration;
    
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
        global $timedate, $current_user;

        // Calculate datetimes fields in user format and bbdd format
        if (!empty($this->start_date)){
            if (!$timedate->check_matching_format($this->start_date, TimeDate::DB_DATETIME_FORMAT)) { 
                $startDate = $this->start_date;
                $this->start_date = $timedate->fromUser($this->start_date, $current_user);
                $this->start_date = $timedate->asDb($this->start_date);

                if (!empty($this->end_date)) {
                    $endDate = $this->end_date;
                    $this->end_date = $timedate->fromUser($this->end_date, $current_user);
                    $this->end_date = $timedate->asDb($this->end_date);
                } else {
                    $endDate = '';
                }
            } else { 
                $startDate = $timedate->fromDbFormat($this->start_date, TimeDate::DB_DATETIME_FORMAT);
                $startDate = $timedate->asUser($startDate, $current_user);
                if (!empty($this->end_date)) {
                    $endDate = $timedate->fromDbFormat($this->end_date, TimeDate::DB_DATETIME_FORMAT);
                    $endDate = $timedate->asUser($endDate, $current_user);                
                } else {
                    $endDate = '';
                }
            }
        }
        // Set name
        $assignedUser = BeanFactory::getBean('Users', $this->assigned_user_id);
        if ($_REQUEST["action"] != "MassUpdate"){
            $this->name = $assignedUser->name . " - " . $startDate;
            if (!empty($endDate)) {
                $this->name .= " - " .  substr($endDate, -5);
            }
        } else {
            // In mass update we do not have access to the dates so the part of the name with the dates is reused
            $this->name = $assignedUser->name . " - " . substr($this->name, -25);
        }

        // Set duration
        if (!empty($this->end_date)) {
            $startTime = strtotime($this->start_date);
            $endTime = strtotime($this->end_date);
            $duration = $endTime - $startTime;       
            $this->duration = (float) number_format($duration / 3600, 2);
        } else {
            $this->duration = 0;
        }

        // Save the bean
        parent::save($check_notify);
    }
  
    /**
     * Return the last record of the indicated user that is within the previous 24 hours
     * @param userId User Identificator
     * @return Stic_time_tracker record or false in case it does not exist
     */
    public static function getLastTodayTimeTrackerRecord($userId)
    {
        global $db, $current_user;
        $tzone = $current_user->getPreference('timezone');

        $query = "SELECT * FROM stic_time_tracker
            WHERE deleted = 0 
            AND start_date IS NOT NULL AND start_date <> ''
            AND TIMESTAMPDIFF(SECOND, CONVERT_TZ(start_date, '+00:00', '" . $tzone . "'), NOW()) BETWEEN 0 AND 86400
            AND assigned_user_id = '" . $userId . "'  
            ORDER BY start_date desc
            LIMIT 1;";

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
        $result = $db->query($query);
        $data = $db->fetchByAssoc($result);
        return $data;
    }

    /**
     * Checks if the given user has a time tracker record between the previous 24 hours and now, in UTC.
     * @param userId User Identificator
     * @return boolean true if exist and false if not
     */
    public static function existAtLeastOneRecordFromYesterday($userId)
    {
        global $db;
        $query = "SELECT count(id) as count
                    FROM stic_time_tracker
                  WHERE deleted = 0 
                    AND start_date BETWEEN DATE_SUB(UTC_TIMESTAMP(), INTERVAL 1 DAY)  AND UTC_TIMESTAMP()
                    AND assigned_user_id = '" . $userId . "';";

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
        $result = $db->query($query);
        $data = $db->fetchByAssoc($result);
        return $data['count'] > 0;
    }
}