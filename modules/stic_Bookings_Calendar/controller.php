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
//prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class stic_Bookings_CalendarController extends SugarController
{
    /**
     * This action is called by the FullCalendar and retrieves a collection of 
     * booked or available resources (depending on user's settings) in a certain time range.
     *
     * @return void
     */
    public function action_getResources()
    {
        global $current_user, $timedate;

        if (!isset($_GET['start']) || !isset($_GET['end'])) {
            die("Please provide a date range.");
        }
        require_once 'modules/stic_Bookings_Calendar/utils/CalendarObject.php';

        // Parse the timeZone parameter if it is present.
        $userTimeZone = $current_user->getPreference('timezone');
        if (empty($userTimeZone)) {
            if (isset($_GET['timeZone'])) {
                $userTimeZone = new DateTimeZone($_GET['timeZone']);
            } else {
                $tz = substr($_GET['start'], -6, 6);
                $userTimeZone = new DateTimeZone($tz);
            }
        } else {
            $userTimeZone = new DateTimeZone($userTimeZone);
        }

        // Parse the dates that arrive from the FullCalendar
        $startDate = new DateTime($_GET['start']);
        $startDate = $timedate->to_display_date_time(date_format($startDate, 'Y-m-d H:i:s'), false, false, $current_user);
        $startDate = $timedate->fromUser($startDate, $current_user);
        $startDate = $startDate->asDb();

        $endDate = new DateTime($_GET['end']);
        $endDate = $timedate->to_display_date_time(date_format($endDate, 'Y-m-d H:i:s'), false, false, $current_user);
        $endDate = $timedate->fromUser($endDate, $current_user);
        $endDate = $endDate->asDb();

        $range_start = parseDateTime($startDate);
        $range_end = parseDateTime($endDate);

        // Get configuration params from the user configuration
        $userBean = new UserPreference($current_user);
        
        // stic_bookings_calendar_availability_mode: sets if the user will see resources availability or existing bookings
        if (!$availabilityMode = $_REQUEST['availabilityMode']) {
            $availabilityMode = $userBean->getPreference('stic_bookings_calendar_availability_mode');
        }

        // stic_bookings_calendar_filtered_resources: the default list of resources the user wants to see
        if (!isset($_REQUEST['filteredResources']) || empty($_REQUEST['filteredResources'])) {
            $filteredResources = $userBean->getPreference('stic_bookings_calendar_filtered_resources');
        } else {
            $filteredResources = explode(',', $_REQUEST['filteredResources']);
        }


        // Get the data to be displayed in the calendar according to stic_bookings_calendar_availability_mode param
        $calendarItems = array();
        if ($availabilityMode == "true") {
            $calendarItems = $this->getResourcesAvailability($startDate, $endDate, $filteredResources);
        } else {
            $calendarItems = $this->getBookedResources($startDate, $endDate, $filteredResources);
        }
        
        // Convert calendar items into calendar printable objects.
        $calendarObjects = array();
        foreach ($calendarItems as $calendarItem) {
            if (
                isset($_REQUEST['module']) && $_REQUEST['module'] === 'stic_Bookings_Calendar' &&
                isset($calendarItem['resourceType']) && $calendarItem['resourceType'] === 'place'
            ) {
                continue;
            }

            $calendarObject = new CalendarObject($calendarItem, $userTimeZone);
            // If the CalendarObject is in-bounds, add it to the output.
            // Probably we don't need this here because we are filtering the bookings before, but we leave it just in case.
            if ($calendarObject->isWithinDayRange($range_start, $range_end)) {
                $calendarObjects[] = $calendarObject->toArray();
            }
        }
        echo json_encode($calendarObjects);

        // Stop the code here to avoid sending unnecessary headers to the request
        die();
    }

    /**
     * Action used to save the user preferences set by the user related to the Bookings Calendar module
     *
     * @return void
     */
    public function action_saveUserPreferences()
    {
        global $current_user;
        if (!$userPreference = $_POST['user_preference']) {
            echo false;
            die();
        }
        if (!isset($_POST['preference_value'])) {
            $_POST['preference_value'] = '';
        }
        $preferenceValue = $_POST['preference_value'];

        require_once 'modules/UserPreferences/UserPreference.php';
        $userBean = new UserPreference($current_user);
        $userBean->setPreference($userPreference, $preferenceValue);
        echo true;
        die();
    }

    /**
     * Returns the booked resources and their details
     *
     * @param String $start_date
     * @param String $end_date
     * @param Array $filteredResources
     * @return array
     */
    protected function getBookedResources($start_date, $end_date, $filteredResources)
    {
        global $current_user;
        $resourcesBean = BeanFactory::getBean('stic_Resources');
        $resources = $resourcesBean->get_full_list('name');
        $bookedResources = array();
        $query = "stic_bookings.end_date >= '$start_date' 
                AND stic_bookings.start_date <= '$end_date' 
                AND stic_bookings.place_booking = 0 
                AND stic_bookings.status != 'cancelled'";
      foreach ($resources as $resource) {
            if (!$filteredResources || in_array($resource->id, $filteredResources)) {
                $relBeans = $resource->get_linked_beans(
                    'stic_resources_stic_bookings',
                    '',
                    '',
                    0,
                    0,
                    0,
                    $query,
                );
                foreach ($relBeans as $relBean) {
                    $status = $relBean->status;
                    $bookedResources[] = array(
                        'title' => $resource->name . ' - ' . str_pad($relBean->code, 5, "0", STR_PAD_LEFT),
                        'resourceName' => $resource->name,
                        'module' => $relBean->module_name,
                        'id' => $relBean->id,
                        'recordId' => $relBean->id,
                        'resourceId' => $resource->id,
                        'resourceType'=> $resource->type,
                        // 'backgroundColor' => $resource->color,
                        // 'borderColor' => $resource->color,
                        'allDay' => $relBean->all_day,
                        'start' => $relBean->fetched_row['start_date'],
                        'end' => $relBean->fetched_row['end_date'],
                        // Classname is defined this way in order to paint each calendar object using the resource's color
                        'className' => 'id-' . $resource->id,
                    );
                }
            }
        }
        return $bookedResources;
    }

    /**
     *  Returns the availability of the existing resources.
     *
     * @param String $start_date
     * @param String $end_date
     * @param Array $filteredResources
     * @return array()
     */
    protected function getResourcesAvailability($start_date, $end_date, $filteredResources)
    {
        $resourcesAvailability = array();
        if (empty($filteredResources) || !is_array($filteredResources)) {
            $filteredResources = [];
        }
        foreach ($filteredResources as $resourceId) {
            $resourceBean = BeanFactory::getBean('stic_Resources', $resourceId);

            $query =
                "SELECT DISTINCT
                date_availability,
                GROUP_CONCAT(DISTINCT availability ORDER BY availability) as superavail
            FROM
            (SELECT
                date_availability,
                if(date_availability BETWEEN start_date AND end_date - INTERVAL 15 MINUTE, 1, 0) as availability
            FROM
            (SELECT
                date('$start_date') + interval (seq * 15) Minute as date_availability,
                booked.start_date,
                booked.end_date
            FROM
                seq_0_to_12000
            LEFT JOIN
                (
                select
                    name,
                    start_date,
                    end_date
                from
                    stic_bookings sb
                join stic_resources_stic_bookings_c srsbc on
                    srsbc.stic_resources_stic_bookingsstic_bookings_idb = sb.id
                WHERE
                    srsbc.stic_resources_stic_bookingsstic_resources_ida = '$resourceBean->id'
                    AND srsbc.deleted = 0
                    AND sb.deleted = 0
                    AND sb.end_date >= '$start_date' 
                    AND sb.start_date <= '$end_date'
                    AND sb.status != 'cancelled') booked
            ON
                1 = 1) main) supermain
            GROUP BY date_availability
            HAVING superavail = '0';";
            $db = DBManagerFactory::getInstance();
            $res = $db->query($query);
            $row = $db->fetchByAssoc($res);
            if (!empty($row)) {
                $startDate = $row['date_availability'];
                $lastDate = $row['date_availability'];
                
                while ($row = $db->fetchByAssoc($res)) {
                    if (strtotime($row['date_availability']) - strtotime($lastDate) > 15 * 60) {
                        // 15 mins has passed
                        $resourcesAvailability[] = array(
                            'title' => $resourceBean->name,
                            'resourceName' => $resourceBean->name,
                            'module' => $resourceBean->module_name,
                            'recordId' => $resourceBean->id,
                            'resourceId' => $resourceBean->id,
                            'start' => $startDate,
                            'end' => $lastDate,
                            // Classname is defined this way in order to paint each calendar object using the resource's color
                            'className' => 'id-' . $resourceBean->id,
                        );
                        $startDate = $row['date_availability'];
                        $lastDate = $row['date_availability'];
                    } else {
                        $lastDate = $row['date_availability'];
                    }
                }                
                if ($startDate != $lastDate) {
                    $resourcesAvailability[] = array(
                        'title' => $resourceBean->name,
                        'resourceName' => $resourceBean->name,
                        'module' => $resourceBean->module_name,
                        'recordId' => $resourceBean->id,
                        'resourceId' => $resourceBean->id,
                        'start' => $startDate,
                        'end' => $lastDate,
                        // Classname is defined this way in order to paint each calendar object using the resource's color
                        'className' => 'id-' . $resourceBean->id,
                    );
                }
            }
            
        }
        return $resourcesAvailability;
    }
    
    /**
     * Validates if a date string is in the expected format
     *
     * @param string $date Date string to validate
     * @param string $format Format to validate against
     * @return bool
     */
    protected function validateDate($date, $format = 'Y-m-d')
    {
        $d = DateTime::createFromFormat($format, $date);
        return $d && $d->format($format) === $date;
    }

    /**
     * Returns an array of dates between start and end dates
     *
     * @param string $startDate Start date (YYYY-MM-DD)
     * @param string $endDate End date (YYYY-MM-DD)
     * @return array
     */
    protected function getDatesArray($startDate, $endDate)
    {
        $dates = array();
        $current = strtotime($startDate);
        $end = strtotime($endDate);

        while ($current <= $end) {
            $dates[] = date('Y-m-d', $current);
            $current = strtotime('+1 day', $current);
        }

        return $dates;
    }
}