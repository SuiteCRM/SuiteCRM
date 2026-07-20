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
class stic_BookingsUtils
{
    /**
     * Prepares periodic booking data for a confirmation step, storing it in the session without saving records.
     * This function replaces the initial record creation logic.
     *
     * @return void
     */
    public static function preparePeriodicBookingsForConfirmation()
    {
        require_once 'modules/stic_Bookings/controller.php';
        global $sugar_config, $app_list_strings, $current_user, $timedate, $db;

        $requestData = $_REQUEST;
        $repeat_type = $requestData['repeat_type'] ?? null;
        $isAllDay = !empty($requestData['all_day']) && $requestData['all_day'] == 1;

        $startDay = self::formatDate($requestData['start_date'] ?? null, $isAllDay, true);
        $finalDay = self::formatDate($requestData['end_date'] ?? null, $isAllDay, false);
        
        if (!$startDay || !$finalDay) {
            $GLOBALS['log']->error("Error: Unable to format start or end dates");
            return;
        }
        
        $duration = strtotime($finalDay) - strtotime($startDay);
        $dates = self::generateBookingDates($requestData, $startDay);
        
        if (empty($dates)) {
            $GLOBALS['log']->error("Error: No dates generated for booking");
            return;
        }
        
        $aux = self::convertDatesToDbFormat($dates, $timedate, $current_user);
    
        $firstBookingCode = self::getLatestBookingCode($db, $repeat_type);
        $bookingAttempts = self::processBookingAttempts($dates, $duration, $requestData, $aux, $firstBookingCode, $timedate, $current_user, $db);

        $_SESSION['bookings_to_confirm'] = $bookingAttempts['bookingsToConfirm'];
        $_SESSION['consolidated_summary'] = array_values($bookingAttempts['allBookingAttempts']);
        $_SESSION['summary'] = $bookingAttempts['summary'];
    }

    /**
     * Formats the start and end dates from the request.
     * @param array $requestData The $_REQUEST array.
     * @param bool $isStart Flag to indicate if it's the start date.
     * @return string Formatted date string.
     */
    private static function formatDate($date, $isAllDay = false, $isStart = true)
    {
        if (empty($date)) {
            return null;
        }
        
        // Replace slashes with dashes for consistent parsing
        $date = str_replace('/', '-', $date);
        
        // Validate the date format
        $timestamp = strtotime($date);
        if ($timestamp === false) {
            $GLOBALS['log']->error("Invalid date format: $date");
            return null;
        }
    
        if ($isAllDay) {
            if($isStart) {
                return date('Y-m-d 00:00:00', $timestamp);
            } else {
                return date('Y-m-d 00:00:00', strtotime('+1 day', $timestamp));
            }
        } else {
            return date('Y-m-d H:i:s', $timestamp);
        }
    }
        
    /**
     * Generates an array of booking dates based on the repeat type.
     * @param array $requestData The $_REQUEST array.
     * @param string $startDay The start date of the first booking.
     * @return array An array of booking date strings.
     */
    private static function generateBookingDates($requestData, $startDay)
    {
        $repeat_type = $requestData['repeat_type'] ?? null;
        $interval = $requestData['repeat_interval'] ?? '1';
        $count = $requestData['repeat_count'] ?? null;
        $until = $requestData['repeat_until'] ?? null;

        if ($until) {
            $until = date('Y-m-d H:i:s', strtotime(str_replace('/', '-', $until)));
        }

        switch ($repeat_type) {
            case 'Daily':
                return self::generateDailyDates($startDay, $interval, $count, $until);
            case 'Weekly':
                return self::generateWeeklyDates($startDay, $interval, $count, $until, $requestData);
            case 'Monthly':
                return self::generateMonthlyDates($startDay, $interval, $count, $until);
            case 'Yearly':
                return self::generateYearlyDates($startDay, $interval, $count, $until);
            default:
                return [];
        }
    }

    /**
     * Generates daily booking dates.
     */
    private static function generateDailyDates($startDay, $interval, $count, $until)
    {
        $dates = [];
        $currentDay = $startDay;
        $i = 0;
        if ($count != '' && $count != '0') {
            while ($i < $count) {
                $dates[] = $currentDay;
                $currentDay = date('Y-m-d H:i:s', strtotime($currentDay . " + $interval days"));
                $i++;
            }
        } else if ($until != '') {
            while (strtotime(date("Y-m-d", strtotime($currentDay))) <= strtotime($until)) {
                $dates[] = $currentDay;
                $currentDay = date('Y-m-d H:i:s', strtotime($currentDay . " + $interval days"));
            }
        }
        return $dates;
    }

    /**
     * Generates weekly booking dates.
     */
    private static function generateWeeklyDates($startDay, $interval, $count, $until, $requestData)
    {
        $dates = [];
        $dow = self::getDaysOfWeekFromRequest($requestData);
        $times = array_sum($dow);

        if ($times === 0) {
            // No specific days selected, repeat weekly
            $currentWeek = $startDay;
            $i = 0;
            if ($count != '' && $count != '0') {
                while ($i < $count) {
                    $dates[] = $currentWeek;
                    $currentWeek = date('Y-m-d H:i:s', strtotime($currentWeek . " + $interval weeks"));
                    $i++;
                }
            } else if ($until != '') {
                while (strtotime(date("Y-m-d", strtotime($currentWeek))) <= strtotime($until)) {
                    $dates[] = $currentWeek;
                    $currentWeek = date('Y-m-d H:i:s', strtotime($currentWeek . " + $interval weeks"));
                }
            }
            return $dates;
        }

        $currentDate = $startDay;
        $i = 0;

        while (true) {
            if (($count != '' && $count != '0' && $i >= $count) || ($until != '' && strtotime($currentDate) > strtotime($until))) {
                break;
            }

            for ($day = 1; $day <= 7; $day++) {
                if ($dow[$day] == 1) {
                    $dayOfWeek = date('N', strtotime($currentDate));
                    $diff = $day - $dayOfWeek;
                    if ($diff < 0) {
                        $diff += 7;
                    }

                    $newDate = date('Y-m-d H:i:s', strtotime($currentDate . " + $diff days"));

                    if (($count != '' && $count != '0')) {
                        if ($i < $count) {
                            $dates[] = $newDate;
                            $i++;
                        }
                    } else if (($until != '')) {
                        if (strtotime(date("Y-m-d", strtotime($newDate))) <= strtotime($until)) {
                            $dates[] = $newDate;
                        }
                    }
                }
            }
            $currentDate = date('Y-m-d H:i:s', strtotime($currentDate . " + $interval weeks"));
        }
        return $dates;
    }

    /**
     * Helper function to get the days of the week from the request.
     */
    private static function getDaysOfWeekFromRequest($requestData)
    {
        $dow = [];
        for ($i = 1; $i <= 6; $i++) {
            $dow[$i] = ($requestData['repeat_dow_' . $i] ?? '') == 'on' ? 1 : 0;
        }
        $dow[7] = ($requestData['repeat_dow_0'] ?? '') == 'on' ? 1 : 0; // Sunday
        return $dow;
    }

    /**
     * Generates monthly booking dates.
     */
    private static function generateMonthlyDates($startDay, $interval, $count, $until)
    {
        $dates = [];
        $currentMonth = $startDay;
        $i = 0;
        if ($count != '' && $count != '0') {
            while ($i < $count) {
                $dates[] = $currentMonth;
                $currentMonth = date('Y-m-d H:i:s', strtotime($currentMonth . " + $interval months"));
                $i++;
            }
        } else if ($until != '') {
            while (strtotime(date("Y-m-d", strtotime($currentMonth))) <= strtotime($until)) {
                $dates[] = $currentMonth;
                $currentMonth = date('Y-m-d H:i:s', strtotime($currentMonth . " + $interval months"));
            }
        }
        return $dates;
    }

    /**
     * Generates yearly booking dates.
     */
    private static function generateYearlyDates($startDay, $interval, $count, $until)
    {
        $dates = [];
        $currentYear = $startDay;
        $i = 0;
        if ($count != '' && $count != '0') {
            while ($i < $count) {
                $dates[] = $currentYear;
                $currentYear = date('Y-m-d H:i:s', strtotime($currentYear . " + $interval years"));
                $i++;
            }
        } else if ($until != '') {
            while (strtotime(date("Y-m-d", strtotime($currentYear))) <= strtotime($until)) {
                $dates[] = $currentYear;
                $currentYear = date('Y-m-d H:i:s', strtotime($currentYear . " + $interval years"));
            }
        }
        return $dates;
    }

    /**
     * Converts a list of dates to the database format.
     */
    private static function convertDatesToDbFormat($dates, $timedate, $currentUser)
    {
        if (!is_array($dates) || empty($dates)) {
            $GLOBALS['log']->error("convertDatesToDbFormat: dates parameter is not a valid array");
            return [];
        }
        
        $aux = [];
        foreach ($dates as $date) {
            if (empty($date)) {
                $GLOBALS['log']->warn("Skipping empty date in convertDatesToDbFormat");
                continue;
            }
            
            $convertedDate = $timedate->to_db($timedate->to_display_date_time($date, true, false, $currentUser));
            if ($convertedDate) {
                $aux[] = $convertedDate;
            }
        }
        return $aux;
    }

    /**
     * Gets the latest booking code from the database.
     */
    private static function getLatestBookingCode($db, $repeatType)
    {
        if (!$repeatType) {
            return null;
        }
        $query = "SELECT code FROM stic_bookings ORDER BY code DESC LIMIT 1";
        $result = $db->query($query, true);
        $row = $db->fetchByAssoc($result);
        $lastNum = $row['code'] ?? 0;
        return str_pad($lastNum + 1, 5, "0", STR_PAD_LEFT);
    }
    
    /**
     * Processes each booking attempt to check resource availability and prepare summary data.
     */
    private static function processBookingAttempts($dates, $duration, $requestData, $aux, $firstBookingCode, $timedate, $currentUser, $db)
    {
        $controller = new stic_BookingsController();
        $resourceIds = $requestData['resource_id'] ?? [];
        $resourceNames = self::getResourceNames($resourceIds, 'stic_Resources');

        $summary = [
            'global' => ['totalRecordsProcessed' => 0, 'totalRecordsCreated' => 0, 'totalRecordsNotCreated' => 0],
            'resources' => self::initializeResourceSummary($resourceIds, $resourceNames),
        ];
        
        $allBookingAttempts = [];
        $bookingsToConfirm = [];
        $availableResourcesInThisBatch = [];
        $code = self::getLatestBookingCodeForName($db);
        $bookingCounter = 0;

        foreach ($dates as $i => $date) {
            $summary['global']['totalRecordsProcessed']++;
            $currentStartDatePhp = $date;
            $currentEndDatePhp = date('Y-m-d H:i:s', strtotime($currentStartDatePhp) + $duration);
            $availability = self::checkResourcesAvailability($controller, $resourceIds, $currentStartDatePhp, $currentEndDatePhp, $requestData['record'] ?? '0', $availableResourcesInThisBatch);

            if ($availability['allResourcesAvailable']) {
                self::updateBatchAvailability($availableResourcesInThisBatch, $resourceIds, $currentStartDatePhp, $currentEndDatePhp);
                $bookingCounter++;
                $code++;
                $bookingName = self::generateBookingName($requestData['name'] ?? '', $firstBookingCode, $bookingCounter);
                $bookingRecord = self::createBookingRecord($aux[$i], $duration, $requestData, $resourceIds, $resourceNames, $availability['allResourcesAvailable'], $bookingName, $firstBookingCode, $timedate, $currentUser);
                $bookingsToConfirm[] = $bookingRecord;
                self::updateSummaryForCreatedBooking($summary, $resourceIds, $bookingName, $resourceNames, $aux[$i], $duration, $timedate, $currentUser);
                self::updateAllBookingAttempts($allBookingAttempts, $aux[$i], $duration, $bookingName, $resourceNames, $timedate, $currentUser, 'created');
            } else {
                self::updateSummaryForUnavailableBooking($summary, $resourceIds, $resourceNames, $availability['resourceAvailability'], $aux[$i], $duration, $timedate, $currentUser);
                self::updateAllBookingAttempts($allBookingAttempts, $aux[$i], $duration, '', $resourceNames, $timedate, $currentUser, 'not_created', $availability['resourceAvailability']);
            }
        }
        
        return [
            'bookingsToConfirm' => $bookingsToConfirm,
            'allBookingAttempts' => $allBookingAttempts,
            'summary' => $summary
        ];
    }
    
    /**
     * Gets resource names from their IDs.
     */
    private static function getResourceNames($resourceIds, $module)
    {
        $names = [];
        foreach ($resourceIds as $resourceId) {
            $resource = BeanFactory::getBean($module, $resourceId);
            if ($resource) {
                $names[$resourceId] = $resource->name;
            }
        }
        return $names;
    }

    /**
     * Initializes the resource summary array.
     */
    private static function initializeResourceSummary($resourceIds, $resourceNames)
    {
        $resources = [];
        foreach ($resourceIds as $resourceId) {
            $resources[$resourceId] = [
                'name' => $resourceNames[$resourceId] ?? "Recurso {$resourceId}",
                'numRecordsProcessed' => 0,
                'numRecordsCreated' => 0,
                'numRecordsNotCreated' => 0,
                'recordsCreated' => [],
                'recordsNotCreated' => [],
            ];
        }
        return $resources;
    }

    /**
     * Checks availability of resources for a specific time slot.
     */
    private static function checkResourcesAvailability($controller, $resourceIds, $startDate, $endDate, $bookingId, &$availableResourcesInThisBatch)
    {
        $resourceAvailability = [];
        $allResourcesAvailable = true;

        foreach ($resourceIds as $resourceId) {
            $availabilityDb = self::checkResourceAvailability($resourceId, $startDate, $endDate, $bookingId);
            $availabilityBatch = self::checkBatchAvailability($availableResourcesInThisBatch, $resourceId, $startDate, $endDate);
            
            $isResourceAvailable = $availabilityDb['resources_allowed'] && $availabilityBatch;
            $resourceAvailability[$resourceId] = $isResourceAvailable;

            if (!$isResourceAvailable) {
                $allResourcesAvailable = false;
                break;
            }
        }
        return ['allResourcesAvailable' => $allResourcesAvailable, 'resourceAvailability' => $resourceAvailability];
    }

    /**
     * Checks if a resource is available in the current batch of bookings.
     */
    private static function checkBatchAvailability($availableResourcesInThisBatch, $resourceId, $startDate, $endDate)
    {
        if (!isset($availableResourcesInThisBatch[$resourceId])) {
            return true;
        }

        foreach ($availableResourcesInThisBatch[$resourceId] as $bookingTime) {
            $start = $bookingTime['start'];
            $end = $bookingTime['end'];
            if (strtotime($startDate) < strtotime($end) && strtotime($endDate) > strtotime($start)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Updates the batch availability data.
     */
    private static function updateBatchAvailability(&$availableResourcesInThisBatch, $resourceIds, $startDate, $endDate)
    {
        foreach ($resourceIds as $resourceId) {
            if (!isset($availableResourcesInThisBatch[$resourceId])) {
                $availableResourcesInThisBatch[$resourceId] = [];
            }
            $availableResourcesInThisBatch[$resourceId][] = ['start' => $startDate, 'end' => $endDate];
        }
    }

    /**
     * Generates the booking name.
     */
    private static function generateBookingName($name, $code, $counter)
    {
        if (empty($name)) {
            $mod_strings = return_module_language($GLOBALS['current_language'], 'stic_Bookings');
            return $mod_strings['LBL_MODULE_NAME_SINGULAR'] . ' ' . str_pad($code, 5, "0", STR_PAD_LEFT) . ' (' . $counter . ')';
        }
        return $name . ' (' . $counter . ')';
    }

    /**
     * Gets the latest booking code for naming.
     */
    private static function getLatestBookingCodeForName($db)
    {
        $query = "SELECT code FROM stic_bookings ORDER BY code DESC LIMIT 1";
        $result = $db->query($query, true);
        $row = $db->fetchByAssoc($result);
        return $row['code'] ?? 0;
    }

    /**
     * Creates a booking record array for the session.
     */
    private static function createBookingRecord($startDateDb, $duration, $requestData, $resourceIds, $resourceNames, $allResourcesAvailable, $bookingName, $firstBookingCode, $timedate, $currentUser)
    {
        if (empty($startDateDb)) {
            $GLOBALS['log']->error("createBookingRecord: startDateDb is empty");
            return null;
        }
        
        $endDateDb = date('Y-m-d H:i:s', strtotime($startDateDb) + $duration);
        
        $startDateObj = $timedate->fromDbFormat($startDateDb, TimeDate::DB_DATETIME_FORMAT);
        $endDateObj = $timedate->fromDbFormat($endDateDb, TimeDate::DB_DATETIME_FORMAT);
        
        if (!$startDateObj || !$endDateObj) {
            $GLOBALS['log']->error("Error converting dates to DateTime objects");
            return null;
        }
    
        return [
            'start_date_db' => $startDateDb,
            'end_date_db' => $endDateDb,
            'start_date_display' => $timedate->asUser($startDateObj, $currentUser),
            'end_date_display' => $timedate->asUser($endDateObj, $currentUser),
            'all_day' => $requestData['all_day'] ?? '0',
            'status' => $requestData['status'] ?? null,
            'periodic_booking' => $requestData['periodic_booking'] ?? null,
            'repeat_type' => $requestData['repeat_type'] ?? null,
            'place_booking' => $requestData['place_booking'] ?? '0',
            'description' => $requestData['description'] ?? '',
            'parent_id' => $requestData['parent_id'] ?? '',
            'parent_type' => $requestData['parent_type'] ?? '',
            'resourceIds' => $resourceIds,
            'allResourcesAvailable' => $allResourcesAvailable,
            'name' => $bookingName,
            'periodic_code' => $firstBookingCode,
            'resource_names' => array_values($resourceNames)
        ];
    }

    /**
     * Updates the summary for a successfully created booking.
     */
    private static function updateSummaryForCreatedBooking(&$summary, $resourceIds, $bookingName, $resourceNames, $startDateDb, $duration, $timedate, $currentUser)
    {
        if (empty($startDateDb)) {
            return;
        }
        
        $endDateDb = date('Y-m-d H:i:s', strtotime($startDateDb) + $duration);
        $startDateObj = $timedate->fromDbFormat($startDateDb, TimeDate::DB_DATETIME_FORMAT);
        $endDateObj = $timedate->fromDbFormat($endDateDb, TimeDate::DB_DATETIME_FORMAT);
        
        if (!$startDateObj || !$endDateObj) {
            $GLOBALS['log']->error("Error converting dates in updateSummaryForCreatedBooking");
            return;
        }
        
        $startDateDisplay = $timedate->asUser($startDateObj, $currentUser);
        $endDateDisplay = $timedate->asUser($endDateObj, $currentUser);

        $summary['global']['totalRecordsCreated']++;
        foreach ($resourceIds as $resourceId) {
            $summary['resources'][$resourceId]['numRecordsCreated']++;
            $summary['resources'][$resourceId]['recordsCreated'][] = [
                'bookingName' => $bookingName,
                'resourceName' => $resourceNames[$resourceId],
                'startDate' => $startDateDisplay,
                'endDate' => $endDateDisplay,
                'status' => $_REQUEST['status'] ?? null,
                'allRequestedResources' => array_values($resourceNames),
            ];
        }
    }

    /**
     * Updates the summary for a booking that could not be created.
     */
    private static function updateSummaryForUnavailableBooking(&$summary, $resourceIds, $resourceNames, $resourceAvailability, $startDateDb, $duration, $timedate, $currentUser)
    {
        if (empty($startDateDb)) {
            return;
        }
        
        $endDateDb = date('Y-m-d H:i:s', strtotime($startDateDb) + $duration);
        $startDateObj = $timedate->fromDbFormat($startDateDb, TimeDate::DB_DATETIME_FORMAT);
        $endDateObj = $timedate->fromDbFormat($endDateDb, TimeDate::DB_DATETIME_FORMAT);
        
        if (!$startDateObj || !$endDateObj) {
            $GLOBALS['log']->error("Error converting dates in updateSummaryForUnavailableBooking");
            return;
        }
        
        $startDateDisplay = $timedate->asUser($startDateObj, $currentUser);
        $endDateDisplay = $timedate->asUser($endDateObj, $currentUser);

        $summary['global']['totalRecordsNotCreated']++;
        $unavailableResources = [];
        foreach ($resourceAvailability as $resId => $available) {
            if (!$available) {
                $unavailableResources[] = $resourceNames[$resId];
            }
        }
        foreach ($resourceIds as $resourceId) {
            if (!$resourceAvailability[$resourceId]) {
                $summary['resources'][$resourceId]['numRecordsNotCreated']++;
                $summary['resources'][$resourceId]['recordsNotCreated'][] = [
                    'resourceName' => $resourceNames[$resourceId],
                    'startDate' => $startDateDisplay,
                    'endDate' => $endDateDisplay,
                    'unavailableResources' => $unavailableResources,
                    'allRequestedResources' => array_values($resourceNames),
                    'thisResourceAvailable' => $resourceAvailability[$resourceId],
                ];
            }
        }
    }

    /**
     * Updates the consolidated list of all booking attempts.
     */
    private static function updateAllBookingAttempts(&$allBookingAttempts, $startDateDb, $duration, $bookingName, $resourceNames, $timedate, $currentUser, $status, $resourceAvailability = [])
    {
        if (empty($startDateDb)) {
            return;
        }
        
        $endDateDb = date('Y-m-d H:i:s', strtotime($startDateDb) + $duration);
        $startDateObj = $timedate->fromDbFormat($startDateDb, TimeDate::DB_DATETIME_FORMAT);
        $endDateObj = $timedate->fromDbFormat($endDateDb, TimeDate::DB_DATETIME_FORMAT);
        
        if (!$startDateObj || !$endDateObj) {
            $GLOBALS['log']->error("Error converting dates in updateAllBookingAttempts");
            return;
        }
        
        $startDateDisplay = $timedate->asUser($startDateObj, $currentUser);
        $endDateDisplay = $timedate->asUser($endDateObj, $currentUser);
        $recordKey = $startDateDisplay . '_' . $endDateDisplay;

        $attempt = [
            'status' => $status,
            'startDate' => $startDateDisplay,
            'endDate' => $endDateDisplay,
            'allRequestedResources' => array_values($resourceNames),
        ];

        if ($status === 'created') {
            $attempt['bookingName'] = $bookingName;
        } else {
            $unavailableResources = [];
            foreach ($resourceAvailability as $resId => $available) {
                if (!$available) {
                    $unavailableResources[] = $resourceNames[$resId];
                }
            }
            $attempt['unavailableResources'] = $unavailableResources;
        }

        $allBookingAttempts[$recordKey] = $attempt;
    }

    /**
     * Creates periodic booking records from the data stored in the session after confirmation.
     *
     * @return void
     */
    public static function savePeriodicBookingsFromConfirmation()
    {
        global $sugar_config, $current_user, $timedate;
        $aodConfig = $sugar_config['aod']['enable_aod'] ?? false;
        $sugar_config['aod']['enable_aod'] = false;

        $bookings_to_create = $_SESSION['bookings_to_confirm'] ?? [];
        if (empty($bookings_to_create)) {
            return;
        }

        foreach ($bookings_to_create as $booking_info) {
            // Only save the records that were marked as available
            if ($booking_info['allResourcesAvailable']) {
                $bookingBean = BeanFactory::newBean('stic_Bookings');

                $bookingBean->name = $booking_info['name'];
                $bookingBean->start_date = $booking_info['start_date_db'];
                $bookingBean->end_date = $booking_info['end_date_db'];
                $bookingBean->all_day = $booking_info['all_day'];
                $bookingBean->status = $booking_info['status'];
                $bookingBean->periodic_booking = $booking_info['periodic_booking'];
                $bookingBean->repeat_type = $booking_info['repeat_type'];
                $bookingBean->place_booking = $booking_info['place_booking'];
                $bookingBean->description = $booking_info['description'];
                $bookingBean->periodic_code = $booking_info['periodic_code'];

                if (!empty($booking_info['parent_id']) && !empty($booking_info['parent_type'])) {
                    $bookingBean->parent_id = $booking_info['parent_id'];
                    $bookingBean->parent_type = $booking_info['parent_type'];
                }

                $bookingBean->assigned_user_id = $current_user->id;
                $bookingBean->save();
                
                if ($bookingBean->load_relationship('stic_resources_stic_bookings')) {
                    foreach ($booking_info['resourceIds'] as $resourceId) {
                        $bookingBean->stic_resources_stic_bookings->add($resourceId);
                    }
                }
            }
        }

        $sugar_config['aod']['enable_aod'] = $aodConfig;
        
        // After saving, clean up the session data.
        unset($_SESSION['bookings_to_confirm']);
    }

    /**
     * This is the original function. It now acts as a redirector to the new confirmation flow.
     */
    public static function createPeriodicBookingsRecords()
    {
        global $sugar_config;
        $aodConfig = $sugar_config['aod']['enable_aod'] ?? false;
        $sugar_config['aod']['enable_aod'] = false;
        $startTime = microtime(true);
        
        // Call the new function to prepare data for confirmation
        self::preparePeriodicBookingsForConfirmation();

        $endTime = microtime(true);
        $totalTime = $endTime - $startTime;
        $GLOBALS['log']->debug(__METHOD__ . '(' . __LINE__ . ") >> Has prepared the booking records in $totalTime seconds, waiting for confirmation");

        $sugar_config['aod']['enable_aod'] = $aodConfig;
        
        header("Location: index.php?module=stic_Bookings&action=bookingsAssistantSummary");
        exit();
    }


    public static function checkResourceAvailability($resourceId, $startDate, $endDate, $bookingId)
    {
        global $current_user;

        $resourcesIds = array();
        if ($resourceId) {
            // If a single resource id is provided, will only check that resource
            $resourcesIds[] = $resourceId;
        } else if ($bookingId) {
            // If a single resource id is not provided, will check all resources attached to the booking
            $booking = BeanFactory::getBean('stic_Bookings', $bookingId);
            if ($booking && $booking->load_relationship('stic_resources_stic_bookings')) {
                foreach ($booking->stic_resources_stic_bookings->getBeans() as $resourceBean) {
                        $resourcesIds[] = $resourceBean->id;
                }
            }
        }
        if (empty($resourcesIds)) {
            return array('success' => true, 'resources_allowed' => true);
        }
    
        $db = DBManagerFactory::getInstance();
        $tzone = $current_user->getPreference('timezone');
        $dateTimeZone = new DateTimeZone($tzone);

        $timeZoneOffsetHourStartDate = $startDate ? $dateTimeZone->getOffset(new DateTime($startDate)) / 3600 : 0;
        $timeZoneOffsetHourEndDate = $endDate ? $dateTimeZone->getOffset(new DateTime($endDate)) / 3600 : 0;

        // Check if there are other bookings in the period that include the required resource(s)
        foreach ($resourcesIds as $resourceId) {
            $query = "SELECT
                COUNT(stic_bookings.id) AS bookingsCount
                FROM stic_bookings
                JOIN stic_resources_stic_bookings_c
                    ON stic_resources_stic_bookings_c.stic_resources_stic_bookingsstic_bookings_idb=stic_bookings.id
                WHERE stic_resources_stic_bookings_c.deleted=0
                    AND stic_bookings.deleted=0
                    AND stic_resources_stic_bookings_c.stic_resources_stic_bookingsstic_resources_ida='" . $resourceId . "'
                    AND stic_bookings.id != '" . $bookingId . "'
                    AND stic_bookings.status != 'cancelled'";

            if ($startDate && $endDate) {
                $query .= " AND TIMESTAMPDIFF(SECOND, DATE_ADD(stic_bookings.start_date, INTERVAL " . $timeZoneOffsetHourStartDate . " HOUR),'" . $endDate . "') > 0
                            AND TIMESTAMPDIFF(SECOND, '" . $startDate . "', DATE_ADD(stic_bookings.end_date, INTERVAL " . $timeZoneOffsetHourEndDate . " HOUR)) > 0 ";
            }

            if ($res = $db->query($query)) {
                $row = $db->fetchByAssoc($res);
                if ($row['bookingsCount'] > 0) {

                    return array('success' => true, 'resources_allowed' => false);
                }
            } else {
                return array('success' => false, 'resources_allowed' => $res);
            }
        }

        return array('success' => true, 'resources_allowed' => true);
    }
    public static function createFilterCondition($filterValue, $columnName, $db) {
        if (empty($filterValue)) {
            return '';
        }
        
        $filters = [];
        $hasNonEmpty = false;
        
        $filterArray = is_array($filterValue) ? $filterValue : [$filterValue];
        
        foreach ($filterArray as $value) {
            if (trim($value) === '') {
                $filters[] = "$columnName = ''";
            } else {
                $valueSafe = $db->quote($value);
                $filters[] = "$columnName = ".$db->quoted($valueSafe)."";
                $hasNonEmpty = true;
            }
        }
        
        if (empty($filters)) {
            return '';
        }
        
        if ($hasNonEmpty && !in_array("$columnName = ''", $filters)) {
            $filters[] = "$columnName = ''";
        }
        
        return " AND (" . implode(" OR ", $filters) . ")";
    }

    public static function getConfigResourceFields()
    {
        $mod_strings = return_module_language($GLOBALS['current_language'], 'stic_Resources');
        return [
            'name' => $mod_strings['LBL_NAME'],
            'code' => $mod_strings['LBL_CODE'],
            'color' => $mod_strings['LBL_COLOR'],
            'type' => $mod_strings['LBL_TYPE'],
            'hourly_rate' => $mod_strings['LBL_HOURLY_RATE'],
            'daily_rate' => $mod_strings['LBL_DAILY_RATE'],
        ];
    }

    public static function getConfigPlaceFields()
    {
        $mod_strings = return_module_language($GLOBALS['current_language'], 'stic_Resources');
        return [
            'name' => $mod_strings['LBL_NAME'],
            'code' => $mod_strings['LBL_CODE'],
            'user_type' => $mod_strings['LBL_USER_TYPE'],
            'place_type' => $mod_strings['LBL_PLACE_TYPE'],
            'gender' => $mod_strings['LBL_GENDER'],
            'amount_day_occupied' => $mod_strings['LBL_AMOUNT_DAY_OCCUPIED'],
            'amount_copayment' => $mod_strings['LBL_AMOUNT_COPAYMENT'],
        ];
    }
}