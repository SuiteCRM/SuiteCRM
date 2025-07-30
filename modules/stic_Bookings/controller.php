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
class stic_BookingsController extends SugarController
{
    /**
     * Action that validates if a booking has all its resources available in the booking dates.
     * It can also check only a specific resource, if the resource id is provided.
     *
     * Returns an object with two properties:
     * 1) success: True if the action completed successfully
     * 2) resources_allowed: True if all the given resources are available
     *
     * @return void
     */
    public function action_isResourceAvailable()
    {

        $startDate = $_REQUEST['startDate'];
        $endDate = $_REQUEST['endDate'];
        $bookingId = $_REQUEST['bookingId'] ?? null;
        $resourceRequestId = $_REQUEST['resourceId'];

        $result = $this->checkResourceAvailability($resourceRequestId, $startDate, $endDate, $bookingId);

        echo json_encode($result);
        return;
    }

    public function action_loadCenterResources()
    {
        $startDate = isset($_REQUEST['startDate']) ? $_REQUEST['startDate'] : '';
        $endDate = isset($_REQUEST['endDate']) ? $_REQUEST['endDate'] : '';
        $bookingId = isset($_REQUEST['bookingId']) ? $_REQUEST['bookingId'] : null;
        $centerIds = $_REQUEST['centerIds'];
        $resourcePlaceUserType = isset($_REQUEST['resourcePlaceUserType']) ? $_REQUEST['resourcePlaceUserType'] : '';
        $resourcePlaceType = isset($_REQUEST['resourcePlaceType']) ? $_REQUEST['resourcePlaceType'] : '';
        $resourceName = isset($_REQUEST['resourceName']) ? $_REQUEST['resourceName'] : '';
        $resourceGender = isset($_REQUEST['resourceGender']) ? $_REQUEST['resourceGender'] : '';
        $numberOfPlaces = isset($_REQUEST['numberOfPlaces']) ? $_REQUEST['numberOfPlaces'] : '';
        $existingResourceIds = isset($_REQUEST['existingResourceIds']) ? $_REQUEST['existingResourceIds'] : '';
        $existingResourceIdsArray = !empty($existingResourceIds) ? explode(',', $existingResourceIds) : [];

        if (empty($centerIds)) {
            echo json_encode(['success' => false]);
            return;
        }
        
        $config_place_fields = require 'modules/stic_Bookings/configPlaceFields.php';

        $db = DBManagerFactory::getInstance();
        $centerIdsArray = explode(',', $centerIds);
        $resources = [];


        $resourceGenderCondition = $this->createFilterCondition($resourceGender, 'gender', $db);
        $resourcePlaceTypeCondition = $this->createFilterCondition($resourcePlaceType, 'place_type', $db);
        $resourcePlaceUserTypeCondition = $this->createFilterCondition($resourcePlaceUserType, 'user_type', $db);

        
        $resourceNameCondition = '';
        if (!empty($resourceName)) {
            $resourceNameSafe = $db->quote($resourceName);
            $resourceNameCondition = " AND name LIKE '%$resourceNameSafe%'";
        }
        
        foreach ($centerIdsArray as $centerId) {
            $query = "SELECT stic_resources_stic_centersstic_resources_idb
                      FROM stic_resources_stic_centers_c
                      WHERE stic_resources_stic_centersstic_centers_ida = '$centerId'";
            $result = $db->query($query, true);
            
            while ($row = $db->fetchByAssoc($result)) {
                $resourceId = $row['stic_resources_stic_centersstic_resources_idb'];
                
                if ($numberOfPlaces && count($resources) >= (int)$numberOfPlaces) {
                    break;
                }
                if (in_array($resourceId, $existingResourceIdsArray)) {
                    continue; 
                }
    
                $availability = $this->checkResourceAvailability($resourceId, $startDate, $endDate, $bookingId);
                
                if ($availability['resources_allowed']) {
                    $resourceQuery = "SELECT *
                                      FROM stic_resources
                                      WHERE id = '$resourceId'
                                      AND type = 'place'
                                      AND deleted = 0";
                
                    $resourceQuery .= $resourcePlaceUserTypeCondition;
                    $resourceQuery .= $resourcePlaceTypeCondition;
                    $resourceQuery .= $resourceGenderCondition;
                    $resourceQuery .= $resourceNameCondition;
                    
                    if (!empty($numberOfPlaces)) {
                        $resourceQuery .= " LIMIT $numberOfPlaces"; 
                    }
                    
                    $resourceResult = $db->query($resourceQuery);
                    
                    if ($resourceResult !== false) {
                        $resourceData = $db->fetchByAssoc($resourceResult);
                        
                        if ($resourceData !== false) {
                            $resourceItem = [
                                'resource_id' => $resourceData['id'],
                            ];
                            
                            foreach ($config_place_fields as $fieldKey => $fieldLabel) {
                                if (isset($resourceData[$fieldKey])) {
                                    $value = $resourceData[$fieldKey];
                                    
                                    if ($fieldKey === 'user_type' && !empty($value)) {
                                        $value = $this->translateDropdownValue('stic_resources_places_users_list', $value);
                                    } elseif ($fieldKey === 'place_type' && !empty($value)) {
                                        $value = $this->translateDropdownValue('stic_resources_places_type_list', $value);
                                    } elseif ($fieldKey === 'gender' && !empty($value)) {
                                        $value = $this->translateDropdownValue('stic_resources_places_gender_list', $value);
                                    } elseif ($fieldKey === 'type' && !empty($value)) {
                                        $value = $this->translateDropdownValue('stic_resources_types_list', $value);
                                    }
                                    
                                    $resourceItem['resource_' . $fieldKey] = $value;
                                }
                            }
                            
                            $resources[] = $resourceItem;
                        }
                    }
                }
            }
            
            if ($numberOfPlaces && count($resources) >= (int)$numberOfPlaces) {
                break;
            }
        }
        
        echo json_encode(['success' => true, 'resources' => $resources]);
        return;
    }

    public function action_loadExistingResources()
    {
        $bookingId = $_REQUEST['bookingId'] ?? null;
        
        if (empty($bookingId)) {
            echo json_encode(['success' => false]);
            return;
        }
        
        $config_place_fields = require 'modules/stic_Bookings/configPlaceFields.php';
        
        $db = DBManagerFactory::getInstance();
        $resources = [];
        
        $booking = BeanFactory::getBean('stic_Bookings', $bookingId);
        if ($booking && $booking->load_relationship('stic_resources_stic_bookings')) {
            foreach ($booking->stic_resources_stic_bookings->getBeans() as $resourceBean) {
                $resourceId = $resourceBean->id;
                
                $resourceQuery = "SELECT * FROM stic_resources WHERE id = '$resourceId' AND deleted = 0";
                $resourceResult = $db->query($resourceQuery);           
                
                if ($resourceResult !== false) {
                    $resourceData = $db->fetchByAssoc($resourceResult);
                    
                    if ($resourceData !== false) {
                        $resourceItem = [
                            'resource_id' => $resourceData['id'],
                        ];
                        
                        foreach ($config_place_fields as $fieldKey => $fieldLabel) {
                            if (isset($resourceData[$fieldKey])) {
                                $value = $resourceData[$fieldKey];
                                
                                if ($fieldKey === 'user_type' && !empty($value)) {
                                    $value = $this->translateDropdownValue('stic_resources_places_users_list', $value);
                                } elseif ($fieldKey === 'place_type' && !empty($value)) {
                                    $value = $this->translateDropdownValue('stic_resources_places_type_list', $value);
                                } elseif ($fieldKey === 'gender' && !empty($value)) {
                                    $value = $this->translateDropdownValue('stic_resources_places_gender_list', $value);
                                } elseif ($fieldKey === 'type' && !empty($value)) {
                                    $value = $this->translateDropdownValue('stic_resources_types_list', $value);
                                }
                                
                                $resourceItem['resource_' . $fieldKey] = $value;
                            }
                        }
                        
                        $resources[] = $resourceItem;
                    }
                }
            }
        }
        
        echo json_encode(['success' => true, 'resources' => $resources]);
        return;
    }
    private function translateDropdownValue($listName, $value)
    {
        global $app_list_strings;
        
        if (empty($app_list_strings)) {
            $app_list_strings = return_app_list_strings_language($GLOBALS['current_language']);
        }
        
        $possibleListNames = [
            $listName,
            'stic_' . str_replace('stic_resources_', '', $listName),
            str_replace('stic_resources_', '', $listName),
            str_replace('_list', '', $listName) . '_dom'
        ];
        
        foreach ($possibleListNames as $listKey) {
            if (isset($app_list_strings[$listKey]) && isset($app_list_strings[$listKey][$value])) {
                return $app_list_strings[$listKey][$value];
            }
        }
        
        return $value;
    }
    private function checkResourceAvailability($resourceId, $startDate, $endDate, $bookingId)
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
                    // Requested resource(s) is(are) not available
                    return array('success' => true, 'resources_allowed' => false);
                }
            } else {
                // Action unsuccessfully completed
                return array('success' => false, 'resources_allowed' => $res);
            }
        }

        // Requested resource(s) is(are) available
        return array('success' => true, 'resources_allowed' => true);
    }
    public function action_getResourceTypes()
    {
        $centerId = $_REQUEST['centerId'];

        require_once 'modules/stic_Resources/vardefs.php';
        global $app_list_strings;
        $stic_resources_places_users_list = $app_list_strings['stic_resources_places_users_list'] ?? [];
        $stic_resources_places_type_list = $app_list_strings['stic_resources_places_type_list'] ?? [];
        $stic_resources_places_gender_list = $app_list_strings['stic_resources_places_gender_list'] ?? [];

        $response = array('success' => true, 'stic_resources_places_users_list' => array(), 'stic_resources_places_type_list' => array(), 'stic_resources_places_gender_list' => array());

        foreach ($stic_resources_places_users_list as $value => $label) {
            $response['stic_resources_places_users_list'][] = array('value' => $value, 'label' => $label);
        }
        foreach ($stic_resources_places_type_list as $value => $label) {
            $response['stic_resources_places_type_list'][] = array('value' => $value, 'label' => $label);
        }
        foreach ($stic_resources_places_gender_list as $value => $label) {
            $response['stic_resources_places_gender_list'][] = array('value' => $value, 'label' => $label);
        }

        echo json_encode($response);
        sugar_cleanup(true);
    }
    public function action_closeResource()
    {
        global $app_list_strings, $timedate, $current_user;

        if (empty($_REQUEST['record_id']) || empty($_REQUEST['resource_id'])) {
            echo json_encode(['success' => false]);
            return;
        }

        $bookingId = $_REQUEST['record_id'];
        $resourceId = $_REQUEST['resource_id'];

        $booking = BeanFactory::getBean('stic_Bookings', $bookingId);

        $newBooking = BeanFactory::newBean('stic_Bookings');

        $baseName = $booking->name;

        $counter = 0;
        $uniqueName = $baseName;
        while (true) {
            $existingBooking = $GLOBALS['db']->getOne("SELECT COUNT(*) FROM stic_bookings WHERE name = '{$uniqueName}'");

            if ($existingBooking == 0) {
                break;
            }

            $counter++;
            $uniqueName = $baseName . '.' . $counter;
        }

        $nowDb = $timedate->nowDb();
        $endDate = $timedate->to_display_date_time($nowDb, true, true, $current_user);

        $newBooking->name = $uniqueName;
        $newBooking->status = $booking->status;
        $newBooking->start_date = $booking->start_date;
        $newBooking->end_date = $endDate;
        $newBooking->parent_name = $booking->parent_name;
        $newBooking->parent_type = $booking->parent_type;
        $newBooking->parent_id = $booking->parent_id;
        $newBooking->code = $booking->code . $counter;
        $newBooking->status = $app_list_strings['stic_bookings_status_list']['closed'];
        $newBooking->assigned_user_id = $booking->assigned_user_id;
        $newBooking->assigned_user_name = $booking->assigned_user_name;
        $newBooking->description = $booking->description;
        $newBooking->place_booking = $booking->place_booking;

        $newBooking->save();

        if ($newBooking->load_relationship('stic_resources_stic_bookings')) {
            $newBooking->stic_resources_stic_bookings->add($resourceId);
        } else {
            echo json_encode(['success' => false]);
            return;
        }

        if ($booking->load_relationship('stic_resources_stic_bookings')) {
            $booking->stic_resources_stic_bookings->delete($bookingId, $resourceId);
        }

        echo json_encode(['success' => true, 'booking_id' => $newBooking->id]);
        return;
    }
    public function action_validateResourceDates()
    {
        global $timedate;

        if (empty($_REQUEST['record_id'])) {
            echo json_encode(['valid' => false]);
            return;
        }

        $bookingId = $_REQUEST['record_id'];
        $booking = BeanFactory::getBean('stic_Bookings', $bookingId);

        if (!$booking) {
            echo json_encode(['valid' => false]);
            return;
        }

        $startDate = $booking->start_date;
        $currentDate = $timedate->nowDb();

        if (!$timedate->fromDb($startDate)) {
            $startDate = $timedate->to_db($startDate);
        }

        $valid = $timedate->fromDb($currentDate) >= $timedate->fromDb($startDate);

        echo json_encode(['valid' => $valid]);
        return;
    }
    /**
     * Get resource type for validation
     */
    public function action_getResourceType()
    {
        global $db;
        
        $resourceId = $_REQUEST['resourceId'] ?? '';
        
        if (empty($resourceId)) {
            echo json_encode(array('success' => false, 'message' => 'Resource ID is required'));
            return;
        }
        
        $query = "SELECT type FROM stic_resources WHERE id = " . $db->quoted($resourceId) . " AND deleted = 0";
        $result = $db->query($query);
        
        if ($row = $db->fetchByAssoc($result)) {
            echo json_encode(array(
                'success' => true, 
                'type' => $row['type']
            ));
        } else {
            echo json_encode(array('success' => false, 'message' => 'Resource not found'));
        }
    }

    public function createFilterCondition($filterValue, $columnName, $db) {
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
    
}