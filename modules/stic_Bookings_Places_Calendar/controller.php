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

require_once 'modules/stic_Bookings_Calendar/controller.php';

class stic_Bookings_Places_CalendarController extends stic_Bookings_CalendarController
{
    /**
     * Override method to get booked resources with additional place-specific fields
     * 
     * @param String $start_date
     * @param String $end_date
     * @param Array $filteredResources
     * @return array
     */
    protected function getBookedResources($start_date, $end_date, $filteredResources)
    {
        global $current_user, $db;
        $resourcesBean = BeanFactory::getBean('stic_Resources');
        $bookedResources = array();
        $query = "
            SELECT
                sr.id AS resource_id,
                sr.name AS resource_name,
                sr.place_type,
                sr.user_type,
                sr.gender,
                sr.type,
                sb.id AS booking_id,
                sb.code AS booking_code,
                sb.name AS booking_name,
                sb.start_date,
                sb.end_date,
                sb.all_day,
                sc.id AS center_id,
                sc.name AS center_name
            FROM stic_resources sr
            JOIN stic_resources_stic_bookings_c srsb ON sr.id = srsb.stic_resources_stic_bookingsstic_resources_ida
            JOIN stic_bookings sb ON srsb.stic_resources_stic_bookingsstic_bookings_idb = sb.id
            LEFT JOIN stic_resources_stic_centers_c srsc ON sr.id = srsc.stic_resources_stic_centersstic_resources_idb
            LEFT JOIN stic_centers sc ON srsc.stic_resources_stic_centersstic_centers_ida = sc.id
            WHERE sb.end_date >= '$start_date'
            AND sb.start_date <= '$end_date'
            AND sr.type ='place'
            AND sb.status != 'cancelled'
            AND sb.deleted = 0
            AND sr.deleted = 0
            AND srsb.deleted = 0
        ";

        if (!empty($filteredResources)) {
            $filteredResourcesStr = implode("','", $filteredResources);
            $query .= " AND sr.id IN ('$filteredResourcesStr')";
        }

        $result = $db->query($query);

        while ($row = $db->fetchByAssoc($result)) {
            $bookedResources[] = array(
                'title' => $row['booking_name'] . ' - ' . str_pad($row['booking_code'], 5, "0", STR_PAD_LEFT),
                'resourceName' => $row['resource_name'],
                'module' => 'stic_Bookings',
                'id' => $row['booking_id'],
                'recordId' => $row['booking_id'],
                'resourceId' => $row['resource_id'],
                'resourcePlaceType' => $row['place_type'],
                'resourcePlaceUserType' => $row['user_type'],
                'resourcePlaceGenderType' => $row['gender'],
                'resourceType' => $row['type'],
                'resourceCenterName' => $row['center_name'],
                'resourceCenterId' => $row['center_id'],
                'allDay' => $row['all_day'],
                'start' => $row['start_date'],
                'end' => $row['end_date'],
                'className' => 'id-' . $row['resource_id'],
            );
        }

        return $bookedResources;
    }
    
    /**
     * Places-specific action to get availability data
     *
     * @return void
     */
    public function action_get_places_availability_data()
    {
        global $current_user, $timedate;
        $savedFilters = json_decode($current_user->getPreference('stic_bookings_places_calendar_filters'), true) ?? [];

        if (!isset($_POST['start']) || !isset($_POST['end'])) {
            echo json_encode(array('error' => 'Please provide start and end dates.'));
            die();
        }

        $startDate = $_POST['start'];
        $endDate = $_POST['end'];

        if (!$this->validateDate($startDate, 'Y-m-d') || !$this->validateDate($endDate, 'Y-m-d')) {
            echo json_encode(array('error' => 'Invalid date format. Please use YYYY-MM-DD.'));
            die();
        }

        $sticCenterId = $_POST['stic_center_id'] ?? $savedFilters['stic_center_id'] ?? '';

        $sticPlacesUser = $_POST['stic_resources_places_users_list'] ?? $savedFilters['stic_resources_places_users_list'] ?? [];
        $sticPlacesType = $_POST['stic_resources_places_type_list'] ?? $savedFilters['stic_resources_places_type_list'] ?? [];
        $sticPlacesGender = $_POST['stic_resources_places_gender_list'] ?? $savedFilters['stic_resources_places_gender_list'] ?? [];

        $filteredResources = $this->getFilteredResources($sticCenterId, $sticPlacesUser, $sticPlacesType, $sticPlacesGender);

        $bookedResources = $this->getBookedResources($startDate, $endDate, $filteredResources);
        $availableResources = $this->getPlacesAvailability($startDate, $endDate, $filteredResources);

        $result = array();
        $dates = $this->getDatesArray($startDate, $endDate);

        foreach ($dates as $date) {
            $result[$date] = array(
                'occupied' => array(),
                'available' => count($availableResources[$date]),
            );
        }

        foreach ($bookedResources as $resource) {
            $resourceStart = max($startDate, substr($resource['start'], 0, 10));
            $resourceEnd = min($endDate, substr($resource['end'], 0, 10));
            $currentDate = $resourceStart;

            if (isset($resource['resourceType']) && $resource['resourceType'] == 'place' && in_array($resource['resourceId'], $filteredResources)) {
                while ($currentDate <= $resourceEnd) {
                    $dateKey = date('Y-m-d', strtotime($currentDate));
                    if (isset($result[$dateKey])) {
                        if (!isset($result[$dateKey]['occupied'][$resource['title']])) {
                            $result[$dateKey]['occupied'][$resource['title']] = array(
                                'name' => $resource['title'],
                                'id' => $resource['recordId'],
                            );
                        }
                        if (!isset($result[$dateKey]['occupied'][$resource['title']][$resource['resourceCenterName']])) {
                            $result[$dateKey]['occupied'][$resource['title']][$resource['resourceCenterName']] = array();
                        }
                        if (!is_array($result[$dateKey]['occupied'][$resource['title']][$resource['resourceCenterName']])) {
                            $result[$dateKey]['occupied'][$resource['title']][$resource['resourceCenterName']] = array();
                        }
                        $result[$dateKey]['occupied'][$resource['title']][$resource['resourceCenterName']][] = $resource['resourceName'];
                    }
                    $currentDate = date('Y-m-d', strtotime($currentDate . ' +1 day'));
                }
            }
        }

        echo json_encode($result);
        die();
    }
    
    /**
     * Places-specific method to get places availability
     * 
     * @param String $start_date
     * @param String $end_date
     * @param Array $filteredResources
     * @return array
     */
    private function getPlacesAvailability($start_date, $end_date, $filteredResources = null)
    {
        global $db;

        $availablePlaces = array();

        $query = "SELECT stic_resources.id AS resource_id, stic_resources.name AS resource_name, stic_resources.type AS resource_type, stic_centers.id AS center_id,
        stic_centers.name AS center_name
                FROM
                    stic_resources
                JOIN  stic_resources_stic_centers_c
                ON    stic_resources.id = stic_resources_stic_centers_c.stic_resources_stic_centersstic_resources_idb
                JOIN
                    stic_centers
                ON    stic_centers.id = stic_resources_stic_centers_c.stic_resources_stic_centersstic_centers_ida
                WHERE
                    stic_resources.deleted = 0 AND stic_resources.type = 'place'";
        // Filters are added
        if (!empty($filteredResources)) {
            $query .= " AND stic_resources.id IN ('" . implode("','", $filteredResources) . "')";
        }

        $result = $db->query($query);

        $allResources = array();
        while ($row = $db->fetchByAssoc($result)) {
            $allResources[$row['resource_id']] = array(
                'name' => $row['resource_name'],
                'type' => $row['resource_type'],
                'center_id' => $row['center_id'],
                'center_name' => $row['center_name'],
            );
        }

        $query = "SELECT stic_resources_stic_bookingsstic_resources_ida as resource_id,
                     start_date, end_date
              FROM stic_bookings
              JOIN stic_resources_stic_bookings_c ON stic_resources_stic_bookingsstic_bookings_idb = stic_bookings.id
              WHERE stic_bookings.deleted = 0
                AND stic_resources_stic_bookings_c.deleted = 0
                AND start_date <= '$end_date'
                AND end_date >= '$start_date'";
        $result = $db->query($query);

        $bookedResources = array();
        while ($row = $db->fetchByAssoc($result)) {
            $resourceId = $row['resource_id'];
            $startDate = $row['start_date'];
            $endDate = $row['end_date'];

            $currentDate = $startDate;
            while ($currentDate <= $endDate && $currentDate <= $end_date) {
                $dateKey = date('Y-m-d', strtotime($currentDate));
                if (!isset($bookedResources[$dateKey])) {
                    $bookedResources[$dateKey] = array();
                }
                $bookedResources[$dateKey][] = $resourceId;
                $currentDate = date('Y-m-d', strtotime($currentDate . ' +1 day'));
            }
        }

        $currentDate = $start_date;
        while ($currentDate <= $end_date) {
            $dateKey = date('Y-m-d', strtotime($currentDate));
            $availablePlaces[$dateKey] = array();

            foreach ($allResources as $resourceId => $resourceInfo) {
                if (!isset($bookedResources[$dateKey]) || !in_array($resourceId, $bookedResources[$dateKey])) {
                    $availablePlaces[$dateKey][] = array(
                        'id' => $resourceId,
                        'name' => $resourceInfo['name'],
                        'type' => $resourceInfo['type'],
                        'center_id' => $resourceInfo['center_id'],
                        'center_name' => $resourceInfo['center_name'],
                    );
                }
            }
            $currentDate = date('Y-m-d', strtotime($currentDate . ' +1 day'));
        }

        return $availablePlaces;
    }

    /**
     * Save place-specific filters for the user
     *
     * @return void
     */
    public function action_SaveFilters()
    {
        global $current_user, $timedate;

        require_once 'modules/UserPreferences/UserPreference.php';
        $filters = array(
            'stic_center_id' => $_POST['stic_center_id'] ?? '',
            'stic_center_name' => $_POST['stic_center_name'] ?? '',
            'stic_resources_places_users_list' => $_POST['stic_resources_places_users_list'] ?? [],
            'stic_resources_places_type_list' => $_POST['stic_resources_places_type_list'] ?? [],
            'stic_resources_places_gender_list' => $_POST['stic_resources_places_gender_list'] ?? []
        );
        $current_user->setPreference('stic_bookings_places_calendar_filters', json_encode($filters));

        if (isset($_REQUEST['day']) && !empty($_REQUEST['day'])) {
            header("Location: index.php?module=stic_Bookings_Places_Calendar&action=index&view=" . $_REQUEST['view'] . "&hour=0&day=" . $_REQUEST['day'] . "&month=" . $_REQUEST['month'] . "&year=" . $_REQUEST['year']);
        } else {
            header("Location: index.php?module=stic_Bookings_Places_Calendar&action=index");
        }
        exit;
    }
    
    /**
     * Get resources filtered by specific place attributes
     *
     * @param string $center Center ID
     * @param array $users User types
     * @param array $types Place types
     * @param array $gender Gender types
     * @return array
     */
    private function getFilteredResources($center, $users, $types, $gender)
    {
        global $db;

        $query = "SELECT 
                    r.id 
                  FROM 
                    stic_resources r 
                  INNER JOIN 
                    stic_resources_stic_centers_c rc
                  ON 
                    r.id = rc.stic_resources_stic_centersstic_resources_idb
                  WHERE 
                    r.deleted = 0 
                  AND  
                    r.type = 'place'
                ";

        if (!empty($users)) {
            $quotedUsers = array_map(array($db, 'quote'), $users);
            $usersStr = implode("','", $quotedUsers);
            $query .= " AND user_type IN ('$usersStr')";
        }

        if (!empty($types)) {
            $quotedTypes = array_map(array($db, 'quote'), $types);
            $typesStr = implode("','", $quotedTypes);
            $query .= " AND place_type IN ('$typesStr')";
        }

        if (!empty($gender)) {
            $quotedGender = array_map(array($db, 'quote'), $gender);
            $genderStr = implode("','", $quotedGender);
            $query .= " AND gender IN ('$genderStr')";
        }
        if (!empty($center)) {
            $centerId = $db->quote($center);
            $query .= " AND rc.stic_resources_stic_centersstic_centers_ida = '$centerId'";
        }
        
        $result = $db->query($query);
        $filteredResources = array();
        while ($row = $db->fetchByAssoc($result)) {
            $filteredResources[] = $row['id'];
        }

        return $filteredResources;
    }
}