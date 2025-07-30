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

class stic_ResourcesController extends SugarController
{
    /**
     * Show the M182 wizard
     *
     * @return void
     */
    public function action_listplaces()
    {        
        // Call to the smarty template
        $this->view = "listplaces";
    }

    /**
     * Check if a resource has associated bookings
     *
     * @return void
     */
    public function action_checkBookings()
    {
        while (ob_get_level() > 0) {
            ob_end_clean();
        }

        $resourceId = $_POST['resource_id'] ?? '';
        if (empty($resourceId)) {
            header('Content-Type: application/json');
            echo json_encode(array("success" => false, "message" => "Resource ID is required"));
            sugar_cleanup(true); 
            exit();
        }
    
        global $db;
    
        $query = "SELECT COUNT(DISTINCT rsb.stic_resources_stic_bookingsstic_bookings_idb) AS booking_count
                  FROM stic_resources_stic_bookings_c rsb
                  INNER JOIN stic_bookings sb ON rsb.stic_resources_stic_bookingsstic_bookings_idb = sb.id
                  WHERE rsb.stic_resources_stic_bookingsstic_resources_ida = '" . $db->quote($resourceId) . "'
                  AND rsb.deleted = 0;";
    
        $result = $db->query($query);
    
        $response = [];
    
        if ($result !== false) {
            $row = $db->fetchByAssoc($result);
            $response = array(
                "success" => true,
                "hasBookings" => ($row["booking_count"] > 0)
            );
        } else {
            $response = array(
                "success" => false,
                "hasBookings" => false
            );
        }
    
        header('Content-Type: application/json');
        echo json_encode($response);
                sugar_cleanup(true); 
        exit(); 
    }
}