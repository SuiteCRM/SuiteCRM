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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * EntryPoint: stic_AWF_checkStatus
 */

if (ob_get_level()) {
    ob_end_clean();
}
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
header('Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With');
header('Content-Type: application/json');

if ($_SERVER['REQUEST_METHOD'] === 'OPTIONS') {
    exit(0);
}

$id = $_REQUEST['id'] ?? '';
if (empty($id)) {
    echo json_encode(['active' => false, 'message' => 'No ID provided']);
    exit;
}

$db = DBManagerFactory::getInstance();
$safeId = $db->quote($id);

// Get the status and dates
$query = "SELECT status, start_date, end_date 
            FROM stic_awf_forms 
            WHERE id = '$safeId' AND deleted = 0";
$result = $db->query($query);
$row = $db->fetchByAssoc($result);

$isActive = false;
if ($row) {
    $status = $row['status'];
    $startDate = $row['start_date'];
    $endDate = $row['end_date'];
    
    // Get the current time in UTC (DB format) for comparison
    $now = gmdate('Y-m-d H:i:s');

    // Validation logic
    if ($status === 'public') {
        $isActive = true;
        if (!empty($startDate) && $startDate > $now) {
            $isActive = false;
        }
        if (!empty($endDate) && $endDate < $now) {
            $isActive = false;
        }
    }
}

// Update the form access statistics counters
if ($status) {
    // Visitor counter
    if ($isActive) {
        $db->query("UPDATE stic_awf_forms SET analytics_views = analytics_views + 1 WHERE id = '$safeId'");
    } else {
        $db->query("UPDATE stic_awf_forms SET analytics_blocked = analytics_blocked + 1 WHERE id = '$safeId'");
    }

    // Referrer registration (domain)
    $referer = $_SERVER['HTTP_REFERER'] ?? '';
    if (!empty($referer)) {
        $parts = parse_url($referer);
        $host = $parts['host'] ?? '';
        $path = $parts['path'] ?? '';

        // Build the clean URL: Domain + Path (without https:// or parameters)
        $cleanUrl = $host . $path;
        $cleanUrl = substr($cleanUrl, 0, 200); 
        $safeUrl = $db->quote($cleanUrl);

        // CONCAT_WS automatically adds a separator
        $queryRef = "UPDATE stic_awf_forms 
                        SET analytics_referrers = CONCAT_WS('\n', analytics_referrers, '$safeUrl')
                        WHERE id = '$safeId' 
                        AND (analytics_referrers IS NULL OR analytics_referrers NOT LIKE '%$safeUrl%')";
        
        $db->query($queryRef);
    }
}

echo json_encode(['active' => $isActive]);
exit;
