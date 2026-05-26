<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2025 SinergiaTIC Association
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

require_once 'include/EditView/SugarVCR.php';

function outputJson($success, $data = null, $error = null) {
    @ob_end_clean();
    ob_start();
    ob_clean();
    header('Content-Type: application/json');
    
    $response = ['success' => $success];
    if ($data !== null) {
        $response = array_merge($response, $data);
    }
    if ($error !== null) {
        $response['error'] = $error;
    }
    
    echo json_encode($response);
    ob_flush();
}

$module = $_REQUEST['module'] ?? '';
$where = html_entity_decode($_REQUEST['where'] ?? '', ENT_QUOTES, 'UTF-8');

if (empty($module)) {
    outputJson(false, null, 'Module not specified');
    return;
}

$bean = BeanFactory::getBean($module);
if (!$bean) {
    outputJson(false, null, 'Invalid module');
    return;
}

if (!$bean->ACLAccess('ListView')) {
    outputJson(false, null, 'Access denied');
    return;
}

// The main query stored by ListViewData.php includes all filter joins (inner_join, custom_from, etc.)
// that are needed for WHERE clauses referencing joined tables (e.g., jt6.name)
$storedQuery = SugarVCR::retrieve($module);
if (!empty($storedQuery)) {
    $countQuery = $bean->create_list_count_query($storedQuery);
} else {
    // Fallback: rebuild query without filter joins (may miss search-specific joins)
    $ret_array = $bean->create_new_list_query('', $where, [], [], 0, '', true, $bean, false);
    if (!empty($bean->listview_inner_join)) {
        $ret_array['inner_join'] = ' ' . implode(' ', $bean->listview_inner_join) . ' ';
    }
    $mainQuery = $ret_array['select'] . $ret_array['from'] . ($ret_array['inner_join'] ?? '') . $ret_array['where'];
    $countQuery = $bean->create_list_count_query($mainQuery);
}

$db = DBManagerFactory::getInstance();
$result = $db->query($countQuery);
$row = $db->fetchByAssoc($result);
$total = intval($row['c'] ?? $row['count'] ?? 0);

outputJson(true, ['total' => $total]);
