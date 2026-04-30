<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2026 SalesAgility Ltd.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/SearchForm/SugarSpot.php';

global $current_user;
global $sugar_config;

header('Content-Type: application/json; charset=UTF-8');

if (isset($sugar_config['suite_sweetspot_enabled']) && !$sugar_config['suite_sweetspot_enabled']) {
    echo json_encode(array('records' => array()));
    sugar_cleanup(true);
}

if (empty($current_user) || empty($current_user->id)) {
    http_response_code(403);
    echo json_encode(array(
        'error' => 'not_authenticated',
        'message' => 'User must be authenticated.',
    ));
    sugar_cleanup(true);
}

$term = isset($_REQUEST['term']) ? trim((string)$_REQUEST['term']) : '';
if ($term === '' && isset($_REQUEST['query'])) {
    $term = trim((string)$_REQUEST['query']);
}
if (strlen($term) > 255) {
    $term = substr($term, 0, 255);
}

$maxRecords = 5;
if (isset($_REQUEST['limit'])) {
    $reqLimit = (int)$_REQUEST['limit'];
    if ($reqLimit > 0 && $reqLimit <= 50) {
        $maxRecords = $reqLimit;
    }
}

if ($term === '') {
    echo json_encode(array('records' => array()));
    sugar_cleanup(true);
}

$spot = new SugarSpot('');
$results = $spot->search($term);

$records = array();
$count = 0;
foreach ($results as $moduleName => $moduleData) {
    if (empty($moduleData['data']) || !is_array($moduleData['data'])) {
        continue;
    }

    foreach ($moduleData['data'] as $row) {
        if ($count >= $maxRecords) {
            break 2;
        }
        if (empty($row['ID'])) {
            continue;
        }

        $name = '';
        if (!empty($row['NAME'])) {
            $name = $row['NAME'];
        } elseif (!empty($row['DOCUMENT_NAME'])) {
            $name = $row['DOCUMENT_NAME'];
        } else {
            foreach ($row as $key => $value) {
                if (strpos((string)$key, 'NAME') !== false && !empty($value)) {
                    $name = $value;
                    break;
                }
            }
        }

        $id = $row['ID'];
        $recordUrl = suite_sweetspot_build_record_url($moduleName, $id);
        $records[] = array(
            'id' => $id,
            'name' => $name,
            'module' => $moduleName,
            'url' => $recordUrl,
        );
        $count++;
    }
}

echo json_encode(array('records' => $records));
sugar_cleanup(true);

/**
 * Build a record URL using a bean when possible.
 *
 * @param string $moduleName
 * @param string $id
 * @return string
 */
function suite_sweetspot_build_record_url($moduleName, $id)
{
    if (!empty($GLOBALS['beanList'][$moduleName])) {
        $bean = BeanFactory::newBean($moduleName);
        if ($bean && method_exists($bean, 'getDetailViewURL')) {
            $bean->id = $id;
            $url = $bean->getDetailViewURL();
            if (!empty($url)) {
                return $url;
            }
        }
    }

    return 'index.php?' . http_build_query(array(
        'module' => $moduleName,
        'action' => 'DetailView',
        'record' => $id,
    ));
}
