<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

global $current_user, $mod_strings, $app_strings, $log;

if (!is_admin($current_user)) {
    echo $app_strings['ERR_NOT_ADMIN'];
    return;
}

require_once __DIR__ . '/../../include/Services/NormalizeRecords/NormalizeRecords.php';

$repairStatus = NormalizeRecords::getRepairStatus();

if ($repairStatus === NormalizeRecords::REPAIR_STATUS_REPAIRED || $repairStatus === NormalizeRecords::REPAIR_STATUS_IN_PROGRESS) {
    $mode = NormalizeRecords::getExecutionMode();

    $smarty = new Sugar_Smarty();
    $smarty->assign('MOD', $mod_strings);
    $smarty->assign('status', $repairStatus);
    $smarty->assign('mode', $mode);
    $smarty->display('modules/Administration/templates/RepairUtfEncodingStatus.tpl');

    return;
}


// the initial settings for the template variables to fill
$wasRepaired           = '';
$config_file_ready      = false;
$lbl_rebuild_config     = $mod_strings['LBL_REBUILD_CONFIG'];
$btn_rebuild_config     = $mod_strings['BTN_REBUILD_CONFIG'];
$disable_config_rebuild = 'disabled="disabled"';

// only do the rebuild if config file checks out and user has posted back
if (!empty($_POST['perform_rebuild_utf_encoding'])) {
    $data = [];

    $syncRun = !empty($_POST['syncRun']);
    $keepTrackingTables = !empty($_POST['keepTrackingTables']);

    $repairFrom = $_POST['repairFrom'] ?? null;
    if ($repairFrom === null) {
        $repairFrom = NormalizeRecords::UTF_REPAIR_FROM;
    } elseif (NormalizeRecords::isValidRepairFrom($repairFrom)) {
        $repairFrom .= ' 00:00:01';
    } elseif (!NormalizeRecords::isValidRepairFrom($repairFrom)) {
        $smarty = new Sugar_Smarty();
        $smarty->assign('MOD', $mod_strings);
        $smarty->assign('invalid_repair_from', true);
        $smarty->display('modules/Administration/templates/RepairUtfEncoding.tpl');
        return;
    }

    $data['repair_from'] = $repairFrom;

    if ($syncRun === true) {
        $smarty = new Sugar_Smarty();
        $smarty->assign('MOD', $mod_strings);
        $smarty->assign('status', 'in_progress');
        $smarty->assign('mode', NormalizeRecords::EXECUTION_MODE_SYNC);
        $smarty->display('modules/Administration/templates/RepairUtfEncodingSyncStatus.tpl');
        ob_flush();
        flush();

        $normalize = new NormalizeRecords();
        $result = $normalize->runAll($data, true);

        echo '<h3 class="pt-0">' . $mod_strings['LBL_RESULT'] . '</h3>';

        if ($result['success'] === true) {
            echo '<div>' . $mod_strings['LBL_NORMALIZE_SUCCESS']. '</div>';
        } else {
            echo '<div>' . $mod_strings['LBL_NORMALIZE_FAILURE']. '</div>';
        }

        if (empty($result['messages'])) {
            return;
        }

        foreach ($result['messages'] as $message) {
            echo '<div>' . $message . '</div>';
        }

        return;
    }


    if (!empty($keepTrackingTables)){
        $data['keepTracking'] = true;
    }

    require_once __DIR__ . '/../../include/Services/NormalizeRecords/NormalizeRecordsSchedulerJob.php';
    NormalizeRecordsSchedulerJob::scheduleJob($data);

    NormalizeRecords::setRepairStatus(NormalizeRecords::REPAIR_STATUS_IN_PROGRESS);
    NormalizeRecords::setExecutionMode(NormalizeRecords::EXECUTION_MODE_SYNC);

    $smarty = new Sugar_Smarty();
    $smarty->assign('MOD', $mod_strings);
    $smarty->assign('status', 'in_progress');
    $smarty->assign('mode', NormalizeRecords::EXECUTION_MODE_ASYNC);
    $smarty->display('modules/Administration/templates/RepairUtfEncodingStatus.tpl');

    return;
}

if (!isset($_REQUEST['perform_rebuild_utf_encoding'])) {
    $smarty = new Sugar_Smarty();
    $smarty->assign('MOD', $mod_strings);
    $smarty->assign('invalid_repair_from', false);
    $smarty->display('modules/Administration/templates/RepairUtfEncoding.tpl');
}
