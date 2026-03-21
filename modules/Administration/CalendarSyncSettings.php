<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $current_user, $mod_strings, $app_strings;

if (!is_admin($current_user)) {
    echo $app_strings['ERR_NOT_ADMIN'];
    return;
}

require_once 'include/CalendarSync/CalendarSync.php';

$calendarSync = CalendarSync::getInstance();

$message = '';
$success = true;

if (($_POST['form_action'] ?? '') === 'manual_trigger') {
    $triggerSuccess = $calendarSync->syncAllCalendarAccounts(true);
    $success = $triggerSuccess;
    $message = $triggerSuccess
        ? $mod_strings['LBL_CALENDAR_SYNC_MANUAL_TRIGGER_SUCCESS']
        : $mod_strings['LBL_CALENDAR_SYNC_MANUAL_TRIGGER_FAILED'];
}

if (($_POST['form_action'] ?? '') === 'save') {
    $configSuccess = $calendarSync->saveConfig($_POST);

    $success = $configSuccess;
    $message = $configSuccess
        ? $mod_strings['LBL_CALENDAR_SYNC_SETTINGS_SAVED_SUCCESS']
        : $mod_strings['LBL_CALENDAR_SYNC_SETTINGS_SAVED_FAILED'];
}

$conflictResolutionOptions = [];
foreach ($calendarSync->getConflictResolutionCases() as $case) {
    $labelKey = 'LBL_CALENDAR_SYNC_CONFLICT_' . strtoupper($case->value);
    $conflictResolutionOptions[$case->value] = $mod_strings[$labelKey] ?? $case->value;
}

$scheduler = $calendarSync->getScheduler();

global $timedate;
$lastScheduledRunUserTz = null;
if (!empty($scheduler?->last_run)) {
    $lastScheduledRunUserTz = $timedate->to_display_date_time($scheduler->last_run);
}
$lastManualRunTime = null;
$lastManualRunRaw = $calendarSync->getConfig()['last_manual_run_time'] ?? null;
if (!empty($lastManualRunRaw)) {
    $lastManualRunTime = $timedate->to_display_date_time($lastManualRunRaw);
}

$schedulerStatus = [
    'id' => $scheduler?->id,
    'enabled' => $scheduler?->status === 'Active',
    'lastScheduledRunTime' => $lastScheduledRunUserTz,
    'exists' => (bool)$scheduler
];

$smarty = new Sugar_Smarty();
$smarty->assign('MOD', $mod_strings);
$smarty->assign('APP', $app_strings);
$smarty->assign('status', $schedulerStatus);
$smarty->assign('lastManualRunTime', $lastManualRunTime);
$smarty->assign('message', $message);
$smarty->assign('success', $success);
$smarty->assign('calendarSyncConfig', $calendarSync->getConfig());
$smarty->assign('conflictResolutionOptions', $conflictResolutionOptions);
$smarty->display('modules/Administration/templates/CalendarSyncSettings.tpl');