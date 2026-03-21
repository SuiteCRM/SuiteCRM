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

global $mod_strings, $current_user;
$module_menu = array();

require_once 'include/CalendarSync/CalendarSync.php';
$calendarSync = CalendarSync::getInstance();

$hasPersonalAccount = $calendarSync->hasPersonalCalendarAccount($current_user->id);
if (!$hasPersonalAccount || is_admin($current_user)) {
    $module_menu[] = array("index.php?module=CalendarAccount&action=EditView&type=personal", $mod_strings['LNK_LIST_CREATE_NEW_PERSONAL'], "Create");
}
$groupActiveAccountsDisabled = true;
if (!$groupActiveAccountsDisabled) {
    $module_menu[] = array("index.php?module=CalendarAccount&action=EditView&type=group", $mod_strings['LNK_LIST_CREATE_NEW_GROUP'], "Create");
}
$module_menu[] = array("index.php?module=CalendarAccount&action=index", $mod_strings['LNK_LIST'], "List");
$module_menu[] = array("index.php?module=ExternalOAuthConnection&action=index", $mod_strings['LNK_LIST_EXTERNAL_OAUTH_CONNECTIONS'], "List");
$module_menu[] = array("index.php?module=ExternalOAuthProvider&action=index", $mod_strings['LNK_LIST_EXTERNAL_OAUTH_PROVIDERS'], "List");
