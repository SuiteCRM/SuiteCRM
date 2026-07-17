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

/**
 * Presentation layer script for adding signers to a signature process.
 *
 * This script reads HTTP request data, delegates the business logic to
 * SignatureSignersManager, and handles UI messages and redirect.
 * The business logic is also available from non-HTTP contexts via
 * SignatureSignersManager::addSignersToSignature().
 *
 * Incorporates the fix from PR #1279: manual authentication of $current_user
 * from the session when the entrypoint is called with 'auth' => false,
 * ensuring created_by and assigned_user_id are correctly populated.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $mod_strings, $current_user;

if (empty($current_user->id) && !empty($_SESSION['authenticated_user_id'])) {
    $current_user = BeanFactory::getBean('Users', $_SESSION['authenticated_user_id']);
}

require_once 'modules/stic_Signatures/SignatureSignersManager.php';

$module = $_REQUEST['module'] ?? '';
$signatureId = $_REQUEST['signature-id'] ?? '';
$currentUserId = $current_user->id ?? null;

if (empty($module)) {
    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Module parameter is empty.");
    sugar_die("Invalid Module");
}

if (empty($signatureId)) {
    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Signature ID is empty.");
    sugar_die("Signature ID is required");
}

// Determine record IDs based on mass update context or direct selection
if (isset($_REQUEST['current_post']) && $_REQUEST['current_post'] !== '') {
    $recordIds = SignatureSignersManager::getRecordIdsFromMassUpdate($module, $_REQUEST['current_post']);
} else {
    $uid = $_REQUEST['uid'] ?? '';
    $recordIds = !empty($uid) ? explode(',', $uid) : [];
}

if (empty($recordIds)) {
    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": No record IDs found.");
    sugar_die("No records selected");
}

$result = SignatureSignersManager::addSignersToSignature(
    $signatureId,
    $module,
    $recordIds,
    $currentUserId
);

$stic_SignatureBean = BeanFactory::getBean('stic_Signatures', $signatureId);

if ($result['ok'] !== 0) {
    SugarApplication::appendSuccessMessage("<p class='label label-success'><strong>{$result['ok']}</strong> " . translate('LBL_SIGNERS_ADDED_MSG', 'stic_Signatures') . ".</p>");
    if ($stic_SignatureBean && in_array($stic_SignatureBean->status, ['completed', 'cancelled', 'paused'])) {
        SugarApplication::appendSuccessMessage("<br><p class='label label-warning'>" . translate('LBL_SIGNERS_ADDED_CLOSED_MSG', 'stic_Signatures') . "</p>");
    }
    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$result['ok']} signers added successfully.");
}

if ($result['ko'] !== 0) {
    SugarApplication::appendErrorMessage("<p class='label label-error'><strong>{$result['ko']}</strong> " . translate('LBL_SIGNERS_NOT_ADDED_MSG', 'stic_Signatures') . ".</p>");
    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$result['ko']} signers could not be added because they already exist or an error occurred.");
}

SugarApplication::redirect('index.php?module=stic_Signatures&action=DetailView&record=' . $signatureId);
