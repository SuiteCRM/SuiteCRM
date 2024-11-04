<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'modules/AOP_Case_Updates/util.php';

/**
 * @param InboundEmail|null $focus
 * @param string $field
 * @param string $value
 * @param string $view
 * @return mixed
 */
function getInboundEmailDistributionOptions(
    ?InboundEmail $focus,
    string $field = 'distribution_options',
    $value = '',
    string $view = 'DetailView'
) {
    if ($view === 'EditView' || $view === 'MassUpdate' || $view === 'QuickCreate' || $view === 'ConvertLead') {
        $html = getAOPAssignField('distribution_options', [$value]);

        return $html;
    }

    return getAOPAssignFieldDetailView($focus->distribution_options) ?? '';
}

/**
 * @param InboundEmail|null $focus
 * @param string $field
 * @param string $value
 * @param string $view
 * @return string
 */
function getUserSignature(
    ?InboundEmail $focus,
    string $field = 'account_signature_id',
    string $value = '',
    string $view = 'DetailView'
): string {

    global $current_user, $app_strings;

    $createdBy = $focus->created_by ?? '';
    /** @var User $owner */
    $owner = $current_user;

    if ($createdBy !== '') {
        $owner = BeanFactory::getBean('Users', $createdBy);
    }

    $defaultSignatureId = $owner->getPreference('signature_default') ?? '';

    $isEditView = $view === 'EditView' || $view === 'MassUpdate' || $view === 'QuickCreate' || $view === 'ConvertLead';

    $inboundEmailId = $focus->id ?? '';

    if ($isEditView === true) {
        return getInboundEmailSignatures($owner, $defaultSignatureId, 'account_signature_id');
    }

    if ($inboundEmailId === '') {
        return '';
    }

    $emailSignatures = $owner->getPreference('account_signatures', 'Emails') ?? '';
    $emailSignatures = sugar_unserialize(base64_decode($emailSignatures));

    $signatureId = $emailSignatures[$inboundEmailId] ?? '';

    if ($signatureId !== '' && $isEditView === true) {
        return getInboundEmailSignatures($owner, $defaultSignatureId, 'account_signature_id', $signatureId);
    }

    $signatures = $owner->getSignaturesArray(false);

    $signature = $signatures[$signatureId] ?? null;

    if ($signature === null) {
        return $app_strings['LBL_DEFAULT_EMAIL_SIGNATURES'];
    }

    if (empty($signature)) {
        return $app_strings['LBL_DEFAULT_EMAIL_SIGNATURES'];
    }

    return $signature['name'] ?? '';
}

/**
 * @param User $owner
 * @param string $defaultSig
 * @param string $elementId
 * @param string $selected
 * @return string
 */
function getInboundEmailSignatures(
    User $owner,
    string $defaultSig = '',
    string $elementId = 'account_signature_id',
    string $selected = ''
): string {
    $sig = $owner->getSignaturesArray();
    $sigs = array();
    foreach ($sig as $key => $arr) {
        $sigs[$key] = !empty($arr['name']) ? $arr['name'] : '';
    }

    $out = "<select id='{$elementId}' name='{$elementId}'>";
    if (empty($defaultSig)) {
        $out .= get_select_empty_option($defaultSig, false, 'LBL_DEFAULT_EMAIL_SIGNATURES');
    } else {
        $out .= get_select_empty_option($defaultSig, $defaultSig === $selected, 'LBL_DEFAULT_EMAIL_SIGNATURES');
    }
    $out .= get_select_full_options_with_id($sigs, $selected);
    $out .= '</select>';

    return $out;
}
