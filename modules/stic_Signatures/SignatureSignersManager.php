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
 * Business logic class for managing signers in a signature process.
 *
 * Encapsulates the logic for adding signers to a signature, validating inputs,
 * avoiding duplicates, creating stic_Signers beans, establishing relationships,
 * and logging actions. Designed to be reusable from any context (HTTP, workflows, AWF).
 */
class SignatureSignersManager
{
    /**
     * Add signers to a signature based on the given record IDs.
     *
     * @param string $signatureId  ID of the stic_Signatures record.
     * @param string $moduleName   Module name of the source records.
     * @param array  $recordIds    Array of source record IDs.
     * @param string $currentUserId  ID of the user performing the action.
     * @return array  Result with keys: success (bool), ok (int), ko (int), errors (array).
     */
    public static function addSignersToSignature($signatureId, $moduleName, array $recordIds, $currentUserId = null)
    {
        $result = [
            'success' => false,
            'ok' => 0,
            'ko' => 0,
            'errors' => [],
        ];

        if (empty($signatureId)) {
            $result['errors'][] = 'Signature ID is required.';
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Signature ID is empty.');
            return $result;
        }

        if (empty($recordIds)) {
            $result['errors'][] = 'No records selected.';
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': No record IDs provided.');
            return $result;
        }

        $bean = BeanFactory::getBean($moduleName);
        if (!$bean) {
            $result['errors'][] = "Invalid module: {$moduleName}";
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid module: {$moduleName}");
            return $result;
        }

        require_once 'modules/stic_Signatures/Utils.php';

        $stic_SignatureBean = BeanFactory::getBean('stic_Signatures', $signatureId);
        if (!$stic_SignatureBean) {
            $result['errors'][] = "Signature not found: {$signatureId}";
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Signature bean not found for ID: {$signatureId}");
            return $result;
        }

        $destSigners = stic_SignaturesUtils::getSignatureSigners($signatureId, $recordIds);

        $existingSigners = self::getExistingSignerIds($signatureId);

        $okCounter = 0;
        $koCounter = 0;

        foreach ($destSigners as $destSignerId => $destSigner) {
            $destSignerBean = BeanFactory::getBean($destSigner['module'], $destSignerId);
            if (!$destSignerBean) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Could not obtain signer data for ID: " . $destSignerId);
                $koCounter++;
                continue;
            }

            if (in_array($destSignerId, $existingSigners)) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Skipping existing signer with ID: " . $destSignerId);
                $koCounter++;
                continue;
            }

            $stic_SignerBean = BeanFactory::newBean('stic_Signers');
            $stic_SignerBean->name = "{$destSignerBean->full_name} - {$stic_SignatureBean->name}";
            $stic_SignerBean->assigned_user_id = $currentUserId ?: $stic_SignatureBean->assigned_user_id;
            $stic_SignerBean->created_by = $currentUserId ?: $stic_SignatureBean->assigned_user_id;
            $stic_SignerBean->parent_type = $destSigner['module'];
            $stic_SignerBean->parent_id = $destSignerId;
            $stic_SignerBean->parent_name = $destSignerBean->full_name;
            $stic_SignerBean->record_id = $destSigner['sourceId'];
            $stic_SignerBean->record_type = $destSigner['sourceModule'];
            $stic_SignerBean->record_name = $destSigner['sourceName'];
            $stic_SignerBean->email_address = $destSigner['email'];
            $stic_SignerBean->phone = $destSigner['phone'];
            $stic_SignerBean->status = 'pending';
            $stic_SignerBean->contact_id_c = $destSigner['onBehalfOfId'] != $destSignerId ? $destSigner['onBehalfOfId'] : null;

            $stic_SignerBean->save();
            if (!empty($stic_SignerBean->id)) {
                require_once 'modules/stic_Signature_Log/Utils.php';
                stic_SignatureLogUtils::logSignatureAction('ADD_SIGNER_TO_SIGNATURE', $stic_SignerBean->id, 'SIGNER', $stic_SignatureBean->name);
                stic_SignatureLogUtils::logSignatureAction('ADD_SIGNER_TO_SIGNATURE', $stic_SignatureBean->id, 'SIGNATURE', $stic_SignerBean->name);
            }

            $stic_SignatureBean->load_relationship('stic_signatures_stic_signers');
            $stic_SignatureBean->stic_signatures_stic_signers->add($stic_SignerBean->id);
            $okCounter++;
        }

        $result['ok'] = $okCounter;
        $result['ko'] = $koCounter;
        $result['success'] = $okCounter > 0 || $koCounter === 0;

        return $result;
    }

    /**
     * Build record IDs from mass update criteria.
     *
     * @param string $moduleName  Module name.
     * @param string $currentPost Serialized mass update POST data.
     * @return array  Array of record IDs.
     */
    public static function getRecordIdsFromMassUpdate($moduleName, $currentPost)
    {
        $recordIds = [];
        $order_by = '';
        require_once 'include/MassUpdate.php';
        $mass = new MassUpdate();
        $mass->generateSearchWhere($moduleName, $currentPost);
        $ret_array = create_export_query_relate_link_patch($moduleName, $mass->searchFields, $mass->where_clauses);

        $bean = BeanFactory::getBean($moduleName);
        if (!$bean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid module: {$moduleName}");
            return $recordIds;
        }

        $query = $bean->create_export_query($order_by, $ret_array['where'], $ret_array['join']);
        $result = DBManagerFactory::getInstance()->query($query, true);
        while ($val = DBManagerFactory::getInstance()->fetchByAssoc($result, false)) {
            $recordIds[] = $val['id'];
        }

        return $recordIds;
    }

    /**
     * Retrieve existing signer IDs for a given signature.
     *
     * @param string $signatureId
     * @return array
     */
    protected static function getExistingSignerIds($signatureId)
    {
        $SQL = "SELECT ss.parent_id as id
                FROM stic_signatures s
                JOIN stic_signatures_stic_signers_c ssssc ON s.id = ssssc.stic_signatures_stic_signersstic_signatures_ida AND ssssc.deleted = 0
                JOIN stic_signers ss ON ss.id = ssssc.stic_signatures_stic_signersstic_signers_idb AND ss.deleted = 0
                WHERE s.deleted = 0
                AND s.id = '{$signatureId}'";
        $result = DBManagerFactory::getInstance()->query($SQL, true);
        $existingSigners = [];
        while ($row = DBManagerFactory::getInstance()->fetchByAssoc($result, false)) {
            $existingSigners[] = $row['id'];
        }
        return $existingSigners;
    }
}
