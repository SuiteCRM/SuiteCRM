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
 * Generates an XML file in SEPA format to communicate to banks a lot of direct debits
 * Based on the SEPA direct debit (SDD) library:
 * Johannes Feichtner <johannes@web-wack.at>
 * http://www.web-wack.at web wack creations
 * http://creativecommons.org/licenses/by-nc/3.0/ CC Attribution-NonCommercial 3.0 license
 *
 *
 * @param Object $remittance Corresponds to the $remittance object of the corresponding action of the module controller.php file,
 * from which this function is invoked, including the View and the Bean of the remittance
 * @return void
 */
function generateSEPADirectDebits($remittance)
{

    require_once 'SticInclude/vendor/php-iban/php-iban.php';
    require_once 'modules/stic_Settings/Utils.php';
    require_once 'SticInclude/Utils.php';
    require_once 'modules/stic_Remittances/Utils.php';

    // We start timestamp to measure performance
    $start = microtime(true);

    global $db, $timedate, $mod_strings, $current_user, $sugar_config;

    // We do some checks on the remittance before continuing:

    // 1) We check that the remittance is the correct type
    $remittance->bean->type != 'direct_debits' ? SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_SEPA_DEBIT_INVALID_TYPE']) : '';

    // 2) We check the bank account
    !verify_iban($remittance->bean->bank_account, true) === true ? SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_SEPA_INVALID_MAIN_IBAN']) : '';

    // 3) We check the charge date
    strtotime(date('Y-m-d')) > strtotime($timedate->swap_formats($remittance->bean->charge_date, $current_user->getPreference('datef'), 'Y-m-d')) ? SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_SEPA_INVALID_LOAD_DATE']) : '';

    require_once 'SticInclude/vendor/sepaDebits/class.SEPADirectDebitTransaction.php';
    require_once 'SticInclude/vendor/sepaDebits/class.SEPAPaymentInfo.php';
    require_once 'SticInclude/vendor/sepaDebits/class.SEPAException.php';
    require_once 'SticInclude/vendor/sepaDebits/class.URLify.php';
    require_once 'SticInclude/vendor/sepaDebits/class.SEPAGroupHeader.php';
    require_once 'SticInclude/vendor/sepaDebits/class.SEPAMessage.php';

    // Load SEPA Settings
    $sepaSettingsTemp = stic_SettingsUtils::getSettingsByType('SEPA');

    // Load GENERAL Settings
    $generalSettingsTemp = stic_SettingsUtils::getSettingsByType('GENERAL');

    // Join SEPA & GENERAL Settings
    $directDebitsVars = array_merge($sepaSettingsTemp, $generalSettingsTemp);

    // Get Issuing Organization Suffix
    $orgKey = $remittance->bean->issuing_organization;
    if (!empty($orgKey)) {
        $orgKey = "_".$orgKey;
    }

    // Check empty settings
    $needingSetting = array(
        'GENERAL_ORGANIZATION_NAME'.$orgKey,
        'SEPA_DEBIT_CREDITOR_IDENTIFIER'.$orgKey,
        'SEPA_DEBIT_DEFAULT_REMITTANCE_INFO'.$orgKey,
    );

    // If so indicated in the CRM configuration, we include the SEPA_BIC_CODE
    if ($directDebitsVars['SEPA_DEBIT_BIC_MODE'.$orgKey] == 1) {
        array_push($needingSetting, "SEPA_BIC_CODE".$orgKey);
    }

    $missingSettings = array();
    foreach ($needingSetting as $key) {
        if (empty($directDebitsVars[$key])) {
            $missingSettings[] = $key;
        }
    }

    if (count($missingSettings) > 0) {
        SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_MISSING_SEPA_VARIABLES'] . ' <br>' . join('<br>', $missingSettings));
    }

   // Truncate GENERAL_ORGANIZATION_NAME to 70 characters as allowed
   $directDebitsVars['GENERAL_ORGANIZATION_NAME'.$orgKey] = substr($directDebitsVars['GENERAL_ORGANIZATION_NAME'.$orgKey],0,70);

    $message = new SEPAMessage('urn:iso:std:iso:20022:tech:xsd:pain.008.001.02');

    // Header of the remittance
    $groupHeader = new SEPAGroupHeader(); // (1..1)
    $groupHeader->setMessageIdentification('SEPA' . time()); // Unique ID for this job
    $groupHeader->setInitiatingPartyId($directDebitsVars['SEPA_DEBIT_CREDITOR_IDENTIFIER'.$orgKey]); // ID of the party sending the job. Usually the creditor
    $groupHeader->setInitiatingPartyName(mb_substr(trim(SticUtils::cleanText($directDebitsVars['GENERAL_ORGANIZATION_NAME'.$orgKey])), 0, 70, 'UTF-8')); // Name of the party sending the job. Usually the creditor
    $message->setGroupHeader($groupHeader);

    $paymentInfo = new SEPAPaymentInfo(); // (1..n)

    // We obtain through SQL all payments associated with the remittance
    $sqlPayments =
        "SELECT
            *
        FROM
            stic_payments_stic_remittances_c pr
        JOIN stic_payments p on
            p.id = pr.stic_payments_stic_remittancesstic_payments_idb
        WHERE
            pr.stic_payments_stic_remittancesstic_remittances_ida = '{$remittance->bean->id}'
            and pr.deleted = 0
            and p.deleted = 0";
    $result = $db->query($sqlPayments);
        
    // Error and warning messages
    $errorMsg = '';
    $warningMsg = '';
    
    // We process the payments obtained and prepare them for the remittance
    while ($paymentResult = $db->fetchByAssoc($result)) {

        // We obtain the data of the person
        $contactSQL = "SELECT
                            c.*,ccstm.*
                        FROM
                            contacts c
                        JOIN contacts_cstm ccstm on
                            ccstm.id_c=c.id
                        JOIN stic_payments_contacts_c pc on
                            c.id = pc.stic_payments_contactscontacts_ida
                        JOIN stic_payments p on
                            p.id = pc.stic_payments_contactsstic_payments_idb
                        WHERE
                            p.id = '{$paymentResult['id']}'
                            and c.deleted = 0
                            and p.deleted = 0
                            and pc.deleted = 0";
        $contact = $db->fetchByAssoc($db->query($contactSQL));

        // If there is no person we obtain the data of the organization
        if ($contact == false) {
            $accountSQL = "SELECT
                                a.*,acstm.*
                            FROM
                                accounts a
                            JOIN accounts_cstm acstm on
                                acstm.id_c=a.id
                            JOIN stic_payments_accounts_c pa on
                                a.id = pa.stic_payments_accountsaccounts_ida
                            JOIN stic_payments p on
                                p.id = pa.stic_payments_accountsstic_payments_idb
                            WHERE
                                p.id = '{$paymentResult['id']}'
                                and a.deleted = 0
                                and p.deleted = 0
                                and pa.deleted = 0";
            $account = $db->fetchByAssoc($db->query($accountSQL));
        }

        // We create the individual receipts
        $transaction = new SEPADirectDebitTransaction();

        // We do some checks on each record:

        // 1) That the person / organization exists and has a name / surname
        $debtorName = '';
        
        // If the debtor is a person
        if ($contact) {
            $debtorName = SticUtils::cleanText(trim($contact['first_name'] . ' ' . $contact['last_name']));
            $contact['first_name'] == '' || $contact['last_name'] == '' ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_CONTACT_NAME'] . " " . stic_RemittancesUtils::goToEdit('Contacts', $contact['id'], $paymentResult['name']) : '';
            trim($contact['stic_identification_number_c']) == '' ? $warningMsg .= '<p class="msg-warning">' . $mod_strings['LBL_SEPA_DEBIT_INVALID_NIF'] . " " . stic_RemittancesUtils::goToEdit('Contacts', $contact['id'], $paymentResult['name']) : '';
        }
        // If it's organization
        elseif ($account) {
            $debtorName = SticUtils::cleanText($account['name']);
            trim($debtorName) == "" ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_ACCOUNT_NAME'] . " " . stic_RemittancesUtils::goToEdit('Accounts', $account['id'], $paymentResult['name']) : '';
            trim($account['stic_identification_number_c']) == "" ? $warningMsg .= '<p class="msg-warning">' . $mod_strings['LBL_SEPA_DEBIT_INVALID_NIF'] . " " . stic_RemittancesUtils::goToEdit('Accounts', $account['id'], $paymentResult['name']) : '';
        }

        // If there is no person or organization
        else {
            $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_WITHOUT_CONTACT_OR_ACCOUNT'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']);
        }

        // 2) That the amount is valid (positive)
        $paymentResult['amount'] <= 0 ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_AMOUNT'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : '';

        // 3) That the IBAN is valid.
        !verify_iban($paymentResult['bank_account'], true) === true ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_IBAN'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : '';

        // 4) That the mandate is set and is valid
        empty($paymentResult['mandate']) || $paymentResult['mandate'] == '' || strlen($paymentResult['mandate']) > 35 || strpos($paymentResult['mandate'], ' ') !== false ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_DEBIT_INVALID_MANDATE'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : '';

        // 5) That the related Payment Commitment exists
        $sqlPC = "SELECT
                                fp.id,
                                fp.signature_date
                            FROM
                                stic_payment_commitments fp
                            JOIN stic_payments_stic_payment_commitments_c fpp on
                                fp.id = fpp.stic_paymebfe2itments_ida
                            WHERE
                                fpp.stic_payments_stic_payment_commitmentsstic_payments_idb = '{$paymentResult['id']}'
                                and fp.deleted = 0
                                and fpp.deleted = 0";

        $PCRow = $db->fetchOne($sqlPC);
        empty($PCRow['id']) ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_DEBIT_INVALID_PAYMENT_COMMITMENT'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : '';

        // 6) That the date of signature of the mandate exists (and PC exists)
        $signatureDate = $PCRow['signature_date'] ?? null;
        !empty($PCRow['id']) && (empty($signatureDate) || $signatureDate == '') ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_DEBIT_INVALID_SIGNATURE_DATE'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : '';

        // 7) That the status is not "paid"
        $paymentResult['status'] == 'paid' ? $warningMsg .= '<p class="msg-warning">' . $mod_strings['LBL_SEPA_INVALID_STATUS'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : null;

        // Set explicit var to generate/not generate XML in case of fatal errors in any payment
        $generateXML = empty($errorMsg) ? true : false;

        // While there have been no fatal errors, we prepare the data for the remittance, but if there are errors we stop doing it because the file will not be generated
        if ($generateXML === true) {
            // We establish the name of the debtor
            $transaction->setDebtorName(mb_substr(trim($debtorName), 0, 70, 'UTF-8'));

            // We establish the debtor's IBAN
            $debtorBankAccount = $paymentResult['bank_account'];
            $transaction->setDebtorIBAN(trim($debtorBankAccount));

            // We establish an id for the receipt based on the internal id of the payment. Necessary to manage possible returns.
            // We remove the hyphens from the internal id to not exceed 35 characters (maximum limit according to SEPA).
            $transaction->setEndToEndIdentification(str_replace('-', '', $paymentResult['id']));

            // We set the amount
            $transaction->setInstructedAmount($paymentResult['amount']);

            // We set the mandate
            $transaction->setMandateIdentification(trim($paymentResult['mandate']));

            // We establish the date of signature of the mandate
            $transaction->setDateOfSignature($signatureDate);

            // We establish the concept of the receipt
            if (empty($paymentResult['banking_concept'])) {
                $finalConcept = $directDebitsVars['SEPA_DEBIT_DEFAULT_REMITTANCE_INFO'.$orgKey];
            } else {
                $finalConcept = $paymentResult['banking_concept'];
            }
            $finalConcept = SticUtils::cleanText(substr($finalConcept, 0, 139));
            $transaction->setRemittanceInformation($finalConcept);

            // We add the receipt to the remittance
            $paymentInfo->addTransaction($transaction);
        }
    }

    // If payments contain no fatal errors then generate XML file
    if ($generateXML === true) {

        // We load the remittance
        if ($paymentInfo->getNumberOfTransactions() > 0) {
            $message->addPaymentInfo($paymentInfo);
        }

        // We update the status of the consignment in the database
        $remittance->bean->status = 'generated';

        $msg = empty($errorMsg) && empty($warningMsg) ? '<p class="msg-success">' . $mod_strings['LBL_SEPA_REMITTANCE_OK'] : $errorMsg . $warningMsg;
        $time_elapsed = round(microtime(true) - $start, 2) . ' s.';
        $remittance->bean->log = '<p class="info">' . $mod_strings['LBL_SEPA_LOG_HEADER_PREFIX'] . ' ' . $current_user->name . ' | ' . date('d/m/Y H:i') . ' | ' . $time_elapsed . $msg;
        $remittance->bean->save();

        // Set 'remitted' status in all payments included in the remittance
        $db->query("UPDATE
                        stic_payments sp
                    JOIN stic_payments_stic_remittances_c spsrc ON
                        sp.id = spsrc.stic_payments_stic_remittancesstic_payments_idb
                    SET
                        sp.status = 'remitted'
                    WHERE
                        spsrc.stic_payments_stic_remittancesstic_remittances_ida = '{$remittance->bean->id}'
                        AND spsrc.deleted = 0;"
        );

        // Remittance data
        $paymentInfo->setPaymentInformationIdentification(str_replace('-', '', $remittance->bean->id)); // Your own unique identifier for this batch
        $paymentInfo->setBatchBooking('false');
        $paymentInfo->setLocalInstrumentCode('CORE'); // Other options: COR1, B2B
        $paymentInfo->setSequenceType('RCUR');
        $paymentInfo->setCreditorSchemeIdentification($directDebitsVars['SEPA_DEBIT_CREDITOR_IDENTIFIER'.$orgKey]);
        $paymentInfo->setRequestedCollectionDate(date('Y-m-d', strtotime(str_replace('/', '-', $remittance->bean->charge_date)))); //Fecha de cargo del fichero

        // Creditor data (organization that issues receipts)
        $paymentInfo->setCreditorName(mb_substr(trim($directDebitsVars['GENERAL_ORGANIZATION_NAME'.$orgKey]), 0, 70, 'UTF-8'));
        $paymentInfo->setCreditorAccountIBAN($remittance->bean->bank_account);

        // If so indicated in the CRM configuration, we include the BIC
        if ($directDebitsVars['SEPA_DEBIT_BIC_MODE'.$orgKey] == 1) {
            $paymentInfo->setCreditorAgentBIC($directDebitsVars['SEPA_BIC_CODE'.$orgKey]);
        }

        // We generate the downloadable file in XML format
        header('Content-type: text/xml');
        header('Content-Disposition: attachment; filename="' . $remittance->bean->name . "_" . date('YmdHis') . '.xml"');
        ob_clean();
        flush();
        echo $message->printXML();

    } else {

        $errorMsg = empty($errorMsg) ? '<p class="msg-error">' . $mod_strings['LBL_SEPA_REMITTANCE_OK'] : $errorMsg . $warningMsg;
        $time_elapsed = round(microtime(true) - $start, 2) . ' s.';
        $remittance->bean->log = '<p class="msg-error"><b>' . $mod_strings['LBL_SEPA_LOG_HEADER_PREFIX_NOT_GENERATED'] . ' ' . $current_user->name . ' | ' . date('d/m/Y H:i') . ' | ' . $time_elapsed . '</b>' . $errorMsg;
        $remittance->bean->save();

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  ' . $mod_strings['LBL_SEPA_XML_HAS_ERRORS']);
        SugarApplication::appendErrorMessage('<div class="msg-fatal-lock">' . $mod_strings['LBL_SEPA_XML_HAS_ERRORS'] . '</div>');
        SugarApplication::redirect("index.php?module={$remittance->bean->module_dir}&action=DetailView&record={$remittance->bean->id}");

    }

    die();

}
