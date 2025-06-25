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
 * Generates an XML file in SEPA format (pain.001.001.03) to communicate to banks a lot of transfers
 *
 * @param Object $remittance Corresponds to the $this object of the corresponding action of the module controller.php file, from which this function is invoked, including the View and the Bean of the remittance
 * @return void
 */
function generateSEPACreditTransfers($remittance)
{
    /**
     * This function generates a remittance of SEPA bank transfers in XML format.
     * Based on the Credit Transfer SEPA library:
     * @author Perry Faro 2015 (http://perryfaro.github.io/sepa/)
     * @license MIT
     */

    require_once 'SticInclude/vendor/php-iban/php-iban.php';
    require_once 'modules/stic_Settings/Utils.php';
    require_once 'SticInclude/Utils.php';
    require_once 'modules/stic_Remittances/Utils.php';

    //Start timestamp to measure performance
    $start = microtime(true);

    global $mod_strings, $timedate, $db, $current_user, $sugar_config;

    // We do some checks on the consignment before continuing:
    // 1) We check that the consignment is the correct type
    $remittance->bean->type != 'transfers' ? SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_SEPA_CREDIT_INVALID_TYPE']) : '';

    // 2) Chek if BANK_ACCOUNT number is according to IBAN
    !verify_iban($remittance->bean->bank_account, true) === true ? SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_SEPA_INVALID_MAIN_IBAN']) : '';

    // 3) We check the charge date
    strtotime(date('Y-m-d')) > strtotime($timedate->swap_formats($remittance->bean->charge_date, $current_user->getPreference('datef'), 'Y-m-d')) ? SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_SEPA_INVALID_LOAD_DATE']) : '';

    // Load SEPA Settings
    $sepaSettingsTemp = stic_SettingsUtils::getSettingsByType('SEPA');

    // Load GENERAL Settings
    $generalSettingsTemp = stic_SettingsUtils::getSettingsByType('GENERAL');

    // Join SEPA & GENERAL Settings
    $directCreditsVars = array_merge($sepaSettingsTemp, $generalSettingsTemp);

    // Get Issuing Organization Suffix
    $orgKey = $remittance->bean->issuing_organization;
    if (!empty($orgKey)) {
        $orgKey = "_".$orgKey;
    }

    // Check empty settings
    $needingSetting = array(
        'SEPA_TRANSFER_DEFAULT_REMITTANCE_INFO'.$orgKey,
        'GENERAL_ORGANIZATION_NAME'.$orgKey,
        'SEPA_TRANSFER_DEBITOR_IDENTIFIER'.$orgKey);
    $missingSettings = array();
    foreach ($needingSetting as $key) {
        if (empty($directCreditsVars[$key])) {
            $missingSettings[] = $key;
        }
    }

    if (count($missingSettings) > 0) {
        SticUtils::showErrorMessagesAndDie($remittance, $mod_strings['LBL_MISSING_SEPA_VARIABLES'] . ' <br>' . join('<br>', $missingSettings));
    }

    // Truncate & clean GENERAL_ORGANIZATION_NAME to 70 characters as allowed
    $directCreditsVars['GENERAL_ORGANIZATION_NAME'.$orgKey] = mb_substr(trim(SticUtils::cleanText($directCreditsVars['GENERAL_ORGANIZATION_NAME'.$orgKey])), 0, 70, 'UTF-8');

    // We start variables to count and add the total payments to be made (and then indicate them in the header)
    $controlSum = 0;
    $controlNumOperations = 0;

    // Include the Credit Transfer library files that are needed at this point
    require_once 'SticInclude/vendor/sepaCredit/CreditTransfer/Payment.php';
    require_once 'SticInclude/vendor/sepaCredit/CreditTransfer/PaymentInformation.php';

    // Initialize Payment information (información del ordenante)
    $paymentInformation = new \Sepa\CreditTransfer\PaymentInformation;

    // We obtain through SQL all payment records associated with the remittance
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
        $contactSQL =
            "SELECT
                c.*
            FROM
                contacts c
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
                                a.*
                            FROM
                                accounts a
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

        $paymentXML = new \Sepa\CreditTransfer\Payment;

        // Do some checks on each record:
        $receptorName = '';
        // 1) That the person / organization exists and has a name / surname

        // If it's a person
        if ($contact) {
            $receptorName = trim($contact['first_name'] . ' ' . $contact['last_name']);
            $contact['first_name'] == '' || $contact['last_name'] == '' ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_CONTACT_NAME'] . " " . stic_RemittancesUtils::goToEdit('Contacts', $contact['id'], $paymentResult['name']) : '';
        }
        // If it's organization
        elseif ($account) {
            $receptorName = trim($account['name']);
            empty($receptorName) ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_ACCOUNT_NAME'] . " " . stic_RemittancesUtils::goToEdit('Accounts', $account['id'], $paymentResult['name']) : '';
        }
        // If there is no person or organization
        else {
            $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_WITHOUT_CONTACT_OR_ACCOUNT'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']);
        }
        // 2) That the amount is valid (positive).
        $paymentResult['amount'] <= 0 ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_AMOUNT'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : null;

        // 3) That the BANK_ACCOUNT is valid.
        !verify_iban($paymentResult['bank_account'], true) === true ? $errorMsg .= '<p class="msg-error">' . $mod_strings['LBL_SEPA_INVALID_IBAN'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : null;

        // 4) That the state is not "paid."
        $paymentResult['status'] == 'paid' ? $warningMsg .= '<p class="msg-warning">' . $mod_strings['LBL_SEPA_INVALID_STATUS'] . " " . stic_RemittancesUtils::goToEdit('stic_Payments', $paymentResult['id'], $paymentResult['name']) : null;

        // Set explicit var to generate/not generate XML in case of fatal errors in any payment
        $generateXML = empty($errorMsg) ? true : false;

        // As long as there have been no fatal errors, we prepare the data for the remittance, but if there are errors we stop doing it because the file will not be generated
        if ($generateXML === true) {
            // We establish the name of the receptor name
            $receptorName = mb_substr(trim(str_replace("&#039;", "", SticUtils::cleanText($receptorName))), 0, 70, 'UTF-8');

            // We establish the concept of the receipt
            if (empty($paymentResult['banking_concept'])) {
                $finalConcept = $directCreditsVars['SEPA_TRANSFER_DEFAULT_REMITTANCE_INFO'.$orgKey];
            } else {
                $finalConcept = $paymentResult['banking_concept'];
            }

            // We enter the payment in the remittance
            $paymentXML->setAmount(number_format($paymentResult['amount'], 2, '.', ''))
                ->setCreditorIBAN(trim($paymentResult['bank_account']))
                ->setCreditorName($receptorName)
                ->setEndToEndId(str_replace('-', '', $paymentResult['id']))
                ->setRemittanceInformation(SticUtils::cleanText(trim(mb_substr($finalConcept, 0, 140))));
            $paymentInformation->addPayments($paymentXML);

            // We update totals of amount and number of operations
            $controlSum += $paymentResult['amount'];
            $controlNumOperations++;
        }
    }

    // If payments contain no fatal errors then generate XML file
    if ($generateXML === true) {

        require_once 'SticInclude/vendor/sepaCredit/CreditTransfer/GroupHeader.php';
        require_once 'SticInclude/vendor/sepaCredit/Builder/Base.php';
        require_once 'SticInclude/vendor/sepaCredit/Builder/CreditTransfer.php';
        require_once 'SticInclude/vendor/sepaCredit/CreditTransfer.php';

        // Group Header
        $groupHeader = new \Sepa\CreditTransfer\GroupHeader();
        $groupHeader->setControlSum(number_format($controlSum, 2, '.', ''))
            ->setInitiatingPartyName($directCreditsVars['GENERAL_ORGANIZATION_NAME'.$orgKey])
            ->setMessageIdentification('SEPACREDIT' . time())
            ->setInitiatingPartyOrgIdOthrId(SticUtils::cleanText($directCreditsVars['SEPA_TRANSFER_DEBITOR_IDENTIFIER'.$orgKey]))
            ->setNumberOfTransactions($controlNumOperations);

        $creditTransfer = new \Sepa\CreditTransfer();
        $creditTransfer->setGroupHeader($groupHeader);

        // We add the total number of operations and the total amount in the header of the consignment
        $paymentInformation
            ->setDebtorIBAN(trim($remittance->bean->bank_account))
            ->setDebtorName($directCreditsVars['GENERAL_ORGANIZATION_NAME'.$orgKey])
            ->setPaymentInformationIdentification(str_replace('-', '', $remittance->bean->id))
            ->setRequestedExecutionDate(date('Y-m-d', strtotime(str_replace('/', '-', $remittance->bean->charge_date))))
            ->setControlSum(number_format($controlSum, 2, '.', ''))
            ->setNumberOfTransactions($controlNumOperations);

        // We update the status of the consignment in the database
        $remittance->bean->status = 'generated';

        $msg = empty($errorMsg) && empty($warningMsg) ? '<p class="msg-success">' . $mod_strings['LBL_SEPA_REMITTANCE_OK'] : $errorMsg . $warningMsg;
        $time_elapsed = round(microtime(true) - $start, 2) . ' s';
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

        $fileName = $remittance->bean->name . '_' . date('YmdHis') . '.xml';
        header('Content-type: text/xml');
        header('Content-Disposition: attachment; filename="' . $fileName . '"');
        header("Content-Type: application/force-download");
        header("Content-type: application/octet-stream");

        // Disable content type sniffing in MSIE
        header("X-Content-Type-Options: nosniff");
        header("Expires: 0");

        // We generate the downloadable file in XML format
        $creditTransfer->setPaymentInformation($paymentInformation);
        $xml = $creditTransfer->xml();
        ob_clean();
        echo $xml;
        flush();

    } else {

        // We update the log of the remittance in the database
        $errorMsg = empty($errorMsg) ? '<p class="msg-success">' . $mod_strings['LBL_SEPA_REMITTANCE_OK'] : $errorMsg . $warningMsg;
        $time_elapsed = round(microtime(true) - $start, 2) . ' s';
        $remittance->bean->log = htmlentities('<p class="info">' . $mod_strings['LBL_SEPA_LOG_HEADER_PREFIX_NOT_GENERATED'] . ' ' . $current_user->name . ' | ' . date('d/m/Y H:i') . ' | ' . $time_elapsed . $errorMsg);
        $remittance->bean->save();

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  ' . $mod_strings['LBL_SEPA_XML_HAS_ERRORS']);
        SugarApplication::appendErrorMessage('<div class="msg-fatal-lock">' . $mod_strings['LBL_SEPA_XML_HAS_ERRORS'] . '</div>');
        SugarApplication::redirect("index.php?module={$remittance->bean->module_dir}&action=DetailView&record={$remittance->bean->id}");
    }

    die();

}
