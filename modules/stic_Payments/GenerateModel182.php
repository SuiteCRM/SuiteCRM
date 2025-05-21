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

// Load the necessary files for M182
require_once 'modules/stic_Settings/Utils.php';
require_once 'SticInclude/Utils.php';

// Relate the province codes with the autonomous community codes
$provinciasComunidades = array(
    '04' => '01',
    '11' => '01',
    '14' => '01',
    '18' => '01',
    '21' => '01',
    '23' => '01',
    '29' => '01',
    '41' => '01',
    '22' => '02',
    '44' => '02',
    '50' => '02',
    '33' => '03',
    '39' => '06',
    '02' => '07',
    '13' => '07',
    '16' => '07',
    '19' => '07',
    '45' => '07',
    '05' => '08',
    '09' => '08',
    '24' => '08',
    '34' => '08',
    '37' => '08',
    '40' => '08',
    '42' => '08',
    '47' => '08',
    '49' => '08',
    '08' => '09',
    '17' => '09',
    '25' => '09',
    '43' => '09',
    '03' => '17',
    '12' => '17',
    '46' => '17',
    '06' => '10',
    '10' => '10',
    '15' => '11',
    '27' => '11',
    '36' => '11',
    '32' => '11',
    '07' => '04',
    '35' => '05',
    '38' => '05',
    '26' => '16',
    '28' => '12',
    '30' => '13',
);

// Generates the M182 type 1 record (declarant)
function model182T1($data) {

    $text = '1182';
    $text .= $data['ejercicio'];
    $text .= SticUtils::fillLeft($data['nif_declarante'], 0, 9);
    $text .= SticUtils::fillRigth($data['declarante'], " ", 40);
    $text .= $data['tipo_soporte'];
    $text .= SticUtils::fillLeft($data['relacionarse_telefono'], " ", 9);
    $text .= SticUtils::fillRigth(trim(trim(trim($data['relacionarse_apellido_1']) . " " . trim($data['relacionarse_apellido_2'])) . " " . trim($data['relacionarse_nombre'])), " ", 40);
    $text .= SticUtils::fillLeft($data['num_justificante'], 0, 13);
    $text .= SticUtils::fillLeft($data['decl_complementaria'], " ", 1);
    $text .= SticUtils::fillLeft($data['decl_sustitutiva'], " ", 1);
    $text .= SticUtils::fillLeft($data['num_justificante_anterior'], 0, 13);
    $text .= SticUtils::fillLeft($data['num_total_registros_declarados'], 0, 9);
    $text .= SticUtils::fillLeft(convertAmount($data['importe_donacion']), 0, 15);
    $text .= SticUtils::fillLeft($data['naturaleza_decl'], " ", 1);
    $text .= ($data['nif_patrimonio_protegido'] == '' ? SticUtils::fillLeft($data['nif_patrimonio_protegido'], " ", 9) : SticUtils::fillLeft($data['nif_patrimonio_protegido'], 0, 9));
    $text .= SticUtils::fillRigth($data['patrimonio_protegido_apellido_1'] . " " . $data['patrimonio_protegido_apellido_2'] . " " . $data['patrimonio_protegido_nombre'], " ", 40);
    $text .= SticUtils::fillLeft("", " ", 41);
    $text .= "\r\n";

    return strtoupper($text);
}

// Generates the M182 type 2 records (declared)
function model182T2($data) {

    $text = '2182';
    $text .= $data['ejercicio'];
    $text .= SticUtils::fillLeft($data['nif_declarante'], 0, 9);
    // If a declared person reaches this point without an associated NIF/NIE, it means that is a "non-resident".
    // In this case the field "NIF of declared person" must be filled with blank spaces instead of zeros.
    $text .= SticUtils::fillLeft($data['nif_declarado'], $data['nif_declarado'] == '' ? ' ' : 0, 9);
    $text .= ($data['nif_representante_legal'] == '' ? SticUtils::fillLeft($data['nif_representante_legal'], " ", 9) : SticUtils::fillLeft($data['nif_representante_legal'], 0, 9));
    $text .= SticUtils::fillRigth(trim($data['nombre_fiscal']) ? trim($data['nombre_fiscal']) : (trim(trim(trim($data['declarado_apellido_1']) . " " . trim($data['declarado_apellido_2'])) . " " . trim($data['declarado_nombre']))), " ", 40);
    $text .= SticUtils::fillLeft($data['declarado_provincia'], 0, 2);
    $text .= SticUtils::fillLeft($data['clave'], " ", 1);
    $text .= SticUtils::fillLeft(convertAmount($data['por_deduccion']), 0, 5);
    $text .= SticUtils::fillLeft(convertAmount($data['importe_donacion']), 0, 13);
    $text .= SticUtils::fillLeft($data['kind'], " ", 1);
    $text .= SticUtils::fillLeft($data['deduccion_com_autonoma'], 0, 2);
    $text .= SticUtils::fillLeft(convertAmount($data['por_deduccion_com_autonoma']), 0, 5);
    $text .= SticUtils::fillLeft($data['naturaleza_declarado'], " ", 1);
    $text .= ' 0000';
    $text .= SticUtils::fillLeft("", " ", 21);
    $text .= SticUtils::fillLeft($data['recurrencia'], 0, 1);
    $text .= SticUtils::fillLeft("", " ", 118);
    $text .= "\r\n";

    return strtoupper($text);
}

function convertAmount($amount) {
    return number_format($amount, 2, "", "");
}

global $db;

// Collect the selected 'payment types' variables
$paymentTypeArray = array();
$paymentTypeArray = $_REQUEST['payment_type'];

// If no payment type has been selected, redirect the user to the selection screen, showing an error message. 
// Although this is redundant, since javascript validation has been included, it will provide extra security.
if ($paymentTypeArray == '') {
    SugarApplication::appendErrorMessage('<div class="msg-fatal-lock"> Es necesario seleccionar un tipo de pago</div>');
    SugarApplication::redirect("index.php?module=stic_Payments&action=model182Wizard");
    sugar_die('');
}
$paymentTypes = "p.payment_type = '" . implode("' OR p.payment_type = '", $paymentTypeArray) . "'";

// Initialize the variables with the years to be analyzed
$lastyear = date("Y") - 1;   // Year for which we are presenting the M182
$twoYearsAgo = date("Y") - 2;   // Year before $lastyear
$threeYearsAgo = date("Y") - 3;   // Year before $twoYearsAgo
$fourYearsAgo = date("Y") - 4; // Year before $threeYearsAgo

// Load M182 settings
$m182SettingsTemp = stic_SettingsUtils::getSettingsByType('M182');

// Load GENERAL settings
$generalSettingsTemp = stic_SettingsUtils::getSettingsByType('GENERAL');

// Set other settings to be used in code
$m182FixedValuesTemp = array(
    'M182_PORCENTAJE_DEDUCCION' => 80,
    'M182_PORCENTAJE_DEDUCCION_CUOTAS_PARTIDOS' => 20,
    'M182_PORCENTAJE_DEDUCCION_EXCESO_NO_RECURRENTE' => 40,
    'M182_PORCENTAJE_DEDUCCION_EXCESO_RECURRENTE' => 45,
    'M182_PORCENTAJE_DEDUCCION_PERSONAS_JURIDICAS' => 40,
    'M182_PORCENTAJE_DEDUCCION_PERSONAS_JURIDICAS_RECURRENTE' => 50,
    'M182_LIMITE_DEDUCCION' => 250,
);

$m182Vars = array_merge($m182SettingsTemp, $generalSettingsTemp, $m182FixedValuesTemp);

$declarantType = $m182Vars["M182_NATURALEZA_DECLARANTE"];

// 1.5 Reset the fields 'stic_total_annual_donations_c' and 'stic_182_error_c' in accounts and contacts to delate previous data (from the same year or from previous ones).
$db->query("UPDATE accounts_cstm SET stic_total_annual_donations_c = 0, stic_182_error_c = 0");
$db->query("UPDATE contacts_cstm SET stic_total_annual_donations_c = 0, stic_182_error_c = 0");

// 2. Select all payments with payment_date in the last year, with "paid" status, that are of the selected types and that are not excluded
$sqlCurrentPayments = "SELECT p.id
                FROM stic_payments p
                WHERE p.status = 'paid'
                AND YEAR(p.payment_date) = '" . $lastyear . "'
                AND (" . $paymentTypes . ")
                AND (p.m182_excluded = 0 OR p.m182_excluded IS NULL)
                AND p.deleted = 0";
$currentPaymentResult = $db->query($sqlCurrentPayments);
$paymentsIds = [];
while ($row = $db->fetchByAssoc($currentPaymentResult)) {
    $paymentsIds[] = $row['id'];
}
$GLOBALS['log']->info('[M182] ' . (is_array($paymentsIds) ? sizeof($paymentsIds) : 0) . ' payments will be processed.');

// 3. Creating 3 arrays:
// - one with all the contacts that have payments in the previous selection.
// - one with all the accounts that have payments in the previous selection.
// - one with the amounts accumulated by each donor (account or contact) in the exercise of the declaration (according to the selected payments).
// The contacts and accounts arrays will be filled later with the historical payment data of each contact and account.
// The arrays indexes are the ids of accounts and contacts.
$contacts = array();
$accounts = array();
$yearPayments = array();

// Process the selected payments
foreach ($paymentsIds as $id) {

    // For each payment get the associated contact/account
    $accountRow = null;
    $contactRow = null;

    // Get the contact (must be non excluded)
    $contactSQL =
        "SELECT
            c.id as contact_id,
            amount,
            payment_type
         FROM stic_payments p
            INNER JOIN stic_payments_contacts_c pc ON p.id = pc.stic_payments_contactsstic_payments_idb
            INNER JOIN contacts c ON pc.stic_payments_contactscontacts_ida = c.id
            INNER JOIN contacts_cstm cc ON c.id = cc.id_c
         WHERE c.deleted = 0
            AND (cc.stic_182_excluded_c = 0 OR cc.stic_182_excluded_c IS NULL)
            AND pc.deleted = 0
            AND p.id = '" . $id . "'";
    $contactResult = $db->query($contactSQL);
    if ($db->getRowCount($contactResult) > 0) {
        $contactRow = $db->fetchByAssoc($contactResult);
    } else {
        // If there is no contact, get the account (must be non excluded)
        $accountSQL =
            "SELECT
                a.id as accountId,
                amount,
                payment_type
            FROM stic_payments p
                INNER JOIN stic_payments_accounts_c pa ON p.id = pa.stic_payments_accountsstic_payments_idb
                INNER JOIN accounts a ON pa.stic_payments_accountsaccounts_ida = a.id
                INNER JOIN accounts_cstm ac ON a.id = ac.id_c
            WHERE a.deleted = 0
                AND (ac.stic_182_excluded_c = 0 OR ac.stic_182_excluded_c IS NULL)
                AND pa.deleted = 0
                AND p.id = '" . $id . "'";
        $accountResult = $db->query($accountSQL);
        if ($db->getRowCount($accountResult) > 0) {
            $accountRow = $db->fetchByAssoc($accountResult);
        }
    }
    // Store the contacts in an array, the accounts in another one and the total amounts in a third one, classified by type of payment.
    if (isset($contactRow)) {
        $contacts[$contactRow['contact_id']] = $contactRow['contact_id'];
        if (!isset($yearPayments[$contactRow['contact_id']])) {
            $yearPayments[$contactRow['contact_id']] = [];
        }
        $yearPayments[$contactRow['contact_id']][$contactRow['payment_type']] = ($yearPayments[$contactRow['contact_id']][$contactRow['payment_type']] ?? 0) + $contactRow['amount'];
    } elseif (isset($accountRow)) {
        $accounts[$accountRow['accountId']] = $accountRow['accountId'];
        if (!isset($yearPayments[$accountRow['accountId']])) {
            $yearPayments[$accountRow['accountId']] = [];
        }
        $yearPayments[$accountRow['accountId']][$accountRow['payment_type']] = ($yearPayments[$accountRow['accountId']][$accountRow['payment_type']] ?? 0) + $accountRow['amount'];
    }
}

// 4. Calculate the payment history for each contact/account and store it in the previously created arrays.

// 4.1. Contacts
foreach ($contacts as $id) {

    $historicalPayments = array();
    $historicalPayments['id'] = $id;

    // 4.1.1 Get contributions by type of payment for the three years prior to the declaration
    $row = null;
    $paymentsSQL =
        "SELECT
            YEAR(p.payment_date) AS periodo,
            p.payment_type,
            SUM(p.amount) as total_donation
        FROM stic_payments p
            INNER JOIN stic_payments_contacts_c pc ON p.id = pc.stic_payments_contactsstic_payments_idb
            INNER JOIN contacts c ON pc.stic_payments_contactscontacts_ida = c.id
        WHERE  c.id = '" . $id . "'
            AND YEAR(p.payment_date) >= '" . $fourYearsAgo . "' AND YEAR(p.payment_date) <= '" . $twoYearsAgo . "'
            AND p.status = 'paid'
            AND (" . $paymentTypes . ")
            AND (p.m182_excluded = 0 OR p.m182_excluded IS NULL)
            AND p.deleted = 0
            AND c.deleted = 0
            AND pc.deleted = 0
        GROUP BY YEAR(p.payment_date), p.payment_type";

    $paymentsResult = $db->query($paymentsSQL);
    while ($row = $db->fetchByAssoc($paymentsResult)) {
        // We keep the total amount of donations for each exercise and type of payment
        $historicalPayments[$row['periodo']][$row['payment_type']] = $row['total_donation'];
        
        // We accumulate totals separately for kind and monetary donations
        if ($row['payment_type'] === 'kind') {
            $historicalPayments[$row['periodo']]['kind_total'] = $row['total_donation'];
        } else if (!($declarantType == '4' && $row['payment_type'] == 'fee')) {
            // For monetary donations, exclude fees in case of political parties
            $historicalPayments[$row['periodo']]['monetary_total'] += $row['total_donation'];
        }

        // Calculate total (kind + monetary)
        $historicalPayments[$row['periodo']]['total'] = 
            $historicalPayments[$row['periodo']]['kind_total'] + 
            $historicalPayments[$row['periodo']]['monetary_total'];
    }

    // 4.1.2 Add to the array the payments of the year of the declaration
    // Initialize totals for last year
    $historicalPayments[$lastyear]['kind_total'] = 0;
    $historicalPayments[$lastyear]['monetary_total'] = 0;
    $historicalPayments[$lastyear]['total'] = 0;

    // Process each payment type
    foreach ($yearPayments[$id] as $paymentType => $paymentTypeValue) {
        // Save individual payment value
        $historicalPayments[$lastyear][$paymentType] = $paymentTypeValue;
        
        // Update totals based on payment type
        if ($paymentType === 'kind') {
            $historicalPayments[$lastyear]['kind_total'] = $paymentTypeValue;
        } else if (!($declarantType == '4' && $paymentType == 'fee')) {
            $historicalPayments[$lastyear]['monetary_total'] += $paymentTypeValue;
        }
    }

    // Calculate total after processing all payments
    $historicalPayments[$lastyear]['total'] = 
        $historicalPayments[$lastyear]['kind_total'] + 
        $historicalPayments[$lastyear]['monetary_total'];

    // 4.1.3 Check if the contact can be considered a recurring donor according to the regulation
    $recurrenceKind = false;
    $recurrenceMonetary = false;

    // Check kind donations recurrence
    if (($historicalPayments[$lastyear]['kind_total'] ?? 0) > 0
    && ($historicalPayments[$twoYearsAgo]['kind_total'] ?? 0) >= ($historicalPayments[$threeYearsAgo]['kind_total'] ?? 0) 
    && ($historicalPayments[$threeYearsAgo]['kind_total'] ?? 0) >= ($historicalPayments[$fourYearsAgo]['kind_total'] ?? 0) 
    && ($historicalPayments[$fourYearsAgo]['kind_total'] ?? 0) > 0) {
    $recurrenceKind = true;
    }

    // Check monetary donations recurrence
    if (($historicalPayments[$lastyear]['monetary_total'] ?? 0) > 0
    && ($historicalPayments[$twoYearsAgo]['monetary_total'] ?? 0) >= ($historicalPayments[$threeYearsAgo]['monetary_total'] ?? 0) 
    && ($historicalPayments[$threeYearsAgo]['monetary_total'] ??0) >= ($historicalPayments[$fourYearsAgo]['monetary_total'] ?? 0) 
    && ($historicalPayments[$fourYearsAgo]['monetary_total'] ?? 0) > 0) {
    $recurrenceMonetary = true;
    }

    $historicalPayments['recurrente_kind'] = $recurrenceKind;
    $historicalPayments['recurrente_monetary'] = $recurrenceMonetary;
        
    // 4.1.4 Save the data obtained for the contact
    $contacts[$id] = $historicalPayments;

    $GLOBALS['log']->debug('[M182] Contact processed: [id] = ' . $historicalPayments['id'] . '; [' . $fourYearsAgo . '] = ' . ($historicalPayments[$fourYearsAgo]['total'] ?? 0) . '; [' . $threeYearsAgo . '] = ' . ($historicalPayments[$threeYearsAgo]['total'] ?? 0) . '; [' . $twoYearsAgo . '] = ' . ($historicalPayments[$twoYearsAgo]['total'] ?? 0) . '; [' . $lastyear . '] = ' . ($historicalPayments[$lastyear]['total'] ?? 0) . '; [recurrente] = ' . ($historicalPayments['recurrente'] ?? 'No') . ';');
}

// 4.2 Accounts
foreach ($accounts as $id) {

    $historicalPayments = array();
    $historicalPayments['id'] = $id;

    // 4.2.1 Get contributions by type of payment for the three years prior to the declaration
    $row = null;
    $paymentsSQL =
        "SELECT
            YEAR(p.payment_date) AS periodo,
            p.payment_type,
            SUM(p.amount) as total_donation
        FROM stic_payments p
            INNER JOIN stic_payments_accounts_c pa ON p.id = pa.stic_payments_accountsstic_payments_idb
            INNER JOIN accounts a ON pa.stic_payments_accountsaccounts_ida = a.id
        WHERE  a.id = '" . $id . "'
            AND YEAR(p.payment_date) >= '" . $fourYearsAgo . "' AND YEAR(p.payment_date) <= '" . $twoYearsAgo . "'
            AND p.status = 'paid'
            AND (" . $paymentTypes . ")
            AND (p.m182_excluded = 0 OR p.m182_excluded IS NULL)
            AND p.deleted = 0
            AND a.deleted = 0
            AND pa.deleted = 0
        GROUP BY YEAR (p.payment_date), p.payment_type";
    $paymentsResult = $db->query($paymentsSQL);
    while ($row = $db->fetchByAssoc($paymentsResult)) {
        // We keep the total amount of donations for each exercise and type of payment
        $historicalPayments[$row['periodo']][$row['payment_type']] = $row['total_donation'];
        // We accumulate the total total per year to assess whether it is a recurring donor.
        $historicalPayments[$row['periodo']]['total'] += $row['total_donation'];
    }

    // 4.2.2 Add to the array the payments of the year of the declaration, previously calculated
    foreach ($yearPayments[$id] as $claveTipoPago => $paymentTypeValue) {
        $historicalPayments[$lastyear][$claveTipoPago] = $paymentTypeValue;
        $historicalPayments[$lastyear]['total'] += $paymentTypeValue;
    }

    // 4.2.3 Check if the account can be considered a recurring donor according to the regulation
    $recurrence = false;
    if ($historicalPayments[$lastyear]['total'] > 0
        && $historicalPayments[$twoYearsAgo]['total'] >= $historicalPayments[$threeYearsAgo]['total'] 
        && $historicalPayments[$threeYearsAgo]['total'] >= $historicalPayments[$fourYearsAgo]['total'] 
        && $historicalPayments[$fourYearsAgo]['total'] > 0) {
        $recurrence = true;
    }
    $historicalPayments['recurrente'] = $recurrence;
    

    // 4.2.4 Save the data obtained for the account
    $accounts[$id] = $historicalPayments;

    $GLOBALS['log']->debug('[M182] Account processed: [id] = ' . $historicalPayments['id'] . '; [' . $fourYearsAgo . '] = ' . $historicalPayments[$fourYearsAgo]['total'] . '; [' . $threeYearsAgo . '] = ' . $historicalPayments[$threeYearsAgo]['total'] . '; [' . $twoYearsAgo . '] = ' . $historicalPayments[$twoYearsAgo]['total'] . '; [' . $lastyear . '] = ' . $historicalPayments[$lastyear]['total'] . '; [recurrente] = ' . ($historicalPayments['recurrente'] ? 'Sí' : 'No') . ';');
}

// 5. M182 generation
$declarantIdentification = $m182Vars["GENERAL_ORGANIZATION_ID"];
$donationKey = $m182Vars["M182_CLAVE_DONATIVO"];
$year = $lastyear;

// Create an array to save records formatted according to the regulations
$model182T2 = array();

// 5.1 Contacts
foreach ($contacts as $id) {

    $contactRow = null;
    $contactSQL = "SELECT * FROM contacts c LEFT JOIN contacts_cstm cc ON c.id = cc.id_c WHERE c.id = '" . $id['id'] . "' AND c.deleted = 0";
    $contactResult = $db->query($contactSQL);
    $contactRow = $db->fetchByAssoc($contactResult);


    // Check if the contact is valid for M182 purposes. If not, mark it as wrong and exclude it from the M182.
    // Causes of exclusion:
    // - General: Missing the combination "name + surname" or the province.
    // - Residents in Spain: The identification type is set and is not NIF/NIE or the identification number is empty.
    // - Non-residents (province = 99): There are no specific causes of exclusion, the M182 admits declared non-residents even without an identification number.
    if ((trim($contactRow['first_name'] . " " . $contactRow['last_name']) == '' || $contactRow['primary_address_state'] == '') || 
        ($contactRow['primary_address_state'] != 99 && (
            ($contactRow['stic_identification_type_c'] != '' && $contactRow['stic_identification_type_c'] != 'nie' && $contactRow['stic_identification_type_c'] != 'nif') 
            || trim($contactRow['stic_identification_number_c']) == ''
            )
        )
    )
    {
        $db->query("UPDATE contacts_cstm SET stic_182_error_c = 1 WHERE id_c = '" . $id['id'] . "'");
        $GLOBALS['log']->fatal('[M182] Contact with errors: ' . $id['id']);
    } else {
        // The contact has all the necessary data, so include it in the M182
        
        // Clean identification number (just in case...)
        $contactRow['stic_identification_number_c'] = SticUtils::cleanNIF($contactRow['stic_identification_number_c']);

        $m182 = array(); // The array will contain the individual record formatted according to the regulations
        $m182['ejercicio'] = $year;
        $m182['nif_declarante'] = $declarantIdentification;
        // Before adding the identification number to the array, check that it is a valid NIF/NIE (a declared "non-resident" may have 
        // an identification number that is not a NIF/NIE. If so, the value should not be included in the array as it would make the file to fail later).
        $m182['nif_declarado'] = SticUtils::isValidNIForNIE($contactRow['stic_identification_number_c']) ? $contactRow['stic_identification_number_c'] : '';
        $m182['nif_representante_legal'] = '';
        $m182['nombre_fiscal'] = $contactRow['stic_tax_name_c'];
        $m182['declarado_apellido_1'] = $contactRow['last_name'];
        $m182['declarado_apellido_2'] = '';
        $m182['declarado_nombre'] = $contactRow['first_name'];
        $m182['declarado_provincia'] = $contactRow['primary_address_state'];
        $m182['naturaleza_declarado'] = 'F';

        switch ($declarantType) {

        case '4': // Political parties

            if ($id[$year]['fee'] > 0) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_CUOTAS_PARTIDOS"];
                $m182['clave'] = 'F';
                $m182['importe_donacion'] = $id[$year]['fee'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = ' ';

                // Recurrence mark
                $m182['recurrencia'] = '0'; // Membership fees paid to political parties are not considered donations for recurrence purposes

                // If applicable, set the percentage of autonomous deduction (not applicable to political parties)
                $m182['deduccion_com_autonoma'] = 0;
                $m182['por_deduccion_com_autonoma'] = 0;

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            if ($id[$year]['donation'] > 0) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION"];
                $m182['clave'] = 'G';
                $m182['importe_donacion'] = $id[$year]['donation'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = ' ';

                // Calculation of the percentage of deduction based on the amount and recurrence of donations
                if ($id[$year]['donation'] + $id[$year]['kind'] > $m182Vars['M182_LIMITE_DEDUCCION']) {
                    if ($id['recurrente']) {
                        $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_EXCESO_RECURRENTE"];
                    } else {
                        $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_EXCESO_NO_RECURRENTE"];
                    }
                }

                // Recurrence mark
                $m182['recurrencia'] = ($id['recurrente'] ? '1' : '2');

                // If applicable, set the percentage of autonomous deduction (not applicable to political parties)
                $m182['deduccion_com_autonoma'] = 0;
                $m182['por_deduccion_com_autonoma'] = 0;

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            if ($id[$year]['kind'] > 0) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION"];
                $m182['clave'] = 'G';
                $m182['importe_donacion'] = $id[$year]['kind'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = 'X';

                // Calculation of the percentage of deduction based on the amount and recurrence of donations
                if ($id[$year]['donation'] + $id[$year]['kind'] > $m182Vars['M182_LIMITE_DEDUCCION']) {
                    if ($id['recurrente']) {
                        $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_EXCESO_RECURRENTE"];
                    } else {
                        $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_EXCESO_NO_RECURRENTE"];
                    }
                }

                // Recurrence mark
                $m182['recurrencia'] = ($id['recurrente'] ? '1' : '2');

                // If applicable, set the percentage of autonomous deduction (not applicable to political parties)
                $m182['deduccion_com_autonoma'] = 0;
                $m182['por_deduccion_com_autonoma'] = 0;

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            break;

        case '1': // Organizations related to law 49/2002

            if ($id[$year]['monetary_total'] > 0) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION"];
                $m182['clave'] = $donationKey;
                $m182['importe_donacion'] = $id[$year]['total'] - $id[$year]['kind'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = ' ';

                // Calculation of the percentage of deduction based on the amount and recurrence of donations
                if ($m182['importe_donacion'] > $m182Vars['M182_LIMITE_DEDUCCION']) {
                    if ($id['recurrente_monetary']) {
                        $m182['por_deduccion'] = $m182Vars['M182_PORCENTAJE_DEDUCCION_EXCESO_RECURRENTE'];
                    } else {
                        $m182['por_deduccion'] = $m182Vars['M182_PORCENTAJE_DEDUCCION_EXCESO_NO_RECURRENTE'];
                    }
                }

                // Recurrence mark
                $m182['recurrencia'] = ($id['recurrente_monetary'] ? '1' : '2');

                // If applicable, set the percentage of autonomous deduction
                if (isset($provinciasComunidades[$m182['declarado_provincia']]) && isset($m182Vars["M182_PORCENTAJE_DEDUCCION_AUTONOMICA_" . $provinciasComunidades[$m182['declarado_provincia']]])) {
                    $m182['deduccion_com_autonoma'] = $provinciasComunidades[$m182['declarado_provincia']];
                    $m182['por_deduccion_com_autonoma'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_AUTONOMICA_" . $provinciasComunidades[$m182['declarado_provincia']]];
                } else {
                    $m182['deduccion_com_autonoma'] = 0;
                    $m182['por_deduccion_com_autonoma'] = 0;
                }

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            if ($id[$year]['kind_total'] > 0) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION"];
                $m182['clave'] = $donationKey;
                $m182['importe_donacion'] = $id[$year]['kind'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = 'X';

                // Calculation of the percentage of deduction based on the amount and recurrence of donations
                if ($m182['importe_donacion'] > $m182Vars['M182_LIMITE_DEDUCCION']) {
                    if ($id['recurrente']) {
                        $m182['por_deduccion'] = $m182Vars['M182_PORCENTAJE_DEDUCCION_EXCESO_RECURRENTE'];
                    } else {
                        $m182['por_deduccion'] = $m182Vars['M182_PORCENTAJE_DEDUCCION_EXCESO_NO_RECURRENTE'];
                    }
                }

                // Recurrence mark
                $m182['recurrencia'] = ($id['recurrente_kind'] ? '1' : '2');

                // If applicable, set the percentage of autonomous deduction
                if (isset($provinciasComunidades[$m182['declarado_provincia']]) && isset($m182Vars["M182_PORCENTAJE_DEDUCCION_AUTONOMICA_" . $provinciasComunidades[$m182['declarado_provincia']]])) {
                    $m182['deduccion_com_autonoma'] = $provinciasComunidades[$m182['declarado_provincia']];
                    $m182['por_deduccion_com_autonoma'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_AUTONOMICA_" . $provinciasComunidades[$m182['declarado_provincia']]];
                } else {
                    $m182['deduccion_com_autonoma'] = 0;
                    $m182['por_deduccion_com_autonoma'] = 0;
                }

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            break;
        }

        // Update the total donation in the contact record
        // Important! Membership fees to political parties are excluded from this total. If political parties 
        // want to generate deduction certificates for both fees and donations, this field only covers the latest.
        $db->query("UPDATE contacts_cstm SET stic_total_annual_donations_c = " . $id[$year]['total'] . " WHERE id_c = '" . $id['id'] . "'");

    }
}

// 5.2. Accounts
foreach ($accounts as $id) {

    $accountRow = null;
    $accountSQL = "SELECT * FROM accounts a LEFT JOIN accounts_cstm ac ON a.id = ac.id_c  WHERE  a.id = '" . $id['id'] . "' AND a.deleted = 0";
    $accountResult = $db->query($accountSQL);
    $accountRow = $db->fetchByAssoc($accountResult);

    // Check if the account is valid for M182 purposes. If not, mark it as wrong and exclude it from the M182.
    if (trim($accountRow['stic_identification_number_c']) == '' or trim($accountRow['name']) == '' or $accountRow['billing_address_state'] == '') {

        $db->query("UPDATE accounts_cstm SET stic_182_error_c = 1 WHERE id_c = '" . $id['id'] . "'");
        $GLOBALS['log']->fatal('[M182] Account with errors: ' . $id['id']);

    } else {
        // The account has all the necessary data, so include it in the M182

        // Importatnt! By law, political parties cannot get donations from organizations, 
        // so this section will only apply to organizations related to law 49/2002 (declarant type is 1).

        $m182 = array(); // The array will contain the individual record formatted according to the regulations

        $m182['ejercicio'] = $year;
        $m182['nif_declarante'] = $declarantIdentification;
        $m182['nif_declarado'] = $accountRow['stic_identification_number_c'];
        $m182['nif_representante_legal'] = "";
        $m182['nombre_fiscal'] = $accountRow['stic_tax_name_c'];
        $m182['declarado_apellido_1'] = "";
        $m182['declarado_apellido_2'] = "";
        $m182['declarado_nombre'] = $accountRow['name'];
        $m182['declarado_provincia'] = $accountRow['billing_address_state'];
        $m182['naturaleza_declarado'] = 'J';

        switch ($declarantType) {

        case '1': // Organizations related to law 49/2002

            if ($id[$year]['kind'] != $id[$year]['total']) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION"];
                $m182['clave'] = $donationKey;
                $m182['importe_donacion'] = $id[$year]['total'] - $id[$year]['kind'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = ' ';

                // Calculation of the percentage of deduction based on the recurrence of donations
                if ($id['recurrente']) {
                    $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_PERSONAS_JURIDICAS_RECURRENTE"];
                } else {
                    $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_PERSONAS_JURIDICAS"];
                }

                // Recurrence mark
                $m182['recurrencia'] = ($id['recurrente'] ? '1' : '2');

                // There is no regional deduction for organizations
                $m182['deduccion_com_autonoma'] = 0;
                $m182['por_deduccion_com_autonoma'] = 0;

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            if ($id[$year]['kind'] > 0) {

                $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION"];
                $m182['clave'] = $donationKey;
                $m182['importe_donacion'] = $id[$year]['kind'];
                $total += $m182['importe_donacion'];
                $m182['kind'] = 'X';

                // Calculation of the percentage of deduction based on the recurrence of donations
                if ($id['recurrente']) {
                    $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_PERSONAS_JURIDICAS_RECURRENTE"];
                } else {
                    $m182['por_deduccion'] = $m182Vars["M182_PORCENTAJE_DEDUCCION_PERSONAS_JURIDICAS"];
                }

                // Recurrence mark
                $m182['recurrencia'] = ($id['recurrente'] ? '1' : '2');

                // There is no regional deduction for organizations
                $m182['deduccion_com_autonoma'] = 0;
                $m182['por_deduccion_com_autonoma'] = 0;

                // Add the formatted record to the general array
                $model182T2[] = $m182;

            }

            break;
        }

        // Update the total donation in the account record
        $db->query("UPDATE accounts_cstm SET stic_total_annual_donations_c = " . $id[$year]['total'] . " WHERE id_c = '" . $id['id'] . "'");

    }
}

// 5.3. Header record (declarant)
$m182 = array();
$m182['ejercicio'] = $year;
$m182['nif_declarante'] = $declarantIdentification;
$m182['declarante'] = $m182Vars["GENERAL_ORGANIZATION_NAME"];
$m182['tipo_soporte'] = 'T';
$m182['relacionarse_telefono'] = $m182Vars["M182_PERSONA_CONTACTO_TELEFONO"];
$m182['relacionarse_apellido_1'] = $m182Vars["M182_PERSONA_CONTACTO_APELLIDO_1"];
$m182['relacionarse_apellido_2'] = $m182Vars["M182_PERSONA_CONTACTO_APELLIDO_2"];
$m182['relacionarse_nombre'] = $m182Vars["M182_PERSONA_CONTACTO_NOMBRE"];
$m182['num_justificante'] = $m182Vars["M182_NUMERO_JUSTIFICANTE"];
$m182['decl_complementaria'] = ' ';
$m182['decl_sustitutiva'] = ' ';
$m182['num_justificante_anterior'] = '';
$m182['num_total_registros_declarados'] = count($model182T2);
$m182['importe_donacion'] = $total ?? null;
$m182['naturaleza_decl'] = $m182Vars["M182_NATURALEZA_DECLARANTE"];
$m182['nif_patrimonio_protegido'] = '';
$m182['patrimonio_protegido_apellido_1'] = '';
$m182['patrimonio_protegido_apellido_2'] = '';
$m182['patrimonio_protegido_nombre'] = '';
$linea1 = model182T1($m182);

// 5.4. Creation of the file to download
header("Content-Type: application/force-download");
header("Content-type: application/octet-stream");
header("Content-Disposition: attachment; filename=\"modelo_182_" . $m182['ejercicio'] . ".txt\";");
// disable content type sniffing in MSIE
header("X-Content-Type-Options: nosniff");
header("Expires: 0");

ob_clean();
flush();
echo $linea1; // Header record (declarant)
foreach ($model182T2 as $linea) {
    echo model182T2($linea); // Declared records
}
die();
