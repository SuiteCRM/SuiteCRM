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
$mod_strings = array(
    'LBL_ASSIGNED_TO_ID' => 'Assigned to (ID)',
    'LBL_ASSIGNED_TO_NAME' => 'Assigned to',
    'LBL_ASSIGNED_TO' => 'Assigned to',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Assigned to',
    'LBL_LIST_ASSIGNED_USER' => 'Assigned to',
    'LBL_CREATED' => 'Created By',
    'LBL_CREATED_USER' => 'Created By',
    'LBL_CREATED_ID' => 'Created By (ID)',
    'LBL_MODIFIED' => 'Modified By',
    'LBL_MODIFIED_NAME' => 'Modified By',
    'LBL_MODIFIED_USER' => 'Modified By',
    'LBL_MODIFIED_ID' => 'Modified By (ID)',
    'LBL_SECURITYGROUPS' => 'Security Groups',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Security Groups',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Date Created',
    'LBL_DATE_MODIFIED' => 'Date Modified',
    'LBL_DESCRIPTION' => 'Description',
    'LBL_DELETED' => 'Deleted',
    'LBL_NAME' => 'Name',
    'LBL_LIST_NAME' => 'Name',
    'LBL_EDIT_BUTTON' => 'Edit',
    'LBL_REMOVE' => 'Remove',
    'LBL_ASCENDING' => 'Ascending',
    'LBL_DESCENDING' => 'Descending',
    'LBL_OPT_IN' => 'Opt In',
    'LBL_OPT_IN_PENDING_EMAIL_NOT_SENT' => 'Pending Confirm opt in, Confirm opt in not sent',
    'LBL_OPT_IN_PENDING_EMAIL_SENT' => 'Pending Confirm opt in, Confirm opt in sent',
    'LBL_OPT_IN_CONFIRMED' => 'Opted in',
    'LBL_LIST_FORM_TITLE' => 'Remittances List',
    'LBL_MODULE_NAME' => 'Remittances',
    'LBL_MODULE_TITLE' => 'Remittances',
    'LBL_HOMEPAGE_TITLE' => 'My Remittances',
    'LNK_NEW_RECORD' => 'Create Remittances',
    'LNK_LIST' => 'View Remittances',
    'LNK_IMPORT_STIC_REMITTANCES' => 'Import Remittances',
    'LBL_SEARCH_FORM_TITLE' => 'Search Remittances',
    'LBL_STIC_REMITTANCES_SUBPANEL_TITLE' => 'Remittances',
    'LBL_NEW_FORM_TITLE' => 'New Remittances',
    'LBL_STIC_PAYMENTS_STIC_REMITTANCES_FROM_STIC_PAYMENTS_TITLE' => 'Payments',
    'LBL_BANK_ACCOUNT' => 'Bank account',
    'LBL_LOG' => 'Errors and warnings in the generated file',
    'LBL_TYPE' => 'Type',
    'LBL_CHARGE_DATE' => 'Charge date',
    'LBL_DEFAULT_PANEL' => 'Overview',
    'LBL_PANEL_RECORD_DETAILS' => 'Record details',
    'LBL_STATUS' => 'Status',
    'LBL_ISSUING_ORGANIZATION' => 'Issuing organization',
    'LBL_GENERATE_SEPA_DIRECT_DEBITS_SEPA' => 'Generate SEPA Direct Debits remittance',
    'LBL_GENERATE_SEPA_CREDIT_TRANSFERS' => 'Generate SEPA Credit Transfers remittance',
    'LBL_PROCESS_REDSYS_CARD_PAYMENTS' => 'Process card payments',
    'LBL_NO_BANK_ACCOUNT_ERROR' => 'Bank account is required in a remittance of direct debits or transfers.',
    'LBL_BANK_ACCOUNT_SHOULD_BE_EMPTY_ERROR' => 'If a bank account is set the remittance must be of direct debits or transfers.',

    // Common SEPA messages for direct debits and transfers
    'LBL_SEPA_FIX_REMITTANCE_ERROR' => 'Check',
    'LBL_SEPA_INVALID_ACCOUNT_NAME' => 'Invalid <b>account name</b> in payment:  ',
    'LBL_SEPA_INVALID_AMOUNT' => 'Invalid <b>amount</b> in payment (and maybe in its payment commitment): ',
    'LBL_SEPA_INVALID_CONTACT_NAME' => 'Invalid <b>contact name</b> in payment: ',
    'LBL_SEPA_INVALID_IBAN' => 'Invalid <b>bank account</b> in payment (and maybe in its payment commitment): ',
    'LBL_SEPA_INVALID_LOAD_DATE' => "File won't be generated because remittance <b>charge date</b> is prior to current date.",
    'LBL_SEPA_INVALID_MAIN_IBAN' => "File won't be generated because remittance <b>bank account</b> is invalid. Select a valid one from the list or create it.",
    'LBL_SEPA_INVALID_STATUS' => 'The payment <b>status</b> was <i>paid</i>. After generating the file it will be set as <i>remitted</i>: ',
    'LBL_SEPA_LOG_HEADER_PREFIX_NOT_GENERATED' => "The file won't be generated due to the following errors:",
    'LBL_SEPA_LOG_HEADER_PREFIX' => 'Last generated file:',
    'LBL_SEPA_REMITTANCE_OK' => 'No errors found while generating remittance file.',
    'LBL_SEPA_WITHOUT_CONTACT_OR_ACCOUNT' => 'No related contact or account in payment: ',
    'LBL_SEPA_XML_HAS_ERRORS' => 'The XML file has not been generated because there are errors that should be corrected.',
    'LBL_MISSING_SEPA_VARIABLES' => 'Some settings needed for remittances generation are empty. Please check next settings in admin area: ',

    // SEPA transfers messages
    'LBL_SEPA_CREDIT_INVALID_TYPE' => "File won't be generated because its type should be <b>credit transfers</b>.",

    // SEPA direct debits messages
    'LBL_SEPA_DEBIT_INVALID_PAYMENT_COMMITMENT' => 'The payment is not related to any payment commitment: ',
    'LBL_SEPA_DEBIT_INVALID_SIGNATURE_DATE' => 'The <b>date of signature</b> of the payment commitment is empty: ',
    'LBL_SEPA_DEBIT_INVALID_MANDATE' => 'The payment <b>mandate</b> is invalid. It is empty, exceeds 35 characters or contains white spaces (check the payment commitment too): ',
    'LBL_SEPA_DEBIT_INVALID_NIF' => 'The contact/account <b>identification number</b> is empty:  ',
    'LBL_SEPA_DEBIT_INVALID_TYPE' => "File won't be generated because its type should be <b>direct debits</b>.",

    // SEPA direct debits returns messages
    'LBL_SEPA_RETURN_ERR_UPLOADING_FILE' => "Error: Can't upload the selected file. Error number ",
    'LBL_SEPA_RETURN_ERR_NO_RECEIPT' => 'Error: Return file does not contain any direct debit.',
    'LBL_SEPA_RETURN_ERR_OPENING_FILE' => "Error: Uploaded return file can't be opened.",
    'LBL_SEPA_RETURN_FILE_OK' => 'Return file has been succesfully processed.',
    'LBL_SEPA_RETURN_UNPAID_PAYMENT' => 'Payment set to unpaid: ',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_1' => 'Error: Direct debit not found: ID(',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_2' => '), Debitor(',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_3' => '), Amount(',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_4' => '), Return date(',
    'LBL_SEPA_RETURN_SELECT_FILE' => 'Select a direct debit returns file',
    'LBL_SEPA_LOAD_RETURNS' => 'Load returned direct debits',
    'LBL_SEPA_RETURN_LOAD_FILE' => 'Load returns file',

    // Redsys recurring payments messages
    'LBL_CARD_PAYMENTS_REMITTANCE_INVALID_TYPE' => 'Remittance will not be processed be generated because its type should be Cards.',
    'LBL_CARD_PAYMENTS_TPV_INVALID_MODE' => 'TPV_TEST value must be 0 or 1.',
    'LBL_CARD_PAYMENTS_NONE_SUCCESS' => 'No payment has been successfully processed.',
    'LBL_CARD_PAYMENTS_ALL_SUCCESS' => 'All payments have been successfully processed.',
    'LBL_CARD_PAYMENTS_SOME_SUCCESS' => 'Some payments have been successfully processed, some not. Please, check the log of the remittance.',
    'LBL_CARD_PAYMENTS_UNKNOWN_ERROR' => 'An unknown error occurred. Please, check SinergiaCRM log.',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_HEADER' => 'Recurring card payments included in the remittance:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_SUCCESS' => 'Successfully processed payments:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_OMITTED' => 'Omitted payments:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_FAILED' => 'Failed payments:',
    'LBL_CARD_PAYMENTS_PAYMENT_INVALID_METHOD' => 'The payment has been omitted because the payment method is not card.',

    // Other strings
    'LBL_ERROR_QUERY_PAYMENTS_TO_REMITTANCE' => 'Error when adding payments to remittance',
);
