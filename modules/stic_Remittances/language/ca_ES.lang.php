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
$mod_strings = array (
    'LBL_ASSIGNED_TO_ID' => 'Assignat a (ID)',
    'LBL_ASSIGNED_TO_NAME' => 'Assignat a',
    'LBL_ASSIGNED_TO' => 'Assignat a',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Assignat a',
    'LBL_LIST_ASSIGNED_USER' => 'Assignat a',
    'LBL_CREATED' => 'Creat per',
    'LBL_CREATED_USER' => 'Creat per',
    'LBL_CREATED_ID' => 'Creat per (ID)',
    'LBL_MODIFIED' => 'Modificat per',
    'LBL_MODIFIED_NAME' => 'Modificat per',
    'LBL_MODIFIED_USER' => 'Modificat per',
    'LBL_MODIFIED_ID' => 'Modificat per (ID)',
    'LBL_SECURITYGROUPS' => 'Grups de seguretat',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grups de seguretat',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Data de Creació',
    'LBL_DATE_MODIFIED' => 'Data de Modificació',
    'LBL_DESCRIPTION' => 'Descripció',
    'LBL_DELETED' => 'Suprimit',
    'LBL_NAME' => 'Nom',
    'LBL_LIST_NAME' => 'Nom',
    'LBL_EDIT_BUTTON' => 'Edita',
    'LBL_REMOVE' => 'Desvincula',
    'LBL_ASCENDING' => 'Ascendent',
    'LBL_DESCENDING' => 'Descendent',
    'LBL_OPT_IN' => 'Alta voluntària',
    'LBL_OPT_IN_PENDING_EMAIL_NOT_SENT' => 'Alta voluntària pendent de confirmació. Correu de confirmació no enviat.',
    'LBL_OPT_IN_PENDING_EMAIL_SENT' => 'Alta voluntària pendent de confirmació. Correu de confirmació enviat.',
    'LBL_OPT_IN_CONFIRMED' => 'Alta voluntària confirmada',
    'LBL_LIST_FORM_TITLE' => 'Llista de Remeses',
    'LBL_MODULE_NAME' => 'Remeses',
    'LBL_MODULE_TITLE' => 'Remeses',
    'LBL_HOMEPAGE_TITLE' => 'Les meves Remeses',
    'LNK_NEW_RECORD' => 'Crea una Remesa',
    'LNK_LIST' => 'Mostra les Remeses',
    'LNK_IMPORT_STIC_REMITTANCES' => 'Importa Remeses',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca Remeses',
    'LBL_STIC_REMITTANCES_SUBPANEL_TITLE' => 'Remeses',
    'LBL_NEW_FORM_TITLE' => 'Nova Remesa',
    'LBL_BANK_ACCOUNT' => 'Compte bancari',
    'LBL_LOG' => 'Errors i avisos al fitxer generat',
    'LBL_TYPE' => 'Tipus',
    'LBL_STIC_PAYMENTS_STIC_REMITTANCES_FROM_STIC_PAYMENTS_TITLE' => 'Pagaments',
    'LBL_CHARGE_DATE' => 'Data de càrrec',
    'LBL_DEFAULT_PANEL' => 'Dades generals',
    'LBL_PANEL_RECORD_DETAILS' => 'Detalls del registre',
    'LBL_STATUS' => 'Estat',
    'LBL_ISSUING_ORGANIZATION' => 'Organització emissora',
    'LBL_GENERATE_SEPA_DIRECT_DEBITS_SEPA' => 'Genera la remesa SEPA de rebuts',
    'LBL_GENERATE_SEPA_CREDIT_TRANSFERS' => 'Genera la remesa SEPA de transferències',
    'LBL_PROCESS_REDSYS_CARD_PAYMENTS' => 'Processa els pagaments amb targeta',
    'LBL_NO_BANK_ACCOUNT_ERROR' => 'El compte bancari és obligatori si la remesa és de domiciliacions o de transferències.',
    'LBL_BANK_ACCOUNT_SHOULD_BE_EMPTY_ERROR' => "Si s'indica un compte bancari, la remesa ha de ser de domiciliacions o de transferències.",

    // Missatges SEPA comuns per a rebuts i transferències
    'LBL_SEPA_FIX_REMITTANCE_ERROR' => 'Comproveu',
    'LBL_SEPA_INVALID_ACCOUNT_NAME' => '<b>Organització</b> no vàlida al pagament: ',
    'LBL_SEPA_INVALID_AMOUNT' => '<b>Import </b> no vàlid al pagament (i potser al seu compromís de pagament): ',
    'LBL_SEPA_INVALID_CONTACT_NAME' => '<b>Persona</b> no vàlida al pagament: ',
    'LBL_SEPA_INVALID_IBAN' => '<b>Compte bancari</b> no vàlid al pagament (i potser al seu compromís de pagament): ',
    'LBL_SEPA_INVALID_LOAD_DATE' => "El fitxer no es pot generar perquè la <b>data de càrrec</b> de la remesa és anterior al dia d'avui.",
    'LBL_SEPA_INVALID_MAIN_IBAN' => 'El fitxer no es pot generar perquè el <b>compte bancari</b> de la remesa no és vàlid. Seleccioneu-ne un de vàlid de la llista o creeu-lo.',
    'LBL_SEPA_INVALID_STATUS' => "L'estat del pagament era <i>pagat</i>. Al generar el fitxer passarà a <i>remesat</i>: ",
    'LBL_SEPA_LOG_HEADER_PREFIX_NOT_GENERATED' => 'El fitxer no es pot generar. Abans cal corregir els següents errors:',
    'LBL_SEPA_LOG_HEADER_PREFIX' => 'Darrer fitxer generat:',
    'LBL_SEPA_REMITTANCE_OK' => "No s'ha trobat cap error durant la generació del fitxer de la remesa.",
    'LBL_SEPA_WITHOUT_CONTACT_OR_ACCOUNT' => 'No hi ha cap persona o organització relacionada al pagament: ',
    'LBL_SEPA_XML_HAS_ERRORS' => "El fitxer XML no s'ha generat perquè hi ha errors que cal corregir.",
    'LBL_MISSING_SEPA_VARIABLES' => "Alguns paràmetres de configuració necessaris per a la generació de remeses no tenen valor. Reviseu-los a l'àrea d'administració del CRM abans de continuar:",
    
    // Missatges SEPA per a transferències
    'LBL_SEPA_CREDIT_INVALID_TYPE' => 'El fitxer no es pot generar perquè el seu tipus hauria de ser <b>transferències emeses</b>.',

    // Missatges SEPA per a rebuts
    'LBL_SEPA_DEBIT_INVALID_PAYMENT_COMMITMENT' => 'El pagament no està relacionat amb cap compromís de pagament: ',
    'LBL_SEPA_DEBIT_INVALID_SIGNATURE_DATE' => 'La <b>data de signatura</b> del compromís de pagament està buida: ',
    'LBL_SEPA_DEBIT_INVALID_MANDATE' => 'El <b>mandat</b> del pagament no és vàlid. Està buit, supera els 35 caràcters o conté espais en blanc (comproveu també el compromís de pagament): ',
    'LBL_SEPA_DEBIT_INVALID_NIF' => "El <b>número d'identificació</b> (NIF, NIE...) de la persona/organització està buit: ",
    'LBL_SEPA_DEBIT_INVALID_TYPE' => 'El fitxer no es pot generar perquè el seu tipus hauria de ser <b>rebuts domiciliats</b>.',

    // Missatges SEPA per a devolució de rebuts
    'LBL_SEPA_RETURN_ERR_UPLOADING_FILE' => 'Error: El fitxer seleccionat no es pot carregar. Error número  ',
    'LBL_SEPA_RETURN_ERR_NO_RECEIPT' => 'Error: El fitxer de devolució no conté cap rebut.',
    'LBL_SEPA_RETURN_ERR_OPENING_FILE' => 'Error: El fitxer carregat no es pot obrir.',
    'LBL_SEPA_RETURN_FILE_OK' => "El fitxer de devolució s'ha processat amb èxit.",
    'LBL_SEPA_RETURN_UNPAID_PAYMENT' => 'Pagament marcat com a impagat:',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_1' => "Error: No s'ha trobat el pagament: ID (",
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_2' => '), Deutor (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_3' => '), Import (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_4' => '), Data de devolució (',
    'LBL_SEPA_RETURN_SELECT_FILE' => 'Seleccioneu el fitxer de devolució',
    'LBL_SEPA_LOAD_RETURNS' => 'Carrega devolucions',
    'LBL_SEPA_RETURN_LOAD_FILE' => 'Carregueu les devolucions',

    // Missatges Redsys per a pagaments recurrents
    'LBL_CARD_PAYMENTS_REMITTANCE_INVALID_TYPE' => 'La remesa no es pot processar perquè el seu tipus hauria de ser Targetes.',
    'LBL_CARD_PAYMENTS_TPV_INVALID_MODE' => 'El valor de TPV_TEST ha de ser 0 o 1.',
    'LBL_CARD_PAYMENTS_NONE_SUCCESS' => "Cap pagament no s'ha processat correctament.",
    'LBL_CARD_PAYMENTS_ALL_SUCCESS' => 'Tots els pagaments han estat processats correctament.',
    'LBL_CARD_PAYMENTS_SOME_SUCCESS' => "Alguns pagaments han estat processats correctament i d'altres no. Reviseu el log de la remesa.",
    'LBL_CARD_PAYMENTS_UNKNOWN_ERROR' => 'Hi ha hagut un error desconegut. Reviseu el log de SinergiaCRM.',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_HEADER' => 'Pagaments recurrents inclosos a la remesa:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_SUCCESS' => 'Pagaments processats correctament:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_OMITTED' => 'Pagaments omesos:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_FAILED' => 'Pagaments amb error:',
    'LBL_CARD_PAYMENTS_PAYMENT_INVALID_METHOD' => "S'ha omès el pagament perquè el mitjà de pagament no és targeta.",

    // Altres cadenes
    'LBL_ERROR_QUERY_PAYMENTS_TO_REMITTANCE' => "S'ha produït un error a l'afegir els pagaments a les remeses",
);
