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
    'LBL_ASSIGNED_TO_ID' => 'Asignado a',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ASSIGNED_TO' => 'Asignado a',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_LIST_ASSIGNED_USER' => 'Asignado a',
    'LBL_CREATED' => 'Creado por',
    'LBL_CREATED_USER' => 'Creado por',
    'LBL_CREATED_ID' => 'Creado por (ID)',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado por',
    'LBL_MODIFIED_USER' => 'Modificado por',
    'LBL_MODIFIED_ID' => 'Modificado por (ID)',
    'LBL_SECURITYGROUPS' => 'Grupos de seguridade',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de seguridade',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Data de Creación',
    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_DESCRIPTION' => 'Descrición',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nome',
    'LBL_LIST_NAME' => 'Nome',
    'LBL_EDIT_BUTTON' => 'Editar',
    'LBL_REMOVE' => 'Desvincular',
    'LBL_ASCENDING' => 'Ascendente',
    'LBL_DESCENDING' => 'Descendente',
    'LBL_OPT_IN' => 'Alta voluntaria',
    'LBL_OPT_IN_PENDING_EMAIL_NOT_SENT' => 'Alta voluntaria pendente de confirmación. Correo de confirmación non enviado.',
    'LBL_OPT_IN_PENDING_EMAIL_SENT' => 'Alta voluntaria pendente de confirmación. Correo de confirmación enviado.',
    'LBL_OPT_IN_CONFIRMED' => 'Alta voluntaria confirmada',
    'LBL_LIST_FORM_TITLE' => 'Lista de Remesas',
    'LBL_MODULE_NAME' => 'Remesas',
    'LBL_MODULE_TITLE' => 'Remesas',
    'LBL_HOMEPAGE_TITLE' => 'As miñas Remesas',
    'LNK_NEW_RECORD' => 'Crear Remesa',
    'LNK_LIST' => 'Ver Remesas',
    'LNK_IMPORT_STIC_REMITTANCES' => 'Importar Remesas',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Remesas',
    'LBL_STIC_REMITTANCES_SUBPANEL_TITLE' => 'Remesas',
    'LBL_NEW_FORM_TITLE' => 'Nova Remesa',
    'LBL_BANK_ACCOUNT' => 'Conta bancaria',
    'LBL_LOG' => 'Erros e advertencias no arquivo xerado',
    'LBL_TYPE' => 'Tipo',
    'LBL_ISSUING_ORGANIZATION' => 'Organización emisora',
    'LBL_STIC_PAYMENTS_STIC_REMITTANCES_FROM_STIC_PAYMENTS_TITLE' => 'Pagos',
    'LBL_CHARGE_DATE' => 'Data de cargo',
    'LBL_DEFAULT_PANEL' => 'Datos xerais',
    'LBL_PANEL_RECORD_DETAILS' => 'Detalles do rexistro',
    'LBL_STATUS' => 'Estado',
    'LBL_GENERATE_SEPA_DIRECT_DEBITS_SEPA' => 'Xerar remesa SEPA de recibos',
    'LBL_GENERATE_SEPA_CREDIT_TRANSFERS' => 'Xerar remesa SEPA de transferencias',
    'LBL_PROCESS_REDSYS_CARD_PAYMENTS' => 'Procesar pagos con tarxeta',
    'LBL_NO_BANK_ACCOUNT_ERROR' => 'A conta bancaria é obrigatoria se a remesa é de domiciliacións ou de transferencias.',
    'LBL_BANK_ACCOUNT_SHOULD_BE_EMPTY_ERROR' => 'Se se indica unha conta bancaria, a remesa debe ser de domiciliacións ou de transferencias.',

    // Mensaxes SEPA comunes para recibos e transferencias
    'LBL_SEPA_FIX_REMITTANCE_ERROR' => 'Comprobe',
    'LBL_SEPA_INVALID_ACCOUNT_NAME' => '<b>Organización</b> non válida no pago: ',
    'LBL_SEPA_INVALID_AMOUNT' => '<b>Importe</b> non válido no pago (e tal vez no seu compromiso de pago): ',
    'LBL_SEPA_INVALID_CONTACT_NAME' => '<b>Persoa</b> non válida no pago: ',
    'LBL_SEPA_INVALID_IBAN' => '<b>Conta bancaria</b> non válida no pago (e tal vez no seu compromiso de pago): ',
    'LBL_SEPA_INVALID_LOAD_DATE' => 'O arquivo non se pode xerar porque a <b>data de cargo</b> da remesa é anterior ao día de hoxe.',
    'LBL_SEPA_INVALID_MAIN_IBAN' => 'O arquivo non se pode xerar porque a <b>conta bancaria</b> da remesa non é válida. Seleccione unha conta válida da lista ou créea.',
    'LBL_SEPA_INVALID_STATUS' => 'O estado do pago era <i>pagado</i>. Ao xerar o ficheiro pasará a <i>remesado</i>: ',
    'LBL_SEPA_LOG_HEADER_PREFIX_NOT_GENERATED' => 'O ficheiro non se pode xerar sen corrixir previamente os seguintes erros:',
    'LBL_SEPA_LOG_HEADER_PREFIX' => 'Último arquivo xerado:',
    'LBL_SEPA_REMITTANCE_OK' => 'Non se encontraron erros ao xerar o ficheiro da remesa.',
    'LBL_SEPA_WITHOUT_CONTACT_OR_ACCOUNT' => 'Non hai ningunha persoa ou organización relacionada no pago: ',
    'LBL_SEPA_XML_HAS_ERRORS' => 'O arquivo XML non se xerou porque existen erros que deben ser corrigidos.',
    'LBL_MISSING_SEPA_VARIABLES' => 'Algúns parámetros de configuración necesarios para a xeración de remesas están baleiros. Revíseos na área de administración do CRM antes de continuar:',

    // Mensaxes SEPA para transferencias
    'LBL_SEPA_CREDIT_INVALID_TYPE' => 'O ficheiro non se pode xerar porque o seu tipo debería ser <b>transferencias emitidas</b>.',

    // Mensaxes SEPA para recibos
    'LBL_SEPA_DEBIT_INVALID_PAYMENT_COMMITMENT' => 'O pago non está relacionado con ningún compromiso de pago: ',
    'LBL_SEPA_DEBIT_INVALID_SIGNATURE_DATE' => 'A <b>data de sinatura</b> do compromiso de pago está baleira: ',
    'LBL_SEPA_DEBIT_INVALID_MANDATE' => 'O <b>mandato</b> do pago non é válido. Está baleiro, supera os 35 caracteres ou contén espazos en branco (verifique tamén o compromiso de pago): ',
    'LBL_SEPA_DEBIT_INVALID_NIF' => 'O <b>número de identificación</b> (NIF, NIE...) da persoa/organización está baleiro: ',
    'LBL_SEPA_DEBIT_INVALID_TYPE' => 'O ficheiro non se pode xerar porque o seu tipo debería ser <b>recibos domiciliados</b>.',

    // Mensaxes SEPA para devolución de recibos
    'LBL_SEPA_RETURN_ERR_UPLOADING_FILE' => 'Erro: o ficheiro seleccionado non se pode cargar. Erro número ',
    'LBL_SEPA_RETURN_ERR_NO_RECEIPT' => 'Erro: o ficheiro de devolución non contén ningún recibo.',
    'LBL_SEPA_RETURN_ERR_OPENING_FILE' => 'Erro: o ficheiro cargado non se pode abrir.',
    'LBL_SEPA_RETURN_FILE_OK' => 'O ficheiro de devolución foi procesado con éxito.',
    'LBL_SEPA_RETURN_UNPAID_PAYMENT' => 'Pago marcado como impagado: ',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_1' => 'Erro: non se atopou o pago: ID (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_2' => '), Debedor (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_3' => '), Importe (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_4' => '), Data de devolución (',
    'LBL_SEPA_RETURN_SELECT_FILE' => 'Seleccione o ficheiro de devolución',
    'LBL_SEPA_LOAD_RETURNS' => 'Cargar devolucións',
    'LBL_SEPA_RETURN_LOAD_FILE' => 'Cargue as devolucións',

    // Mensajes Redsys para pagos recurrentes
    'LBL_CARD_PAYMENTS_REMITTANCE_INVALID_TYPE' => 'A remesa non se pode procesar porque o seu tipo debería ser Tarxetas.',
    'LBL_CARD_PAYMENTS_TPV_INVALID_MODE' => 'O valor de TPV_TEST debe ser 0 ou 1.',
    'LBL_CARD_PAYMENTS_NONE_SUCCESS' => 'Ningún pago se procesou con éxito.',
    'LBL_CARD_PAYMENTS_ALL_SUCCESS' => 'Todos os pagos foron procesados con éxito.',
    'LBL_CARD_PAYMENTS_SOME_SUCCESS' => 'Algúns pagos procesáronse con éxito e outros non. Revise o log da remesa.',
    'LBL_CARD_PAYMENTS_UNKNOWN_ERROR' => 'Produciuse un erro descoñecido. Revise o log de SinergiaCRM.',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_HEADER' => 'Pagos recurrentes con tarxeta incluidos na remesa:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_SUCCESS' => 'Pagos procesados con éxito:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_OMITTED' => 'Pagos omitidos:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_FAILED' => 'Pagos con erro:',
    'LBL_CARD_PAYMENTS_PAYMENT_INVALID_METHOD' => 'Omitiuse o pago porque o medio pago non é tarxeta.',

    // Outras cadeas
    'LBL_ERROR_QUERY_PAYMENTS_TO_REMITTANCE' => 'Erro ao agregar pagos a remesas',
);
