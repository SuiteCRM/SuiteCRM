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
    'LBL_ASSIGNED_TO_ID' => 'Asignado a (ID)',
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
    'LBL_SECURITYGROUPS' => 'Grupos de seguridad',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de seguridad',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_DATE_MODIFIED' => 'Fecha de Modificación',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nombre',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_EDIT_BUTTON' => 'Editar',
    'LBL_REMOVE' => 'Desvincular',
    'LBL_ASCENDING' => 'Ascendente',
    'LBL_DESCENDING' => 'Descendente',
    'LBL_OPT_IN' => 'Alta voluntaria',
    'LBL_OPT_IN_PENDING_EMAIL_NOT_SENT' => 'Alta voluntaria pendiente de confirmación. Correo de confirmación no enviado.',
    'LBL_OPT_IN_PENDING_EMAIL_SENT' => 'Alta voluntaria pendiente de confirmación. Correo de confirmación enviado.',
    'LBL_OPT_IN_CONFIRMED' => 'Alta voluntaria confirmada',
    'LBL_LIST_FORM_TITLE' => 'Lista de Remesas',
    'LBL_MODULE_NAME' => 'Remesas',
    'LBL_MODULE_TITLE' => 'Remesas',
    'LBL_HOMEPAGE_TITLE' => 'Mis Remesas',
    'LNK_NEW_RECORD' => 'Crear Remesa',
    'LNK_LIST' => 'Ver Remesas',
    'LNK_IMPORT_STIC_REMITTANCES' => 'Importar Remesas',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Remesas',
    'LBL_STIC_REMITTANCES_SUBPANEL_TITLE' => 'Remesas',
    'LBL_NEW_FORM_TITLE' => 'Nueva Remesa',
    'LBL_BANK_ACCOUNT' => 'Cuenta bancaria',
    'LBL_LOG' => 'Errores y advertencias en el archivo generado',
    'LBL_TYPE' => 'Tipo',
    'LBL_ISSUING_ORGANIZATION' => 'Organización emisora',
    'LBL_STIC_PAYMENTS_STIC_REMITTANCES_FROM_STIC_PAYMENTS_TITLE' => 'Pagos',
    'LBL_CHARGE_DATE' => 'Fecha de cargo',
    'LBL_DEFAULT_PANEL' => 'Datos generales',
    'LBL_PANEL_RECORD_DETAILS' => 'Detalles del registro',
    'LBL_STATUS' => 'Estado',
    'LBL_GENERATE_SEPA_DIRECT_DEBITS_SEPA' => 'Generar remesa SEPA de recibos',
    'LBL_GENERATE_SEPA_CREDIT_TRANSFERS' => 'Generar remesa SEPA de transferencias',
    'LBL_PROCESS_REDSYS_CARD_PAYMENTS' => 'Procesar pagos con tarjeta',
    'LBL_NO_BANK_ACCOUNT_ERROR' => 'La cuenta bancaria es obligatoria si la remesa es de domiciliaciones o de transferencias.',
    'LBL_BANK_ACCOUNT_SHOULD_BE_EMPTY_ERROR' => 'Si se indica una cuenta bancaria la remesa debe ser de domiciliaciones o de transferencias.',

    // Mensajes SEPA comunes para recibos y transferencias
    'LBL_SEPA_FIX_REMITTANCE_ERROR' => 'Compruebe',
    'LBL_SEPA_INVALID_ACCOUNT_NAME' => '<b>Organización</b> no válida en el pago: ',
    'LBL_SEPA_INVALID_AMOUNT' => '<b>Importe</b> no válido en el pago (y tal vez en su compromiso de pago): ',
    'LBL_SEPA_INVALID_CONTACT_NAME' => '<b>Persona</b> no válida en el pago: ',
    'LBL_SEPA_INVALID_IBAN' => '<b>Cuenta bancaria</b> no válida en el pago (y tal vez en su compromiso de pago): ',
    'LBL_SEPA_INVALID_LOAD_DATE' => 'El archivo no se puede generar porque la <b>fecha de cargo</b> de la remesa es anterior al día de hoy.',
    'LBL_SEPA_INVALID_MAIN_IBAN' => 'El archivo no se puede generar porque la <b>cuenta bancaria</b> de la remesa no es válida. Seleccione una cuenta válida de la lista o créela.',
    'LBL_SEPA_INVALID_STATUS' => 'El estado del pago era <i>pagado</i>. Al generar el fichero pasará a <i>remesado</i>: ',
    'LBL_SEPA_LOG_HEADER_PREFIX_NOT_GENERATED' => 'El fichero no se puede generar sin corregir previamente los siguientes errores:',
    'LBL_SEPA_LOG_HEADER_PREFIX' => 'Último archivo generado:',
    'LBL_SEPA_REMITTANCE_OK' => 'No se encontraron errores al generar el fichero de la remesa.',
    'LBL_SEPA_WITHOUT_CONTACT_OR_ACCOUNT' => 'No hay ninguna persona u organización relacionada en el pago: ',
    'LBL_SEPA_XML_HAS_ERRORS' => 'El archivo XML no se ha generado porque existen errores que deben ser corregidos.',
    'LBL_MISSING_SEPA_VARIABLES' => 'Algunos parámetros de configuración necesarios para la generación de remesas están vacíos. Revíselos en el área de administración del CRM antes de continuar:',

    // Mensajes SEPA para transferencias
    'LBL_SEPA_CREDIT_INVALID_TYPE' => 'El fichero no se puede generar porque su tipo debería ser <b>transferencias emitidas</b>.',

    // Mensajes SEPA para recibos
    'LBL_SEPA_DEBIT_INVALID_PAYMENT_COMMITMENT' => 'El pago no está relacionado con ningún compromiso de pago: ',
    'LBL_SEPA_DEBIT_INVALID_SIGNATURE_DATE' => 'La <b>fecha de firma</b> del compromiso de pago está vacía: ',
    'LBL_SEPA_DEBIT_INVALID_MANDATE' => 'El <b>mandato</b> del pago es inválido. Está vacío, supera los 35 caracteres o contiene espacios en blanco (verifique también el compromiso de pago): ',
    'LBL_SEPA_DEBIT_INVALID_NIF' => 'El <b>número de identificación</b> (NIF, NIE...) de la persona/organización está vacío: ',
    'LBL_SEPA_DEBIT_INVALID_TYPE' => 'El fichero no se puede generar porque su tipo debería ser <b>recibos domiciliados</b>.',

    // Mensajes SEPA para devolución de recibos
    'LBL_SEPA_RETURN_ERR_UPLOADING_FILE' => 'Error: El fichero seleccionado no se puede cargar. Error número ',
    'LBL_SEPA_RETURN_ERR_NO_RECEIPT' => 'Error: El fichero de devolución no contiene ningún recibo.',
    'LBL_SEPA_RETURN_ERR_OPENING_FILE' => 'Error: El fichero cargado no se puede abrir.',
    'LBL_SEPA_RETURN_FILE_OK' => 'El fichero de devolución ha sido procesado con éxito.',
    'LBL_SEPA_RETURN_UNPAID_PAYMENT' => 'Pago marcado como impagado: ',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_1' => 'Error: No se ha encontrado el pago: ID (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_2' => '), Deudor (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_3' => '), Importe (',
    'LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_4' => '), Fecha de devolución (',
    'LBL_SEPA_RETURN_SELECT_FILE' => 'Seleccione el fichero de devolución',
    'LBL_SEPA_LOAD_RETURNS' => 'Cargar devoluciones',
    'LBL_SEPA_RETURN_LOAD_FILE' => 'Cargue las devoluciones',

    // Mensajes Redsys para pagos recurrentes
    'LBL_CARD_PAYMENTS_REMITTANCE_INVALID_TYPE' => 'La remesa no se puede procesar porque su tipo debería ser Tarjetas.',
    'LBL_CARD_PAYMENTS_TPV_INVALID_MODE' => 'El valor de TPV_TEST debe ser 0 o 1.',
    'LBL_CARD_PAYMENTS_NONE_SUCCESS' => 'Ningún pago se ha procesado con éxito.',
    'LBL_CARD_PAYMENTS_ALL_SUCCESS' => 'Todos los pagos han sido procesados con éxito.',
    'LBL_CARD_PAYMENTS_SOME_SUCCESS' => 'Algunos pagos se han procesado con éxito y otros no. Revise el log de la remesa.',
    'LBL_CARD_PAYMENTS_UNKNOWN_ERROR' => 'Se ha producido un error desconocido. Revise el log de SinergiaCRM.',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_HEADER' => 'Pagos recurrentes con tarjeta incluidos en la remesa:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_SUCCESS' => 'Pagos procesados con éxito:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_OMITTED' => 'Pagos omitidos:',
    'LBL_CARD_PAYMENTS_REMITTANCE_INFO_FAILED' => 'Pagos con error:',
    'LBL_CARD_PAYMENTS_PAYMENT_INVALID_METHOD' => 'Se ha omitido el pago porque el medio pago no es tarjeta.',

    // Otras cadenas
    'LBL_ERROR_QUERY_PAYMENTS_TO_REMITTANCE' => 'Error al agregar pagos a remesas',
);
