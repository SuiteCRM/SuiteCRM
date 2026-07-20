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
  'LBL_QUICKEDIT_BUTTON' => '↙ Editar',
  'LBL_REMOVE' => 'Desvincular',
  'LBL_ASCENDING' => 'Ascendente',
  'LBL_DESCENDING' => 'Descendente',

  'LBL_LIST_FORM_TITLE' => 'Lista de Respuestas a formularios',
  'LBL_MODULE_NAME' => 'Respuestas a formularios',
  'LBL_MODULE_TITLE' => 'Respuestas a formularios',
  'LBL_HOMEPAGE_TITLE' => 'Mis Respuestas a formularios',
  'LNK_NEW_RECORD' => 'Crear Respuesta a formulario',
  'LNK_LIST' => 'Ver Respuestas a formularios',
  'LNK_IMPORT_stic_AWF_Responses' => 'Importar Respuestas a formularios',
  'LBL_SEARCH_FORM_TITLE' => 'Buscar Respuestas a formularios',
  'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
  'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
  'LBL_NEW_FORM_TITLE' => 'Nueva Respuesta a formulario',
  'LBL_EMPTY' => 'Vacío',
   
  'LBL_DEFAULT_PANEL' => 'Datos generales',
  'LBL_PANEL_RECORD_DETAILS' => 'Detalles del registro',

  // Module fields
  'LBL_FORM' => 'Formulario',
  'LBL_FORM_URL' => 'URL del formulario',
  'LBL_CLEAN_REFERRER' => 'Página de origen',
  'LBL_USER_AGENT' => 'Navegador y sistema operativo',
  'LBL_REMOTE_IP' => 'Dirección IP',
  'LBL_RAW_PAYLOAD' => 'Respuesta json (no visible)',
  'LBL_RESPONSE_HASH' => 'Hash de la respuesta',
  'LBL_HTML_SUMMARY' => 'Respuesta',
  'LBL_STATUS' => 'Estado',
  'LBL_EXECUTION_LOG' => 'Registro de ejecución',
  
  // Execution log: action results
  'LBL_EXECUTION_ITEM_OK' => '✅ [OK]',
  'LBL_EXECUTION_ITEM_SKIPPED' => '⏭️ [OMITIDO]',
  'LBL_EXECUTION_ITEM_ERROR' => '❌ [ERROR]',
  'LBL_EXECUTION_DEFERRED' => 'Ejecución diferida',

  // General
  'LBL_FIELD' => 'Campo',
  
  // Generic response messages
  'LBL_DUPLICATE_RESPONSE_TITLE' => 'Aviso',
  'LBL_DUPLICATE_RESPONSE_MSG' => 'Esta respuesta ya ha sido enviada y procesada anteriormente.',

  'LBL_ERROR_GENERIC_TITLE' => 'Error',
  'LBL_ERROR_GENERIC_MSG' => 'Se ha producido un error al procesar su respuesta.',

  'LBL_ERROR_FORM_VALIDATION' => 'Error en la validación de datos del formulario',
  'LBL_ERROR_FORM_VALIDATION_MSG' => 'Se han detectado errores en los datos enviados.',
  'LBL_BUTTON_GO_BACK_AND_FIX' => 'Volver a editar el formulario',

  // Internal processing errors
  'LBL_RESPONSE_NO_PUBLIC_STATUS' => 'Respuesta recibida con el formulario sin publicar.',
  'LBL_RESPONSE_HONEYPOT_SPAM' => 'Respuesta no deseada: se ha rellenado el campo trampa oculto.',
  'LBL_RESPONSE_TIMETRAP_SPAM' => 'Respuesta no deseada: se ha rellenado el formulario demasiado rápido.',
  'LBL_RESPONSE_USERAGENT_SPAM' => 'Respuesta no deseada: el formulario ha sido enviado por una aplicación.',
  'LBL_ERROR_FORM_CONFIG' => 'Error en la configuración del formulario',
  'LBL_ERROR_GENERATING_HTML_SUMMARY' => 'Error generando resumen de la respuesta en formato tabla',

  // User-facing validation errors
  'LBL_ERROR_REQUIRED_FIELD' => 'Es un campo obligatorio.',
  'LBL_ERROR_INTEGER_FIELD' => 'El valor debe ser un número entero.',
  'LBL_ERROR_NUMERIC_FIELD' => 'El valor debe ser numérico.',
  'LBL_ERROR_DATE_FIELD' => 'El valor debe ser una fecha válida.',
  'LBL_ERROR_BOOL_FIELD' => 'El valor debe ser verdadero o falso.',
  'LBL_ERROR_ENUM_FIELD' => 'Opción no válida para el desplegable.',
  'LBL_ERROR_EMAIL_FIELD' => 'El valor debe ser una dirección de correo válida.',
  'LBL_ERROR_VALUE_FIELD' => 'El valor no es válido para el campo.',
  
  // Subpanels
  'LBL_STIC_AWF_RESPONSES_STIC_AWF_LINKS_FROM_STIC_AWF_LINKS_TITLE' => 'Vínculos de la respuesta al formulario',
  'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_FORMS_TITLE' => 'Formulario Web Avanzado',
  'LBL_ANSWERS_SUBPANEL_TITLE' => 'Detalles de la respuesta',
);
