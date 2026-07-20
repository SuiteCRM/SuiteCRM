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
  'LBL_ASSIGNED_TO_NAME' => 'Asignado a:',
  'LBL_ASSIGNED_TO' => 'Asignado a',
  'LBL_LIST_ASSIGNED_TO_NAME' => 'Asignado a',
  'LBL_LIST_ASSIGNED_USER' => 'Asignado a',
  'LBL_CREATED' => 'Creado Por',
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
  'LBL_QUICKEDIT_BUTTON' => '↙ Editar',
  'LBL_REMOVE' => 'Desvincular',
  'LBL_ASCENDING' => 'Ascendente',
  'LBL_DESCENDING' => 'Descendente',

  'LBL_LIST_FORM_TITLE' => 'Lista de Respostas a formularios',
  'LBL_MODULE_NAME' => 'Respostas a formularios',
  'LBL_MODULE_TITLE' => 'Respostas a formularios',
  'LBL_HOMEPAGE_TITLE' => 'As miñas Respostas a formularios',
  'LNK_NEW_RECORD' => 'Crear Resposta a formulario',
  'LNK_LIST' => 'Ver Respostas a formularios',
  'LNK_IMPORT_stic_AWF_Responses' => 'Importar Respostas a formularios',
  'LBL_SEARCH_FORM_TITLE' => 'Buscar Respostas a formularios',
  'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
  'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
  'LBL_NEW_FORM_TITLE' => 'Nova Resposta a formulario',
  'LBL_EMPTY' => 'Baleiro',
   
  'LBL_DEFAULT_PANEL' => 'Datos xerais',
  'LBL_PANEL_RECORD_DETAILS' => 'Detalles do rexistro',

  // Module fields
  'LBL_FORM' => 'Formulario ',
  'LBL_FORM_URL' => 'URL do Formulario',
  'LBL_CLEAN_REFERRER' => 'Páxina de orixe',
  'LBL_USER_AGENT' => 'Navegador e sistema operativo',
  'LBL_REMOTE_IP' => 'Enderezo IP',
  'LBL_RAW_PAYLOAD' => 'Resposta json (non visible)',
  'LBL_RESPONSE_HASH' => 'Hash da resposta',
  'LBL_HTML_SUMMARY' => 'Resposta',
  'LBL_STATUS' => 'Estado',
  'LBL_EXECUTION_LOG' => 'Rexistro de execución',
  
  // Execution log: action results
  'LBL_EXECUTION_ITEM_OK' => '✅ [OK]',
  'LBL_EXECUTION_ITEM_SKIPPED' => '⏭️ [OMITIDO]',
  'LBL_EXECUTION_ITEM_ERROR' => '❌ [ERRO]',

  // General
  'LBL_FIELD' => 'Campo',
  
  // Generic response messages
  'LBL_DUPLICATE_RESPONSE_TITLE' => 'Aviso',
  'LBL_DUPLICATE_RESPONSE_MSG' => 'Esta resposta xa foi enviada e procesada anteriormente.',

  'LBL_ERROR_GENERIC_TITLE' => 'Erro',
  'LBL_ERROR_GENERIC_MSG' => 'Produciuse un erro ao procesar a súa resposta.',

  'LBL_ERROR_FORM_VALIDATION' => 'Erro na validación de datos do formulario',
  'LBL_ERROR_FORM_VALIDATION_MSG' => 'Detectáronse erros nos datos enviados.',
  'LBL_BUTTON_GO_BACK_AND_FIX' => 'Volver a editar o formulario',

  // Internal processing errors
  'LBL_RESPONSE_NO_PUBLIC_STATUS' => 'Resposta recibida co formulario sen publicar.',
  'LBL_RESPONSE_HONEYPOT_SPAM' => 'Resposta non desexada: encheuse o campo trampa oculto.',
  'LBL_RESPONSE_TIMETRAP_SPAM' => 'Resposta non desexada: encheuse o formulario demasiado rápido.',
  'LBL_RESPONSE_USERAGENT_SPAM' => 'Resposta non desexada: o formulario foi enviado por unha aplicación.',
  'LBL_ERROR_FORM_CONFIG' => 'Erro na configuración do formulario',
  'LBL_ERROR_GENERATING_HTML_SUMMARY' => 'Erro xerando resumo da resposta en formato táboa',

  // User-facing validation errors
  'LBL_ERROR_REQUIRED_FIELD' => 'É un campo obrigatorio.',
  'LBL_ERROR_INTEGER_FIELD' => 'O valor debe ser un número enteiro.',
  'LBL_ERROR_NUMERIC_FIELD' => 'O valor debe ser numérico.',
  'LBL_ERROR_DATE_FIELD' => 'O valor debe ser unha data válida.',
  'LBL_ERROR_BOOL_FIELD' => 'O valor debe ser verdadeiro ou falso.',
  'LBL_ERROR_ENUM_FIELD' => 'Opción non válida para o despregable.',
  'LBL_ERROR_EMAIL_FIELD' => 'O valor debe ser un enderezo de correo válida.',
  'LBL_ERROR_VALUE_FIELD' => 'O valor non é válido para o campo.',
  
  // Subpanels
  'LBL_STIC_AWF_RESPONSES_STIC_AWF_LINKS_FROM_STIC_AWF_LINKS_TITLE' => 'Vínculos da resposta ao formulario',
  'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_FORMS_TITLE' => 'Formulario Web Avanzado',
  'LBL_ANSWERS_SUBPANEL_TITLE' => 'Detalles da resposta',
);
