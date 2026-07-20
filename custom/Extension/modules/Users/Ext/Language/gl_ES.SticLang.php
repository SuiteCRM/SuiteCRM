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
$mod_strings['LBL_ASSIGNED_TO_ID'] = 'Asignado a';
$mod_strings['LBL_ASSIGNED_TO_NAME'] = 'Asignado a';
$mod_strings['LBL_ASSIGNED_TO'] = 'Asignado a';
$mod_strings['LBL_LIST_ASSIGNED_TO_NAME'] = 'Asignado a';
$mod_strings['LBL_LIST_ASSIGNED_USER'] = 'Asignado a';
$mod_strings['LBL_CREATED'] = 'Creado por';
$mod_strings['LBL_CREATED_USER'] = 'Creado por';
$mod_strings['LBL_CREATED_ID'] = 'Creado por';
$mod_strings['LBL_MODIFIED'] = 'Modificado por';
$mod_strings['LBL_MODIFIED_NAME'] = 'Modificado por';
$mod_strings['LBL_MODIFIED_USER'] = 'Modificado por';
$mod_strings['LBL_MODIFIED_ID'] = 'Modificado por';
$mod_strings['LBL_STIC_PROSPECT_LISTS_SUBPANEL_TITLE'] = 'Listas de Público Obxectivo';
$mod_strings['LBL_INC_REFERENCE_GROUP'] = 'Grupo de referencia (Incorpora)';
$mod_strings['LBL_INC_REFERENCE_ENTITY'] = 'Entidade de referencia (Incorpora)';
$mod_strings['LBL_INC_REFERENCE_OFFICER'] = 'Técnico de referencia (Incorpora)';
$mod_strings['LBL_INC_INCORPORA_USER'] = 'Identificador (Incorpora)';
$mod_strings['LBL_INCORPORA_CONNECTION_PARAMS'] = 'Parámetros de conexión con Incorpora';
$mod_strings['LBL_STIC_WORK_CALENDAR'] = 'Calendario laboral';
$mod_strings['LBL_STIC_WORK_CALENDAR_INFO'] = 'Indica se a funcionalidade de Calendario laboral está activada para o usuario.';
$mod_strings['LBL_STIC_CLOCK'] = 'Rexistro horario';
$mod_strings['LBL_STIC_CLOCK_INFO'] = 'Indica se a funcionalidade de Rexistro horario está activada para o usuario.';
$mod_strings['LBL_SDA_ALLOWED'] = 'Acceso a SinergiaDA';
$mod_strings['LBL_SDA_ALLOWED_INFO'] = 'Indica o nivel de acceso do usuario en Sinergia Data Analytics.<ul><li><b>Sen acceso:</b> o usuario non poderá acceder.</li><li><b>Acceso completo:</b> o usuario poderá crear, modificar e executar informes.</li><li><b>Acceso de só lectura:</b> o usuario poderá executar informes e ver os seus resultados pero non poderá crear informes novos nin modificar os existentes. Esta opción só se aplica a usuarios normais; os administradores con acceso a SinergiaDA sempre terán acceso completo.</li></ul>';

// Virtual Field Kreporter
$mod_strings['LBL_KREPORTER_EMAILS_LIST'] = 'Lista de direccións de correo';

// Asistente de creación de registros de Calendario laboral
$mod_strings['LBL_PERIODIC_WORK_CALENDAR_BUTTON'] = 'Xerar Calendario laboral';

// Autenticación OAuth
$mod_strings['LBL_OAUTH_AUTH_LOGIN_CONTAINER'] = '<h3>Inicio de sesión alternativo</h3>';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_1'] = 'O enderezo de correo electrónico ';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_2'] = ' non pertence a ningún usuario de SinergiaCRM. Se considera que se trata dun erro, contacte cun administrador.';
$mod_strings['LBL_OAUTH_AUTH_ERR_INACTIVE_USER'] = 'O usuario asociado a esta dirección de correo electrónico está inactivo. Se considera que se trata dun erro, contacte cun administrador.';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_USER_TYPE'] = 'O usuario asociado a esta dirección de correo electrónico non pode iniciar sesión mediante autenticación externa. Se considera que se trata dun erro, contacte cun administrador.';

// Autenticación OAuth - Google
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_AUTHENTICATION_TEXT'] = 'Iniciar sesión con Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_TITLE'] = 'Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_ENABLE'] = 'Habilitar autenticación con Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_ENABLE_HELP'] = "Esta opción habilita a autenticación con Google. <a href='https://developers.google.com/identity/protocols/oauth2' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_ID'] = 'ID del Cliente';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_ID_HELP'] = "ID da aplicación rexistrada en Google API Console. <a href='https://developers.google.com/identity/protocols/oauth2' target='_blank'>Máis información</a>.";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_SECRET'] = 'Secreto de Cliente';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_SECRET_HELP'] = "Secreto da aplicación rexistrada en Google API Console. <a href='https://developers.google.com/identity/protocols/oauth2' target='_blank'>Máis información</a>.";

// Autenticación OAuth - Microsoft
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_AUTHENTICATION_TEXT'] = 'Iniciar sesión con Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TITLE'] = 'Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_ENABLE'] = 'Habilitar autenticación con Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_ENABLE_HELP'] = "Esta opción habilita a autenticación con Microsoft. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Máis información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_CLIENT_ID'] = 'ID de Cliente';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_CLIENT_ID_HELP'] = "ID da aplicación rexistrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Máis información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TENANT_ID'] = 'ID de tenant';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TENANT_ID_HELP'] = "ID de tenant da aplicación rexistrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Máis información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_REDIRECT_URI'] = 'URI de Redirección';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_REDIRECT_URI_HELP'] = "URI de redirección da aplicación rexistrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Máis información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_SCOPES'] = 'Ámbitos';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_SCOPES_HELP'] = "Ámbitos da aplicación rexistrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-permissions-and-consent' target='_blank'>Máis información</a>.";

// Emulación de usuarios - Usuario emulador
$mod_strings['LBL_IMPERSONATE_USER_BUTTON'] = 'Emular usuario';
$mod_strings['LBL_IMPERSONATE_MESSAGE_TITLE'] = 'Emulación de usuario';
$mod_strings['LBL_IMPERSONATE_MESSAGE_DESCRIPTION'] = '¡Atención! Está emulando ao usuario: ';
$mod_strings['LBL_IMPERSONATE_MESSAGE_STOP_DESCRIPTION'] = '<br>Para volver ao seu propio usuario faga clic <a href="index.php?module=Users&action=stopImpersonation" style="color: #fff; text-decoration: underline;">aquí</a>.';
$mod_strings['LBL_IMPERSONATE_ORIGINAL_USER'] = 'Usuario orixinal';
$mod_strings['LBL_IMPERSONATE_TARGET_USER'] = 'Usuario emulado';
$mod_strings['LBL_IMPERSONATE_STOP_BUTTON'] = 'Finalizar emulación';

// Emulación de usuarios - Usuario emulado
$mod_strings['LBL_IMPERSONATION_ALERT_HEADER'] = 'Emulación de usuario';
$mod_strings['LBL_IMPERSONATION_START_ALERT_DESCRIPTION_TITLE'] = 'Un administrador está emulando o seu usuario.';
$mod_strings['LBL_IMPERSONATION_STOP_ALERT_DESCRIPTION_TITLE'] = 'Un administrador finalizou a emulación do seu usuario.';
$mod_strings['LBL_IMPERSONATION_ALERT_USER'] = 'Usuario emulador: ';

// Emulación de usuarios - Monitorización
$mod_strings['LBL_IMPERSONATION_MONITORING_ITEM_SUMMARY_1'] = 'Emulación do usuario ';
$mod_strings['LBL_IMPERSONATION_MONITORING_ITEM_SUMMARY_2'] = ' con ID ';

// Modelo 182
$mod_strings['LBL_STIC_M182_ISSUING_ORGANIZATION'] = 'Emisión do Modelo 182';
$mod_strings['LBL_STIC_M182_ISSUING_ORGANIZATION_INFO'] = 'Indica para que organizacións poderá o usuario xerar o Modelo 182. A lista xérase dinámicamente a partir dos <a href="index.php?module=stic_Settings" target="_blank">parámetros de configuración</a>.';
