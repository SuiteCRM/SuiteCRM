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
$mod_strings['LBL_ASSIGNED_TO_ID'] = 'Asignado a (ID)';
$mod_strings['LBL_ASSIGNED_TO_NAME'] = 'Asignado a';
$mod_strings['LBL_ASSIGNED_TO'] = 'Asignado a';
$mod_strings['LBL_LIST_ASSIGNED_TO_NAME'] = 'Asignado a';
$mod_strings['LBL_LIST_ASSIGNED_USER'] = 'Asignado a';
$mod_strings['LBL_CREATED'] = 'Creado por';
$mod_strings['LBL_CREATED_USER'] = 'Creado por';
$mod_strings['LBL_CREATED_ID'] = 'Creado por (ID)';
$mod_strings['LBL_MODIFIED'] = 'Modificado por';
$mod_strings['LBL_MODIFIED_NAME'] = 'Modificado por';
$mod_strings['LBL_MODIFIED_USER'] = 'Modificado por';
$mod_strings['LBL_MODIFIED_ID'] = 'Modificado por (ID)';
$mod_strings['LBL_STIC_PROSPECT_LISTS_SUBPANEL_TITLE'] = 'Listas de Público Objetivo';
$mod_strings['LBL_INC_REFERENCE_GROUP'] = 'Grupo de referencia (Incorpora)';
$mod_strings['LBL_INC_REFERENCE_ENTITY'] = 'Entidad de referencia (Incorpora)';
$mod_strings['LBL_INC_REFERENCE_OFFICER'] = 'Técnico de referencia (Incorpora)';
$mod_strings['LBL_INC_INCORPORA_USER'] = 'Identificador (Incorpora)';
$mod_strings['LBL_INCORPORA_CONNECTION_PARAMS'] = 'Parámetros de conexión con Incorpora';
$mod_strings['LBL_STIC_WORK_CALENDAR'] = 'Calendario laboral';
$mod_strings['LBL_STIC_WORK_CALENDAR_INFO'] = 'Indica si la funcionalidad de Calendario laboral está activada para el usuario.';
$mod_strings['LBL_STIC_CLOCK'] = 'Registro horario';
$mod_strings['LBL_STIC_CLOCK_INFO'] = 'Indica si la funcionalidad de Registro horario está activada para el usuario.';
$mod_strings['LBL_SDA_ALLOWED'] = 'Acceso a SinergiaDA';
$mod_strings['LBL_SDA_ALLOWED_INFO'] = 'Indica el nivel de acceso del usuario a Sinergia Data Analytics.<ul><li><b>Sin acceso:</b> el usuario no podrá acceder.</li><li><b>Acceso completo:</b> el usuario podrá crear, modificar y ejecutar informes.</li><li><b>Acceso de solo lectura:</b> el usuario podrá ejecutar informes y ver sus resultados pero no podrá crear otros nuevos ni modificar los existentes. Esta opción solo aplica en usuarios normales, los administradores con acceso a SinergiaDA siempre tendrán acceso completo.</li></ul>';

// Virtual Field Kreporter
$mod_strings['LBL_KREPORTER_EMAILS_LIST'] = 'Lista de direcciones de correo';

// Asistente de creación de registros de Calendario laboral
$mod_strings['LBL_PERIODIC_WORK_CALENDAR_BUTTON'] = 'Generar Calendario laboral';

// Autenticación OAuth
$mod_strings['LBL_OAUTH_AUTH_LOGIN_CONTAINER'] = '<h3>Inicio de sesión alternativo</h3>';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_1'] = 'La dirección de correo electrónico ';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_2'] = ' no pertenece a ningún usuario de SinergiaCRM. Si considera que se trata de un error, contacte con un administrador.';
$mod_strings['LBL_OAUTH_AUTH_ERR_INACTIVE_USER'] = 'El usuario asociado a esta dirección de correo electrónico está inactivo. Si considera que se trata de un error, contacte con un administrador.';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_USER_TYPE'] = 'El usuario asociado a esta dirección de correo electrónico no puede iniciar sesión mediante autenticación externa. Si considera que se trata de un error, contacte con un administrador.';

// Autenticación OAuth - Google
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_AUTHENTICATION_TEXT'] = 'Iniciar sesión con Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_TITLE'] = 'Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_ENABLE'] = 'Habilitar autenticación con Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_ENABLE_HELP'] = "Esta opción habilita la autenticación con Google. <a href='https://developers.google.com/identity/protocols/oauth2' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_ID'] = 'ID de cliente';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_ID_HELP'] = "ID de la aplicación registrada en Google API Console. <a href='https://developers.google.com/identity/protocols/oauth2' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_SECRET'] = 'Secreto de cliente';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_SECRET_HELP'] = "Secreto de la aplicación registrada en Google API Console. <a href='https://developers.google.com/identity/protocols/oauth2' target='_blank'>Más información</a>.";

// Autenticación OAuth - Microsoft
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_AUTHENTICATION_TEXT'] = 'Iniciar sesión con Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TITLE'] = 'Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_ENABLE'] = 'Habilitar autenticación con Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_ENABLE_HELP'] = "Esta opción habilita la autenticación con Microsoft. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_CLIENT_ID'] = 'ID de cliente';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_CLIENT_ID_HELP'] = "ID de la aplicación registrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TENANT_ID'] = 'ID de tenant';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TENANT_ID_HELP'] = "ID de tenant de la aplicación registrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_REDIRECT_URI'] = 'URI de redirección';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_REDIRECT_URI_HELP'] = "URI de redirección de la aplicación registrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Más información</a>.";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_SCOPES'] = 'Ámbitos';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_SCOPES_HELP'] = "Ámbitos de la aplicación registrada en Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-permissions-and-consent' target='_blank'>Más información</a>.";

// Emulación de usuarios - Usuario emulador
$mod_strings['LBL_IMPERSONATE_USER_BUTTON'] = 'Emular usuario';
$mod_strings['LBL_IMPERSONATE_MESSAGE_TITLE'] = 'Emulación de usuario';
$mod_strings['LBL_IMPERSONATE_MESSAGE_DESCRIPTION'] = '¡Atención! Está emulando al usuario: ';
$mod_strings['LBL_IMPERSONATE_MESSAGE_STOP_DESCRIPTION'] = '<br>Para volver a su propio usuario haga clic <a href="index.php?module=Users&action=stopImpersonation" style="color: #fff; text-decoration: underline;">aquí</a>.';
$mod_strings['LBL_IMPERSONATE_ORIGINAL_USER'] = 'Usuario original';
$mod_strings['LBL_IMPERSONATE_TARGET_USER'] = 'Usuario emulado';
$mod_strings['LBL_IMPERSONATE_STOP_BUTTON'] = 'Finalizar emulación';

// Emulación de usuarios - Usuario emulado
$mod_strings['LBL_IMPERSONATION_ALERT_HEADER'] = 'Emulación de usuario';
$mod_strings['LBL_IMPERSONATION_START_ALERT_DESCRIPTION_TITLE'] = 'Un administrador está emulando su usuario.';
$mod_strings['LBL_IMPERSONATION_STOP_ALERT_DESCRIPTION_TITLE'] = 'Un administrador ha finalizado la emulación de su usuario.';
$mod_strings['LBL_IMPERSONATION_ALERT_USER'] = 'Usuario emulador: ';

// Emulación de usuarios - Monitorización
$mod_strings['LBL_IMPERSONATION_MONITORING_ITEM_SUMMARY_1'] = 'Emulación del usuario ';
$mod_strings['LBL_IMPERSONATION_MONITORING_ITEM_SUMMARY_2'] = ' con ID ';

// Modelo 182
$mod_strings['LBL_STIC_M182_ISSUING_ORGANIZATION'] = 'Emisión del Modelo 182';
$mod_strings['LBL_STIC_M182_ISSUING_ORGANIZATION_INFO'] = 'Indica para qué organizaciones podrá el usuario generar el Modelo 182. La lista se genera dinámicamente a partir de los <a href="index.php?module=stic_Settings" target="_blank">parámetos de configuración</a>.';
