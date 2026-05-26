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
$mod_strings['LBL_ASSIGNED_TO_ID'] = 'Assignat a (ID)';
$mod_strings['LBL_ASSIGNED_TO_NAME'] = 'Assignat a';
$mod_strings['LBL_ASSIGNED_TO'] = 'Assignat a';
$mod_strings['LBL_LIST_ASSIGNED_TO_NAME'] = 'Assignat a';
$mod_strings['LBL_LIST_ASSIGNED_USER'] = 'Assignat a';
$mod_strings['LBL_CREATED'] = 'Creat per';
$mod_strings['LBL_CREATED_USER'] = 'Creat per';
$mod_strings['LBL_CREATED_ID'] = 'Creat per (ID)';
$mod_strings['LBL_MODIFIED'] = 'Modificat per';
$mod_strings['LBL_MODIFIED_NAME'] = 'Modificat per';
$mod_strings['LBL_MODIFIED_USER'] = 'Modificat per';
$mod_strings['LBL_MODIFIED_ID'] = 'Modificat per (ID)';
$mod_strings['LBL_STIC_PROSPECT_LISTS_SUBPANEL_TITLE'] = 'Llistes de Públic Objectiu';
$mod_strings['LBL_INC_REFERENCE_GROUP'] = 'Grup de referència (Incorpora)';
$mod_strings['LBL_INC_REFERENCE_ENTITY'] = 'Entitat de referència (Incorpora)';
$mod_strings['LBL_INC_REFERENCE_OFFICER'] = 'Tècnic de referència (Incorpora)';
$mod_strings['LBL_INC_INCORPORA_USER'] = 'Identificador (Incorpora)';
$mod_strings['LBL_INCORPORA_CONNECTION_PARAMS'] = 'Paràmetres de connexió amb Incorpora';
$mod_strings['LBL_STIC_WORK_CALENDAR'] = 'Calendari laboral';
$mod_strings['LBL_STIC_WORK_CALENDAR_INFO'] = "Indica si la funcionalitat de Calendari laboral està activada per a l'usuari.";
$mod_strings['LBL_STIC_CLOCK'] = 'Registre horari';
$mod_strings['LBL_STIC_CLOCK_INFO'] = "Indica si la funcionalitat de Registre horari està activada per a l'usuari.";
$mod_strings['LBL_SDA_ALLOWED'] = 'Accés a SinergiaDA';
$mod_strings['LBL_SDA_ALLOWED_INFO'] = "Indica el nivell d'accés de l'usuari a Sinergia Data Analytics.<ul><li><b>Sense accés:</b> l'usuari no podrà accedir-hi.</li><li><b>Accés complet:</b> l'usuari podrà crear, modificar i executar informes.</li><li><b>Accés de només lectura:</b> l'usuari podrà executar informes i veure'n els resultats però no podrà crear-ne de nous ni modificar els existents. Aquesta opció només és d'aplicació en usuaris normals, els administradors amb accés a SinergiaDA sempre hi tindran accés complet.</li></ul>";

// Virtual Field Kreporter
$mod_strings['LBL_KREPORTER_EMAILS_LIST'] = "Llista d'adreces de correu";

// Assistent de creació de registres de Calendari laboral
$mod_strings['LBL_PERIODIC_WORK_CALENDAR_BUTTON'] = 'Genera el Calendari laboral';

// Autenticació OAuth
$mod_strings['LBL_OAUTH_AUTH_LOGIN_CONTAINER'] = '<h3>Inici de sessió alternatiu</h3>';
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_1'] = "L'adreça de correu electrònic ";
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_EMAIL_2'] = " no pertany a cap usuari de SinergiaCRM. Si creieu que és un error, contacteu amb un administrador.";
$mod_strings['LBL_OAUTH_AUTH_ERR_INACTIVE_USER'] = "L'usuari associat a aquesta adreça de correu electrònic està inactiu. Si creieu que és un error, contacteu amb un administrador.";
$mod_strings['LBL_OAUTH_AUTH_ERR_INVALID_USER_TYPE'] = "L'usuari associat a aquesta adreça de correu electrònic no pot iniciar sessió mitjançant autenticació externa. Si creieu que és un error, contacteu amb un administrador.";

// Autenticació OAuth - Google
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_AUTHENTICATION_TEXT'] = 'Inicia la sessió amb Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_TITLE'] = 'Google';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_ENABLE'] = "Habilita l'autenticació amb Google";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_ENABLE_HELP'] = "Aquesta opció habilita l'autenticació amb Google. <a href='https://developers.google.com/identity/protocols/oauth2'>Més informació</a>";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_ID'] = 'ID de client';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_ID_HELP'] = "ID de l'aplicació registrada a Google API Console. <a href='https://developers.google.com/identity/protocols/oauth2'>Més informació</a>";
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_SECRET'] = 'Secret de client';
$mod_strings['LBL_OAUTH_AUTH_GOOGLE_CLIENT_SECRET_HELP'] = "Secret de l'aplicació registrada a Google API Console. <a href='https://developers.google.com/identity/protocols/oauth2'>Més informació</a>";

// Autenticació OAuth - Microsoft
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_AUTHENTICATION_TEXT'] = 'Inicia la sessió amb Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TITLE'] = 'Microsoft';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_ENABLE'] = "Habilita l'autenticació amb Microsoft";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_ENABLE_HELP'] = "Aquesta opció habilita l'autenticació amb Microsoft. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow'>Més informació</a>";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_CLIENT_ID'] = 'ID de client';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_CLIENT_ID_HELP'] = "ID de l'aplicació registrada a Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow'>Més informació</a>";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TENANT_ID'] = 'ID de tenant';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_TENANT_ID_HELP'] = "ID de tenant de l'aplicació registrada a Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow'>Més informació</a>";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_REDIRECT_URI'] = 'URL de redirecció';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_REDIRECT_URI_HELP'] = "URL de redirecció de l'aplicació registrada a Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow' target='_blank'>Més informació</a>";
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_SCOPES'] = 'Àmbits';
$mod_strings['LBL_OAUTH_AUTH_MICROSOFT_SCOPES_HELP'] = "Ámbits de l'aplicació registrada a Microsoft Azure Portal. <a href='https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-permissions-and-consent' target='_blank'>Més informació</a>";

// Emulació d'usuaris - Usuari que emula
$mod_strings['LBL_IMPERSONATE_USER_BUTTON'] = 'Emula aquest usuari';
$mod_strings['LBL_IMPERSONATE_MESSAGE_TITLE'] = "Emulació d'usuari";
$mod_strings['LBL_IMPERSONATE_MESSAGE_DESCRIPTION'] = "Atenció! Esteu emulant l'usuari: ";
$mod_strings['LBL_IMPERSONATE_MESSAGE_STOP_DESCRIPTION'] = '<br>Per tornar al vostre usuari feu clic <a href="index.php?module=Users&action=stopImpersonation" style="color: #fff; text-decoration: underline;">aquí</a>.';
$mod_strings['LBL_IMPERSONATE_ORIGINAL_USER'] = 'Usuari original';
$mod_strings['LBL_IMPERSONATE_TARGET_USER'] = 'Usuari emulat';
$mod_strings['LBL_IMPERSONATE_STOP_BUTTON'] = "Finalitza l'emulació";

// Emulació d'usuaris - Usuari emulat
$mod_strings['LBL_IMPERSONATION_ALERT_HEADER'] = "Emulació d'usuari";
$mod_strings['LBL_IMPERSONATION_START_ALERT_DESCRIPTION_TITLE'] = 'Un administrador està emulant el vostre usuari.';
$mod_strings['LBL_IMPERSONATION_STOP_ALERT_DESCRIPTION_TITLE'] = "Un administrador ha finalitzat l'emulació del vostre usuari.";
$mod_strings['LBL_IMPERSONATION_ALERT_USER'] = 'Usuari emulador: ';

// Emulació d'usuaris - Monitorització
$mod_strings['LBL_IMPERSONATION_MONITORING_ITEM_SUMMARY_1'] = "Emulació de l'usuari ";
$mod_strings['LBL_IMPERSONATION_MONITORING_ITEM_SUMMARY_2'] = ' amb ID ';

// Model 182
$mod_strings['LBL_STIC_M182_ISSUING_ORGANIZATION'] = 'Emissió del Model 182';
$mod_strings['LBL_STIC_M182_ISSUING_ORGANIZATION_INFO'] = "Indica per a quines organitzacions l'usuari podrà generar el Model 182. La llista es genera dinàmicament a partir dels <a href='index.php?module=stic_Settings' target='_blank'>paràmetres de configuració</a>.";
