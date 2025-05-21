<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$mod_strings = array(
    'LBL_ASSIGNED_TO_ID' => 'Assignat a (ID)',
    'LBL_ASSIGNED_TO_NAME' => 'Assignat a',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Data de creació',
    'LBL_DATE_MODIFIED' => 'Data de modificació',
    'LBL_MODIFIED' => 'Modificat per',
    'LBL_MODIFIED_NAME' => 'Modificat per',
    'LBL_CREATED' => 'Creat per',
    'LBL_DESCRIPTION' => 'Descripció',
    'LBL_DELETED' => 'Eliminat',
    'LBL_NAME' => 'Nom',
    'LBL_CREATED_USER' => 'Creat per',
    'LBL_MODIFIED_USER' => 'Modificat per',
    'LBL_LIST_NAME' => 'Nom',
    'LBL_EDIT_BUTTON' => 'Edita',
    'LBL_REMOVE' => 'Elimina',
    'LBL_LIST_FORM_TITLE' => 'Llista de comptes de correu electrònic sortint',
    'LBL_MODULE_NAME' => 'Comptes de correu electrònic sortint',
    'LBL_MODULE_TITLE' => 'Comptes de correu electrònic sortint',
    'LBL_HOMEPAGE_TITLE' => 'Els meus comptes de correu electrònic sortint',
    'LNK_NEW_RECORD' => 'Crea un compte de correu electrònic sortint',

    'LNK_LIST_CREATE_NEW_PERSONAL' => 'Nova Comptes de Correu Electrònic Sortint Personal',
    'LNK_LIST_CREATE_NEW_GROUP' => 'Nou Comptes de Correu Electrònic Sortint de Grup',
    'LNK_LIST_CREATE_SYSTEM_OVERRIDE_GROUP' => 'Nou Comptes de Correu Electrònic Sortint amb Sobrecàrrega del Sistema',

    'LNK_LIST' => 'Comptes de Correu Electrònic Sortint',
    'LNK_LIST_INBOUND_EMAIL_ACCOUNTS' => 'Comptes de Correu Electrònic Entrant',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca comptes de correu electrònic sortint',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activitats',
    'LBL_NEW_FORM_TITLE' => 'Nou compte de correu electrònic sortint',
    'LBL_USERNAME' => "Nom d'usuari",
    'LBL_PASSWORD' => 'Contrasenya',
    'LBL_SMTP_SERVERNAME' => 'Nom del servidor SMTP',
    'LBL_SMTP_AUTH' => 'SMTP Auth',
    'LBL_SMTP_PORT' => 'Port SMTP',
    'LBL_SMTP_PROTOCOL' => 'Protocol SMTP',
    'LBL_EDITVIEW_PANEL1' => 'Configuració del compte',
    'LBL_OUTBOUND_CONFIGURATION' => 'Configuració de Sortida',
    'LBL_CONNECTION_CONFIGURATION' => 'Configuració del Servidor',
    'LBL_CHANGE_PASSWORD' => 'Canvia la contrasenya',
    'LBL_SEND_TEST_EMAIL' => 'Envia un correu electrònic de prova',

    // for outbound email dialog
    'LBL_MISSING_DEFAULT_OUTBOUND_SMTP_SETTINGS' => "No es pot enviar un correu electrònic de prova perquè l'administrador encara no ha configurat el compte per defecte de correu electrònic sortint.",
    'LBL_MAIL_SMTPAUTH_REQ' => 'Utilitzar autentificació SMTP?',
    'LBL_MAIL_SMTPPASS' => 'Contrasenya',
    'LBL_MAIL_SMTPPORT' => 'Port',
    'LBL_MAIL_SMTPSERVER' => 'Servidor',
    'LBL_MAIL_SMTPUSER' => 'Usuari',
    'LBL_MAIL_SMTP_SETTINGS' => 'Especificació del servidor SMTP',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Trieu el vostre proveïdor de correu electrònic:',
    'LBL_YAHOOMAIL_SMTPPASS' => 'Contrasenya de Yahoo! Mail:',
    'LBL_YAHOOMAIL_SMTPUSER' => 'Usuari de Yahoo! Mail:',
    'LBL_GMAIL_SMTPPASS' => 'Contrasenya de Gmail:',
    'LBL_GMAIL_SMTPUSER' => 'Usuari de Gmail:',
    'LBL_EXCHANGE_SMTPPASS' => 'Contrasenya d&#39;Exchange:', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPUSER' => 'Usuari d&#39;Exchange:', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPPORT' => 'Port del servidor d&#39;Exchange:', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor d&#39;Exchange:', // Excepció d'escapat 

    'LBL_TYPE' => 'Tipus',
    'LBL_MAIL_SENDTYPE' => "Mode d'enviament del correu electrònic",
    'LBL_MAIL_SMTPSSL' => 'Mail SMTP/SSL',
    'LBL_SMTP_FROM_NAME' => 'Nom del remitent',
    'LBL_SMTP_FROM_ADDR' => 'Adreça del remitent',
    'LBL_REPLY_TO_NAME' => 'Nom de "Respondre a"',
    'LBL_REPLY_TO_ADDR' => 'Adreça de "Respondre a"',

    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grups de Seguretat',

    'LBL_OWNER' => 'Propietari',
    'LBL_OWNER_NAME' => 'Propietari',

    'LNK_EXTERNAL_OAUTH_CONNECTIONS' => 'Connexions OAuth Externes',
);
