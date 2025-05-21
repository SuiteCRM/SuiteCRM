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
    'LBL_ASSIGNED_TO_ID' => 'Id de usuario asignado',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Data de Creación',
    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_MODIFIED' => 'Modificado Por',
    'LBL_MODIFIED_NAME' => 'Modificado Por Nome',
    'LBL_CREATED' => 'Creado Por',
    'LBL_DESCRIPTION' => 'Descrición',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nome',
    'LBL_CREATED_USER' => 'Creado polo Usuario',
    'LBL_MODIFIED_USER' => 'Modificado polo Usuario',
    'LBL_LIST_NAME' => 'Nome',
    'LBL_EDIT_BUTTON' => 'Editar',
    'LBL_REMOVE' => 'Quitar',
    'LBL_LIST_FORM_TITLE' => 'Lista de contas de correo electrónico saínte',
    'LBL_MODULE_NAME' => 'Contas de correo electrónico saínte',
    'LBL_MODULE_TITLE' => 'Contas de correo electrónico saínte',
    'LBL_HOMEPAGE_TITLE' => 'As miñas Contas de correo electrónico saínte',
    'LNK_NEW_RECORD' => 'Crear contas de correo electrónico saínte',

    'LNK_LIST_CREATE_NEW_PERSONAL' => 'Nueva Cuenta de Correo Electrónico Saliente Personal',
    'LNK_LIST_CREATE_NEW_GROUP' => 'Nueva Cuenta de Correo Electrónico Saliente de Grupo',
    'LNK_LIST_CREATE_SYSTEM_OVERRIDE_GROUP' => 'Nueva Cuenta de Correo Electrónico Saliente con Sobrecarga del Sistema',

    'LNK_LIST' => 'Cuentas de Correo Electrónico Saliente',
    'LNK_LIST_INBOUND_EMAIL_ACCOUNTS' => 'Cuentas de Correo Electrónico Entrante',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar contas de correo electrónico saínte',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Ver Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_NEW_FORM_TITLE' => 'Nova conta de correo electrónico saínte',
    'LBL_USERNAME' => 'Nome de usuario',
    'LBL_PASSWORD' => 'Contrasinal',
    'LBL_SMTP_SERVERNAME' => 'Servidor SMTP',
    'LBL_SMTP_AUTH' => 'Autentificación SMTP',
    'LBL_SMTP_PORT' => 'Porto SMTP',
    'LBL_SMTP_PROTOCOL' => 'Protocolo SMTP',
    'LBL_EDITVIEW_PANEL1' => 'Configuración de conta',
    'LBL_OUTBOUND_CONFIGURATION' => 'Configuración de Salida',
    'LBL_CONNECTION_CONFIGURATION' => 'Configuración del Servidor',
    'LBL_CHANGE_PASSWORD' => 'Cambiar contrasinal',
    'LBL_SEND_TEST_EMAIL' => 'Enviar Correo de Proba',

    // for outbound email dialog
    'LBL_MISSING_DEFAULT_OUTBOUND_SMTP_SETTINGS' => 'O administrador aínda non configurou a conta saínte por defecto. Non é posible enviar un correo de proba.',
    'LBL_MAIL_SMTPAUTH_REQ' => '¿Utilizar Autenticación SMTP?',
    'LBL_MAIL_SMTPPASS' => 'Contrasinal',
    'LBL_MAIL_SMTPPORT' => 'Porto',
    'LBL_MAIL_SMTPSERVER' => 'Servidor',
    'LBL_MAIL_SMTPUSER' => 'Nome de Usuario',
    'LBL_MAIL_SMTP_SETTINGS' => 'Especificación de Servidor SMTP',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Escolla o seu proveedor de Email:',
    'LBL_YAHOOMAIL_SMTPPASS' => 'Contrasinal de Yahoo! Mail:',
    'LBL_YAHOOMAIL_SMTPUSER' => 'ID de Yahoo! Mail:',
    'LBL_GMAIL_SMTPPASS' => 'Contrasinal de Gmail:',
    'LBL_GMAIL_SMTPUSER' => 'Enderezo de Email de Gmail:',
    'LBL_EXCHANGE_SMTPPASS' => 'Contrasinal de Exchange:',
    'LBL_EXCHANGE_SMTPUSER' => 'Nome de usuario de Exchange:',
    'LBL_EXCHANGE_SMTPPORT' => 'Porto de Servidor Exchange:',
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor Exchange:',

    'LBL_TYPE' => 'Tipo',
    'LBL_MAIL_SENDTYPE' => 'Mode de envío de mail',
    'LBL_MAIL_SMTPSSL' => 'Mail SMTP/SSL',
    'LBL_SMTP_FROM_NAME' => 'Nome do remitente',
    'LBL_SMTP_FROM_ADDR' => 'Enderezo do Remitente',
    'LBL_REPLY_TO_NAME' => 'Nombre de "Responder a"',
    'LBL_REPLY_TO_ADDR' => 'Dirección de "Responder a"',

    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de Seguridad',

    'LBL_OWNER' => 'Propietario',
    'LBL_OWNER_NAME' => 'Propietario',

    'LNK_EXTERNAL_OAUTH_CONNECTIONS' => 'Conexiones OAuth Externas',
);
