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
    'LBL_SEND_DATE_TIME' => 'Data de Envío',
    'LBL_IN_QUEUE' => 'En Proceso',
    'LBL_IN_QUEUE_DATE' => 'Data de Encolamento',

    'ERR_INT_ONLY_EMAIL_PER_RUN' => 'Utilice unicamente valores enteiros para especificar o número de emails enviados por lote',

    'LBL_ATTACHMENT_AUDIT' => 'foi enviado.  non se duplicou en local para minimizar a utilización de espazo no disco duro.',
    'LBL_CONFIGURE_SETTINGS' => 'Configurar Opcións de Correo',
    'LBL_CUSTOM_LOCATION' => 'Definido polo Usuario',
    'LBL_DEFAULT_LOCATION' => 'Por Defecto',

    'LBL_EMAIL_DEFAULT_DELETE_ATTACHMENTS' => 'Eliminar arquivos adxuntos e notas relacionadas xunto aos Emails borrados',
    'LBL_EMAIL_WARNING_NOTIFICATIONS' => 'Correo electrónico para notificación de alertas', // PR 7610
    'LBL_EMAIL_ENABLE_CONFIRM_OPT_IN' => 'Configuracións de Autorización',
    'LBL_EMAIL_ENABLE_SEND_OPT_IN' => 'Enviar mensaxe de autorización automaticamente',
    'LBL_EMAIL_CONFIRM_OPT_IN_TEMPLATE_ID' => 'Plantilla de e-mail de confirmación de autorización',
    'LBL_LEGACY_EMAIL_COMPOSE_BEHAVIOR' => 'Habilitar el Comportamiento Antiguo de Redacción de Correo Electrónico',
    'LBL_EMAIL_OUTBOUND_CONFIGURATION' => 'Configuración de Correo Saínte',
    'LBL_EMAILS_PER_RUN' => 'Número de emails enviados por lote',
    'LBL_ID' => 'Identificación',
    'LBL_LIST_CAMPAIGN' => 'Campaña',
    'LBL_LIST_FORM_TITLE' => 'Cola',
    'LBL_LIST_FROM_NAME' => 'Nome do Remitente',
    'LBL_LIST_IN_QUEUE' => 'En Proceso',
    'LBL_LIST_MESSAGE_NAME' => 'Mensaxe de Marketing',
    'LBL_LIST_RECIPIENT_EMAIL' => 'Email do Destinatario',
    'LBL_LIST_RECIPIENT_NAME' => 'Nome do Destinatario',
    'LBL_LIST_SEND_ATTEMPTS' => 'Intentos de Envío',
    'LBL_LIST_SEND_DATE_TIME' => 'Enviar o',
    'LBL_LIST_USER_NAME' => 'Nome de Usuario',
    'LBL_LOCATION_ONLY' => 'Ubicación',
    'LBL_LOCATION_TRACK' => 'Ubicación dos arquivos de seguimento de campaña (como campaign_tracker.php)',
    'LBL_CAMP_MESSAGE_COPY' => 'Gardar copias das mensaxes da campaña:',
    'LBL_CAMP_MESSAGE_COPY_DESC' => '¿Desexa gardar copias completas de <bold>CADA</bold> mensaxe de email enviado durante todas as campañas?  <bold>Recoméndase o valor por defecto de non facelo</bold>.  Se escolle non, só se gardará a plantilla enviada e as variables para recrear a mensaxe individual.',
    'LBL_MAIL_SENDTYPE' => 'Axente de Transferencia de Correo (MTA):',
    'LBL_MAIL_SMTPAUTH_REQ' => 'Usar Autenticación SMTP:',
    'LBL_MAIL_SMTPPASS' => 'Contrasinal:',
    'LBL_MAIL_SMTPPORT' => 'Porto SMTP:',
    'LBL_MAIL_SMTPSERVER' => 'Servidor de Correo SMTP:',
    'LBL_MAIL_SMTPUSER' => 'Nome de Usuario:',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Escolla o seu proveedor de Email:',
    'LBL_YAHOOMAIL_SMTPPASS' => 'Contrasinal de Yahoo! Mail:',
    'LBL_YAHOOMAIL_SMTPUSER' => 'ID de Yahoo! Mail:',
    'LBL_GMAIL_SMTPPASS' => 'Contrasinal de Gmail:',
    'LBL_GMAIL_SMTPUSER' => 'Enderezo de Email de Gmail:',
    'LBL_EXCHANGE_SMTPPASS' => 'Contrasinal de Exchange:',
    'LBL_EXCHANGE_SMTPUSER' => 'Nome de usuario de Exchange:',
    'LBL_EXCHANGE_SMTPPORT' => 'Porto de Servidor Exchange:',
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor Exchange:',
    'LBL_EMAIL_LINK_TYPE' => 'Cliente de Correo',
    'LBL_MARKETING_ID' => 'Id de Marketing',
    'LBL_MODULE_ID' => 'EmailMan',
    'LBL_MODULE_NAME' => 'Configuración de Correo',
    'LBL_MODULE_TITLE' => 'Administración de Cola de Email Saínte',
    'LBL_NOTIFICATION_ON_DESC' => 'Cando está habilitado, enviaranse emails aos usuarios cando se lles asignen rexistros.',
    'LBL_NOTIFY_FROMADDRESS' => 'Enderezo do Remitente:',
    'LBL_NOTIFY_FROMNAME' => 'Nome do Remitente:',
    'LBL_NOTIFY_ON' => 'Notificacións de Asignación',
    'LBL_NOTIFY_TITLE' => 'Opcións de Correo',
    'LBL_OUTBOUND_EMAIL_TITLE' => 'Opcións de Correo Saínte',
    'LBL_RELATED_ID' => 'Id Relacionado',
    'LBL_RELATED_TYPE' => 'Tipo Relacionado',
    'LBL_SEARCH_FORM_TITLE' => 'Busca de Colas',
    'TRACKING_ENTRIES_LOCATION_DEFAULT_VALUE' => 'Valor en Config.php para site_url',
    'TXT_REMOVE_ME_ALT' => 'Para borrar a súa subscrición a esta lista de correo vaia a',
    'TXT_REMOVE_ME_CLICK' => 'faga clic aquí',
    'TXT_REMOVE_ME' => 'Para borrar a súa subscrición a esta lista de correo',
    'LBL_NOTIFY_SEND_FROM_ASSIGNING_USER' => 'Enviar notificación desde a enderezo de correo electrónico do usuario que asigna',

    'LBL_SECURITY_TITLE' => 'Configuración de Seguridade de Email',
    'LBL_SECURITY_DESC' => 'Marque aquilo que NON debería ser permitido en Email entrante ou mostradas no módulo de Emails.',
    'LBL_SECURITY_APPLET' => 'Etiqueta Applet',
    'LBL_SECURITY_BASE' => 'Etiqueta Base',
    'LBL_SECURITY_EMBED' => 'Etiqueta Embed',
    'LBL_SECURITY_FORM' => 'Etiqueta Form',
    'LBL_SECURITY_FRAME' => 'Etiqueta Frame',
    'LBL_SECURITY_FRAMESET' => 'Etiqueta Frameset',
    'LBL_SECURITY_IFRAME' => 'Etiqueta iFrame',
    'LBL_SECURITY_IMPORT' => 'Etiqueta Import',
    'LBL_SECURITY_LAYER' => 'Etiqueta Layer',
    'LBL_SECURITY_LINK' => 'Etiqueta Link',
    'LBL_SECURITY_OBJECT' => 'Etiqueta Object',
    'LBL_SECURITY_OUTLOOK_DEFAULTS' => 'Seleccionar a configuración mínima de seguridade por defecto en Outlook (erros na visualización do contido).',
    'LBL_SECURITY_STYLE' => 'Etiqueta Style',
    'LBL_SECURITY_TOGGLE_ALL' => 'Cambiar Todas as Opcións',
    'LBL_SECURITY_XMP' => 'Etiqueta Xmp',
    'LBL_YES' => 'Si',
    'LBL_NO' => 'Non',
    'LBL_PREPEND_TEST' => '[Proba]:',
    'LBL_SEND_ATTEMPTS' => 'Intentos de Envío',
    'LBL_OUTGOING_SECTION_HELP' => 'Configurar o servidor de correo saínte por defecto para enviar notificacións de correo, incluíndo alertas de fluxo de traballo.',
    'LBL_ALLOW_DEFAULT_SELECTION' => "Os usuarios poden enviar como identidade desta conta:",
    'LBL_ALLOW_DEFAULT_SELECTION_HELP' => 'Cando esta opción está seleccionada, todos os usuarios poderán enviar correos utilizando a mesma<br> conta de correo saínte para enviar notificacións do sistema e alertas.  Se a opción non está seleccionada,<br> os usuarios poderán utilizar o servidor de correo saínte unha vez proporcionen a información sobre a súa propia conta.',
    'LBL_FROM_ADDRESS_HELP' => 'Cando esta activado, o nome do usuario e Email do usuario que asigna inluirase no campo &#39;De&#39; do Email. É posible que esta funcionalidade non funcione con servidores SMTP que non permiten o envío desde un enderezo de Email diferente ao da conta do servidor.',
    'LBL_HELP' => 'Axuda' /*for 508 compliance fix*/,
    'LBL_OUTBOUND_EMAIL_ACCOUNT_VIEW' => 'Ver contas de correo electrónico saínte',
    'LBL_ALLOW_SEND_AS_USER' => 'Os usuarios poden enviar como eles mesmos:',
    'LBL_ALLOW_SEND_AS_USER_DESC' => 'Cando está habilitado, <b>os usuarios de</b> poden enviar correo electrónico usando o servidor de correo saínte, usando o seu propio enderezo de correo principal como a enderezo &quot;Desde: &quot;.<br>Esta característica podería non funcionar con servidores SMTP que non permiten enviar desde unha conta de correo electrónico diferente á da conta do servidor.',
);
