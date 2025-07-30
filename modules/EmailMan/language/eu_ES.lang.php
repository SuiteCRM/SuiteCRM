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
    'LBL_SEND_DATE_TIME' => 'Fecha de Envío',
    'LBL_IN_QUEUE' => 'En Proceso',
    'LBL_IN_QUEUE_DATE' => 'Fecha de Encolamiento',

    'ERR_INT_ONLY_EMAIL_PER_RUN' => 'Utilice únicamente valores enteros para especificar el número de mensajes enviados por lote.',

    'LBL_ATTACHMENT_AUDIT' => 'ha sido enviado.  No se ha duplicado en local para minimizar la utilización de espacio en el disco duro.',
    'LBL_CONFIGURE_SETTINGS' => 'Configurar Opciones de Correo',
    'LBL_CUSTOM_LOCATION' => 'Definido por el Usuario',
    'LBL_DEFAULT_LOCATION' => 'Por Defecto',

    'LBL_EMAIL_DEFAULT_DELETE_ATTACHMENTS' => 'Eliminar archivos adjuntos y notas relacionadas junto a los mensajes borrados',
    'LBL_EMAIL_WARNING_NOTIFICATIONS' => 'Correo electrónico para notificación de alertas', // PR 7610
    'LBL_EMAIL_ENABLE_CONFIRM_OPT_IN' => 'Configuraciones de Autorización',
    'LBL_EMAIL_ENABLE_SEND_OPT_IN' => 'Enviar mensaje de autorización automáticamente',
    'LBL_EMAIL_CONFIRM_OPT_IN_TEMPLATE_ID' => 'Plantilla de e-mail de confirmación de autorización',
    'LBL_LEGACY_EMAIL_COMPOSE_BEHAVIOR' => 'Habilitar el Comportamiento Antiguo de Redacción de Correo Electrónico',
    'LBL_EMAIL_OUTBOUND_CONFIGURATION' => 'Configuración de Correo Saliente',
    'LBL_EMAILS_PER_RUN' => 'Número de mensajes enviados por lote',
    'LBL_ID' => 'Identificación',
    'LBL_LIST_CAMPAIGN' => 'Campaña',
    'LBL_LIST_FORM_TITLE' => 'Cola',
    'LBL_LIST_FROM_NAME' => 'Nombre del Remitente',
    'LBL_LIST_IN_QUEUE' => 'En proceso',
    'LBL_LIST_MESSAGE_NAME' => 'Mensaje de Marketing',
    'LBL_LIST_RECIPIENT_EMAIL' => 'Dirección de correo del destinatario',
    'LBL_LIST_RECIPIENT_NAME' => 'Nombre del destinatario',
    'LBL_LIST_SEND_ATTEMPTS' => 'Intentos de envío',
    'LBL_LIST_SEND_DATE_TIME' => 'Enviar el',
    'LBL_LIST_USER_NAME' => 'Nombre de Usuario',
    'LBL_LOCATION_ONLY' => 'Ubicación',
    'LBL_LOCATION_TRACK' => 'Ubicación de los archivos de seguimiento de campaña (como campaign_tracker.php)',
    'LBL_CAMP_MESSAGE_COPY' => 'Guardar copias de los mensajes de la campaña:',
    'LBL_CAMP_MESSAGE_COPY_DESC' => '¿Desea guardar copias completas de todos los mensajes de correo enviados en todas las campañas? <bold>Por defecto, se recomienda no hacerlo.</bold> Si elige "No", sólo se guardarán la plantilla enviada y las variables necesarias para recrear el mensaje individual.',
    'LBL_MAIL_SENDTYPE' => 'Agente de Transferencia de Correo (MTA):',
    'LBL_MAIL_SMTPAUTH_REQ' => 'Usar autenticación SMTP:',
    'LBL_MAIL_SMTPPASS' => 'Contraseña:',
    'LBL_MAIL_SMTPPORT' => 'Puerto SMTP:',
    'LBL_MAIL_SMTPSERVER' => 'Servidor de correo SMTP:',
    'LBL_MAIL_SMTPUSER' => 'Nombre de Usuario:',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Elija su proveedor de correo:',
    'LBL_YAHOOMAIL_SMTPPASS' => 'Contraseña de Yahoo! Mail:',
    'LBL_YAHOOMAIL_SMTPUSER' => 'ID de Yahoo! Mail:',
    'LBL_GMAIL_SMTPPASS' => 'Contraseña de Gmail:',
    'LBL_GMAIL_SMTPUSER' => 'Dirección de correo de Gmail:',
    'LBL_EXCHANGE_SMTPPASS' => 'Contraseña de Exchange:',
    'LBL_EXCHANGE_SMTPUSER' => 'Nombre de usuario de Exchange:',
    'LBL_EXCHANGE_SMTPPORT' => 'Puerto de Servidor Exchange:',
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor Exchange:',
    'LBL_EMAIL_LINK_TYPE' => 'Cliente de Correo',
    'LBL_MARKETING_ID' => 'Id de Marketing',
    'LBL_MODULE_ID' => 'EmailMan',
    'LBL_MODULE_NAME' => 'Configuración de correo',
    'LBL_MODULE_TITLE' => 'Administración de la cola de correo',
    'LBL_NOTIFICATION_ON_DESC' => 'Cuando está habilitado, se enviarán correos a los usuarios cuando se les asignen registros.',
    'LBL_NOTIFY_FROMADDRESS' => 'Dirección del Remitente:',
    'LBL_NOTIFY_FROMNAME' => 'Nombre del Remitente:',
    'LBL_NOTIFY_ON' => 'Notificaciones de Asignación',
    'LBL_NOTIFY_TITLE' => 'Opciones de Correo',
    'LBL_OUTBOUND_EMAIL_TITLE' => 'Opciones de Correo Saliente',
    'LBL_RELATED_ID' => 'Id Relacionado',
    'LBL_RELATED_TYPE' => 'Tipo Relacionado',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Colas',
    'TRACKING_ENTRIES_LOCATION_DEFAULT_VALUE' => 'Valor en Config.php para site_url',
    'TXT_REMOVE_ME_ALT' => 'Para borrar su suscripción a esta lista de correo vaya a',
    'TXT_REMOVE_ME_CLICK' => 'haga clic aquí',
    'TXT_REMOVE_ME' => 'Para borrar su suscripción a esta lista de correo',
    'LBL_NOTIFY_SEND_FROM_ASSIGNING_USER' => 'Enviar notificación desde la dirección de correo electrónico del usuario que asigna',

    'LBL_SECURITY_TITLE' => 'Configuración de seguridad de correo',
    'LBL_SECURITY_DESC' => 'Marque aquello que NO debería ser permitido en correo entrante o mostrado en el módulo Correos.',
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
    'LBL_SECURITY_OUTLOOK_DEFAULTS' => 'Seleccionar la configuración mínima de seguridad por defecto en Outlook (errores en la visualización del contenido).',
    'LBL_SECURITY_STYLE' => 'Etiqueta Style',
    'LBL_SECURITY_TOGGLE_ALL' => 'Cambiar Todas las Opciones',
    'LBL_SECURITY_XMP' => 'Etiqueta Xmp',
    'LBL_YES' => 'Sí',
    'LBL_NO' => 'No',
    'LBL_PREPEND_TEST' => '[Prueba]:',
    'LBL_SEND_ATTEMPTS' => 'Intentos de Envío',
    'LBL_OUTGOING_SECTION_HELP' => 'Configurar el servidor de correo saliente por defecto para enviar notificaciones de correo, incluyendo alertas de flujo de trabajo.',
    'LBL_ALLOW_DEFAULT_SELECTION' => "Los usuarios pueden enviar como identidad de esta cuenta:",
    'LBL_ALLOW_DEFAULT_SELECTION_HELP' => 'Cuando esta opción está seleccionada, todos los usuarios podrán enviar correos utilizando la misma<br> cuenta de correo saliente para enviar notificaciones del sistema y alertas.  Si la opción no está seleccionada,<br> los usuarios podrán utilizar el servidor de correo saliente una vez proporcionen la información sobre su propia cuenta.',
    'LBL_FROM_ADDRESS_HELP' => 'Cuando está activado, el nombre y la dirección de correo del usuario que asigna se incluirán en el campo &#39;De&#39; del mensaje. Es posible que esta funcionalidad no funcione con servidores SMTP que no permiten el envío desde una dirección de correo diferente a la de la cuenta del servidor.',
    'LBL_HELP' => 'Ayuda' /*for 508 compliance fix*/,
    'LBL_OUTBOUND_EMAIL_ACCOUNT_VIEW' => 'Ver cuentas de correo electrónico saliente',
    'LBL_ALLOW_SEND_AS_USER' => 'Los usuarios pueden enviar como ellos mismos:',
    'LBL_ALLOW_SEND_AS_USER_DESC' => 'Cuando está habilitado, <b>los usuarios de</b> pueden enviar correo electrónico usando el servidor de correo saliente, usando su propia dirección de correo principal como la dirección &quot;Desde: &quot;.<br>Esta característica podría no funcionar con servidores SMTP que no permiten enviar desde una cuenta de correo electrónico diferente a la de la cuenta del servidor.',
);
