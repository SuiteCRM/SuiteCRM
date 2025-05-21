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
    'LBL_SEND_DATE_TIME' => 'Data de tramesa',
    'LBL_IN_QUEUE' => 'En procés',
    'LBL_IN_QUEUE_DATE' => 'Data de posada en cua',

    'ERR_INT_ONLY_EMAIL_PER_RUN' => 'El nombre de missatges enviats per lot ha de ser un valor enter.',

    'LBL_ATTACHMENT_AUDIT' => 'ha estat enviat. No s\'ha duplicat en local per minimitzar la utilització d\'espai al disc dur.',
    'LBL_CONFIGURE_SETTINGS' => 'Configurar',
    'LBL_CUSTOM_LOCATION' => 'Definit per l\'Usuari',
    'LBL_DEFAULT_LOCATION' => 'Per defecte',

    'LBL_EMAIL_DEFAULT_DELETE_ATTACHMENTS' => 'Esborrar els arxius adjunts i les notes relacionades dels missatges esborrats',
    'LBL_EMAIL_WARNING_NOTIFICATIONS' => 'Notificacions per correu electrònic', // PR 7610
    'LBL_EMAIL_ENABLE_CONFIRM_OPT_IN' => 'Configuració d\'Autoritzat a enviar',
    'LBL_EMAIL_ENABLE_SEND_OPT_IN' => 'Enviar automàticament Autoritzat a enviar per correu electrònic',
    'LBL_EMAIL_CONFIRM_OPT_IN_TEMPLATE_ID' => 'Confirmar la plantilla de correu electrònic Autoritzat a enviar',
    'LBL_LEGACY_EMAIL_COMPOSE_BEHAVIOR' => 'Activar el Comportament Antic de Redacció de Correu Electrònic',
    'LBL_EMAIL_OUTBOUND_CONFIGURATION' => 'Configuració de correu electrònic sortint',
    'LBL_EMAILS_PER_RUN' => 'Nombre de missatges enviats per lot',
    'LBL_ID' => 'Id',
    'LBL_LIST_CAMPAIGN' => 'Campanya',
    'LBL_LIST_FORM_TITLE' => 'Cua',
    'LBL_LIST_FROM_NAME' => 'Nom del remitent',
    'LBL_LIST_IN_QUEUE' => 'En procés',
    'LBL_LIST_MESSAGE_NAME' => 'Missatge de Màrqueting',
    'LBL_LIST_RECIPIENT_EMAIL' => 'Correu electrònic del destinatari',
    'LBL_LIST_RECIPIENT_NAME' => 'Nom del destinatari',
    'LBL_LIST_SEND_ATTEMPTS' => 'Intents de tramesa',
    'LBL_LIST_SEND_DATE_TIME' => 'Enviar el',
    'LBL_LIST_USER_NAME' => 'Nom d\'Usuari',
    'LBL_LOCATION_ONLY' => 'Ubicació',
    'LBL_LOCATION_TRACK' => 'Ubicació dels arxius de seguiment de campanya (com campaign_tracker.php)',
    'LBL_CAMP_MESSAGE_COPY' => 'Desar còpies dels missatges de la campanya:',
    'LBL_CAMP_MESSAGE_COPY_DESC' => 'Desitja desar còpies completes de <bold>CADA</bold> correu electrònic enviat durant totes les campanyes? <bold>Es recomana el valor per defecte de no fer-ho</bold>. Si elegeix no, només es desarà la plantilla enviada i les variables per recrear el missatge individual.',
    'LBL_MAIL_SENDTYPE' => 'Agent de Transferència de Correu (MTA):',
    'LBL_MAIL_SMTPAUTH_REQ' => 'Usar Autentificació SMTP?',
    'LBL_MAIL_SMTPPASS' => 'Contrasenya SMTP:',
    'LBL_MAIL_SMTPPORT' => 'Port SMTP:',
    'LBL_MAIL_SMTPSERVER' => 'Servidor SMTP:',
    'LBL_MAIL_SMTPUSER' => 'Usuari SMTP:',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Seleccioneu el proveïdor de correu electrònic',
    'LBL_YAHOOMAIL_SMTPPASS' => 'Contrasenya de Yahoo! Mail',
    'LBL_YAHOOMAIL_SMTPUSER' => 'Id de Yahoo! Mail:',
    'LBL_GMAIL_SMTPPASS' => 'Contrasenya de Gmail',
    'LBL_GMAIL_SMTPUSER' => 'Adreça de correu electrònic de Gmail',
    'LBL_EXCHANGE_SMTPPASS' => 'Contrasenya d&#39;Exchange:', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPUSER' => 'Nom d&#39;usuari d&#39;Exchange', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPPORT' => 'Port del servidor d&#39;Exchange', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor d&#39;Exchange', // Excepció d'escapat 
    'LBL_EMAIL_LINK_TYPE' => 'Client de correu electrònic',
    'LBL_MARKETING_ID' => 'Id de Màrqueting',
    'LBL_MODULE_ID' => 'EmailMan',
    'LBL_MODULE_NAME' => 'Configuració de correu electrònic',
    'LBL_MODULE_TITLE' => 'Administració de cua de correu electrònic sortint',
    'LBL_NOTIFICATION_ON_DESC' => 'Envia correus electrònics de notificació en assignar registres.',
    'LBL_NOTIFY_FROMADDRESS' => 'Adreça remitent:',
    'LBL_NOTIFY_FROMNAME' => 'Nom remitent:',
    'LBL_NOTIFY_ON' => 'Activar notificacions?',
    'LBL_NOTIFY_TITLE' => 'Opcions de correu electrònic',
    'LBL_OUTBOUND_EMAIL_TITLE' => 'Opcions de correu electrònic sortint',
    'LBL_RELATED_ID' => 'Id Relacionat',
    'LBL_RELATED_TYPE' => 'Tipus Relacionat',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca de cues',
    'TRACKING_ENTRIES_LOCATION_DEFAULT_VALUE' => 'Valor en Config.php per site_url',
    'TXT_REMOVE_ME_ALT' => 'Per eliminar la subscripció a aquesta llista de correu electrònic aneu a',
    'TXT_REMOVE_ME_CLICK' => 'feu clic aquí',
    'TXT_REMOVE_ME' => 'Per eliminar la subscripció a aquesta llista de correu electrònic',
    'LBL_NOTIFY_SEND_FROM_ASSIGNING_USER' => 'Enviar la notificació de l\'adreça de correu electrònic de l\'usuari assignat',

    'LBL_SECURITY_TITLE' => 'Configuració de seguretat de correu electrònic',
    'LBL_SECURITY_DESC' => 'Marqueu allò que NO hauria de ser permès als correus electrònics entrants o mostrat al mòdul de correu electrònic.',
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
    'LBL_SECURITY_OUTLOOK_DEFAULTS' => 'Seleccioneu les mesures mínimes de seguretat per defecte a Outlook (pot provocar errors en la visualització del contingut).',
    'LBL_SECURITY_STYLE' => 'Etiqueta Style',
    'LBL_SECURITY_TOGGLE_ALL' => 'Canvia totes les opcions',
    'LBL_SECURITY_XMP' => 'Etiqueta Xmp',
    'LBL_YES' => 'Sí',
    'LBL_NO' => 'No',
    'LBL_PREPEND_TEST' => '[Prova]: ',
    'LBL_SEND_ATTEMPTS' => 'Intents d\'enviament',
    'LBL_OUTGOING_SECTION_HELP' => 'Configurar el servidor de correu sortint per defecte per a enviar notificacions de correu electrònic, incloent avisos de flux de treball.',
    'LBL_ALLOW_DEFAULT_SELECTION' => "Els usuaris poden enviar amb l'identitat d'aquest compte:",
    'LBL_ALLOW_DEFAULT_SELECTION_HELP' => 'Quan aquesta opció està seleccionada, tots els usuaris poden enviar correus electrònics utilitzant el compte de correu sortint general. Si l\'opció no està seleccionada, els usuaris podran enviar correu quan hagin configurat el seu propi compte.',
    'LBL_FROM_ADDRESS_HELP' => 'Quan està activat, el nom i adreça de correu electrònic de l\'usuari assignat s\'inclourà al camp del correu electrònic. Aquesta funció no funciona amb servidors SMTP que no permeten l\'enviament d\'un compte de correu electrònic diferent al compte del servidor.',
    'LBL_HELP' => 'Ajuda' /*for 508 compliance fix*/,
    'LBL_OUTBOUND_EMAIL_ACCOUNT_VIEW' => 'Vista de compte de correu electrònic sortint',
    'LBL_ALLOW_SEND_AS_USER' => 'Els usuaris poden enviar com ells mateixos:',
    'LBL_ALLOW_SEND_AS_USER_DESC' => 'Quan està activat, <b>tots</b> els usuaris poden enviar un correu electrònic mitjançant el servidor de correu de sortida, fent servir la seva pròpia adreça de correu electrònic com l\'adreça remitent. <br>Aquesta característica podria no funcionar amb servidors SMTP que no permetin enviar des d\'un compte de correu electrònic diferent del del compte del servidor.',
);
