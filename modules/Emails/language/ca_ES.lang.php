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
    'LBL_EMAIL_INFORMATION' => 'CORREU ELECTRÒNIC',
    'LBL_FW' => 'RV:',
    'LBL_RE' => 'RE: ',

    'LBL_BUTTON_CREATE' => 'Crea',
    'LBL_BUTTON_EDIT' => 'Edita',
    'LBL_BUTTON_EDIT_EDIT_DRAFT' => 'Edita esborrany',
    'LBL_QS_DISABLED' => 'No disponible',
    'LBL_SIGNATURE_PREPEND' => 'Firmar damunt en contestar?',
    'LBL_IMPORT' => 'Importar',
    'LBL_LOADING' => 'Carregant',
    'LBL_MARKING' => 'Marcant',
    'LBL_DELETING' => 'S\'està esborrant',

    'LBL_CONFIRM_DELETE_EMAIL' => 'Segur que voleu eliminar aquest missatge?',
    'LBL_ENTER_FOLDER_NAME' => 'Indiqueu un nom de carpeta',

    'LBL_ERROR_SELECT_MODULE' => 'Seleccioneu un mòdul per al camp Relacionar amb',

    'ERR_ARCHIVE_EMAIL' => 'Error: Seleccioneu els correus electrònics a arxivar.',
    'ERR_DELETE_RECORD' => 'Error: Heu d\'especificar un número de registre a eliminar.',
    'LBL_ACCOUNTS_SUBPANEL_TITLE' => 'Comptes',
    'LBL_ADD_DASHLETS' => 'Afegeix dashlets',
    'LBL_ADD_DOCUMENT' => 'Afegeix documents',
    'LBL_ADD_ENTRIES' => 'Afegeix entrades',
    'LBL_ADD_FILE' => 'Afegeix un arxiu',
    'LBL_ATTACHMENTS' => 'Adjunts:',
    'LBL_ATTACH_FILES' => 'Adjunta fitxers',
    'LBL_ATTACH_DOCUMENTS' => 'Adjunta documents',
    'LBL_HAS_ATTACHMENT' => 'Té adjunt?',
    'LBL_BCC' => 'CCO:',
    'LBL_BODY' => 'Cos:',
    'LBL_BUGS_SUBPANEL_TITLE' => 'Incidències',
    'LBL_CC' => 'CC:',
    'LBL_COMPOSE_MODULE_NAME' => 'Redactar correu electrònic',
    'LBL_CONTACT_NAME' => 'Contacte:',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactes',
    'LBL_CREATED_BY' => 'Creat per',
    'LBL_DATE_SENT_RECEIVED' => 'Data Enviat/Rebut:',
    'LBL_DATE' => 'Data tramesa:',
    'LBL_DELETE_FROM_SERVER' => 'Esborrar missatge del servidor',
    'LBL_DESCRIPTION' => 'Descripció',
    'LBL_EDIT_ALT_TEXT' => 'Edita el text pla',
    'LBL_SEND_IN_PLAIN_TEXT' => 'Envia en text pla',
    'LBL_SEND_CONFIRM_OPT_IN' => 'Email d\'Autoritzat a enviar enviat',
    'LBL_EMAIL_ATTACHMENT' => 'Arxiu adjunt',
    'LBL_EMAIL_SELECTOR_SELECT' => 'Selecciona',
    'LBL_EMAIL_SELECTOR_CLEAR' => 'Neteja',
    'LBL_EMAIL' => 'Adreces de correu electrònic:',
    'LBL_EMAILS_ACCOUNTS_REL' => 'Correus electrònics:Comptes',
    'LBL_EMAILS_BUGS_REL' => 'Correus electrònics:Incidències',
    'LBL_EMAILS_CASES_REL' => 'Correus electrònics:Casos',
    'LBL_EMAILS_CONTACTS_REL' => 'Correus electrònics:Contactes',
    'LBL_EMAILS_LEADS_REL' => 'Correus electrònics:Clients Potencials',
    'LBL_EMAILS_OPPORTUNITIES_REL' => 'Correus electrònics:Oportunitats',
    'LBL_EMAILS_NOTES_REL' => 'Correus electrònics:Notes',
    'LBL_EMAILS_PROJECT_REL' => 'Correus electrònics:Projecte',
    'LBL_EMAILS_PROJECT_TASK_REL' => 'Correus electrònics:Tasques de projecte',
    'LBL_EMAILS_PROSPECT_REL' => 'Correus electrònics:Públic objetiu',
    'LBL_EMAILS_CONTRACTS_REL' => 'E-mail: contracte',
    'LBL_EMAILS_TASKS_REL' => 'Correus electrònics:Tasques',
    'LBL_EMAILS_USERS_REL' => 'Correus electrònics:Usuaris',
    'LBL_EMPTY_FOLDER' => 'No hi ha correus electrònics a mostrar',
    'LBL_SELECT_FOLDER' => 'Seleccionar carpeta',
    'LBL_ERROR_SENDING_EMAIL' => 'Error Enviant',
    'LBL_ERROR_SAVING_DRAFT' => 'Error al desar l\'esborrany',
    'LBL_FROM_NAME' => 'Nom Remitent',
    'LBL_FROM' => 'De:',
    'LBL_REPLY_TO' => 'Respondre A:',
    'LBL_HTML_BODY' => 'Cos de HTML',
    'LBL_INVITEE' => 'Destinataris',
    'LBL_LEADS_SUBPANEL_TITLE' => 'Clients Potencials',
    'LBL_MESSAGE_SENT' => 'Missatge Enviat',
    'LBL_MODIFIED_BY' => 'Modificat Per',
    'LBL_MODULE_NAME' => 'Tots els correus electrònics',
    'LBL_MODULE_TITLE' => 'Correus electrònics:',
    'LBL_MY_EMAILS' => 'Correus electrònics',
    'LBL_NEW_FORM_TITLE' => 'Arxivar correu electrònic',
    'LBL_NONE' => 'Cap',
    'LBL_NOT_SENT' => 'Error enviant',
    'LBL_NOTES_SUBPANEL_TITLE' => 'Dades adjuntes',
    'LBL_OPPORTUNITY_SUBPANEL_TITLE' => 'Oportunitats',
    'LBL_PROJECT_SUBPANEL_TITLE' => 'Projectes',
    'LBL_PROJECT_TASK_SUBPANEL_TITLE' => 'Tasques de Projecte',
    'LBL_RAW' => 'Codi font del correu electrònic',
    'LBL_SAVE_AS_DRAFT_BUTTON_TITLE' => 'Desa l\'esborrany',
    'LBL_DISREGARD_DRAFT_BUTTON_TITLE' => 'Descarta l\'esborrany',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca correus electrònics',
    'LBL_SEND_ANYWAYS' => 'Aquest correu electrònic no té assumpte. Voleu enviar-lo igualment?',
    'LBL_SEND_BUTTON_LABEL' => 'Envia',
    'LBL_SEND_BUTTON_TITLE' => 'Envia',
    'LBL_SEND' => 'ENVIAR',
    'LBL_SENT_MODULE_NAME' => 'Correus electrònics enviats',
    'LBL_SHOW_ALT_TEXT' => 'Mostra el text pla',
    'LBL_SIGNATURE' => 'Firma',
    'LBL_SUBJECT' => 'Assumpte:',
    'LBL_TEXT_BODY' => 'Text del cos',
    'LBL_TIME' => 'Hora tramesa:',
    'LBL_TO_ADDRS' => 'Per a',
    'LBL_USERS_SUBPANEL_TITLE' => 'Usuaris',
    'LBL_USERS' => 'Usuaris',

    'LNK_CALL_LIST' => 'Trucades',
    'LBL_EMAIL_RELATE' => 'Relacionat amb',
    'LNK_EMAIL_TEMPLATE_LIST' => 'Plantilles de correu electrònic',
    'LNK_MEETING_LIST' => 'Reunions',
    'LNK_NEW_CALL' => 'Programar Trucada',
    'LNK_NEW_EMAIL_TEMPLATE' => 'Crear plantilla de correu electrònic',
    'LNK_NEW_EMAIL' => 'Enviar correu electrònic',
    'LNK_NEW_MEETING' => 'Programar Reunió',
    'LNK_NEW_NOTE' => 'Nova Nota o Arxiu Adjunt',
    'LNK_NEW_SEND_EMAIL' => 'Redactar',
    'LNK_NEW_TASK' => 'Nova Tasca',
    'LNK_NOTE_LIST' => 'Notes',
    'LNK_SENT_EMAIL_LIST' => 'Correus electrònics enviats',
    'LNK_TASK_LIST' => 'Tasques',
    'LNK_VIEW_CALENDAR' => 'Avui',

    'LBL_LIST_ASSIGNED' => 'Assignat',
    'LBL_LIST_CONTACT_NAME' => 'Nom Contacte',
    'LBL_LIST_DATE_SENT' => 'Data Tramesa',
    'LBL_LIST_DATE_SENT_RECEIVED' => 'Data Enviat/Rebut', // PR 6728
    'LBL_LIST_FORM_DRAFTS_TITLE' => 'Borrador',
    'LBL_LIST_FORM_SENT_TITLE' => 'Correus electrònics enviats',
    'LBL_LIST_FORM_TITLE' => 'Llista de correus electrònics',
    'LBL_LIST_FROM_ADDR' => 'De',
    'LBL_LIST_RELATED_TO' => 'Relacionat amb',
    'LBL_LIST_SUBJECT' => 'Assumpte',
    'LBL_LIST_TO_ADDR' => 'Per a',
    'LBL_LIST_TYPE' => 'Tipus',

    'WARNING_SETTINGS_NOT_CONF' => 'Advertència: La seva configuració de correu electrònic no està preparada per a l\'enviament de correu electrònic.',

    // for All emails
    'LBL_BUTTON_RAW_LABEL' => 'Mostrar codi font',
    'LBL_BUTTON_RAW_LABEL_HIDE' => 'Amagar codi font',

    // for InboundEmail
    'LBL_BUTTON_CHECK' => 'Comprovar correu',
    'LBL_BUTTON_CHECK_TITLE' => 'Comprovar nous correus',
    'LBL_BUTTON_FORWARD' => 'Reenviar',
    'LBL_BUTTON_REPLY_TITLE' => 'Respondre',
    'LBL_BUTTON_REPLY_ALL' => 'Contestar a Tots',
    'LBL_BUTTON_DELETE_IMAP' => 'Esborrar d\'IMAP',
    'LBL_CASES_SUBPANEL_TITLE' => 'Casos',
    'LBL_INBOUND_TITLE' => 'Correu electrònic entrant',
    'LBL_INTENT' => 'Intenció',
    'LBL_MESSAGE_ID' => 'ID Missatge',
    'LBL_REPLY_HEADER_1' => 'El ',
    'LBL_REPLY_HEADER_2' => 'va escriure:',
    'LBL_REPLY_TO_ADDRESS' => 'Adreça de Respondre A',
    'LBL_REPLY_TO_NAME' => 'Nom de Respondre A',

    'LBL_LIST_BUG' => 'Incidències',
    'LBL_LIST_CASE' => 'Casos',
    'LBL_LIST_CONTACT' => 'Contactes',
    'LBL_LIST_LEAD' => 'Clients Potencials',
    'LBL_LIST_TASK' => 'Tasques',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Usuari Assignat',

    // for Inbox
    'LBL_ALL' => 'Tot',
    'LBL_ASSIGN_WARN' => 'Asseguri\'s que les dues opcions estan seleccionades.',
    'LBL_BACK_TO_GROUP' => 'Tornar a Safata d\'Entrada de Grup',
    'LBL_BUTTON_DISTRIBUTE_TITLE' => 'Assignar',
    'LBL_BUTTON_DISTRIBUTE' => 'Assignar',
    'LBL_BUTTON_GRAB_TITLE' => 'Agafar del grup',
    'LBL_BUTTON_GRAB' => 'Tomar del Grup',
    'LBL_CREATE_BUG' => 'Nova Incidència',
    'LBL_CREATE_CASE' => 'Nou Cas',
    'LBL_CREATE_CONTACT' => 'Nou Contacte',
    'LBL_CREATE_LEAD' => 'Nou Client Potencial',
    'LBL_CREATE_TASK' => 'Nova Tasca',
    'LBL_DIST_TITLE' => 'Assignació',
    'LBL_LOCK_FAIL_DESC' => 'L\'element elegit no està disponible actualment.',
    'LBL_LOCK_FAIL_USER' => ' ha pres possesió.',
    'LBL_MASS_DELETE_ERROR' => 'No s\'ha seleccionat cap element per esborrar.',
    'LBL_NEW' => 'Nou',
    'LBL_NEXT_EMAIL' => 'Següent Element Lliure',
    'LBL_REPLIED' => 'Va Contestar',
    'LBL_TO' => 'Per a:',
    'LBL_TOGGLE_ALL' => 'Activar tots',
    'LBL_UNKNOWN' => 'Desconegut',
    'LBL_USE' => 'Assignar:',
    'LBL_ASSIGN_SELECTED_RESULTS_TO' => 'Assignar Resultats Seleccionats a:',
    'LBL_USER_SELECT' => 'Seleccionar Usuaris',
    'LBL_USING_RULES' => 'Usant Regles:',
    'LBL_WARN_NO_DIST' => 'No s\'han seleccionat el mètode de distribució',
    'LBL_WARN_NO_USERS' => 'No s\'han seleccionat usuaris',
    'LBL_IMPORT_STATUS_TITLE' => 'Estat',
    'LBL_INDICATOR' => 'Indicador',
    'LBL_LIST_STATUS' => 'Estat',
    'LBL_LIST_TITLE_GROUP_INBOX' => 'Safata d\'Entrada de Grup',
    'LBL_LIST_TITLE_MY_DRAFTS' => 'Esborranys',
    'LBL_LIST_TITLE_MY_INBOX' => 'Bústia d\'entrada',
    'LBL_LIST_TITLE_MY_SENT' => 'Correu electrònic enviat',
    'LBL_LIST_TITLE_MY_ARCHIVES' => 'Correus electrònics arxivats',

    'LNK_MY_DRAFTS' => 'Esborranys',
    'LNK_MY_INBOX' => 'Correu electrònic',
    'LNK_VIEW_MY_INBOX' => 'Mostra el correu electrònic',
    'LNK_QUICK_REPLY' => 'Contestar',
    'LBL_EMAILS_NO_PRIMARY_TEAM_SPECIFIED' => 'No s\'ha especificat un equip principal',
    'LBL_INSERT_CONTACT_EMAIL' => 'Introduïu l\'adreça de correu electrònic d\'un contacte',
    'LBL_INSERT_ACCOUNT_EMAIL' => 'Introduïu l\'adreça de correu electrònic d\'un compte',
    'LBL_INSERT_TARGET_EMAIL' => 'Insereix una adreça d\'E-mail d\'un objectiu',
    'LBL_INSERT_USER_EMAIL' => 'Introduïu l\'adreça de correu electrònic d\'un usuari',
    'LBL_INSERT_LEAD_EMAIL' => 'Insereix una adreça d\'E-mail d\'un client potencial',
    'LBL_INSERT_ERROR_BLANK_EMAIL' => 'L\'adreça de correu electrònic no és vàlida',

    // advanced search
    'LBL_ASSIGNED_TO' => 'Assignat A:',
    'LBL_MEMBER_OF' => 'Pare',
    'LBL_QUICK_CREATE' => 'Creació Ràpida',
    'LBL_CREATE' => 'Crear',
    'LBL_STATUS' => 'Estat del correu electrònic:',
    'LBL_EMAIL_FLAGGED' => 'Etiquetat:',
    'LBL_EMAIL_REPLY_TO_STATUS' => 'Estat de Respondre A:',
    'LBL_TYPE' => 'Tipus:',
    //#20680 EmialTemplate Ext.Message.show;
    'LBL_EMAILTEMPLATE_MESSAGE_SHOW_TITLE' => 'Comproveu el següent!',
    'LBL_EMAILTEMPLATE_MESSAGE_SHOW_MSG' => 'Esteu segur que voleu reemplaçar la plantilla? Això farà perdre la informació que hi heu introduït!',
    'LBL_EMAILTEMPLATE_MESSAGE_CLEAR_MSG' => 'Al seleccionar "--Cap--" s\'eliminarà qualsevol informació ja introduïda al cos del correu electrònic. Voleu continuar?',
    'LBL_EMAILTEMPLATE_MESSAGE_WARNING_TITLE' => 'Avís',
    'LBL_EMAILTEMPLATE_MESSAGE_MULTIPLE_RECIPIENTS' => 'L\'ús d\'una plantilla de correu electrònic que conté variables de contacte, com el nom del contacte, per enviar missatges de correu electrònic a diversos destinataris pot tenir resultats inesperats. Es recomana l\'ús d\'una plantilla de correu electrònic per a enviaments massius.',
    'LBL_CHECK_ATTACHMENTS' => 'Comproveu els arxius adjunts.',
    'LBL_HAS_ATTACHMENTS' => 'Aquest correu electrònic ja té arxius adjunts. Voleu conservar-los?',
    'LBL_HAS_ATTACHMENT_INDICATOR' => 'Té adjunts',
    'ERR_MISSING_REQUIRED_FIELDS' => 'Falta un camp requerit',
    'ERR_INVALID_REQUIRED_FIELDS' => 'Camp requerit no vàlid',
    'LBL_FILTER_BY_RELATED_BEAN' => 'Només mostrar destinataris relacionats',
    'LBL_ADD_INBOUND_ACCOUNT' => 'Afegeix',
    'LBL_ADD_OUTBOUND_ACCOUNT' => 'Afegeix',
    'LBL_EMAIL_ACCOUNTS_INBOUND' => 'Propietats del compte de correu',
    'LBL_EMAIL_SETTINGS_OUTBOUND_ACCOUNT' => 'Servidor de correu sortint SMTP',
    'LBL_EMAIL_SETTINGS_OUTBOUND_ACCOUNTS' => 'Servidors de correu sortint SMTP',
    'LBL_EMAIL_SETTINGS_INBOUND_ACCOUNTS' => 'Comptes de correu',
    'LBL_EMAIL_SETTINGS_INBOUND' => 'Correu electrònic entrant',
    'LBL_EMAIL_SETTINGS_OUTBOUND' => 'Correu electrònic sortint',
    'LBL_ADD_CC' => 'Afegeix Cc',
    'LBL_ADD_BCC' => 'Afegeix Cco',
    'LBL_MOVE_TO_BCC' => 'Mou a Cco',
    'LBL_ADD_TO_ADDR' => 'Afegeix a',
    'LBL_SELECTED_ADDR' => 'Seleccionat',
    'LBL_ADD_CC_BCC_SEP' => '|',
    'LBL_SEND_EMAIL_FAIL_TITLE' => 'Error enviant el correu electrònic',
    'LBL_EMAIL_DETAIL_VIEW_SHOW' => 'mostrar',
    'LBL_EMAIL_DETAIL_VIEW_MORE' => 'més',
    'LBL_MORE_OPTIONS' => 'més',
    'LBL_LESS_OPTIONS' => 'menys',
    'LBL_MAILBOX_TYPE_PERSONAL' => 'Personal',
    'LBL_MAILBOX_TYPE_GROUP' => 'Grup',
    'LBL_MAILBOX_TYPE_GROUP_FOLDER' => 'Grup - Auto-Importar',
    'LBL_SEARCH_FOR' => 'Cercar per',
    'LBL_EMAIL_INBOUND_TYPE_HELP' => '<b> Personal </b>: Compte de correu accessible per vostè. Només vostè pot administrar i importar correus des d\'aquest compte. <br> <b> Grup </b>: Compte de correu accessible per membres d\'equips específics. Els membres d\'equips poden administrar i importar correus d\'aquest compte. <br> <b> Grup - auto-importació </b>: Compte de correu accessible per membres d\'equips específics. Els correus són importats com registres de forma automàtica.',
    'LBL_ADDRESS_BOOK_SEARCH_HELP' => 'Introdueixi una adreça de correu electrònic, nom, cognom o nom del compte per a trobar destinataris.',
    'LBL_TEST_SETTINGS' => 'Comprovar Configuració',
    'LBL_EMPTY_EMAIL_BODY' => '<p><span style="color: #888888;"><em>Aquest missatge no té contingut</em></span></p>',
    'LBL_HAS_EMPTY_EMAIL_SUBJECT' => 'Si us plau, especifiqueu un assumpte',
    'LBL_HAS_EMPTY_EMAIL_BODY' => 'Si us plau, escrigui el seu missatge en el cos',
    'LBL_HAS_INVALID_EMAIL_CC' => 'Les adreces en l\'àmbit del Cc no són vàlides',
    'LBL_HAS_INVALID_EMAIL_BCC' => 'Les adreces en l\'àmbit del Bcc no són vàlides',
    'LBL_HAS_INVALID_EMAIL_TO' => 'Les adreces en l\'àmbit del To no són vàlides',
    'LBL_TEST_EMAIL_SUBJECT' => 'Correu electrònic de prova de SuiteCRM',
    'LBL_NO_SUBJECT' => '(sense assumpte)',
    'LBL_CHECKING_ACCOUNT' => 'Comprovant la compta',
    'LBL_OF' => 'de',
    'LBL_TEST_EMAIL_BODY' => 'Aquest correu electrònic va ser enviat per tal de comprovar la informació del servidor de correu sortint presentades amb la sol·licitud SuiteCRM. A la recepció reeixida d\'aquest missatge indica que la informació del servidor de correu sortint sempre és vàlid.',

    // for outbound email dialog
    'LBL_MISSING_DEFAULT_OUTBOUND_SMTP_SETTINGS' => 'L\'administrador encara no ha configurat la compta sortint per defecte. No és possible enviar un correu electrònic de prova.',
    'LBL_MAIL_SMTPAUTH_REQ' => 'Fer servir Autentificació SMTP?',
    'LBL_MAIL_SMTPPASS' => 'Clau de pas SMTP:',
    'LBL_MAIL_SMTPPORT' => 'Port SMTP:',
    'LBL_MAIL_SMTPSERVER' => 'Servidor SMTP:',
    'LBL_MAIL_SMTPUSER' => 'Usuari SMTP:',
    'LBL_MAIL_SMTP_SETTINGS' => 'Especificació del servidor SMTP',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Triï el seu proveïdor de correu electrònic:',
    'LBL_YAHOOMAIL_SMTPPASS' => 'Contrasenya de Yahoo! Mail:',
    'LBL_YAHOOMAIL_SMTPUSER' => 'Id de Yahoo! Mail:',
    'LBL_GMAIL_SMTPPASS' => 'Contrasenya de Gmail:',
    'LBL_GMAIL_SMTPUSER' => 'Adreça de correu electrònic de Gmail:',
    'LBL_EXCHANGE_SMTPPASS' => 'Contrasenya de Exchange:',
    'LBL_EXCHANGE_SMTPUSER' => 'Nom d&#39;usuari d&#39;Exchange:', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPPORT' => 'Port del servidor d&#39;Exchange:', // Excepció d'escapat 
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor d&#39;Exchange:', // Excepció d'escapat 

    'LBL_EDIT_LAYOUT' => 'Editar Diseny' /*for 508 compliance fix*/,
    'LBL_ATTACHMENT' => 'Adjunt' /*for 508 compliance fix*/,
    'LBL_DELETE_INLINE' => 'Eliminar' /*for 508 compliance fix*/,
    'LBL_CREATE_CASES' => 'Crear casos' /*for 508 compliance fix*/,
    'LBL_CREATE_LEADS' => 'Crear clients potencials' /*for 508 compliance fix*/,
    'LBL_CREATE_CONTACTS' => 'Crear contactes' /*for 508 compliance fix*/,
    'LBL_CREATE_BUGS' => 'Crear incidència' /*for 508 compliance fix*/,
    'LBL_CREATE_TASKS' => 'Crear tasca' /*for 508 compliance fix*/,
    'LBL_CHECK_INLINE' => 'Correcte' /*for 508 compliance fix*/,
    'LBL_CLOSE' => 'Tancar' /*for 508 compliance fix*/,
    'LBL_HELP' => 'Ajuda' /*for 508 compliance fix*/,
    'LBL_WAIT' => 'Esperi' /*for 508 compliance fix*/,
    'LBL_CHECKEMAIL' => 'Revisar el correu electrònic' /*for 508 compliance fix*/,
    'LBL_COMPOSEEMAIL' => 'Redactar correu electrònic' /*for 508 compliance fix*/,
    'LBL_EMAILSETTINGS' => 'Configuració de correu electrònic' /*for 508 compliance fix*/,

    // SNIP
    'LBL_EMAILS_MEETINGS_REL' => 'Correus electrònics: Reunions',
    'LBL_DATE_MODIFIED' => 'Última Modificació',

    'LBL_CATEGORY' => 'Categoria',
    'LBL_LIST_CATEGORY' => 'Categoria',
    'LBL_EMAIL_TEMPLATE' => 'Plantilla de correu electrònic',

    'LBL_CONFIRM_DISREGARD_DRAFT_TITLE' => 'Descartar el esborrany',
    'LBL_CONFIRM_DISREGARD_DRAFT_BODY' => 'Amb aquesta operació se suprimirà aquest missatge, voleu continuar?',
    'LBL_EMAIL_DRAFT_DELETED' => 'L\'esborrany s\'ha esborrat correctament',
    'LBL_EMAIL_DRAFT_ERROR_DELETING' => 'S\'ha produït un error en intentar esborrar l\'esborrany.',

    'LBL_QUICK_CREATE_SUCCESS1' => 'El registre s\'ha creat correctament.',
    'LBL_QUICK_CREATE_SUCCESS2' => 'Feu clic a OK per mostrar el registre nou.',
    'LBL_QUICK_CREATE_SUCCESS3' => 'Feu clic a Cancel·la per tornar al correu.',

    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE' => 'Aplica una plantilla de missatge',
    'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_BODY' => 'Aquesta operació esborrarà el camp cos del missatge, voleu continuar?',

    'LBL_MAILBOX_ID' => 'Id de bústia',
    'LBL_PARENT_ID' => 'Id pare',
    'LBL_LAST_SYNCED' => 'Última sincronització',
    'LBL_ORPHANED' => 'Orfes',
    'LBL_IMAP_KEYWORDS' => 'Paraules clau IMAP',
    'LBL_ERROR_NO_FOLDERS' => 'Error: No hi ha carpetes disponibles. Comproveu la configuració de correu.',
    'LBL_ORIGINAL_MESSAGE_SEPARATOR' => '---',


    'LBL_MARK_UNREAD' => 'Marca com a no llegit',
    'LBL_MARK_READ' => 'Marca com a llegit',
    'LBL_MARK_FLAGGED' => 'Marca com a destacat',
    'LBL_MARK_UNFLAGGED' => 'Marca com a no destacat',
    'LBL_CONFIRM_OPT_IN_SENT_DATE' => 'E-mail d\'Autoritzat a enviar enviat',
    'LBL_CONFIRM_OPT_IN_FAIL_DATE' => 'E-mail d\'Autoritzat a enviar no enviat',
    'LBL_CONFIRM_OPT_IN_TOKEN' => 'Confirmar Autoritzat a enviar en Token',

    'ERR_NO_RETURN_ID' => 'Fitxer adjunt no es troba.',

    'LBL_LIST_DATE_MODIFIED' => 'Última Modificació',
    'LNK_IMPORT_CAMPAIGNS' => 'Importar Campanya',
    
    // Email Validation Error messages. Typicaly for Email Validation:
    'ERR_FIELD_FROM_IS_NOT_SET' => 'El camp "De" no està definit.',
    'ERR_FIELD_FROM_IS_EMPTY' => 'El camp "De" està buit.',
    'ERR_FIELD_FROM_IS_INVALID' => 'El camp "De" no és vàlid.',
    'ERR_FIELD_FROM_ADDR_IS_NOT_SET' => 'L\'adreça "De" no està definida.',
    'ERR_FIELD_FROM_ADDR_IS_EMPTY' => 'L\'adreça "De" està buida.',
    'ERR_FIELD_FROM_ADDR_IS_INVALID' => 'L\'adreça "De" no és vàlida.',
    'ERR_FIELD_FROMNAME_IS_NOT_SET' => 'El nom del remitent no està definit.',
    'ERR_FIELD_FROMNAME_IS_EMPTY' => 'El nom del remitent buit.',
    'ERR_FIELD_FROMNAME_IS_INVALID' => 'El nom del remitent no és vàlid.',
    'ERR_FIELD_FROM_NAME_IS_NOT_SET' => 'El nom del remitent no està definit.',
    'ERR_FIELD_FROM_NAME_IS_EMPTY' => 'El nom del remitent buit.',
    'ERR_FIELD_FROM_NAME_IS_INVALID' => 'El nom del remitent no és vàlid.',
    'ERR_FIELD_FROM_ADDR_NAME_IS_NOT_SET' => 'El parell adreça d\'enviament i nom no estan establerts.',
    'ERR_FIELD_FROM_ADDR_NAME_IS_EMPTY' => 'El parell adreça d\'enviament i nom estan buits.',
    'ERR_FIELD_FROM_ADDR_NAME_IS_INVALID' => 'El parell adreça d\'enviament i nom no són vàlids.',
    'ERR_FIELD_FROM_ADDR_NAME_DOESNT_MATCH_REGEX' => 'El format del parell adreça d\'enviament i nom no són vàlids, fes anar "nom@correu.org <Nom de la Persona>".',
    'ERR_FIELD_FROM_ADDR_NAME_INVALID_NAME_PART' => 'Part del nom invàlida del parell de l\'adreça de remitent i nom.',
    'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART' => 'Correu electrònic invàlid del parell de l\'adreça de remitent i nom.',
    'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM' => 'Adreça del remitent i nom no corresponen amb el nom o correu electrònic d\'origen.',
    'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_ADDR' => 'Adreça del remitent i nom no corresponen amb l\'adreça d\'origen.',
    'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROMNAME' => 'Adreça del remitent i nom no corresponen amb el nom d\'origen.',
    'ERR_FIELD_FROM_ADDR_NAME_INVALID_EMAIL_PART_TO_FIELD_FROM_NAME' => 'Adreça del remitent i nom no corresponen amb el nom d\'origen.',
);
