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
    'LBL_MODULE_NAME' => 'Empleats',
    'LBL_MODULE_TITLE' => 'Empleats: Inici',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca de Empleats',
    'LBL_LIST_FORM_TITLE' => 'Empleats',
    'LBL_NEW_FORM_TITLE' => 'Nou Empleat',
    'LBL_LOGIN' => 'Accedir',
    'LBL_RESET_PREFERENCES' => 'Restableix Preferències per defecte',
    'LBL_TIME_FORMAT' => 'Format Hora:',
    'LBL_DATE_FORMAT' => 'Format Data:',
    'LBL_TIMEZONE' => 'Zona horària:',
    'LBL_CURRENCY' => 'Moneda:',
    'LBL_LIST_NAME' => 'Nom',
    'LBL_LIST_LAST_NAME' => 'Cognom',
    'LBL_LIST_EMPLOYEE_NAME' => 'Nom de l\'empleat',
    'LBL_LIST_DEPARTMENT' => 'Departament',
    'LBL_LIST_REPORTS_TO_NAME' => 'Informa a',
    'LBL_LIST_EMAIL' => 'Correu electrònic',
    'LBL_LIST_USER_NAME' => 'Nom d\'Usuari',
    'LBL_ERROR' => 'Error:',
    'LBL_PASSWORD' => 'Contrasenya:',
    'LBL_USER_NAME' => 'Nom d\'Usuari:',
    'LBL_USER_TYPE' => 'Tipus d\'usuari',
    'LBL_FIRST_NAME' => 'Nom:',
    'LBL_LAST_NAME' => 'Cognom:',
    'LBL_THEME' => 'Tema:',
    'LBL_LANGUAGE' => 'Idioma:',
    'LBL_ADMIN' => 'Administrador:',
    'LBL_EMPLOYEE_INFORMATION' => 'Informació de l\'empleat',
    'LBL_OFFICE_PHONE' => 'Tel. Oficina:',
    'LBL_REPORTS_TO' => 'Informa a Id:',
    'LBL_REPORTS_TO_NAME' => 'Informa a:',
    'LBL_OTHER_PHONE' => 'Tel. Alternatiu:',
    'LBL_NOTES' => 'Notes:',
    'LBL_DEPARTMENT' => 'Departament:',
    'LBL_TITLE' => 'Càrrec:',
    'LBL_ANY_ADDRESS' => 'Qualsevol adreça:',
    'LBL_ANY_PHONE' => 'Qualsevol telèfon:',
    'LBL_ANY_EMAIL' => 'Qualsevol adreça electrònica:',
    'LBL_ADDRESS' => 'Adreça:',
    'LBL_CITY' => 'Ciutat:',
    'LBL_STATE' => 'Estat o regió:',
    'LBL_POSTAL_CODE' => 'Codi postal:',
    'LBL_COUNTRY' => 'País:',
    'LBL_NAME' => 'Nom:',
    'LBL_MOBILE_PHONE' => 'Mòbil:',
    'LBL_FAX' => 'Fax:',
    'LBL_EMAIL' => 'Adreça de correu electrònic:',
    'LBL_EMAIL_LINK_TYPE' => 'Client de correu electrònic',
    'LBL_EMAIL_LINK_TYPE_HELP' => '<b>Client de correu electrònic de SuiteCRM:</b> Enviar correus utilitzant el client de correu de l\'aplicació de SuiteCRM.<br><b>Client de correu extern:</b> Enviar correu electrònic utilitzant un client de correu extern a l\'aplicació SuiteCRM, com Microsoft Outlook.',
    'LBL_HOME_PHONE' => 'Tel. Casa:',
    'LBL_WORK_PHONE' => 'Tel. Feina:',
    'LBL_EMPLOYEE_STATUS' => 'Estat de l\'empleat:',
    'LBL_PRIMARY_ADDRESS' => 'Adreça principal:',
    'LBL_SAVED_SEARCH' => 'Cerques i dissenys desats',
    'LBL_MESSENGER_ID' => 'Nom MI:',
    'LBL_MESSENGER_TYPE' => 'Tipus MI:',
    'ERR_LAST_ADMIN_1' => 'L\'empleat "',
    'ERR_LAST_ADMIN_2' => '" és l\'últim empleat amb permisos d\'administrador. Almenys un empleat ha de ser un administrador.',
    'LNK_NEW_EMPLOYEE' => 'Crear Empleat',
    'LNK_EMPLOYEE_LIST' => 'Empleats',
    'ERR_DELETE_RECORD' => 'Heu d\'especificar un número de registre per eliminar l\'empleat.',
    'LBL_LIST_EMPLOYEE_STATUS' => 'Estat',

    'LBL_SUITE_LOGIN' => 'És usuari',
    'LBL_RECEIVE_NOTIFICATIONS' => 'Notificar al seu Assignat',
    'LBL_IS_ADMIN' => 'És administrador',
    'LBL_GROUP' => 'Usuari de Grup',
    'LBL_PHOTO' => 'Foto',
    'LBL_DELETE_USER_CONFIRM' => 'Aquest empleat també és un usuari. Eliminant aquest empleat també s\'eliminarà l\'usuari i l\'usuari no podrà seguir accedint a l\'aplicació. Desitja continuar amb l\'eliminació d\'aquest registre?',
    'LBL_DELETE_EMPLOYEE_CONFIRM' => 'Està segur que vol eliminar aquest empleat?',
    'LBL_ONLY_ACTIVE' => 'Empleats actius',
    'LBL_SELECT' => 'Seleccionar' /*for 508 compliance fix*/,
    'LBL_AUTHENTICATE_ID' => 'Id d\'autenticació',
    'LBL_EXT_AUTHENTICATE' => 'Autenticació externa',
    'LBL_GROUP_USER' => 'Usuari de Grup',
    'LBL_LIST_ACCEPT_STATUS' => 'Estat d\'acceptació',
    'LBL_MODIFIED_BY' => 'Modificat Per',
    'LBL_MODIFIED_BY_ID' => 'Modificat per Id',
    'LBL_CREATED_BY_NAME' => 'Creat Per', //bug48978
    'LBL_PORTAL_ONLY_USER' => 'Usuari del API del portal',
    'LBL_PSW_MODIFIED' => 'Contrasenya canviada per última vegada',
    'LBL_SHOW_ON_EMPLOYEES' => 'Mostra el registre d\'empleats',
    'LBL_USER_HASH' => 'Clau de pas',
    'LBL_SYSTEM_GENERATED_PASSWORD' => 'Contrasenya generada pel sistema',
    'LBL_DESCRIPTION' => 'Descripció',
    'LBL_FAX_PHONE' => 'Fax',
    'LBL_STATUS' => 'Estat',
    'LBL_ADDRESS_CITY' => 'Ciutat',
    'LBL_ADDRESS_COUNTRY' => 'País',
    'LBL_ADDRESS_INFORMATION' => 'Informació d\'adreça',
    'LBL_ADDRESS_POSTALCODE' => 'Codi postal',
    'LBL_ADDRESS_STATE' => 'Estat/Província',
    'LBL_ADDRESS_STREET' => 'Carrer',

    'LBL_DATE_MODIFIED' => 'Última Modificació',
    'LBL_DATE_ENTERED' => 'Data Creació',
    'LBL_DELETED' => 'Esborrat',

    'LBL_BUTTON_SELECT' => 'Seleccionar',
    'LBL_BUTTON_CLEAR' => 'Netejar',

    'LBL_CONTACTS_SYNC' => 'Sincronització de contacte',
    'LBL_OAUTH_TOKENS' => 'Tokens d\'OAuth',
    'LBL_PROJECT_USERS_1_FROM_PROJECT_TITLE' => 'Usuaris del projecte a partir del nom del projecte',
    'LBL_PROJECT_CONTACTS_1_FROM_CONTACTS_TITLE' => 'Contactes del projecte a partir del nom de contactes',
    'LBL_ROLES' => 'Rols',
    'LBL_SECURITYGROUPS' => 'Grups de Seguretat',
    'LBL_PROSPECT_LIST' => 'Públic Objectiu',

    'LBL_FACTOR_AUTH_INTERFACE' => "Interfície d'Autenticació de Doble Factor:",
    'LBL_EDITOR_TYPE' => 'Editor',
);
