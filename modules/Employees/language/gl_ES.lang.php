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
    'LBL_MODULE_NAME' => 'Empregados',
    'LBL_MODULE_TITLE' => 'Empregados: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Busca de Empregados',
    'LBL_LIST_FORM_TITLE' => 'Empregados',
    'LBL_NEW_FORM_TITLE' => 'Novo Empregado',
    'LBL_LOGIN' => 'Iniciar sesión',
    'LBL_RESET_PREFERENCES' => 'Restablecer Preferencias Por Defecto',
    'LBL_TIME_FORMAT' => 'Formato Hora:',
    'LBL_DATE_FORMAT' => 'Formato Data:',
    'LBL_TIMEZONE' => 'Zona Horaria:',
    'LBL_CURRENCY' => 'Moeda:',
    'LBL_LIST_NAME' => 'Nome',
    'LBL_LIST_LAST_NAME' => 'Apelidos',
    'LBL_LIST_EMPLOYEE_NAME' => 'Nome do Empregado',
    'LBL_LIST_DEPARTMENT' => 'Departamento',
    'LBL_LIST_REPORTS_TO_NAME' => 'Informa a',
    'LBL_LIST_EMAIL' => 'Email',
    'LBL_LIST_USER_NAME' => 'Nome de Usuario',
    'LBL_ERROR' => 'Erro:',
    'LBL_PASSWORD' => 'Contrasinal:',
    'LBL_USER_NAME' => 'Nome de Usuario:',
    'LBL_USER_TYPE' => 'Tipo de Usuario',
    'LBL_FIRST_NAME' => 'Nome:',
    'LBL_LAST_NAME' => 'Apelidos:',
    'LBL_THEME' => 'Tema:',
    'LBL_LANGUAGE' => 'Idioma:',
    'LBL_ADMIN' => 'Administrador:',
    'LBL_EMPLOYEE_INFORMATION' => 'Información do Empregado',
    'LBL_OFFICE_PHONE' => 'Tel. Oficina:',
    'LBL_REPORTS_TO' => 'Informa a Id:',
    'LBL_REPORTS_TO_NAME' => 'Informa a:',
    'LBL_OTHER_PHONE' => 'Tel. Alternativo:',
    'LBL_NOTES' => 'Notas:',
    'LBL_DEPARTMENT' => 'Departamento:',
    'LBL_TITLE' => 'Posto de traballo:',
    'LBL_ANY_ADDRESS' => 'Enderezo alternativo:',
    'LBL_ANY_PHONE' => 'Tel. Alternativo:',
    'LBL_ANY_EMAIL' => 'Calquera Correo:',
    'LBL_ADDRESS' => 'Enderezo:',
    'LBL_CITY' => 'Cidade:',
    'LBL_STATE' => 'Estado ou rexión:',
    'LBL_POSTAL_CODE' => 'Código Postal:',
    'LBL_COUNTRY' => 'País:',
    'LBL_NAME' => 'Nome:',
    'LBL_MOBILE_PHONE' => 'Móbil:',
    'LBL_FAX' => 'Fax:',
    'LBL_EMAIL' => 'Correo electrónico:',
    'LBL_EMAIL_LINK_TYPE' => 'Cliente de Correo',
    'LBL_EMAIL_LINK_TYPE_HELP' => '<b>Cliente de Correo SuiteCRM:</b> Enviar correos usando o cliente de correo da aplicación SuiteCRM.<br><b>Cliente de Correo Externo:</b> Enviar correo usando un cliente de correo externo á aplicación SuiteCRM, como Microsoft Outlook.',
    'LBL_HOME_PHONE' => 'Tel. Casa:',
    'LBL_WORK_PHONE' => 'Tel. Traballo:',
    'LBL_EMPLOYEE_STATUS' => 'Estado do Empregado:',
    'LBL_PRIMARY_ADDRESS' => 'Enderezo Principal:',
    'LBL_SAVED_SEARCH' => 'Opcións de Deseño',
    'LBL_MESSENGER_ID' => 'Nome MI:',
    'LBL_MESSENGER_TYPE' => 'Tipo MI:',
    'ERR_LAST_ADMIN_1' => 'O nome do empregado "',
    'ERR_LAST_ADMIN_2' => '" é o último empregado con permisos de administrador.  Polo menos un empregado debe ser un administrador.',
    'LNK_NEW_EMPLOYEE' => 'Crear Empregado',
    'LNK_EMPLOYEE_LIST' => 'Ver Empregados',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de rexistro para eliminar o empregado.',
    'LBL_LIST_EMPLOYEE_STATUS' => 'Estado do Empregado',

    'LBL_SUITE_LOGIN' => 'É Usuario',
    'LBL_RECEIVE_NOTIFICATIONS' => 'Notificar ao ser Asignado',
    'LBL_IS_ADMIN' => 'É Administrador',
    'LBL_GROUP' => 'Usuario de Grupo',
    'LBL_PHOTO' => 'Foto',
    'LBL_DELETE_USER_CONFIRM' => 'Este Empregado é un Usuario. Eliminando este Empregado tamén se eliminara o Usuario e o Usuario non poderá seguir accedendo á aplicación. ¿Desexa continuar coa eliminación deste rexistro?',
    'LBL_DELETE_EMPLOYEE_CONFIRM' => '¿Está seguro que desexa eliminar este Empregado?',
    'LBL_ONLY_ACTIVE' => 'Empregados Activos',
    'LBL_SELECT' => 'Seleccionar' /*for 508 compliance fix*/,
    'LBL_AUTHENTICATE_ID' => 'Id Autenticación',
    'LBL_EXT_AUTHENTICATE' => 'Autenticación Externa',
    'LBL_GROUP_USER' => 'Usuario do Grupo',
    'LBL_LIST_ACCEPT_STATUS' => 'Aceptar Estato',
    'LBL_MODIFIED_BY' => 'Modificado Por',
    'LBL_MODIFIED_BY_ID' => 'Modificado por Id',
    'LBL_CREATED_BY_NAME' => 'Creado por', //bug48978
    'LBL_PORTAL_ONLY_USER' => 'Usuario da API do Portal',
    'LBL_PSW_MODIFIED' => 'Último Cambio da Contrasinal',
    'LBL_SHOW_ON_EMPLOYEES' => 'Visualización do Rexistro de Empregados',
    'LBL_USER_HASH' => 'Contrasinal:',
    'LBL_SYSTEM_GENERATED_PASSWORD' => 'Contrasinal Xerada polo Sistema',
    'LBL_DESCRIPTION' => 'Descrición',
    'LBL_FAX_PHONE' => 'Fax:',
    'LBL_STATUS' => 'Estado',
    'LBL_ADDRESS_CITY' => 'Cidade',
    'LBL_ADDRESS_COUNTRY' => 'País',
    'LBL_ADDRESS_INFORMATION' => 'Información de Enderezo',
    'LBL_ADDRESS_POSTALCODE' => 'Código Postal',
    'LBL_ADDRESS_STATE' => 'Estado/Provincia',
    'LBL_ADDRESS_STREET' => 'Enderezo',

    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_DATE_ENTERED' => 'Data de Creación',
    'LBL_DELETED' => 'Eliminado',

    'LBL_BUTTON_SELECT' => 'Seleccionar',
    'LBL_BUTTON_CLEAR' => 'Limpar',

    'LBL_CONTACTS_SYNC' => 'Sincronización de contacto',
    'LBL_OAUTH_TOKENS' => 'Tokens OAuth',
    'LBL_PROJECT_USERS_1_FROM_PROJECT_TITLE' => 'Usuarios do proxecto a partir do nome do proxecto',
    'LBL_PROJECT_CONTACTS_1_FROM_CONTACTS_TITLE' => 'Contactos do proxecto a partir do nome de contactos',
    'LBL_ROLES' => 'Roles',
    'LBL_SECURITYGROUPS' => 'Grupos de Seguridade',
    'LBL_PROSPECT_LIST' => 'Lista de Público Obxectivo',

    'LBL_FACTOR_AUTH_INTERFACE' => 'Interfaz de Autenticación de Dos Factores:',
    'LBL_EDITOR_TYPE' => 'Editor',
);
