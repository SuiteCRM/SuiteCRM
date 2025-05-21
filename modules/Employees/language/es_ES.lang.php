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
    'LBL_MODULE_NAME' => 'Empleados',
    'LBL_MODULE_TITLE' => 'Empleados: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Empleados',
    'LBL_LIST_FORM_TITLE' => 'Empleados',
    'LBL_NEW_FORM_TITLE' => 'Nuevo Empleado',
    'LBL_LOGIN' => 'Iniciar sesión',
    'LBL_RESET_PREFERENCES' => 'Restablecer Preferencias Por Defecto',
    'LBL_TIME_FORMAT' => 'Formato Hora:',
    'LBL_DATE_FORMAT' => 'Formato Fecha:',
    'LBL_TIMEZONE' => 'Zona Horaria:',
    'LBL_CURRENCY' => 'Moneda:',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_LIST_LAST_NAME' => 'Apellidos',
    'LBL_LIST_EMPLOYEE_NAME' => 'Nombre del Empleado',
    'LBL_LIST_DEPARTMENT' => 'Departamento',
    'LBL_LIST_REPORTS_TO_NAME' => 'Informa a',
    'LBL_LIST_EMAIL' => 'Email',
    'LBL_LIST_USER_NAME' => 'Nombre de Usuario',
    'LBL_ERROR' => 'Error:',
    'LBL_PASSWORD' => 'Contraseña:',
    'LBL_USER_NAME' => 'Nombre de Usuario:',
    'LBL_USER_TYPE' => 'Tipo de Usuario',
    'LBL_FIRST_NAME' => 'Nombre:',
    'LBL_LAST_NAME' => 'Apellidos:',
    'LBL_THEME' => 'Tema:',
    'LBL_LANGUAGE' => 'Idioma:',
    'LBL_ADMIN' => 'Administrador:',
    'LBL_EMPLOYEE_INFORMATION' => 'Información del Empleado',
    'LBL_OFFICE_PHONE' => 'Tel. Oficina:',
    'LBL_REPORTS_TO' => 'Informa a Id:',
    'LBL_REPORTS_TO_NAME' => 'Informa a:',
    'LBL_OTHER_PHONE' => 'Tel. Alternativo:',
    'LBL_NOTES' => 'Notas:',
    'LBL_DEPARTMENT' => 'Departamento:',
    'LBL_TITLE' => 'Puesto de trabajo:',
    'LBL_ANY_ADDRESS' => 'Dirección Alternativa:',
    'LBL_ANY_PHONE' => 'Tel. Alternativo:',
    'LBL_ANY_EMAIL' => 'Cualquier Correo:',
    'LBL_ADDRESS' => 'Dirección:',
    'LBL_CITY' => 'Ciudad:',
    'LBL_STATE' => 'Estado o región:',
    'LBL_POSTAL_CODE' => 'Código Postal:',
    'LBL_COUNTRY' => 'País:',
    'LBL_NAME' => 'Nombre:',
    'LBL_MOBILE_PHONE' => 'Móvil:',
    'LBL_FAX' => 'Fax:',
    'LBL_EMAIL' => 'Correo electrónico:',
    'LBL_EMAIL_LINK_TYPE' => 'Cliente de Correo',
    'LBL_EMAIL_LINK_TYPE_HELP' => '<b>Cliente de Correo SuiteCRM:</b> Enviar correos usando el cliente de correo de la aplicación SuiteCRM.<br><b>Cliente de Correo Externo:</b> Enviar correo usando un cliente de correo externo a la aplicación SuiteCRM, como Microsoft Outlook.',
    'LBL_HOME_PHONE' => 'Tel. Casa:',
    'LBL_WORK_PHONE' => 'Tel. Trabajo:',
    'LBL_EMPLOYEE_STATUS' => 'Estado del Empleado:',
    'LBL_PRIMARY_ADDRESS' => 'Dirección Principal:',
    'LBL_SAVED_SEARCH' => 'Opciones de Diseño',
    'LBL_MESSENGER_ID' => 'Nombre MI:',
    'LBL_MESSENGER_TYPE' => 'Tipo MI:',
    'ERR_LAST_ADMIN_1' => 'El nombre del empleado "',
    'ERR_LAST_ADMIN_2' => '" es el último empleado con permisos de administrador.  Al menos un empleado debe ser un administrador.',
    'LNK_NEW_EMPLOYEE' => 'Crear Empleado',
    'LNK_EMPLOYEE_LIST' => 'Ver Empleados',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de registro para eliminar el empleado.',
    'LBL_LIST_EMPLOYEE_STATUS' => 'Estado del Empleado',

    'LBL_SUITE_LOGIN' => 'Es Usuario',
    'LBL_RECEIVE_NOTIFICATIONS' => 'Notificar al ser Asignado',
    'LBL_IS_ADMIN' => 'Es Administrador',
    'LBL_GROUP' => 'Usuario de Grupo',
    'LBL_PHOTO' => 'Foto',
    'LBL_DELETE_USER_CONFIRM' => 'Este Empleado es un Usuario. Eliminando este Empleado también se eliminara el Usuario y el Usuario no podrá seguir accediendo a la aplicación. ¿Desea continuar con la eliminación de este registro?',
    'LBL_DELETE_EMPLOYEE_CONFIRM' => '¿Está seguro que desea eliminar este Empleado?',
    'LBL_ONLY_ACTIVE' => 'Empleados Activos',
    'LBL_SELECT' => 'Seleccionar' /*for 508 compliance fix*/,
    'LBL_AUTHENTICATE_ID' => 'Id Autenticación',
    'LBL_EXT_AUTHENTICATE' => 'Autenticación Externa',
    'LBL_GROUP_USER' => 'Usuario del Grupo',
    'LBL_LIST_ACCEPT_STATUS' => 'Aceptar Estato',
    'LBL_MODIFIED_BY' => 'Modificado Por',
    'LBL_MODIFIED_BY_ID' => 'Modificado por Id',
    'LBL_CREATED_BY_NAME' => 'Creado por', //bug48978
    'LBL_PORTAL_ONLY_USER' => 'Usuario de la API del Portal',
    'LBL_PSW_MODIFIED' => 'Último Cambio de la Contraseña',
    'LBL_SHOW_ON_EMPLOYEES' => 'Visualización del Registro de Empleados',
    'LBL_USER_HASH' => 'Contraseña:',
    'LBL_SYSTEM_GENERATED_PASSWORD' => 'Contraseña Generada por el Sistema',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_FAX_PHONE' => 'Fax:',
    'LBL_STATUS' => 'Estado',
    'LBL_ADDRESS_CITY' => 'Ciudad',
    'LBL_ADDRESS_COUNTRY' => 'País',
    'LBL_ADDRESS_INFORMATION' => 'Información de Dirección',
    'LBL_ADDRESS_POSTALCODE' => 'Código Postal',
    'LBL_ADDRESS_STATE' => 'Estado/Provincia',
    'LBL_ADDRESS_STREET' => 'Dirección',

    'LBL_DATE_MODIFIED' => 'Fecha de Modificación',
    'LBL_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_DELETED' => 'Eliminado',

    'LBL_BUTTON_SELECT' => 'Seleccionar',
    'LBL_BUTTON_CLEAR' => 'Limpiar',

    'LBL_CONTACTS_SYNC' => 'Sincronización de contacto',
    'LBL_OAUTH_TOKENS' => 'Tokens OAuth',
    'LBL_PROJECT_USERS_1_FROM_PROJECT_TITLE' => 'Usuarios del proyecto a partir del nombre del proyecto',
    'LBL_PROJECT_CONTACTS_1_FROM_CONTACTS_TITLE' => 'Contactos del proyecto a partir del nombre de contactos',
    'LBL_ROLES' => 'Roles',
    'LBL_SECURITYGROUPS' => 'Grupos de Seguridad',
    'LBL_PROSPECT_LIST' => 'Lista de Público Objetivo',

    'LBL_FACTOR_AUTH_INTERFACE' => 'Interfaz de Autenticación de Dos Factores:',
    'LBL_EDITOR_TYPE' => 'Editor',
);
