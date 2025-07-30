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
    'LBL_ALL_MODULES' => 'Todos', //rost fix
    'LBL_ASSIGNED_TO_ID' => 'Id de usuario asignado',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_DATE_MODIFIED' => 'Fecha de Modificación',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado por Nombre',
    'LBL_CREATED' => 'Creado por',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_DELETED' => 'Borrado',
    'LBL_NONINHERITABLE' => 'No heredable',
    'LBL_LIST_NONINHERITABLE' => 'No heredable',
    'LBL_NAME' => 'Nombre',
    'LBL_CREATED_USER' => 'Creado por el Usuario',
    'LBL_MODIFIED_USER' => 'Modificado por el Usuario',
    'LBL_LIST_FORM_TITLE' => 'Grupos de Seguridad',
    'LBL_MODULE_NAME' => 'Grupos de Seguridad',
    'LBL_MODULE_TITLE' => 'Grupos de Seguridad',
    'LNK_NEW_RECORD' => 'Crea un Grupo de Seguridad',
    'LNK_LIST' => 'Ver Grupos de Seguridad',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Grupos de Seguridad',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de Seguridad',
    'LBL_USERS' => 'Usuarios',
    'LBL_USERS_SUBPANEL_TITLE' => 'Usuarios',
    'LBL_ROLES_SUBPANEL_TITLE' => 'Roles',
    'LBL_ROLES' => 'Roles',

    'LBL_CONFIGURE_SETTINGS' => 'Configurar',
    'LBL_ADDITIVE' => 'Privilegios agregados',
    'LBL_ADDITIVE_DESC' => 'El usuario tiene la suma de privilegios de todos los roles y grupos asignados a él.',
    'LBL_STRICT_RIGHTS' => 'Privilegios estrictos',
    'LBL_STRICT_RIGHTS_DESC' => 'Si un usuario es miembro de varios grupos, solamente se le aplicarán los privilegios del grupo asignado al registro en curso.',
    'LBL_USER_ROLE_PRECEDENCE' => 'Prioridad del rol de usuario',
    'LBL_USER_ROLE_PRECEDENCE_DESC' => 'Cualquier rol asignado directamente al usuario tendrá prioridad sobre cualquier rol de grupo.',
    'LBL_INHERIT_TITLE' => 'Reglas de herencia de grupo',
    'LBL_INHERIT_TITLE_DESC' => 'Las reglas de herencia de grupos se aplican exclusivamente en el momento de la creación del registro.',
    'LBL_INHERIT_CREATOR' => 'Heredar del usuario creador',
    'LBL_INHERIT_CREATOR_DESC' => 'El registro heredará todos los grupos del usuario que lo ha creado.',
    'LBL_INHERIT_PARENT' => 'Heredar del registro padre',
    'LBL_INHERIT_PARENT_DESC' => 'El registro heredará todos los grupos de cualquier registro relacionado con él.',
    'LBL_USER_POPUP' => 'Ventana de grupos del nuevo usuario',
    'LBL_USER_POPUP_DESC' => 'Al crear un nuevo usuario, muestra una ventana emergente para asignar el usuario a los grupos pertinentes.',
    'LBL_INHERIT_ASSIGNED' => 'Heredar del usuario asignado',
    'LBL_INHERIT_ASSIGNED_DESC' => 'El registro heredará todos los grupos del usuario asignado.',
    'LBL_POPUP_SELECT' => 'Mostrar selector de grupos',
    'LBL_POPUP_SELECT_DESC' => 'Cuando un registro es creado por parte de un usuario que pertenece a más de un grupo, se  muestra un selector de grupos en pantalla. En caso contrario, el registro hereda directamente el único grupo del usuario.',
    'LBL_FILTER_USER_LIST' => 'Filtrar la lista de usuarios',
    'LBL_FILTER_USER_LIST_DESC' => "Los usuarios no administradores pueden asignar registros solamente a los usuarios de los grupos a los que pertenecen.",

    'LBL_DEFAULT_GROUP_TITLE' => 'Grupo por defecto para nuevos registros',
    'LBL_ADD_BUTTON_LABEL' => 'Agregar',
    'LBL_REMOVE_BUTTON_LABEL' => 'Quitar',
    'LBL_GROUP' => 'Grupo:',
    'LBL_MODULE' => 'Módulo:',

    'LBL_MASS_ASSIGN' => 'Grupos de Seguridad: Asignación masiva',
    'LBL_ASSIGN' => 'Asignar',
    'LBL_REMOVE' => 'Quitar',
    'LBL_ASSIGN_CONFIRM' => '¿Está seguro que quiere agregar a este Grupo de Seguridad (los) ',
    'LBL_REMOVE_CONFIRM' => '¿Está seguro que quiere quitar de este Grupo de Seguridad (los) ',
    'LBL_CONFIRM_END' => ' registro(s) seleccionado(s)?',

    'LBL_SECURITYGROUP_USER_FORM_TITLE' => 'Grupo de Seguridad / Usuario',
    'LBL_USER_NAME' => 'Nombre de Usuario',
    'LBL_SECURITYGROUP_NAME' => 'Nombre de Grupo de Seguridad',
    'LBL_HOMEPAGE_TITLE' => 'Mensajes del Grupo',
    'LBL_TITLE' => 'Título',
    'LBL_ROWS' => 'Filas',
    'LBL_POST' => 'Enviar',
    'LBL_SELECT_GROUP_ERROR' => 'Seleccione un grupo y vuelva a intentarlo.',

    'LBL_GROUP_SELECT' => 'Seleccione los grupos que deben tener acceso a este registro.',
    'LBL_ERROR_DUPLICATE' => 'Debido a un posible duplicado detectado por SuiteCRM usted tendrá que agregar manualmente los grupos de seguridad de su registro recién creado.',
    'LBL_ERROR_EXPORT_WHERE_CHANGED' => 'La actualización falló porque se modificó el filtro de búsqueda. Inténtelo de nuevo.', // PR 7999

    'LBL_INBOUND_EMAIL' => 'Cuenta de correo electrónico entrante',
    'LBL_INBOUND_EMAIL_DESC' => 'Bloquea las cuentas de correo electrónico entrante en el cliente de correo electrónico para mostrar solo aquellas que pertenecen al mismo grupo que el usuario actual.',
    'LBL_PRIMARY_GROUP' => 'Grupo Principal',
    'LBL_CHECKMARK' => 'Marca de Verificación',

);
