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
    'LBL_MODULE_NAME' => 'Proyecto',
    'LBL_MODULE_TITLE' => 'Proyectos: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Proyectos',
    'LBL_LIST_FORM_TITLE' => 'Lista de Proyectos',
    'LBL_HISTORY_TITLE' => 'Historial',
    'LBL_ID' => 'ID:',
    'LBL_DATE_ENTERED' => 'Fecha de Creación:',
    'LBL_DATE_MODIFIED' => 'Fecha Modificación:',
    'LBL_ASSIGNED_USER_ID' => 'Asignado a:',
    'LBL_ASSIGNED_USER_NAME' => 'Director del proyecto:',
    'LBL_MODIFIED_USER_ID' => 'Modificado por:',
    'LBL_CREATED_BY' => 'Creado por:',
    'LBL_NAME' => 'Nombre:',
    'LBL_DESCRIPTION' => 'Descripción:',
    'LBL_DELETED' => 'Eliminado:',
    'LBL_DATE' => 'Fecha',
    'LBL_DATE_START' => 'Fecha Inicio:',
    'LBL_DATE_END' => 'Fecha Fin:',
    'LBL_PRIORITY' => 'Prioridad:',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_LIST_TOTAL_ESTIMATED_EFFORT' => 'Trabajo Total Estimado (h)',
    'LBL_LIST_TOTAL_ACTUAL_EFFORT' => 'Trabajo Total Real (h)',
    'LBL_LIST_END_DATE' => 'Fecha Fin',
    'LBL_PROJECT_SUBPANEL_TITLE' => 'Proyectos',
    'LBL_PROJECT_TASK_SUBPANEL_TITLE' => 'Tareas de Proyecto',
    'LBL_OPPORTUNITY_SUBPANEL_TITLE' => 'Oportunidades',
    'LBL_PROJECT_PREDECESSOR_NONE' => 'Ninguno',
    'LBL_ALL_PROJECTS' => 'Todos los proyectos',
    'LBL_ALL_USERS' => 'Todos los usuarios',
    'LBL_ALL_CONTACTS' => 'Todos los contactos',

    // quick create label
    'LBL_NEW_FORM_TITLE' => 'Nuevo Proyecto',
    'LNK_NEW_PROJECT' => 'Crear Proyecto',
    'LNK_PROJECT_LIST' => 'Ver Lista de Proyectos',
    'LNK_NEW_PROJECT_TASK' => 'Crear Tarea de Proyecto',
    'LNK_PROJECT_TASK_LIST' => 'Ver Tareas de Proyecto',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Proyectos',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactos',
    'LBL_ACCOUNTS_SUBPANEL_TITLE' => 'Cuentas',
    'LBL_OPPORTUNITIES_SUBPANEL_TITLE' => 'Oportunidades',
    'LBL_CASES_SUBPANEL_TITLE' => 'Casos',
    'LBL_BUGS_SUBPANEL_TITLE' => 'Incidencias',
    'LBL_TASK_ID' => 'ID',
    'LBL_TASK_NAME' => 'Nombre de Tarea',
    'LBL_DURATION' => 'Duración',
    'LBL_ACTUAL_DURATION' => 'Duración Real',
    'LBL_START' => 'Inicio',
    'LBL_FINISH' => 'Fin',
    'LBL_PREDECESSORS' => 'Predecesoras',
    'LBL_PERCENT_COMPLETE' => '% Completado',
    'LBL_MORE' => 'Más...',
    'LBL_OPPORTUNITIES' => 'Oportunidades',
    'LBL_NEXT_WEEK' => 'Siguiente',
    'LBL_PROJECT_INFORMATION' => 'Visión general del proyecto',
    'LBL_EDITLAYOUT' => 'Editar diseño' /*for 508 compliance fix*/,
    'LBL_PROJECT_TASKS_SUBPANEL_TITLE' => 'Tareas de Proyecto',
    'LBL_VIEW_GANTT_TITLE' => 'Vista Gantt',
    'LBL_VIEW_GANTT_DURATION' => 'Duración',
    'LBL_TASK_TITLE' => 'Editar tarea',
    'LBL_DURATION_TITLE' => 'Editar la duración',
    'LBL_LAG' => 'Lag',
    'LBL_DAYS' => 'Días',
    'LBL_HOURS' => 'Horas',
    'LBL_MONTHS' => 'Meses',
    'LBL_SUBTASK' => 'Tarea',
    'LBL_MILESTONE_FLAG' => 'Hito',
    'LBL_ADD_NEW_TASK' => 'Agregar nueva tarea',
    'LBL_DELETE_TASK' => 'Eliminar tarea',
    'LBL_EDIT_TASK_PROPERTIES' => 'Editar propiedades de la tarea.',
    'LBL_PARENT_TASK_ID' => 'Id Tarea Padre:',
    'LBL_RESOURCE_CHART' => 'Calendario de recursos',
    'LBL_RELATIONSHIP_TYPE' => 'Tipo de relación',
    'LBL_ASSIGNED_TO' => 'Director del proyecto',
    'LBL_AM_PROJECTTEMPLATES_PROJECT_1_FROM_AM_PROJECTTEMPLATES_TITLE' => 'Plantilla de proyecto',
    'LBL_STATUS' => 'Estado:',
    'LBL_LIST_ASSIGNED_USER_ID' => 'Director del proyecto',
    'LBL_TOOLTIP_PROJECT_NAME' => 'Proyecto',
    'LBL_TOOLTIP_TASK_NAME' => 'Nombre de Tarea',
    'LBL_TOOLTIP_TITLE' => 'Tareas en este día',
    'LBL_TOOLTIP_TASK_DURATION' => 'Duración',
    'LBL_RESOURCE_TYPE_TITLE_USER' => 'Recurso es un usuario',
    'LBL_RESOURCE_TYPE_TITLE_CONTACT' => 'Recurso es un contacto',
    'LBL_RESOURCE_CHART_PREVIOUS_MONTH' => 'Mes Anterior',
    'LBL_RESOURCE_CHART_NEXT_MONTH' => 'Próximo mes',
    'LBL_RESOURCE_CHART_WEEK' => 'Semana',
    'LBL_RESOURCE_CHART_DAY' => 'Día',
    'LBL_RESOURCE_CHART_WARNING' => 'Recursos no han sido asignados a un proyecto.',
    'LBL_PROJECT_DELETE_MSG' => '¿Está seguro que desea eliminar este proyecto y sus tareas relacionadas?',
    'LBL_LIST_MY_PROJECT' => 'Mis Proyectos',
    'LBL_LIST_ASSIGNED_USER' => 'Director del proyecto',
    'LBL_UNASSIGNED' => 'Sin Asignar',
    'LBL_PROJECT_USERS_1_FROM_USERS_TITLE' => 'Recursos de proyecto',

    'LBL_EMAIL' => 'Email',
    'LBL_PHONE' => 'Teléfono de Oficina:',
    'LBL_ADD_BUTTON' => 'Añadir',
    'LBL_ADD_INVITEE' => 'Añadir recurso',
    'LBL_FIRST_NAME' => 'Nombre',
    'LBL_LAST_NAME' => 'Apellidos',
    'LBL_SEARCH_BUTTON' => 'Búsqueda',
    'LBL_EMPTY_SEARCH_RESULT' => 'Lo sentimos, no se encontraron resultados. Por favor cree una invitación abajo.',
    'LBL_CREATE_INVITEE' => 'Crear nuevo recurso',
    'LBL_CREATE_CONTACT' => 'Nuevo Contacto',
    'LBL_CREATE_AND_ADD' => 'Crear y Añadir',
    'LBL_CANCEL_CREATE_INVITEE' => 'Cancelar',
    'LBL_NO_ACCESS' => 'No tiene permisos para crear registros en el módulo $module',
    'LBL_SCHEDULING_FORM_TITLE' => 'Lista de recursos',
    'LBL_REMOVE' => 'Quitar',
    'LBL_VIEW_DETAIL' => 'Ver Detalles',
    'LBL_OVERRIDE_BUSINESS_HOURS' => 'Considerar días laborales',

    'LBL_IMPORT_PROJECTS' => 'Importar Proyectos',

    'LBL_PROJECTS_SEARCH' => 'Buscar proyectos',
    'LBL_USERS_SEARCH' => 'Buscar usuarios',
    'LBL_CONTACTS_SEARCH' => 'Seleccionar contactos',
    'LBL_RESOURCE_CHART_SEARCH_BUTTON' => 'Búsqueda',

    'LBL_CHART_TYPE' => 'Tipo',
    'LBL_CHART_WEEKLY' => 'Semanal',
    'LBL_CHART_MONTHLY' => 'Mensual',
    'LBL_CHART_QUARTERLY' => 'Trimestral',

    'LBL_RESOURCE_CHART_MONTH' => 'Mes',
    'LBL_RESOURCE_CHART_QUARTER' => 'Trimestre',

    'LBL_PROJECT_CONTACTS_1_FROM_CONTACTS_TITLE' => 'Contactos del proyecto a partir del nombre de contactos',
    'LBL_AM_PROJECTTEMPLATES_PROJECT_1_FROM_PROJECT_TITLE' => 'Plantilla de proyecto: Proyecto a partir del nombre del proyecto',
    'LBL_AOS_QUOTES_PROJECT' => 'Presupuestos: Proyecto',

    'LBL_ASCENDING' => 'Ascendente',
    'LBL_DESCENDING' => 'Descendente',
);
