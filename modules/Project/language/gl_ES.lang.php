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
    'LBL_MODULE_NAME' => 'Proxecto',
    'LBL_MODULE_TITLE' => 'Proxectos: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Busca de Proxectos',
    'LBL_LIST_FORM_TITLE' => 'Lista de Proxectos',
    'LBL_HISTORY_TITLE' => 'Historial',
    'LBL_ID' => 'ID:',
    'LBL_DATE_ENTERED' => 'Data de Creación:',
    'LBL_DATE_MODIFIED' => 'Data Modificación:',
    'LBL_ASSIGNED_USER_ID' => 'Asignado a:',
    'LBL_ASSIGNED_USER_NAME' => 'Director do proxecto:',
    'LBL_MODIFIED_USER_ID' => 'Modificado por:',
    'LBL_CREATED_BY' => 'Creado por:',
    'LBL_NAME' => 'Nome:',
    'LBL_DESCRIPTION' => 'Descrición:',
    'LBL_DELETED' => 'Eliminado:',
    'LBL_DATE' => 'Data',
    'LBL_DATE_START' => 'Data Inicio:',
    'LBL_DATE_END' => 'Data Fin:',
    'LBL_PRIORITY' => 'Prioridade:',
    'LBL_LIST_NAME' => 'Nome',
    'LBL_LIST_TOTAL_ESTIMATED_EFFORT' => 'Traballo Total Estimado (h)',
    'LBL_LIST_TOTAL_ACTUAL_EFFORT' => 'Traballo Total Real (h)',
    'LBL_LIST_END_DATE' => 'Data Fin',
    'LBL_PROJECT_SUBPANEL_TITLE' => 'Proxectos',
    'LBL_PROJECT_TASK_SUBPANEL_TITLE' => 'Tarefas de Proxecto',
    'LBL_OPPORTUNITY_SUBPANEL_TITLE' => 'Oportunidades',
    'LBL_PROJECT_PREDECESSOR_NONE' => 'Ningún',
    'LBL_ALL_PROJECTS' => 'Todos os proxectos',
    'LBL_ALL_USERS' => 'Todos os usuarios',
    'LBL_ALL_CONTACTS' => 'Todos os contactos',

    // quick create label
    'LBL_NEW_FORM_TITLE' => 'Novo Proxecto',
    'LNK_NEW_PROJECT' => 'Crear Proxecto',
    'LNK_PROJECT_LIST' => 'Ver Lista de Proxectos',
    'LNK_NEW_PROJECT_TASK' => 'Crear Tarefa de Proxecto',
    'LNK_PROJECT_TASK_LIST' => 'Ver Tarefas de Proxecto',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Proxectos',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactos',
    'LBL_ACCOUNTS_SUBPANEL_TITLE' => 'Contas',
    'LBL_OPPORTUNITIES_SUBPANEL_TITLE' => 'Oportunidades',
    'LBL_CASES_SUBPANEL_TITLE' => 'Casos',
    'LBL_BUGS_SUBPANEL_TITLE' => 'Incidencias',
    'LBL_TASK_ID' => 'ID',
    'LBL_TASK_NAME' => 'Nome de Tarefa',
    'LBL_DURATION' => 'Duración',
    'LBL_ACTUAL_DURATION' => 'Duración Real',
    'LBL_START' => 'Inicio',
    'LBL_FINISH' => 'Fin',
    'LBL_PREDECESSORS' => 'Predecesoras',
    'LBL_PERCENT_COMPLETE' => '% Completado',
    'LBL_MORE' => 'Máis...',
    'LBL_OPPORTUNITIES' => 'Oportunidades',
    'LBL_NEXT_WEEK' => 'Seguinte',
    'LBL_PROJECT_INFORMATION' => 'Visión xeral do proxecto',
    'LBL_EDITLAYOUT' => 'Editar deseño' /*for 508 compliance fix*/,
    'LBL_PROJECT_TASKS_SUBPANEL_TITLE' => 'Tarefas de Proxecto',
    'LBL_VIEW_GANTT_TITLE' => 'Vista Gantt',
    'LBL_VIEW_GANTT_DURATION' => 'Duración',
    'LBL_TASK_TITLE' => 'Editar tarefa',
    'LBL_DURATION_TITLE' => 'Editar a duración',
    'LBL_LAG' => 'Lag',
    'LBL_DAYS' => 'Días',
    'LBL_HOURS' => 'Horas',
    'LBL_MONTHS' => 'Meses',
    'LBL_SUBTASK' => 'Tarefa',
    'LBL_MILESTONE_FLAG' => 'Fito',
    'LBL_ADD_NEW_TASK' => 'Agregar nova tarefa',
    'LBL_DELETE_TASK' => 'Eliminar tarefa',
    'LBL_EDIT_TASK_PROPERTIES' => 'Editar propiedades da tarefa.',
    'LBL_PARENT_TASK_ID' => 'Id Tarefa Pai:',
    'LBL_RESOURCE_CHART' => 'Calendario de recursos',
    'LBL_RELATIONSHIP_TYPE' => 'Tipo de relación',
    'LBL_ASSIGNED_TO' => 'Director do proxecto',
    'LBL_AM_PROJECTTEMPLATES_PROJECT_1_FROM_AM_PROJECTTEMPLATES_TITLE' => 'Plantilla de proxecto',
    'LBL_STATUS' => 'Estado:',
    'LBL_LIST_ASSIGNED_USER_ID' => 'Director do proxecto',
    'LBL_TOOLTIP_PROJECT_NAME' => 'Proxecto',
    'LBL_TOOLTIP_TASK_NAME' => 'Nome de Tarefa',
    'LBL_TOOLTIP_TITLE' => 'Tarefas neste día',
    'LBL_TOOLTIP_TASK_DURATION' => 'Duración',
    'LBL_RESOURCE_TYPE_TITLE_USER' => 'Recurso é un usuario',
    'LBL_RESOURCE_TYPE_TITLE_CONTACT' => 'Recurso é un contacto',
    'LBL_RESOURCE_CHART_PREVIOUS_MONTH' => 'Mes Anterior',
    'LBL_RESOURCE_CHART_NEXT_MONTH' => 'Próximo mes',
    'LBL_RESOURCE_CHART_WEEK' => 'Semana',
    'LBL_RESOURCE_CHART_DAY' => 'Día',
    'LBL_RESOURCE_CHART_WARNING' => 'Recursos non foron asignados a un proxecto.',
    'LBL_PROJECT_DELETE_MSG' => '¿Está seguro que desexa eliminar este proxecto e as súas tarefas relacionadas?',
    'LBL_LIST_MY_PROJECT' => 'Os Meus Proxectos',
    'LBL_LIST_ASSIGNED_USER' => 'Director do proxecto',
    'LBL_UNASSIGNED' => 'Sen Asignar',
    'LBL_PROJECT_USERS_1_FROM_USERS_TITLE' => 'Recursos',

    'LBL_EMAIL' => 'Email',
    'LBL_PHONE' => 'Teléfono de Oficina:',
    'LBL_ADD_BUTTON' => 'Engadir',
    'LBL_ADD_INVITEE' => 'Engadir recurso',
    'LBL_FIRST_NAME' => 'Nome',
    'LBL_LAST_NAME' => 'Apelidos',
    'LBL_SEARCH_BUTTON' => 'Busca',
    'LBL_EMPTY_SEARCH_RESULT' => 'Sentímolo, non se encontraron resultados. Por favor cree unha invitación abaixo.',
    'LBL_CREATE_INVITEE' => 'Crear novo recurso',
    'LBL_CREATE_CONTACT' => 'Novo Contacto',
    'LBL_CREATE_AND_ADD' => 'Crear e Engadir',
    'LBL_CANCEL_CREATE_INVITEE' => 'Cancelar',
    'LBL_NO_ACCESS' => 'No ten permisos para crear rexistros no módulo $module',
    'LBL_SCHEDULING_FORM_TITLE' => 'Lista de recursos',
    'LBL_REMOVE' => 'Quitar',
    'LBL_VIEW_DETAIL' => 'Ver Detalles',
    'LBL_OVERRIDE_BUSINESS_HOURS' => 'Considerar días laborais',

    'LBL_IMPORT_PROJECTS' => 'Importar Proxectos',

    'LBL_PROJECTS_SEARCH' => 'Buscar proxectos',
    'LBL_USERS_SEARCH' => 'Buscar usuarios',
    'LBL_CONTACTS_SEARCH' => 'Seleccionar contactos',
    'LBL_RESOURCE_CHART_SEARCH_BUTTON' => 'Busca',

    'LBL_CHART_TYPE' => 'Tipo',
    'LBL_CHART_WEEKLY' => 'Semanal',
    'LBL_CHART_MONTHLY' => 'Mensual',
    'LBL_CHART_QUARTERLY' => 'Trimestral',

    'LBL_RESOURCE_CHART_MONTH' => 'Mes',
    'LBL_RESOURCE_CHART_QUARTER' => 'Trimestre',

    'LBL_PROJECT_CONTACTS_1_FROM_CONTACTS_TITLE' => 'Contactos do proxecto a partir do nome de contactos',
    'LBL_AM_PROJECTTEMPLATES_PROJECT_1_FROM_PROJECT_TITLE' => 'Plantilla de proxecto: Proxecto a partir do nome do proxecto',
    'LBL_AOS_QUOTES_PROJECT' => 'Presupostos: Proxecto',

    'LBL_ASCENDING' => 'Ascendente',
    'LBL_DESCENDING' => 'Descendente',
);
