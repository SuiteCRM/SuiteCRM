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
    'LBL_MODULE_NAME' => 'Projecte',
    'LBL_MODULE_TITLE' => 'Projecte Inici',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca de Projectes',
    'LBL_LIST_FORM_TITLE' => 'Llista de Projectes',
    'LBL_HISTORY_TITLE' => 'Historial',
    'LBL_ID' => 'ID:',
    'LBL_DATE_ENTERED' => 'Data Creació:',
    'LBL_DATE_MODIFIED' => 'Data Modificació:',
    'LBL_ASSIGNED_USER_ID' => 'Assignat a:',
    'LBL_ASSIGNED_USER_NAME' => 'Cap del projecte:',
    'LBL_MODIFIED_USER_ID' => 'Modificat per:',
    'LBL_CREATED_BY' => 'Creat per:',
    'LBL_NAME' => 'Nom:',
    'LBL_DESCRIPTION' => 'Descripció:',
    'LBL_DELETED' => 'Esborrat:',
    'LBL_DATE' => 'Data:',
    'LBL_DATE_START' => 'Data Inici:',
    'LBL_DATE_END' => 'Data Fi:',
    'LBL_PRIORITY' => 'Prioritat:',
    'LBL_LIST_NAME' => 'Nom',
    'LBL_LIST_TOTAL_ESTIMATED_EFFORT' => 'Treball Total Estimat (h)',
    'LBL_LIST_TOTAL_ACTUAL_EFFORT' => 'Treball Total Real (h)',
    'LBL_LIST_END_DATE' => 'Data Fi',
    'LBL_PROJECT_SUBPANEL_TITLE' => 'Projectes',
    'LBL_PROJECT_TASK_SUBPANEL_TITLE' => 'Tasques de Projecte',
    'LBL_OPPORTUNITY_SUBPANEL_TITLE' => 'Oportunitats',
    'LBL_PROJECT_PREDECESSOR_NONE' => 'Cap',
    'LBL_ALL_PROJECTS' => 'Tots els projectes',
    'LBL_ALL_USERS' => 'Tots els usuaris',
    'LBL_ALL_CONTACTS' => 'Tots els contactes',

    // quick create label
    'LBL_NEW_FORM_TITLE' => 'Nou Projecte',
    'LNK_NEW_PROJECT' => 'Crear Projecte',
    'LNK_PROJECT_LIST' => 'Llista de Projectes',
    'LNK_NEW_PROJECT_TASK' => 'Crear Tasca de Projecte',
    'LNK_PROJECT_TASK_LIST' => 'Tasques de Projecte',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Proyectes',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activitats',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactes',
    'LBL_ACCOUNTS_SUBPANEL_TITLE' => 'Comptes',
    'LBL_OPPORTUNITIES_SUBPANEL_TITLE' => 'Oportunitats',
    'LBL_CASES_SUBPANEL_TITLE' => 'Casos',
    'LBL_BUGS_SUBPANEL_TITLE' => 'Incidències',
    'LBL_TASK_ID' => 'ID',
    'LBL_TASK_NAME' => 'Nom de Tasca',
    'LBL_DURATION' => 'Duració',
    'LBL_ACTUAL_DURATION' => 'Duració Real',
    'LBL_START' => 'Inici',
    'LBL_FINISH' => 'Fi',
    'LBL_PREDECESSORS' => 'Anteriors',
    'LBL_PERCENT_COMPLETE' => '% Completat',
    'LBL_MORE' => 'Més...',
    'LBL_OPPORTUNITIES' => 'Oportunitats',
    'LBL_NEXT_WEEK' => 'Següent',
    'LBL_PROJECT_INFORMATION' => 'Visió general del projecte',
    'LBL_EDITLAYOUT' => 'Editar Diseny' /*for 508 compliance fix*/,
    'LBL_PROJECT_TASKS_SUBPANEL_TITLE' => 'Tasques de Projecte',
    'LBL_VIEW_GANTT_TITLE' => 'Veure gantt',
    'LBL_VIEW_GANTT_DURATION' => 'Durada',
    'LBL_TASK_TITLE' => 'Editar tasca',
    'LBL_DURATION_TITLE' => 'Editar la durada',
    'LBL_LAG' => 'Retard',
    'LBL_DAYS' => 'Dies',
    'LBL_HOURS' => 'Hores',
    'LBL_MONTHS' => 'Messos',
    'LBL_SUBTASK' => 'Tasques',
    'LBL_MILESTONE_FLAG' => 'Fita',
    'LBL_ADD_NEW_TASK' => 'Afegir nova tasca',
    'LBL_DELETE_TASK' => 'Eliminar tasca',
    'LBL_EDIT_TASK_PROPERTIES' => 'Editar les propietats de la tasca.',
    'LBL_PARENT_TASK_ID' => 'Id tasca pare',
    'LBL_RESOURCE_CHART' => 'Calendari del recurs',
    'LBL_RELATIONSHIP_TYPE' => 'Tipus de relació',
    'LBL_ASSIGNED_TO' => 'Director del projecte',
    'LBL_AM_PROJECTTEMPLATES_PROJECT_1_FROM_AM_PROJECTTEMPLATES_TITLE' => 'Plantilla de projecte',
    'LBL_STATUS' => 'Estat:',
    'LBL_LIST_ASSIGNED_USER_ID' => 'Director del projecte',
    'LBL_TOOLTIP_PROJECT_NAME' => 'Projectes',
    'LBL_TOOLTIP_TASK_NAME' => 'Nom de Tasca',
    'LBL_TOOLTIP_TITLE' => 'Tasques d\'aquest dia',
    'LBL_TOOLTIP_TASK_DURATION' => 'Durada',
    'LBL_RESOURCE_TYPE_TITLE_USER' => 'El recurs és un usuari',
    'LBL_RESOURCE_TYPE_TITLE_CONTACT' => 'El recurs és un contacte',
    'LBL_RESOURCE_CHART_PREVIOUS_MONTH' => 'Mes Anterior',
    'LBL_RESOURCE_CHART_NEXT_MONTH' => 'Proper Mes',
    'LBL_RESOURCE_CHART_WEEK' => 'Setmana',
    'LBL_RESOURCE_CHART_DAY' => 'Dia',
    'LBL_RESOURCE_CHART_WARNING' => 'No s\'han assignat recursos a aquest projecte.',
    'LBL_PROJECT_DELETE_MSG' => 'Està segur que vol eliminar aquest projecte i les tasques relacionades?',
    'LBL_LIST_MY_PROJECT' => 'Els Meus Projectes',
    'LBL_LIST_ASSIGNED_USER' => 'Director del projecte',
    'LBL_UNASSIGNED' => 'No assignat',
    'LBL_PROJECT_USERS_1_FROM_USERS_TITLE' => 'Recursos de projecte',

    'LBL_EMAIL' => 'Correu electrònic',
    'LBL_PHONE' => 'Telèfon d\'oficina:',
    'LBL_ADD_BUTTON' => 'Afegir',
    'LBL_ADD_INVITEE' => 'Afegeix un recurs',
    'LBL_FIRST_NAME' => 'Nom',
    'LBL_LAST_NAME' => 'Cognoms',
    'LBL_SEARCH_BUTTON' => 'Cerca',
    'LBL_EMPTY_SEARCH_RESULT' => 'Disculpi, no s\'han trobat resultats. Si us plau, creï una invitació a sota.',
    'LBL_CREATE_INVITEE' => 'Crear un recurs',
    'LBL_CREATE_CONTACT' => 'Com a contacte',
    'LBL_CREATE_AND_ADD' => 'Crear i afegir',
    'LBL_CANCEL_CREATE_INVITEE' => 'Cancelar',
    'LBL_NO_ACCESS' => 'Vostè no té permís per a crear $module',
    'LBL_SCHEDULING_FORM_TITLE' => 'Llista de recursos',
    'LBL_REMOVE' => 'Eliminar',
    'LBL_VIEW_DETAIL' => 'Veure Detalls',
    'LBL_OVERRIDE_BUSINESS_HOURS' => 'Considerar dies laborables',

    'LBL_IMPORT_PROJECTS' => 'Importa Projectes',

    'LBL_PROJECTS_SEARCH' => 'Projectes de recerca',
    'LBL_USERS_SEARCH' => 'Cerca usuaris/es',
    'LBL_CONTACTS_SEARCH' => 'Selecciona contacte',
    'LBL_RESOURCE_CHART_SEARCH_BUTTON' => 'Cerca',

    'LBL_CHART_TYPE' => 'Tipus',
    'LBL_CHART_WEEKLY' => 'Setmanal',
    'LBL_CHART_MONTHLY' => 'Mensual',
    'LBL_CHART_QUARTERLY' => 'Trimestral',

    'LBL_RESOURCE_CHART_MONTH' => 'Mes',
    'LBL_RESOURCE_CHART_QUARTER' => 'Trimestre',

    'LBL_PROJECT_CONTACTS_1_FROM_CONTACTS_TITLE' => 'Contactes del projecte a partir del nom de contactes',
    'LBL_AM_PROJECTTEMPLATES_PROJECT_1_FROM_PROJECT_TITLE' => 'Plantilla de projecte: Projecte a partir del títol del projecte',
    'LBL_AOS_QUOTES_PROJECT' => 'Pressupostos: Projecte',

    'LBL_ASCENDING' => 'Ascendent',
    'LBL_DESCENDING' => 'Descendent',
);
