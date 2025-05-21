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
    'LBL_MODULE_NAME' => 'Tasques de Projecte',
    'LBL_MODULE_TITLE' => 'Tasques de Projecte: Inici',

    'LBL_ID' => 'ID:',
    'LBL_PROJECT_TASK_ID' => 'Id Tasca de Projecte:',
    'LBL_PROJECT_ID' => 'Id Projecte',
    'LBL_DATE_ENTERED' => 'Data Creació:',
    'LBL_DATE_MODIFIED' => 'Data Modificació:',
    'LBL_ASSIGNED_USER_ID' => 'Assignat a:',
    'LBL_MODIFIED_USER_ID' => 'Modificat per:',
    'LBL_CREATED_BY' => 'Creat per:',
    'LBL_NAME' => 'Nom:',
    'LBL_STATUS' => 'Estat:',
    'LBL_DATE_DUE' => 'Data Venciment:',
    'LBL_TIME_DUE' => 'Hora Venciment:',
    'LBL_PREDECESSORS' => 'Predecesors:',
    'LBL_DATE_START' => 'Data Inici:',
    'LBL_DATE_FINISH' => 'Data Fi:',
    'LBL_TIME_START' => 'Hora Inici:',
    'LBL_TIME_FINISH' => 'Hora Fi:',
    'LBL_DURATION' => 'Durada:',
    'LBL_DURATION_UNIT' => 'Unitat de Durada:',
    'LBL_ACTUAL_DURATION' => 'Durada Real:',
    'LBL_PARENT_ID' => 'Projecte:',
    'LBL_PARENT_TASK_ID' => 'Id Tasca Pare:',
    'LBL_PERCENT_COMPLETE' => '% Completat:',
    'LBL_PRIORITY' => 'Prioritat:',
    'LBL_DESCRIPTION' => 'Notes:',
    'LBL_ORDER_NUMBER' => 'Ordre:',
    'LBL_TASK_NUMBER' => 'Número de Tasca:',
    'LBL_TASK_ID' => 'Id Tasca:',
    'LBL_MILESTONE_FLAG' => 'Hito:',
    'LBL_ESTIMATED_EFFORT' => 'Treball Estimat (h):',
    'LBL_ACTUAL_EFFORT' => 'Treball Real (h):',
    'LBL_UTILIZATION' => 'Ocupació (%):',
    'LBL_DELETED' => 'Eliminada:',

    'LBL_LIST_NAME' => 'Nom',
    'LBL_LIST_PARENT_NAME' => 'Projecte',
    'LBL_LIST_PERCENT_COMPLETE' => '% Completat',
    'LBL_LIST_STATUS' => 'Estat',
    'LBL_LIST_ASSIGNED_USER_ID' => 'Assignada a',
    'LBL_LIST_DATE_DUE' => 'Data Venciment',
    'LBL_LIST_PRIORITY' => 'Prioritat',
    'LBL_LIST_CLOSE' => 'Tancar',
    'LBL_PROJECT_NAME' => 'Nom Projecte',

    'LNK_NEW_PROJECT' => 'Crear Projecte',
    'LNK_PROJECT_LIST' => 'Llista de Projectes',
    'LNK_NEW_PROJECT_TASK' => 'Crear Tasca de Projecte',
    'LNK_PROJECT_TASK_LIST' => 'Tasques de Projecte',

    'LBL_LIST_MY_PROJECT_TASKS' => 'Les Meves Tasques de Projecte',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Tasques de Projecte',
    'LBL_NEW_FORM_TITLE' => 'Nova Tasca de Projecte',

    'LBL_HISTORY_TITLE' => 'Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activitats',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',

    'LBL_ASSIGNED_USER_NAME' => 'Assignat a',
    'LBL_PARENT_NAME' => 'Nom de Projecte',
    'LBL_EDITLAYOUT' => 'Editar Diseny' /*for 508 compliance fix*/,
    'LBL_PANEL_TIMELINE' => 'Línia de temps',

    'LBL_SUBTASK' => 'Sub-Tasca',
    'LBL_LAG' => 'Retard',
    'LBL_DAYS' => 'Dies',
    'LBL_HOURS' => 'Hores',
    'LBL_RELATIONSHIP_TYPE' => 'Tipus de Relació',

    'LBL_ASCENDING' => 'Ascendent',
    'LBL_DESCENDING' => 'Descendent',    
);
