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
    'LBL_ASSIGNED_TO_ID' => 'Id de usuario asignado',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_DATE_MODIFIED' => 'Fecha de Modificación',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado por Nombre',
    'LBL_CREATED' => 'Creado por',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nombre',
    'LBL_CREATED_USER' => 'Creado por el Usuario',
    'LBL_MODIFIED_USER' => 'Modificado por el Usuario',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_EDIT_BUTTON' => 'Editar',
    'LBL_REMOVE' => 'Quitar',
    'LBL_LIST_FORM_TITLE' => 'Lista de Reportes',
    'LBL_MODULE_NAME' => 'Reportes',
    'LBL_MODULE_TITLE' => 'Reportes',
    'LBL_HOMEPAGE_TITLE' => 'Mis Reportes',
    'LNK_NEW_RECORD' => 'Crear Reporte',
    'LNK_LIST' => 'Ver Reportes',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Reportes',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Ver Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_NEW_FORM_TITLE' => 'Nuevo Reporte',
    'LBL_REPORT_MODULE' => 'Módulo Reporteado',
    'LBL_GRAPHS_PER_ROW' => 'Gráficos por fila',
    'LBL_FIELD_LINES' => 'Campos Visualizados',
    'LBL_ADD_FIELD' => 'Agregar Campo',
    'LBL_CONDITION_LINES' => 'Condiciones',
    'LBL_ADD_CONDITION' => 'Agregar Condición',
    'LBL_EXPORT' => 'Exportar',
    'LBL_DOWNLOAD_PDF' => 'Descargar PDF',
    'LBL_ADD_TO_PROSPECT_LIST' => 'Agregar a Lista de Público Objetivo',
    'LBL_AOR_MODULETREE_SUBPANEL_TITLE' => 'Arbol del Módulo',
    'LBL_AOR_FIELDS_SUBPANEL_TITLE' => 'Campos',
    'LBL_AOR_CONDITIONS_SUBPANEL_TITLE' => 'Condiciones',
    'LBL_TOTAL' => 'Total',
    'LBL_AOR_CHARTS_SUBPANEL_TITLE' => 'Gráficos',
    'LBL_ADD_CHART' => 'Añadir carta',
    'LBL_ADD_PARENTHESIS' => 'Añadir paréntesis',// PR 5471 and 6252 to be removed after merged
    'LBL_INSERT_PARENTHESIS' => 'Insertar paréntesis', // PR 5471
    'LBL_CHART_TITLE' => 'Título',
    'LBL_CHART_TYPE' => 'Tipo',
    'LBL_CHART_X_FIELD' => 'Eje X',
    'LBL_CHART_Y_FIELD' => 'Eje Y',
    'LBL_AOR_REPORTS_DASHLET' => 'Reportes',
    'LBL_DASHLET_TITLE' => 'Título',
    'LBL_DASHLET_REPORT' => 'Informe',
    'LBL_DASHLET_CHOOSE_REPORT' => 'Por favor, elija un informe',
    'LBL_DASHLET_SAVE' => 'Guardar',
    'LBL_DASHLET_CHARTS' => 'Gráficos',
    'LBL_DASHLET_ONLY_CHARTS' => 'Sólo mostrar gráficos',
    'LBL_UPDATE_PARAMETERS' => 'Actualizar',
    'LBL_PARAMETERS' => 'Parámetros',
    'LBL_TOOLTIP_DRAG_DROP_ELEMS' => 'Arrastrar y soltar elementos en el área de campo o de condición',
    'LBL_MAIN_GROUPS' => 'Grupo principal:',
    'LBL_CHAR_UNNAMED_DEFAULT_TITLE' => 'Gráfico sin nombre',
    'LBL_REPORT' => 'Informe',
);
