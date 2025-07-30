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
    'ERR_NO_OPPS' => 'Por favor, cree al menos una Oportunidad para ver sus gráficos.',
    'LBL_ALL_OPPORTUNITIES' => 'El valor total de todas las oportunidades es',
    'LBL_CHART_TYPE' => 'Tipo de Gráfico',
    'LBL_CREATED_ON' => 'Ejecutado por última vez el',
    'LBL_CLOSE_DATE_START' => 'Fecha Estimada de Cierre - Desde:',
    'LBL_CLOSE_DATE_END' => 'Fecha Estimada de Cierre - Hasta:',
    'LBL_DATE_END' => 'Fecha de Fin:',
    'LBL_DATE_RANGE_TO' => 'a',
    'LBL_DATE_RANGE' => 'El rango de fechas es',
    'LBL_DATE_START' => 'Fecha de Inicio:',
    'LBL_EDIT' => 'Editar',
    'LBL_LEAD_SOURCE_BY_OUTCOME_DESC' => 'Muestra las cantidades acumuladas de oportunidades por la toma de contacto seleccionada por el resultado para los usuarios seleccionados. El resultado se basa en si el etapa de venta es Ganado, Perdido o cualquier otro valor.',
    'LBL_LEAD_SOURCE_BY_OUTCOME' => 'Todas las Oportunidades por Toma de Contacto por Resultado',
    'LBL_LEAD_SOURCE_FORM_DESC' => 'Muestra las cantidades acumuladas de oportunidades por la toma de contacto seleccionada por los usuarios seleccionados.',
    'LBL_LEAD_SOURCE_FORM_TITLE' => 'Todas las oportunidades por Toma de Contacto',
    'LBL_LEAD_SOURCE_OTHER' => 'Otro',
    'LBL_LEAD_SOURCES' => 'Tomas de contacto:',
    'LBL_MODULE_NAME' => 'Cuadro de Mando',
    'LBL_MODULE_TITLE' => 'Cuadro de Mando: Inicio',
    'LBL_MONTH_BY_OUTCOME_DESC' => 'Muestra las cantidades acumuladas de oportunidades por mes y por resultado para los usuarios seleccionados donde la fecha estimada de cierre está dentro del rango de fechas especificado.  El resultado se basa en si el etapa de venta es Ganado, Perdido o cualquier otro valor.',
    'LBL_OPP_SIZE' => 'Valor de la oportunidad en',
    'LBL_OPP_THOUSANDS' => 'K',
    'LBL_OPPS_IN_LEAD_SOURCE' => 'oportunidades donde la toma de contacto es',
    'LBL_OPPS_IN_STAGE' => 'en donde la etapa de venta es',
    'LBL_OPPS_OUTCOME' => 'donde el resultado es',
    'LBL_OPPS_WORTH' => 'valor total de oportunidades',
    'LBL_PIPELINE_FORM_TITLE_DESC' => 'Muestra las cantidades acumuladas por los etapas de venta seleccionados para sus oportunidades donde la fecha de cierre esperada está dentro del rango de fechas especificado.',
    'LBL_REFRESH' => 'Actualizar',
    'LBL_ROLLOVER_DETAILS' => 'Mueva el cursor sobre una barra para más detalles.',
    'LBL_ROLLOVER_WEDGE_DETAILS' => 'Mueva el cursor sobre una sección para más detalles.',
    'LBL_SALES_STAGE_FORM_DESC' => 'Muestra las cantidades acumuladas de oportunidades por los etapas de venta seleccionados para los usuarios seleccionados donde la fecha estimada de cierre está dentro del rango de fechas especificado.',
    'LBL_SALES_STAGE_FORM_TITLE' => 'Proceso por Etapa de Ventas',
    'LBL_SALES_STAGES' => 'Etapas de venta:',
    'LBL_TOTAL_PIPELINE' => 'Total en pipeline',
    'LBL_USERS' => 'Usuarios:',
    'LBL_YEAR_BY_OUTCOME' => 'Embudo por Mes por Resultado',
    'LBL_YEAR' => 'Año:',
    'LNK_NEW_ACCOUNT' => 'Crear una cuenta',
    'LNK_NEW_CALL' => 'Registrar Llamada',
    'LNK_NEW_CASE' => 'Nuevo Caso',
    'LNK_NEW_CONTACT' => 'Nuevo Contacto',
    'LNK_NEW_LEAD' => 'Crear Cliente potencial',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_NEW_NOTE' => 'Nueva Nota o Adjunto',
    'LNK_NEW_OPPORTUNITY' => 'Nueva Oportunidad',
    'LNK_NEW_TASK' => 'Nueva Tarea',
    'NTC_NO_LEGENDS' => 'Ninguno',

    'LBL_TITLE' => 'Título',
    'LBL_MY_MODULES_USED_SIZE' => 'Número de Accesos',

    'LBL_CHART_PIPELINE_BY_SALES_STAGE' => 'Proceso por Etapa de Ventas',
    'LBL_CHART_LEAD_SOURCE_BY_OUTCOME' => 'Toma de Contacto por Resultado',
    'LBL_CHART_OUTCOME_BY_MONTH' => 'Resultado por Mes',
    'LBL_CHART_PIPELINE_BY_LEAD_SOURCE' => 'Embudo por Fuente de Oportunidad',
    'LBL_CHART_MY_PIPELINE_BY_SALES_STAGE' => 'Mi Proceso por Etapa de Ventas',
    'LBL_CHART_MY_MODULES_USED_30_DAYS' => 'Mis Módulos Utilizados (Últimos 30 Días)',
);
