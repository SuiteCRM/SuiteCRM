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

    'LBL_SHAREDWEEK' => 'Semana compartida',
    'LBL_SHAREDMONTH' => 'Mes compartido',

    'LBL_MODULE_NAME' => 'Calendario',
    'LBL_MODULE_TITLE' => 'Calendario',
    'LNK_NEW_CALL' => 'Programar Llamada',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_NEW_TASK' => 'Crear Tarea',
    'LNK_CALL_LIST' => 'Llamadas',
    'LNK_MEETING_LIST' => 'Ver Reuniones',
    'LNK_TASK_LIST' => 'Ver Tareas',
    'LNK_TASK' => 'Tarea',
    'LNK_TASK_VIEW' => 'Ver Tarea',
    'LNK_EVENT' => 'Evento',
    'LNK_EVENT_VIEW' => 'Ver Evento',
    'LNK_VIEW_CALENDAR' => 'Hoy',
    'LNK_IMPORT_CALLS' => 'Importar Llamadas',
    'LNK_IMPORT_MEETINGS' => 'Importar Reuniones',
    'LNK_IMPORT_TASKS' => 'Importar Tareas',
    'LBL_MONTH' => 'Mes',
    'LBL_AGENDADAY' => 'Día',
    'LBL_YEAR' => 'Año',

    'LBL_AGENDAWEEK' => 'Semana',
    'LBL_PREVIOUS_MONTH' => 'Mes Anterior',
    'LBL_PREVIOUS_DAY' => 'Día Anterior',
    'LBL_PREVIOUS_YEAR' => 'Año Anterior',
    'LBL_PREVIOUS_WEEK' => 'Semana Anterior',
    'LBL_NEXT_MONTH' => 'Mes Siguiente',
    'LBL_NEXT_DAY' => 'Día Siguiente',
    'LBL_NEXT_YEAR' => 'Año Siguiente',
    'LBL_NEXT_WEEK' => 'Semana Siguiente',
    'LBL_AM' => 'AM',
    'LBL_PM' => 'PM',
    'LBL_SCHEDULED' => 'Planificado',
    'LBL_BUSY' => 'Ocupado',
    'LBL_CONFLICT' => 'Conflicto',
    'LBL_USER_CALENDARS' => 'Calendarios de Usuario',
    'LBL_SHARED' => 'Compartido',
    'LBL_PREVIOUS_SHARED' => 'Anterior',
    'LBL_NEXT_SHARED' => 'Siguiente',
    'LBL_SHARED_CAL_TITLE' => 'Calendario Compartido',
    'LBL_USERS' => 'Usuario',
    'LBL_REFRESH' => 'Actualizar',
    'LBL_EDIT_USERLIST' => 'Lista de Usuarios',
    'LBL_SELECT_USERS' => 'Seleccione usuarios para la visualización de calendario',
    'LBL_FILTER_BY_TEAM' => 'Filtrado de usuarios por equipo:',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_DATE' => 'Fecha y Hora de Inicio',
    'LBL_CREATE_MEETING' => 'Programar Reunión',
    'LBL_CREATE_CALL' => 'Registrar Llamada',
    'LBL_HOURS_ABBREV' => 'h',
    'LBL_MINS_ABBREV' => 'm',


    'LBL_YES' => 'Sí',
    'LBL_NO' => 'No',
    'LBL_SETTINGS' => 'Configuración',
    'LBL_CREATE_NEW_RECORD' => 'Crear Actividad',
    'LBL_LOADING' => 'Cargando ...',
    'LBL_SAVING' => 'Guardando...',
    'LBL_SENDING_INVITES' => 'Guardando y Enviando invitaciones .....',
    'LBL_CONFIRM_REMOVE' => 'Esta seguro que desea eliminar el registro?',
    'LBL_CONFIRM_REMOVE_ALL_RECURRING' => '¿Está seguro que desea eliminar todos los registros recurrentes?',
    'LBL_EDIT_RECORD' => 'Editar Actividad',
    'LBL_ERROR_SAVING' => 'Error al guardar',
    'LBL_ERROR_LOADING' => 'Error al cargar',
    'LBL_GOTO_DATE' => 'Ir a Fecha',
    'NOTICE_DURATION_TIME' => 'El tiempo de duración debe ser mayor que 0',
    'LBL_STYLE_BASIC' => 'Básico', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_STYLE_ADVANCED' => 'Avanzado', //Can be translated in all caps. This string will be used by SuiteP template menu actions

    'LBL_NO_USER' => 'Ningún resultado para el campo: Asignado a',
    'LBL_SUBJECT' => 'Asunto',
    'LBL_DURATION' => 'Duración',
    'LBL_STATUS' => 'Estado',
    'LBL_PRIORITY' => 'Prioridad',

    'LBL_SETTINGS_TITLE' => 'Configuración',
    'LBL_SETTINGS_DISPLAY_TIMESLOTS' => 'Mostrar espacios de tiempo en vistas de día y semana:',
    'LBL_SETTINGS_TIME_STARTS' => 'Hora inicio:',
    'LBL_SETTINGS_TIME_ENDS' => 'Hora fin:',
    'LBL_SETTINGS_CALLS_SHOW' => 'Ver llamadas:',
    'LBL_SETTINGS_TASKS_SHOW' => 'Ver tareas:',
    'LBL_SETTINGS_COMPLETED_SHOW' => 'Mostrar Reuniones, Llamadas y Tareas Hechas:',
    'LBL_SETTINGS_DISPLAY_SHARED_CALENDAR_SEPARATE' => 'Calendario Compartido separado:',

    'LBL_SAVE_BUTTON' => 'Guardar',
    'LBL_DELETE_BUTTON' => 'Eliminar',
    'LBL_APPLY_BUTTON' => 'Aplicar',
    'LBL_SEND_INVITES' => 'Guardar y Enviar Invitaciones',
    'LBL_CANCEL_BUTTON' => 'Cancelar',
    'LBL_CLOSE_BUTTON' => 'Cerrar:',

    'LBL_GENERAL_TAB' => 'Detalles',
    'LBL_PARTICIPANTS_TAB' => 'Asistentes',
    'LBL_REPEAT_TAB' => 'Repetir',

    'LBL_REPEAT_TYPE' => 'Repetir',
    'LBL_REPEAT_INTERVAL' => 'Intervalo',
    'LBL_REPEAT_END' => 'Fin',
    'LBL_REPEAT_END_AFTER' => 'Después de',
    'LBL_REPEAT_OCCURRENCES' => 'Ocurrencias',
    'LBL_REPEAT_END_BY' => 'Por',
    'LBL_REPEAT_DOW' => 'En',
    'LBL_REPEAT_UNTIL' => 'Repetir Hasta',
    'LBL_REPEAT_COUNT' => 'Número de ocurrencias',
    'LBL_REPEAT_LIMIT_ERROR' => 'Tu solicitud iba a crear más  de $limit reuniones.',

    'LBL_EDIT_ALL_RECURRENCES' => 'Editar todas las recurrencias',
    'LBL_REMOVE_ALL_RECURRENCES' => 'Eliminar todas las recurrencias',

    'LBL_DATE_END_ERROR' => 'La fecha de finalización es antes que la fecha de inicio',
    'ERR_YEAR_BETWEEN' => 'Lo siento, el calendario no dispone del año solicitado<br />Es necesario que el año sea entre 1970 y 2037',
    'ERR_NEIGHBOR_DATE' => 'get_neighbor_date_str: no definido para esta vista',
    'LBL_NO_ITEMS_MOBILE' => 'Su calendario esta libre para la semana.',
    'LBL_GENERAL_SETTINGS' => 'Configuración general',
    'LBL_COLOR_SETTINGS' => 'Ajustes de color',
    'LBL_MODULE' => 'Módulo',
    'LBL_BODY' => 'Contenido',
    'LBL_BORDER' => 'Borde',
    'LBL_TEXT' => 'Texto',
);


$mod_list_strings = array(
    'dom_cal_weekdays' =>
        array(
            '0' => "Dom",
            '1' => "Lun",
            '2' => "Mar",
            '3' => "Mie",
            '4' => "Jue",
            '5' => "Vie",
            '6' => "Sab",
        ),
    'dom_cal_weekdays_long' =>
        array(
            '0' => "Domingo",
            '1' => "Lunes",
            '2' => "Martes",
            '3' => "Miércoles",
            '4' => "Jueves",
            '5' => "Viernes",
            '6' => "Sábado",
        ),
    'dom_cal_month' =>
        array(
            '0' => "",
            '1' => "Ene",
            '2' => "Feb",
            '3' => "Mar",
            '4' => "Abr",
            '5' => "May",
            '6' => "Jun",
            '7' => "Jul",
            '8' => "Ago",
            '9' => "Sep",
            '10' => "Oct",
            '11' => "Nov",
            '12' => "Dic",
        ),
    'dom_cal_month_long' =>
        array(
            '0' => "",
            '1' => "Enero",
            '2' => "Febrero",
            '3' => "Marzo",
            '4' => "Abril",
            '5' => "Mayo",
            '6' => "Junio",
            '7' => "Julio",
            '8' => "Agosto",
            '9' => "Septiembre",
            '10' => "Octubre",
            '11' => "Noviembre",
            '12' => "Diciembre",
        ),
);