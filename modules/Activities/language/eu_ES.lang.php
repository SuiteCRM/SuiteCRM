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
    'LBL_MODULE_NAME' => 'Actividades',
    'LBL_MODULE_TITLE' => 'Actividades: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Actividades',
    'LBL_LIST_FORM_TITLE' => 'Lista de Actividades',
    'LBL_LIST_SUBJECT' => 'Asunto',
    'LBL_OVERVIEW' => 'Visión Global',
    'LBL_TASKS' => 'Tareas',
    'LBL_MEETINGS' => 'Reuniones',
    'LBL_CALLS' => 'Llamadas',
    'LBL_EMAILS' => 'Correos',
    'LBL_NOTES' => 'Notas',
    'LBL_PRINT' => 'Imprenta',
    'LBL_MEETING_TYPE' => 'Reunión',
    'LBL_CALL_TYPE' => 'Llamada',
    'LBL_EMAIL_TYPE' => 'Email',
    'LBL_NOTE_TYPE' => 'Nota',
    'LBL_DATA_TYPE_START' => 'Inicio:',
    'LBL_DATA_TYPE_SENT' => 'Enviado:',
    'LBL_DATA_TYPE_MODIFIED' => 'Modificado:',
    'LBL_LIST_CONTACT' => 'Contacto',
    'LBL_LIST_RELATED_TO' => 'Relacionado con',
    'LBL_LIST_DATE' => 'Fecha',
    'LBL_LIST_CLOSE' => 'Cerrar',
    'LBL_SUBJECT' => 'Asunto:',
    'LBL_STATUS' => 'Estado:',
    'LBL_LOCATION' => 'Lugar:',
    'LBL_DATE_TIME' => 'Fecha y hora de inicio:',
    'LBL_DATE' => 'Fecha de inicio:',
    'LBL_TIME' => 'Hora de inicio:',
    'LBL_DURATION' => 'Duración:',
    'LBL_HOURS_MINS' => '(horas/minutos)',
    'LBL_CONTACT_NAME' => 'Nombre de contacto: ',
    'LBL_DESCRIPTION' => 'Descripción:',
    'LNK_NEW_CALL' => 'Registrar Llamada',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_NEW_TASK' => 'Nueva Tarea',
    'LNK_NEW_NOTE' => 'Nueva Nota o Adjunto',
    'LNK_NEW_EMAIL' => 'Nuevo Email Archivado',
    'LNK_CALL_LIST' => 'Ver Llamadas',
    'LNK_MEETING_LIST' => 'Ver Reuniones',
    'LNK_TASK_LIST' => 'Ver Tareas',
    'LNK_NOTE_LIST' => 'Ver Notas',
    'LBL_DELETE_ACTIVITY' => '¿Está seguro de que desea eliminar esta actividad?',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de registro para eliminar la cuenta.',
    'LBL_INVITEE' => 'Asistentes',
    'LBL_LIST_DIRECTION' => 'Dirección',
    'LBL_DIRECTION' => 'Dirección',
    'LNK_NEW_APPOINTMENT' => 'Nueva Cita',
    'LNK_VIEW_CALENDAR' => 'Ver Calendario',
    'LBL_OPEN_ACTIVITIES' => 'Actividades Abiertas',
    'LBL_HISTORY' => 'Historial',
    'LBL_NEW_TASK_BUTTON_TITLE' => 'Nueva Tarea',
    'LBL_NEW_TASK_BUTTON_LABEL' => 'Nueva Tarea',
    'LBL_SCHEDULE_MEETING_BUTTON_TITLE' => 'Programar Reunión',
    'LBL_SCHEDULE_MEETING_BUTTON_LABEL' => 'Programar Reunión',
    'LBL_SCHEDULE_CALL_BUTTON_LABEL' => 'Registrar Llamada',
    'LBL_NEW_NOTE_BUTTON_TITLE' => 'Nueva Nota o Archivo Adjunto',
    'LBL_NEW_NOTE_BUTTON_LABEL' => 'Nueva Nota o Archivo Adjunto',
    'LBL_TRACK_EMAIL_BUTTON_TITLE' => 'Archivar Email',
    'LBL_TRACK_EMAIL_BUTTON_LABEL' => 'Archivar Email',
    'LBL_LIST_STATUS' => 'Estado',
    'LBL_LIST_DUE_DATE' => 'Fecha Vencimiento',
    'LBL_LIST_LAST_MODIFIED' => 'Última Modificación',
    'LNK_IMPORT_CALLS' => 'Importar Llamadas',
    'LNK_IMPORT_MEETINGS' => 'Importar Reuniones',
    'LNK_IMPORT_TASKS' => 'Importar Tareas',
    'LNK_IMPORT_NOTES' => 'Importar Notas',
    'LBL_ACCEPT_THIS' => '¿Aceptar?',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Actividades Abiertas',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Usuario Asignado',

    'LBL_ACCEPT' => 'Aceptar' /*for 508 compliance fix*/,
);
