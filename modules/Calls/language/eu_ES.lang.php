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
    'LBL_BLANK' => ' ',
    'LBL_MODULE_NAME' => 'Llamadas',
    'LBL_MODULE_TITLE' => 'Llamadas: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Llamadas',
    'LBL_LIST_FORM_TITLE' => 'Lista de Llamadas',
    'LBL_NEW_FORM_TITLE' => 'Crear Cita',
    'LBL_LIST_CLOSE' => 'Cerrar',
    'LBL_LIST_SUBJECT' => 'Asunto',
    'LBL_LIST_CONTACT' => 'Contacto',
    'LBL_LIST_RELATED_TO' => 'Relacionado con',
    'LBL_LIST_RELATED_TO_ID' => 'Relacionado con ID',
    'LBL_LIST_DATE' => 'Fecha Inicio',
    'LBL_LIST_DIRECTION' => 'Dirección',
    'LBL_SUBJECT' => 'Asunto:',
    'LBL_REMINDER' => 'Aviso',
    'LBL_CONTACT_NAME' => 'Contacto:',
    'LBL_DESCRIPTION' => 'Descripción:',
    'LBL_STATUS' => 'Estado:',
    'LBL_DIRECTION' => 'Dirección:',
    'LBL_DATE' => 'Fecha Inicio:',
    'LBL_DURATION' => 'Duración:',
    'LBL_DURATION_HOURS' => 'Duración (Horas):',
    'LBL_DURATION_MINUTES' => 'Duración (Minutos):',
    'LBL_HOURS_MINUTES' => '(horas/minutos)',
    'LBL_DATE_TIME' => 'Inicio:',
    'LBL_TIME' => 'Hora inicio:',
    'LBL_HOURS_ABBREV' => 'h',
    'LBL_MINSS_ABBREV' => 'm',
    'LNK_NEW_CALL' => 'Registrar Llamada',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_CALL_LIST' => 'Ver Llamadas',
    'LNK_IMPORT_CALLS' => 'Importar Llamadas',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de registro a eliminar.',
    'LBL_INVITEE' => 'Participantes',
    'LBL_RELATED_TO' => 'Relacionado con',
    'LNK_NEW_APPOINTMENT' => 'Crear Cita',
    'LBL_SCHEDULING_FORM_TITLE' => 'Planificación',
    'LBL_ADD_INVITEE' => 'Añadir asistentes',
    'LBL_NAME' => 'Nombre',
    'LBL_FIRST_NAME' => 'Nombre',
    'LBL_LAST_NAME' => 'Apellidos',
    'LBL_EMAIL' => 'Email',
    'LBL_PHONE' => 'Teléfono',
    'LBL_REMINDER_POPUP' => 'Ventana emergente',
    'LBL_REMINDER_EMAIL_ALL_INVITEES' => 'Enviar correo electrónico a todos los asistentes',
    'LBL_EMAIL_REMINDER' => 'Recordatorio por correo electrónico',
    'LBL_EMAIL_REMINDER_TIME' => 'Tiempo de recordatorio por correo electrónico',
    'LBL_SEND_BUTTON_TITLE' => 'Guardar y Enviar Invitaciones',
    'LBL_SEND_BUTTON_LABEL' => 'Enviar Invitaciones',
    'LBL_DATE_END' => 'Fecha de Fin',
    'LBL_REMINDER_TIME' => 'Hora Aviso',
    'LBL_EMAIL_REMINDER_SENT' => 'Recordatorio por correo electrónico enviado',
    'LBL_SEARCH_BUTTON' => 'Buscar',
    'LBL_ADD_BUTTON' => 'Añadir',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Llamadas',
    'LNK_SELECT_ACCOUNT' => 'Seleccionar Cuenta',
    'LNK_NEW_ACCOUNT' => 'Nueva Cuenta',
    'LNK_NEW_OPPORTUNITY' => 'Nueva Oportunidad',
    'LBL_LEADS_SUBPANEL_TITLE' => 'Clientes Potenciales',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactos',
    'LBL_USERS_SUBPANEL_TITLE' => 'Usuarios',
    'LBL_OUTLOOK_ID' => 'ID Outlook',
    'LBL_MEMBER_OF' => 'Miembro De',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Notas',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_LIST_MY_CALLS' => 'Mis Llamadas',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ASSIGNED_TO_ID' => 'Usuario Asignado',
    'NOTICE_DURATION_TIME' => 'El tiempo de duración debe ser mayor que 0',
    'LBL_CALL_INFORMATION' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_REMOVE' => 'Quitar',
    'LBL_ACCEPT_STATUS' => 'Aceptar estato',
    'LBL_ACCEPT_LINK' => 'Aceptar Link',

    // create invitee functionality
    'LBL_CREATE_INVITEE' => 'Crear una invitación',
    'LBL_CREATE_CONTACT' => 'Nuevo Contacto',
    'LBL_CREATE_LEAD' => 'Nuevo Cliente Potencial',
    'LBL_CREATE_AND_ADD' => 'Crear y Añadir',
    'LBL_CANCEL_CREATE_INVITEE' => 'Cancelar',
    'LBL_EMPTY_SEARCH_RESULT' => 'Lo sentimos, no se encontraron resultados. Por favor cree una invitación abajo.',
    'LBL_NO_ACCESS' => 'No tiene permisos para crear registros en el módulo $module',

    'LBL_REPEAT_TYPE' => 'Repetición',
    'LBL_REPEAT_INTERVAL' => 'Intervalo de repetición',
    'LBL_REPEAT_DOW' => 'Repita el Dow',
    'LBL_REPEAT_UNTIL' => 'Repetir Hasta',
    'LBL_REPEAT_COUNT' => 'Número de repeticiones',
    'LBL_REPEAT_PARENT_ID' => 'Repita el ID principal',
    'LBL_RECURRING_SOURCE' => 'Fuente periódica',

    'LBL_SYNCED_RECURRING_MSG' => 'Esta convocatoria se originó en otro sistema y se sincronizan con el SuiteCRM. Para realizar cambios, vaya a la llamada original en el otro sistema. Los cambios realizados en el otro sistema se puede sincronizar con este registro.',

    // for reminders
    'LBL_REMINDERS' => 'Recordatorios',
    'LBL_REMINDERS_ACTIONS' => 'Acciones:',
    'LBL_REMINDERS_POPUP' => 'Ventana emergente',
    'LBL_REMINDERS_EMAIL' => 'Enviar e-mail a asistentes',
    'LBL_REMINDERS_WHEN' => 'Cuando:',
    'LBL_REMINDERS_REMOVE_REMINDER' => 'Eliminar recordatorio',
    'LBL_REMINDERS_ADD_ALL_INVITEES' => 'Añadir a todos los invitados',
    'LBL_REMINDERS_ADD_REMINDER' => 'Añadir recordatorio',

    'LBL_RESCHEDULE' => 'Replanificaciones',
    'LBL_RESCHEDULE_COUNT' => 'Intentos de Llamada',
    'LBL_RESCHEDULE_DATE' => 'Fecha',
    'LBL_RESCHEDULE_REASON' => 'Razón',
    'LBL_RESCHEDULE_ERROR1' => 'Por favor seleccione una fecha v&aacute;lida',
    'LBL_RESCHEDULE_ERROR2' => 'Por favor seleccione una raz&oacute;n',
    'LBL_RESCHEDULE_PANEL' => 'Replanificaciones',
    'LBL_RESCHEDULE_HISTORY' => 'Historial de Intentos de Llamada',
    'LBL_CANCEL' => 'Cancelar',
    'LBL_SAVE' => 'Guardar',

    'LBL_CALLS_RESCHEDULE' => 'Reprogramación de llamadas',
    'LBL_LIST_STATUS'=>'Estado',
    'LBL_LIST_DATE_MODIFIED'=>'Fecha de Modificación',
    'LBL_LIST_DUE_DATE'=>'Fecha de Vencimiento',
);