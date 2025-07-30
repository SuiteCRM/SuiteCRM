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
    'LBL_ACCEPT_THIS' => '¿Aceptar?',
    'LBL_ADD_BUTTON' => 'Añadir',
    'LBL_ADD_INVITEE' => 'Añadir Asistentes',
    'LBL_CONTACT_NAME' => 'Contacto:',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactos',
    'LBL_CREATED_BY' => 'Creado por',
    'LBL_DATE_END' => 'Fecha Fin',
    'LBL_DATE_TIME' => 'Fecha y hora de inicio:',
    'LBL_DATE' => 'Fecha Inicio:',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Reuniones',
    'LBL_DESCRIPTION' => 'Descripción:',
    'LBL_DIRECTION' => 'Dirección:',
    'LBL_DURATION_HOURS' => 'Duración (Horas):',
    'LBL_DURATION_MINUTES' => 'Duración (Minutos):',
    'LBL_DURATION' => 'Duración:',
    'LBL_EMAIL' => 'Email',
    'LBL_FIRST_NAME' => 'Nombre',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Notas',
    'LBL_HOURS_ABBREV' => 'h',
    'LBL_HOURS_MINS' => '(horas/minutos)',
    'LBL_INVITEE' => 'Asistentes',
    'LBL_LAST_NAME' => 'Apellidos',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a:',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Usuario Asignado',
    'LBL_LIST_CLOSE' => 'Cerrado',
    'LBL_LIST_CONTACT' => 'Contacto',
    'LBL_LIST_DATE_MODIFIED' => 'Fecha de Modificación',
    'LBL_LIST_DATE' => 'Fecha Inicio',
    'LBL_LIST_DIRECTION' => 'Dirección',
    'LBL_LIST_DUE_DATE' => 'Fecha de Vencimiento',
    'LBL_LIST_FORM_TITLE' => 'Lista de Reuniones',
    'LBL_LIST_MY_MEETINGS' => 'Mis Reuniones',
    'LBL_LIST_RELATED_TO' => 'Relacionado con',
    'LBL_LIST_STATUS' => 'Estado',
    'LBL_LIST_SUBJECT' => 'Asunto',
    'LBL_LEADS_SUBPANEL_TITLE' => 'Clientes Potenciales',
    'LBL_LOCATION' => 'Lugar:',
    'LBL_MINSS_ABBREV' => 'm',
    'LBL_MODIFIED_BY' => 'Modificado por',
    'LBL_MODULE_NAME' => 'Reuniones',
    'LBL_MODULE_TITLE' => 'Reuniones: Inicio',
    'LBL_NAME' => 'Nombre',
    'LBL_NEW_FORM_TITLE' => 'Crear Cita',
    'LBL_OUTLOOK_ID' => 'ID Outlook',
    'LBL_SEQUENCE' => 'Secuencia de actualización de la reunión',
    'LBL_PHONE' => 'Teléfono',
    'LBL_REMINDER_TIME' => 'Hora Aviso',
    'LBL_EMAIL_REMINDER_SENT' => 'Recordatorio por correo electrónico enviado',
    'LBL_REMINDER' => 'Aviso:',
    'LBL_REMINDER_POPUP' => 'Ventana emergente',
    'LBL_REMINDER_EMAIL_ALL_INVITEES' => 'Enviar correo electrónico a todos los asistentes',
    'LBL_EMAIL_REMINDER' => 'Eecordatorio por correo electrónico',
    'LBL_EMAIL_REMINDER_TIME' => 'Tiempo de recordatorio por correo electrónico',
    'LBL_REMOVE' => 'Quitar',
    'LBL_SCHEDULING_FORM_TITLE' => 'Planificación',
    'LBL_SEARCH_BUTTON' => 'Búsqueda',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Reuniones',
    'LBL_SEND_BUTTON_LABEL' => 'Guardar y Enviar Invitaciones',
    'LBL_SEND_BUTTON_TITLE' => 'Guardar y Enviar Invitaciones',
    'LBL_STATUS' => 'Estado:',
    'LBL_TYPE' => 'Tipo de reunión',
    'LBL_PASSWORD' => 'Contraseña de la reunión',
    'LBL_URL' => 'Iniciar/Unirse a la reunión',
    'LBL_HOST_URL' => 'Host URL',
    'LBL_DISPLAYED_URL' => 'Ver URL',
    'LBL_CREATOR' => 'Creador de reuniones',
    'LBL_EXTERNALID' => 'ID App Externa',
    'LBL_SUBJECT' => 'Asunto:',
    'LBL_TIME' => 'Hora Inicio:',
    'LBL_USERS_SUBPANEL_TITLE' => 'Usuarios',
    'LBL_PARENT_TYPE' => 'Tipo de Padre',
    'LBL_PARENT_ID' => 'ID Padre',
    'LNK_MEETING_LIST' => 'Ver Reuniones',
    'LNK_NEW_APPOINTMENT' => 'Crear Cita',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_IMPORT_MEETINGS' => 'Importar Reuniones',

    'LBL_CREATED_USER' => 'Usuario Creado',
    'LBL_MODIFIED_USER' => 'Usuario Modificado',
    'NOTICE_DURATION_TIME' => 'El tiempo de duración debe ser mayor que 0',
    'LBL_MEETING_INFORMATION' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_LIST_JOIN_MEETING' => 'Unirser a la reunión',
    'LBL_ACCEPT_STATUS' => 'Aceptar estato',
    'LBL_ACCEPT_LINK' => 'Aceptar Link',
    // You are not invited to the meeting messages
    'LBL_EXTNOT_MAIN' => 'No es capaz de unirse a esta reunión, porque usted no es un asistente.',
    'LBL_EXTNOT_RECORD_LINK' => 'Ver reunión',

    //cannot start messages
    'LBL_EXTNOSTART_MAIN' => 'No se puede iniciar esta reunión, ya que no es un administrador o el dueño de la reunión.',

    // create invitee functionallity
    'LBL_CREATE_INVITEE' => 'Crear una invitación',
    'LBL_CREATE_CONTACT' => 'Nuevo Contacto',  // Create invitee functionallity
    'LBL_CREATE_LEAD' => 'Nuevo Cliente Potencial', // Create invitee functionallity
    'LBL_CREATE_AND_ADD' => 'Crear y Añadir', // Create invitee functionallity
    'LBL_CANCEL_CREATE_INVITEE' => 'Cancelar',
    'LBL_EMPTY_SEARCH_RESULT' => 'Lo sentimos, no se encontraron resultados. Por favor cree un asistente abajo.',
    'LBL_NO_ACCESS' => 'No tiene permisos para crear registros en el módulo $module',  // Create invitee functionallity

    'LBL_REPEAT_TYPE' => 'Repetición',
    'LBL_REPEAT_INTERVAL' => 'Intervalo de repetición',
    'LBL_REPEAT_DOW' => 'Repita el Dow',
    'LBL_REPEAT_UNTIL' => 'Repetir hasta',
    'LBL_REPEAT_COUNT' => 'Número de repeticiones',
    'LBL_REPEAT_PARENT_ID' => 'Repita el ID principal',
    'LBL_RECURRING_SOURCE' => 'Fuente periódica',

    'LBL_SYNCED_RECURRING_MSG' => 'Esta convocatoria se originó en otro sistema y se sincronizan con el SuiteCRM. Para realizar cambios, vaya a la reunión original en el otro sistema. Los cambios realizados en el otro sistema se puede sincronizar con este registro.',
    'LBL_RELATED_TO' => 'Relacionado con:',

    // for reminders
    'LBL_REMINDERS' => 'Recordatorios',
    'LBL_REMINDERS_ACTIONS' => 'Acciones:',
    'LBL_REMINDERS_POPUP' => 'Ventana emergente',
    'LBL_REMINDERS_EMAIL' => 'Enviar correo electrónico a todos los asistentes',
    'LBL_REMINDERS_WHEN' => 'Cuando:',
    'LBL_REMINDERS_REMOVE_REMINDER' => 'Eliminar recordatorio',
    'LBL_REMINDERS_ADD_ALL_INVITEES' => 'Añadir a todos los invitados',
    'LBL_REMINDERS_ADD_REMINDER' => 'Añadir recordatorio',

    // for google sync
    'LBL_GSYNC_ID' => 'ID de evento de Google',
    'LBL_GSYNC_LASTSYNC' => 'Última marca de tiempo de Google Sync',
);
