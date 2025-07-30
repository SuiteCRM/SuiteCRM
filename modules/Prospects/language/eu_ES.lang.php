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
    'LBL_MODULE_NAME' => 'Público Objetivo',
    'LBL_MODULE_ID' => 'Público Objetivo',
    'LBL_INVITEE' => 'Informa Directamente',
    'LBL_MODULE_TITLE' => 'Público Objetivo: Inicio',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Público Objetivo',
    'LBL_LIST_FORM_TITLE' => 'Lista de Público Objetivo',
    'LBL_NEW_FORM_TITLE' => 'Nuevo Público Objetivo',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_LIST_LAST_NAME' => 'Apellidos',
    'LBL_LIST_TITLE' => 'Puesto de trabajo',
    'LBL_LIST_EMAIL_ADDRESS' => 'Email',
    'LBL_LIST_PHONE' => 'Teléfono',
    'LBL_LIST_FIRST_NAME' => 'Nombre',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ASSIGNED_TO_ID' => 'Asignado a:',
    'LBL_CAMPAIGN_ID' => 'ID Campaña',
    'LBL_EXISTING_ACCOUNT' => 'Usada una cuenta existente',
    'LBL_CREATED_ACCOUNT' => 'Creada una nueva cuenta',
    'LBL_CREATED_CALL' => 'Nueva llamada creada',
    'LBL_CREATED_MEETING' => 'Nueva reunión creada',
    'LBL_NAME' => 'Nombre:',
    'LBL_PROSPECT_INFORMATION' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_MORE_INFORMATION' => 'Más Información',
    'LBL_FIRST_NAME' => 'Nombre:',
    'LBL_OFFICE_PHONE' => 'Tel. Oficina:',
    'LBL_ANY_PHONE' => 'Tel. Cualquiera:',
    'LBL_PHONE' => 'Teléfono:',
    'LBL_LAST_NAME' => 'Apellidos:',
    'LBL_MOBILE_PHONE' => 'Móvil:',
    'LBL_HOME_PHONE' => 'Casa:',
    'LBL_OTHER_PHONE' => 'Tel. Alternativo:',
    'LBL_FAX_PHONE' => 'Fax:',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Calle Dirección Principal:',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Ciudad Dirección Principal:',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País Dirección Principal:',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Provincia/Estado Dirección Principal:',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'CP Dirección Principal:',
    'LBL_ALT_ADDRESS_STREET' => 'Calle Dirección Alternativa:',
    'LBL_ALT_ADDRESS_CITY' => 'Ciudad Dirección Alternativa:',
    'LBL_ALT_ADDRESS_COUNTRY' => 'País Dirección Alternativa:',
    'LBL_ALT_ADDRESS_STATE' => 'Provincia/Estado Dirección Alternativa:',
    'LBL_ALT_ADDRESS_POSTALCODE' => 'CP Dirección Alternativa:',
    'LBL_TITLE' => 'Puesto de trabajo:',
    'LBL_DEPARTMENT' => 'Departamento:',
    'LBL_BIRTHDATE' => 'Fecha de nacimiento:',
    'LBL_EMAIL_ADDRESS' => 'Correo electrónico:',
    'LBL_OTHER_EMAIL_ADDRESS' => 'Email alternativo:',
    'LBL_ANY_EMAIL' => 'Cualquier Correo:',
    'LBL_ASSISTANT' => 'Asistente:',
    'LBL_ASSISTANT_PHONE' => 'Tel. Asistente:',
    'LBL_DO_NOT_CALL' => 'No Llamar:',
    'LBL_EMAIL_OPT_OUT' => 'Rehusar Email:',
    'LBL_PRIMARY_ADDRESS' => 'Dirección Principal:',
    'LBL_ALTERNATE_ADDRESS' => 'Dirección Alternativa:',
    'LBL_ANY_ADDRESS' => 'Dirección Alternativa:',
    'LBL_CITY' => 'Ciudad:',
    'LBL_STATE' => 'Estado o región:',
    'LBL_POSTAL_CODE' => 'CP:',
    'LBL_COUNTRY' => 'País:',
    'LBL_ADDRESS_INFORMATION' => 'Información de Dirección',
    'LBL_DESCRIPTION' => 'Descripción:',
    'LBL_OPP_NAME' => 'Nombre de la oportunidad:',
    'LBL_IMPORT_VCARD' => 'Importar vCard',
    'LBL_IMPORT_VCARDTEXT' => 'Crear un nuevo contacto automáticamente importando una vCard de su sistema de ficheros.',
    'LBL_DUPLICATE' => 'Posible Público Objetivo Duplicado',
    'MSG_SHOW_DUPLICATES' => 'El registro para el prospecto que va a crear podría ser un duplicado de otro registro de prospecto existente. Los registros de prospectos con nombres y/o direcciones de correo similares se listan a continuación.<br>Haga clic en Guardar para continuar con la creación de este prospecto, o en Cancelar para volver al módulo sin crear el prospecto.',
    'MSG_DUPLICATE' => 'El registro para el prospecto que va a crear podría ser un duplicado de otro registro de prospecto existente. Los registros de prospectos con nombres y/o direcciones de correo similares se listan a continuación.<br>Haga clic en Guardar para continuar con la creación de este prospecto, o en Cancelar para volver al módulo sin crear el prospecto.',
    'LNK_IMPORT_VCARD' => 'Crear desde vCard',
    'LNK_NEW_ACCOUNT' => 'Crear una cuenta',
    'LNK_NEW_OPPORTUNITY' => 'Crear Oportunidad',
    'LNK_NEW_CASE' => 'Crear Caso',
    'LNK_NEW_NOTE' => 'Crear Nota or Adjunto',
    'LNK_NEW_CALL' => 'Registrar Llamada',
    'LNK_NEW_EMAIL' => 'Archivar Email',
    'LNK_NEW_MEETING' => 'Planificar Reunión',
    'LNK_NEW_TASK' => 'Crear Tarea',
    'LNK_NEW_APPOINTMENT' => 'Crear Cita',
    'LNK_IMPORT_PROSPECTS' => 'Importar Prospectos',
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro de que desea eliminar este registro?',
    'NTC_REMOVE_CONFIRMATION' => '¿Está seguro de que desea quitar este contacto del caso?',
    'ERR_DELETE_RECORD' => 'Debe especificar un número de registro para eliminar el contacto.',
    'LBL_SALUTATION' => 'Saludo',
    'LBL_CREATED_OPPORTUNITY' => 'Nueva oportunidad creada',
    'LNK_SELECT_ACCOUNT' => "Seleccionar Cuenta",
    'LNK_NEW_PROSPECT' => 'Crear Público Objetivo',
    'LNK_PROSPECT_LIST' => 'Ver Público Objetivo',
    'LNK_NEW_CAMPAIGN' => 'Crear Campaña',
    'LNK_CAMPAIGN_LIST' => 'Campañas',
    'LNK_NEW_PROSPECT_LIST' => 'Crear Lista de Público Objetivo',
    'LNK_PROSPECT_LIST_LIST' => 'Listas de Público Objetivo',
    'LBL_SELECT_CHECKED_BUTTON_LABEL' => 'Seleccione Público Objetivo Marcado',
    'LBL_SELECT_CHECKED_BUTTON_TITLE' => 'Seleccione Público Objetivo Marcado',
    'LBL_INVALID_EMAIL' => 'Email No Válido:',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Público Objetivo',
    'LBL_PROSPECT_LIST' => 'Público Objetivo',
    'LBL_CONVERT_BUTTON_TITLE' => 'Convertir Público Objetivo',
    'LBL_CONVERT_BUTTON_LABEL' => 'Convertir Público Objetivo',
    'LNK_NEW_CONTACT' => 'Nuevo Contacto',
    'LBL_CREATED_CONTACT' => "Nuevo contacto creado",
    'LBL_CAMPAIGNS' => 'Campañas',
    'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE' => 'Registro de Campañas',
    'LBL_TRACKER_KEY' => 'Clave de Seguimiento',
    'LBL_LEAD_ID' => 'Id Cliente Potencial',
    'LBL_CONVERTED_LEAD' => 'Cliente Potencial Convertido',
    'LBL_ACCOUNT_NAME' => 'Nombre de Cuenta',
    'LBL_EDIT_ACCOUNT_NAME' => 'Nombre de Cuenta:',
    'LBL_CREATED_USER' => 'Usuario Creado',
    'LBL_MODIFIED_USER' => 'Usuario Modificado',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    //For export labels
    'LBL_FP_EVENTS_PROSPECTS_1_FROM_FP_EVENTS_TITLE' => 'Eventos',
);
