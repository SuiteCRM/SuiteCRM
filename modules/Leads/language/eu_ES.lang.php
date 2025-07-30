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
    //DON'T CONVERT THESE THEY ARE MAPPINGS
    'db_last_name' => 'LBL_LIST_LAST_NAME',
    'db_first_name' => 'LBL_LIST_FIRST_NAME',
    'db_title' => 'LBL_LIST_TITLE',
    'db_email1' => 'LBL_LIST_EMAIL_ADDRESS',
    'db_account_name' => 'LBL_LIST_ACCOUNT_NAME',
    'db_email2' => 'LBL_LIST_EMAIL_ADDRESS',
    //END DON'T CONVERT

    'ERR_DELETE_RECORD' => 'Debe especificar un número de registro para eliminar el Cliente potencial.',
    'LBL_ACCOUNT_DESCRIPTION' => 'Descripción de la Cuenta',
    'LBL_ACCOUNT_ID' => 'ID de Cuenta',
    'LBL_ACCOUNT_NAME' => 'Nombre de Cuenta:',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_ADDRESS_INFORMATION' => 'Información de Dirección',
    'LBL_ALT_ADDRESS_CITY' => 'Ciudad de dirección alternativa',
    'LBL_ALT_ADDRESS_COUNTRY' => 'País de dirección alternativa',
    'LBL_ALT_ADDRESS_POSTALCODE' => 'CP de dirección alternativa',
    'LBL_ALT_ADDRESS_STATE' => 'Estado/Provincia de dirección alternativa',
    'LBL_ALT_ADDRESS_STREET_2' => 'Calle de dirección alternativa 2',
    'LBL_ALT_ADDRESS_STREET_3' => 'Calle de dirección alternativa 3',
    'LBL_ALT_ADDRESS_STREET' => 'Calle de dirección alternativa',
    'LBL_ALTERNATE_ADDRESS' => 'Dirección alternativa:',
    'LBL_ALT_ADDRESS' => 'Otra dirección:',
    'LBL_ANY_ADDRESS' => 'Cualquier dirección:',
    'LBL_ANY_EMAIL' => 'Cualquier Correo:',
    'LBL_ANY_PHONE' => 'Cualquier teléfono:',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ASSIGNED_TO_ID' => 'Usuario Asignado:',
    'LBL_BUSINESSCARD' => 'Convertir Cliente Potencial',
    'LBL_CITY' => 'Ciudad:',
    'LBL_CONTACT_ID' => 'ID Contacto',
    'LBL_CONTACT_INFORMATION' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CONTACT_NAME' => 'Nombre del cliente potencial:',
    'LBL_CONTACT_OPP_FORM_TITLE' => 'Cliente Potencial-Oportunidad:',
    'LBL_CONTACT_ROLE' => 'Rol:',
    'LBL_CONTACT' => 'Cliente Potencial:',
    'LBL_CONVERTED_ACCOUNT' => 'Cuenta Convertida:',
    'LBL_CONVERTED_CONTACT' => 'Contacto Convertido:',
    'LBL_CONVERTED_OPP' => 'Oportunidad Convertida:',
    'LBL_CONVERTED' => 'Convertido',
    'LBL_CONVERTLEAD_BUTTON_KEY' => 'V',
    'LBL_CONVERTLEAD_TITLE' => 'Convertir Cliente Potencial',
    'LBL_CONVERTLEAD' => 'Convertir Cliente Potencial',
    'LBL_CONVERTLEAD_WARNING' => 'Aviso: El estado del Cliente Potencial que está a punto de convertir es "Convertido". Es posible que ya se haya creado algún registros de tipo Contacto y/o Cuenta a partir de este Cliente Potencial. Si desea continuar con la conversión del Cliente Potencial, haga clic en Guardar. Para volver al Cliente Potencial sin realizar la conversión, haga clic en Cancelar.',
    'LBL_CONVERTLEAD_WARNING_INTO_RECORD' => 'Posible Contacto:',
    'LBL_COUNTRY' => 'País:',
    'LBL_CREATED_NEW' => 'Ha creado un nuevo',
    'LBL_CREATED_ACCOUNT' => 'Creada una nueva cuenta',
    'LBL_CREATED_CALL' => 'Nueva llamada creada',
    'LBL_CREATED_CONTACT' => 'Nuevo contacto creado',
    'LBL_CREATED_MEETING' => 'Nueva reunión creada',
    'LBL_CREATED_OPPORTUNITY' => 'Nueva oportunidad creada',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Clientes Potenciales',
    'LBL_DEPARTMENT' => 'Departamento:',
    'LBL_DESCRIPTION' => 'Descripción:',
    'LBL_DO_NOT_CALL' => 'No llamar:',
    'LBL_DUPLICATE' => 'Clientes potenciales similares',
    'LBL_EMAIL_ADDRESS' => 'Correo electrónico:',
    'LBL_EMAIL_OPT_OUT' => 'Rehusar Email:',
    'LBL_EXISTING_ACCOUNT' => 'Usada una cuenta existente',
    'LBL_EXISTING_CONTACT' => 'Usado un contacto existente',
    'LBL_EXISTING_OPPORTUNITY' => 'Usada una oportunidad existente',
    'LBL_FAX_PHONE' => 'Fax:',
    'LBL_FIRST_NAME' => 'Nombre:',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_HOME_PHONE' => 'Tel. casa:',
    'LBL_IMPORT_VCARD' => 'Importar vCard',
    'LBL_VCARD' => 'vCard',
    'LBL_IMPORT_VCARDTEXT' => 'Automaticamente crea un nuevo cliente potencial importando una vCard.',
    'LBL_INVALID_EMAIL' => 'Email No Váildo:',
    'LBL_INVITEE' => 'Informadores',
    'LBL_LAST_NAME' => 'Apellidos:',
    'LBL_LEAD_SOURCE_DESCRIPTION' => 'Descripción de toma de contacto:',
    'LBL_LEAD_SOURCE' => 'Toma de contacto:',
    'LBL_LIST_ACCEPT_STATUS' => 'Aceptar Estato',
    'LBL_LIST_ACCOUNT_NAME' => 'Nombre de Cuenta',
    'LBL_LIST_CONTACT_NAME' => 'Nombre del cliente potencial',
    'LBL_LIST_CONTACT_ROLE' => 'Rol',
    'LBL_LIST_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_LIST_EMAIL_ADDRESS' => 'Email',
    'LBL_LIST_FIRST_NAME' => 'Nombre',
    'LBL_LIST_FORM_TITLE' => 'Lista de Clientes Potenciales',
    'LBL_LIST_LAST_NAME' => 'Apellidos',
    'LBL_LIST_LEAD_SOURCE_DESCRIPTION' => 'Descripción de toma de contacto',
    'LBL_LIST_LEAD_SOURCE' => 'Toma de contacto',
    'LBL_LIST_MY_LEADS' => 'Mis Clientes Potenciales',
    'LBL_LIST_NAME' => 'Nombre completo',
    'LBL_LIST_PHONE' => 'Teléfono',
    'LBL_LIST_REFERED_BY' => 'Referido por',
    'LBL_LIST_STATUS' => 'Estado',
    'LBL_LIST_TITLE' => 'Cargo',
    'LBL_MOBILE_PHONE' => 'Móvil:',
    'LBL_MODULE_NAME' => 'Clientes Potenciales',
    'LBL_MODULE_TITLE' => 'Clientes Potenciales: Inicio',
    'LBL_NAME' => 'Nombre completo:',
    'LBL_NEW_FORM_TITLE' => 'Nuevo Cliente Potencial',
    'LBL_OFFICE_PHONE' => 'Tel. Oficina:',
    'LBL_OPP_NAME' => 'Nombre de la oportunidad:',
    'LBL_OPPORTUNITY_AMOUNT' => 'Cantidad de la Oportunidad:',
    'LBL_OPPORTUNITY_ID' => 'ID Oportunidad',
    'LBL_OPPORTUNITY_NAME' => 'Nombre de la oportunidad:',
    'LBL_OTHER_EMAIL_ADDRESS' => 'Email Alternativo:',
    'LBL_OTHER_PHONE' => 'Tel. Alternativo:',
    'LBL_PHONE' => 'Teléfono:',
    'LBL_PORTAL_APP' => 'Aplicación del Portal',
    'LBL_PORTAL_INFORMATION' => 'Información del Portal',
    'LBL_PORTAL_NAME' => 'Nombre del Portal:',
    'LBL_POSTAL_CODE' => 'Código postal:',
    'LBL_STREET' => 'Calle',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Ciudad de dirección principal',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País de dirección principal',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'CP de dirección principal',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Estado/Provincia de dirección principal',
    'LBL_PRIMARY_ADDRESS_STREET_2' => 'Calle de dirección principal 2',
    'LBL_PRIMARY_ADDRESS_STREET_3' => 'Calle de dirección principal 3',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Calle de dirección principal',
    'LBL_PRIMARY_ADDRESS' => 'Dirección principal:',
    'LBL_REFERED_BY' => 'Referido por:',
    'LBL_REPORTS_TO_ID' => 'Informa a ID',
    'LBL_REPORTS_TO' => 'Informa a:',
    'LBL_SALUTATION' => 'Saludo',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_CREATED' => 'Creado por',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Clientes Potenciales',
    'LBL_SELECT_CHECKED_BUTTON_LABEL' => 'Seleccionar Clientes Potenciales Marcados',
    'LBL_SELECT_CHECKED_BUTTON_TITLE' => 'Seleccionar Clientes Potenciales Marcados',
    'LBL_STATE' => 'Estado o región:',
    'LBL_STATUS_DESCRIPTION' => 'Descripción estado:',
    'LBL_STATUS' => 'Estado:',
    'LBL_TITLE' => 'Puesto de trabajo:',
    'LNK_IMPORT_VCARD' => 'Nuevo Cliente Potencial desde vCard',
    'LNK_LEAD_LIST' => 'Ver Clientes Potenciales',
    'LNK_NEW_ACCOUNT' => 'Crear una cuenta',
    'LNK_NEW_APPOINTMENT' => 'Nueva Cita',
    'LNK_NEW_CONTACT' => 'Nuevo Contacto',
    'LNK_NEW_LEAD' => 'Nuevo Cliente Potencial',
    'LNK_NEW_NOTE' => 'Nueva Nota',
    'LNK_NEW_TASK' => 'Nueva Tarea',
    'LNK_NEW_CASE' => 'Nuevo Caso',
    'LNK_NEW_CALL' => 'Registrar Llamada',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_NEW_OPPORTUNITY' => 'Nueva Oportunidad',
    'LNK_SELECT_ACCOUNTS' => '<b>O</b> Seleccione una Cuenta',
    'LNK_SELECT_CONTACTS' => ' <b>O</b> Selecciona Contacto',
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro de que desea eliminar este registro?',
    'NTC_REMOVE_CONFIRMATION' => '¿Está seguro de que desea quitar este cliente potencial del caso?',
    'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE' => 'Campañas',
    'LBL_CAMPAIGN' => 'Campaña:',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Usuario Asignado',
    'LBL_PROSPECT_LIST' => 'Público Objetivo',
    'LBL_CAMPAIGN_LEAD' => 'Campañas',
    'LBL_BIRTHDATE' => 'Fecha de nacimiento:',
    'LBL_ASSISTANT_PHONE' => 'Tel. Asistente',
    'LBL_ASSISTANT' => 'Asistente',
    'LBL_CREATED_USER' => 'Usuario Creado',
    'LBL_MODIFIED_USER' => 'Usuario Modificado',
    'LBL_CAMPAIGNS' => 'Campañas',
    'LBL_CONVERT_MODULE_NAME' => 'Módulo',
    'LBL_CONVERT_REQUIRED' => 'Requerido',
    'LBL_CONVERT_SELECT' => 'Permitir Selección',
    'LBL_CONVERT_COPY' => 'Copiar Datos',
    'LBL_CONVERT_EDIT' => 'Editar',
    'LBL_CONVERT_DELETE' => 'Eliminar',
    'LBL_CONVERT_ADD_MODULE' => 'Agregar Módulo',
    'LBL_CREATE' => 'Crear',
    'LBL_SELECT' => '<b>O</b> Seleccionar',
    'LBL_WEBSITE' => 'Sitio Web',
    'LNK_IMPORT_LEADS' => 'Importar Clientes Potenciales',
//Convert lead tooltips
    'LBL_MODULE_TIP' => 'Módulo en el que crear un nuevo registro.',
    'LBL_REQUIRED_TIP' => 'Debe seleccionar o crear los módulos requeridos antes de que el cliente potencial pueda ser convertido.',
    'LBL_COPY_TIP' => 'Si está seleccionado, los campos del cliente potencial serán copiados a campos con el mismo nombre en los registros recién creados.',
    'LBL_SELECTION_TIP' => 'Los módulos con un campo relacionado en Contactos pueden ser seleccionados en lugar de creados durante el proceso de conversión del cliente potencial.',
    'LBL_EDIT_TIP' => 'Modificar el diseño de conversión para este módulo.',
    'LBL_DELETE_TIP' => 'Quitar este módulo del diseño de conversión.',

    'LBL_ACTIVITIES_MOVE' => 'Mover actividad a',
    'LBL_ACTIVITIES_COPY' => 'Copiar actividad a',
    'LBL_ACTIVITIES_MOVE_HELP' => "Seleccione los registros de actividad que quiera mover de los clientes potenciales. Tareas, llamadas, reuniones, notas y correos electrónicos que serán trasladados al registro seleccionado.",
    'LBL_ACTIVITIES_COPY_HELP' => "Selecciona el o los resgistros para cada copia creada de las actividades de los Clientes Potenciales. Las nuevas Tareas, Llamadas, Reuniones y Notas serán creadas para cada registro seleccionado. Los Emails se relacionarán con los registros seleccionados.",
    //For export labels
    'LBL_CAMPAIGN_ID' => 'Id de Campaña',
    'LBL_EDITLAYOUT' => 'Editar diseño' /*for 508 compliance fix*/,
    'LBL_ENTERDATE' => 'Introducir fecha' /*for 508 compliance fix*/,
    'LBL_LOADING' => 'Cargando ...' /*for 508 compliance fix*/,
    'LBL_EDIT_INLINE' => 'Editar' /*for 508 compliance fix*/,
    'LBL_FP_EVENTS_LEADS_1_FROM_FP_EVENTS_TITLE' => 'Eventos',
    'LBL_WWW' => 'WWW',
);
