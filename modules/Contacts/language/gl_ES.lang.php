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
    'ERR_DELETE_RECORD' => 'Especifique o número de rexistro para eliminar o contacto.',
    'LBL_ACCOUNT_ID' => 'ID de Conta:',
    'LBL_ACCOUNT_NAME' => 'Nome de Conta:',
    'LBL_CAMPAIGN' => 'Campaña:',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_ADDRESS_INFORMATION' => 'Información de Enderezo',
    'LBL_ALT_ADDRESS_CITY' => 'Cidade de enderezo alternativo:',
    'LBL_ALT_ADDRESS_COUNTRY' => 'País de enderezo alternativo:',
    'LBL_ALT_ADDRESS_POSTALCODE' => 'CP de enderezo alternativo:',
    'LBL_ALT_ADDRESS_STATE' => 'Estado/Provincia de enderezo alternativo:',
    'LBL_ALT_ADDRESS_STREET_2' => 'Rúa de enderezo alternativo 2',
    'LBL_ALT_ADDRESS_STREET_3' => 'Rúa de enderezo alternativo 3',
    'LBL_ALT_ADDRESS_STREET' => 'Rúa de enderezo alternativo:',
    'LBL_ALTERNATE_ADDRESS' => 'Enderezo alternativo:',
    'LBL_ALT_ADDRESS' => 'Outra enderezo:',
    'LBL_ANY_ADDRESS' => 'Calquera enderezo:',
    'LBL_ANY_EMAIL' => 'Calquera Correo:',
    'LBL_ANY_PHONE' => 'Calquera Teléfono:',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ASSIGNED_TO_ID' => 'Usuario Asignado',
    'LBL_ASSISTANT_PHONE' => 'Tel. asistente:',
    'LBL_ASSISTANT' => 'Asistente:',
    'LBL_BIRTHDATE' => 'Data de nacemento:',
    'LBL_CITY' => 'Cidade:',
    'LBL_CAMPAIGN_ID' => 'ID Campaña',
    'LBL_CONTACT_INFORMATION' => 'Visión Global',  //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CONTACT_NAME' => 'Nome Contacto:',
    'LBL_CONTACT_OPP_FORM_TITLE' => 'Oportunidade-Contacto:',
    'LBL_CONTACT_ROLE' => 'Rol:',
    'LBL_CONTACT' => 'Contacto:',
    'LBL_COUNTRY' => 'País:',
    'LBL_CREATED_ACCOUNT' => 'Creada unha nova conta',
    'LBL_CREATED_CALL' => 'Nova chamada creada',
    'LBL_CREATED_CONTACT' => 'Novo contacto creado',
    'LBL_CREATED_MEETING' => 'Nova reunión creada',
    'LBL_CREATED_OPPORTUNITY' => 'Nova oportunidade creada',
    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Contactos',
    'LBL_DEPARTMENT' => 'Departamento:',
    'LBL_DESCRIPTION' => 'Descrición:',
    'LBL_DIRECT_REPORTS_SUBPANEL_TITLE' => 'Informadores Directos',
    'LBL_DO_NOT_CALL' => 'Non chamar:',
    'LBL_DUPLICATE' => 'Posible contacto duplicado',
    'LBL_EMAIL_ADDRESS' => 'Correo electrónico:',
    'LBL_EMAIL_OPT_OUT' => 'Rehusar Email:',
    'LBL_EXISTING_ACCOUNT' => 'Usada conta existente',
    'LBL_EXISTING_CONTACT' => 'Usado contacto existente',
    'LBL_EXISTING_OPPORTUNITY' => 'Usada oportunidade existente',
    'LBL_FAX_PHONE' => 'Fax:',
    'LBL_FIRST_NAME' => 'Nome:',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_HOME_PHONE' => 'Tel. casa:',
    'LBL_ID' => 'ID:',
    'LBL_IMPORT_VCARD' => 'Importar vCard',
    'LBL_VCARD' => 'vCard',
    'LBL_IMPORT_VCARDTEXT' => 'Crea automaticamente un novo contacto a partir dunha vCard.',
    'LBL_INVALID_EMAIL' => 'Email non válido:',
    'LBL_INVITEE' => 'Informadores',
    'LBL_LAST_NAME' => 'Apelidos:',
    'LBL_LEAD_SOURCE' => 'Toma de contacto:',
    'LBL_LIST_ACCEPT_STATUS' => 'Estado de Aceptación',
    'LBL_LIST_ACCOUNT_NAME' => 'Nome de Conta',
    'LBL_LIST_CONTACT_NAME' => 'Nome Contacto',
    'LBL_LIST_CONTACT_ROLE' => 'Rol',
    'LBL_LIST_EMAIL_ADDRESS' => 'Email',
    'LBL_LIST_FIRST_NAME' => 'Nome',
    'LBL_LIST_FORM_TITLE' => 'Lista de Contactos',
    'LBL_LIST_LAST_NAME' => 'Apelidos',
    'LBL_LIST_NAME' => 'Nome completo',
    'LBL_LIST_PHONE' => 'Teléfono',
    'LBL_LIST_TITLE' => 'Posto de traballo',
    'LBL_MOBILE_PHONE' => 'Móbil:',
    'LBL_MODIFIED' => 'Modificado por:',
    'LBL_MODULE_NAME' => 'Contactos',
    'LBL_MODULE_TITLE' => 'Contactos: Inicio',
    'LBL_NAME' => 'Nome completo:',
    'LBL_NEW_FORM_TITLE' => 'Novo Contacto',
    'LBL_NOTE_SUBJECT' => 'Asunto de Nota',
    'LBL_OFFICE_PHONE' => 'Tel. Oficina:',
    'LBL_OPP_NAME' => 'Nome da oportunidade:',
    'LBL_OPPORTUNITY_ROLE_ID' => 'ID de Rol en Oportunidade',
    'LBL_OPPORTUNITY_ROLE' => 'Rol en Oportunidade',
    'LBL_OTHER_EMAIL_ADDRESS' => 'Email Alternativo:',
    'LBL_OTHER_PHONE' => 'Tel. Alternativo:',
    'LBL_PHONE' => 'Teléfono:',
    'LBL_PORTAL_APP' => 'Aplicación de Portal',
    'LBL_PORTAL_INFORMATION' => 'Información de Portal',
    'LBL_PORTAL_NAME' => 'Nome do Portal:',
    'LBL_STREET' => 'Rúa',
    'LBL_POSTAL_CODE' => 'Código postal:',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Cidade de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'CP de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Estado/Provincia de enderezo principal:',
    'LBL_PRIMARY_ADDRESS_STREET_2' => 'Rúa de enderezo principal 2',
    'LBL_PRIMARY_ADDRESS_STREET_3' => 'Rúa de enderezo principal 3',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Rúa de enderezo principal:',
    'LBL_PRIMARY_ADDRESS' => 'Enderezo principal:',
    'LBL_PRODUCTS_TITLE' => 'Produtos',
    'LBL_REPORTS_TO_ID' => 'Informa a ID:',
    'LBL_REPORTS_TO' => 'Informa a:',
    'LBL_RESOURCE_NAME' => 'Nome de Recurso',
    'LBL_SALUTATION' => 'Saúdo',
    'LBL_SAVE_CONTACT' => 'Gardar Contacto',
    'LBL_SEARCH_FORM_TITLE' => 'Busca de Contactos',
    'LBL_SELECT_CHECKED_BUTTON_LABEL' => 'Seleccionar Contactos Marcados',
    'LBL_SELECT_CHECKED_BUTTON_TITLE' => 'Seleccionar Contactos Marcados',
    'LBL_STATE' => 'Estado ou rexión:',
    'LBL_SYNC_CONTACT' => 'Sincronizar con Outlook&reg;',
    'LBL_PROSPECT_LIST' => 'Público Obxectivo',
    'LBL_TITLE' => 'Posto de traballo:',
    'LNK_CONTACT_LIST' => 'Ver Contactos',
    'LNK_IMPORT_VCARD' => 'Novo Contacto desde vCard',
    'LNK_NEW_ACCOUNT' => 'Crear unha conta',
    'LNK_NEW_APPOINTMENT' => 'Nova Cita',
    'LNK_NEW_CALL' => 'Rexistrar Chamada',
    'LNK_NEW_CASE' => 'Novo Caso',
    'LNK_NEW_CONTACT' => 'Novo Contacto',
    'LNK_NEW_EMAIL' => 'Arquivar Email',
    'LNK_NEW_MEETING' => 'Programar Reunión',
    'LNK_NEW_NOTE' => 'Nova Nota',
    'LNK_NEW_OPPORTUNITY' => 'Nova Oportunidade',
    'LNK_NEW_TASK' => 'Nova Tarefa',
    'LNK_SELECT_ACCOUNT' => "Seleccione Conta",
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro de que quere eliminar este rexistro?',
    'NTC_OPPORTUNITY_REQUIRES_ACCOUNT' => 'A creación dunha oportunidade require unha conta.\n Por favor, cree unha nova conta ou seleccione unha existente.',
    'NTC_REMOVE_CONFIRMATION' => '¿Está seguro que desexa eliminar este contacto do caso?',

    'LBL_LEADS_SUBPANEL_TITLE' => 'Clientes Potenciais',
    'LBL_OPPORTUNITIES_SUBPANEL_TITLE' => 'Oportunidades',
    'LBL_DOCUMENTS_SUBPANEL_TITLE' => 'Documentos',
    'LBL_COPY_ADDRESS_CHECKED_PRIMARY' => 'Copiar enderezo principal',
    'LBL_COPY_ADDRESS_CHECKED_ALT' => 'Copiar outra enderezo',

    'LBL_CASES_SUBPANEL_TITLE' => 'Casos',
    'LBL_BUGS_SUBPANEL_TITLE' => 'Incidencias',
    'LBL_PROJECTS_SUBPANEL_TITLE' => 'Proxectos',
    'LBL_PROJECTS_RESOURCES' => 'Proxectos de recursos',
    'LBL_CAMPAIGNS' => 'Campañas',
    'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE' => 'Campañas',
    'LBL_LIST_CITY' => 'Cidade',
    'LBL_LIST_STATE' => 'Estado ou rexión:',
    'LBL_HOMEPAGE_TITLE' => 'Os Meus Contactos',
    'LBL_OPPORTUNITIES' => 'Oportunidades',

    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactos',
    'LBL_PROJECT_SUBPANEL_TITLE' => 'Proxectos',
    'LNK_IMPORT_CONTACTS' => 'Importar Contactos',

    // SNIP
    'LBL_USER_SYNC' => 'Sincronización de usuario',

    'LBL_FP_EVENTS_CONTACTS_FROM_FP_EVENTS_TITLE' => 'Eventos',

    'LBL_AOP_CASE_UPDATES' => 'Actualizacións do caso',
    'LBL_CREATE_PORTAL_USER' => 'Crear Usuario do Portal',
    'LBL_ENABLE_PORTAL_USER' => 'Habilitar Usuario do Portal',
    'LBL_DISABLE_PORTAL_USER' => 'Deshabilitar o Usuario do Portal',
    'LBL_CREATE_PORTAL_USER_FAILED' => 'Erro ao crear o usuario do portal',
    'LBL_ENABLE_PORTAL_USER_FAILED' => 'Erro ao habilitar o usuario do portal',
    'LBL_DISABLE_PORTAL_USER_FAILED' => 'Erro ao deshabilitar o usuario do portal',
    'LBL_CREATE_PORTAL_USER_SUCCESS' => 'Usuario do portal creado',
    'LBL_ENABLE_PORTAL_USER_SUCCESS' => 'Usuario do portal habilitado',
    'LBL_DISABLE_PORTAL_USER_SUCCESS' => 'Usuario do portal deshabilitado',
    'LBL_NO_JOOMLA_URL' => 'Non se especificu URL do portal',
    'LBL_PORTAL_USER_TYPE' => 'Tipo de usuario do portal',
    'LBL_PORTAL_ACCOUNT_DISABLED' => 'Conta deshabilitada',
    'LBL_JOOMLA_ACCOUNT_ID' => 'ID de conta de Joomla',
   
    'LBL_ERROR_NO_PORTAL_SELECTED' => 'Non hai ningún portal seleccionado', // escaped single quotes required. PR 5426
    'LBL_PLEASE_UPDATE_DEPRECATED_PORTAL_ERROR' => 'Configuráronse máis dunha URL de portal pero non se admiten múltiples portais. Actualice o compoñente do portal no sitio: ',
    'LBL_PLEASE_UPDATE_DEPRECATED_PORTAL_WARNING' => 'O compoñente portal é obsoleto, por favor actualice o compoñente portal en sitio: ',

    'LBL_INVALID_USER_DATA' => 'Está intentando crear un usuario portal sen nome e/ou sen enderezo de correo electrónico. Por favor revise os detalles de contacto',
    'LBL_NO_RELATED_JACCOUNT' => 'Intentando desactivar un usuario do CRM sen unha conta relacionada Joomla Portal',
    'LBL_UNABLE_READ_PORTAL_VERSION' => 'Non se pode ler a version de AOP desde portal', // PR 5426
 
    'LBL_AOS_CONTRACTS' => 'Contratos',
    'LBL_AOS_INVOICES' => 'Facturas',
    'LBL_AOS_QUOTES' => 'Presupostos',
    'LBL_PROJECT_CONTACTS_1_FROM_PROJECT_TITLE' => 'Contactos do proxecto a partir do nome do proxecto',

    'LBL_LIST_INVITE_STATUS' => 'Estado de la Invitación',
);
