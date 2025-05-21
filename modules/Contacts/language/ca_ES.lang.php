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
    'ERR_DELETE_RECORD' => 'Ha d\'especificar un número de registre a eliminar.',
    'LBL_ACCOUNT_ID' => 'ID de compte',
    'LBL_ACCOUNT_NAME' => 'Compte:',
    'LBL_CAMPAIGN' => 'Campanya:',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activitats',
    'LBL_ADDRESS_INFORMATION' => 'Informació d\'adreça',
    'LBL_ALT_ADDRESS_CITY' => 'Ciutat alternativa:',
    'LBL_ALT_ADDRESS_COUNTRY' => 'País alternatiu:',
    'LBL_ALT_ADDRESS_POSTALCODE' => 'Codi postal alternatiu:',
    'LBL_ALT_ADDRESS_STATE' => 'Estat/Província alternatiu:',
    'LBL_ALT_ADDRESS_STREET_2' => 'Carrer alternatiu 2:',
    'LBL_ALT_ADDRESS_STREET_3' => 'Carrer alternatiu 3:',
    'LBL_ALT_ADDRESS_STREET' => 'Carrer alternatiu:',
    'LBL_ALTERNATE_ADDRESS' => 'Adreça alternativa:',
    'LBL_ALT_ADDRESS' => 'Una altra adreça:',
    'LBL_ANY_ADDRESS' => 'Qualsevol adreça:',
    'LBL_ANY_EMAIL' => 'Qualsevol correu electrònic:',
    'LBL_ANY_PHONE' => 'Qualsevol Telèfon:',
    'LBL_ASSIGNED_TO_NAME' => 'Assignat a',
    'LBL_ASSIGNED_TO_ID' => 'Usuari Assignat',
    'LBL_ASSISTANT_PHONE' => 'Tel. assistent:',
    'LBL_ASSISTANT' => 'Assistent:',
    'LBL_BIRTHDATE' => 'Data de naixement:',
    'LBL_CITY' => 'Ciutat:',
    'LBL_CAMPAIGN_ID' => 'ID Campanya',
    'LBL_CONTACT_INFORMATION' => 'Visió general del contacte',  //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_CONTACT_NAME' => 'Nom Contacte:',
    'LBL_CONTACT_OPP_FORM_TITLE' => 'Oportunitat-Contacte:',
    'LBL_CONTACT_ROLE' => 'Rol:',
    'LBL_CONTACT' => 'Contacte:',
    'LBL_COUNTRY' => 'País:',
    'LBL_CREATED_ACCOUNT' => 'Nou compte creat',
    'LBL_CREATED_CALL' => 'Nova trucada creada',
    'LBL_CREATED_CONTACT' => 'Nou contacte creat',
    'LBL_CREATED_MEETING' => 'Nova reunió creada',
    'LBL_CREATED_OPPORTUNITY' => 'Creada nova oportunitat',
    'LBL_DATE_MODIFIED' => 'Data de Modificació',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Contactes',
    'LBL_DEPARTMENT' => 'Departament:',
    'LBL_DESCRIPTION' => 'Descripció:',
    'LBL_DIRECT_REPORTS_SUBPANEL_TITLE' => 'Informadors Directes',
    'LBL_DO_NOT_CALL' => 'No trucar:',
    'LBL_DUPLICATE' => 'Possible contacte duplicat',
    'LBL_EMAIL_ADDRESS' => 'Adreça de correu electrònic:',
    'LBL_EMAIL_OPT_OUT' => 'Refusar correu electrònic:',
    'LBL_EXISTING_ACCOUNT' => 'Usat compte existent',
    'LBL_EXISTING_CONTACT' => 'Usat contacte existent',
    'LBL_EXISTING_OPPORTUNITY' => 'Usada oportunitat existent',
    'LBL_FAX_PHONE' => 'Fax:',
    'LBL_FIRST_NAME' => 'Nom:',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_HOME_PHONE' => 'Tel. casa:',
    'LBL_ID' => 'ID:',
    'LBL_IMPORT_VCARD' => 'Importar vCard',
    'LBL_VCARD' => 'vCard',
    'LBL_IMPORT_VCARDTEXT' => 'Crea automáticamente un nou contacte a partir d\'una vCard.',
    'LBL_INVALID_EMAIL' => 'Correu electrònic no vàlid:',
    'LBL_INVITEE' => 'Informadors',
    'LBL_LAST_NAME' => 'Cognoms:',
    'LBL_LEAD_SOURCE' => 'Presa de contacte:',
    'LBL_LIST_ACCEPT_STATUS' => 'Estat d\'acceptació',
    'LBL_LIST_ACCOUNT_NAME' => 'Compte',
    'LBL_LIST_CONTACT_NAME' => 'Nom Contacte',
    'LBL_LIST_CONTACT_ROLE' => 'Rol',
    'LBL_LIST_EMAIL_ADDRESS' => 'Correu electrònic',
    'LBL_LIST_FIRST_NAME' => 'Nom',
    'LBL_LIST_FORM_TITLE' => 'Llista de Contactes',
    'LBL_LIST_LAST_NAME' => 'Cognoms',
    'LBL_LIST_NAME' => 'Nom complet',
    'LBL_LIST_PHONE' => 'Telèfon',
    'LBL_LIST_TITLE' => 'Càrrec',
    'LBL_MOBILE_PHONE' => 'Mòbil:',
    'LBL_MODIFIED' => 'Modificat per:',
    'LBL_MODULE_NAME' => 'Contactes',
    'LBL_MODULE_TITLE' => 'Contactes: Inici',
    'LBL_NAME' => 'Nom complet:',
    'LBL_NEW_FORM_TITLE' => 'Nou Contacte',
    'LBL_NOTE_SUBJECT' => 'Assumpte de Nota',
    'LBL_OFFICE_PHONE' => 'Tel. oficina:',
    'LBL_OPP_NAME' => 'Nom oportunitat:',
    'LBL_OPPORTUNITY_ROLE_ID' => 'ID de Rol en Oportunitat:',
    'LBL_OPPORTUNITY_ROLE' => 'Rol en Oportunitat',
    'LBL_OTHER_EMAIL_ADDRESS' => 'Correu electrònic alternatiu:',
    'LBL_OTHER_PHONE' => 'Tel. alternatiu:',
    'LBL_PHONE' => 'Telèfon:',
    'LBL_PORTAL_APP' => 'Aplicació de Portal',
    'LBL_PORTAL_INFORMATION' => 'Informació de Portal',
    'LBL_PORTAL_NAME' => 'Nom del Portal:',
    'LBL_STREET' => 'Carrer',
    'LBL_POSTAL_CODE' => 'Codi postal:',
    'LBL_PRIMARY_ADDRESS_CITY' => 'Ciutat principal:',
    'LBL_PRIMARY_ADDRESS_COUNTRY' => 'País principal:',
    'LBL_PRIMARY_ADDRESS_POSTALCODE' => 'Codi postal principal:',
    'LBL_PRIMARY_ADDRESS_STATE' => 'Estat/Província principal:',
    'LBL_PRIMARY_ADDRESS_STREET_2' => 'Carrer principal 2:',
    'LBL_PRIMARY_ADDRESS_STREET_3' => 'Carrer principal 3:',
    'LBL_PRIMARY_ADDRESS_STREET' => 'Carrer principal:',
    'LBL_PRIMARY_ADDRESS' => 'Adreça principal:',
    'LBL_PRODUCTS_TITLE' => 'Productes',
    'LBL_REPORTS_TO_ID' => 'Informa a ID:',
    'LBL_REPORTS_TO' => 'Informa a:',
    'LBL_RESOURCE_NAME' => 'Nom de Recurs',
    'LBL_SALUTATION' => 'Salutacio',
    'LBL_SAVE_CONTACT' => 'Desar Contacte',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca de Contactes',
    'LBL_SELECT_CHECKED_BUTTON_LABEL' => 'Seleccionar Contactes Marcats',
    'LBL_SELECT_CHECKED_BUTTON_TITLE' => 'Seleccionar Contactes Marcats',
    'LBL_STATE' => 'Estat o regió:',
    'LBL_SYNC_CONTACT' => 'Sincronitzar amb Outlook&reg;',
    'LBL_PROSPECT_LIST' => 'Públic Objectiu',
    'LBL_TITLE' => 'Càrrec:',
    'LNK_CONTACT_LIST' => 'Contactes',
    'LNK_IMPORT_VCARD' => 'Importar vCard',
    'LNK_NEW_ACCOUNT' => 'Nou Compte',
    'LNK_NEW_APPOINTMENT' => 'Nova Cita',
    'LNK_NEW_CALL' => 'Programar Trucada',
    'LNK_NEW_CASE' => 'Nou Cas',
    'LNK_NEW_CONTACT' => 'Nou Contacte',
    'LNK_NEW_EMAIL' => 'Arxivar correu electrònic',
    'LNK_NEW_MEETING' => 'Programar Reunió',
    'LNK_NEW_NOTE' => 'Nova Nota',
    'LNK_NEW_OPPORTUNITY' => 'Nova Oportunitat',
    'LNK_NEW_TASK' => 'Nova Tasca',
    'LNK_SELECT_ACCOUNT' => "Seleccioni Compte",
    'NTC_DELETE_CONFIRMATION' => 'Està segur que vol eliminar aquest registre?',
    'NTC_OPPORTUNITY_REQUIRES_ACCOUNT' => 'La creació d\'una oportunitat requereix una cuenta.\n Si us plau, creï un nou compte o en seleccioni una existent.',
    'NTC_REMOVE_CONFIRMATION' => 'Està segur que desitja eliminar aquest contacte del cas?',

    'LBL_LEADS_SUBPANEL_TITLE' => 'Clients Potencials',
    'LBL_OPPORTUNITIES_SUBPANEL_TITLE' => 'Oportunitats',
    'LBL_DOCUMENTS_SUBPANEL_TITLE' => 'Documents',
    'LBL_COPY_ADDRESS_CHECKED_PRIMARY' => "Copia l'adreça principal",
    'LBL_COPY_ADDRESS_CHECKED_ALT' => "Copia una altra adreça",

    'LBL_CASES_SUBPANEL_TITLE' => 'Casos',
    'LBL_BUGS_SUBPANEL_TITLE' => 'Incidències',
    'LBL_PROJECTS_SUBPANEL_TITLE' => 'Projectes',
    'LBL_PROJECTS_RESOURCES' => 'Recursos de projectes',
    'LBL_CAMPAIGNS' => 'Campanyes',
    'LBL_CAMPAIGN_LIST_SUBPANEL_TITLE' => 'Campanyes',
    'LBL_LIST_CITY' => 'Ciutat',
    'LBL_LIST_STATE' => 'Estat o regió:',
    'LBL_HOMEPAGE_TITLE' => 'Els Meus Contactes',
    'LBL_OPPORTUNITIES' => 'Oportunitats',

    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactes',
    'LBL_PROJECT_SUBPANEL_TITLE' => 'Projectes',
    'LNK_IMPORT_CONTACTS' => 'Importar contactes',

    // SNIP
    'LBL_USER_SYNC' => 'Sincronitzar usuari',

    'LBL_FP_EVENTS_CONTACTS_FROM_FP_EVENTS_TITLE' => 'Esdeveniments',

    'LBL_AOP_CASE_UPDATES' => 'Actualitzacions de casos',
    'LBL_CREATE_PORTAL_USER' => 'Crear portal d\'usuari',
    'LBL_ENABLE_PORTAL_USER' => 'Habilitar portal d\'usuari',
    'LBL_DISABLE_PORTAL_USER' => 'Deshabilitar portal d\'usuari',
    'LBL_CREATE_PORTAL_USER_FAILED' => 'Error al crear el portal d\'usuari',
    'LBL_ENABLE_PORTAL_USER_FAILED' => 'Error al habilitar el portal d\'usuari',
    'LBL_DISABLE_PORTAL_USER_FAILED' => 'Error al deshabilitar el portal d\'usuari',
    'LBL_CREATE_PORTAL_USER_SUCCESS' => 'S\'ha creat el portal d\'usuari',
    'LBL_ENABLE_PORTAL_USER_SUCCESS' => 'S\'ha habilitat el portal d\'usuari',
    'LBL_DISABLE_PORTAL_USER_SUCCESS' => 'S\'ha deshabilitat el portal d\'usuari',
    'LBL_NO_JOOMLA_URL' => 'No s\'ha especificat la URL del portal',
    'LBL_PORTAL_USER_TYPE' => 'Tipus de portal d\'usuari',
    'LBL_PORTAL_ACCOUNT_DISABLED' => 'Compta desactivada',
    'LBL_JOOMLA_ACCOUNT_ID' => 'ID de compte de Joomla',
   
    'LBL_ERROR_NO_PORTAL_SELECTED' => 'No hi ha cap portal seleccionat', // escaped single quotes required. PR 5426
    'LBL_PLEASE_UPDATE_DEPRECATED_PORTAL_ERROR' => 'S\'ha trobat més d\'un portal URL, però múltiples portals no son compatibles, si us plau actualitzi el portal component en lloc: ',
    'LBL_PLEASE_UPDATE_DEPRECATED_PORTAL_WARNING' => 'Portal és obsolet, si us plau actualitzi el portal en lloc: ',

    'LBL_INVALID_USER_DATA' => 'Esteu creant un usuari al portal sense nom i/o adreça de correu. Comproveu les dades de contacte.',
    'LBL_NO_RELATED_JACCOUNT' => 'Esteu intentant desactivar un usuari de CRM sense compte relacionats al portal Joomla.',
    'LBL_UNABLE_READ_PORTAL_VERSION' => 'No es pot llegir la versió AOP del portal', // PR 5426
 
    'LBL_AOS_CONTRACTS' => 'Contractes',
    'LBL_AOS_INVOICES' => 'Factures',
    'LBL_AOS_QUOTES' => 'Pressupostos',
    'LBL_PROJECT_CONTACTS_1_FROM_PROJECT_TITLE' => 'Contactes de projecte del títol del projecte',

    'LBL_LIST_INVITE_STATUS' => 'Estat de la Invitació',
);
