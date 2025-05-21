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
    'LBL_ACCOUNT_ID' => 'ID Compte:',
    'LBL_CASE_ID' => 'ID Cas:',
    'LBL_CLOSE' => 'Tancar:',
    'LBL_CONTACT_ID' => 'ID Contacte:',
    'LBL_CONTACT_NAME' => 'Contacte:',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Notes',
    'LBL_DESCRIPTION' => 'Descripció',
    'LBL_EMAIL_ADDRESS' => 'Adreça de correu electrònic:',
    'LBL_EMAIL_ATTACHMENT' => 'Adjunt de correu electrònic:',
    'LBL_FILE_MIME_TYPE' => 'Tipus MIME',
    'LBL_FILE_URL' => 'URL d\'Arxiu',
    'LBL_FILENAME' => 'Adjunt:',
    'LBL_LEAD_ID' => 'ID Client Potencial:',
    'LBL_LIST_CONTACT_NAME' => 'Contacte',
    'LBL_LIST_DATE_MODIFIED' => 'Modificat',
    'LBL_LIST_FILENAME' => 'Adjunt',
    'LBL_LIST_FORM_TITLE' => 'Llista de Notes',
    'LBL_LIST_RELATED_TO' => 'Relacionat amb',
    'LBL_LIST_SUBJECT' => 'Assumpte',
    'LBL_LIST_STATUS' => 'Estat',
    'LBL_LIST_CONTACT' => 'Contacte',
    'LBL_MODULE_NAME' => 'Notes',
    'LBL_MODULE_TITLE' => 'Notes: Inici',
    'LBL_NEW_FORM_TITLE' => 'Nova Nota o Arxiu Adjunt',
    'LBL_NOTE_STATUS' => 'Nota',
    'LBL_NOTE_SUBJECT' => 'Assumpte:',
    'LBL_NOTES_SUBPANEL_TITLE' => 'Adjunts',
    'LBL_NOTE' => 'Nota:',
    'LBL_OPPORTUNITY_ID' => 'ID Oportunitat:',
    'LBL_PARENT_ID' => 'ID Padre:',
    'LBL_PARENT_TYPE' => 'Tipus de Padre',
    'LBL_PHONE' => 'Telèfon:',
    'LBL_PORTAL_FLAG' => 'Mostrar en el Portal?',
    'LBL_EMBED_FLAG' => 'Incloure al correu electrònic?',
    'LBL_PRODUCT_ID' => 'ID Producte:',
    'LBL_QUOTE_ID' => 'ID Pressupost:',
    'LBL_RELATED_TO' => 'Relacionat amb:',
    'LBL_SEARCH_FORM_TITLE' => 'Cerca de Notes',
    'LBL_STATUS' => 'Estat',
    'LBL_SUBJECT' => 'Assumpte:',
    'LNK_IMPORT_NOTES' => 'Importa Notes',
    'LNK_NEW_NOTE' => 'Nova Nota o Adjunt',
    'LNK_NOTE_LIST' => 'Notes',
    'LBL_MEMBER_OF' => 'Membre de:',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Usuari Assignat',
    'LBL_REMOVING_ATTACHMENT' => 'Traient adjunt...',
    'ERR_REMOVING_ATTACHMENT' => 'Error al treure adjunt...',
    'LBL_CREATED_BY' => 'Creat Per',
    'LBL_MODIFIED_BY' => 'Modificat Per',
    'LBL_SEND_ANYWAYS' => 'Aquest correu electrònic no te assumpte. Enviar/desar de totes maneres?',
    'LBL_NOTE_INFORMATION' => 'Visió general de la nota', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_MY_NOTES_DASHLETNAME' => 'Les meves notes',
    'LBL_EDITLAYOUT' => 'Editar Diseny' /*for 508 compliance fix*/,
    //For export labels
    'LBL_FIRST_NAME' => 'Nom',
    'LBL_LAST_NAME' => 'Cognoms',
    'LBL_DATE_ENTERED' => 'Data de Creació',
    'LBL_DATE_MODIFIED' => 'Última Modificació',
    'LBL_DELETED' => 'Esborrat',
    'LBL_FILE_CONTENTS' => 'Continguts del fitxer',

    'LBL_ASCENDING' => 'Ascendent',
    'LBL_DESCENDING' => 'Descendent',
);
