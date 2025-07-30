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
    //module
    'LBL_MODULE_NAME' => 'Documentos',
    'LBL_MODULE_TITLE' => 'Documentos: Inicio',
    'LNK_NEW_DOCUMENT' => 'Crear Documento',
    'LNK_DOCUMENT_LIST' => 'Lista de Documentos',
    'LBL_SEARCH_FORM_TITLE' => 'Búsqueda de Documentos',
    //vardef labels
    'LBL_NAME' => 'Nombre de Documento',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_ASSIGNED_TO' => 'Asignado a:',
    'LBL_CATEGORY' => 'Categoría',
    'LBL_SUBCATEGORY' => 'Subcategoría',
    'LBL_STATUS' => 'Estado',
    'LBL_IS_TEMPLATE' => 'Es una Plantilla',
    'LBL_TEMPLATE_TYPE' => 'Tipo de Documento',
    'LBL_REVISION_NAME' => 'Número de Versión',
    'LBL_MIME' => 'Tipo MIME',
    'LBL_REVISION' => 'Versión',
    'LBL_DOCUMENT' => 'Documento Relacionado',
    'LBL_LATEST_REVISION' => 'Última Versión',
    'LBL_CHANGE_LOG' => 'Historial de Cambios',
    'LBL_ACTIVE_DATE' => 'Fecha de Publicación',
    'LBL_EXPIRATION_DATE' => 'Fecha de Caducidad',
    'LBL_FILE_EXTENSION' => 'Extensión de Archivo',

    'LBL_CAT_OR_SUBCAT_UNSPEC' => 'Sin especificar',
    //quick search
    'LBL_NEW_FORM_TITLE' => 'Nuevo Documento',
    //document edit and detail view
    'LBL_DOC_NAME' => 'Nombre de Documento:',
    'LBL_FILENAME' => 'Nombre de Archivo:',
    'LBL_FILE_UPLOAD' => 'Archivo:',
    'LBL_DOC_VERSION' => 'Versión:',
    'LBL_CATEGORY_VALUE' => 'Categoría:',
    'LBL_SUBCATEGORY_VALUE' => 'Subcategoría:',
    'LBL_DOC_STATUS' => 'Estado:',
    'LBL_DET_TEMPLATE_TYPE' => 'Tipo de Documento:',
    'LBL_DOC_DESCRIPTION' => 'Descripción:',
    'LBL_DOC_ACTIVE_DATE' => 'Fecha de Publicación:',
    'LBL_DOC_EXP_DATE' => 'Fecha de Caducidad:',

    //document list view.
    'LBL_LIST_FORM_TITLE' => 'Lista de Documentos',
    'LBL_LIST_DOCUMENT' => 'Documento',
    'LBL_LIST_CATEGORY' => 'Categoría',
    'LBL_LIST_SUBCATEGORY' => 'Subcategoría',
    'LBL_LIST_REVISION' => 'Versión',
    'LBL_LIST_LAST_REV_CREATOR' => 'Publicado Por',
    'LBL_LIST_LAST_REV_DATE' => 'Fecha de Versión',
    'LBL_LIST_VIEW_DOCUMENT' => 'Ver',
    'LBL_LIST_ACTIVE_DATE' => 'Fecha de Publicación',
    'LBL_LIST_EXP_DATE' => 'Fecha de Caducidad',
    'LBL_LIST_STATUS' => 'Estado',

    //document search form.
    'LBL_SF_CATEGORY' => 'Categoría:',
    'LBL_SF_SUBCATEGORY' => 'Subcategoría:',

    'DEF_CREATE_LOG' => 'Documento Creado',

    //error messages
    'ERR_DOC_NAME' => 'Nombre de Documento',
    'ERR_DOC_ACTIVE_DATE' => 'Fecha de Publicación',
    'ERR_FILENAME' => 'Nombre de Archivo',

    //sub-panel vardefs.
    'LBL_LIST_DOCUMENT_NAME' => 'Nombre de Documento',

    'LBL_EDIT_BUTTON' => 'Editar',
    // STIC-Custom 20240214 JBL - QuickEdit view
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/93
    'LBL_QUICKEDIT_BUTTON' => '↙ Editar',
    // END STIC-Custom
    'LBL_REMOVE' => 'Quitar',

);
