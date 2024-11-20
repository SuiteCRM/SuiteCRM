<?php
/**
 * This file is part of Mail Merge Reports by Izertis.
 * Copyright (C) 2015 Izertis.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * You can contact Izertis at email address info@izertis.com.
 */
// STIC-Custom 20241009 ART - Does not show available fields in Popup View
// https://github.com/SinergiaTIC/SinergiaCRM/pull/432

// if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

// $module_name = 'DHA_PlantillasDocumentos';
// $object_name = 'DHA_PlantillasDocumentos';
// $_module_name = 'dha_plantillasdocumentos';
// $popupMeta = array(
//    'moduleMain' => $module_name,
//    'varName' => $object_name,
//    'orderBy' => $_module_name.'.name',
//    'whereClauses' => array(
//       'name' => $_module_name . '.name',
//    ),
//    'searchInputs'=> array(
//       'name',
//    ),
// );
$popupMeta = array(
    'moduleMain' => 'DHA_PlantillasDocumentos',
    'varName' => 'DHA_PlantillasDocumentos',
    'orderBy' => 'dha_plantillasdocumentos.name',
    'whereClauses' => array(
        'document_name' => 'dha_plantillasdocumentos.document_name',
        'modulo' => 'dha_plantillasdocumentos.modulo',
        'idioma' => 'dha_plantillasdocumentos.idioma',
        'status_id' => 'dha_plantillasdocumentos.status_id',
        'assigned_user_id' => 'dha_plantillasdocumentos.assigned_user_id',
        'description' => 'dha_plantillasdocumentos.description',
        'created_by' => 'dha_plantillasdocumentos.created_by',
        'date_entered' => 'dha_plantillasdocumentos.date_entered',
        'modified_user_id' => 'dha_plantillasdocumentos.modified_user_id',
        'date_modified' => 'dha_plantillasdocumentos.date_modified',
        'aclroles' => 'dha_plantillasdocumentos.aclroles',
        'current_user_only' => 'dha_plantillasdocumentos.current_user_only',
        'favorites_only' => 'dha_plantillasdocumentos.favorites_only',
    ),
    'searchInputs' => array(
        1 => 'document_name',
        2 => 'modulo',
        3 => 'idioma',
        4 => 'status_id',
        5 => 'assigned_user_id',
        6 => 'description',
        7 => 'created_by',
        8 => 'date_entered',
        9 => 'modified_user_id',
        10 => 'date_modified',
        11 => 'aclroles',
        12 => 'current_user_only',
        13 => 'favorites_only',
    ),
    'searchdefs' => array(
        'document_name' => array(
            'name' => 'document_name',
            'width' => '10%',
        ),
        'modulo' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_MODULO',
            'width' => '10%',
            'name' => 'modulo',
        ),
        'idioma' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_IDIOMA_PLANTILLA',
            'width' => '10%',
            'name' => 'idioma',
        ),
        'status_id' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_DOC_STATUS',
            'width' => '10%',
            'name' => 'status_id',
        ),
        'assigned_user_id' => array(
            'name' => 'assigned_user_id',
            'label' => 'LBL_ASSIGNED_TO',
            'type' => 'enum',
            'function' => array(
                'name' => 'get_user_array',
                'params' => array(
                    0 => false,
                ),
            ),
            'width' => '10%',
        ),
        'description' => array(
            'type' => 'text',
            'label' => 'LBL_DESCRIPTION',
            'sortable' => false,
            'width' => '10%',
            'name' => 'description',
        ),
        'created_by' => array(
            'type' => 'assigned_user_name',
            'label' => 'LBL_CREATED',
            'width' => '10%',
            'name' => 'created_by',
        ),
        'date_entered' => array(
            'type' => 'datetime',
            'label' => 'LBL_DATE_ENTERED',
            'width' => '10%',
            'name' => 'date_entered',
        ),
        'modified_user_id' => array(
            'type' => 'assigned_user_name',
            'label' => 'LBL_MODIFIED',
            'width' => '10%',
            'name' => 'modified_user_id',
        ),
        'date_modified' => array(
            'type' => 'datetime',
            'label' => 'LBL_DATE_MODIFIED',
            'width' => '10%',
            'name' => 'date_modified',
        ),
        'aclroles' => array(
            'type' => 'multienum',
            'studio' => 'visible',
            'label' => 'LBL_ROLES_WITH_ACCESS',
            'width' => '10%',
            'name' => 'aclroles',
        ),
        'current_user_only' => array(
            'label' => 'LBL_CURRENT_USER_FILTER',
            'type' => 'bool',
            'width' => '10%',
            'name' => 'current_user_only',
        ),
        'favorites_only' => array(
            'name' => 'favorites_only',
            'label' => 'LBL_FAVORITES_FILTER',
            'type' => 'bool',
            'width' => '10%',
        ),
    ),
    'listviewdefs' => array(
        'FILE_URL' => array(
            'width' => '2%',
            'label' => '&nbsp;',
            'link' => true,
            'default' => true,
            'related_fields' => array(
                0 => 'file_ext',
            ),
            'sortable' => false,
            'studio' => false,
            'name' => 'file_url',
        ),
        'MODULO_URL' => array(
            'width' => '2%',
            'label' => '&nbsp;',
            'link' => false,
            'default' => true,
            'sortable' => false,
            'studio' => false,
            'name' => 'modulo_url',
        ),
        'DOCUMENT_NAME' => array(
            'width' => '25%',
            'label' => 'LBL_NAME',
            'link' => true,
            'default' => true,
            'name' => 'document_name',
        ),
        'MODULO' => array(
            'type' => 'enum',
            'default' => true,
            'studio' => 'visible',
            'label' => 'LBL_MODULO',
            'width' => '10%',
            'name' => 'modulo',
        ),
        'IDIOMA' => array(
            'type' => 'enum',
            'default' => true,
            'studio' => 'visible',
            'label' => 'LBL_IDIOMA_PLANTILLA',
            'width' => '10%',
            'name' => 'idioma',
        ),
        'STATUS_ID' => array(
            'type' => 'enum',
            'default' => true,
            'studio' => 'visible',
            'label' => 'LBL_DOC_STATUS',
            'width' => '10%',
            'name' => 'status_id',
        ),
        'ASSIGNED_USER_NAME' => array(
            'link' => 'assigned_user_link',
            'type' => 'relate',
            'label' => 'LBL_ASSIGNED_TO_NAME',
            'width' => '10%',
            'default' => true,
            'module' => 'Employees',
            'name' => 'assigned_user_name',
        ),
    ),
);
// END STIC-Custom
