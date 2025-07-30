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

    'LBL_ADMINISTRATION_MAIN' => 'Configuración de Conectores',
    'LBL_AVAILABLE' => 'Disponible',
    'LBL_BACK' => '< Atrás',
    'LBL_CONFIRM_CONTINUE_SAVE' => 'Algunos campos requeridos se han dejado en blanco.  ¿Desea continuar y guardar los cambios?',
    'LBL_CONNECTOR_FIELDS' => 'Campos del Conector',
    'LBL_DATA' => 'Datos',
    'LBL_DEFAULT' => 'Por Defecto',
    'LBL_DISABLED' => 'Deshabilitado',
    'LBL_ENABLED' => 'Habilitado',
    'LBL_EXTERNAL' => 'Permiten a los usuarios crear registros en cuentas externas.',
    'LBL_EXTERNAL_SET_PROPERTIES' => 'Para poder usar este conector, las propiedades del conector también se debe definir en la página de configuración.',
    'LBL_MERGE' => 'Combinar',
    'LBL_MODIFY_DISPLAY_TITLE' => 'Habilitar Conectores',
    'LBL_MODIFY_DISPLAY_DESC' => 'Seleccione qué módulos están habilitados para cada conector.',
    'LBL_MODULE_FIELDS' => 'Campos del Módulo',
    'LBL_MODIFY_MAPPING_TITLE' => 'Mapear Campos del Conector',
    'LBL_MODIFY_MAPPING_DESC' => 'Mapear campos del conector a campos del módulo para determinar qué datos del conector pueden ser vistos y combinados junto con los registros del módulo.',
    'LBL_MODIFY_PROPERTIES_TITLE' => 'Establecer Propiedades del Conector',
    'LBL_MODIFY_PROPERTIES_DESC' => 'Configurar las propiedades de cada conector, incluyendo los URLs y las claves del API.',
    'LBL_MODIFY_SEARCH_TITLE' => 'Administrar Búsqueda de Conectores',
    'LBL_MODIFY_SEARCH' => 'Búsqueda',
    'LBL_MODIFY_SEARCH_DESC' => 'Seleccione los campos del conector a utilizar para la búsqueda de datos de cada módulo.',
    'LBL_MODULE_NAME' => 'Conectores',
    'LBL_NO_PROPERTIES' => 'No hay propiedades configurables para este conector.',
    'LBL_SAVE' => 'Guardar',
    'LBL_SUMMARY' => 'Resumen',
    'LBL_STEP1' => 'Buscar y Ver Datos',
    'LBL_STEP2' => 'Combinar Registros con',
    'LBL_TEST_SOURCE' => 'Prueba de Conector',
    'LBL_TEST_SOURCE_FAILED' => 'Error en Prueba',
    'LBL_TEST_SOURCE_SUCCESS' => 'Prueba Exitosa',
    'LBL_TITLE' => 'Combinación de Datos',

    'ERROR_NO_ADDITIONAL_DETAIL' => 'Error: No se han encontrado más detalles para el registro.',
    'ERROR_NO_SEARCHDEFS_DEFINED' => 'No se han habilitado módulos para este conector.  Seleccione un módulo para este conector en la página Habilitar Conectores.',
    'ERROR_NO_SEARCHDEFS_MAPPED' => 'Error: No hay ningún conector habilitado con campos de búsqueda definidos.',
    'ERROR_NO_SEARCHDEFS_MAPPING' => 'Error: No se han definido campos de búsqueda para el módulo y el conector.  Por favor, póngase en contacto con el administrador del sistema.',
    'ERROR_NO_DISPLAYABLE_MAPPED_FIELDS' => 'Error: No se ha mapeado ningún campo de módulo para ser mostrado como parte de los resultados.  Por favor, póngase en contacto con el administrador del sistema.',
    'LBL_INFO_INLINE' => 'Información' /*for 508 compliance fix*/,
    'LBL_CLOSE' => 'Cierre' /*for 508 compliance fix*/,

);
