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
    'LBL_MODULE_NAME' => 'Monedas',
    'LBL_LIST_FORM_TITLE' => 'Monedas',
    'LBL_CURRENCY' => 'Moneda',
    'LBL_ADD' => 'Agregar',
    'LBL_MERGE' => 'Unificar',
    'LBL_MERGE_TXT' => 'Por favor, seleccione las monedas que desee asociar a la moneda seleccionada. Esto eliminará todas las monedas marcadas y reasignará cualquier valor asociado con ellas a la moneda seleccionada.',
    'LBL_US_DOLLAR' => 'Dólar EEUU',
    'LBL_DELETE' => 'Eliminar',
    'LBL_LIST_SYMBOL' => 'Símbolo',
    'LBL_LIST_NAME' => 'Nombre de Moneda',
    'LBL_LIST_ISO4217' => 'Código ISO 4217',
    'LBL_LIST_ISO4217_HELP' => 'Introduzca el código de tres letras ISO 4217 que define el nombre y el símbolo de la moneda.',
    'LBL_UPDATE' => 'Actualizar',
    'LBL_LIST_RATE' => 'Cambio',
    'LBL_LIST_RATE_HELP' => 'Un cambio de 0,5 para el Euro significa que 10 dólares EEUU = 5 Euros.',
    'LBL_LIST_STATUS' => 'Estado',
    'LNK_NEW_CONTACT' => 'Nuevo Contacto',
    'LNK_NEW_ACCOUNT' => 'Nueva Cuenta',
    'LNK_NEW_OPPORTUNITY' => 'Nueva Oportunidad',
    'LNK_NEW_CASE' => 'Nuevo Caso',
    'LNK_NEW_NOTE' => 'Nueva Nota',
    'LNK_NEW_CALL' => 'Nueva Llamada',
    'LNK_NEW_EMAIL' => 'Nuevo Email',
    'LNK_NEW_MEETING' => 'Nueva Reunión',
    'LNK_NEW_TASK' => 'Nueva Tarea',
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro de que desea eliminar esta moneda? Todo registro que use esta moneda será convertido a la moneda por defecto del sistema cuando sea accedido. Sería mejor dejarla inactiva.',
    'LBL_BELOW_MIN' => 'El ratio de conversión debe ser mayor que 0',
    'currency_status_dom' =>
        array(
            'Active' => 'Activa',
            'Inactive' => 'Inactiva',
        ),
    'LBL_CREATED_BY' => 'Creado por',
    'LBL_EDIT_LAYOUT' => 'Editar diseño' /*for 508 compliance fix*/,
);
