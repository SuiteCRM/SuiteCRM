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
    'LBL_ASSIGNED_TO_ID' => 'Asignado a (ID)',
    'LBL_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_ASSIGNED_TO' => 'Asignado a',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Asignado a',
    'LBL_LIST_ASSIGNED_USER' => 'Asignado a',
    'LBL_CREATED' => 'Creado por',
    'LBL_CREATED_USER' => 'Creado por',
    'LBL_CREATED_ID' => 'Creado por (ID)',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado por',
    'LBL_MODIFIED_USER' => 'Modificado por',
    'LBL_MODIFIED_ID' => 'Modificado por (ID)',
    'LBL_SECURITYGROUPS' => 'Grupos de Seguridad',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de Seguridad',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Fecha de Creación',
    'LBL_DATE_MODIFIED' => 'Fecha de Modificación',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nombre',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_EDIT_BUTTON' => 'Editar',
    'LBL_REMOVE' => 'Quitar',

    'LBL_CONTRACT_ACCOUNT' => 'Cuenta',
    'LBL_OPPORTUNITY' => 'Oportunidad',
    'LBL_LIST_FORM_TITLE' => 'Lista de Contratos',
    'LBL_MODULE_NAME' => 'Contratos',
    'LBL_MODULE_TITLE' => 'Contratos',
    'LBL_HOMEPAGE_TITLE' => 'Mis Contratos',
    'LNK_NEW_RECORD' => 'Crear Contrato',
    'LNK_LIST' => 'Ver Contratos',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Contratos',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_NEW_FORM_TITLE' => 'Nuevo Contrato',
    'LBL_CONTRACT_NAME' => 'Nombre del Contrato',
    'LBL_REFERENCE_CODE ' => 'Código',
    'LBL_START_DATE' => 'Fecha de inicio',
    'LBL_END_DATE' => 'Fecha de finalización',
    'LBL_TOTAL_CONTRACT_VALUE' => 'Valor del Contrato',
    'LBL_STATUS' => 'Estado',
    'LBL_CUSTOMER_SIGNED_DATE' => 'Fecha de Firma del Cliente',
    'LBL_COMPANY_SIGNED_DATE' => 'Fecha de Firma de la Compañía',
    'LBL_RENEWAL_REMINDER' => 'Recordatorio de Renovación de Contrato',
    'LBL_RENEWAL_REMINDER_DATE' => 'Fecha de Recordatorio de Renovación',
    'LBL_CONTRACT_TYPE' => 'Tipo',
    'LBL_CONTACT' => 'Contacto',
    'LBL_ADD_GROUP' => 'Nuevo Grupo',
    'LBL_DELETE_GROUP' => 'Eliminar el Grupo',
    'LBL_GROUP_NAME' => 'Nombre del Grupo',
    'LBL_GROUP_TOTAL' => 'Total del Grupo',
    'LBL_PRODUCT_QUANITY' => 'Cantidad',
    'LBL_PRODUCT_NAME' => 'Producto',
    'LBL_PART_NUMBER' => 'Código',
    'LBL_PRODUCT_NOTE' => 'Nota',
    'LBL_PRODUCT_DESCRIPTION' => 'Descripción',
    'LBL_LIST_PRICE' => 'Precio de lista',
    'LBL_DISCOUNT_AMT' => 'Descuento',
    'LBL_UNIT_PRICE' => 'Precio con descuento',
    'LBL_TOTAL_PRICE' => 'Precio total',
    'LBL_VAT' => '% IVA',
    'LBL_VAT_AMT' => 'IVA',
    'LBL_SERVICE_NAME' => 'Servicio',
    'LBL_SERVICE_LIST_PRICE' => 'Precio de lista',
    'LBL_SERVICE_PRICE' => 'Precio con descuento',
    'LBL_SERVICE_DISCOUNT' => 'Descuento',
    'LBL_LINE_ITEMS' => 'Artículos',
    'LBL_SUBTOTAL_AMOUNT' => 'Subtotal',
    'LBL_DISCOUNT_AMOUNT' => 'Descuento',
    'LBL_TAX_AMOUNT' => 'IVA',
    'LBL_SHIPPING_AMOUNT' => 'Envío',
    'LBL_TOTAL_AMT' => 'Total',
    'LBL_GRAND_TOTAL' => 'Total global',
    'LBL_SHIPPING_TAX' => '% IVA envío',
    'LBL_SHIPPING_TAX_AMT' => 'IVA envío',
    'LBL_ADD_PRODUCT_LINE' => 'Nueva línea de producto',
    'LBL_ADD_SERVICE_LINE' => 'Nueva línea de servicio',
    'LBL_PRINT_AS_PDF' => 'Generar documento PDF',
    'LBL_EMAIL_PDF' => 'Enviar PDF por Email',
    'LBL_PDF_NAME' => 'Contrato',
    'LBL_EMAIL_NAME' => 'Contrato para',
    'LBL_NO_TEMPLATE' => 'ERROR: No se han encontrado plantillas. Si usted no ha creado una plantilla de Contrato, vaya al módulo de Plantillas PDF y cree una.',
    'LBL_TOTAL_CONTRACT_VALUE_USDOLLAR' => 'Valor del contrato (moneda predeterminada)',
    'LBL_SUBTOTAL_AMOUNT_USDOLLAR' => 'Subtotal (moneda predeterminada)',
    'LBL_DISCOUNT_AMOUNT_USDOLLAR' => 'Descuento (moneda predeterminada)',
    'LBL_TAX_AMOUNT_USDOLLAR' => 'IVA (moneda predeterminada)',
    'LBL_SHIPPING_AMOUNT_USDOLLAR' => 'Envío (moneda predeterminada)',
    'LBL_TOTAL_AMT_USDOLLAR' => 'Total (moneda predeterminada)',
    'LBL_SHIPPING_TAX_AMT_USDOLLAR' => 'IVA de envío (moneda predeterminada)',
    'LBL_GRAND_TOTAL_USDOLLAR' => 'Total global (moneda predeterminada)',

    'LBL_CALL_ID' => 'Identificador de llamada',
    'LBL_AOS_LINE_ITEM_GROUPS' => 'Grupos de líneas de presupuesto',
    'LBL_AOS_PRODUCT_QUOTES' => 'Líneas de Presupuesto',
    'LBL_AOS_QUOTES_AOS_CONTRACTS' => 'Presupuestos: Contratos',
);