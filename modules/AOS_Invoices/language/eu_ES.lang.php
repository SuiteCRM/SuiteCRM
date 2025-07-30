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

    'ERR_DELETE_RECORD' => 'Debe especificar un registro para borrarlo de la cuenta.',
    'LBL_ACCOUNT_NAME' => 'Nombre',
    'LBL_ACCOUNT' => 'Cuenta:',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_ADDRESS_INFORMATION' => 'Información de Dirección',
    'LBL_ANNUAL_REVENUE' => 'Ingresos Anuales:',
    'LBL_ANY_ADDRESS' => 'Cualquier Dirección:',
    'LBL_ANY_EMAIL' => 'Cualquier Email:',
    'LBL_ANY_PHONE' => 'Cualquier Teléfono:',
    'LBL_RATING' => 'Puntuación',
    'LBL_ASSIGNED_USER' => 'Asignado a:',
    'LBL_BILLING_ADDRESS_CITY' => 'Facturación - Población:',
    'LBL_BILLING_ADDRESS_COUNTRY' => 'Facturación - País:',
    'LBL_BILLING_ADDRESS_POSTALCODE' => 'Facturación - Código Postal:',
    'LBL_BILLING_ADDRESS_STATE' => 'Facturación - Provincia:',
    'LBL_BILLING_ADDRESS_STREET_2' => 'Facturación - Calle 2',
    'LBL_BILLING_ADDRESS_STREET_3' => 'Facturación - Calle 3',
    'LBL_BILLING_ADDRESS_STREET_4' => 'Facturación - Calle 4',
    'LBL_BILLING_ADDRESS_STREET' => 'Facturación - Calle:',
    'LBL_BILLING_ADDRESS' => 'Dirección de Facturación:',
    'LBL_ACCOUNT_INFORMATION' => 'Visión general',
    'LBL_CITY' => 'Población:',
    'LBL_CONTACTS_SUBPANEL_TITLE' => 'Contactos',
    'LBL_COUNTRY' => 'País:',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Cuentas',
    'LBL_DUPLICATE' => 'Posible cuenta duplicada',
    'LBL_EMAIL' => 'Email:',
    'LBL_EMPLOYEES' => 'Empleados:',
    'LBL_FAX' => 'Fax:',
    'LBL_INDUSTRY' => 'Industria:',
    'LBL_LIST_ACCOUNT_NAME' => 'Cuenta',
    'LBL_LIST_CITY' => 'Población',
    'LBL_LIST_EMAIL_ADDRESS' => 'Email',
    'LBL_LIST_PHONE' => 'Teléfono',
    'LBL_LIST_STATE' => 'Provincia',
    'LBL_MEMBER_OF' => 'Miembro de:',
    'LBL_MEMBER_ORG_SUBPANEL_TITLE' => 'Organizaciones Miembro',
    'LBL_OTHER_EMAIL_ADDRESS' => 'Otro Email:',
    'LBL_OTHER_PHONE' => 'Teléfono alternativo 2:',
    'LBL_OWNERSHIP' => 'Propiedad:',
    'LBL_PARENT_ACCOUNT_ID' => 'ID de la Cuenta origen',
    'LBL_PHONE_ALT' => 'Teléfono alternativo:',
    'LBL_PHONE_FAX' => 'Fax:',
    'LBL_PHONE_OFFICE' => 'Teléfono de Oficina:',
    'LBL_PHONE' => 'Teléfono:',
    'LBL_POSTAL_CODE' => 'Código Postal:',
    'LBL_SAVE_ACCOUNT' => 'Guardar Cuenta',
    'LBL_SHIPPING_ADDRESS_CITY' => 'Envío - Población:',
    'LBL_SHIPPING_ADDRESS_COUNTRY' => 'Envío - País:',
    'LBL_SHIPPING_ADDRESS_POSTALCODE' => 'Envío - Código Postal:',
    'LBL_SHIPPING_ADDRESS_STATE' => 'Envío - Provincia:',
    'LBL_SHIPPING_ADDRESS_STREET_2' => 'Envío - Calle 2',
    'LBL_SHIPPING_ADDRESS_STREET_3' => 'Envío - Calle 3',
    'LBL_SHIPPING_ADDRESS_STREET_4' => 'Envío - Calle 4',
    'LBL_SHIPPING_ADDRESS_STREET' => 'Envío - Calle:',
    'LBL_SHIPPING_ADDRESS' => 'Dirección de Envío:',
    'LBL_STATE' => 'Provincia:',
    'LBL_TICKER_SYMBOL' => 'Identificador bursátil:',
    'LBL_TYPE' => 'Tipo:',
    'LBL_WEBSITE' => 'Web:',
    'LNK_ACCOUNT_LIST' => 'Cuentas',
    'LNK_NEW_ACCOUNT' => 'Nueva cuenta',
    'MSG_DUPLICATE' => "El registro que está a punto de crear podría ser un duplicado de otra cuenta existente. Los registros de cuentas con nombres similares se listan a continuación. Para confirmar la creación de esta cuenta haga click en Guardar. En caso contrario, haga click en Cancelar.",
    'MSG_SHOW_DUPLICATES' => "El registro que está a punto de crear podría ser un duplicado de otra cuenta existente. Los registros de cuentas con nombres similares se listan a continuación. Para confirmar la creación de esta cuenta haga click en Guardar. En caso contrario, haga click en Cancelar.",
    'NTC_DELETE_CONFIRMATION' => '¿Está seguro que quiere eliminar este registro?',
    'LBL_LIST_FORM_TITLE' => 'Lista de Facturas',
    'LBL_MODULE_NAME' => 'Facturas',
    'LBL_MODULE_TITLE' => 'Facturas',
    'LBL_HOMEPAGE_TITLE' => 'Mis Facturas',
    'LNK_NEW_RECORD' => 'Crear Factura',
    'LNK_LIST' => 'Facturas',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Facturas',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_NEW_FORM_TITLE' => 'Nueva Factura',
    'LBL_TERMS_C' => 'Condiciones',
    'LBL_APPROVAL_ISSUE' => 'Incidencias de Aprobación',
    'LBL_APPROVAL_STATUS' => 'Estado de Aprobación',
    'LBL_BILLING_ACCOUNT' => 'Cuenta',
    'LBL_BILLING_CONTACT' => 'Contacto',
    'LBL_EXPIRATION' => 'Válida Hasta',
    'LBL_INVOICE_NUMBER' => 'Número de Factura',
    'LBL_OPPORTUNITY' => 'Oportunidad',
    'LBL_TEMPLATE_DDOWN_C' => 'Plantilla de Factura',
    'LBL_STAGE' => 'Etapa de Presupuesto',
    'LBL_TERM' => 'Condiciones de Pago',
    'LBL_SUBTOTAL_AMOUNT' => 'Subtotal',
    'LBL_DISCOUNT_AMOUNT' => 'Descuento',
    'LBL_TAX_AMOUNT' => 'IVA',
    'LBL_SHIPPING_AMOUNT' => 'Envío',
    'LBL_TOTAL_AMT' => 'Total',
    'VALUE' => 'Título',
    'LBL_EMAIL_ADDRESSES' => 'Direcciones de Email',
    'LBL_LINE_ITEMS' => 'Artículos',
    'LBL_GRAND_TOTAL' => 'Total global',
    'LBL_QUOTE_NUMBER' => 'Número de Presupuesto',
    'LBL_QUOTE_DATE' => 'Fecha del Presupuesto',
    'LBL_INVOICE_DATE' => 'Fecha de Facturación',
    'LBL_DUE_DATE' => 'Fecha de Vencimiento',
    'LBL_STATUS' => 'Estado',
    'LBL_INVOICE_STATUS' => 'Estado de la Factura',
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
    'LBL_ADD_PRODUCT_LINE' => 'Nueva Línea de Producto',
    'LBL_SERVICE_NAME' => 'Servicio',
    'LBL_SERVICE_LIST_PRICE' => 'Precio de lista',
    'LBL_SERVICE_PRICE' => 'Precio con descuento',
    'LBL_SERVICE_DISCOUNT' => 'Descuento',
    'LBL_ADD_SERVICE_LINE' => 'Nueva Línea de Servicio',
    'LBL_REMOVE_PRODUCT_LINE' => 'Quitar',
    'LBL_PRINT_AS_PDF' => 'Generar documento PDF',
    'LBL_EMAIL_INVOICE' => 'Enviar Factura por Email',
    'LBL_LIST_NUM' => 'Número',
    'LBL_PDF_NAME' => 'Factura',
    'LBL_EMAIL_NAME' => 'Para',
    'LBL_NO_TEMPLATE' => 'ERROR: No se han encontrado plantillas. Si no ha creado ninguna planilla de facturas, vaya a Plantillas PDF y cree una.',
    'LBL_SUBTOTAL_TAX_AMOUNT' => 'Subtotal + IVA',//pre shipping
    'LBL_EMAIL_PDF' => 'Enviar PDF por Email',
    'LBL_ADD_GROUP' => 'Nuevo Grupo',
    'LBL_DELETE_GROUP' => 'Eliminar Grupo',
    'LBL_GROUP_NAME' => 'Nombre del Grupo',
    'LBL_GROUP_TOTAL' => 'Total de Grupo',
    'LBL_SHIPPING_TAX' => '% IVA envío',
    'LBL_SHIPPING_TAX_AMT' => 'IVA envío',
    'LBL_IMPORT_LINE_ITEMS' => 'Importar Líneas de Presupuesto',
    'LBL_SUBTOTAL_AMOUNT_USDOLLAR' => 'Subtotal (moneda predeterminada)',
    'LBL_DISCOUNT_AMOUNT_USDOLLAR' => 'Descuento (moneda predeterminada)',
    'LBL_TAX_AMOUNT_USDOLLAR' => 'IVA (moneda predeterminada)',
    'LBL_SHIPPING_AMOUNT_USDOLLAR' => 'Envío (moneda predeterminada)',
    'LBL_TOTAL_AMT_USDOLLAR' => 'Total (moneda predeterminada)',
    'LBL_SHIPPING_TAX_AMT_USDOLLAR' => 'IVA de envío (moneda predeterminada)',
    'LBL_GRAND_TOTAL_USDOLLAR' => 'Total global (moneda predeterminada)',
    'LBL_INVOICE_TO' => 'Facturar a',
    'LBL_AOS_LINE_ITEM_GROUPS' => 'Grupos de Líneas de presupuesto',
    'LBL_AOS_PRODUCT_QUOTES' => 'Líneas de presupuesto',
    'LBL_AOS_QUOTES_AOS_INVOICES' => 'Presupuestos: Facturas',
);
