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
    'LBL_SECURITYGROUPS' => 'Grupos de Seguridade',
    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de Seguridade',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Data de Creación',
    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_DESCRIPTION' => 'Descrición',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nome',
    'LBL_LIST_NAME' => 'Nome',
    'LBL_EDIT_BUTTON' => 'Editar',
    'LBL_REMOVE' => 'Quitar',
    
    'LBL_CONTRACT_ACCOUNT' => 'Conta',
    'LBL_OPPORTUNITY' => 'Oportunidade',
    'LBL_LIST_FORM_TITLE' => 'Lista de Contratos',
    'LBL_MODULE_NAME' => 'Contratos',
    'LBL_MODULE_TITLE' => 'Contactos',
    'LBL_HOMEPAGE_TITLE' => 'Os Meus Contratos',
    'LNK_NEW_RECORD' => 'Crear Contrato',
    'LNK_LIST' => 'Ver Contratos',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar Contratos',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_NEW_FORM_TITLE' => 'Novo Contrato',
    'LBL_CONTRACT_NAME' => 'Nome do Contrato',
    'LBL_REFERENCE_CODE ' => 'Código',
    'LBL_START_DATE' => 'Data de Inicio',
    'LBL_END_DATE' => 'Data de Finalización',
    'LBL_TOTAL_CONTRACT_VALUE' => 'Valor do Contrato',
    'LBL_STATUS' => 'Estado',
    'LBL_CUSTOMER_SIGNED_DATE' => 'Data de Sinatura do Cliente',
    'LBL_COMPANY_SIGNED_DATE' => 'Data de Sinatura da Compañía',
    'LBL_RENEWAL_REMINDER' => 'Recordatorio de Renovación de Contrato',
    'LBL_RENEWAL_REMINDER_DATE' => 'Data de Recordatorio de Renovación',
    'LBL_CONTRACT_TYPE' => 'Tipo',
    'LBL_CONTACT' => 'Contacto',
    'LBL_ADD_GROUP' => 'Novo Grupo',
    'LBL_DELETE_GROUP' => 'Eliminar Grupo',
    'LBL_GROUP_NAME' => 'Nome do Grupo',
    'LBL_GROUP_TOTAL' => 'Total do Grupo',
    'LBL_PRODUCT_QUANITY' => 'Cantidade',
    'LBL_PRODUCT_NAME' => 'Produto',
    'LBL_PART_NUMBER' => 'Código',
    'LBL_PRODUCT_NOTE' => 'Nota',
    'LBL_PRODUCT_DESCRIPTION' => 'Descrición',
    'LBL_LIST_PRICE' => 'Prezo de lista',
    'LBL_DISCOUNT_AMT' => 'Desconto',
    'LBL_UNIT_PRICE' => 'Prezo con desconto',
    'LBL_TOTAL_PRICE' => 'Prezo total',
    'LBL_VAT' => '% IVE',
    'LBL_VAT_AMT' => 'IVE',
    'LBL_SERVICE_NAME' => 'Servizo',
    'LBL_SERVICE_LIST_PRICE' => 'Prezo de lista',
    'LBL_SERVICE_PRICE' => 'Prezo con desconto',
    'LBL_SERVICE_DISCOUNT' => 'Desconto',
    'LBL_LINE_ITEMS' => 'Artículos',
    'LBL_SUBTOTAL_AMOUNT' => 'Subtotal',
    'LBL_DISCOUNT_AMOUNT' => 'Desconto',
    'LBL_TAX_AMOUNT' => 'IVE',
    'LBL_SHIPPING_AMOUNT' => 'Envío',
    'LBL_TOTAL_AMT' => 'Total',
    'LBL_GRAND_TOTAL' => 'Total global',
    'LBL_SHIPPING_TAX' => '% IVE envío',
    'LBL_SHIPPING_TAX_AMT' => 'IVE envío',
    'LBL_ADD_PRODUCT_LINE' => 'Nova liña de produto',
    'LBL_ADD_SERVICE_LINE' => 'Nova liña de servizo',
    'LBL_PRINT_AS_PDF' => 'Xerar documento PDF',
    'LBL_EMAIL_PDF' => 'Enviar PDF por Email',
    'LBL_PDF_NAME' => 'Contrato',
    'LBL_EMAIL_NAME' => 'Contrato para',
    'LBL_NO_TEMPLATE' => 'ERRO: Non se encontraron plantillas. Se vostede non creou unha plantilla de Contrato, vaia ao módulo de Plantillas PDF e cree unha.',
    'LBL_TOTAL_CONTRACT_VALUE_USDOLLAR' => 'Valor do contrato (moeda predeterminada)',
    'LBL_SUBTOTAL_AMOUNT_USDOLLAR' => 'Subtotal (moeda predeterminada)',
    'LBL_DISCOUNT_AMOUNT_USDOLLAR' => 'Desconto (moeda predeterminada)',
    'LBL_TAX_AMOUNT_USDOLLAR' => 'IVE (moeda predeterminada)',
    'LBL_SHIPPING_AMOUNT_USDOLLAR' => 'Envío (moeda predeterminada)',
    'LBL_TOTAL_AMT_USDOLLAR' => 'Total (moeda predeterminada)',
    'LBL_SHIPPING_TAX_AMT_USDOLLAR' => 'IVE de envío (moeda predeterminada)',
    'LBL_GRAND_TOTAL_USDOLLAR' => 'Total global (moeda predeterminada)',

    'LBL_CALL_ID' => 'Identificador de chamada',
    'LBL_AOS_LINE_ITEM_GROUPS' => 'Grupos de liñas de presuposto',
    'LBL_AOS_PRODUCT_QUOTES' => 'Liñas de Presuposto',
    'LBL_AOS_QUOTES_AOS_CONTRACTS' => 'Presupostos: Contratos',
);
