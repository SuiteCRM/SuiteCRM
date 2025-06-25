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
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Data de Creación',
    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_ID' => 'Modificado Por ID',
    'LBL_MODIFIED_NAME' => 'Modificado por Nome',
    'LBL_CREATED_USER' => 'Creado polo Usuario',
    'LBL_MODIFIED_USER' => 'Modificado polo Usuario',
    'LBL_CREATED' => 'Creado por',
    'LBL_DESCRIPTION' => 'Descrición',
    'LBL_CREATED_ID' => 'Creado Por ID',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nome',
    'LBL_MODULE_NAME' => 'Accións de Fluxo de Traballo',
    'LBL_MODULE_TITLE' => 'Accións de Fluxo de Traballo',
    'LBL_AOW_WORKFLOW_ID' => 'ID de Fluxo de Traballo',
    'LBL_ACTION' => 'Acción',
    'LBL_PARAMETERS' => 'Parámetros',
    'LBL_SELECT_ACTION' => 'Seleccionar Acción',
    'LBL_CREATE_EMAIL_TEMPLATE' => 'Crear',
    'LBL_RECORD_TYPE' => 'Tipo de Rexistro',
    'LBL_SENDEMAIL' => 'Enviar Email',
    'LBL_CREATERECORD' => 'Crear rexistro',
    'LBL_MODIFYRECORD' => 'Modificar rexistro',
    'LBL_ADD_FIELD' => 'Agregar Campo',
    'LBL_ADD_RELATIONSHIP' => 'Agregar Relación',
    'LBL_EDIT_EMAIL_TEMPLATE' => 'Editar',
    'LBL_EMAIL' => 'Email',
    'LBL_EMAIL_TEMPLATE' => 'Plantilla de Email',
    'LBL_SETAPPROVAL' => 'Asignar aprobación',
    'LBL_RELATE_WORKFLOW' => 'Relacionar co Módulo de Fluxo de Traballo',
    'LBL_INDIVIDUAL_EMAILS' => 'Enviar correos electrónicos individuais',
    'LBL_COMPUTEFIELD' => 'Calcular campos',
    'LBL_COMPUTEFIELD_PARAMETERS' => 'Parámetros',
    'LBL_COMPUTEFIELD_FIELD_NAME' => 'Nome do campo',
    'LBL_COMPUTEFIELD_IDENTIFIER' => 'Identificador',
    'LBL_COMPUTEFIELD_ADD_PARAMETER' => 'Engadir parámetro',
    'LBL_COMPUTEFIELD_RELATION_PARAMETERS' => 'Parámetros de relación',
    'LBL_COMPUTEFIELD_RELATION' => 'Relación',
    'LBL_COMPUTEFIELD_ADD_RELATION_PARAMETER' => 'Engadir parámetro de relación',
    'LBL_COMPUTEFIELD_FORMULA' => 'Fórmula',
    'LBL_COMPUTEFIELD_FORMULAS' => 'Fórmulas',
    'LBL_COMPUTEFIELD_ADD_FORMULA' => 'Agregar fórmula',
    'LBL_COMPUTEFIELD_VALUE_TYPE' => 'Tipo de valor',
    'LBL_COMPUTEFIELD_RAW_VALUE' => 'Valor orixinal',
    'LBL_COMPUTEFIELD_FORMATTED_VALUE' => 'Valor con formato',
    'LBL_COPY_EMAIL_ADDRESSES_WORKFLOW' => 'Copiar mensaxes de correo electrónico do módulo de fluxo de traballo',
    // STIC-Custom 20240307 EPS - Improve send mail action
    'LBL_FROM_EMAIL' => 'Remitente (dirección)',
    'LBL_FROM_NAME' => 'Remitente (nombre)',
    'LBL_REPLY_TO_EMAIL' => 'Responder a (dirección)',
    'LBL_REPLY_TO_NAME' => 'Responder a (nombre)',
    'LBL_OUTPUT_SMTP' => 'Correo saínte',
    'LBL_SHOW_ADVANCED'=> 'Amosar a configuración avanzada',
    'LBL_ADVANCED_TOOLTIP_HEADER' => 'Opcións avazadas',
    'LBL_ADVANCED_TOOLTIP_BODY' => 'Estas opcións permiten indicar a conta de correo saínte e o nome e o enderezo do remitente.',
    // END STIC-Custom
);
