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
    'ERR_DELETE_RECORD' => 'Debe especificar un número de rexistro para eliminar a conta.',
    'LBL_ACCOUNT_ID' => 'ID de Conta:',
    'LBL_CASE_ID' => 'ID Caso:',
    'LBL_CLOSE' => 'Cerrar:',
    'LBL_CONTACT_ID' => 'ID Contacto:',
    'LBL_CONTACT_NAME' => 'Contacto:',
    'LBL_DEFAULT_SUBPANEL_TITLE' => 'Notas',
    'LBL_DESCRIPTION' => 'Nota',
    'LBL_EMAIL_ADDRESS' => 'Enderezo de Correo:',
    'LBL_EMAIL_ATTACHMENT' => 'Adxunto de Correo:',
    'LBL_FILE_MIME_TYPE' => 'Tipo MIME',
    'LBL_FILE_URL' => 'URL do arquivo',
    'LBL_FILENAME' => 'Adxunto:',
    'LBL_LEAD_ID' => 'ID Cliente Potencial:',
    'LBL_LIST_CONTACT_NAME' => 'Contacto',
    'LBL_LIST_DATE_MODIFIED' => 'Última Modificación',
    'LBL_LIST_FILENAME' => 'Adxunto',
    'LBL_LIST_FORM_TITLE' => 'Lista de Notas',
    'LBL_LIST_RELATED_TO' => 'Relacionado con',
    'LBL_LIST_SUBJECT' => 'Asunto',
    'LBL_LIST_STATUS' => 'Estado',
    'LBL_LIST_CONTACT' => 'Contacto',
    'LBL_MODULE_NAME' => 'Notas',
    'LBL_MODULE_TITLE' => 'Notas: Inicio',
    'LBL_NEW_FORM_TITLE' => 'Nova Nota ou Adxunto',
    'LBL_NOTE_STATUS' => 'Nota',
    'LBL_NOTE_SUBJECT' => 'Asunto:',
    'LBL_NOTES_SUBPANEL_TITLE' => 'Adxuntos',
    'LBL_NOTE' => 'Nota:',
    'LBL_OPPORTUNITY_ID' => 'ID Oportunidade:',
    'LBL_PARENT_ID' => 'ID Pai:',
    'LBL_PARENT_TYPE' => 'Tipo de Pai',
    'LBL_PHONE' => 'Teléfono:',
    'LBL_PORTAL_FLAG' => '¿Mostrar no Portal?',
    'LBL_EMBED_FLAG' => '¿Incluir en Correo?',
    'LBL_PRODUCT_ID' => 'ID Produto:',
    'LBL_QUOTE_ID' => 'ID Presuposto:',
    'LBL_RELATED_TO' => 'Relacionado con:',
    'LBL_SEARCH_FORM_TITLE' => 'Busca de Notas',
    'LBL_STATUS' => 'Estado',
    'LBL_SUBJECT' => 'Asunto:',
    'LNK_IMPORT_NOTES' => 'Importar Notas',
    'LNK_NEW_NOTE' => 'Nova Nota ou Adxunto',
    'LNK_NOTE_LIST' => 'Ver Notas',
    'LBL_MEMBER_OF' => 'Membro de:',
    'LBL_LIST_ASSIGNED_TO_NAME' => 'Usuario Asignado',
    'LBL_REMOVING_ATTACHMENT' => 'Quitando adxunto...',
    'ERR_REMOVING_ATTACHMENT' => 'Erro ao quitar adxunto...',
    'LBL_CREATED_BY' => 'Creado Por',
    'LBL_MODIFIED_BY' => 'Modificado Por',
    'LBL_SEND_ANYWAYS' => 'Este correo non ten asunto. ¿Enviar/gardar de todas formas?',
    'LBL_NOTE_INFORMATION' => 'Visión Global', //Can be translated in all caps. This string will be used by SuiteP template menu actions
    'LBL_MY_NOTES_DASHLETNAME' => 'As Miñas Notas',
    'LBL_EDITLAYOUT' => 'Editar Deseño' /*for 508 compliance fix*/,
    //For export labels
    'LBL_FIRST_NAME' => 'Nome',
    'LBL_LAST_NAME' => 'Apelidos',
    'LBL_DATE_ENTERED' => 'Data de Creación',
    'LBL_DATE_MODIFIED' => 'Data de Modificación',
    'LBL_DELETED' => 'Eliminada',
    'LBL_FILE_CONTENTS' => 'Contido do arquivo',

    'LBL_ASCENDING' => 'Ascendente',
    'LBL_DESCENDING' => 'Descendente',
);
