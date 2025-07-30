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
    'LBL_ASSIGNED_TO_ID' => 'Id de usuario asignado',
    'LBL_ASSIGNED_TO_NAME' => 'Usuario de SuiteCRM',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Fecha de creación',
    'LBL_DATE_MODIFIED' => 'Fecha de modificación',
    'LBL_MODIFIED' => 'Modificado por',
    'LBL_MODIFIED_NAME' => 'Modificado Por Nombre',
    'LBL_CREATED' => 'Creado por',
    'LBL_DESCRIPTION' => 'Descripción',
    'LBL_DELETED' => 'Eliminado',
    'LBL_NAME' => 'Nombre del usuario de la App',
    'LBL_CREATED_USER' => 'Creado por el Usuario',
    'LBL_MODIFIED_USER' => 'Modificado por el Usuario',
    'LBL_LIST_NAME' => 'Nombre',
    'LBL_LIST_FORM_TITLE' => 'Lista de cuentas externas',
    'LBL_MODULE_NAME' => 'Cuenta externa',
    'LBL_MODULE_TITLE' => 'Cuentas externas',
    'LBL_HOMEPAGE_TITLE' => 'Mis cuentas externas',
    'LNK_NEW_RECORD' => 'Crear cuenta externa',
    'LNK_LIST' => 'Ver cuentas externas',
    'LBL_SEARCH_FORM_TITLE' => 'Buscar fuentes externas',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'Ver historial',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Actividades',
    'LBL_NEW_FORM_TITLE' => 'Nueva cuenta externa',
    'LBL_PASSWORD' => 'Contraseña App',
    'LBL_USER_NAME' => 'Nombre del usuario de la App',
    'LBL_URL' => 'URL',
    'LBL_APPLICATION' => 'Aplicación',
    'LBL_API_DATA' => 'Datos API',
    'LBL_API_CONSKEY' => 'Clave del Consumidor',
    'LBL_API_CONSSECRET' => 'Secreto del Consumidor',
    'LBL_API_OAUTHTOKEN' => 'Token OAuth',
    'LBL_AUTH_UNSUPPORTED' => "Este método de registro no es compatible con la aplicación",
    'LBL_AUTH_ERROR' => 'El intento de conectarse con la cuenta fallo.',
    'LBL_VALIDATED' => 'Conectado',
    'LBL_ACTIVE' => 'Activa',
    'LBL_OAUTH_NAME' => '%s',
    'LBL_CONNECT_BUTTON_TITLE' => 'Conecta',
    'LBL_NOTE' => 'Tenga en cuenta',
    'LBL_CONNECTED' => 'Conectado',

    'LBL_ERR_NO_AUTHINFO' => 'No hay información de autenticación para esta cuenta.',
    'LBL_ERR_NO_TOKEN' => 'No hay token válido para registrar esta cuenta.',

    'LBL_ERR_FAILED_QUICKCHECK' => 'Usted no está conectado a su cuenta {0}. Haga clic en Aceptar para acceder a su cuenta y volver a activar la conexión.',

    'LBL_CLICK_TO_EDIT' => 'Haz clic para editar',

    // Various strings used throughout the external account modules
    'LBL_REAUTHENTICATE_LABEL' => 'Volver a registrarse',
    'LBL_APPLICATION_FOUND_NOTICE' => 'Una cuenta para esta aplicación ya existe. Se ha restituido la cuenta existente.',
    'LBL_OMIT_URL' => '(Omitir http:// o https://)',
    'LBL_OAUTH_SAVE_NOTICE' => 'Haga clic en <b> conectar </b> para dirigirse a una página para proporcionar información de su cuenta y para autorizar el acceso a la cuenta por SuiteCRM. Después de conectar, se le redirige de vuelta SuiteCRM.',
    'LBL_BASIC_SAVE_NOTICE' => 'Haga clic en <b>Conectar</b> para conectar la cuenta en SuiteCRM.',
    'LBL_ERR_POPUPS_DISABLED' => 'Por favor habilite las ventanas emergentes en el navegador, o agregue una excepción para el sitio web "{0}" para poder conectarse.',

    'LBL_API_OAUTHSECRET' => 'API OAuth Secreta',
);
