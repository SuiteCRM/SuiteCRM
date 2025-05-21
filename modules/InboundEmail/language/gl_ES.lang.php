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


    'LBL_RE' => 'RE:',

    'ERR_BAD_LOGIN_PASSWORD' => 'Usuario ou contrasinal incorrecto',
    'ERR_INI_ZLIB' => 'Non puido deshabilitarse a compresión Zlib temporalmente.  Pode que "Comprobar Configuración" falle.',
    'ERR_NO_IMAP' => 'Non se encontraron as librerías de IMAP.  Por favor, resolva isto antes de continuar coa configuración de correo entrante',
    'ERR_NO_OPTS_SAVED' => 'Non se gardaron valores óptimos coa súa conta de correo entrante.  Por favor, revise a configuración',
    'ERR_TEST_MAILBOX' => 'Por favor, comprobe a súa configuración e inténteo de novo.',
    'ERR_INVALID_PORT' => 'Puerto Inválido',

    'LBL_ASSIGN_TO_USER' => 'Asignar a Usuario',
    'LBL_AUTOREPLY' => 'Plantilla de Resposta Automática',
    'LBL_AUTOREPLY_HELP' => 'Seleccione unha resposta automática para notificar aos remitentes de correo que a súa resposta foi recibida.',
    'LBL_BASIC' => 'Información da Conta de Correo',
    'LBL_CASE_MACRO' => 'Macro de Casos',
    'LBL_CASE_MACRO_DESC' => 'Establece a macro que será analizada e utilizada para vincular o email importado a un Caso.',
    'LBL_CASE_MACRO_DESC2' => 'Estableza ésto a calquera valor que desexe, pero preserve o <b>"%1"</b>.',
    'LBL_CLOSE_POPUP' => 'Cerrar Ventá',
    'LBL_CREATE_TEMPLATE' => 'Crear',
    'LBL_DELETE_SEEN' => 'Eliminar Emails Lidos Tras Importación',
    'LBL_EDIT_TEMPLATE' => 'Editar',
    'LBL_EMAIL_OPTIONS' => 'Opcións de Xestión de Correo',
    'LBL_EMAIL_BOUNCE_OPTIONS' => 'Opcións de Xestión de Rebotes',
    'LBL_FILTER_DOMAIN_DESC' => 'Especificar un dominio ao cal non se enviarán respostas automáticas.',
    'LBL_ASSIGN_TO_GROUP_FOLDER_DESC' => 'Seleccione esta opción para se creen automaticamente rexistros de correo en SuiteCRM para todos os correos entrantes.',
    'LBL_FILTER_DOMAIN' => 'Non enviar Respostas Automáticas a este Dominio',
    'LBL_FIND_SSL_WARN' => '<br>A comprobación de SSL pode durar bastante tempo.  Por favor, teña paciencia.<br>',
    'LBL_FROM_ADDR' => 'Enderezo do Remitente',
    'LBL_FROM_ADDR_DESC' => 'O enderezo de correo electrónico posto aquí non apareza no campo "De" enderezo do correo electrónico do remitente debido ás restricións impostas polo proveedor de servizos de correo electrónico. Nestas circunstancias, o enderezo de correo electrónico que se define é a do servidor de correo saínte que esta configurado.',
    'LBL_FROM_NAME' => 'Nome do remitente',
    'LBL_GROUP_QUEUE' => 'Asignar a Grupo',
    'LBL_HOME' => 'Inicio',
    'LBL_LIST_MAILBOX_TYPE' => 'Utilización da Conta de Correo',
    'LBL_LIST_NAME' => 'Nome:',
    'LBL_LIST_GLOBAL_PERSONAL' => 'Tipo',
    'LBL_LIST_SERVER_URL' => 'Servidor de Correo',
    'LBL_SERVER_ADDRESS' => 'Dirección del servidor',
    'LBL_LIST_STATUS' => 'Estado',
    'LBL_LOGIN' => 'Nome de Usuario',
    'LBL_MAILBOX_DEFAULT' => 'BANDEXA DE ENTRADA',
    'LBL_MAILBOX_SSL' => 'Usar SSL',
    'LBL_USERNAME' => 'Nombre de usuario',
    'LBL_MAILBOX_TYPE' => 'Accións Posibles',
    'LBL_DISTRIBUTION_METHOD' => 'Método de Distribución',
    'LBL_CREATE_CASE_REPLY_TEMPLATE' => 'Nova Plantilla de Resposta Automática para Caso',
    'LBL_CREATE_CASE_REPLY_TEMPLATE_HELP' => 'Seleccione unha resposta automática para notificar aos remitentes de correo que se creou un novo caso. O correo contén o número de caso na liña de Asunto acorde coa configuración da Macro de Caso.  Esta resposta só se enviará cando se reciba o primeiro correo dun remitente.',
    'LBL_MAILBOX' => 'Carpetas Monitorizadas',
    'LBL_TRASH_FOLDER' => 'Papeleira',
    'LBL_SENT_FOLDER' => 'Elementos Enviados',
    'LBL_SELECT' => 'Seleccionar',
    'LBL_MARK_READ_NO' => 'Email marcado como borrado tras importación',
    'LBL_MARK_READ_YES' => 'Email deixado no servidor tras importación',
    'LBL_MARK_READ' => 'Deixar mensaxes no servidor',
    'LBL_MAX_AUTO_REPLIES' => 'Número de respostas automáticas',
    'LBL_MAX_AUTO_REPLIES_DESC' => 'Establece o máximo número de respostas automáticas a enviar a un único enderezo de correo durante un período de 24 horas.',
    'LBL_PERSONAL_MODULE_NAME' => 'Conta de Correo Persoal',
    'LBL_CREATE_CASE' => 'Crear Caso desde Correo',
    'LBL_CREATE_CASE_HELP' => 'Seleccione esta opción para crear automaticamente rexistros de casos en SuiteCRM a partir de correos entrantes.',
    'LBL_MODULE_NAME' => 'Conta de Correo de Grupo',
    'LBL_BOUNCE_MODULE_NAME' => 'Bandexa de Xestión de Correo Rebotado',
    'LBL_MODULE_TITLE' => 'Correo Entrante',
    'LBL_NAME' => 'Nome',
    'LBL_NONE' => 'Ningún',
    'LBL_ONLY_SINCE_NO' => 'Non. Comprobar contra todos os correos no servidor de correo.',
    'LBL_ONLY_SINCE_YES' => 'Si.',
    'LBL_PASSWORD' => 'Contrasinal',
    'LBL_EMAIL_PASSWORD' => 'Contraseña',
    'LBL_POP3_SUCCESS' => 'A súa proba de conexión de POP3 tivo éxito.',
    'LBL_POPUP_TITLE' => 'Comprobar Configuración',
    'LBL_SELECT_SUBSCRIBED_FOLDERS' => 'Seleccionar Carpetas Suscritas',
    'LBL_SELECT_TRASH_FOLDERS' => 'Seleccionar Papeleira',
    'LBL_SELECT_SENT_FOLDERS' => 'Seleccionar Carpeta de Elementos Enviados',
    'LBL_DELETED_FOLDERS_LIST' => 'As seguintes carpetas %s ou non existen ou foron eliminadas do servidor',
    'LBL_PORT' => 'Porto do Servidor de Correo',
    'LBL_REPLY_TO_NAME' => 'Nome de "Responder A"',
    'LBL_REPLY_TO_ADDR' => 'Enderezo de "Responder A"',
    'LBL_SAME_AS_ABOVE' => 'Usando o mesmo Nome/Enderezo',
    'LBL_SERVER_OPTIONS' => 'Configuración Avanzada',
    'LBL_SERVER_TYPE' => 'Protocolo do Servidor de Correo',
    'LBL_SERVER_PORT' => 'Puerto del servidor de correo',
    'LBL_SERVER_URL' => 'Enderezo do Servidor de Correo',
    'LBL_SSL_DESC' => 'Se o seu servidor de correo soporta conexións seguras de sockets (SSL), habilitar esta opción forzará conexións SSL ao importar o correo.',
    'LBL_ASSIGN_TO_TEAM_DESC' => 'O equipo seleccionado ten acceso á conta de correo.',
    'LBL_SSL' => 'Usar SSL',
    'LBL_STATUS' => 'Estado',
    'LBL_EMAIL_BODY_FILTERING' => 'Tipo de filtro de cuerpo de correo electrónico',
    'LBL_SYSTEM_DEFAULT' => 'Por Defecto no Sistema',
    'LBL_TEST_BUTTON_TITLE' => 'Probar',
    'LBL_TEST_SETTINGS' => 'Probar Configuración',
    'LBL_TEST_CONNECTION_SETTINGS' => 'Probar la configuración de conexión',    
    'LBL_TEST_SUCCESSFUL' => 'Conexión completada con éxito.',
    'LBL_TEST_WAIT_MESSAGE' => 'Un momento, por favor...',
    'LBL_WARN_IMAP_TITLE' => 'Correo Entrante Deshabilitado',
    'LBL_WARN_IMAP' => 'Avisos:',
    'LBL_WARN_NO_IMAP' => 'O Correo Entrante <b>non pode</b> funcionar sen as librerías de C do cliente de IMAP habilitadas/compiladas no módulo de PHP.  Por favor, contacte co seu administrador para resolver este problema.',

    'LNK_LIST_CREATE_NEW_PERSONAL' => 'Nueva Cuenta de Correo Personal',
    'LNK_LIST_CREATE_NEW_GROUP' => 'Nova Conta de Correo de Grupo',
    'LNK_LIST_CREATE_NEW_CASES_TYPE' => 'Nueva Cuenta de Correo para el manejo de casos',
    'LNK_LIST_CREATE_NEW_BOUNCE' => 'Nova Conta de Xestión de Rebotes',
    'LNK_LIST_MAILBOXES' => 'Todas as Contas de Correo',
    'LNK_LIST_OUTBOUND_EMAILS' => 'Cuentas de correo electrónico salientes',    
    'LNK_LIST_SCHEDULER' => 'Planificadores',
    'LNK_SEED_QUEUES' => 'Crear Cabeza de Serie para Colas de Equipos',
    'LBL_GROUPFOLDER_ID' => 'Id de Carpeta de Grupo',

    'LBL_ALLOW_OUTBOUND_GROUP_USAGE' => 'Permitir que os usuarios envíen correo usando o Nome e a Enderezo do campo "De" como enderezo de resposta',
    'LBL_ALLOW_OUTBOUND_GROUP_USAGE_DESC' => 'Cando se selecciona esta opción, o Nome e Enderezo do remitente asociados á conta de correo deste grupo aparecerán como unha opción para o campo "De" ao escribir un correo para os usuarios que teñan acceso á conta de correo do grupo.',
    'LBL_STATUS_ACTIVE' => 'Activo',
    'LBL_STATUS_INACTIVE' => 'Inactivo',
    'LBL_IS_PERSONAL' => 'Persoal',
    'LBL_IS_GROUP' => 'grupo',
    'LBL_ENABLE_AUTO_IMPORT' => 'Importar Correos Automaticamente',
    'LBL_WARNING_CHANGING_AUTO_IMPORT' => 'Aviso: Está modificando a súa configuración de importación automática o cal pode provocar pérdida de datos.',
    'LBL_WARNING_CHANGING_AUTO_IMPORT_WITH_CREATE_CASE' => 'Aviso: a importación automática debe estar habilitada para a creación automática de casos.',
    'LBL_LIST_TITLE_MY_DRAFTS' => 'Borradores',
    'LBL_LIST_TITLE_MY_INBOX' => 'Bandexa de entrada',
    'LBL_LIST_TITLE_MY_SENT' => 'Email Enviado',
    'LBL_LIST_TITLE_MY_ARCHIVES' => 'Emails Arquivados',
    'LNK_MY_DRAFTS' => 'Borradores',
    'LNK_MY_INBOX' => 'Email',
    'LNK_VIEW_MY_INBOX' => 'Ver Email',
    'LNK_QUICK_REPLY' => 'Responder',
    'LNK_SENT_EMAIL_LIST' => 'Emails Enviados',
    'LBL_EDIT_LAYOUT' => 'Editar deseño' /*for 508 compliance fix*/,
    'LBL_TYPE_DIFFERENT' => 'El tipo de conexión OAuth externa debe ser el MISMO que el tipo de la cuenta de correo electrónico entrante',

    'LBL_MODIFIED_BY' => 'Modificado Por',
    'LBL_SERVICE' => 'Servizo',
    'LBL_STORED_OPTIONS' => 'Opcións Almacenadas',
    'LBL_GROUP_ID' => 'Grupo ID',

    'LBL_OUTBOUND_CONFIGURATION' => 'Configuración de correo saliente',
    'LBL_CONNECTION_CONFIGURATION' => 'Configuración del servidor',
    'LBL_AUTO_REPLY_CONFIGURATION' => 'Configuración de respuesta automática',
    'LBL_CASE_CONFIGURATION' => 'Configuración del caso',
    'LBL_GROUP_CONFIGURATION' => 'Configuración del grupo',

    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Grupos de seguridad',

    'LBL_OUTBOUND_EMAIL_ACCOUNT' => 'Cuenta de correo saliente',
    'LBL_OUTBOUND_EMAIL_ACCOUNT_ID' => 'ID de la cuenta de correo saliente',
    'LBL_OUTBOUND_EMAIL_ACCOUNT_NAME' => 'Cuenta de correo saliente',

    'LBL_AUTOREPLY_EMAIL_TEMPLATE' => 'Plantilla de correo electrónico de respuesta automática',
    'LBL_AUTOREPLY_EMAIL_TEMPLATE_ID' => 'ID de la plantilla de correo electrónico de respuesta automática',
    'LBL_AUTOREPLY_EMAIL_TEMPLATE_NAME' => 'Plantilla de correo electrónico de respuesta automática',

    'LBL_CASE_EMAIL_TEMPLATE' => 'Plantilla de correo electrónico de caso',
    'LBL_CASE_EMAIL_TEMPLATE_ID' => 'ID de la plantilla de correo electrónico de caso',
    'LBL_CASE_EMAIL_TEMPLATE_NAME' => 'Plantilla de correo electrónico de caso',

    'LBL_PROTOCOL' => 'Protocolo',
    'LBL_CONNECTION_STRING' => 'Cadena de conexión',
    'LBL_DISTRIB_METHOD' => 'Método de distribución',
    'LBL_DISTRIB_OPTIONS' => 'Opciones de distribución',

    'LBL_DISTRIBUTION_USER' => 'Usuario de distribución',
    'LBL_DISTRIBUTION_USER_ID' => 'ID del usuario de distribución',
    'LBL_DISTRIBUTION_USER_NAME' => 'Usuario de distribución',

    'LBL_EXTERNAL_OAUTH_CONNECTION' => 'Conexión OAuth externa',
    'LBL_EXTERNAL_OAUTH_CONNECTION_ID' => 'ID de la conexión OAuth externa',
    'LBL_EXTERNAL_OAUTH_CONNECTION_NAME' => 'Conexión OAuth externa',
    'LNK_EXTERNAL_OAUTH_CONNECTIONS' => 'Conexiones OAuth externas',

    'LBL_TYPE' => 'Tipo',
    'LBL_AUTH_TYPE' => 'Tipo de autenticación',
    'LBL_IS_DEFAULT' => 'Predeterminado',
    'LBL_SIGNATURE' => 'Firma',

    'LBL_OWNER_NAME' => 'Propietario',

    'LBL_SET_AS_DEFAULT_BUTTON' => 'Establecer como predeterminado',

    'LBL_MOVE_MESSAGES_TO_TRASH_AFTER_IMPORT' => '¿Mover mensajes a la papelera después de importarlos?',
);