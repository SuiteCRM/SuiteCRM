<?php
/**
 * ç
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
    'LBL_BASIC_SEARCH' => 'Filtro rápido',
    'LBL_ADVANCED_SEARCH' => 'Filtro avanzado',
    'LBL_BASIC_TYPE' => 'Tipo Básico',
    'LBL_ADVANCED_TYPE' => 'Tipo Avanzado',
    'LBL_SYSOPTS_2' => '¿Qué tipo de base de datos será utilizada para a instancia de SuiteCRM que está a punto de instalar?',
    'LBL_SYSOPTS_DB' => 'Especifique Tipo de Base de Datos',
    'LBL_SYSOPTS_DB_TITLE' => 'Tipo de Base de Datos',
    'LBL_SYSOPTS_ERRS_TITLE' => 'Por favor corrixa os seguintes erros antes de continuar:',
    'ERR_DB_VERSION_FAILURE' => 'Non se pode verificar a versión da base de datos.',
    'DEFAULT_CHARSET' => 'UTF-8',
    'ERR_ADMIN_USER_NAME_BLANK' => 'Provea o nome de usuario para o usuario administrador de SuiteCRM. ',
    'ERR_ADMIN_PASS_BLANK' => 'Provea un contrasinal para o usuario administrador de SuiteCRM. ',

    'ERR_CHECKSYS' => 'Detectáronse erros durante a verificación de compatibilidade. Para que a súa instalación de SuiteCRM funcione correctamente, por favor realice os pasos necesarios para corrixir os inconvintes listados máis abaixo, e volva a presionar o botón volver a verificar, ou volva a intentar realizar a instalación novamente.',
    'ERR_CHECKSYS_CURL' => 'Non encontrado: o planificador de tarefas de SuiteCRM executaraase con funcionalidade limitada.',
    'ERR_CHECKSYS_IMAP' => 'Non encontrado: Correo Entrante e Campañas (Email) requiren a librería IMAP. non estarán funcionais.',
    'ERR_CHECKSYS_MEM_LIMIT_1' => ' (Cambie este valor a  ',
    'ERR_CHECKSYS_MEM_LIMIT_2' => 'M ou máis no seu arquivo php.ini)',
    'ERR_CHECKSYS_NOT_WRITABLE' => 'Advertencia: non se pode escribir',
    'ERR_CHECKSYS_PHP_INVALID_VER' => 'A súa versión de PHP non é soportada por SuiteCRM. Deberá instalar unha versión que sexa compatible coa aplicación SuiteCRM. Por favor consulte a Matriz de Compatibilidade nas Notas da Versión para coñecer as versións de PHP soportadas. A súa versión é ',
    'ERR_CHECKSYS_IIS_INVALID_VER' => 'A súa versión de IIS non é soportada por SuiteCRM. Deberá instalar unha versión que sexa compatible coa aplicación SuiteCRM. Por favor consulte a Matriz de Compatibilidade nas Notas da Versión para coñecer as versións de IIS soportadas. A súa versión é ',
    'ERR_CHECKSYS_FASTCGI' => 'Detectamos que non está utilizando un FastCGI handler mapping para PHP. Deberá instalar/configurar unha versión que sexa compatible con SuiteCRM. Por favor consulte a Matriz de Compatibilidade nas Notas da Versión para coñecer as versións soportadas. Vexa <a href="http://www.iis.net/php/" target="_blank">http://www.iis.net/php/</a> para máis detalles ',
    'ERR_CHECKSYS_FASTCGI_LOGGING' => 'Para unha mellor experiencia use IIS/FastCGI sapi, asigne fastcgi.logging en 0 no seu arquivo php.ini.',
    'LBL_DB_UNAVAILABLE' => 'Base de datos non dispoñible',
    'LBL_CHECKSYS_DB_SUPPORT_NOT_AVAILABLE' => 'Non se encontrou Soporte de Base de Datos. Por favor asegúrese de ter os drivers necesarios para algunha das seguintes Base de Datos soportadas: MySQL ou MS SqlServer. Pode que sexa necesario descomentar a extensión correspondente no arquivo php.ini, ou recompilar co binario correspondente, dependendo da súa versión de PHP. Por favor consulte o manual de PHP para máis información sobre como habilitar o Soporte de Base de Datos.',
    'LBL_CHECKSYS_XML_NOT_AVAILABLE' => 'Non se encontraron as funcións das librerías de XML Parsing necesarias para executar SuiteCRM. Pode que sexa necesario descomentar a extensión correspondente no arquivo php.ini, ou recompilar co binario correspondente, dependendo da súa versión de PHP. Por favor consulte o manual de PHP para máis información.',
    'ERR_CHECKSYS_MBSTRING' => 'Non se encontraron as funcións asociadas coa extensión Multibyte Strings (mbstring) necesaria para executar SuiteCRM. <br/><br/>Xeralmente o módulo mbstring non está habilitado por defecto en PHP e debe ser activado con --enable-mbstring ao momento de compilar PHP. Por favor consulte o manual de PHP para máis información sobre como habilitar mbstring.',
    'ERR_CHECKSYS_CONFIG_NOT_WRITABLE' => 'O arquivo de configuración existe pero non se pode escribir. Por favor realice os pasos necesarios para dar permisos de escritura. <br/>Dependendo do seu Sistema Operativo pode chegar a neccesitar cambiar os permisos executando chmod 766, ou con click dereito sobre o arquivo para acceder ao menú propiedades e desactivar a opción "só lectura".',
    'ERR_CHECKSYS_CONFIG_OVERRIDE_NOT_WRITABLE' => 'O arquivo config override existe pero non se pode escribir. Por favor realice os pasos necesarios para dar permisos de escritura. <br/>Dependendo do seu Sistema Operativo pode chegar a neccesitar cambiar os permisos executando chmod 766, ou con click dereito sobre o arquivo para acceder ao menú propiedades e desactivar a opción "só lectura".',
    'ERR_CHECKSYS_CUSTOM_NOT_WRITABLE' => 'O directorio Custom existe pero non se pode escribir. Por favor realice os pasos necesarios para dar permisos de escritura. <br/>Dependendo do seu Sistema Operativo pode chegar a neccesitar cambiar os permisos executando chmod 766, ou con click dereito sobre o arquivo para acceder ao menú propiedades e desactivar a opción "só lectura".',
    'ERR_CHECKSYS_FILES_NOT_WRITABLE' => "Os arquivos ou directorios listados máis abaixo non se poden escribir. Dependendo do seu Sistema Operativo, corrixir isto pode requirir que cambie os permisos dos arquivos ou directorio pai (chmod 755), ou click dereito na carpeta pai, desactivar a opción \"só lectura\" e aplicala a todas as subcarpetas. ",
    'LBL_CHECKSYS_OVERRIDE_CONFIG' => 'Sobreescribir a configuración',
    'ERR_CHECKSYS_SAFE_MODE' => 'Safe Mode está activado (pode ser conveniente deshabilitarlo en php.ini)',
    'ERR_CHECKSYS_ZLIB' => 'ZLib non se encontrou: SuiteCRM alcanza grandes beneficios de performance coa compresión zlib.',
    'ERR_CHECKSYS_ZIP' => 'ZIP non se encontrou: SuiteCRM necesita soporte ZIP para procesar arquivos comprimidos.',
    'ERR_CHECKSYS_PCRE' => 'PCRE non se encontrou: SuiteCRM necesita a librería PCRE para procesar expresións regulares no estilo Perl.',
    'ERR_CHECKSYS_PCRE_VER' => 'PCRE versión de librería: SuiteCRM necesita a versión 7.0 ou superior da librería PCRE para procesar expresións regulares no estilo Perl.',
    'ERR_DB_ADMIN' => 'O nome de usuario ou contrasinal do administrador de base de datos é inválido, e non se pode establecer a conexión coa base de datos. Por favor ingrese un nome de usuario e contrasinal correctos. (Erro: ',
    'ERR_DB_ADMIN_MSSQL' => 'O nome de usuario ou contrasinal do administrador de base de datos é inválido, e non se pode establecer a conexión coa base de datos. Por favor ingrese un nome de usuario e contrasinal correctos.',
    'ERR_DB_EXISTS_NOT' => 'A base de datos especificada non existe.',
    'ERR_DB_EXISTS_WITH_CONFIG' => 'A base de datos xa existe con información de configuración. Para executar unha instalación coa base de datos seleccionada volva a executar a instalación e seleccione "Eliminar as táboas existentes e volver a crealas?". Para actualizar utilice o Asistente de Actualizacións na Consola de Administración. Por favor lea a documentación de actualización <a href="https://docs.suitecrm.com/admin/installation-guide/upgrading/" target="_new">aquí</a>.',
    'ERR_DB_EXISTS' => 'Xa existe unha Base de Datos co nome provisto -- non se pode crear outra co mesmo nome.',
    'ERR_DB_EXISTS_PROCEED' => 'Xa existe unha Base de Datos co nome provisto.  Vostede pode<br>1.  presionar o botón Atrás e seleccionar un novo nome de base de datos <br>2.  facer click en Seguinte e continuar, aínda que todas as táboas existentes nesta base de datos serán descartadas.  <strong>Isto significa que as súas táboas e información serán eliminadas </strong>',
    'ERR_DB_HOSTNAME' => 'O nome de Host non pode ser baleiro.',
    'ERR_DB_INVALID' => 'Tipo de base de datos seleccionado non válido.',
    'ERR_DB_LOGIN_FAILURE' => 'O host, nome de usuario e/ou contrasinal provistos non é válido, e non se pode establecer unha conexión á base de datos. Por favor ingrese un host, nome de usuario e contrasinal válidos.',
    'ERR_DB_LOGIN_FAILURE_MYSQL' => 'O host, nome de usuario e/ou contrasinal provistos non é válido, e non se pode establecer unha conexión á base de datos. Por favor ingrese un host, nome de usuario e contrasinal válidos.',
    'ERR_DB_LOGIN_FAILURE_MSSQL' => 'O host, nome de usuario e/ou contrasinal provistos non é válido, e non se pode establecer unha conexión á base de datos. Por favor ingrese un host, nome de usuario e contrasinal válidos.',
    'ERR_DB_MYSQL_VERSION' => 'A súa versión de MySQL (%s) non é soportada por SuiteCRM.  Deberá instalar unha versión que sexa compatible con SuiteCRM. Por favor consulte a Matriz de Compatibilidade nas Notas de Versión para coñecer as versións soportadas.',
    'ERR_DB_NAME' => 'O nome de base de datos non pode estar baleiro.',
    'ERR_DB_MYSQL_DB_NAME_INVALID' => "O nome de base de datos non pode conter '\\', '/', ou '.'",
    'ERR_DB_MSSQL_DB_NAME_INVALID' => "O nome de base de datos non pode comezar cun número, '#', ou '@' e non pode conter espazos, '\"', \"'\", '*', '/', '\\', '?', ':', '<', '>', '&', '!', ou '-'",
    'ERR_DB_OCI8_DB_NAME_INVALID' => "O nome de base de datos só pode estar formado por caracteres alfanuméricos e os símbolos '#', '_' ou '$'",
    'ERR_DB_PASSWORD' => 'Os contrasinais provistas para o administrador de base de datos non coinciden.  Por favor volva a ingresar os mesmos contrasinais nos campos.',
    'ERR_DB_PRIV_USER' => 'Provea un nome de usuario administrador de base de datos. O usuario é requirido para a conexión inicial á base de datos.',
    'ERR_DB_USER_EXISTS' => 'O nome de usuario para a base de datos xa existe -- non se pode crear outro co mesmo nome. Por favor ingrese un novo nome de usuario.',
    'ERR_DB_USER' => 'Ingrese un nome de usuario para o administrador de base de datos.',
    'ERR_DBCONF_VALIDATION' => 'Por favor corrixa os seguintes erros antes de continuar:',
    'ERR_DBCONF_PASSWORD_MISMATCH' => 'Os contrasinais provistas para o administrador de base de datos non coinciden.  Por favor volva a ingresar os mesmos contrasinais nos campos.',
    'ERR_ERROR_GENERAL' => 'Encontráronse os seguintes erros:',
    'ERR_LANG_CANNOT_DELETE_FILE' => 'Non se pode eliminar o arquivo: ',
    'ERR_LANG_MISSING_FILE' => 'Non se encontra o arquivo: ',
    'ERR_LANG_NO_LANG_FILE' => 'Non se encontrou arquivo de Tradución en include/language dentro de: ',
    'ERR_LANG_UPLOAD_1' => 'Houbo un problema co seu upload.  Por favor volva a intentalo.',
    'ERR_LANG_UPLOAD_2' => 'Os paquetes de idioma deben ser arquivos ZIP.',
    'ERR_LANG_UPLOAD_3' => 'PHP non puido mover o arquivo temporal á carpeta upgrade.',
    'ERR_LOG_DIRECTORY_NOT_EXISTS' => 'O directorio de Log provisto non é un directorio válido.',
    'ERR_LOG_DIRECTORY_NOT_WRITABLE' => 'O directorio de Log provisto non ten permisos de escritura.',
    'ERR_NO_DIRECT_SCRIPT' => 'Non se pode procesar o script de forma directa.',
    'ERR_NO_SINGLE_QUOTE' => 'Non se poden usar comillas simples para ',
    'ERR_PASSWORD_MISMATCH' => 'Os contrasinais provistos para o usuario admin de SuiteCRM non coinciden.  Por favor volva a ingresar os mesmos contrasinais nos campos.',
    'ERR_PERFORM_CONFIG_PHP_1' => 'Non se pode escribir o arquivo <span class=stop>config.php</span>.',
    'ERR_PERFORM_CONFIG_PHP_2' => 'Pode continuar con esta instalación creando manualmente o arquivo config.php e pegando a seguinte información de configuración dentro do arquivo.  De todos modos, vostede <strong>debería </strong>crear o arquivo config.php antes de continuar co seguinte paso.',
    'ERR_PERFORM_CONFIG_PHP_3' => 'Acordouse de crear o arquivo config.php?',
    'ERR_PERFORM_CONFIG_PHP_4' => 'Advertencia: non se puido escribir o arquivo config.php.  Asegúrese de que existe.',
    'ERR_PERFORM_HTACCESS_1' => 'Non é pode escribir o arquivo ',
    'ERR_PERFORM_HTACCESS_2' => ' .',
    'ERR_PERFORM_HTACCESS_3' => 'Se desexa protexer o seu arquivo de log de ser accedido via navegador, cree un arquivo .htaccess na súa carpeta log coa liña:',
    'ERR_PERFORM_NO_TCPIP' => '<b>Non puidemos detectar unha conexión a Internet.</b> Cando teña unha conexión, por favor visite <a href="http://www.suitecrm.com/">http://www.suitecrm.com/</a> para rexgistrarse con SuiteCRM. Se nos conta un pouco como a súa compañía planea utilizar SuiteCRM poderemos asegurarnos de entregarlle a aplicación correcta para as necesidades do seu negocio.',
    'ERR_SESSION_DIRECTORY_NOT_EXISTS' => 'O directorio de Sesión provisto non é un directorio válido.',
    'ERR_SESSION_DIRECTORY' => 'O directorio de Sesión provisto non ten permisos de escritura.',
    'ERR_SESSION_PATH' => 'Requírese unha ruta de Sesión, se é que desexa especificar unha.',
    'ERR_SI_NO_CONFIG' => 'Non incluiu o arquivo config_si.php no directorio raíz, ou non definiu a variable $sugar_config_si en config.php',
    'ERR_SITE_GUID' => 'Requírese un ID de Aplicación se é que desexa especificar un.',
    'ERROR_SPRITE_SUPPORT' => "Non puidemos encontrar a librería GD, como resultado non poderá utilizar a funcionalidade CSS Sprite.",
    'ERR_UPLOAD_MAX_FILESIZE' => 'Advertencia: Debe cambiar a súa configuración de PHP para permitir a subida de arquivos de polo menos 6MB.',
    'LBL_UPLOAD_MAX_FILESIZE_TITLE' => 'Tamaño de Subida de Arquivos',
    'ERR_URL_BLANK' => 'Provea a URL base para a súa instancia de SuiteCRM.',
    'ERR_UW_NO_UPDATE_RECORD' => 'Non se puido encontrar o rexistro de instalación de ',
    'ERROR_MANIFEST_TYPE' => 'O arquivo Manifest debe especificar o tipo de paquete.',
    'ERROR_PACKAGE_TYPE' => 'O arquivo Manifest especifica un tipo de paquete non recoñecido',
    'ERROR_VERSION_INCOMPATIBLE' => 'O arquivo subido non é compatible con esta versión de SuiteCRM: ',

    'LBL_BACK' => 'Atrás',
    'LBL_CANCEL' => 'Cancelar',
    'LBL_ACCEPT' => 'Acepto',
    'LBL_CHECKSYS_CACHE' => 'Sub-Directories de Cache Editables',
    'LBL_DROP_DB_CONFIRM' => 'O nome de Base de Datos provisto xa existe. <br>Vostede pode<br>1.  presionar o botón Atrás e seleccionar un novo nome de base de datos <br>2.  facer click en Seguinte e continuar, aínda que todas as táboas existentes nesta base de datos serán descartadas.  <strong>Isto significa que as súas táboas e información serán eliminadas </strong>',
    'LBL_CHECKSYS_COMPONENT' => 'Compoñente',
    'LBL_CHECKSYS_CONFIG' => 'Arquivo de Configuración editable (config.php)',
    'LBL_CHECKSYS_CURL' => 'Módulo cURL',
    'LBL_CHECKSYS_CUSTOM' => 'Directorio Custom editable',
    'LBL_CHECKSYS_DATA' => 'Subdirectorios de Data editables',
    'LBL_CHECKSYS_IMAP' => 'Módulo IMAP',
    'LBL_CHECKSYS_FASTCGI' => 'FastCGI',
    'LBL_CHECKSYS_MBSTRING' => 'Módulo MB Strings',
    'LBL_CHECKSYS_MEM_OK' => 'OK (Sen Límite)',
    'LBL_CHECKSYS_MEM_UNLIMITED' => 'OK (Ilimitado)',
    'LBL_CHECKSYS_MEM' => 'Límite de Memoria PHP',
    'LBL_CHECKSYS_MODULE' => 'Subdirectorios e Arquivos de módulos editables',
    'LBL_CHECKSYS_NOT_AVAILABLE' => 'Non dispoñible',
    'LBL_CHECKSYS_OK' => 'Aceptar',
    'LBL_CHECKSYS_PHP_INI' => 'Ubicación do seu arquivo de configuración PHP (php.ini):',
    'LBL_CHECKSYS_PHP_OK' => 'OK (ver ',
    'LBL_CHECKSYS_PHPVER' => 'Versión de PHP',
    'LBL_CHECKSYS_IISVER' => 'Versión de IIS',
    'LBL_CHECKSYS_RECHECK' => 'Volver a Verificar',
    'LBL_CHECKSYS_STATUS' => 'Estado',
    'LBL_CHECKSYS_TITLE' => 'Aceptación de Verificación do Sistema',
    'LBL_CHECKSYS_XML' => 'Análise XML',
    'LBL_CHECKSYS_ZLIB' => 'Módulo de Compresión ZLIB',
    'LBL_CHECKSYS_ZIP' => 'Módulo de manejo de ZIP',
    'LBL_CHECKSYS_PCRE' => 'Librería PCRE',
    'LBL_CHECKSYS_FIX_FILES' => 'Por favor corrixa os seguintes arquivos ou directorios antes de continuar:',
    'LBL_CHECKSYS_FIX_MODULE_FILES' => 'Por favor corrixa os seguintes directorios de módulos e arquivos dentro deles antes de continuar:',
    'LBL_CHECKSYS_UPLOAD' => 'Directorio Upload Editable',
    'LBL_CLOSE' => 'Cerrar',
    'LBL_THREE' => '3',
    'LBL_CONFIRM_BE_CREATED' => 'creada',
    'LBL_CONFIRM_DB_TYPE' => 'Tipo de Base de Datos',
    'LBL_CONFIRM_NOT' => 'non',
    'LBL_CONFIRM_TITLE' => 'Confirmar configuración',
    'LBL_CONFIRM_WILL' => 'será',
    'LBL_DBCONF_DB_DROP' => 'Eliminar Táboas',
    'LBL_DBCONF_DB_NAME' => 'Nome de Base de Datos',
    'LBL_DBCONF_DB_PASSWORD' => 'Contrasinal do Usuario de Base de Datos',
    'LBL_DBCONF_DB_PASSWORD2' => 'Volva a Ingresar Contrasinal do Usuario de Base de Datos',
    'LBL_DBCONF_DB_USER' => 'Nome de Usuario de Base de Datos',
    'LBL_DBCONF_SUGAR_DB_USER' => 'Nome de Usuario de Base de Datos',
    'LBL_DBCONF_DB_ADMIN_USER' => 'Nome de Usuario do Administrador de Base de Datos',
    'LBL_DBCONF_DB_ADMIN_PASSWORD' => 'Contrasinal do Administrador de Base de Datos',
    'LBL_DBCONF_COLLATION' => 'Ordenación',
    'LBL_DBCONF_CHARSET' => 'Xogo de Caracteres',
    'LBL_DBCONF_ADV_DB_CFG_TITLE' => 'Configuración avanzada da Base de Datos',
    'LBL_DBCONF_DEMO_DATA' => 'Encher a Base de Datos con Información de Proba?',
    'LBL_DBCONF_DEMO_DATA_TITLE' => 'Seleccionar Información de Proba',
    'LBL_DBCONF_HOST_NAME' => 'Nome do Host',
    'LBL_DBCONF_HOST_INSTANCE' => 'Instancia do Host',
    'LBL_DBCONFIG_SECURITY' => 'Por razóns de seguridade, pódese especificar un usuario exclusivo para conectar á base de datos de SuiteCRM. Este usuario debe ter permisos para escribir, modificar e recuperar información da base de datos de SuiteCRM que será creada para esta instancia. Este usuario pode ser o administrador de base de datos que se especificou máis arriba, vostede pode proveer un novo ou un existente.',
    'LBL_DBCONFIG_PROVIDE_DD' => 'Proveer usuario existente',
    'LBL_DBCONFIG_CREATE_DD' => 'Definir un usuario que se creará',
    'LBL_DBCONFIG_SAME_DD' => 'O mesmo que o Administrador',
    'LBL_DBCONF_TITLE' => 'Configuración da Base de Datos',
    'LBL_DBCONF_TITLE_NAME' => 'Provea un Nome de Base de Datos',
    'LBL_DBCONF_TITLE_USER_INFO' => 'Provea a información do usuario da base de datos',
    'LBL_DBCONF_TITLE_PSWD_INFO_LABEL' => 'Contrasinal',
    'LBL_DISABLED_DESCRIPTION_2' => 'Despois de realizado este cambio, poderá facer click non botón "Comezar" de abaixo para comezar a instalación. <i>Unha vez finalizada a instalación, pode cambiar o valor de \'installer_locked\' a \'true\'.</i>',
    'LBL_DISABLED_DESCRIPTION' => 'A instalación xa se executou unha vez. Como medida de seguridade deshabilitouse unha segunda execución. Se está absolutamente seguro de que quere volver a executala, por favor vaia ao seu arquivo config.php e encontre (ou agregue) unha variable chamada \'installer_locked\' e colóquela en \'false\'. A liña debería quedar así:',
    'LBL_DISABLED_HELP_1' => 'Para axuda coa instalación, por favor visite ',
    'LBL_DISABLED_HELP_LNK' => 'https://community.suitecrm.com',
    'LBL_DISABLED_HELP_2' => 'foros de soporte',
    'LBL_DISABLED_TITLE_2' => 'A instalación de SuiteCRM foi deshabilitada',
    'LBL_HELP' => 'Axuda',
    'LBL_INSTALL' => 'Instalar',
    'LBL_INSTALL_TYPE_TITLE' => 'Opcións de Instalación',
    'LBL_INSTALL_TYPE_SUBTITLE' => 'Seleccione o Tipo de Instalación',
    'LBL_INSTALL_TYPE_TYPICAL' => ' <b>Instalación Típica</b>',
    'LBL_INSTALL_TYPE_CUSTOM' => ' <b>Instalación Personalizada</b>',
    'LBL_INSTALL_TYPE_MSG2' => 'Require información mínima para a instalación. Recomendada para usuarios novos.',
    'LBL_INSTALL_TYPE_MSG3' => 'Provee opcións adicionais a establecer durante a instalación. A maior parte destas opcións tamén están dispoñibles despois da instalación na pantalla de administración. Recomendada para usuarios avanzados.',
    'LBL_LANG_1' => 'Para usar outro idioma diferente ao idioma por defecto (US-English), pode subir e instalar un paquete de idioma neste momento. Tamén poderá subir e instalar paquetes de idioma desde adentro da aplicación. Se quere omitir este paso, faga click en Seguinte.',
    'LBL_LANG_BUTTON_COMMIT' => 'Instalar',
    'LBL_LANG_BUTTON_REMOVE' => 'Quitar',
    'LBL_LANG_BUTTON_UNINSTALL' => 'Desinstalar',
    'LBL_LANG_BUTTON_UPLOAD' => 'Subir',
    'LBL_LANG_NO_PACKS' => 'ningún',
    'LBL_LANG_PACK_INSTALLED' => 'Instaláronse os seguintes paquetes de idioma: ',
    'LBL_LANG_PACK_READY' => 'Os seguintes paquetes de idioma están listos para ser instalados: ',
    'LBL_LANG_SUCCESS' => 'Subiuse correctamente o paquete de idioma.',
    'LBL_LANG_TITLE' => 'Paquete de Idioma',
    'LBL_LAUNCHING_SILENT_INSTALL' => 'Instalando SuiteCRM.  Este proceso pode tomar uns minutos.',
    'LBL_LANG_UPLOAD' => 'Subir un Paquete de Idioma',
    'LBL_LICENSE_ACCEPTANCE' => 'Aceptación de Licenza',
    'LBL_LICENSE_CHECKING' => 'Verificando a compatibilidade do sistema.',
    'LBL_LICENSE_CHKENV_HEADER' => 'Verificando o entorno',
    'LBL_LICENSE_CHKDB_HEADER' => 'Verificando as credenciais de BD.',
    'LBL_LICENSE_CHECK_PASSED' => 'O sistema cumpliu os requisitos.',
    'LBL_CREATE_CACHE' => 'Preparando para instalar...',
    'LBL_CREATE_DEFAULT_ENC_KEY' => 'Creando clave de cifrado predeterminada...',
    'LBL_LICENSE_REDIRECT' => 'Redireccionando en ',
    'LBL_LICENSE_I_ACCEPT' => 'Acepto',
    'LBL_LICENSE_PRINTABLE' => ' Versión Imprimible ',
    'LBL_PRINT_SUMM' => 'Imprimir Resumo',
    'LBL_LICENSE_TITLE_2' => 'Licenza SuiteCRM',

    'LBL_LOCALE_NAME_FIRST' => 'Juan',
    'LBL_LOCALE_NAME_LAST' => 'Pérez',
    'LBL_LOCALE_NAME_SALUTATION' => 'Dr.',

    'LBL_ML_ACTION' => 'Acción',
    'LBL_ML_DESCRIPTION' => 'Descrición',
    'LBL_ML_INSTALLED' => 'Data de Instalación',
    'LBL_ML_NAME' => 'Nome',
    'LBL_ML_PUBLISHED' => 'Data de Publicación',
    'LBL_ML_TYPE' => 'Tipo',
    'LBL_ML_UNINSTALLABLE' => 'Desinstalable',
    'LBL_ML_VERSION' => 'Versión',
    'LBL_MSSQL' => 'SQL Server',
    'LBL_MSSQL2' => 'SQL Server (FreeTDS)',
    'LBL_MSSQL_SQLSRV' => 'SQL Server (Microsoft SQL Server Driver para PHP)',
    'LBL_MYSQL' => 'MySQL',
    'LBL_MYSQLI' => 'MySQL (extensión mysqli)',
    'LBL_NEXT' => 'Seguinte',
    'LBL_NO' => 'Non',
    'LBL_PERFORM_ADMIN_PASSWORD' => 'Establecendo password de admin',
    'LBL_PERFORM_CONFIG_PHP' => 'Creando o arquivo de configuración de SuiteCRM',
    'LBL_PERFORM_CREATE_DB_1' => '<b>Creando a base de datos</b> ',
    'LBL_PERFORM_CREATE_DB_2' => ' <b>en</b> ',
    'LBL_PERFORM_CREATE_DB_USER' => 'Creando nome de usuario e password da Base de datos...',
    'LBL_PERFORM_CREATE_DEFAULT' => 'Creando información por defecto de SuiteCRM',
    'LBL_PERFORM_DEFAULT_SCHEDULER' => 'Creando tarefas planificadas por defecto',
    'LBL_PERFORM_DEFAULT_USERS' => 'Creando usuarios por defecto',
    'LBL_PERFORM_DEMO_DATA' => 'Enchendo as táboas da base de datos con Información de Proba (isto pode tomar varios minutos)',
    'LBL_PERFORM_DONE' => 'listo<br>',
    'LBL_PERFORM_FINISH' => 'Finalizar',
    'LBL_PERFORM_OUTRO_1' => 'A configuración de SuiteCRM ',
    'LBL_PERFORM_OUTRO_2' => ' finalizou!',
    'LBL_PERFORM_OUTRO_3' => 'Tempo total: ',
    'LBL_PERFORM_OUTRO_4' => ' segundos.',
    'LBL_PERFORM_OUTRO_5' => 'Memoria utilizada (aprox): ',
    'LBL_PERFORM_OUTRO_6' => ' bytes.',
    'LBL_PERFORM_SUCCESS' => 'Éxito!',
    'LBL_PERFORM_TABLES' => 'Creando as táboas da aplicación, táboas de auditoría e metadatos de relacións',
    'LBL_PERFORM_TITLE' => 'Configurar',
    'LBL_PRINT' => 'Imprimir',
    'LBL_REG_CONF_1' => 'Por favor complete o breve formulario a continuación para recibir anuncios de produtos, novidades de capacitaciones, ofertas especiais e invitacións especiais a eventos de SuiteCRM. Non vendemos, alquilamos, compartimos nin distribuimos de ningunha forma a información recolectada aquí.',
    'LBL_REG_CONF_3' => 'Grazas por rexgistrarse. Faga click non botón Finalizar para ingresar a SuiteCRM. Necesitará ingresar por primeira vez utilizando o nome de usuario "admin" e o contrasinal que ingresou no paso 2.',
    'LBL_REG_TITLE' => 'Registración',
    'LBL_REQUIRED' => '* Campo Requirido',

    'LBL_SITECFG_ADMIN_Name' => 'Nome do Administrador de SuiteCRM',
    'LBL_SITECFG_ADMIN_PASS_2' => 'Volva a Ingresar o contrasinal do Usuario Administrador',
    'LBL_SITECFG_ADMIN_PASS' => 'Contrasinal do Usuario Administrador',
    'LBL_SITECFG_APP_ID' => 'ID da Aplicación',
    'LBL_SITECFG_CUSTOM_ID_DIRECTIONS' => 'Se se selecciona, deberá proveer un ID de Aplicación para sobreescribir o ID autoxerado. o ID asegura que as sesións dunha instancia de SuiteCRM non sexan usadas por outras instancia. Se ten un cluster de instalacións de SuiteCRM, todas deberían compartir o mesmo ID de aplicación.',
    'LBL_SITECFG_CUSTOM_ID' => 'Provea o seu propio ID de Aplicación',
    'LBL_SITECFG_CUSTOM_LOG_DIRECTIONS' => 'Se se selecciona, deberá especificar un directorio de logs para sobreescribir o directorio por defecto do log de SuiteCRM. Sen importar dónde estea ubicado o arquivo de log, o acceso a través de navegador deberase restrinxir utilizando unha redirección .htacces.',
    'LBL_SITECFG_CUSTOM_LOG' => 'Utilizar un Directorio Personalizado para Logs',
    'LBL_SITECFG_CUSTOM_SESSION_DIRECTIONS' => 'Se se selecciona, deberá proveer un directorio seguro para almacenar a información de sesións de SuiteCRM. Isto pódese facer para evitar que a información da sesión sexa vulnerada en servidores compartidos.',
    'LBL_SITECFG_CUSTOM_SESSION' => 'Utilizar un Directorio Personalizado para as Sesións de SuiteCRM',
    'LBL_SITECFG_FIX_ERRORS' => '<b>Por favor corrixa os seguintes erros antes de continuar:</b>',
    'LBL_SITECFG_LOG_DIR' => 'Directorio de Log',
    'LBL_SITECFG_SESSION_PATH' => 'Ruta do Directorio de Sesións<br>(debe ser editable)',
    'LBL_SITECFG_SITE_SECURITY' => 'Seleccionar Opcións de Seguridade',
    'LBL_SITECFG_SUITE_UP_DIRECTIONS' => 'Se se selecciona, o sistema periodicamente verificará por actualizacións da aplicación.',
    'LBL_SITECFG_SUITE_UP' => '¿Verificar Automaticamente novas Actualizacións?',
    'LBL_SITECFG_TITLE' => 'Configuración do Sitio',
    'LBL_SITECFG_TITLE2' => 'Identificar o Usuario Administrador',
    'LBL_SITECFG_SECURITY_TITLE' => 'Seguridade do Sitio',
    'LBL_SITECFG_URL' => 'URL da Instancia de SuiteCRM',
    'LBL_SITECFG_ANONSTATS' => '¿Enviar estadísticas anónimas de uso?',
    'LBL_SITECFG_ANONSTATS_DIRECTIONS' => 'Se se selecciona, SuiteCRM enviará estadísticas <b>anónimas</b> acerca da súa instalación a SuiteCRM Inc. cada vez que o seu sistema verifique novas versións. Esta información axudaranos a mellorar a nosa comprensión de como a aplicación é utilizada, e guiará as melloras do produto.',
    'LBL_SITECFG_URL_MSG' => 'Ingrese a URL que será utilizada para acceder á instancia de SuiteCRM despois da instalación. A URL tamén vai a ser utilizada como base para para as URL nas páxinas da aplicación. A URL pode incluir o nome do servidor web, nome do equipo ou enderezo IP.',
    'LBL_SITECFG_SYS_NAME_MSG' => 'Ingrese un nome para o seu sistema. O nome mostrarase na barra de título do navegador cando os usuarios visiten a aplicación SuiteCRM.',
    'LBL_SITECFG_PASSWORD_MSG' => 'Despois da instalación, necesitará utilizar o usuario administrador de SuiteCRM (nome por defecto = admin) para ingresar á instancia de SuiteCRM. Ingrese un contrasinal para este usuario administrador. Esta contrasinal pode ser cambiada despois de ingresar por primeira vez. Tamén pode ingresar outro nome de usuario diferente ao valor por defecto provisto.',
    'LBL_SITECFG_COLLATION_MSG' => 'Seleccione a configuración de collation (ordenamento dos datos) para o seu sistema. Esta configuración creará as táboas co idioma específico que vostede usa. En caso de que o seu idioma non requira unha configuración especial, utilice o valor por defecto.',
    'LBL_SPRITE_SUPPORT' => 'Soporte de Sprite',
    'LBL_SYSTEM_CREDS' => 'Credenciais do Sistema',
    'LBL_SYSTEM_ENV' => 'Entorno do Sistema',
    'LBL_SHOW_PASS' => 'Mostrar Contrasinais',
    'LBL_HIDE_PASS' => 'Ocultar Contrasinais',
    'LBL_HIDDEN' => '<i>(oculto)</i>',
    'LBL_STEP1' => 'Paso 1 de 2 - Requisitos de preinstalación',
    'LBL_STEP2' => 'Paso 2 de 2 - Configuración',
    'LBL_STEP' => 'Paso',
    'LBL_TITLE_WELCOME' => 'Benvido a SuiteCRM ',
    //welcome page variables
    'LBL_TITLE_ARE_YOU_READY' => 'Está listo para instalar?',
    'REQUIRED_SYS_COMP' => 'Compoñentes de Sistema Requiridos',
    'REQUIRED_SYS_COMP_MSG' =>
        'Antes de comezar, por favor asegúrese de que ten as versións soportadas dos seguintes compoñentes do sistema:<br>
                      <ul>
                      <li> Base de Datos/Manexador de Sistema de Base de Datos (Exemplos: MariaDB, MySQL or SQL Server)</li>
                      <li> Servidor Web (Apache, IIS)</li>
                      </ul>
                      Consulte a Matriz de Compatibilidade nas Notas da Versión para encontrar os
                      compoñentes de sistema compatibles para a versión de SuiteCRM que está instalando<br>',
    'REQUIRED_SYS_CHK' => 'Chequeo Inicial do Sistema',
    'REQUIRED_SYS_CHK_MSG' =>
        'Cando vostede comeza o proceso de instalación, se realizará unha comprobación do sistema non servidor web onde se encontran os arquivos SuiteCRM para asegurarse de que o sistema está configurado correctamente e ten todos os compoñentes necesarios para completar a instalación. <br><br>O sistema verifica o seguinte: <br><ul><li><b>versión de PHP</b> &#8211; debe ser compatible coa aplicación</li> <li><b>Variables de sesión</b> &#8211; debe estar traballando correctamente</li> <li><b>Cadeas MB</b> &#8211; debe ser instalado e habilitado en php.ini</li> <li><b>Apoio de base de datos</b> &#8211; debe existir para MariaDB, MySQL ou SQL Server</li> <li><b>Config.php</b> &#8211; debe existir e debe ter os permisos adecuados para que sexa escribible</li> <li>os seguintes arquivos de SuiteCRM deben ser escribible: <ul><li><b>/ aduana</li> <li>/ cache</li> <li>/ módulos</li> <li>/ subir</b></li></ul></li></ul> si a comprobación falla, vostede non será capaz de proceder coa instalación.                                    Se mostrará un mensaxe de erro, explicando por que o seu sistema non pasó a verificación.                                   Despois de facer os cambios necesarios, pode someterse á comprobación do sistema outra vez para continuar a instalación. <br>',


    'REQUIRED_INSTALLTYPE' => 'Instalación Típica ou Personalizada',
    'REQUIRED_INSTALLTYPE_MSG' =>
        'Despois de realizado o chequeo do sistema, pode seleccionar entre instalación Típica ou Personalizada..<br><br>
                      Para ambas <b>Típica</b> e <b>Personalizada</b>, necesitará saber o seguinte:<br>
                      <ul>
                      <li> <b>Tipo de Base de Datos</b> que albergará a información de SuiteCRM <ul><li>Tipos de Base de Datos Compatibles: MariaDB, MySQL ou SQL Server.<br><br></li></ul></li>
                      <li> <b>Nome do servidor web</b> ou equipo (host) no cal se encontra a base de datos
                      <ul><li>Pode ser <i>localhost</i> se a base de datos se encontra no seu equipo local, ou no mesmo servidor web ou equipo que os arquivos de SuiteCRM.<br><br></li></ul></li>
                      <li><b>Nome da base de datos</b> que desexa utilizar para albergar a información de SuiteCRM</li>
                        <ul>
                          <li> Quizais vostede xa teña unha base de datos existente que desexaría utilizar. Se vostede provee o nome dunha base de datos existente, eliminaranse as táboas existentes da base de datos durante a instalación cando se defina o novo esquema para a base de datos de SuiteCRM.</li>
                          <li> Se aínda non posúe unha base de datos, o nome que vostede provea será utilizado durante a instalación para a nova base de datos que sexa creada para a instancia..<br><br></li>
                        </ul>
                      <li><b>Nome e contrasinal do administrador da base de datos</b> <ul><li>O administrador de baes de datos debería ter permisos para crear táboas, usuarios e escribir na base de datos.</li><li>Pode que sexa necesario contactar ao seu administrador de base de datos para que lle provea esta información se a base de datos non se encontra no seu equipo local e/ou se vostede non é o administrador de base de datos.<br><br></ul></li></li>
                      <li> <b>Nome de usuario de base de datos e contrasinal</b>
                      </li>
                        <ul>
                          <li> O usuario pode ser o administrador da base de datos, ou pode proveer o nome doutro usuario existente da base de datos. </li>
                          <li> Se desexa crear un novo usuario de base de datos para este propósito, deberá proveer un novo nome de usuario e contrasinal, e o usuario será creado durante a instalación. </li>
                        </ul></ul><p>

                      Para a configuración <b>Personalizada</b>, tamén deberá saber o seguinte:<br>
                      <ul>
                      <li> <b>URL que será utilizada para acceder á instancia de SuiteCRM</b> despois de ser instalada.
                      Esta URL pode incluir o nome ou enderezo IP do servidor ou equipo.<br><br></li>
                                  <li> [Opcional] <b>Ruta do directorio de sesión</b> se desexa utilizar un directorio personalizado para a información de SuiteCRM co obxectivo de evitar vulnerabilidade en servidores compartidos.<br><br></li>
                                  <li> [Opcional] <b>Ruta personalizada do directorio de log</b> se desexa sobreescribir o directorio por defecto utilizado para os arquivos de log de SuiteCRM.<br><br></li>
                                  <li> [Opcional] <b>ID de Aplicación</b> se desexa sobreescribir o ID autoxerado para garantizar que as sesións dunha instancia de SuiteCRM non son utilizadas por outras instancias.<br><br></li>
                                  <li><b>Set de Caracteres</b> comunmente utilizado segundo su zona.<br><br></li></ul>
                                  Para obter información máis detallada, por favor consulte a Guía de Instalación.
                                ',
    'LBL_WELCOME_PLEASE_READ_BELOW' => 'Por favor lea a seguinte información importante antes de continuar coa instalación. A información axudaralle a determinar se está listo ou non para instalar a aplicación neste momento.',

    'LBL_WELCOME_CHOOSE_LANGUAGE' => '<b>Seleccione o seu idioma</b>',
    'LBL_WELCOME_SETUP_WIZARD' => 'Asistente de Configuración',
    'LBL_WIZARD_TITLE' => 'Asistente de Configuración de SuiteCRM: ',
    'LBL_YES' => 'Si',

    'LBL_PATCHES_TITLE' => 'Instalar os Ultimos Parches',
    'LBL_MODULE_TITLE' => 'Instalar Paquetes de Idioma',
    'LBL_PATCH_1' => 'Se desexa omitir este paso, faga click en Seguinte.',
    'LBL_PATCH_TITLE' => 'Parche do Sistema',
    'LBL_PATCH_READY' => 'Os seguintes parches están listos para ser instalados:',
    'LBL_SESSION_ERR_DESCRIPTION' => "SuiteCRM depende das Sesións de PHP para almacenar información importante mentres se conecta ao servidor web. A súa instalación de PHP non ten a información de Sesións correctamente configurada. 
	<br><br>Un problema común de configuración é que a directiva <b>'session.save_path'</b> non está sinalando un directorio válido. <br>
	<br> Por favor corrixa a súa <a target=_new href='http://us2.php.net/manual/en/ref.session.php'>configuración PHP</a> no arquivo php.ini ubicado a continuación.",
    'LBL_SESSION_ERR_TITLE' => 'Erro de Configuración de Sesións PHP',
    'LBL_SYSTEM_NAME' => 'Nome do Sistema',
    'LBL_COLLATION' => 'Configuración de Collation',
    'LBL_REQUIRED_SYSTEM_NAME' => 'Provea un Nome de Sistema para a súa instancia SuiteCRM.',
    'LBL_PATCH_UPLOAD' => 'Seleccione un arquivo de parche do seu equipo local',
    'LBL_INCOMPATIBLE_PHP_VERSION' => 'RequíresePhp versión 5 ou superior.',
    'LBL_MINIMUM_PHP_VERSION' => 'A versión mínima de PHP requirida é 5.1.0. A versión recomendada é 5.2.x.',
    'LBL_YOUR_PHP_VERSION' => '(A súa versión actual é ',
    'LBL_RECOMMENDED_PHP_VERSION' => ' a versión recomendada é 5.2.x)',
    'LBL_BACKWARD_COMPATIBILITY_ON' => 'O modo Compatibilidade con versións anteriores de PHP está activado. Estableza zend.ze1_compatibility_mode en Off para poder continuar.',
    'LBL_STREAM' => 'PHP permite o uso de streaming',

    'advanced_password_new_account_email' => array(
        'subject' => 'Información da Nova Conta de Usuario',
        'type' => 'sistema',
        'description' => 'Esta plantilla é utilizada cando un Administrador de Sistema envía un novo contrasinal a un usuario.',
        'body' => '<div><table border=\\"0\\" cellspacing=\\"0\\" cellpadding=\\"0\\" width="550" align=\\"\\&quot;\\&quot;center\\&quot;\\&quot;\\"><tbody><tr><td colspan=\\"2\\"><p>Aquí está o seu novo nome de usuario e contrasinal temporal:</p><p>Nome de Usuario : $contact_user_user_name </p><p>Contrasinal : $contact_user_user_hash </p><br><p>$config_site_url</p><br><p>Despois de ingresar utilizando o contrasinal de arriba, pode que se lle pida cambiar o contrasinal por un da súa propia elección.</p>   </td>         </tr><tr><td colspan=\\"2\\"></td>         </tr> </tbody></table> </div>',
        'txt_body' =>
            '
Aquí está o seu novo nome de usuario e contrasinal temporal:
Nome de Usuario : $contact_user_user_name
Contrasinal : $contact_user_user_hash

$config_site_url

Despois de ingresar utilizando o contrasinal de arriba, pode que se lle pida cambiar o contrasinal por un da súa propia elección.',
        'name' => 'Email de contrasinal xerado polo sistema',
    ),
    'advanced_password_forgot_password_email' => array(
        'subject' => 'Reestablecer o seu contrasinal',
        'type' => 'sistema',
        'description' => "Esta plantilla é utilizada para enviarlle un enlace ao usuario que ao cliquearse reestablece o contrasinal da conta do usuario.",
        'body' => '<div><table border=\\"0\\" cellspacing=\\"0\\" cellpadding=\\"0\\" width="550" align=\\"\\&quot;\\&quot;center\\&quot;\\&quot;\\"><tbody><tr><td colspan=\\"2\\"><p>Recentemente ($contact_user_pwd_last_changed) requiriu reestablecer o contrasinal da súa conta. </p><p>Faga click no seguinte enlace para reestablecer o seu contrasinal:</p><p> $contact_user_link_guid </p>  </td>         </tr><tr><td colspan=\\"2\\"></td>         </tr> </tbody></table> </div>',
        'txt_body' =>
            '
Recentemente ($contact_user_pwd_last_changed) requiriu reestablecer o contrasinal da súa conta.

Faga click no seguinte enlace para reestablecer o seu contrasinal:

$contact_user_link_guid',
        'name' => 'Email de Contrasinal Olvidado',
    ),


    'two_factor_auth_email' => array(
        'subject' => 'Código de autenticación de dous factores',
        'type' => 'sistema',
        'description' => "Esta plantilla é usada para enviar ao usuario un código de autenticación de dous factores.",
        'body' => '<div><table border=\\"0\\" cellspacing=\\"0\\" cellpadding=\\"0\\" width="550" align=\\"\\&quot;\\&quot;center\\&quot;\\&quot;\\"><tbody><tr><td colspan=\\"2\\"><p>código de autenticación de doble Factor é  <b>$code</b>.</p>  </td>         </tr><tr><td colspan=\\"2\\"></td>         </tr> </tbody></table> </div>',
        'txt_body' =>
            'Código de autenticación de dous factores é $code.',
        'name' => 'Correo de autenticación de dous factores',
    ),

    // SMTP settings

    'LBL_FROM_NAME' => 'Nome do Remitente:',
    'LBL_FROM_ADDR' => 'Enderezo do Remitente:',

    'LBL_WIZARD_SMTP_DESC' => 'Proporcione a conta de correo que se utilizará para enviar correos, como as notificacións de asignación e os contrasinais de novos usuarios. Os usuarios recibirán correos de SuiteCRM, como se foran enviados desde a conta de correo especificada.',
    'LBL_CHOOSE_EMAIL_PROVIDER' => 'Escolla o seu proveedor de Email:',

    'LBL_SMTPTYPE_GMAIL' => 'Gmail',
    'LBL_SMTPTYPE_YAHOO' => 'Correo Yahoo',
    'LBL_SMTPTYPE_EXCHANGE' => 'Microsoft Exchange',
    'LBL_SMTPTYPE_OTHER' => 'Outro',
    'LBL_MAIL_SMTP_SETTINGS' => 'Especificación de Servidor SMTP',
    'LBL_MAIL_SMTPSERVER' => 'Servidor SMTP:',
    'LBL_MAIL_SMTPPORT' => 'Porto SMTP:',
    'LBL_MAIL_SMTPAUTH_REQ' => '¿Usar Autenticación SMTP?',
    'LBL_EMAIL_SMTP_SSL_OR_TLS' => '¿Habilitar SMTP sobre SSL ou TLS?',
    'LBL_GMAIL_SMTPUSER' => 'Enderezo de Email de Gmail:',
    'LBL_GMAIL_SMTPPASS' => 'Contrasinal de Gmail:',
    'LBL_ALLOW_DEFAULT_SELECTION' => 'Permite aos usuarios utilizar esta conta para correo saínte:',
    'LBL_ALLOW_DEFAULT_SELECTION_HELP' => 'Cando esta opción está seleccionada, todos os usuarios poderán enviar correos usando a mesma conta de correo saínte para o envío de notificacións do sistema e alertas.  Se a opción non está seleccionada, os usuarios poden usar o servidor de correo saínte tras proporcionar a información da súa propia conta.',

    'LBL_YAHOOMAIL_SMTPPASS' => 'Contrasinal de Yahoo! Mail:',
    'LBL_YAHOOMAIL_SMTPUSER' => 'ID de Yahoo! Mail:',

    'LBL_EXCHANGE_SMTPPASS' => 'Contrasinal de Exchange:',
    'LBL_EXCHANGE_SMTPUSER' => 'Nome de usuario de Exchange:',
    'LBL_EXCHANGE_SMTPPORT' => 'Porto de Servidor Exchange:',
    'LBL_EXCHANGE_SMTPSERVER' => 'Servidor Exchange:',


    'LBL_MAIL_SMTPUSER' => 'Nome de Usuario SMTP:',
    'LBL_MAIL_SMTPPASS' => 'Contrasinal SMTP:',

    // Branding

    'LBL_WIZARD_SYSTEM_TITLE' => 'Imaxe de marca',
    'LBL_WIZARD_SYSTEM_DESC' => 'Proporcione o nome e logo da súa organización para establecer a imaxe da súa marca en SuiteCRM.',
    'SYSTEM_NAME_WIZARD' => 'Nome:',
    'SYSTEM_NAME_HELP' => 'Éste é o nome mostrado na barra de título do seu navegador.',
    'NEW_LOGO' => 'Seleccionar Logo:',
    'NEW_LOGO_HELP' => 'O formato do arquivo de imaxe pode ser tanto .png como .jpg. A altura máxima é 170px, e a anchura máxima é 450px. Calquera imaxe cargada que se sobrepase nalgunha das medidas será modificada ao tamaño indicado, segundo a medida que exceda.',
    'COMPANY_LOGO_UPLOAD_BTN' => 'Subir',
    'CURRENT_LOGO' => 'Logo Actual:',
    'CURRENT_LOGO_HELP' => 'Este logotipo mostrase no centro da pantalla de inicio de sesión da aplicación SuiteCRM.',


    //Scenario selection of modules
    'LBL_WIZARD_SCENARIO_TITLE' => 'Selección de escenario',
    'LBL_WIZARD_SCENARIO_DESC' => 'Isto é para permitir a personalización dos módulos mostrados segundo os seus requirimento.  Cada un dos módulos pódese activar trala instalación utilizando a páxina de administración.',
    'LBL_WIZARD_SCENARIO_EMPTY' => 'Non hai escenarios establecidos actualmente no arquivo de configuración (config.php)',


    // System Local Settings


    'LBL_LOCALE_TITLE' => 'Configuración Rexional do Sistema',
    'LBL_WIZARD_LOCALE_DESC' => 'Especifique como desexa que os datos sexan mostrados en SuiteCRM, baseándose na súa ubicación xeográfica. A configuración que proporcione aquí será a utiliza por defecto. Os usuarios poderán establecer as súas propias preferências.',
    'LBL_DATE_FORMAT' => 'Formato de Data:',
    'LBL_TIME_FORMAT' => 'Formato de Hora:',
    'LBL_TIMEZONE' => 'Zona Horaria:',
    'LBL_LANGUAGE' => 'Idioma:',
    'LBL_CURRENCY' => 'Moeda:',
    'LBL_CURRENCY_SYMBOL' => 'Símbolo de moeda:',
    'LBL_CURRENCY_ISO4217' => 'Código de moeda ISO 4217:',
    'LBL_NUMBER_GROUPING_SEP' => 'Separador de miles',
    'LBL_DECIMAL_SEP' => 'Símbolo Decimal',
    'LBL_NAME_FORMAT' => 'Formato de Nome:',
    'UPLOAD_LOGO' => 'Por favor espere, cargando logo..',
    'ERR_UPLOAD_FILETYPE' => 'Tipo de arquivo non permitido, por favor cargue un jpg ou png.',
    'ERR_LANG_UPLOAD_UNKNOWN' => 'Ocurreu un erro descoñecido de cargue de arquivo.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_INI_SIZE' => 'O arquivo subido excede o límite definido pola directiva upload_max_filesize en php.ini.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_FORM_SIZE' => 'O arquivo subido excede o límite definido pola directiva MAX_FILE_SIZE especificada no formulario HTML.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_PARTIAL' => 'O arquivo foi só parcialmente subido.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_NO_FILE' => 'Non se subiu ningún arquivo.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_NO_TMP_DIR' => 'Falta unha carpeta temporal.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_CANT_WRITE' => 'Erro ao escribir o arquivo.',
    'ERR_UPLOAD_FILE_UPLOAD_ERR_EXTENSION' => 'Unha extensión PHP detivo a carga de ficheiros. PHP non proporciona unha maneira de averiguar que extensión causou a parada na subida de ficheiros.',

    'LBL_INSTALL_PROCESS' => 'Instalar...',

    'LBL_EMAIL_ADDRESS' => 'Correo electrónico:',
    'ERR_ADMIN_EMAIL' => 'O enderezo de correo electrónico do administrador é incorrecto.',
    'ERR_SITE_URL' => 'A URL do sitio é necesaria.',

    'STAT_CONFIGURATION' => 'Relacións de configuración...',
    'STAT_CREATE_DB' => 'Crear base de datos...',

    'STAT_CREATE_DEFAULT_SETTINGS' => 'Crear valores predeterminados...',
    'STAT_INSTALL_FINISH' => 'Fin da instalación...',
    'STAT_INSTALL_FINISH_LOGIN' => 'O proceso de instalación terminou, <a href="%s"> por favor, iniciar sesión...</a>',
    'LBL_LICENCE_TOOLTIP' => 'Por favor acepte a licenza primeiro',

    'LBL_MORE_OPTIONS_TITLE' => 'Máis opcións',
    'LBL_START' => 'Comezar',
    'LBL_DB_CONN_ERR' => 'Erro de base de datos',
    'LBL_OLD_PHP' => 'Versión PHP antigüa detectada!',
    'LBL_OLD_PHP_MSG' => 'A versión de PHP recomendada para instalar SuiteCRM é %s <br />A versión mínima de PHP requirida é %s<br />Estás a usar a versión de PHP %s, que está en EOL: <a href="http://php.net/eol.php">http://php.net/eol.php</a>.<br />Por favor, considera actualizar a túa versión de PHP.',
    'LBL_OLD_PHP_OK' => 'Son consciente dos riscos e desexo continuar.',

    'LBL_DBCONF_TITLE_USER_INFO_LABEL' => 'Usuario',
    'LBL_DBCONFIG_MSG3_LABEL' => 'Nome de Base de Datos',
    'LBL_DBCONFIG_MSG3' => 'Nome da base de datos que contenrá a información da instancia de SuiteCRM que está a punto de instalar:',
    'LBL_DBCONFIG_MSG2_LABEL' => 'Nome do Host',
    'LBL_DBCONFIG_MSG2' => 'Nome do servidor web ou no que se encontra (por exemplo www.mydomain.com) a base de datos da máquina (host). Se está instalando localmente, é mellor utilizar \'localhost\' que \'127.0.0.1\', por razóns de rendemento.',
    'LBL_DBCONFIG_B_MSG1_LABEL' => '', // this label dynamically needed in install/installConfig.php:293
    'LBL_DBCONFIG_B_MSG1' => 'Para configurar a base de datos de SuiteCRM é necesario o nome de usuario e contrasinal dun administrador de base de datos que poida crear as táboas, usuarios e escribir nela.'
);
