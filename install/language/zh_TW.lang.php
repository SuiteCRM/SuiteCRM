<?php
if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');
/*********************************************************************************
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc. ç
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
 * FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
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
 * SugarCRM" logo. If the display of the logo is not reasonably feasible for
 * technical reasons, the Appropriate Legal Notices must display the words
 * "Powered by SugarCRM".
 ********************************************************************************/

/*********************************************************************************

 * Description:
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc. All Rights
 * Reserved. Contributor(s): ______________________________________..
 * *******************************************************************************/

$mod_strings = array(
	'LBL_BASIC_SEARCH'					=> '基本搜尋',
	'LBL_ADVANCED_SEARCH'				=> '進階搜尋',
	'LBL_BASIC_TYPE'					=> '基本類型',
	'LBL_ADVANCED_TYPE'					=> '進階類型',
	'LBL_SYSOPTS_1'						=> '從下列系統設定選項中選取。',
    'LBL_SYSOPTS_2'                     => '你將使用什麼類型的資料庫在本次SuiteCRM安裝?',
	'LBL_SYSOPTS_CONFIG'				=> '系統設定',
	'LBL_SYSOPTS_DB_TYPE'				=> '',
	'LBL_SYSOPTS_DB'					=> '選擇資料庫類型',
    'LBL_SYSOPTS_DB_TITLE'              => '資料庫類型',
	'LBL_SYSOPTS_ERRS_TITLE'			=> '在繼續之前，請先將下列錯誤排除：',
	'LBL_MAKE_DIRECTORY_WRITABLE'      => '請將將下列資料夾改為可讀取：',
    'ERR_DB_VERSION_FAILURE'			=> '無法確認資料庫版本。',
	'DEFAULT_CHARSET'					=> 'UTF-8',
    'ERR_ADMIN_USER_NAME_BLANK'         => '提供SuiteCRM管理員的名稱。',
	'ERR_ADMIN_PASS_BLANK'				=> '提供SuiteCRM管理員的密碼。',

    //'ERR_CHECKSYS_CALL_TIME'			=> 'Allow Call Time Pass Reference is Off (please enable in php.ini)',
    'ERR_CHECKSYS'                      => '相容性確認時發生錯誤。為了讓你的SuiteCRM安裝過程正常，請按照適當步驟將下列問題解決，解決請按重新確認按鈕，或是再安裝一次。',
    'ERR_CHECKSYS_CALL_TIME'            => 'Allow Call Time Pass Reference is On (this should be set to Off in php.ini)',
	'ERR_CHECKSYS_CURL'					=> 'Not found: SuiteCRM Scheduler will run with limited functionality.',
    'ERR_CHECKSYS_IMAP'					=> 'Not found: InboundEmail and Campaigns (Email) require the IMAP libraries. Neither will be functional.',
	'ERR_CHECKSYS_MSSQL_MQGPC'			=> 'Magic Quotes GPC cannot be turned "On" when using MS SQL Server.',
	'ERR_CHECKSYS_MEM_LIMIT_0'			=> '警告：',
	'ERR_CHECKSYS_MEM_LIMIT_1'			=> '(設定為',
	'ERR_CHECKSYS_MEM_LIMIT_2'			=> 'M或更大，於你的php.ini)',
	'ERR_CHECKSYS_MYSQL_VERSION'		=> '最低需求版本4.1.2 - 偵測到：',
	'ERR_CHECKSYS_NO_SESSIONS'			=> '讀寫session失敗。無法進行安裝。',
	'ERR_CHECKSYS_NOT_VALID_DIR'		=> '不是一個有效的資料夾',
	'ERR_CHECKSYS_NOT_WRITABLE'			=> '警告：無法寫入',
	'ERR_CHECKSYS_PHP_INVALID_VER'		=> '你安裝的PHP版本不被SuiteCRM支援。請在Release Notes中參考相容性表格，並安裝有效的PHP版本。你的版本是',
	'ERR_CHECKSYS_IIS_INVALID_VER'      => '你安裝的IIS版本不被SuiteCRM支援。請在Release Notes中參考相容性表格，並安裝有效的IIS版本。你的版本是',
	'ERR_CHECKSYS_FASTCGI'              => 'We detect that you are not using a FastCGI handler mapping for PHP. You will need to install/configure a version that is compatible with the SuiteCRM application.  Please consult the Compatibility Matrix in the Release Notes for supported Versions. Please see <a href="http://www.iis.net/php/" target="_blank">http://www.iis.net/php/</a> for details ',
	'ERR_CHECKSYS_FASTCGI_LOGGING'      => 'For optimal experience using IIS/FastCGI sapi, set fastcgi.logging to 0 in your php.ini file.',
    'ERR_CHECKSYS_PHP_UNSUPPORTED'		=> '安裝的PHP版本不支援：(版本',
    'LBL_DB_UNAVAILABLE'                => '資料庫無法取得',
    'LBL_CHECKSYS_DB_SUPPORT_NOT_AVAILABLE' => '查無資料庫支援。請確認你已安裝必要的驅動程式供下列資料庫類型使用：MySQL或MS SQLServer。你可能需要將php.ini中的extension取消註解，或以正確的二進位檔案重新編譯，視你的PHP版本不同而異。請參照你的PHP使用手冊或更多資訊以啟用資料庫支援。',
    'LBL_CHECKSYS_XML_NOT_AVAILABLE'        => '查無與XML解析庫有關且為SuiteCRM必要的功能。你可能需要將php.ini中的extension取消註解，或以正確的二進位檔案重新編譯，視你的PHP版本不同而異。請參照你的PHP使用手冊或更多資訊。',
    'ERR_CHECKSYS_MBSTRING'             => '查無與Multibyte Strings PHP extension (mbstring)有關且為SuiteCRM必要的功能。<br/><br/>一般來說，mbstring模組在PHP中並非預設，需要在PHP在建構時加入--enable-mbstring。請參照你的PHP使用手冊或更多資訊以啟用mbstring支援。',
    'ERR_CHECKSYS_SESSION_SAVE_PATH_NOT_SET'       => '你的PHP設定檔(php.ini)中，session.save_path並未設定，或是被指向一個不存在的資料夾。你可能需要在php.ini中設定session.save_path或是確認設定的資料夾存在。',
    'ERR_CHECKSYS_SESSION_SAVE_PATH_NOT_WRITABLE'  => '你的PHP設定檔(php.ini)中，session.save_path指向的資料夾不可寫入。請依必要步驟將資料夾設為可寫入。 <br>依據你的作業系統不同，或許需要執行chmod 766，或是在檔案名上按右鍵查看檔案屬性內容並取消唯讀選項。',
    'ERR_CHECKSYS_CONFIG_NOT_WRITABLE'  => '設定檔存在但是無法寫入。請依必要步驟將檔案設為可寫入。依據你的作業系統不同，或許需要執行chmod 766，或是在檔案名上按右鍵查看檔案屬性內容並取消唯讀選項。',
    'ERR_CHECKSYS_CONFIG_OVERRIDE_NOT_WRITABLE'  => '設定覆蓋檔存在但是無法寫入。請依必要步驟將檔案設為可寫入。依據你的作業系統不同，或許需要執行chmod 766，或是在檔案名上按右鍵查看檔案屬性內容並取消唯讀選項。',
    'ERR_CHECKSYS_CUSTOM_NOT_WRITABLE'  => '自訂資料夾Custom存在但是無法寫入。請依必要步驟將檔案設為可寫入。依據你的作業系統不同，或許需要進行權限變更(chmod 766)，或是在檔案名上按右鍵查看檔案屬性內容並取消唯讀選項。',
    'ERR_CHECKSYS_FILES_NOT_WRITABLE'   => "下列檔案貨資料夾無法被寫入或是不存在且無法被創建。依據你的作業系統不同，你可能需要修改檔案或父資料夾的權限(chmod 755)，或是在父資料夾按右鍵取消唯讀選項並套用到所有子目錄。",
    'LBL_CHECKSYS_OVERRIDE_CONFIG' => 'Config override',
	//'ERR_CHECKSYS_SAFE_MODE'			=> 'Safe Mode is On (please disable in php.ini)',
	'ERR_CHECKSYS_SAFE_MODE'			=> '安全模式開啟 (你或許希望在php.ini中關閉)',
    'ERR_CHECKSYS_ZLIB'					=> '查無ZLib支援：SuiteCRM系統效能須倚靠zlib壓縮來提昇。',
    'ERR_CHECKSYS_ZIP'					=> '查無ZIP支援：SuiteCRM需要ZIP支援以處理壓縮檔。',
    'ERR_CHECKSYS_PCRE'					=> '查無PCRE程序庫：SuiteCRM需要PCRE程序庫以處理Perl形式的正規表示法樣式匹配。',
    'ERR_CHECKSYS_PCRE_VER'				=> 'PCRE程序庫版本：SuiteCRM需要PCRE程序庫7.0或更新版本以處理Perl形式的正規表示法樣式匹配。',
	'ERR_DB_ADMIN'						=> '提供的資料庫管理員帳號與/或密碼不正確，無法成功進行資料庫連線。請填寫正確的帳號及密碼。(錯誤：',
    'ERR_DB_ADMIN_MSSQL'                => '提供的資料庫管理員帳號與/或密碼不正確，無法成功進行資料庫連線。請填寫正確的帳號及密碼。',
	'ERR_DB_EXISTS_NOT'					=> '指定的資料庫不存在。',
	'ERR_DB_EXISTS_WITH_CONFIG'			=> '資料庫已存在並有設定資料在其中。若要用所選資料庫進行一次全新安裝，請重新執行安裝步驟並選擇"放棄並重新建立已存在的SuiteCRM資料表?" 若要進行系統升級，請用管理員介面中的升級精靈。請閱讀<a href="http://www.suitecrm.com target="_new">此處</a>的升級文件。',
	'ERR_DB_EXISTS'						=> '所提供的資料庫名稱已存在，無法用同樣名稱創建另一個資料庫。',
    'ERR_DB_EXISTS_PROCEED'             => 'The provided Database Name already exists.  You can<br>1.  hit the back button and choose a new database name <br>2.  click next and continue but all existing tables on this database will be dropped.  <strong>This means your tables and data will be blown away.</strong>',
	'ERR_DB_HOSTNAME'					=> 'Host name cannot be blank.',
	'ERR_DB_INVALID'					=> 'Invalid database type selected.',
	'ERR_DB_LOGIN_FAILURE'				=> 'The provided database host, username, and/or password is invalid, and a connection to the database could not be established.  Please enter a valid host, username and password',
	'ERR_DB_LOGIN_FAILURE_MYSQL'		=> 'The provided database host, username, and/or password is invalid, and a connection to the database could not be established.  Please enter a valid host, username and password',
	'ERR_DB_LOGIN_FAILURE_MSSQL'		=> 'The provided database host, username, and/or password is invalid, and a connection to the database could not be established.  Please enter a valid host, username and password',
	'ERR_DB_MYSQL_VERSION'				=> 'Your MySQL version (%s) is not supported by SuiteCRM.  You will need to install a version that is compatible with the SuiteCRM application.  Please consult the Compatibility Matrix in the Release Notes for supported MySQL versions.',
	'ERR_DB_NAME'						=> 'Database name cannot be blank.',
	'ERR_DB_NAME2'						=> "Database name cannot contain a '\\', '/', or '.'",
    'ERR_DB_MYSQL_DB_NAME_INVALID'      => "Database name cannot contain a '\\', '/', or '.'",
    'ERR_DB_MSSQL_DB_NAME_INVALID'      => "Database name cannot begin with a number, '#', or '@' and cannot contain a space, '\"', \"'\", '*', '/', '\\', '?', ':', '<', '>', '&', '!', or '-'",
    'ERR_DB_OCI8_DB_NAME_INVALID'       => "Database name can only consist of alphanumeric characters and the symbols '#', '_' or '$'",
	'ERR_DB_PASSWORD'					=> 'The passwords provided for the SuiteCRM database administrator do not match.  Please re-enter the same passwords in the password fields.',
	'ERR_DB_PRIV_USER'					=> 'Provide a database administrator user name.  The user is required for the initial connection to the database.',
	'ERR_DB_USER_EXISTS'				=> 'User name for SuiteCRM database user already exists -- cannot create another one with the same name. Please enter a new user name.',
	'ERR_DB_USER'						=> 'Enter a user name for the SuiteCRM database administrator.',
	'ERR_DBCONF_VALIDATION'				=> '在繼續之前，請先將下列錯誤排除：',
    'ERR_DBCONF_PASSWORD_MISMATCH'      => 'The passwords provided for the SuiteCRM database user do not match. Please re-enter the same passwords in the password fields.',
	'ERR_ERROR_GENERAL'					=> 'The following errors were encountered:',
	'ERR_LANG_CANNOT_DELETE_FILE'		=> 'Cannot delete file: ',
	'ERR_LANG_MISSING_FILE'				=> 'Cannot find file: ',
	'ERR_LANG_NO_LANG_FILE'			 	=> 'No language pack file found at include/language inside: ',
	'ERR_LANG_UPLOAD_1'					=> 'There was a problem with your upload.  Please try again.',
	'ERR_LANG_UPLOAD_2'					=> 'Language Packs must be ZIP archives.',
	'ERR_LANG_UPLOAD_3'					=> 'PHP could not move the temp file to the upgrade directory.',
	'ERR_LICENSE_MISSING'				=> 'Missing Required Fields',
	'ERR_LICENSE_NOT_FOUND'				=> 'License file not found!',
	'ERR_LOG_DIRECTORY_NOT_EXISTS'		=> 'Log directory provided is not a valid directory.',
	'ERR_LOG_DIRECTORY_NOT_WRITABLE'	=> 'Log directory provided is not a writable directory.',
	'ERR_LOG_DIRECTORY_REQUIRED'		=> 'Log directory is required if you wish to specify your own.',
	'ERR_NO_DIRECT_SCRIPT'				=> 'Unable to process script directly.',
	'ERR_NO_SINGLE_QUOTE'				=> '不可以使用單引號',
	'ERR_PASSWORD_MISMATCH'				=> 'The passwords provided for the SuiteCRM admin user do not match.  Please re-enter the same passwords in the password fields.',
	'ERR_PERFORM_CONFIG_PHP_1'			=> 'Cannot write to the <span class=stop>config.php</span> file.',
	'ERR_PERFORM_CONFIG_PHP_2'			=> 'You can continue this installation by manually creating the config.php file and pasting the configuration information below into the config.php file.  However, you <strong>must </strong>create the config.php file before you continue to the next step.',
	'ERR_PERFORM_CONFIG_PHP_3'			=> 'Did you remember to create the config.php file?',
	'ERR_PERFORM_CONFIG_PHP_4'			=> 'Warning: Could not write to config.php file.  Please ensure it exists.',
	'ERR_PERFORM_HTACCESS_1'			=> 'Cannot write to the ',
	'ERR_PERFORM_HTACCESS_2'			=> ' file.',
	'ERR_PERFORM_HTACCESS_3'			=> 'If you want to secure your log file from being accessible via browser, create an .htaccess file in your log directory with the line:',
	'ERR_PERFORM_NO_TCPIP'				=> '<b>We could not detect an Internet connection.</b> When you do have a connection, please visit <a href="http://www.suitecrm.com/">http://www.suitecrm.com/</a> to register with SuiteCRM. By letting us know a little bit about how your company plans to use SuiteCRM, we can ensure we are always delivering the right application for your business needs.',
	'ERR_SESSION_DIRECTORY_NOT_EXISTS'	=> 'Session directory provided is not a valid directory.',
	'ERR_SESSION_DIRECTORY'				=> 'Session directory provided is not a writable directory.',
	'ERR_SESSION_PATH'					=> 'Session path is required if you wish to specify your own.',
	'ERR_SI_NO_CONFIG'					=> 'You did not include config_si.php in the document root, or you did not define $sugar_config_si in config.php',
	'ERR_SITE_GUID'						=> 'Application ID is required if you wish to specify your own.',
    'ERROR_SPRITE_SUPPORT'              => "Currently we are not able to locate the GD library, as a result you will not be able to use the CSS Sprite functionality.",
	'ERR_UPLOAD_MAX_FILESIZE'			=> 'Warning: Your PHP configuration should be changed to allow files of at least 6MB to be uploaded.',
    'LBL_UPLOAD_MAX_FILESIZE_TITLE'     => 'Upload File Size',
	'ERR_URL_BLANK'						=> 'Provide the base URL for the SuiteCRM instance.',
	'ERR_UW_NO_UPDATE_RECORD'			=> '不能定位安裝記錄',
	'ERROR_FLAVOR_INCOMPATIBLE'			=> 'The uploaded file is not compatible with this flavor (Community Edition, Professional, or Enterprise) of SuiteCRM: ',
	'ERROR_LICENSE_EXPIRED'				=> "錯誤：您的許可過期 ",
	'ERROR_LICENSE_EXPIRED2'			=> " day(s) ago.   Please go to the <a href='index.php?action=LicenseSettings&module=Administration'>'\"License Management\"</a>  in the Admin screen to enter your new license key.  If you do not enter a new license key within 30 days of your license key expiration, you will no longer be able to log in to this application.",
	'ERROR_MANIFEST_TYPE'				=> '名單文件必須指定程序包類型。',
	'ERROR_PACKAGE_TYPE'				=> '名單文件指定了一個不能識別的程序包類型。',
	'ERROR_VALIDATION_EXPIRED'			=> "錯誤：您驗證的關鍵字過期 ",
	'ERROR_VALIDATION_EXPIRED2'			=> " day(s) ago.   Please go to the <a href='index.php?action=LicenseSettings&module=Administration'>'\"License Management\"</a> in the Admin screen to enter your new validation key.  If you do not enter a new validation key within 30 days of your validation key expiration, you will no longer be able to log in to this application.",
	'ERROR_VERSION_INCOMPATIBLE'		=> '上傳的文件和當前版本的Sugar Suite不兼容:',

	'LBL_BACK'							=> '上一步',
    'LBL_CANCEL'                        => '取消[Alt+X]',
    'LBL_ACCEPT'                        => 'I Accept',
	'LBL_CHECKSYS_1'					=> 'In order for your SuiteCRM installation to function properly, please ensure all of the system check items listed below are green. If any are red, please take the necessary steps to fix them.<BR><BR> For help on these system checks, please visit the <a href="http://www.suitecrm.com" target="_blank">SuiteCRM</a>.',
	'LBL_CHECKSYS_CACHE'				=> 'Writable Cache Sub-Directories',
    'LBL_DROP_DB_CONFIRM'               => 'The provided Database Name already exists.<br>You can either:<br>1.  Click on the Cancel button and choose a new database name, or <br>2.  Click the Accept button and continue.  All existing tables in the database will be dropped. <strong>This means that all of the tables and pre-existing data will be blown away.</strong>',
	'LBL_CHECKSYS_CALL_TIME'			=> 'PHP Allow Call Time Pass Reference Turned Off',
    'LBL_CHECKSYS_COMPONENT'			=> 'Component',
	'LBL_CHECKSYS_COMPONENT_OPTIONAL'	=> 'Optional Components',
	'LBL_CHECKSYS_CONFIG'				=> 'Writable SuiteCRM Configuration File (config.php)',
	'LBL_CHECKSYS_CONFIG_OVERRIDE'		=> 'Writable SuiteCRM Configuration File (config_override.php)',
	'LBL_CHECKSYS_CURL'					=> 'CURL模式',
    'LBL_CHECKSYS_SESSION_SAVE_PATH'    => 'Session Save Path Setting',
	'LBL_CHECKSYS_CUSTOM'				=> 'Writeable Custom Directory',
	'LBL_CHECKSYS_DATA'					=> 'Writable Data Sub-Directories',
	'LBL_CHECKSYS_IMAP'					=> 'IMAP模式',
	'LBL_CHECKSYS_FASTCGI'             => 'FastCGI',
	'LBL_CHECKSYS_MQGPC'				=> 'Magic Quotes GPC',
	'LBL_CHECKSYS_MBSTRING'				=> 'MB Strings Module',
	'LBL_CHECKSYS_MEM_OK'				=> 'OK (No Limit)',
	'LBL_CHECKSYS_MEM_UNLIMITED'		=> 'OK (Unlimited)',
	'LBL_CHECKSYS_MEM'					=> 'PHP Memory Limit',
	'LBL_CHECKSYS_MODULE'				=> 'Writable Modules Sub-Directories and Files',
	'LBL_CHECKSYS_MYSQL_VERSION'		=> 'MySQL Version',
	'LBL_CHECKSYS_NOT_AVAILABLE'		=> '無效',
	'LBL_CHECKSYS_OK'					=> '確定',
	'LBL_CHECKSYS_PHP_INI'				=> 'Location of your PHP configuration file (php.ini):',
	'LBL_CHECKSYS_PHP_OK'				=> 'OK (ver ',
	'LBL_CHECKSYS_PHPVER'				=> 'PHP Version',
    'LBL_CHECKSYS_IISVER'               => 'IIS Version',
	'LBL_CHECKSYS_RECHECK'				=> 'Re-check',
	'LBL_CHECKSYS_SAFE_MODE'			=> 'PHP Safe Mode Turned Off',
	'LBL_CHECKSYS_SESSION'				=> 'Writable Session Save Path (',
	'LBL_CHECKSYS_STATUS'				=> '狀態',
	'LBL_CHECKSYS_TITLE'				=> 'System Check Acceptance',
	'LBL_CHECKSYS_VER'					=> 'Found: ( ver ',
	'LBL_CHECKSYS_XML'					=> 'XML解析',
	'LBL_CHECKSYS_ZLIB'					=> 'ZLIB Compression Module',
	'LBL_CHECKSYS_ZIP'					=> 'ZIP Handling Module',
	'LBL_CHECKSYS_PCRE'					=> 'PCRE Library',
	'LBL_CHECKSYS_FIX_FILES'            => 'Please fix the following files or directories before proceeding:',
    'LBL_CHECKSYS_FIX_MODULE_FILES'     => 'Please fix the following module directories and the files under them before proceeding:',
    'LBL_CHECKSYS_UPLOAD'               => 'Writable Upload Directory',
    'LBL_CLOSE'							=> '關閉',
    'LBL_THREE'                         => '3',
	'LBL_CONFIRM_BE_CREATED'			=> 'be created',
	'LBL_CONFIRM_DB_TYPE'				=> '資料庫類型',
	'LBL_CONFIRM_DIRECTIONS'			=> 'Please confirm the settings below.  If you would like to change any of the values, click "Back" to edit.  Otherwise, click "Next" to start the installation.',
	'LBL_CONFIRM_LICENSE_TITLE'			=> 'License Information',
	'LBL_CONFIRM_NOT'					=> 'not',
	'LBL_CONFIRM_TITLE'					=> '確認設置',
	'LBL_CONFIRM_WILL'					=> 'will',
	'LBL_DBCONF_CREATE_DB'				=> 'Create Database',
	'LBL_DBCONF_CREATE_USER'			=> '新增用戶',
	'LBL_DBCONF_DB_DROP_CREATE_WARN'	=> 'Caution: All SuiteCRM data will be erased<br>if this box is checked.',
	'LBL_DBCONF_DB_DROP_CREATE'			=> 'Drop and Recreate Existing SuiteCRM tables?',
    'LBL_DBCONF_DB_DROP'                => 'Drop Tables',
    'LBL_DBCONF_DB_NAME'				=> 'Database Name',
	'LBL_DBCONF_DB_PASSWORD'			=> 'SuiteCRM Database User Password',
	'LBL_DBCONF_DB_PASSWORD2'			=> 'Re-enter SuiteCRM Database User Password',
	'LBL_DBCONF_DB_USER'				=> 'SuiteCRM Database User',
    'LBL_DBCONF_SUGAR_DB_USER'          => 'SuiteCRM Database User',
    'LBL_DBCONF_DB_ADMIN_USER'          => 'Database Administrator Username',
    'LBL_DBCONF_DB_ADMIN_PASSWORD'      => 'Database Admin Password',
	'LBL_DBCONF_DEMO_DATA'				=> 'Populate Database with Demo Data?',
    'LBL_DBCONF_DEMO_DATA_TITLE'        => 'Choose Demo Data',
	'LBL_DBCONF_HOST_NAME'				=> 'Host Name',
	'LBL_DBCONF_HOST_INSTANCE'			=> 'Host Instance',
	'LBL_DBCONF_HOST_PORT'				=> '埠',
	'LBL_DBCONF_INSTRUCTIONS'			=> 'Please enter your database configuration information below. If you are unsure of what to fill in, we suggest that you use the default values.',
	'LBL_DBCONF_MB_DEMO_DATA'			=> 'Use multi-byte text in demo data?',
    'LBL_DBCONFIG_MSG2'                 => 'Name of web server or machine (host) on which the database is located ( such as localhost or www.mydomain.com ):',
	'LBL_DBCONFIG_MSG2_LABEL' => 'Host Name',
    'LBL_DBCONFIG_MSG3'                 => 'Name of the database that will contain the data for the SuiteCRM instance you are about to install:',
	'LBL_DBCONFIG_MSG3_LABEL' => 'Database Name',
    'LBL_DBCONFIG_B_MSG1'               => 'The username and password of a database administrator who can create database tables and users and who can write to the database is necessary in order to set up the SuiteCRM database.',
	'LBL_DBCONFIG_B_MSG1_LABEL' => '',
    'LBL_DBCONFIG_SECURITY'             => 'For security purposes, you can specify an exclusive database user to connect to the SuiteCRM database.  This user must be able to write, update and retrieve data on the SuiteCRM database that will be created for this instance.  This user can be the database administrator specified above, or you can provide new or existing database user information.',
    'LBL_DBCONFIG_AUTO_DD'              => 'Do it for me',
    'LBL_DBCONFIG_PROVIDE_DD'           => 'Provide existing user',
    'LBL_DBCONFIG_CREATE_DD'            => 'Define user to create',
    'LBL_DBCONFIG_SAME_DD'              => 'Same as Admin User',
	//'LBL_DBCONF_I18NFIX'              => 'Apply database column expansion for varchar and char types (up to 255) for multi-byte data?',
    'LBL_FTS'                           => 'Full Text Search',
    'LBL_FTS_INSTALLED'                 => '已安裝',
    'LBL_FTS_INSTALLED_ERR1'            => 'Full Text Search capability is not installed.',
    'LBL_FTS_INSTALLED_ERR2'            => 'You can still install but will not be able to use Full Text Search functionality.  Please refer to your database server install guide on how to do this, or contact your Administrator.',
	'LBL_DBCONF_PRIV_PASS'				=> 'Privileged Database User Password',
	'LBL_DBCONF_PRIV_USER_2'			=> 'Database Account Above Is a Privileged User?',
	'LBL_DBCONF_PRIV_USER_DIRECTIONS'	=> 'This privileged database user must have the proper permissions to create a database, drop/create tables, and create a user.  This privileged database user will only be used to perform these tasks as needed during the installation process.  You may also use the same database user as above if that user has sufficient privileges.',
	'LBL_DBCONF_PRIV_USER'				=> 'Privileged Database User Name',
	'LBL_DBCONF_TITLE'					=> 'Database Configuration',
    'LBL_DBCONF_TITLE_NAME'             => 'Provide Database Name',
    'LBL_DBCONF_TITLE_USER_INFO'        => 'Provide Database User Information',
	'LBL_DBCONF_TITLE_USER_INFO_LABEL' => '負責人',
	'LBL_DBCONF_TITLE_PSWD_INFO_LABEL' => 'SMTP密碼:',
	'LBL_DISABLED_DESCRIPTION_2'		=> 'After this change has been made, you may click the "Start" button below to begin your installation.  <i>After the installation is complete, you will want to change the value for \'installer_locked\' to \'true\'.</i>',
	'LBL_DISABLED_DESCRIPTION'			=> 'The installer has already been run once.  As a safety measure, it has been disabled from running a second time.  If you are absolutely sure you want to run it again, please go to your config.php file and locate (or add) a variable called \'installer_locked\' and set it to \'false\'.  The line should look like this:',
	'LBL_DISABLED_HELP_1'				=> 'For installation help, please visit the SuiteCRM',
    'LBL_DISABLED_HELP_LNK'             => 'http://www.suitecrm.com/forum/index',
	'LBL_DISABLED_HELP_2'				=> 'support forums',
	'LBL_DISABLED_TITLE_2'				=> 'SuiteCRM Installation has been Disabled',
	'LBL_DISABLED_TITLE'				=> 'SuiteCRM Installation Disabled',
	'LBL_EMAIL_CHARSET_DESC'			=> 'Character Set most commonly used in your locale',
	'LBL_EMAIL_CHARSET_TITLE'			=> 'Outbound Email Settings',
    'LBL_EMAIL_CHARSET_CONF'            => 'Character Set for Outbound Email ',
	'LBL_HELP'							=> '幫助',
    'LBL_INSTALL'                       => '安裝',
    'LBL_INSTALL_TYPE_TITLE'            => 'Installation Options',
    'LBL_INSTALL_TYPE_SUBTITLE'         => 'Choose Install Type',
    'LBL_INSTALL_TYPE_TYPICAL'          => ' <b>Typical Install</b>',
    'LBL_INSTALL_TYPE_CUSTOM'           => ' <b>Custom Install</b>',
    'LBL_INSTALL_TYPE_MSG1'             => 'The key is required for general application functionality, but it is not required for installation. You do not need to enter the key at this time, but you will need to provide the key after you have installed the application.',
    'LBL_INSTALL_TYPE_MSG2'             => 'Requires minimum information for the installation. Recommended for new users.',
    'LBL_INSTALL_TYPE_MSG3'             => 'Provides additional options to set during the installation. Most of these options are also available after installation in the admin screens. Recommended for advanced users.',
	'LBL_LANG_1'						=> 'To use a language in SuiteCRM other than the default language (US-English), you can upload and install the language pack at this time. You will be able to upload and install language packs from within the SuiteCRM application as well.  If you would like to skip this step, click Next.',
	'LBL_LANG_BUTTON_COMMIT'			=> '安裝',
	'LBL_LANG_BUTTON_REMOVE'			=> '移除',
	'LBL_LANG_BUTTON_UNINSTALL'			=> '刪除',
	'LBL_LANG_BUTTON_UPLOAD'			=> '上傳',
	'LBL_LANG_NO_PACKS'					=> '無',
	'LBL_LANG_PACK_INSTALLED'			=> 'The following language packs have been installed: ',
	'LBL_LANG_PACK_READY'				=> 'The following language packs are ready to be installed: ',
	'LBL_LANG_SUCCESS'					=> 'The language pack was successfully uploaded.',
	'LBL_LANG_TITLE'			   		=> '語言包',
    'LBL_LAUNCHING_SILENT_INSTALL'     => 'Installing SuiteCRM now.  This may take up to a few minutes.',
	'LBL_LANG_UPLOAD'					=> 'Upload a Language Pack',
	'LBL_LICENSE_ACCEPTANCE'			=> 'License Acceptance',
    'LBL_LICENSE_CHECKING'              => 'Checking system for compatibility.',
    'LBL_LICENSE_CHKENV_HEADER'         => 'Checking Environment',
    'LBL_LICENSE_CHKDB_HEADER'          => 'Verifying DB Credentials.',
    'LBL_LICENSE_CHECK_PASSED'          => 'System passed check for compatibility.',
	'LBL_CREATE_CACHE' => 'Preparing to Install...',
    'LBL_LICENSE_REDIRECT'              => 'Redirecting in ',
	'LBL_LICENSE_DIRECTIONS'			=> 'If you have your license information, please enter it in the fields below.',
	'LBL_LICENSE_DOWNLOAD_KEY'			=> 'Enter Download Key',
	'LBL_LICENSE_EXPIRY'				=> '有效期',
	'LBL_LICENSE_I_ACCEPT'				=> 'I Accept',
	'LBL_LICENSE_NUM_USERS'				=> '用戶數',
	'LBL_LICENSE_OC_DIRECTIONS'			=> 'Please enter the number of purchased offline clients.',
	'LBL_LICENSE_OC_NUM'				=> '離線客戶端許可證數',
	'LBL_LICENSE_OC'					=> 'Offline Client Licenses',
	'LBL_LICENSE_PRINTABLE'				=> ' Printable View ',
    'LBL_PRINT_SUMM'                    => 'Print Summary',
	'LBL_LICENSE_TITLE_2'				=> 'SuiteCRM License',
	'LBL_LICENSE_TITLE'					=> 'License Information',
	'LBL_LICENSE_USERS'					=> 'Licensed Users',

	'LBL_LOCALE_CURRENCY'				=> 'Currency Settings',
	'LBL_LOCALE_CURR_DEFAULT'			=> '默認貨幣',
	'LBL_LOCALE_CURR_SYMBOL'			=> '貨幣符號',
	'LBL_LOCALE_CURR_ISO'				=> 'Currency Code (ISO 4217)',
	'LBL_LOCALE_CURR_1000S'				=> '千分符',
	'LBL_LOCALE_CURR_DECIMAL'			=> 'Decimal Separator',
	'LBL_LOCALE_CURR_EXAMPLE'			=> '實例',
	'LBL_LOCALE_CURR_SIG_DIGITS'		=> 'Significant Digits',
	'LBL_LOCALE_DATEF'					=> 'Default Date Format',
	'LBL_LOCALE_DESC'					=> 'The specified locale settings will be reflected globally within the SuiteCRM instance.',
	'LBL_LOCALE_EXPORT'					=> 'Character Set for Import/Export<br> <i>(Email, .csv, vCard, PDF, data import)</i>',
	'LBL_LOCALE_EXPORT_DELIMITER'		=> 'Export (.csv) Delimiter',
	'LBL_LOCALE_EXPORT_TITLE'			=> 'Import/Export Settings',
	'LBL_LOCALE_LANG'					=> 'Default Language',
	'LBL_LOCALE_NAMEF'					=> 'Default Name Format',
	'LBL_LOCALE_NAMEF_DESC'				=> 's = salutation<br />f = first name<br />l = last name',
	'LBL_LOCALE_NAME_FIRST'				=> '約翰',
	'LBL_LOCALE_NAME_LAST'				=> '某',
	'LBL_LOCALE_NAME_SALUTATION'		=> '博士',
	'LBL_LOCALE_TIMEF'					=> 'Default Time Format',

    'LBL_CUSTOMIZE_LOCALE'              => 'Customize Locale Settings',
	'LBL_LOCALE_UI'						=> '用戶界面',

	'LBL_ML_ACTION'						=> '操作',
	'LBL_ML_DESCRIPTION'				=> '描述',
	'LBL_ML_INSTALLED'					=> '安裝日期',
	'LBL_ML_NAME'						=> '名稱',
	'LBL_ML_PUBLISHED'					=> '公佈日期',
	'LBL_ML_TYPE'						=> '類型',
	'LBL_ML_UNINSTALLABLE'				=> '可卸載',
	'LBL_ML_VERSION'					=> '版本',
	'LBL_MSSQL'							=> 'SQL Server',
	'LBL_MSSQL2'                        => 'SQL Server (FreeTDS)',
	'LBL_MSSQL_SQLSRV'				    => 'SQL Server (Microsoft SQL Server Driver for PHP)',
	'LBL_MYSQL'							=> 'MySQL',
    'LBL_MYSQLI'						=> 'MySQL (mysqli extension)',
	'LBL_IBM_DB2'						=> 'IBM DB2',
	'LBL_NEXT'							=> '下頁',
	'LBL_NO'							=> '否',
    'LBL_ORACLE'						=> 'Oracle',
	'LBL_PERFORM_ADMIN_PASSWORD'		=> 'Setting site admin password',
	'LBL_PERFORM_AUDIT_TABLE'			=> 'audit table / ',
	'LBL_PERFORM_CONFIG_PHP'			=> 'Creating SuiteCRM configuration file',
	'LBL_PERFORM_CREATE_DB_1'			=> '<b>Creating the database</b> ',
	'LBL_PERFORM_CREATE_DB_2'			=> ' <b>on</b> ',
	'LBL_PERFORM_CREATE_DB_USER'		=> 'Creating the Database username and password...',
	'LBL_PERFORM_CREATE_DEFAULT'		=> 'Creating default SuiteCRM data',
	'LBL_PERFORM_CREATE_LOCALHOST'		=> 'Creating the Database username and password for localhost...',
	'LBL_PERFORM_CREATE_RELATIONSHIPS'	=> 'Creating SuiteCRM relationship tables',
	'LBL_PERFORM_CREATING'				=> 'creating / ',
	'LBL_PERFORM_DEFAULT_REPORTS'		=> 'Creating default reports',
	'LBL_PERFORM_DEFAULT_SCHEDULER'		=> 'Creating default scheduler jobs',
	'LBL_PERFORM_DEFAULT_SETTINGS'		=> 'Inserting default settings',
	'LBL_PERFORM_DEFAULT_USERS'			=> 'Creating default users',
	'LBL_PERFORM_DEMO_DATA'				=> 'Populating the database tables with demo data (this may take a little while)',
	'LBL_PERFORM_DONE'					=> 'done<br>',
	'LBL_PERFORM_DROPPING'				=> 'dropping / ',
	'LBL_PERFORM_FINISH'				=> '完成',
	'LBL_PERFORM_LICENSE_SETTINGS'		=> 'Updating license information',
	'LBL_PERFORM_OUTRO_1'				=> 'The setup of SuiteCRM ',
	'LBL_PERFORM_OUTRO_2'				=> ' is now complete!',
	'LBL_PERFORM_OUTRO_3'				=> 'Total time: ',
	'LBL_PERFORM_OUTRO_4'				=> ' seconds.',
	'LBL_PERFORM_OUTRO_5'				=> 'Approximate memory used: ',
	'LBL_PERFORM_OUTRO_6'				=> ' bytes.',
	'LBL_PERFORM_OUTRO_7'				=> 'Your system is now installed and configured for use.',
	'LBL_PERFORM_REL_META'				=> 'relationship meta ... ',
	'LBL_PERFORM_SUCCESS'				=> 'Success!',
	'LBL_PERFORM_TABLES'				=> 'Creating SuiteCRM application tables, audit tables and relationship metadata',
	'LBL_PERFORM_TITLE'					=> 'Perform Setup',
	'LBL_PRINT'							=> '列印',
	'LBL_REG_CONF_1'					=> 'Please complete the short form below to receive product announcements, training news, special offers and special event invitations from SuiteCRM. We do not sell, rent, share or otherwise distribute the information collected here to third parties.',
	'LBL_REG_CONF_2'					=> 'Your name and email address are the only required fields for registration. All other fields are optional, but very helpful. We do not sell, rent, share, or otherwise distribute the information collected here to third parties.',
	'LBL_REG_CONF_3'					=> 'Thank you for registering. Click on the Finish button to login to SuiteCRM. You will need to log in for the first time using the username "admin" and the password you entered in step 2.',
	'LBL_REG_TITLE'						=> '註冊',
    'LBL_REG_NO_THANKS'                 => 'No Thanks',
    'LBL_REG_SKIP_THIS_STEP'            => 'Skip this Step',
	'LBL_REQUIRED'						=> '* Required field',

    'LBL_SITECFG_ADMIN_Name'            => 'SuiteCRM Application Admin Name',
	'LBL_SITECFG_ADMIN_PASS_2'			=> 'Re-enter SuiteCRM Admin User Password',
	'LBL_SITECFG_ADMIN_PASS_WARN'		=> 'Caution: This will override the admin password of any previous installation.',
	'LBL_SITECFG_ADMIN_PASS'			=> 'SuiteCRM Admin User Password',
	'LBL_SITECFG_APP_ID'				=> 'Application ID',
	'LBL_SITECFG_CUSTOM_ID_DIRECTIONS'	=> 'If selected, you must provide an application ID to override the auto-generated ID. The ID ensures that sessions of one SuiteCRM instance are not used by other instances.  If you have a cluster of SuiteCRM installations, they all must share the same application ID.',
	'LBL_SITECFG_CUSTOM_ID'				=> 'Provide Your Own Application ID',
	'LBL_SITECFG_CUSTOM_LOG_DIRECTIONS'	=> 'If selected, you must specify a log directory to override the default directory for the SuiteCRM log. Regardless of where the log file is located, access to it through a web browser will be restricted via an .htaccess redirect.',
	'LBL_SITECFG_CUSTOM_LOG'			=> 'Use a Custom Log Directory',
	'LBL_SITECFG_CUSTOM_SESSION_DIRECTIONS'	=> 'If selected, you must provide a secure folder for storing SuiteCRM session information. This can be done to prevent session data from being vulnerable on shared servers.',
	'LBL_SITECFG_CUSTOM_SESSION'		=> 'Use a Custom Session Directory for SuiteCRM',
	'LBL_SITECFG_DIRECTIONS'			=> 'Please enter your site configuration information below. If you are unsure of the fields, we suggest that you use the default values.',
	'LBL_SITECFG_FIX_ERRORS'			=> '<b>Please fix the following errors before proceeding:</b>',
	'LBL_SITECFG_LOG_DIR'				=> 'Log Directory',
	'LBL_SITECFG_SESSION_PATH'			=> 'Path to Session Directory<br>(must be writable)',
	'LBL_SITECFG_SITE_SECURITY'			=> 'Select Security Options',
	'LBL_SITECFG_SUGAR_UP_DIRECTIONS'	=> 'If selected, the system will periodically check for updated versions of the application.',
	'LBL_SITECFG_SUGAR_UP'				=> 'Automatically Check For Updates?',
	'LBL_SITECFG_SUGAR_UPDATES'			=> 'SuiteCRM Updates Config',
	'LBL_SITECFG_TITLE'					=> 'Site Configuration',
    'LBL_SITECFG_TITLE2'                => 'Identify Administration User',
    'LBL_SITECFG_SECURITY_TITLE'        => 'Site Security',
	'LBL_SITECFG_URL'					=> 'URL of SuiteCRM Instance',
	'LBL_SITECFG_USE_DEFAULTS'			=> 'Use Defaults?',
	'LBL_SITECFG_ANONSTATS'             => 'Send Anonymous Usage Statistics?',
	'LBL_SITECFG_ANONSTATS_DIRECTIONS'  => 'If selected, SuiteCRM will send <b>anonymous</b> statistics about your installation to SuiteCRM Inc. every time your system checks for new versions. This information will help us better understand how the application is used and guide improvements to the product.',
    'LBL_SITECFG_URL_MSG'               => 'Enter the URL that will be used to access the SuiteCRM instance after installation. The URL will also be used as a base for the URLs in the SuiteCRM application pages. The URL should include the web server or machine name or IP address.',
    'LBL_SITECFG_SYS_NAME_MSG'          => 'Enter a name for your system.  This name will be displayed in the browser title bar when users visit the SuiteCRM application.',
    'LBL_SITECFG_PASSWORD_MSG'          => 'After installation, you will need to use the SuiteCRM admin user (default username = admin) to log in to the SuiteCRM instance.  Enter a password for this administrator user. This password can be changed after the initial login.  You may also enter another admin username to use besides the default value provided.',
    'LBL_SITECFG_COLLATION_MSG'         => 'Select collation (sorting) settings for your system. This settings will create the tables with the specific language you use. In case your language doesn\'t require special settings please use default value.',
    'LBL_SPRITE_SUPPORT'                => 'Sprite Support',
	'LBL_SYSTEM_CREDS'                  => 'System Credentials',
    'LBL_SYSTEM_ENV'                    => 'System Environment',
	'LBL_START'							=> '首頁',
    'LBL_SHOW_PASS'                     => 'Show Passwords',
    'LBL_HIDE_PASS'                     => 'Hide Passwords',
    'LBL_HIDDEN'                        => '<i>(hidden)</i>',
	'LBL_STEP1' => 'Step 1 of 2 - Pre-Installation requirements',
	'LBL_STEP2' => 'Step 2 of 2 - Configuration',
//    'LBL_STEP1'                         => 'Step 1 of 8 - Pre-Installation requirements',
//    'LBL_STEP2'                         => 'Step 2 of 8 - License Agreement',
//    'LBL_STEP3'                         => 'Step 3 of 8 - Installation Type',
//    'LBL_STEP4'                         => 'Step 4 of 8 - Database Selection',
//    'LBL_STEP5'                         => 'Step 5 of 8 - Database Configuration',
//    'LBL_STEP6'                         => 'Step 6 of 8 - Site Configuration',
//    'LBL_STEP7'                         => 'Step 7 of 8 - Confirm Settings',
//    'LBL_STEP8'                         => 'Step 8 of 8 - Installation Successful',
//	'LBL_NO_THANKS'						=> 'Continue to installer',
	'LBL_CHOOSE_LANG'					=> '<b>Choose your language</b>',
	'LBL_STEP'							=> 'Step',
	'LBL_TITLE_WELCOME'					=> 'Welcome to the SuiteCRM ',
	'LBL_WELCOME_1'						=> 'This installer creates the SuiteCRM database tables and sets the configuration variables that you need to start. The entire process should take about ten minutes.',
	'LBL_WELCOME_2'						=> 'For installation documentation, please visit the <a href="http://www.SuiteCRM.com/" target="_blank">SuiteCRM</a>.  <BR><BR> You can also find help from the SuiteCRM Community in the <a href="http://www.SuiteCRM.com/" target="_blank">SuiteCRM Forums</a>.',
    //welcome page variables
    'LBL_TITLE_ARE_YOU_READY'            => 'Are you ready to install?',
    'REQUIRED_SYS_COMP' => 'Required System Components',
    'REQUIRED_SYS_COMP_MSG' =>
                    'Before you begin, please be sure that you have the supported versions of the following system
                      components:<br>
                      <ul>
                      <li> Database/Database Management System (Examples: MariaDB, MySQL or SQL Server)</li>
                      <li> Web Server (Apache, IIS)</li>
                      </ul>
                      Consult the Compatibility Matrix in the Release Notes for
                      compatible system components for the SuiteCRM version that you are installing.<br>',
    'REQUIRED_SYS_CHK' => 'Initial System Check',
    'REQUIRED_SYS_CHK_MSG' =>
                    'When you begin the installation process, a system check will be performed on the web server on which the SuiteCRM files are located in order to
                      make sure the system is configured properly and has all of the necessary components
                      to successfully complete the installation. <br><br>
                      The system checks all of the following:<br>
                      <ul>
                      <li><b>PHP version</b> &#8211; must be compatible
                      with the application</li>
                                        <li><b>Session Variables</b> &#8211; must be working properly</li>
                                            <li> <b>MB Strings</b> &#8211; must be installed and enabled in php.ini</li>

                      <li> <b>Database Support</b> &#8211; must exist for MariaDB, MySQL or SQL Server</li>

                      <li> <b>Config.php</b> &#8211; must exist and must have the appropriate
                                  permissions to make it writeable</li>
					  <li>The following SuiteCRM files must be writeable:<ul><li><b>/custom</li>
<li>/cache</li>
<li>/modules</li>
<li>/upload</b></li></ul></li></ul>
                                  If the check fails, you will not be able to proceed with the installation. An error message will be displayed, explaining why your system
                                  did not pass the check.
                                  After making any necessary changes, you can undergo the system
                                  check again to continue the installation.<br>',
    'REQUIRED_INSTALLTYPE' => 'Typical or Custom install',
    'REQUIRED_INSTALLTYPE_MSG' =>
                    'After the system check is performed, you can choose either
                      the Typical or the Custom installation.<br><br>
                      For both <b>Typical</b> and <b>Custom</b> installations, you will need to know the following:<br>
                      <ul>
                      <li> <b>Type of database</b> that will house the SuiteCRM data <ul><li>Compatible database
                      types: MariaDB, MySQL or SQL Server.<br><br></li></ul></li>
                      <li> <b>Name of the web server</b> or machine (host) on which the database is located
                      <ul><li>This may be <i>localhost</i> if the database is on your local computer or is on the same web server or machine as your SuiteCRM files.<br><br></li></ul></li>
                      <li><b>Name of the database</b> that you would like to use to house the SuiteCRM data</li>
                        <ul>
                          <li> You might already have an existing database that you would like to use. If
                          you provide the name of an existing database, the tables in the database will
                          be dropped during installation when the schema for the SuiteCRM database is defined.</li>
                          <li> If you do not already have a database, the name you provide will be used for
                          the new database that is created for the instance during installation.<br><br></li>
                        </ul>
                      <li><b>Database administrator user name and password</b> <ul><li>The database administrator should be able to create tables and users and write to the database.</li><li>You might need to
                      contact your database administrator for this information if the database is
                      not located on your local computer and/or if you are not the database administrator.<br><br></ul></li></li>
                      <li> <b>SuiteCRM database user name and password</b>
                      </li>
                        <ul>
                          <li> The user may be the database administrator, or you may provide the name of
                          another existing database user. </li>
                          <li> If you would like to create a new database user for this purpose, you will
                          be able to provide a new username and password during the installation process,
                          and the user will be created during installation. </li>
                        </ul></ul><p>

                      For the <b>Custom</b> setup, you might also need to know the following:<br>
                      <ul>
                      <li> <b>URL that will be used to access the SuiteCRM instance</b> after it is installed.
                      This URL should include the web server or machine name or IP address.<br><br></li>
                                  <li> [Optional] <b>Path to the session directory</b> if you wish to use a custom
                                  session directory for SuiteCRM information in order to prevent session data from
                                  being vulnerable on shared servers.<br><br></li>
                                  <li> [Optional] <b>Path to a custom log directory</b> if you wish to override the default directory for the SuiteCRM log.<br><br></li>
                                  <li> [Optional] <b>Application ID</b> if you wish to override the auto-generated
                                  ID that ensures that sessions of one SuiteCRM instance are not used by other instances.<br><br></li>
                                  <li><b>Character Set</b> most commonly used in your locale.<br><br></li></ul>
                                  For more detailed information, please consult the Installation Guide.
                                ',
    'LBL_WELCOME_PLEASE_READ_BELOW' => 'Please read the following important information before proceeding with the installation.  The information will help you determine whether or not you are ready to install the application at this time.',

	'LBL_WELCOME_CHOOSE_LANGUAGE'		=> '<b>Choose your language</b>',
	'LBL_WELCOME_SETUP_WIZARD'			=> 'Setup Wizard',
	'LBL_WELCOME_TITLE_WELCOME'			=> 'Welcome to the SuiteCRM ',
	'LBL_WELCOME_TITLE'					=> 'SuiteCRM Setup Wizard',
	'LBL_WIZARD_TITLE'					=> 'SuiteCRM Setup Wizard: ',
	'LBL_YES'							=> '是',
    'LBL_YES_MULTI'                     => 'Yes - Multibyte',
	// OOTB Scheduler Job Names:
	'LBL_OOTB_WORKFLOW'		=> '使工作流程任務進行',
	'LBL_OOTB_REPORTS'		=> '為計劃任務產生報表',
	'LBL_OOTB_IE'			=> '檢查收件箱',
	'LBL_OOTB_BOUNCE'		=> '每晚處理退回的電子郵件',
    'LBL_OOTB_CAMPAIGN'		=> '每晚批量運行電子郵件市場活動',
	'LBL_OOTB_PRUNE'		=> '每月1號精簡資料庫',
    'LBL_OOTB_TRACKER'		=> 'Prune tracker tables',
    'LBL_OOTB_SUGARFEEDS'   => 'Prune SuiteCRM Feed Tables',
    'LBL_OOTB_SEND_EMAIL_REMINDERS'	=> '運行Email提醒通知',
    'LBL_UPDATE_TRACKER_SESSIONS' => 'Update tracker_sessions table',
    'LBL_OOTB_CLEANUP_QUEUE' => '清除計劃任務隊列',
    'LBL_OOTB_REMOVE_DOCUMENTS_FROM_FS' => 'Removal of documents from filesystem',


    'LBL_PATCHES_TITLE'     => 'Install Latest Patches',
    'LBL_MODULE_TITLE'      => 'Install Language Packs',
    'LBL_PATCH_1'           => 'If you would like to skip this step, click Next.',
    'LBL_PATCH_TITLE'       => 'System Patch',
    'LBL_PATCH_READY'       => 'The following patch(es) are ready to be installed:',
	'LBL_SESSION_ERR_DESCRIPTION'		=> "SuiteCRM relies upon PHP sessions to store important information while connected to this web server.  Your PHP installation does not have the Session information correctly configured.
											<br><br>A common misconfiguration is that the <b>'session.save_path'</b> directive is not pointing to a valid directory.  <br>
											<br> Please correct your <a target=_new href='http://us2.php.net/manual/en/ref.session.php'>PHP configuration</a> in the php.ini file located here below.",
	'LBL_SESSION_ERR_TITLE'				=> 'PHP Sessions Configuration Error',
	'LBL_SYSTEM_NAME'=>'系統名稱',
    'LBL_COLLATION' => 'Collation Settings',
	'LBL_REQUIRED_SYSTEM_NAME'=>'Provide a System Name for the SuiteCRM instance.',
	'LBL_PATCH_UPLOAD' => 'Select a patch file from your local computer',
	'LBL_INCOMPATIBLE_PHP_VERSION' => '需要Php5或以上版本.',
	'LBL_MINIMUM_PHP_VERSION' => 'Minimum Php version required is 5.1.0. Recommended Php version is 5.2.x.',
	'LBL_YOUR_PHP_VERSION' => '(您當前的PHP版本是 ',
	'LBL_RECOMMENDED_PHP_VERSION' =>' Recommended php version is 5.2.x)',
	'LBL_BACKWARD_COMPATIBILITY_ON' => 'Php向後兼容模式已打開. 如果想關閉請將zend.ze1_compatibility_mode設置成off.',
    'LBL_STREAM' => 'PHP allows to use stream',

    'advanced_password_new_account_email' => array(
        'subject' => 'New account information',
        'description' => 'This template is used when the System Administrator sends a new password to a user.',
        'body' => '<div><table border=\\"0\\" cellspacing=\\"0\\" cellpadding=\\"0\\" width="550" align=\\"\\&quot;\\&quot;center\\&quot;\\&quot;\\"><tbody><tr><td colspan=\\"2\\"><p>Here is your account username and temporary password:</p><p>Username : $contact_user_user_name </p><p>Password : $contact_user_user_hash </p><br><p>$config_site_url</p><br><p>After you log in using the above password, you may be required to reset the password to one of your own choice.</p>   </td>         </tr><tr><td colspan=\\"2\\"></td>         </tr> </tbody></table> </div>',
        'txt_body' =>
'
Here is your account username and temporary password:
Username : $contact_user_user_name
Password : $contact_user_user_hash

$config_site_url

After you log in using the above password, you may be required to reset the password to one of your own choice.',
        'name' => 'System-generated password email',
        ),
    'advanced_password_forgot_password_email' => array(
        'subject' => 'Reset your account password',
        'description' => "This template is used to send a user a link to click to reset the user's account password.",
        'body' => '<div><table border=\\"0\\" cellspacing=\\"0\\" cellpadding=\\"0\\" width="550" align=\\"\\&quot;\\&quot;center\\&quot;\\&quot;\\"><tbody><tr><td colspan=\\"2\\"><p>You recently requested on $contact_user_pwd_last_changed to be able to reset your account password. </p><p>Click on the link below to reset your password:</p><p> $contact_user_link_guid </p>  </td>         </tr><tr><td colspan=\\"2\\"></td>         </tr> </tbody></table> </div>',
        'txt_body' =>
'
You recently requested on $contact_user_pwd_last_changed to be able to reset your account password.

Click on the link below to reset your password:

$contact_user_link_guid',
        'name' => 'Forgot Password email',
        ),

	// SMTP settings

	'LBL_WIZARD_SMTP_DESC' => 'Provide the email account that will be used to send emails, such as the assignment notifications and new user passwords. Users will receive emails from SuiteCRM, as sent from the specified email account.',
	'LBL_CHOOSE_EMAIL_PROVIDER'        => '選擇電子郵件提供商：',

	'LBL_SMTPTYPE_GMAIL'                    => 'Gmail',
	'LBL_SMTPTYPE_YAHOO'                    => 'Yahoo! Mail',
	'LBL_SMTPTYPE_EXCHANGE'                 => 'Microsoft Exchange',
	'LBL_SMTPTYPE_OTHER'                  => '其它',
	'LBL_MAIL_SMTP_SETTINGS'           => 'SMTP伺服器設置',
	'LBL_MAIL_SMTPSERVER'				=> 'SMTP伺服器地址:',
	'LBL_MAIL_SMTPPORT'					=> 'SMTP埠:',
	'LBL_MAIL_SMTPAUTH_REQ'				=> '是否使用SMTP認證？',
	'LBL_EMAIL_SMTP_SSL_OR_TLS'         => '以SSL或TLS啟用SMTP',
	'LBL_GMAIL_SMTPUSER'					=> 'Gmail地址:',
	'LBL_GMAIL_SMTPPASS'					=> 'Gmail密碼:',
	'LBL_ALLOW_DEFAULT_SELECTION'           => '允許用戶使用該郵件帳戶外發電子郵件：',
	'LBL_ALLOW_DEFAULT_SELECTION_HELP'          => 'When this option is selected, all users will be able to send emails using the same outgoing mail account used to send system notifications and alerts.  If the option is not selected, users can still use the outgoing mail server after providing their own account information.',

	'LBL_YAHOOMAIL_SMTPPASS'					=> 'Yahoo! Mail 密碼:',
	'LBL_YAHOOMAIL_SMTPUSER'					=> 'Yahoo! Mail:',

	'LBL_EXCHANGE_SMTPPASS'					=> 'Exchange密碼:',
	'LBL_EXCHANGE_SMTPUSER'					=> 'Exchange用戶名:',
	'LBL_EXCHANGE_SMTPPORT'					=> 'Exchange伺服器埠:',
	'LBL_EXCHANGE_SMTPSERVER'				=> 'Exchange伺服器:',


	'LBL_MAIL_SMTPUSER'					=> 'SMTP用戶名:',
	'LBL_MAIL_SMTPPASS'					=> 'SMTP密碼:',

	// Branding

	'LBL_WIZARD_SYSTEM_TITLE' => '商標',
	'LBL_WIZARD_SYSTEM_DESC' => 'Provide your organization\'s name and logo in order to brand your SuiteCRM.',
	'SYSTEM_NAME_WIZARD'=>'姓名:',
	'SYSTEM_NAME_HELP'=>'顯示在瀏覽器標題欄的系統名稱',
	'NEW_LOGO'=>'上傳新標識(212x40)',
	'NEW_LOGO_HELP'=>'圖象文件格式一定不是 .png就是 .jpg.<BR>推薦尺寸為 212x40 px.',
	'COMPANY_LOGO_UPLOAD_BTN' => '上傳',
	'CURRENT_LOGO'=>'當前使用的標識',
    'CURRENT_LOGO_HELP'=>'本標識將顯示在 Sugar 程序的左上角。',

	// System Local Settings


	'LBL_LOCALE_TITLE' => 'System Locale Settings',
	'LBL_WIZARD_LOCALE_DESC' => 'Specify how you would like data in SuiteCRM to be displayed, based on your geographical location. The settings you provide here will be the default settings. Users will be able set their own preferences.',
	'LBL_DATE_FORMAT' => '日期格式:',
	'LBL_TIME_FORMAT' => '時間格式:',
		'LBL_TIMEZONE' => '時區:',
	'LBL_LANGUAGE'=>'組標簽語言：',
	'LBL_CURRENCY'=>'貨幣',
	'LBL_CURRENCY_SYMBOL'=>'Currency Symbol:',
	'LBL_CURRENCY_ISO4217' => 'ISO 4217 Currency Code:',
	'LBL_NUMBER_GROUPING_SEP' => '千位分隔符',
	'LBL_DECIMAL_SEP' => '數字標記',
	'LBL_NAME_FORMAT' => 'Name Format:',
	'UPLOAD_LOGO' => 'Please wait, logo uploading..',
	'ERR_UPLOAD_FILETYPE' => 'File type do not allowed, please upload a jpeg or png.',
	'ERR_LANG_UPLOAD_UNKNOWN' => 'Unknown file upload error occured.',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_INI_SIZE' => '上傳文件大小超出了php.ini文件中變數upload_max_filesize指定的值。',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_FORM_SIZE' => '上傳文件大小超出了HTML表單中變數MAX_FILE_SIZE指定的值。',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_PARTIAL' => '只上傳了部分文件。',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_NO_FILE' => '沒有上傳文件。',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_NO_TMP_DIR' => '缺少臨時文件夾。',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_CANT_WRITE' => '寫入磁碟文件錯誤。',
	'ERR_UPLOAD_FILE_UPLOAD_ERR_EXTENSION' => 'A PHP extension stopped the file upload. PHP does not provide a way to ascertain which extension caused the file upload to stop.',

	'LBL_INSTALL_PROCESS' => 'Install...',

	'LBL_EMAIL_ADDRESS' => '郵件地址:',
	'ERR_ADMIN_EMAIL' => 'Administrator Email Address is incorrect.',
	'ERR_SITE_URL' => 'Site URL is required.',

	'STAT_CONFIGURATION' => 'Configuration relationships...',
	'STAT_CREATE_DB' => 'Create database...',
	//'STAT_CREATE_DB_TABLE' => 'Create database... (table: %s)',
	'STAT_CREATE_DEFAULT_SETTINGS' => 'Create default settings...',
	'STAT_INSTALL_FINISH' => 'Install finish...',
	'STAT_INSTALL_FINISH_LOGIN' => 'Installation process finished, <a href="%s">please log in...</a>',
	'LBL_LICENCE_TOOLTIP' => 'Please accept license first',

	'LBL_MORE_OPTIONS_TITLE' => 'More options',
	'LBL_START' => '首頁',


);

?>
