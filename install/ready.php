<?php

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

include_once __DIR__ . '/../include/Imap/ImapHandlerFactory.php';

if (!isset($install_script) || !$install_script) {
    die($mod_strings['ERR_NO_DIRECT_SCRIPT']);
}
// $mod_strings come from calling page.


// System Environment
$envString = '

	   <h3>'.$mod_strings['LBL_SYSTEM_ENV'].'</h3>';

// PHP VERSION
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_PHPVER'].'</b> '.constant('PHP_VERSION').'</p>';


//Begin List of already known good variables.  These were checked during the initial sys check
// XML Parsing
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_XML'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';



// mbstrings

$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_MBSTRING'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';

// config.php
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_CONFIG'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';

// custom dir


$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_CUSTOM'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';


// modules dir
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_MODULE'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';

// upload dir
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_UPLOAD'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';

// data dir

$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_DATA'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';

// cache dir
$error_found = true;
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_CACHE'].'</b> '.$mod_strings['LBL_CHECKSYS_OK'].'</p>';
// End already known to be good

// memory limit
$memory_msg     = "";
// CL - fix for 9183 (if memory_limit is enabled we will honor it and check it; otherwise use unlimited)
$memory_limit = ini_get('memory_limit');
if (empty($memory_limit)) {
    $memory_limit = "-1";
}
if (!defined('SUGARCRM_MIN_MEM')) {
    define('SUGARCRM_MIN_MEM', 64*1024*1024);
}
$sugarMinMem = constant('SUGARCRM_MIN_MEM');
// logic based on: http://us2.php.net/manual/en/ini.core.php#ini.memory-limit
if ($memory_limit == "") {          // memory_limit disabled at compile time, no memory limit
    $memory_msg = "<b>{$mod_strings['LBL_CHECKSYS_MEM_OK']}</b>";
} elseif ($memory_limit == "-1") {   // memory_limit enabled, but set to unlimited
    $memory_msg = (string)($mod_strings['LBL_CHECKSYS_MEM_UNLIMITED']);
} else {
    $mem_display = $memory_limit;
    preg_match('/^\s*([0-9.]+)\s*([KMGTPE])B?\s*$/i', $memory_limit, $matches);
    $num = (float)$matches[1];
    // Don't break so that it falls through to the next case.
    switch (strtoupper($matches[2])) {
        case 'G':
            $num = $num * 1024;
            // no break
        case 'M':
            $num = $num * 1024;
            // no break
        case 'K':
            $num = $num * 1024;
    }
    $memory_limit_int = (int)$num;
    $SUGARCRM_MIN_MEM = (int) constant('SUGARCRM_MIN_MEM');
    if ($memory_limit_int < constant('SUGARCRM_MIN_MEM')) {
        // Bug59667: The string ERR_CHECKSYS_MEM_LIMIT_2 already has 'M' in it,
        // so we divide the constant by 1024*1024.
        $min_mem_in_megs = constant('SUGARCRM_MIN_MEM')/(1024*1024);
        $memory_msg = "<span class='stop'><b>$memory_limit{$mod_strings['ERR_CHECKSYS_MEM_LIMIT_1']}" . $min_mem_in_megs . "{$mod_strings['ERR_CHECKSYS_MEM_LIMIT_2']}</b></span>";
        $memory_msg = str_replace('$memory_limit', $mem_display, $memory_msg);
    } else {
        $memory_msg = "{$mod_strings['LBL_CHECKSYS_OK']} ({$memory_limit})";
    }
}

$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_MEM'].'</strong></b> '.$memory_msg.'</p>';

// zlib
if (function_exists('gzclose')) {
    $zlibStatus = (string)($mod_strings['LBL_CHECKSYS_OK']);
} else {
    $zlibStatus = "<span class='stop'><b>{$mod_strings['ERR_CHECKSYS_ZLIB']}</b></span>";
}
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_ZLIB'].'</b> '.$zlibStatus.'</p>';

// zip
if (class_exists("ZipArchive")) {
    $zipStatus = (string)($mod_strings['LBL_CHECKSYS_OK']);
} else {
    $zipStatus = "<span class='stop'><b>{$mod_strings['ERR_CHECKSYS_ZIP']}</b></span>";
}
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_ZIP'].'</b> '.$zipStatus.'</p>';

// PCRE
if (defined('PCRE_VERSION')) {
    if (version_compare(PCRE_VERSION, '7.0') < 0) {
        $pcreStatus = "<span class='stop'><b>{$mod_strings['ERR_CHECKSYS_PCRE_VER']}</b></span>";
    } else {
        $pcreStatus = (string)($mod_strings['LBL_CHECKSYS_OK']);
    }
} else {
    $pcreStatus = "<span class='stop'><b>{$mod_strings['ERR_CHECKSYS_PCRE']}</b></span>";
}
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_PCRE'].'</b> '.$pcreStatus.'</p>';

// imap
$imapFactory = new ImapHandlerFactory();
if ($imapFactory->areAllHandlersAvailable()) {
    $imapStatus = (string)($mod_strings['LBL_CHECKSYS_OK']);
} else {
    $imapStatus = "<span class='stop'><b>{$mod_strings['ERR_CHECKSYS_IMAP']}</b></span>";
}

$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_IMAP'].'</b> '.$imapStatus.'</p>';


// cURL
if (function_exists('curl_init')) {
    $curlStatus = (string)($mod_strings['LBL_CHECKSYS_OK']);
} else {
    $curlStatus = "<span class='stop'><b>{$mod_strings['ERR_CHECKSYS_CURL']}</b></span>";
}

$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_CURL'].'</b> '.$curlStatus.'</p>';


//CHECK UPLOAD FILE SIZE
$upload_max_filesize = ini_get('upload_max_filesize');
$upload_max_filesize_bytes = return_bytes($upload_max_filesize);
if (!defined('SUGARCRM_MIN_UPLOAD_MAX_FILESIZE_BYTES')) {
    define('SUGARCRM_MIN_UPLOAD_MAX_FILESIZE_BYTES', 6 * 1024 * 1024);
}

if ($upload_max_filesize_bytes >= constant('SUGARCRM_MIN_UPLOAD_MAX_FILESIZE_BYTES')) {
    $fileMaxStatus = "{$mod_strings['LBL_CHECKSYS_OK']}</font>";
} else {
    $fileMaxStatus = "<span class='stop'><b>{$mod_strings['ERR_UPLOAD_MAX_FILESIZE']}</font></b></span>";
}

$envString .='<p><b>'.$mod_strings['LBL_UPLOAD_MAX_FILESIZE_TITLE'].'</b> '.$fileMaxStatus.'</p>';

//CHECK Sprite support
if (function_exists('imagecreatetruecolor')) {
    $spriteSupportStatus = "{$mod_strings['LBL_CHECKSYS_OK']}</font>";
} else {
    $spriteSupportStatus = "<span class='stop'><b>{$mod_strings['ERROR_SPRITE_SUPPORT']}</b></span>";
}
$envString .='<p><b>'.$mod_strings['LBL_SPRITE_SUPPORT'].'</b> '.$spriteSupportStatus.'</p>';

// Suhosin allow to use upload://
if (UploadStream::getSuhosinStatus() == true || (strpos(ini_get('suhosin.perdir'), 'e') !== false && strpos((string) $_SERVER["SERVER_SOFTWARE"], 'Microsoft-IIS') === false)) {
    $suhosinStatus = (string)($mod_strings['LBL_CHECKSYS_OK']);
} else {
    $suhosinStatus = "<span class='stop'><b>{$app_strings['ERR_SUHOSIN']}</b></span>";
}
$envString .= "<p><b>{$mod_strings['LBL_STREAM']} (" . UploadStream::STREAM_NAME . "://)</b> " . $suhosinStatus . "</p>";

// PHP.ini
$phpIniLocation = get_cfg_var("cfg_file_path");
$envString .='<p><b>'.$mod_strings['LBL_CHECKSYS_PHP_INI'].'</b> '.$phpIniLocation.'</p>';

$out =<<<EOQ

<div id="syscred">

EOQ;

$out .= $envString;

$out .=<<<EOQ

</div>
<div class="clear"></div>
EOQ;

$sugar_config_defaults = get_sugar_config_defaults();

// CRON Settings
if (!isset($sugar_config['default_language'])) {
    $sugar_config['default_language'] = $_SESSION['default_language'];
}
if (!isset($sugar_config['cache_dir'])) {
    $sugar_config['cache_dir'] = $sugar_config_defaults['cache_dir'];
}
if (!isset($sugar_config['site_url'])) {
    $sugar_config['site_url'] = $_SESSION['setup_site_url'];
}
if (!isset($sugar_config['translation_string_prefix'])) {
    $sugar_config['translation_string_prefix'] = $sugar_config_defaults['translation_string_prefix'];
}
$mod_strings_scheduler = return_module_language($GLOBALS['current_language'], 'Schedulers');
$error = '';

if (!isset($_SERVER['Path'])) {
    $_SERVER['Path'] = getenv('Path');
}
if (is_windows()) {
    if (isset($_SERVER['Path']) && !empty($_SERVER['Path'])) { // IIS IUSR_xxx may not have access to Path or it is not set
        if (!strpos((string) $_SERVER['Path'], 'php')) {
//        $error = '<em>'.$mod_strings_scheduler['LBL_NO_PHP_CLI'].'</em>';
        }
    }
    $cronString = '<p><b>'.$mod_strings_scheduler['LBL_CRON_WINDOWS_DESC'].'</b><br>
						cd /D '.realpath('./').'<br>
						php.exe -f cron.php
						<br>'.$error.'</p>
			   ';
} else {
    if (isset($_SERVER['Path']) && !empty($_SERVER['Path'])) { // some Linux servers do not make this available
        if (!strpos((string) $_SERVER['PATH'], 'php')) {
//        $error = '<em>'.$mod_strings_scheduler['LBL_NO_PHP_CLI'].'</em>';
        }
    }
    require_once 'install/install_utils.php';
    $webServerUser = getRunningUser();
    if ($webServerUser == '') {
        $webServerUser = '<web_server_user>';
    }
    $cronString = '<p><b>'.$mod_strings_scheduler['LBL_CRON_INSTRUCTIONS_LINUX'].'</b><br> '.$mod_strings_scheduler['LBL_CRON_LINUX_DESC1'].'<br>
                        <span style=\'background-color:#dfdfdf\'>sudo crontab -e -u '.$webServerUser.'</span><br> '.$mod_strings_scheduler['LBL_CRON_LINUX_DESC2'].'<br>
						<span style=\'background-color:#dfdfdf\'>*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;
						cd '.realpath('./').'; php -f cron.php > /dev/null 2>&1
						</span><br>'.$mod_strings_scheduler['LBL_CRON_LINUX_DESC3'].'
                        <br><br><hr><br>'.$error.'</p>
              ';
}

$out .= $cronString;

$sysEnv = $out;

///////////////////////////////////////////////////////////////////////////////
////    START OUTPUT

$langHeader = get_language_header();
$out = <<<EOQ
<!DOCTYPE HTML>
<html {$langHeader}>
<head>
   <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
   <meta http-equiv="Content-Style-Type" content="text/css">
   <meta content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0' name='viewport' />
   <title>{$mod_strings['LBL_WIZARD_TITLE']} {$mod_strings['LBL_TITLE_ARE_YOU_READY']}</title>   <link REL="SHORTCUT ICON" HREF="include/images/sugar_icon.ico">
   <link rel="stylesheet" href="install/install2.css" type="text/css">
   <link rel="stylesheet" href="themes/SuiteP/css/responsiveslides.css" type="text/css">
   <link rel="stylesheet" href="themes/SuiteP/css/themes.css" type="text/css">
   <link rel="stylesheet" href="themes/SuiteP/css/fontello.css">
   <link rel="stylesheet" href="themes/SuiteP/css/animation.css"><!--[if IE 7]><link rel="stylesheet" href="css/fontello-ie7.css"><![endif]-->
   <script src="include/javascript/jquery/jquery-min.js"></script>
   <script src="themes/SuiteP/js/responsiveslides.min.js"></script>
</head>
<body>
    <!--SuiteCRM installer-->
    <div id="install_container">
    <div id="install_box">
        <header id="install_header">
                    <div id="steps">
                        <p>{$mod_strings['LBL_STEP1']}</p>
                        <i class="icon-progress-0" id="complete"></i>
                        <i class="icon-progress-1"></i>
                        <i class="icon-progress-2"></i>
                    </div>
            <!--
            <div id="steps"><p>{$mod_strings['LBL_STEP1']}</p><i class="icon-progress-0"></i><i class="icon-progress-1"></i><i class="icon-progress-2"></i><i class="icon-progress-3"></i><i class="icon-progress-4"></i><i class="icon-progress-5"></i><i class="icon-progress-6"></i><i class="icon-progress-7"></i></div>
            -->
            <div class="install_img"><a href="https://suitecrm.com" target="_blank"><img src="{$sugar_md}" alt="SuiteCRM"></a></div>
        </header>
	        <form action="install.php" method="post" name="form" id="form">
	        	<div id="install_content">
	        	{$sysEnv}

	        	<!--

			    <h3>{$mod_strings['LBL_TITLE_ARE_YOU_READY']}</h3>
				<p><strong>{$mod_strings['LBL_WELCOME_PLEASE_READ_BELOW']}</strong></p>
				<span onclick="showtime('sys_comp');" style="cursor:pointer;cursor:hand">
				    <span id='basic_sys_comp'><img alt="{$mod_strings['LBL_BASIC_SEARCH']}" src="themes/default/images/basic_search.gif" border="0"></span>
				    <span id='adv_sys_comp' style='display:none'><img alt="{$mod_strings['LBL_ADVANCED_SEARCH']}" src="themes/default/images/advanced_search.gif" border="0"></span>
				    &nbsp;{$mod_strings['REQUIRED_SYS_COMP']}
				</span>
				<div id='sys_comp' >{$mod_strings['REQUIRED_SYS_COMP_MSG']}</div>
				<span onclick="showtime('sys_check');" style="cursor:pointer;cursor:hand">
				    <span id='basic_sys_check'><img alt="{$mod_strings['LBL_BASIC_SEARCH']}" src="themes/default/images/basic_search.gif" border="0"></span>
					<span id='adv_sys_check' style='display:none'><img alt="{$mod_strings['LBL_ADVANCED_SEARCH']}" src="themes/default/images/advanced_search.gif" border="0"></span>
					&nbsp;{$mod_strings['REQUIRED_SYS_CHK']}
				</span>
				<div id='sys_check' >{$mod_strings['REQUIRED_SYS_CHK_MSG']}</div>
				<span onclick="showtime('installType');" style="cursor:pointer;cursor:hand">
					<span id='basic_installType'><img alt="{$mod_strings['LBL_BASIC_TYPE']}" src="themes/default/images/basic_search.gif" border="0"></span>
					<span id='adv_installType' style='display:none'><img alt="{$mod_strings['LBL_ADVANCED_TYPE']}" src="themes/default/images/advanced_search.gif" border="0"></span>
					&nbsp;{$mod_strings['REQUIRED_INSTALLTYPE']}
				</span>
				<div id='installType' >{$mod_strings['REQUIRED_INSTALLTYPE_MSG']}</div>
				<hr>

				-->

				</div>
                <div id="installcontrols">
				    <input type="hidden" name="current_step" value="{$next_step}">
					<input class="acceptButton" type="button" name="goto" value="{$mod_strings['LBL_BACK']}" id="button_back_ready" onclick="document.getElementById('form').submit();" />
			        <input class="button" type="submit" name="goto" value="{$mod_strings['LBL_NEXT']}" id="button_next2" />
			    </div>
            </form>
    <script>
        function showtime(div){

            if(document.getElementById(div).style.display == ''){
                document.getElementById(div).style.display = 'none';
                document.getElementById('adv_'+div).style.display = '';
                document.getElementById('basic_'+div).style.display = 'none';
            }else{
                document.getElementById(div).style.display = '';
                document.getElementById('adv_'+div).style.display = 'none';
                document.getElementById('basic_'+div).style.display = '';
            }

        }
    </script>
</div>
<footer id="install_footer">
    <p id="footer_links"><a href="https://suitecrm.com" target="_blank">Visit suitecrm.com</a> | <a href="https://suitecrm.com/suitecrm/forum" target="_blank">Support Forums</a> | <a href="https://docs.suitecrm.com/admin/installation-guide/" target="_blank">Installation Guide</a> | <a href="LICENSE.txt" target="_blank">License</a>
</footer>
</div>
</body>
</html>
EOQ;
echo $out;
