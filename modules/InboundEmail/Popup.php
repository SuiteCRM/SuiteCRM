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

/**

 * Description:
 */
// cn: bug 6078: zlib breaks test-settings
$iniError = '';
if (ini_get('zlib.output_compression') == 1) { // ini_get() returns 1/0, not value
    if (!ini_set('zlib.output_compression', 'Off')) { // returns False on failure
        $iniError = $mod_strings['ERR_INI_ZLIB'];
    }
}

// hack to allow "&", "%" and "+" through a $_GET var
// set by ie_test_open_popup() javascript call
foreach ($_REQUEST as $k => $v) {
    $v = str_replace('::amp::', '&', (string) $v);
    $v = str_replace('::plus::', '+', $v);
    $v = str_replace('::percent::', '%', $v);
    $_REQUEST[$k] = $v;
}

if (ob_get_level() > 0) {
    ob_end_clean();
}

if (ob_get_level() < 1) {
    ob_start();
}


require_once('modules/InboundEmail/language/'.$current_language.'.lang.php');
global $theme;

$title                = '';
$msg                = '';
$tls                = '';
$cert                = '';
$ssl                = '';
$notls                = '';
$novalidate_cert    = '';
$useSsl                = false;

///////////////////////////////////////////////////////////////////////////////
////	TITLES

$popupBoolean = false;
if (isset($_REQUEST['target']) && $_REQUEST['target'] == 'Popup') {
    $popupBoolean = true;
}
if (isset($_REQUEST['target1']) && $_REQUEST['target1'] == 'Popup') {
    $popupBoolean = true;
}

if ($popupBoolean) {
    $title = '';
    $msg = $mod_strings['LBL_TEST_WAIT_MESSAGE'];
}

if (isset($_REQUEST['ssl']) && ($_REQUEST['ssl'] == "true" || $_REQUEST['ssl'] == 1)) {
    $msg .= $mod_strings['LBL_FIND_SSL_WARN'];
    $useSsl = true;
}

$ie                 = BeanFactory::newBean('InboundEmail');
if (!empty($_REQUEST['ie_id'])) {
    $ie->retrieve($_REQUEST['ie_id']);
}
$ie->email_user     = $_REQUEST['email_user'];
$ie->server_url     = $_REQUEST['server_url'];
$ie->port           = $_REQUEST['port'];
$ie->protocol       = $_REQUEST['protocol'];

if (!empty($_REQUEST['auth_type'])){
    $ie->auth_type = $_REQUEST['auth_type'];
}

//Bug 23083.Special characters in email password results in IMAP authentication failure
if (!empty($_REQUEST['email_password'])) {
    $ie->email_password = html_entity_decode((string) $_REQUEST['email_password'], ENT_QUOTES);
    $ie->email_password = str_rot13($ie->email_password);
}
$ie->mailbox        = 'INBOX';
if ($_REQUEST['externalOauthConnectionName']){
    $ie->external_oauth_connection_name = $_REQUEST['externalOauthConnectionName'];
}
if ($_REQUEST['externalOauthConnectionId']){
    $ie->external_oauth_connection_id = $_REQUEST['externalOauthConnectionId'];
}
if (!empty($_REQUEST['connection_string'])) {
    $ie->connection_string = urldecode($_REQUEST['connection_string'] ?? '');
}

if ($popupBoolean) {
    $msg = $ie->connectMailserver(true);
}

////	END TITLES
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
////	COMMON CODE
echo /*'
<HTML>
    <HEAD>
        <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
        <title>SugarCRM - Commercial Open Source CRM</title>
        '.SugarThemeRegistry::current()->getCSS().'
        <script type="text/javascript">
                function setMailbox(box) {
                    var mb = opener.document.getElementById("mailbox");
                    mb.value = box;
                }
        </script>

    </HEAD>
    <body style="margin: 10px">*/'
	<p>
		<table width="100%" cellpadding="0" cellspacing="0" border="0">
			<tr>
				<td>
					'.SugarThemeRegistry::current()->getImage('h3Arrow', 'border="0"', 11, 11, ".gif", $mod_strings['LBL_POPUP_TITLE']).'
				</td>
				<td>
					<h3>&nbsp;'.$title.'</h3>
				</td>
			</tr>
			<tr>
				<td></td>
				<td valign="top">
					<div id="testSettingsMsg">
					'.$msg.'
					</div>
					<div id="testSettingsTic"></div>
					<div id="testSettingsErr">'.$iniError.'</div>
				</td>
			</tr>';

if ($popupBoolean) {
    /*	echo '	<tr><td>&nbsp;</td><td>&nbsp;</td></tr><tr>
    				<td></td>
    				<td>
    					<form name="form">
    					<input name="close" type="button" title="'.$mod_strings['LBL_CLOSE_POPUP'].'"  value="    '.$mod_strings['LBL_CLOSE_POPUP'].'    " onClick="window.close()">
    					</form>
    				</td>
    			</tr>';*/
}

echo '	</table>';


////	END COMMON CODE
///////////////////////////////////////////////////////////////////////////////
