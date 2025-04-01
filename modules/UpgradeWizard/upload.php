<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


logThis('At upload.php');

//set the upgrade progress status.
set_upgrade_progress('upload', 'in_progress');


$stop = true; // flag to show "next"
$run = isset($_REQUEST['run']) ? $_REQUEST['run'] : '';
$out = '';

if (file_exists('ModuleInstall/PackageManager/PackageManagerDisplay.php')) {
    require_once('ModuleInstall/PackageManager/PackageManagerDisplay.php');
}

///////////////////////////////////////////////////////////////////////////////
////	UPLOAD FILE PROCESSING
switch ($run) {
    case 'upload':
        logThis('running upload');
        $perform = false;
        $tempFile = '';

        if (isset($_REQUEST['release_id']) && $_REQUEST['release_id'] != "") {
            require_once('ModuleInstall/PackageManager/PackageManager.php');
            $pm = new PackageManager();
            $tempFile = '';
            $perform = false;
            if (!empty($_SESSION['ML_PATCHES'])) {
                $release_map = $_SESSION['ML_PATCHES'][$_REQUEST['release_id']];
                if (!empty($release_map)) {
                    $tempFile = $pm->download($release_map['category_id'], $release_map['package_id'], $_REQUEST['release_id']);
                    $perform = true;
                    if ($release_map['type'] != 'patch') {
                        $pm->performSetup($tempFile, $release_map['type'], false);
                        header('Location: index.php?module=Administration&action=UpgradeWizard&view=module');
                    }
                }
            }

            $base_filename = urldecode($tempFile);
        } else {
            $upload = new UploadFile('upgrade_zip');
            /* Bug 51722 - Cannot Upload Upgrade File if System Settings Are Not Sufficient, Just Make sure that we can
            upload no matter what, set the default to 60M */
            global $sugar_config;
            $upload_maxsize_backup = $sugar_config['upload_maxsize'];
            $sugar_config['upload_maxsize'] = 60000000;
            /* End Bug 51722 */
            if (!$upload->confirm_upload()) {
                logThis('ERROR: no file uploaded!');
                echo $mod_strings['ERR_UW_NO_FILE_UPLOADED'];
                $error = $upload->get_upload_error();
                // add PHP error if isset
                if ($error) {
                    $out = "<b><span class='error'>{$mod_strings['ERR_UW_PHP_FILE_ERRORS'][$error]}</span></b><br />";
                }
            } else {
                $tempFile = "upload://".$upload->get_stored_file_name();
                if (!$upload->final_move($tempFile)) {
                    logThis('ERROR: could not move temporary file to final destination!');
                    unlinkUWTempFiles();
                    $out = "<b><span class='error'>{$mod_strings['ERR_UW_NOT_VALID_UPLOAD']}</span></b><br />";
                } else {
                    logThis('File uploaded to '.$tempFile);
                    $base_filename = urldecode(basename($tempFile));
                    $perform = true;
                }
            }
            /* Bug 51722 - Restore the upload size in the config */
            $sugar_config['upload_maxsize'] = $upload_maxsize_backup;
            /* End Bug 51722 */
        }
        if ($perform) {
            $manifest_file = extractManifest($tempFile);

            if (is_file($manifest_file)) {

                //SCAN THE MANIFEST FILE TO MAKE SURE NO COPIES OR ANYTHING ARE HAPPENING IN IT
                require_once __DIR__ . '/../../ModuleInstall/ModuleScanner.php';

                $ms = new ModuleScanner();
                $ms->lockConfig();
                $fileIssues = $ms->scanFile($manifest_file);
                if (!empty($fileIssues)) {
                    $out .= '<h2>' . translate('ML_MANIFEST_ISSUE', 'Administration') . '</h2><br>';
                    $out .= $ms->getIssuesLog();
                    break;
                }

                list($manifest, $installdefs) = MSLoadManifest($manifest_file);
                if ($ms->checkConfig($manifest_file)) {
                    $out .= '<h2>' . translate('ML_MANIFEST_ISSUE', 'Administration') . '</h2><br>';
                    $out .= $ms->getIssuesLog();
                    break;
                }

                $error = validate_manifest($manifest);
                if (!empty($error)) {
                    $out = "<b><span class='error'>{$error}</span></b><br />";
                    break;
                }

                $upgrade_zip_type = $manifest['type'];

                // exclude the bad permutations
                if ($upgrade_zip_type != "patch") {
                    logThis('ERROR: incorrect patch type found: '.$upgrade_zip_type);
                    unlinkUWTempFiles();
                    $out = "<b><span class='error'>{$mod_strings['ERR_UW_ONLY_PATCHES']}</span></b><br />";
                    break;
                }

                sugar_mkdir("$base_upgrade_dir/$upgrade_zip_type", 0775, true);
                $target_path = "$base_upgrade_dir/$upgrade_zip_type/$base_filename";
                $target_manifest = remove_file_extension($target_path) . "-manifest.php";

                if (isset($manifest['icon']) && $manifest['icon'] != "") {
                    logThis('extracting icons.');
                    $icon_location = extractFile($tempFile, $manifest['icon']);
                    $path_parts = pathinfo((string) $icon_location);
                    copy($icon_location, remove_file_extension($target_path) . "-icon." . pathinfo((string) $icon_location, PATHINFO_EXTENSION));
                }

                if (rename($tempFile, $target_path)) {
                    logThis('copying manifest.php to final destination.');
                    copy($manifest_file, $target_manifest);
                    $out .= "<b>{$base_filename} {$mod_strings['LBL_UW_FILE_UPLOADED']}.</b><br>\n";
                } else {
                    logThis('ERROR: cannot copy manifest.php to final destination.');
                    $out .= "<b><span class='error'>{$mod_strings['ERR_UW_UPLOAD_ERR']}</span></b><br />";
                    break;
                }
            } else {
                logThis('ERROR: no manifest.php file found!');
                unlinkUWTempFiles();
                $out = "<b><span class='error'>{$mod_strings['ERR_UW_NO_MANIFEST']}</span></b><br />";
                break;
            }
            $_SESSION['install_file'] = basename((string) $tempFile);
            logThis('zip file moved to ['.$_SESSION['install_file'].']');
            //rrs serialize manifest for saving in the db
            $serial_manifest = array();
            $serial_manifest['manifest'] = (isset($manifest) ? $manifest : '');
            $serial_manifest['installdefs'] = (isset($installdefs) ? $installdefs : '');
            $serial_manifest['upgrade_manifest'] = (isset($upgrade_manifest) ? $upgrade_manifest : '');
            $_SESSION['install_manifest'] = base64_encode(serialize($serial_manifest));
        }

        if (!empty($tempFile)) {
            upgradeUWFiles($target_path);
            //set the upgrade progress status. actually it should be set when a file is uploaded
            set_upgrade_progress('upload', 'done');
        }

    break; // end 'upload'

    case 'delete':
        logThis('running delete');

        if (!isset($_REQUEST['install_file']) || ($_REQUEST['install_file'] == "")) {
            logThis('ERROR: trying to delete non-existent file: ['.$_REQUEST['install_file'].']');
            $error = $mod_strings['ERR_UW_NO_FILE_UPLOADED'];
        }

        // delete file in upgrades/patch
        $delete_me = 'upload://upgrades/patch/'.basename(urldecode($_REQUEST['install_file']));
        if (@unlink($delete_me)) {
            logThis('unlinking: '.$delete_me);
            $out = basename($delete_me).$mod_strings['LBL_UW_FILE_DELETED'];
        } else {
            logThis('ERROR: could not delete ['.$delete_me.']');
            $error = $mod_strings['ERR_UW_FILE_NOT_DELETED'].$delete_me;
        }

        if (!empty($error)) {
            $out = "<b><span class='error'>{$error}</span></b><br />";
        }

        unlinkUWTempFiles();
        //set the upgrade progress status. actually it should be set when a file is uploaded
        set_upgrade_progress('upload', 'in_progress');

    break;
}
////	END UPLOAD FILE PROCESSING FORM
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////	READY TO INSTALL UPGRADES
$validReturn = getValidPatchName();
$ready = $validReturn['ready'];
$disabled = $validReturn['disabled'];
////	END READY TO INSTALL UPGRADES
///////////////////////////////////////////////////////////////////////////////

if (isset($_SESSION['install_file']) && !empty($_SESSION['install_file']) && is_file($_SESSION['install_file'])) {
    $stop = false;
} else {
    $stop = true;
}
if ($stop == false) {
    set_upgrade_progress('upload', 'done');
}
$frozen = $out;

if (!$stop) {
    if (!empty($GLOBALS['top_message'])) {
        $GLOBALS['top_message'] .= "<br />";
    } else {
        $GLOBALS['top_message'] = '';
    }
    $GLOBALS['top_message'] .= "<b>{$mod_strings['LBL_UPLOAD_SUCCESS']}</b>";
} else {
    if (!$frozen) {
        $GLOBALS['top_message'] .= "<br />";
    } else {
        $GLOBALS['top_message'] = "<b>{$frozen}</b>";
    }
}

///////////////////////////////////////////////////////////////////////////////
////	UPLOAD FORM
$form = '';
if (empty($GLOBALS['sugar_config']['disable_uw_upload'])) {
    $form =<<<eoq
<form name="the_form" id='the_form' enctype="multipart/form-data" action="index.php" method="post">
	<input type="hidden" name="module" value="UpgradeWizard">
	<input type="hidden" name="action" value="index">
	<input type="hidden" name="step" value="{$_REQUEST['step']}">
	<input type="hidden" name="run" value="upload">
<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
<tr><td>
	<table width="450" border="0" cellspacing="0" cellpadding="0">
		<tr>
			<td>
				{$mod_strings['LBL_SELECT_FILE']}
				<input type="file" onchange="uploadCheck();" name="upgrade_zip" id="upgrade_zip" size="40" />
			</td>
			<td valign="bottom">&nbsp;
				<input id="upload_button" class='button' type=button
						{$disabled}
						disabled="disabled"
						value="{$mod_strings['LBL_UW_TITLE_UPLOAD']}"
						onClick="uploadCheck();upgradeP('uploadingUpgradePackage', false);document.the_form.upgrade_zip_escaped.value = escape( document.the_form.upgrade_zip.value );document.the_form.submit();" />
				<input type=hidden name="upgrade_zip_escaped" value="" />
			</td>
		</tr>
	</table>
</td></tr>
</table>
</form>
eoq;
}
$hidden_fields = "<input type=\"hidden\" name=\"module\" value=\"UpgradeWizard\">";
$hidden_fields .= "<input type=\"hidden\" name=\"action\" value=\"index\">";
$hidden_fields .= "<input type=\"hidden\" name=\"step\" value=\"{$_REQUEST['step']}\">";
$hidden_fields .= "<input type=\"hidden\" name=\"run\" value=\"upload\">";
$form2 = '';
/*  Removing Install From Sugar tab from Upgradewizard.
if(class_exists("PackageManagerDisplay")) {
    $form2 = PackageManagerDisplay::buildPatchDisplay($form, $hidden_fields, 'index.php', array('patch', 'module'));
}
*/
if ($form2 == null) {
    $form2 = $form;
}
$json = getVersionedScript('include/JSON.js');
$form3 =<<<eoq2
<br>

<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
<tr><td>

	<table width="100%" border="0" cellspacing="0" cellpadding="0">
		<tr>
			<td>
				{$mod_strings['LBL_UW_FILES_QUEUED']}<br>
				{$ready}
			</td>
		</tr>
	</table>
</td></tr>
</table>
<script>
 function fileBrowseLoaded(){
 	//alert(document.the_form.upgrade_zip.value.length);
 	if(escape(document.the_form.upgrade_zip.value).length == 0 || escape(document.the_form.upgrade_zip.value) == 'undefined'){
       document.the_form.upload_button.disabled= 'disabled';
 	}
 	else{
 		document.the_form.upload_button.disabled= '';
 	}
 	var len = escape(document.the_form.upgrade_zip.value).length;
 }

 function uploadCheck(){
   var len = escape(document.the_form.upgrade_zip.value).length;
   var file_extn = escape(document.the_form.upgrade_zip.value).substr(len-3,len);
   if(file_extn.toLowerCase() !='zip'){
   		//document.the_form.upgrade_zip.value = '';
   		//document.getElementById("upgrade_zip").value = '';
   		alert('Not a zip file');
   		document.getElementById("upgrade_zip").value='';
   		document.getElementById("upload_button").disabled='disabled';
   } else {
	//AJAX call for checking the file size and comparing with php.ini settings.
	var callback = {
		 success:function(r) {
		     var file_size = r.responseText;
		     //alert(file_size.length);
		     if(file_size.length >0){
		       var msg = SUGAR.language.get('UpgradeWizard','LBL_UW_FILE_SIZE');
		       msg1 =SUGAR.language.get('UpgradeWizard','LBL_UW_FILE_BIGGER_MSG');
		       msg2 = SUGAR.language.get('UpgradeWizard','LBL_BYTES_WEBSERVER_MSG');
		       if(msg  == 'undefined') msg = 'The file size, ';
		       if(msg1 == 'undefined') msg1 = ' Bytes, is greater than what is allowed by the upload_max_filesize and/or the post_max_size settings in php.ini. Change the settings so that they are greater than ';
		       if(msg2 == 'undefined') msg2 = ' Bytes and restart the webserver.';
		       msg = msg+file_size+msg1;
		       msg = msg+file_size+msg2;
		       alert(msg);
		       document.getElementById("upload_button").disabled='disabled';
		     }
		     else{
		       document.getElementById("upload_button").disabled='';
		     }
		 }
	}

    //var file_name = document.getElementById('upgrade_zip').value;
	var file_name = document.the_form.upgrade_zip.value;
	postData = 'file_name=' + YAHOO.lang.JSON.stringify(file_name) + '&module=UpgradeWizard&action=UploadFileCheck&to_pdf=1';
	YAHOO.util.Connect.asyncRequest('POST', 'index.php', callback, postData);
   }
}
</script>
eoq2;
$form5 =<<<eoq5
<br>
<div id="upgradeDiv" style="display:none">
    <table cellspacing="0" cellpadding="0" border="0">
        <tr><td>
           <p><!--not_in_theme!--><img src='modules/UpgradeWizard/processing.gif' alt='Processing'></p>
        </td></tr>
     </table>
 </div>

eoq5;
$uwMain = $form2.$form3.$form5;
////	END UPLOAD FORM
///////////////////////////////////////////////////////////////////////////////
//set the upgrade progress status. actually it should be set when a file is uploaded
//set_upgrade_progress('upload','done');


$showBack		= true;
$showCancel		= true;
$showRecheck	= false;
$showNext		= true;

$stepBack		= $_REQUEST['step'] - 1;
$stepNext		= $_REQUEST['step'] + 1;
$stepCancel		= -1;
$stepRecheck	= $_REQUEST['step'];


$_SESSION['step'][$steps['files'][$_REQUEST['step']]] = ($stop) ? 'failed' : 'success';
