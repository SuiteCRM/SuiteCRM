<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

global $current_user;
global $app_strings;

if (!is_admin($current_user)) {
    sugar_die($app_strings['ERR_NOT_ADMIN']);
}

require_once('include/utils/db_utils.php');
require_once('include/utils/php_zip_utils.php');
require_once('modules/UpgradeWizard/uw_utils.php');
require_once('modules/Administration/UpgradeHistory.php');

$GLOBALS['top_message'] = '';

if (!isset($locale) || empty($locale)) {
    $locale = new Localization();
}
global $sugar_config;
global $sugar_flavor;
global $current_language;


require_once('modules/Trackers/TrackerManager.php');
$trackerManager = TrackerManager::getInstance();
$trackerManager->pause();
$trackerManager->unsetMonitors();

///////////////////////////////////////////////////////////////////////////////
////	SYSTEM PREP
list($base_upgrade_dir, $base_tmp_upgrade_dir) = getUWDirs();
$subdirs = array('full', 'langpack', 'module', 'patch', 'theme');

global $sugar_flavor;

prepSystemForUpgrade();

$uwMain = '';
$steps = array();
$step = 0;
$showNext = '';
$showCancel = '';
$showBack = '';
$showRecheck = '';
$stepNext = '';
$stepCancel = '';
$stepBack = '';
$stepRecheck = '';
$showDone = '';
$showExit = '';
$disableNextForLicense='';

if (!isset($_SESSION['step']) || !is_array($_SESSION['step'])) {
    $_SESSION['step'] = array();
}

////	END SYSTEM PREP
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
////	LOGIC
$uh = new UpgradeHistory();
$smarty = new Sugar_Smarty();
set_upgrade_vars();
//Initialize the session variables. If upgrade_progress.php is already created
//look for session vars there and restore them
initialize_session_vars();

$deletedPackage =false;
$cancelUpgrade = false;
$backOrRecheckUpgrade = false;

// this flag set in pre_install.php->UWUpgrade();

//ADDING A SESSION VARIBALE FOR KEEPING TRACK OF TOTAL UPGRADE TIME.
if (!isset($_SESSION['totalUpgradeTime'])) {
    $_SESSION['totalUpgradeTime'] = 0;
}

if (!isset($mod_strings['LBL_UW_ACCEPT_THE_LICENSE']) || $mod_strings['LBL_UW_ACCEPT_THE_LICENSE'] == null) {
    $mod_strings['LBL_UW_ACCEPT_THE_LICENSE'] = 'Accept License';
}
if (!isset($mod_strings['LBL_UW_CONVERT_THE_LICENSE']) || $mod_strings['LBL_UW_CONVERT_THE_LICENSE'] == null) {
    $mod_strings['LBL_UW_CONVERT_THE_LICENSE'] = 'Convert License';
}

$license_title = $mod_strings['LBL_UW_ACCEPT_THE_LICENSE'];
if ((isset($sugar_flavor) && $sugar_flavor != null) && ($sugar_flavor=='OS' || $sugar_flavor=='CE')) {
    $license_title = $mod_strings['LBL_UW_CONVERT_THE_LICENSE'];
}

if (isset($_REQUEST['delete_package']) && $_REQUEST['delete_package'] == 'true') {
    logThis('running delete old package');
    $error = '';
    if (!isset($_REQUEST['install_file']) || ($_REQUEST['install_file'] == "")) {
        logThis('ERROR: trying to delete non-existent file: ['.$_REQUEST['install_file'].']');
        $error .= $mod_strings['ERR_UW_NO_FILE_UPLOADED'].'<br>';
    }

    // delete file in upgrades/patch
    $delete_me = 'upload://upgrades/patch/'.basename(urldecode($_REQUEST['install_file']));
    if (is_file($delete_me) && !@unlink($delete_me)) {
        logThis('ERROR: could not delete: '.$delete_me);
        $error .= $mod_strings['ERR_UW_FILE_NOT_DELETED'].$delete_me.'<br>';
    }

    // delete back up instance
    $delete_dir = 'upload://upgrades/patch/'.remove_file_extension(urldecode($_REQUEST['install_file'])) . "-restore";
    if (is_dir($delete_dir) && !@rmdir_recursive($delete_dir)) {
        logThis('ERROR: could not delete: '.$delete_dir);
        $error .= $mod_strings['ERR_UW_FILE_NOT_DELETED'].$delete_dir.'<br>';
    }

    if (!empty($error)) {
        $out = "<b><span class='error'>{$error}</span></b><br />";
        if (!empty($GLOBALS['top_message'])) {
            $GLOBALS['top_message'] .= "<br />{$out}";
        } else {
            $GLOBALS['top_message'] = $out;
        }
    }
}

//redirect to the new upgradewizard
if (isset($_SESSION['Upgraded451Wizard']) && $_SESSION['Upgraded451Wizard']==true) {
    if (!isset($_SESSION['Initial_451to500_Step'])) {
        //redirect to the new upgradewizard
        $redirect_new_wizard = $sugar_config['site_url' ].'/index.php?module=UpgradeWizard&action=index';
        //'<form name="redirect" action="' .$redirect_new_wizard. '" >';
        //echo "<meta http-equiv='refresh' content='0; url={$redirect_new_wizard}'>";
        $_SESSION['Initial_451to500_Step'] = true;
        //unset($_SESSION['step']);
        $_REQUEST['step'] = 0;
    }
    $steps = array(
            'files' => array(
                'license_fiveO',
                'preflight',
                'commit',
                'end',
                'cancel',
            ),
            'desc' => array(
                $license_title,
                $mod_strings['LBL_UW_TITLE_PREFLIGHT'],
                $mod_strings['LBL_UW_TITLE_COMMIT'],
                $mod_strings['LBL_UW_TITLE_END'],
                $mod_strings['LBL_UW_TITLE_CANCEL'],
            ),
        );
} else {
    if (isset($_SESSION['UpgradedUpgradeWizard']) && $_SESSION['UpgradedUpgradeWizard'] == true) {
        // Upgrading from 5.0 upwards and upload already performed.
        $steps = array(
            'files' => array(
                    'start',
                    'systemCheck',
                    'preflight',
                    'commit',
                    'end',
                    'cancel',
            ),
            'desc' => array(
                    $mod_strings['LBL_UW_TITLE_START'],
                    $mod_strings['LBL_UW_TITLE_SYSTEM_CHECK'],
                    $mod_strings['LBL_UW_TITLE_PREFLIGHT'],
                    $mod_strings['LBL_UW_TITLE_COMMIT'],
                    $mod_strings['LBL_UW_TITLE_END'],
                    $mod_strings['LBL_UW_TITLE_CANCEL'],
            ),
        );
    } else {
        if (empty($mod_strings['LBL_UW_TITLE_LAYOUTS'])) {
            $mod_strings['LBL_UW_TITLE_LAYOUTS'] = 'Layouts';
        }
        /* END TEMP FIX */

        // Upgrading from 5.0 upwards and upload not performed yet.
        $steps = array(
            'files' => array(
                    'start',
                    'systemCheck',
                    'upload',
                    'preflight',
                    'commit',
                    'layouts',
                    'end',
                    'cancel',
            ),
            'desc' => array(
                    $mod_strings['LBL_UW_TITLE_START'],
                    $mod_strings['LBL_UW_TITLE_SYSTEM_CHECK'],
                    $mod_strings['LBL_UPLOAD_UPGRADE'],
                    $mod_strings['LBL_UW_TITLE_PREFLIGHT'],
                    $mod_strings['LBL_UW_TITLE_COMMIT'],
                    $mod_strings['LBL_UW_TITLE_LAYOUTS'],
                    $mod_strings['LBL_UW_TITLE_END'],
                    $mod_strings['LBL_UW_TITLE_CANCEL'],
            ),
        );
    }
}

$upgradeStepFile = '';
if (isset($_REQUEST['step']) && $_REQUEST['step'] !=null) {
    if ($_REQUEST['step'] == -1) {
        $_REQUEST['step'] = count($steps['files']) - 1;
    } elseif ($_REQUEST['step'] >= count($steps['files'])) {
        $_REQUEST['step'] = 0;
    }
    $upgradeStepFile = $steps['files'][$_REQUEST['step']];
} else {
    //check if upgrade was run before. If so then resume from there
    $previouUpgradeRun = get_upgrade_progress();
    if ($previouUpgradeRun != null) {
        //echo 'Previous run '.$previouUpgradeRun.'</br>';
        $upgradeStepFile = $previouUpgradeRun;
        //reset REQUEST
        $itemsCount = count($steps['files']);
        //reset REQUEST
        for ($i=0; $i<$itemsCount; $i++) {
            if ($steps['files'][$i]== $previouUpgradeRun) {
                $_REQUEST['step']=$i;
                break;
            }
        }
    } else {
        // first time through - kill off old sessions
        unset($_SESSION['step']);
        $_REQUEST['step'] = 0;
        $upgradeStepFile = $steps['files'][$_REQUEST['step']];
    }
}

if ($upgradeStepFile == 'license_fiveO') {
    $disableNextForLicense = 'disabled = "disabled"';
}
if ($upgradeStepFile == 'end') {
    //if(isset($_SESSION['current_db_version']) && substr($_SESSION['current_db_version'],0,1) == 4){
    ob_start();
    include('modules/ACL/install_actions.php');
    $old_mod_strings = $mod_strings;
    $mod_strings = return_module_language($current_language, 'Administration');
    include('modules/Administration/RebuildRelationship.php');
    $mod_strings = $old_mod_strings;
    //also add the cache cleaning here.
    if (function_exists('deleteCache')) {
        deleteCache();
    }
    ob_end_clean();
    //}
}

$additionalStep = $_REQUEST['additional_step'] ?? false;
if (!isset($additionalStep) || !$additionalStep) {
    require('modules/UpgradeWizard/' . $upgradeStepFile . '.php');
}

$afterCurrentStep = $_REQUEST['step'] + 1;

////	END LOGIC
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
////	UPGRADE HISTORY
// Reload language strings after copy
if (empty($GLOBALS['current_language'])) {
    $GLOBALS['current_language'] = 'en_us';
}
LanguageManager::loadModuleLanguage('UpgradeWizard', $GLOBALS['current_language'], true);
// display installed pieces and versions
$installeds = $uh->getAll();
$upgrades_installed = 0;

$uwHistory  = '<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view"><tr><td>'.$mod_strings['LBL_UW_DESC_MODULES_INSTALLED']."<br>\n";
$uwHistory .= "<ul>\n";
$uwHistory .= "<table class=\"edit view\" cellspacing=5>\n";
$uwHistory .= <<<eoq
	<tr>
		<td></td>
		<td align=left>
			<b>{$mod_strings['LBL_ML_NAME']}</b>
		</td>
		<td align=left>
			<b>{$mod_strings['LBL_ML_TYPE']}</b>
		</td>
		<td align=left>
			<b>{$mod_strings['LBL_ML_VERSION']}</b>
		</td>
		<td align=left>
			<b>{$mod_strings['LBL_ML_INSTALLED']}</b>
		</td>
		<td align=left>
			<b>{$mod_strings['LBL_ML_DESCRIPTION']}</b>
		</td>
		<td align=left>
			<b>{$mod_strings['LBL_ML_ACTION']}</b>
		</td>
	</tr>
eoq;

foreach ($installeds as $installed) {
    $form_action = '';
    $filename = from_html($installed->filename);
    $date_entered = $installed->date_entered;
    $type = $installed->type;
    //rrs only display patches here
    if ($type == 'patch') {
        $version = $installed->version;
        $upgrades_installed++;
        $link = is_file($filename)? '   <input type="hidden" name="module" value="UpgradeWizard">
					<input type="hidden" name="action" value="index">
					<input type="hidden" name="step" value="'.$_REQUEST['step'].'">
					<input type="hidden" name="delete_package" value="true">
	        		<input type=hidden name="install_file" value="'.$filename.'" />
	        		<input type=submit value="'.$mod_strings['LBL_BUTTON_DELETE'].'" />':'';

        $view = 'default';

        $target_manifest = remove_file_extension($filename) . "-manifest.php";

        // cn: bug 9174 - cleared out upgrade dirs, or corrupt entries in upgrade_history give us bad file paths
        if (is_file($target_manifest)) {
            require_once(getUploadRelativeName($target_manifest));
            $name = empty($manifest['name']) ? $filename : $manifest['name'];
            $description = empty($manifest['description']) ? $mod_strings['LBL_UW_NONE'] : $manifest['description'];

            if (isset($manifest['icon']) && $manifest['icon'] != "") {
                $manifest_copy_files_to_dir = isset($manifest['copy_files']['to_dir']) ? clean_path($manifest['copy_files']['to_dir']) : "";
                $manifest_copy_files_from_dir = isset($manifest['copy_files']['from_dir']) ? clean_path($manifest['copy_files']['from_dir']) : "";
                $manifest_icon = clean_path($manifest['icon']);
                $icon = "<!--not_in_theme!--><img src=\"" . $manifest_copy_files_to_dir . ($manifest_copy_files_from_dir != "" ? substr((string) $manifest_icon, strlen((string) $manifest_copy_files_from_dir)+1) : $manifest_icon) . "\">";
            } else {
                $icon = getImageForType($manifest['type']);
            }

            $uwHistory .= "<form action=\"index.php\" method=\"post\">\n".
                "<tr><td align=left>$icon</td><td align=left>$name</td><td align=left>$type</td><td align=left>$version</td><td align=left>$date_entered</td><td align=left>$description</td><td align=left>$link</td></tr>\n".
                "</form>\n";
        }
    }
}


if ($upgrades_installed == 0) {
    $uwHistory .= "<td colspan='6'>";
    $uwHistory .= $mod_strings['LBL_UW_NO_INSTALLED_UPGRADES'];
    $uwHistory .= "</td></tr>";
}

$uwHistory .= "</table></td></tr>
</table>\n";
$uwHistory .= "</ul>\n";
////	END UPGRADE HISTORY
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
////	PAGE OUTPUT

if ($upgradeStepFile=='preflight' || $upgradeStepFile=='commit' || $upgradeStepFile=='end') {
    $UW_510RC_PACKAGE_MESSAGE=<<<eoq
<table cellpadding="3" cellspacing="0" border="0">
	<tr>
		<th colspan="2" align="center">
			<h1><span class='error'><b>We do not recommended upgrading your production system to 5.1.0 RC. We recommend upgrading a development system for testing purposes.</b></span></h1>
		</th>
	</tr>
</table>
eoq;
}
$js=<<<eoq
<script type="text/javascript" language="Javascript">
	function toggleNwFiles(target) {
		var div = document.getElementById(target);

		if(div.style.display == "none") {
			div.style.display = "";
		} else {
			div.style.display = "none";
		}
	}



function handlePreflight(step) {
		if(step == 'preflight') {
			if(document.getElementById('select_schema_change') != null){
				document.getElementById('schema').value = document.getElementById('select_schema_change').value;
			}
			if(document.getElementById('diffs') != null) {
				/* preset the hidden var for defaults */
				checkSqlStatus(false);

				theForm = document.getElementById('diffs');
				var serial = '';
				for(i=0; i<theForm.elements.length; i++) {
						if(theForm.elements[i].type == 'checkbox' && theForm.elements[i].checked == false) {
						// we only want "DON'T OVERWRITE" files
						if(serial != '') {
							serial += "::";
						}
						serial += theForm.elements[i].value;
					}
				}				document.getElementById('overwrite_files_serial').value = serial;

				if(document.getElementById('addTask').checked == true) {
					document.getElementById('addTaskReminder').value = 'remind';
				}
				if(document.getElementById('addEmail').checked == true) {
					document.getElementById('addEmailReminder').value = 'remind';
				}
			}
		}

		var merge_necessary = true;
		if(step == 'layouts')
		   merge_necessary = getSelectedModulesForLayoutMerge();

		if(!merge_necessary){
			document.getElementById('step').value = '{$afterCurrentStep}';
		}

		return;
	}

function handleUploadCheck(step, u_allow) {
	if(step == 'upload' && !u_allow) {
		document.getElementById('top_message').innerHTML = '<span class="error"><b>{$mod_strings['LBL_UW_FROZEN']}</b></span>';
	}

	return;
}


function getSelectedModulesForLayoutMerge()
{
	var found_one = false;
    var results = new Array();
    var table = document.getElementById('layoutSelection');
    var moduleCheckboxes = table.getElementsByTagName('input');
    for (var i = 0; i < moduleCheckboxes.length; i++)
    {
        var singleCheckbox = moduleCheckboxes[i];
        if( typeof(singleCheckbox.type) != 'undefined' && singleCheckbox.type == 'checkbox'
            && singleCheckbox.name.substring(0,2) == 'lm' && singleCheckbox.checked )
        {
            found_one = true;
            results.push(singleCheckbox.name.substring(3)); //remove the 'lm_' key
        }
    }

    var selectedModules = results.join('^,^');

    var selectedModulesElement = document.createElement('input');
    selectedModulesElement.setAttribute('type', 'hidden');
    selectedModulesElement.setAttribute('name', 'layoutSelectedModules');
    selectedModulesElement.setAttribute('value', selectedModules);

    var upgradeForms = document.getElementsByName('UpgradeWizardForm');
    upgradeForms[0].appendChild(selectedModulesElement);
    return found_one;
}
</script>
eoq;

// If we are on step  4 then force a redirect to run again to pick up changes to smarty before setting the template.
if ($_REQUEST['step'] === '4' && !$additionalStep) {
    // Set session variables
    $_SESSION['UW_MAIN'] = $uwMain;
    $_SESSION['UW_JS'] = $js;
    $_SESSION['STEP'] = $step;
    $_SESSION['STEPS'] = $steps;
    $_SESSION['MOD'] = $mod_strings;
    $_SESSION['APP'] = $app_strings;
    $_SESSION['showNext'] = $showNext;
    $_SESSION['showCancel'] = $showCancel;
    $_SESSION['showBack'] = $showBack;
    $_SESSION['showRecheck'] = $showRecheck;
    $_SESSION['showDone'] = $showDone;
    $_SESSION['showExit'] = $showExit;
    $_SESSION['STEP_NEXT'] = $stepNext;
    $_SESSION['STEP_CANCEL'] = $stepCancel;
    $_SESSION['STEP_BACK'] = $stepBack;
    $_SESSION['STEP_RECHECK'] = $stepRecheck;
    $_SESSION['STEPS_FILE'] = $steps['files'];
    $_SESSION['UW_HISTORY'] = $uwHistory;
    $_SESSION['disableNextForLicense'] = $disableNextForLicense;
    $u_allow = 'true';
    if (isset($stop) && $stop === true) {
        $frozen = (isset($frozen)) ? '<br />' . $frozen : '';
        $_SESSION['frozen'] = $frozen;
        if ($step === 'upload') {
            $u_allow = 'false';
        }
    }
    $_SESSION['u_allow'] = $u_allow;
    if (!empty($GLOBALS['top_message'])) {
        $_SESSION['top_message'] = $GLOBALS['top_message'];
    }

    // Redirect to additional step and kill the process
    header('Location: ' . $_SERVER['HTTP_REFERER'] . '?module=UpgradeWizard&action=index&step=4&additional_step=true');
    die();
}

if($_REQUEST['step'] === '4' && $additionalStep) {
    // Set Smarty variables
    $uwMain = $_SESSION['UW_MAIN'];
    $js = $_SESSION['UW_JS'];
    $steps = $_SESSION['STEPS'];
    $step = $_SESSION['STEP'];
    $mod_strings = $_SESSION['MOD'];
    $app_strings = $_SESSION['APP'];
    $showNext = $_SESSION['showNext'];
    $showCancel = $_SESSION['showCancel'];
    $showBack = $_SESSION['showBack'];
    $showRecheck = $_SESSION['showRecheck'];
    $showDone = $_SESSION['showDone'];
    $showExit = $_SESSION['showExit'];
    $stepNext = $_SESSION['STEP_NEXT'];
    $stepCancel = $_SESSION['STEP_CANCEL'];
    $stepBack = $_SESSION['STEP_BACK'];
    $stepRecheck = $_SESSION['STEP_RECHECK'];
    $steps['files'] = $_SESSION['STEPS_FILE'];
    $_SESSION['STEPS_STEP'] = $_REQUEST['step'];
    $uwHistory = $_SESSION['UW_HISTORY'];
    $disableNextForLicense = $_SESSION['disableNextForLicense'];
    $frozen = $_SESSION['frozen'] ?? '';
    $u_allow = $_SESSION['u_allow'];
    $_SESSION['top_message'] = $GLOBALS['top_message'];
}

$smarty->assign('UW_MAIN', $uwMain);
$smarty->assign('UW_JS', $js);
$smarty->assign('CHECKLIST', getChecklist($steps, $step));
$smarty->assign('UW_TITLE', getClassicModuleTitle($mod_strings['LBL_UW_TITLE'], array($mod_strings['LBL_UW_TITLE'],$steps['desc'][$_REQUEST['step']]), false));
$smarty->assign('MOD', $mod_strings);
$smarty->assign('APP', $app_strings);
$smarty->assign('GRIDLINE', $current_user->getPreference('gridline'));
$smarty->assign('showNext', $showNext);
$smarty->assign('showCancel', $showCancel);
$smarty->assign('showBack', $showBack);
$smarty->assign('showRecheck', $showRecheck);
$smarty->assign('showDone', $showDone);
$smarty->assign('showExit', $showExit);
$smarty->assign('STEP_NEXT', $stepNext);
$smarty->assign('STEP_CANCEL', $stepCancel);
$smarty->assign('STEP_BACK', $stepBack);
$smarty->assign('STEP_RECHECK', $stepRecheck);
$smarty->assign('step', $steps['files'][$_REQUEST['step']]);
$smarty->assign('UW_HISTORY', $uwHistory);
$smarty->assign('disableNextForLicense', $disableNextForLicense);
$u_allow='true';
if (isset($stop) && $stop == true) {
    $frozen = (isset($frozen)) ? "<br />".$frozen : '';
    $smarty->assign('frozen', $frozen);
    if ($step == 'upload') {
        $u_allow = 'false';
    }
}
$smarty->assign('u_allow', $u_allow);
if (!empty($GLOBALS['top_message'])) {
    $smarty->assign('top_message', $GLOBALS['top_message']);
}

$smarty->assign('includeContainerCSS', false);
$smarty->display('modules/UpgradeWizard/uw_main.tpl');

// Clear session variables
unset($_SESSION['UW_MAIN'], $_SESSION['UW_JS'], $_SESSION['STEP'], $_SESSION['STEPS'], $_SESSION['MOD'], $_SESSION['APP'], $_SESSION['showNext'], $_SESSION['showCancel'], $_SESSION['showBack'], $_SESSION['showRecheck'], $_SESSION['showDone'], $_SESSION['showExit'], $_SESSION['STEP_NEXT'], $_SESSION['STEP_CANCEL'], $_SESSION['STEP_BACK'], $_SESSION['STEP_RECHECK'], $_SESSION['STEPS_FILE'], $_SESSION['UW_HISTORY'], $_SESSION['disableNextForLicense'], $_SESSION['frozen'], $_SESSION['u_allow'], $_SESSION['top_message']);
////	END PAGE OUTPUT
///////////////////////////////////////////////////////////////////////////////
