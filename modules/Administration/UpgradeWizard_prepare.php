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

require_once('modules/Administration/UpgradeWizardCommon.php');

unset($_SESSION['rebuild_relationships']);
unset($_SESSION['rebuild_extensions']);
// process commands
if (empty($_REQUEST['install_file'])) {
    die("File to install not specified.");
}
if (!isset($_REQUEST['mode']) || ($_REQUEST['mode'] == "")) {
    die("No mode specified.");
}

if (!file_exists($base_tmp_upgrade_dir) && !mkdir($base_tmp_upgrade_dir, 0755,
        true) && !is_dir($base_tmp_upgrade_dir)) {
    throw new \RuntimeException(sprintf('Directory "%s" was not created', $base_tmp_upgrade_dir));
}

$unzip_dir      = mk_temp_dir($base_tmp_upgrade_dir);
$install_file   = hashToFile($_REQUEST['install_file']);
$hidden_fields = "";
$new_lang_name = "";
$new_lang_desc = "";

$mode           = $_REQUEST['mode'];
$hidden_fields .= "<input type=hidden name=\"mode\" value=\"$mode\"/>";


$install_type   = getInstallType($install_file);

$version        = "";
$previous_version = "";
$show_files     = true;

$zip_from_dir   = ".";
$zip_to_dir     = ".";
$zip_force_copy = array();
$license_file = $unzip_dir.'/LICENSE.txt';
$readme_file  = $unzip_dir.'/README.txt';
$require_license = false;
$found_readme = false;
$author = '';
$name = '';
$description = '';
$is_uninstallable = true;
$id_name = '';
$dependencies = array();
$remove_tables = 'true';

unzip($install_file, $unzip_dir);
if ($install_type == 'module' && $mode != 'Uninstall') {
    if (file_exists($license_file)) {
        $require_license = true;
    }
}

//Scan the unzip dir for unsafe files
if (((defined('MODULE_INSTALLER_PACKAGE_SCAN') && MODULE_INSTALLER_PACKAGE_SCAN)
    || !empty($GLOBALS['sugar_config']['moduleInstaller']['packageScan'])) && $install_type != 'patch') {
    require_once('ModuleInstall/ModuleScanner.php');
    $ms = new ModuleScanner();
    $ms->scanPackage($unzip_dir);
    if ($ms->hasIssues()) {
        rmdir_recursive($unzip_dir);
        $ms->displayIssues();
        sugar_cleanup(true);
    }
}

// assumption -- already validated manifest.php at time of upload
require("$unzip_dir/manifest.php");



if (isset($manifest['copy_files']['from_dir']) && $manifest['copy_files']['from_dir'] != "") {
    $zip_from_dir   = $manifest['copy_files']['from_dir'];
}
if (isset($manifest['copy_files']['to_dir']) && $manifest['copy_files']['to_dir'] != "") {
    $zip_to_dir     = $manifest['copy_files']['to_dir'];
}
if (isset($manifest['copy_files']['force_copy']) && $manifest['copy_files']['force_copy'] != "") {
    $zip_force_copy     = $manifest['copy_files']['force_copy'];
}
if (isset($manifest['version'])) {
    $version    = $manifest['version'];
}
if (isset($manifest['author'])) {
    $author    = $manifest['author'];
}
if (isset($manifest['name'])) {
    $name    = $manifest['name'];
}
if (isset($manifest['description'])) {
    $description    = $manifest['description'];
}
if (isset($manifest['is_uninstallable'])) {
    $is_uninstallable    = $manifest['is_uninstallable'];
}
if (isset($installdefs) && isset($installdefs['id'])) {
    $id_name    = $installdefs['id'];
}
if (isset($manifest['dependencies'])) {
    $dependencies    = $manifest['dependencies'];
}
if (isset($manifest['remove_tables'])) {
    $remove_tables = $manifest['remove_tables'];
}

if ($remove_tables != 'prompt') {
    $hidden_fields .= "<input type=hidden name=\"remove_tables\" value='".$remove_tables."'>";
}
if (file_exists($readme_file) || !empty($manifest['readme'])) {
    $found_readme = true;
}
$uh = new UpgradeHistory();
//check dependencies first
if (!empty($dependencies)) {
    $not_found = $uh->checkDependencies($dependencies);
    if (!empty($not_found) && (is_countable($not_found) ? count($not_found) : 0) > 0) {
        die($mod_strings['ERR_UW_NO_DEPENDENCY']."[".implode(',', $not_found)."]");
    }//fi
}
switch ($install_type) {
    case "full":
    case "patch":
        if (!is_writable("config.php")) {
            die($mod_strings['ERR_UW_CONFIG']);
        }
        break;
    case "theme":
        break;
    case "langpack":
        // find name of language pack: find single file in include/language/xx_xx.lang.php
        $d = dir("$unzip_dir/$zip_from_dir/include/language");
        while ($f = $d->read()) {
            if ($f == "." || $f == "..") {
                continue;
            } else {
                if (preg_match("/(.*)\.lang\.php\$/", $f, $match)) {
                    $new_lang_name = $match[1];
                }
            }
        }
        if ($new_lang_name == "") {
            die($mod_strings['ERR_UW_NO_LANGPACK'].$install_file);
        }
        $hidden_fields .= "<input type=hidden name=\"new_lang_name\" value=\"$new_lang_name\"/>";

        $new_lang_desc = getLanguagePackName("$unzip_dir/$zip_from_dir/include/language/$new_lang_name.lang.php");
        if ($new_lang_desc == "") {
            die($mod_strings['ERR_UW_NO_LANG_DESC_1']."include/language/$new_lang_name.lang.php".$mod_strings['ERR_UW_NO_LANG_DESC_2']."$install_file.");
        }
        $hidden_fields .= "<input type=hidden name=\"new_lang_desc\" value=\"$new_lang_desc\"/>";

        if (!is_writable("config.php")) {
            die($mod_strings['ERR_UW_CONFIG']);
        }
        break;
    case "module":
        $previous_install = array();
        if (!empty($id_name) & !empty($version)) {
            $previous_install = $uh->determineIfUpgrade($id_name, $version);
        }
        $previous_version = (empty($previous_install['version'])) ? '' : $previous_install['version'];
        $previous_id = (empty($previous_install['id'])) ? '' : $previous_install['id'];
        $show_files = false;
        //rrs pull out unique_key
        $hidden_fields .= "<input type=hidden name=\"author\" value=\"$author\"/>";
        $hidden_fields .= "<input type=hidden name=\"name\" value=\"$name\"/>";
        $hidden_fields .= "<input type=hidden name=\"description\" value=\"$description\"/>";
        $hidden_fields .= "<input type=hidden name=\"is_uninstallable\" value=\"$is_uninstallable\"/>";
        $hidden_fields .= "<input type=hidden name=\"id_name\" value=\"$id_name\"/>";
        $hidden_fields .= "<input type=hidden name=\"previous_version\" value=\"$previous_version\"/>";
        $hidden_fields .= "<input type=hidden name=\"previous_id\" value=\"$previous_id\"/>";
        break;
    default:
        die($mod_strings['ERR_UW_WRONG_TYPE'].$install_type);
}


$new_files      = findAllFilesRelative("$unzip_dir/$zip_from_dir", array());
$hidden_fields .= "<input type=hidden name=\"version\" value=\"$version\"/>";
$serial_manifest = array();
$serial_manifest['manifest'] = (isset($manifest) ? $manifest : '');
$serial_manifest['installdefs'] = (isset($installdefs) ? $installdefs : '');
$serial_manifest['upgrade_manifest'] = (isset($upgrade_manifest) ? $upgrade_manifest : '');
$hidden_fields .= "<input type=hidden name=\"s_manifest\" value='".base64_encode(serialize($serial_manifest))."'>";
// present list to user
?>
<form action="<?php print($form_action . "_commit"); ?>" name="files" method="post"  onSubmit="return validateForm(<?php print($require_license); ?>);">
<?php
if (empty($new_studio_mod_files)) {
    if (!empty($mode) && $mode == 'Uninstall') {
        echo $mod_strings['LBL_UW_UNINSTALL_READY'];
    } else {
        if ($mode == 'Disable') {
            echo $mod_strings['LBL_UW_DISABLE_READY'];
        } else {
            if ($mode == 'Enable') {
                echo $mod_strings['LBL_UW_ENABLE_READY'];
            } else {
                echo $mod_strings['LBL_UW_PATCH_READY'];
            }
        }
    }
} else {
    echo $mod_strings['LBL_UW_PATCH_READY2'];
    echo '<input type="checkbox" onclick="toggle_these(0, ' . (is_countable($new_studio_mod_files) ? count($new_studio_mod_files) : 0) . ', this)"> '.$mod_strings['LBL_UW_CHECK_ALL'];
    foreach ($new_studio_mod_files as $the_file) {
        $new_file   = clean_path("$zip_to_dir/$the_file");
        print("<li><input id=\"copy_$count\" name=\"copy_$count\" type=\"checkbox\" value=\"" . $the_file . "\"> " . $new_file . "</li>");
        $count++;
    }
}
echo '<br>';
if ($require_license) {
    $contents = sugar_file_get_contents($license_file);
    $readme_contents = '';
    if ($found_readme) {
        if (file_exists($readme_file) && filesize($readme_file) > 0) {
            $readme_contents = file_get_contents($readme_file);
        } elseif (!empty($manifest['readme'])) {
            $readme_contents = $manifest['readme'];
        }
    }
    $license_final =<<<eoq2
	<table width='100%'>
	<tr>
	<td colspan="3"><ul class="tablist">
	<li id="license_li" class="active"><a id="license_link"  class="current" href="javascript:selectTabCSS('license');">{$mod_strings['LBL_LICENSE']}</a></li>
	<li class="active" id="readme_li"><a id="readme_link" href="javascript:selectTabCSS('readme');">{$mod_strings['LBL_README']}</a></li>
	</ul></td>
	</tr>
	</table>
	<div id='license_div'>
	<table>
	<tr>
	<td colspan="3">&nbsp;</td>
	</tr>
	<tr>
	<td align="left" valign="top" colspan=2>
	<b>{$mod_strings['LBL_MODULE_LICENSE']}</b>
	</td>
	</tr>
	<tr>
	<td align="left" valign="top" colspan=2>
	<textarea cols="100" rows="8" readonly>{$contents}</textarea>
	</td>

	</tr>
	<tr>
	<td align="left" valign="top" colspan=2>
	<input type='radio' id='radio_license_agreement_accept' name='radio_license_agreement' value='accept'>{$mod_strings['LBL_ACCEPT']}&nbsp;
	<input type='radio' id='radio_license_agreement_reject' name='radio_license_agreement' value='reject' checked>{$mod_strings['LBL_DENY']}
	</td>

	</tr></table>
	</div>
	<div id='readme_div' style='display: none;'>
	<table>
	<tr>
	<td colspan="3">&nbsp;</td>
	</tr>
	<tr>
	<td align="left" valign="top" colspan=2>
	<b>{$mod_strings['LBL_README']}</b>
	</td>
	</tr>
	<tr>
	<td align="left" valign="top" colspan=2>
	<textarea cols="100" rows="8" readonly>{$readme_contents}</textarea>
        </td>

    </tr>
</table>
</div>

eoq2;
    echo $license_final;
    echo "<br>";
}

switch ($mode) {
    case "Install":
        if ($install_type == "langpack") {
            print($mod_strings['LBL_UW_LANGPACK_READY']);
            echo '<br><br>';
        }
        break;
    case "Uninstall":
        if ($install_type == "langpack") {
            print($mod_strings['LBL_UW_LANGPACK_READY_UNISTALL']);
            echo '<br><br>';
        } else {
            if ($install_type != "module") {
                print($mod_strings['LBL_UW_FILES_REMOVED']);
            }
        }
        break;
    case "Disable":
        if ($install_type == "langpack") {
            print($mod_strings['LBL_UW_LANGPACK_READY_DISABLE']);
            echo '<br><br>';
        }
        break;
    case "Enable":
        if ($install_type == "langpack") {
            print($mod_strings['LBL_UW_LANGPACK_READY_ENABLE']);
            echo '<br><br>';
        }
        break;
}


?>
<input type=submit value="<?php echo $mod_strings['LBL_ML_COMMIT'];?>" class="button" id="submit_button" />
<input type=button value="<?php echo $mod_strings['LBL_ML_CANCEL'];?>" class="button" onClick="location.href='index.php?module=Administration&action=UpgradeWizard&view=module';"/>

<?php

if ($remove_tables == 'prompt' && $mode == 'Uninstall') {
    print("<br/><br/>");
    print("<input type='radio' id='remove_tables_true' name='remove_tables' value='true' checked>".$mod_strings['ML_LBL_REMOVE_TABLES']."&nbsp;");
    print("<input type='radio' id='remove_tables_false' name='remove_tables' value='false'>".$mod_strings['ML_LBL_DO_NOT_REMOVE_TABLES']."<br>");
}
$count = 0;

if ($show_files == true) {
    $count = 0;

    $new_studio_mod_files = array();
    $new_sugar_mod_files = array();

    $cache_html_files = findAllFilesRelative(sugar_cached("layout"), array());

    foreach ($new_files as $the_file) {
        if (substr(strtolower($the_file), -5, 5) == '.html' && in_array($the_file, $cache_html_files)) {
            array_push($new_studio_mod_files, $the_file);
        } else {
            array_push($new_sugar_mod_files, $the_file);
        }
    }

    echo '<script>
            function toggle_these(start, end, ca) {
              while(start < end) {
                elem = eval("document.forms.files.copy_" + start);
                if(!ca.checked) elem.checked = false;
                else elem.checked = true;
                start++;
              }
            }
			</script>';



    global $theme;

    echo '<br/><br/>';

    echo '<div style="text-align: left; cursor: hand; cursor: pointer; text-decoration: underline;'.(($mode == 'Enable' || $mode == 'Disable')?'display:none;':'').'" onclick=\'this.style.display="none"; toggleDisplay("more");\'id="all_text">
        '.SugarThemeRegistry::current()->getImage('advanced_search', '', null, null, ".gif", $mod_strings['LBL_ADVANCED_SEARCH']).$mod_strings['LBL_UW_SHOW_DETAILS'].'</div><div id=\'more\' style=\'display: none\'>
              <div style="text-align: left; cursor: hand; cursor: pointer; text-decoration: underline;" onclick=\'document.getElementById("all_text").style.display=""; toggleDisplay("more");\'>'
         .SugarThemeRegistry::current()->getImage('basic_search', '', null, null, ".gif", $mod_strings['LBL_BASIC_SEARCH']).$mod_strings['LBL_UW_HIDE_DETAILS'].'</div><br>';
    echo '<input type="checkbox" checked onclick="toggle_these(' . count($new_studio_mod_files) . ',' . (is_countable($new_files) ? count($new_files) : 0) . ', this)"> '.$mod_strings['LBL_UW_CHECK_ALL'];
    echo '<ul>';
    foreach ($new_sugar_mod_files as $the_file) {
        $highlight_start = "";
        $highlight_end = "";
        $checked = "";
        $disabled = "";
        $unzip_file = "$unzip_dir/$zip_from_dir/$the_file";
        $new_file = clean_path("$zip_to_dir/$the_file");
        $forced_copy = false;

        if ($mode == "Install") {
            $checked = "checked";

            if ($install_type === 'langpack' && $the_file == "./manifest.php") {
                $checked = '';
                $disabled = "disabled=\"true\"";
            }

            foreach ($zip_force_copy as $pattern) {
                if (preg_match("#" . $pattern . "#", $unzip_file)) {
                    $disabled = "disabled=\"true\"";
                    $forced_copy = true;
                }
            }
            if (!$forced_copy && is_file($new_file) && (md5_file($unzip_file) === md5_file($new_file))) {
                $disabled = "disabled=\"true\"";
            }
            if ($checked != "" && $disabled != "") {    // need to put a hidden field
                print("<input name=\"copy_$count\" type=\"hidden\" value=\"" . $the_file . "\">\n");
            }
            print("<li><input id=\"copy_$count\" name=\"copy_$count\" type=\"checkbox\" value=\"" . $the_file . "\" $checked $disabled > " . $highlight_start . $new_file . $highlight_end);
            if ($checked == "" && $disabled != "") {    // need to explain this file hasn't changed
                print(" (no changes)");
            }
            print("<br>\n");
        } else {
            if ($mode == "Uninstall" && file_exists($new_file)) {
                if (md5_file($unzip_file) === md5_file($new_file)) {
                    $checked = "checked=\"true\"";
                } else {
                    $highlight_start = "<font color=red>";
                    $highlight_end = "</font>";
                }
                print("<li><input name=\"copy_$count\" type=\"checkbox\" value=\"" . $the_file . "\" $checked $disabled > " . $highlight_start . $new_file . $highlight_end . "<br>\n");
            }
        }
        $count++;
    }
    print("</ul>\n");
}

if ($mode == "Disable" || $mode == "Enable") {
    //check to see if any files have been modified
    $modified_files = getDiffFiles($unzip_dir, $install_file, ($mode == 'Enable'), $previous_version);
    if ((is_countable($modified_files) ? count($modified_files) : 0) > 0) {
        //we need to tell the user that some files have been modified since they last did an install
        echo '<script>' .
                'function handleFileChange(){';
        if ((is_countable($modified_files) ? count($modified_files) : 0) > 0) {
            echo 'if(document.getElementById("radio_overwrite_files") != null && document.getElementById("radio_do_not_overwrite_files") != null){
                			var overwrite = false;
                			if(document.getElementById("radio_overwrite_files").checked){
                   			 overwrite = true
                			}
            			}
        				return true;';
        } else {
            echo 'return true;';
        }
        echo '}</script>';
        print('<b>'.$mod_strings['ML_LBL_OVERWRITE_FILES'].'</b>');
        print('<table><td align="left" valign="top" colspan=2>');
        print("<input type='radio' id='radio_overwrite_files' name='radio_overwrite' value='overwrite'>{$mod_strings['LBL_OVERWRITE_FILES']}&nbsp;");
        print("<input type='radio' id='radio_do_not_overwrite_files' name='radio_overwrite' value='do_not_overwrite' checked>{$mod_strings['LBL_DO_OVERWRITE_FILES']}");
        print("</td></tr></table>");
        print('<ul>');
        foreach ($modified_files as $modified_file) {
            print('<li>'.$modified_file.'</li>');
        }
        print('</ul>');
    } else {
        echo '<script>' .
            'function handleFileChange(){';
        echo 'return true;';
        echo '}</script>';
    }
} else {
    echo '<script>' .
        'function handleFileChange(){';
    echo 'return true;';
    echo '}</script>';
}
echo '<script>' .
                 'function validateForm(process){'.
                     'return (handleCommit(process) && handleFileChange());'.
                 '}'.
                'function handleCommit(process){
        if(process == 1) {
            if(document.getElementById("radio_license_agreement_reject") != null && document.getElementById("radio_license_agreement_accept") != null){
                var accept = false;
                if(document.getElementById("radio_license_agreement_accept").checked){
                    accept = true
                }
                if(!accept){
                    //do not allow the form to submit
                    alert("'.$mod_strings['ERR_UW_ACCEPT_LICENSE'].'");
                    return false;
                }
            }
        }
        document.getElementById("submit_button").disabled = true;
        return true;
    }
    var keys = [ "license","readme"];
    function selectTabCSS(key){
            	for( var i=0; i<keys.length;i++)
              	{
                	var liclass = "";
                	var linkclass = "";

                	if ( key == keys[i])
                	{
                    	var liclass = "active";
                    	var linkclass = "current";
                    	document.getElementById(keys[i]+"_div").style.display = "block";
                	}else{
                    	document.getElementById(keys[i]+"_div").style.display = "none";
                	}
                	document.getElementById(keys[i]+"_li").className = liclass;
                	document.getElementById(keys[i]+"_link").className = linkclass;
            	}
                tabPreviousKey = key;
            }
      </script>';

    $fileHash = fileToHash($install_file);
?>
    <?php print($hidden_fields); ?>
    <input type="hidden" name="copy_count" value="<?php print($count);?>"/>
    <input type="hidden" name="run" value="commit" />
    <input type="hidden" name="install_file"  value="<?php echo $fileHash; ?>" />
    <input type="hidden" name="unzip_dir"     value="<?php echo basename((string) $unzip_dir); ?>" />
    <input type="hidden" name="zip_from_dir"  value="<?php echo $zip_from_dir; ?>" />
    <input type="hidden" name="zip_to_dir"    value="<?php echo $zip_to_dir; ?>" />
</form>

<?php
    $GLOBALS['log']->info("Upgrade Wizard patches");
?>
