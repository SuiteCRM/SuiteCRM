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

logThis('Upgrade Wizard At Layout Commits');

global $mod_strings;
$curr_lang = 'en_us';
if (isset($GLOBALS['current_language']) && ($GLOBALS['current_language'] != null)) {
    $curr_lang = $GLOBALS['current_language'];
}

return_module_language($curr_lang, 'UpgradeWizard');

$errorLevelStored = error_reporting();
$maxExecutionTime = ini_get('max_execution_time');
error_reporting(E_ERROR);
set_time_limit(0);
set_upgrade_progress('layouts', 'in_progress');

//If the user has seleceted which modules they want to merge, perform the filtering and
//execute the merge.
if (isset($_POST['layoutSelectedModules'])) {
    logThis('Layout Commits examining modules to revert');
    
    $mergedModules = $_SESSION['sugarMergeRunResults'];
    $selectedModules  = explode("^,^", $_POST['layoutSelectedModules']);
    logThis('Layout Commits, selected modules by user: ' . print_r($selectedModules, true));
    $rollBackList = array();
    $actualMergedList = array();
    
    foreach ($mergedModules as $moduleKey => $layouts) {
        if (! in_array($moduleKey, $selectedModules)) {
            logThis("Adding $moduleKey module to rollback list.");
            $rollBackList[$moduleKey] = $layouts;
        } else {
            $actualMergedList[$moduleKey] = $layouts;
        }
    }
    
    logThis('Layout Commits will rollback the following modules: ' . print_r($rollBackList, true));
    logThis('Layout Commits merged the following modules: ' . print_r($actualMergedList, true));
    
    $layoutMergeData = $actualMergedList;
    
    rollBackMergedModules($rollBackList);
    
    $stepBack = $_REQUEST['step'] - 1;
    $stepNext = $_REQUEST['step'] + 1;
    $stepCancel = -1;
    $stepRecheck = $_REQUEST['step'];
    $_SESSION['step'][$steps['files'][$_REQUEST['step']]] = 'success';
    
    logThis('Layout Commits completed successfully');
    $smarty->assign("CONFIRM_LAYOUT_HEADER", $mod_strings['LBL_UW_CONFIRM_LAYOUT_RESULTS']);
    $smarty->assign("CONFIRM_LAYOUT_DESC", $mod_strings['LBL_UW_CONFIRM_LAYOUT_RESULTS_DESC']);
    $showCheckBoxes = false;
    $GLOBALS['top_message'] = "<b>{$mod_strings['LBL_LAYOUT_MERGE_TITLE2']}</b>";
} else {
    //Fist visit to the commit layout page.  Display the selection table to the user.
    logThis('Layout Commits about to show selection table');
    $smarty->assign("CONFIRM_LAYOUT_HEADER", $mod_strings['LBL_UW_CONFIRM_LAYOUTS']);
    $smarty->assign("CONFIRM_LAYOUT_DESC", $mod_strings['LBL_LAYOUT_MERGE_DESC']);
    $layoutMergeData = cleanMergeData($_SESSION['sugarMergeRunResults']);
    $stepNext = $_REQUEST['step'];
    $showCheckBoxes = true;
    $GLOBALS['top_message'] = "<b>{$mod_strings['LBL_LAYOUT_MERGE_TITLE']}</b>";
}

$smarty->assign("APP", $app_strings);
$smarty->assign("APP_LIST", $app_list_strings);
$smarty->assign("MOD", $mod_strings);
$smarty->assign("showCheckboxes", $showCheckBoxes);
$layoutMergeData = formatLayoutMergeDataForDisplay($layoutMergeData);
$smarty->assign("METADATA_DATA", $layoutMergeData);
$uwMain = $smarty->fetch('modules/UpgradeWizard/tpls/layoutsMerge.tpl');
    
$showBack = false;
$showCancel = false;
$showRecheck = false;
$showNext = true;

set_upgrade_progress('layouts', 'done');

error_reporting($errorLevelStored);
set_time_limit($maxExecutionTime);

/**
 * Clean the merge data results, removing any emptys or blanks that should not be displayed
 * to the user on the confirm layout screen.
 *
 * @param array $data
 * @return array
 */
function cleanMergeData($data)
{
    $results = array();
    foreach ($data as $m => $layouts) {
        if ((is_countable($layouts) ? count($layouts) : 0) > 0) {
            $results[$m] = $layouts;
        }
    }
    
    return $results;
}
/**
 * Rollback metadata files for each module provided in the list.
 *
 * @param array $data
 */
function rollBackMergedModules($data)
{
    logThis('Layout Commits, starting rollback');
    $backupFileSufix = '.suback.php';
    foreach ($data as $moduleName => $layouts) {
        logThis('Layout Commits, iterating over module:' . $moduleName);
        foreach ($layouts as $fileName => $wasMerged) {
            if ($wasMerged) {
                $srcFile = 'custom' . DIRECTORY_SEPARATOR . 'modules' . DIRECTORY_SEPARATOR . $moduleName . DIRECTORY_SEPARATOR . 'metadata'. DIRECTORY_SEPARATOR . $fileName;
                $srcBackupFile = $srcFile . $backupFileSufix;
                logThis('Layout Commits, rollBackMergedModules source file: ' . $srcDirectory);
                logThis('Layout Commits, rollBackMergedModules backup file: ' . $srcBackupFile);
                if (file_exists($srcBackupFile)) {
                    if (file_exists($srcFile)) {
                        logThis('Layout Commits, rollBackMergedModules is removing file: ' . $srcFile);
                        @unlink($srcFile);
                    }
                    $copyResult = @copy($srcBackupFile, $srcFile);
                    if ($copyResult === true) {
                        @unlink($srcBackupFile);
                        logThis("Layout Commits, rollBackMergedModules successfully reverted file $srcFile");
                    } else {
                        logThis("Layout Commits, rollBackMergedModules was unable to copy file: $srcBackupFile, to $srcFile.");
                    }
                } else {
                    logThis("Layout Commits, rollBackMergedModules is unable to find backup file $srcBackupFile , nothing to do.");
                }
            }
        }
    }
}

/**
 * Format results from SugarMerge output to be used in the selection table.
 *
 * @param array $layoutMergeData
 * @return array
 */
function formatLayoutMergeDataForDisplay($layoutMergeData)
{
    global $mod_strings,$app_list_strings;
    
    $curr_lang = 'en_us';
    if (isset($GLOBALS['current_language']) && ($GLOBALS['current_language'] != null)) {
        $curr_lang = $GLOBALS['current_language'];
    }

    $module_builder_language = return_module_language($curr_lang, 'ModuleBuilder');

    $results = array();
    foreach ($layoutMergeData as $k => $v) {
        $layouts = array();
        foreach ($v as $layoutPath => $isMerge) {
            if (preg_match('/listviewdefs.php/i', (string) $layoutPath)) {
                $label = $module_builder_language['LBL_LISTVIEW'];
            } else {
                if (preg_match('/detailviewdefs.php/i', (string) $layoutPath)) {
                    $label = $module_builder_language['LBL_DETAILVIEW'];
                } else {
                    if (preg_match('/editviewdefs.php/i', (string) $layoutPath)) {
                        $label = $module_builder_language['LBL_EDITVIEW'];
                    } else {
                        if (preg_match('/quickcreatedefs.php/i', (string) $layoutPath)) {
                            $label = $module_builder_language['LBL_QUICKCREATE'];
                        } else {
                            if (preg_match('/searchdefs.php/i', (string) $layoutPath)) {
                                $label = $module_builder_language['LBL_SEARCH_BUTTON'];
                            } else {
                                continue;
                            }
                        }
                    }
                }
            }

            $layouts[] = array('path' => $layoutPath, 'label' => $label);
        }

        $results[$k]['layouts'] = $layouts;
        $results[$k]['moduleName'] = $app_list_strings['moduleList'][$k];
    }

    return $results;
}
