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




////	COMMON

function ajaxSqlProgress($persistence, $sql, $type)
{
    global $mod_strings;

    // $type is sql_to_check or sql_to_run
    $whatsLeft = is_countable($persistence[$type]) ? count($persistence[$type]) : 0;

    ob_start();
    $out  = "<b>{$mod_strings['LBL_UW_PREFLIGHT_QUERY']}</b><br />";
    $out .= round((($persistence['sql_total'] - $whatsLeft) / $persistence['sql_total']) * 100, 1)."%
				{$mod_strings['LBL_UW_DONE']} - {$mod_strings['LBL_UW_PREFLIGHT_QUERIES_LEFT']}: {$whatsLeft}";
    $out .= "<br /><textarea cols='60' rows='3' DISABLED>{$sql}</textarea>";
    echo $out;
    ob_flush();

    if ($whatsLeft < 1) {
        $persistence['sql_check_done'] = true;
    }

    return $persistence;
}


///////////////////////////////////////////////////////////////////////////////
////	COMMIT AJAX
/**
 * does post-post-install stuff
 * @param array persistence
 * @return array persistence
 */
function commitAjaxFinalTouches($persistence)
{
    global $current_user;
    global $timedate;
    global $mod_strings;
    global $sugar_version;

    if (empty($sugar_version)) {
        require('sugar_version.php');
    }

    // convert to UTF8 if needed
    if (!empty($persistence['allTables'])) {
        executeConvertTablesSql($persistence['allTables']);
    }

    // rebuild
    logThis('Performing UWrebuild()...');
    UWrebuild();
    logThis('UWrebuild() done.');

    // upgrade history
    registerUpgrade($persistence);

    // flag to say upgrade has completed
    $persistence['upgrade_complete'] = true;

    // reminders if needed
    ///////////////////////////////////////////////////////////////////////////////
    ////	HANDLE REMINDERS
    if ((is_countable($persistence['skipped_files']) ? count($persistence['skipped_files']) : 0) > 0) {
        $desc  = $mod_strings['LBL_UW_COMMIT_ADD_TASK_OVERVIEW']."\n\n";
        $desc .= $mod_strings['LBL_UW_COMMIT_ADD_TASK_DESC_1'];
        $desc .= $persistence['uw_restore_dir']."\n\n";
        $desc .= $mod_strings['LBL_UW_COMMIT_ADD_TASK_DESC_2']."\n\n";

        foreach ($persistence['skipped_files'] as $file) {
            $desc .= $file."\n";
        }

        //MFH #13468
        $nowDate = $timedate->nowDbDate();
        $nowTime = $timedate->asDbTime($timedate->getNow());
        $nowDateTime = $nowDate.' '.$nowTime;

        if ($_REQUEST['addTaskReminder'] == 'remind') {
            logThis('Adding Task for admin for manual merge.');

            $task = BeanFactory::newBean('Tasks');
            $task->name = $mod_strings['LBL_UW_COMMIT_ADD_TASK_NAME'];
            $task->description = $desc;
            $task->date_due = $nowDate;
            $task->time_due = $nowTime;
            $task->priority = 'High';
            $task->status = 'Not Started';
            $task->assigned_user_id = $current_user->id;
            $task->created_by = $current_user->id;
            $task->date_entered = $nowDateTime;
            $task->date_modified = $nowDateTime;
            $task->save();
        }

        if ($_REQUEST['addEmailReminder'] == 'remind') {
            logThis('Sending Reminder for admin for manual merge.');

            $email = BeanFactory::newBean('Emails');
            $email->assigned_user_id = $current_user->id;
            $email->name = $mod_strings['LBL_UW_COMMIT_ADD_TASK_NAME'];
            $email->description = $desc;
            $email->description_html = nl2br($desc);
            $email->from_name = $current_user->full_name;
            $email->from_addr = $current_user->email1;
            isValidEmailAddress($email->from_addr);
            $email->to_addrs_arr = $email->parse_addrs($current_user->email1, '', '', '');
            $email->cc_addrs_arr = array();
            $email->bcc_addrs_arr = array();
            $email->date_entered = $nowDateTime;
            $email->date_modified = $nowDateTime;
            $email->send();
            $email->save();
        }
    }
    ////	HANDLE REMINDERS
    ///////////////////////////////////////////////////////////////////////////////

    // clean up
    unlinkUWTempFiles();

    ob_start();
    echo 'done';
    ob_flush();

    return $persistence;
}

/**
 * runs one line of sql
 * @param array $persistence
 * @return array $persistence
 */
function commitAjaxRunSql($persistence)
{
    $db = DBManagerFactory::getInstance();

    if (!isset($persistence['commit_sql_errors'])) {
        $persistence['commit_sql_errors'] = array();
    }

    // This flag is determined by the preflight check in the installer
    if ($persistence['schema_change'] == 'sugar') {
        if (isset($persistence['sql_to_run'])
            && (is_countable($persistence['sql_to_run']) ? count($persistence['sql_to_run']) : 0) > 0
            && !empty($persistence['sql_to_run'])) {
            $sql = array_shift($persistence['sql_to_run']);
            $sql = trim($sql);

            if (!empty($sql)) {
                logThis("[RUNNING SQL QUERY] {$sql}");
                $db->query($sql);

                $error = $db->lastError();
                if (!empty($error)) {
                    logThis('************************************************************');
                    logThis('*** ERROR: SQL Commit Error!');
                    logThis('*** Query: [ '.$sql.' ]');
                    logThis('************************************************************');
                    $persistence['commit_sql_errors'][] = getFormattedError($error, $sql);
                }
                $persistence = ajaxSqlProgress($persistence, $sql, 'sql_to_run');
            }
        } else {
            ob_start();
            echo 'done';
            ob_flush();
        }
    } else {
        ob_start();
        echo 'done';
        ob_flush();
    }

    return $persistence;
}

/**
 * returns errors found during SQL operations
 * @param array persistence
 * @return string Error message or empty string on success
 */
function commitAjaxGetSqlErrors($persistence)
{
    global $mod_strings;

    $out = '';
    if (isset($persistence['commit_sql_errors']) && !empty($persistence['commit_sql_errors'])) {
        $out = "<div class='error'>";
        foreach ($persistence['commit_sql_errors'] as $error) {
            $out .= $error;
        }
        $out .= "</div>";
    }

    if (empty($out)) {
        $out = $mod_strings['LBL_UW_COMMIT_ALL_SQL_SUCCESSFULLY_RUN'];
    }

    ob_start();
    echo $out;
    ob_flush();
}

/**
 * parses the sql upgrade file for sequential querying
 * @param array persistence
 * @return array persistence
 */
function commitAjaxPrepSql($persistence)
{
    return preflightCheckJsonPrepSchemaCheck($persistence, false);
}


/**
 * handles post-install tasks
 */
function commitAjaxPostInstall($persistence)
{
    $errors = [];
    global $mod_strings;
    global $sugar_config;
    global $sugar_version;

    if (empty($sugar_version)) {
        require('sugar_version.php');
    }

    // update versions info
    if (!updateVersions($sugar_version)) {
        echo $mod_strings['ERR_UW_COMMIT_UPDATE_VERSIONS'];
    }

    logThis('Starting post_install()...');
    $postInstallResults = "<b>{$mod_strings['LBL_UW_COMMIT_POSTINSTALL_RESULTS']}</b><br />
							<a href='javascript:toggleNwFiles(\"postInstallResults\");'>{$mod_strings['LBL_UW_SHOW']}</a><br />
							<div id='postInstallResults' style='display:none'>";
    $file = $persistence['unzip_dir']. "/" . constant('SUGARCRM_POST_INSTALL_FILE');
    if (is_file($file)) {
        include($file);
        ob_start();
        post_install();
    }

    require("sugar_version.php");

    if (!rebuildConfigFile($sugar_config, $sugar_version)) {
        logThis('*** ERROR: could not write config.php! - upgrade will fail!');
        $errors[] = $mod_strings['ERR_UW_CONFIG_WRITE'];
    }

    $res = ob_get_contents();
    $postInstallResults .= (empty($res)) ? $mod_strings['LBL_UW_SUCCESS'] : $res;
    $postInstallResults .= "</div>";

    ob_start();
    echo $postInstallResults;
    ob_flush();

    logThis('post_install() done.');
}
////	END COMMIT AJAX
///////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////
////	PREFLIGHT JSON STYLE

function preflightCheckJsonFindUpgradeFiles($persistence)
{
    $manifest = [];
    global $sugar_config;
    global $mod_strings;

    unset($persistence['rebuild_relationships']);
    unset($persistence['rebuild_extensions']);

    // don't bother if are rechecking
    $manualDiff			= array();
    if (!isset($persistence['unzip_dir']) || empty($persistence['unzip_dir'])) {
        logThis('unzipping files in upgrade archive...');

        $errors					= array();
        $base_upgrade_dir      = "upload://upgrades";
        $base_tmp_upgrade_dir  = sugar_cached("upgrades/temp");
        $install_file			= urldecode($persistence['install_file']);
        $show_files				= true;
        $unzip_dir				= mk_temp_dir($base_tmp_upgrade_dir);
        $zip_from_dir			= ".";
        $zip_to_dir			= ".";
        $zip_force_copy			= array();

        unzip($install_file, $unzip_dir);

        // assumption -- already validated manifest.php at time of upload
        include("$unzip_dir/manifest.php");

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
        if (!is_writable("config.php")) {
            logThis('BAD error');
            return $mod_strings['ERR_UW_CONFIG'];
        }

        logThis('setting "unzip_dir" to '.$unzip_dir);
        $persistence['unzip_dir'] = clean_path($unzip_dir);
        $persistence['zip_from_dir'] = clean_path($zip_from_dir);

        logThis('unzip done.');
    } else {
        $unzip_dir = $persistence['unzip_dir'];
        $zip_from_dir = $persistence['zip_from_dir'];
    }

    $persistence['upgrade_files'] = uwFindAllFiles(clean_path("$unzip_dir/$zip_from_dir"), array(), true, array(), true);

    return $persistence;
}

function preflightCheckJsonDiffFiles($persistence)
{
    global $sugar_version;
    global $mod_strings;

    if (empty($sugar_version)) {
        require('sugar_version.php');
    }

    // get md5 sums
    $md5_string = array();
    $finalZipDir = $persistence['unzip_dir'].'/'.$persistence['zip_from_dir'];

    if (is_file(getcwd().'/files.md5')) {
        require(getcwd().'/files.md5');
    }

    // initialize pass array
    $manualDiff = array();

    // file preflight checks
    logThis('verifying md5 checksums for files...');
    $cache_html_files = findAllFilesRelative(sugar_cached("layout"), array());

    foreach ($persistence['upgrade_files'] as $file) {
        if (strpos((string) $file, '.md5')) {
            continue;
        } // skip md5 file

        // normalize file paths
        $file = clean_path($file);

        // check that we can move/delete the upgraded file
        if (!is_writable($file)) {
            $errors[] = $mod_strings['ERR_UW_FILE_NOT_WRITABLE'].": ".$file;
        }
        // check that destination files are writable
        $destFile = getcwd().str_replace($finalZipDir, '', (string) $file);

        if (is_file($destFile)) { // of course it needs to exist first...
            if (!is_writable($destFile)) {
                $errors[] = $mod_strings['ERR_UW_FILE_NOT_WRITABLE'].": ".$destFile;
            }
        }

        ///////////////////////////////////////////////////////////////////////
        ////	DIFFS
        // compare md5s and build up a manual merge list
        $targetFile = clean_path(".".str_replace(getcwd(), '', $destFile));
        $targetMd5 = '0';
        if (is_file($destFile)) {
            if (strpos((string) $targetFile, '.php')) {
                // handle PHP files that were hit with the security regex
                $filesize = filesize($destFile);
                if ($filesize > 0) {
                    $fileContents = file_get_contents($destFile);
                    $targetMd5 = md5($fileContents);
                }
            } else {
                $targetMd5 = md5_file($destFile);
            }
        }

        if (isset($md5_string[$targetFile]) && $md5_string[$targetFile] != $targetMd5) {
            logThis('found a file with a differing md5: ['.$targetFile.']');
            $manualDiff[] = $destFile;
        }
        ////	END DIFFS
        ///////////////////////////////////////////////////////////////////////
        echo ".";
    }
    logThis('md5 verification done.');

    $persistence['manual'] = $manualDiff;
    $persistence['diff_errors'] = $errors;

    return $persistence;
}


function preflightCheckJsonGetDiff($persistence)
{
    global $mod_strings;
    global $current_user;

    $out  = $mod_strings['LBL_UW_PREFLIGHT_TESTS_PASSED'];
    $stop = false;

    $disableEmail = (empty($current_user->email1)) ? 'DISABLED' : 'CHECKED';

    if ((is_countable($persistence['manual']) ? count($persistence['manual']) : 0) > 0) {
        $preserveFiles = array();

        $diffs =<<<eoq
			<script type="text/javascript" language="Javascript">
				function preflightToggleAll(cb) {
					var checkAll = false;
					var form = document.getElementById('diffs');

					if(cb.checked == true) {
						checkAll = true;
					}

					for(i=0; i<form.elements.length; i++) {
						if(form.elements[i].type == 'checkbox') {
							form.elements[i].checked = checkAll;
						}
					}
					return;
				}
			</script>

			<table cellpadding='0' cellspacing='0' border='0'>
				<tr>
					<td valign='top'>
						<input type='checkbox' name='addTask' id='addTask' CHECKED>
					</td>
					<td valign='top'>
						{$mod_strings['LBL_UW_PREFLIGHT_ADD_TASK']}
					</td>
				</tr>
				<tr>
					<td valign='top'>
						<input type='checkbox' name='addEmail' id='addEmail' $disableEmail>
					</td>
					<td valign='top'>
						{$mod_strings['LBL_UW_PREFLIGHT_EMAIL_REMINDER']}
					</td>
				</tr>
			</table>

			<form name='diffs' id='diffs'>
			<p><a href='javascript:void(0); toggleNwFiles("diffsHide");'>{$mod_strings['LBL_UW_SHOW_DIFFS']}</a></p>
			<div id='diffsHide' style='display:none'>
				<table cellpadding='0' cellspacing='0' border='0'>
					<tr>
						<td valign='top' colspan='2'>
							{$mod_strings['LBL_UW_PREFLIGHT_FILES_DESC']}
							<br />&nbsp;
						</td>
					</tr>
					<tr>
						<td valign='top' colspan='2'>
							<input type='checkbox' onchange='preflightToggleAll(this);'>&nbsp;<i><b>{$mod_strings['LBL_UW_PREFLIGHT_TOGGLE_ALL']}</b></i>
							<br />&nbsp;
						</td>
					</tr>
eoq;
        foreach ($persistence['manual'] as $diff) {
            $diff = clean_path($diff);
            $persistence['files']['manual'][] = $diff;

            $checked = (isAutoOverwriteFile($diff)) ? 'CHECKED' : '';

            if (empty($checked)) {
                $preserveFiles[] = $diff;
            }

            $diffs .= "<tr><td valign='top'>";
            $diffs .= "<input type='checkbox' name='diff_files[]' value='{$diff}' $checked>";
            $diffs .= "</td><td valign='top'>";
            $diffs .= str_replace(getcwd(), '.', (string) $diff);
            $diffs .= "</td></tr>";
        }
        $diffs .= "</table>";
        $diffs .= "</div></p>";
        $diffs .= "</form>";

        // list preserved files (templates, etc.)
        $preserve = '';
        foreach ($preserveFiles as $pf) {
            if (empty($preserve)) {
                $preserve .= "<table cellpadding='0' cellspacing='0' border='0'><tr><td><b>";
                $preserve .= $mod_strings['LBL_UW_PREFLIGHT_PRESERVE_FILES'];
                $preserve .= "</b></td></tr>";
            }
            $preserve .= "<tr><td valign='top'><i>".str_replace(getcwd(), '.', (string) $pf)."</i></td></tr>";
        }
        if (!empty($preserve)) {
            $preserve .= '</table><br>';
        }
        $diffs = $preserve.$diffs;
    } else { // NO FILE DIFFS REQUIRED
        $diffs = $mod_strings['LBL_UW_PREFLIGHT_NO_DIFFS'];
    }

    echo $diffs;

    return $persistence;
}

/**
 * loads the sql file into an array
 * @param array persistence
 * @param bool preflight Flag to load for Preflight or Commit
 * @return array persistence
 */
function preflightCheckJsonPrepSchemaCheck($persistence, $preflight=true)
{
    global $mod_strings;
    $db = DBManagerFactory::getInstance();
    global $sugar_db_version;
    global $manifest;

    unset($persistence['sql_to_run']);

    $persistence['sql_to_check'] = array();
    $persistence['sql_to_check_backup'] = array();

    if (isset($persistence['sql_check_done'])) {
        // reset flag to not check (on Recheck)
        unset($persistence['sql_check_done']);
        unset($persistence['sql_errors']);
    }

    // get schema script if not loaded
    if ($preflight) {
        logThis('starting schema preflight check...');
    } else {
        logThis('Preparing SQL statements for sequential execution...');
    }

    if (empty($sugar_db_version)) {
        include('sugar_version.php');
    }

    if (!isset($manifest['version']) || empty($manifest['version'])) {
        include($persistence['unzip_dir'].'/manifest.php');
    }

    $origVersion = implodeVersion($sugar_db_version);
    $destVersion = implodeVersion($manifest['version']);

    $script_name = $db->getScriptType();
    $sqlScript = $persistence['unzip_dir']."/scripts/{$origVersion}_to_{$destVersion}_{$script_name}.sql";

    $newTables = array();

    logThis('looking for schema script at: '.$sqlScript);
    if (is_file($sqlScript)) {
        logThis('found schema upgrade script: '.$sqlScript);
        $fp = sugar_fopen($sqlScript, 'r');

        if (!empty($fp)) {
            $completeLine = '';
            while ($line = fgets($fp)) {
                if (strpos($line, '--') === false) {
                    $completeLine .= " ".trim($line);
                    if (strpos($line, ';') !== false) {
                        $completeLine = str_replace(';', '', $completeLine);
                        $persistence['sql_to_check'][] = $completeLine;
                        $completeLine = ''; //reset for next loop
                    }
                }
            }

            $persistence['sql_total'] = is_countable($persistence['sql_to_check']) ? count($persistence['sql_to_check']) : 0;
        } else {
            logThis('*** ERROR: could not read schema script: '.$sqlScript);
            $persistence['sql_errors'][] = $mod_strings['ERR_UW_FILE_NOT_READABLE'].'::'.$sqlScript;
        }
    }

    // load a new array if for commit
    if ($preflight) {
        $persistence['sql_to_check_backup'] = $persistence['sql_to_check'];
        $persistence['sql_to_run'] = $persistence['sql_to_check'];
        echo "1% ".$mod_strings['LBL_UW_DONE'];
    } else {
        $persistence['sql_to_run'] = $persistence['sql_to_check'];
        unset($persistence['sql_to_check']);
    }

    return $persistence;
}

function preflightCheckJsonSchemaCheck($persistence)
{
    global $mod_strings;
    $db = DBManagerFactory::getInstance();

    if (!isset($persistence['sql_check_done']) || $persistence['sql_check_done'] != true) {
        // must keep sql in order
        $completeLine = array_shift($persistence['sql_to_check']);
        $whatsLeft = count($persistence['sql_to_check']);

        // populate newTables array to prevent "getting sample data" from non-existent tables
        $newTables = array();
        if (strtoupper(substr((string) $completeLine, 1, 5)) == 'CREAT') {
            $newTables[] = getTableFromQuery($completeLine);
        }

        logThis('Verifying statement: '.$completeLine);
        $bad = $db->verifySQLStatement($completeLine, $newTables);

        if (!empty($bad)) {
            logThis('*** ERROR: schema change script has errors: '.$completeLine);
            logThis('*** '.$bad);
            $persistence['sql_errors'][] = getFormattedError($bad, $completeLine);
        }

        $persistence = ajaxSqlProgress($persistence, $completeLine, 'sql_to_check');
    } else {
        $persistence['sql_to_check'] = $persistence['sql_to_check_backup'];
        echo 'done';
    }

    return $persistence;
}


function preflightCheckJsonGetSchemaErrors($persistence)
{
    global $mod_strings;

    if (isset($persistence['sql_errors']) && (is_countable($persistence['sql_errors'] > 0) ? count($persistence['sql_errors'] > 0) : 0)) {
        $out = "<b class='error'>{$mod_strings['ERR_UW_PREFLIGHT_ERRORS']}:</b> ";
        $out .= "<a href='javascript:void(0);toggleNwFiles(\"sqlErrors\");'>{$mod_strings['LBL_UW_SHOW_SQL_ERRORS']}</a><div id='sqlErrors' style='display:none'>";
        foreach ($persistence['sql_errors'] as $sqlError) {
            $out .= "<br><span class='error'>{$sqlError}</span>";
        }
        $out .= "</div><hr />";
    } else {
        $out = '';
    }

    // reset errors if Rechecking
    if (isset($persistence['sql_errors'])) {
        //unset($persistence['sql_errors']);

        echo $out;
    }

    return $persistence;
}


function preflightCheckJsonFillSchema()
{
    global $mod_strings;
    global $persistence;
    global $sugar_db_version;
    global $manifest;
    $db = DBManagerFactory::getInstance();

    if (empty($sugar_db_version)) {
        include('sugar_version.php');
    }
    if (empty($manifest)) {
        include($persistence['unzip_dir'].'/manifest.php');
    }

    ///////////////////////////////////////////////////////////////////////////////
    ////	SCHEMA SCRIPT HANDLING
    $schema = '';
    $alterTableSchemaOut = '';

    $origVersion = implodeVersion($sugar_db_version);
    $destVersion = implodeVersion($manifest['version']);

    $script_name = $db->getScriptType();
    $sqlScript = $persistence['unzip_dir']."/scripts/{$origVersion}_to_{$destVersion}_{$script_name}.sql";
    $newTables = array();

    logThis('looking for SQL script for DISPLAY at '.$sqlScript);
    if (file_exists($sqlScript)) {
        $contents = sugar_file_get_contents($sqlScript);
        $schema  = "<p><a href='javascript:void(0); toggleNwFiles(\"schemashow\");'>{$mod_strings['LBL_UW_SHOW_SCHEMA']}</a>";
        $schema .= "<div id='schemashow' style='display:none;'>";
        $schema .= "<textarea readonly cols='80' rows='10'>{$contents}</textarea>";
        $schema .= "</div></p>";
    }
    ////	END SCHEMA SCRIPT HANDLING
    ///////////////////////////////////////////////////////////////////////////////

    ob_start();
    echo $schema;
    ob_flush();
}


function preflightCheckJsonAlterTableCharset()
{
    global $mod_strings;
    global $sugar_db_version;
    global $persistence;

    if (empty($sugar_db_version)) {
        include('sugar_version.php');
    }

    $alterTableSchema = '<i>'.$mod_strings['LBL_UW_PREFLIGHT_NOT_NEEDED'].'</i>';

    ob_start();
    echo $alterTableSchema;
    ob_flush();
}


///////////////////////////////////////////////////////////////////////////////
////	SYSTEMCHECK AJAX FUNCTIONS

function systemCheckJsonGetFiles($persistence)
{
    global $sugar_config;
    global $mod_strings;

    // add directories here that should be skipped when doing file permissions checks (cache/upload is the nasty one)
    $skipDirs = array(
        $sugar_config['upload_dir'],
        'themes',
    );

    if (!isset($persistence['dirs_checked'])) {
        $the_array = array();
        $files = array();
        $dir = getcwd();
        $d = dir($dir);
        while ($f = $d->read()) {
            if ($f == "." || $f == "..") { // skip *nix self/parent
                continue;
            }

            if (is_dir("$dir/$f")) {
                $the_array[] = clean_path("$dir/$f");
            } else {
                $files[] = clean_path("$dir/$f");
            }
        }
        $persistence['files_to_check'] = $files;
        $persistence['dirs_to_check'] = $the_array;
        $persistence['dirs_total']	= count($the_array);
        $persistence['dirs_checked'] = false;

        $out = "1% {$mod_strings['LBL_UW_DONE']}";

        return $persistence;
    } elseif ($persistence['dirs_checked'] == false) {
        $dir = array_pop($persistence['dirs_to_check']);

        $files = uwFindAllFiles($dir, array(), true, $skipDirs);

        $persistence['files_to_check'] = array_merge($persistence['files_to_check'], $files);

        $whatsLeft = is_countable($persistence['dirs_to_check']) ? count($persistence['dirs_to_check']) : 0;

        if (!isset($persistence['dirs_to_check']) || $whatsLeft < 1) {
            $whatsLeft = 0;
            $persistence['dirs_checked'] = true;
        }

        $out  = round((($persistence['dirs_total'] - $whatsLeft) / 21) * 100, 1)."% {$mod_strings['LBL_UW_DONE']}";
        $out .= " [{$mod_strings['LBL_UW_SYSTEM_CHECK_CHECKING_JSON']} {$dir}]";
    } else {
        $out = "Done";
    }

    echo trim($out);

    return $persistence;
}



/**
 * checks files for permissions
 * @param array files Array of files with absolute paths
 * @return string result of check
 */
function systemCheckJsonCheckFiles($persistence)
{
    global $mod_strings;
    global $persistence;

    $filesNotWritable = array();
    $i=0;
    $filesOut = "
		<a href='javascript:void(0); toggleNwFiles(\"filesNw\");'>{$mod_strings['LBL_UW_SHOW_NW_FILES']}</a>
		<div id='filesNw' style='display:none;'>
		<table cellpadding='3' cellspacing='0' border='0'>
		<tr>
			<th align='left'>{$mod_strings['LBL_UW_FILE']}</th>
			<th align='left'>{$mod_strings['LBL_UW_FILE_PERMS']}</th>
			<th align='left'>{$mod_strings['LBL_UW_FILE_OWNER']}</th>
			<th align='left'>{$mod_strings['LBL_UW_FILE_GROUP']}</th>
		</tr>";

    $isWindows = is_windows();
    foreach ($persistence['files_to_check'] as $file) {

        // admin deletes a bad file mid-check:
        if (!file_exists($file)) {
            continue;
        }
        if (!is_writable($file)) {
            logThis('File ['.$file.'] not writable - saving for display');
            $filesNotWritable[$i] = $file;
            $perms = substr(sprintf('%o', fileperms($file)), -4);
            $owner = fileowner($file);
            $group = filegroup($file);
            if (!$isWindows && function_exists('posix_getpwuid')) {
                $ownerData = posix_getpwuid($owner);
                $owner = !empty($ownerData) ? $ownerData['name'] : $owner;
            }
            if (!$isWindows && function_exists('posix_getgrgid')) {
                $groupData = posix_getgrgid($group);
                $group = !empty($groupData) ? $groupData['name'] : $group;
            }
            $filesOut .= "<tr>" .
                "<td><span class='error'>{$file}</span></td>" .
                "<td>{$perms}</td>" .
                "<td>{$owner}</td>" .
                "<td>{$group}</td>" .
                "</tr>";
        }

        $i++;
    }

    $filesOut .= '</table></div>';
    // not a stop error
    $persistence['filesNotWritable'] = (count($filesNotWritable) > 0) ? true : false;

    if (count($filesNotWritable) < 1) {
        $filesOut = (string)($mod_strings['LBL_UW_FILE_NO_ERRORS']);
        $persistence['step']['systemCheck'] = 'success';
    }

    echo $filesOut;
    return $persistence;
}
