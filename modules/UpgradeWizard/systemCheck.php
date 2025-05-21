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


logThis('[At systemCheck.php]');

$stop = false; // flag to prevent going to next step

//FILE CHECKS
logThis('Starting file permission check...');
$filesNotWritable = [];
$baseDirectory = getcwd();
$isWindows = is_windows();

$includeDirs = [
    'Api',
    'cache',
    'custom',
    'data',
    'include',
    'install',
    'jssource',
    'lib',
    'metadata',
    'ModuleInstall',
    'modules',
    'service',
    'soap',
    'themes',
    'XTemplate',
    'Zend',
    'vendor'
];

$skipDirs = [
    $sugar_config['upload_dir'],
    '.svn',
    '.git'
];
/**
 * @param SplFileInfo $file
 * @param mixed $key
 * @param RecursiveCallbackFilterIterator $iterator
 * @return bool
 */
$fileCheck = function ($file, $key, $iterator) use ($baseDirectory, $skipDirs, $includeDirs) {
    $subDir = explode(DIRECTORY_SEPARATOR, str_replace($baseDirectory . DIRECTORY_SEPARATOR, '', (string) $key));
    if ($iterator->hasChildren() &&
        !in_array($file->getFilename(), $skipDirs, true) &&
        (empty($subDir) || in_array($subDir[0], $includeDirs, true))
    ) {
        return true;
    }

    return $file->isFile() && !$file->isWritable();
};

/** @var SplFileInfo[] $files */
$files = new RecursiveIteratorIterator(
    new RecursiveCallbackFilterIterator(
        new RecursiveDirectoryIterator(
            clean_path($baseDirectory),
            RecursiveDirectoryIterator::SKIP_DOTS
        ),
        $fileCheck
    )
);

$i = 0;
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

foreach ($files as $file) {
    logThis('File [' . $file->getPathname() . '] not writable - saving for display');

    $filesNotWritable[$i] = $file->getPathname();
    $perms = substr(sprintf('%o', $file->getPerms()), -4);
    $owner = $file->getOwner();
    $group = $file->getGroup();
    if (!$isWindows && function_exists('posix_getpwuid')) {
        $ownerData = posix_getpwuid($owner);
        $owner = !empty($ownerData) ? $ownerData['name'] : $owner;
    }
    if (!$isWindows && function_exists('posix_getgrgid')) {
        $groupData = posix_getgrgid($group);
        $group = !empty($groupData) ? $groupData['name'] : $group;
    }
    $filesOut .= "<tr>" .
        "<td><span class='error'>{$file->getFilename()}</span></td>" .
        "<td>{$perms}</td>" .
        "<td>{$owner}</td>" .
        "<td>{$group}</td>" .
        "</tr>";

    $i++;
}

$filesOut .= '</table></div>';
$errors['files']['filesNotWritable'] = true;

if (count($filesNotWritable) < 1) {
    $filesOut = "<b>{$mod_strings['LBL_UW_FILE_NO_ERRORS']}</b>";
    $errors['files']['filesNotWritable'] = false;
}

logThis('Finished file permission check.');


///////////////////////////////////////////////////////////////////////////////
////	DATABASE CHECKS
logThis('Starting database permissions check...');
$dbOut = "
	<a href='javascript:void(0); toggleNwFiles(\"dbPerms\");'>{$mod_strings['LBL_UW_SHOW_DB_PERMS']}</a>
	<div id='dbPerms' style='display:none;'>
	<table cellpadding='3' cellspacing='0' border='0'>
	<tr>
		<th align='left'>{$mod_strings['LBL_UW_DB_PERMS']}</th>
	</tr>";

$db = DBManagerFactory::getInstance();
$outs = array();
$outs['skip'] = false;
$outs['db'] = array();
$outs['dbOut'] = $dbOut;
$outs = testPermsCreate($db, $outs);
$outs = testPermsInsert($db, $outs, $outs['skip']);
$outs = testPermsUpdate($db, $outs, $outs['skip']);
$outs = testPermsSelect($db, $outs, $outs['skip']);
$outs = testPermsDelete($db, $outs, $outs['skip']);
$outs = testPermsAlterTableAdd($db, $outs, $outs['skip']);
$outs = testPermsAlterTableChange($db, $outs, $outs['skip']);
$outs = testPermsAlterTableDrop($db, $outs, $outs['skip']);
$outs = testPermsDropTable($db, $outs, $outs['skip']);
$outs['dbOut'] .= '</table>';


if ((is_countable($outs['db']) ? count($outs['db']) : 0) < 1) {
    logThis('No permissions errors found!');
    $outs['dbOut'] = "<b>".$mod_strings['LBL_UW_DB_NO_ERRORS']."</b>";
}
logThis('Finished database permissions check.');
$dbOut = $outs['dbOut'];
////	END DATABASE CHECKS
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////	INSTALLER TYPE CHECKS
$result = checkSystemCompliance();
$checks = array(
    'phpVersion'				=> $mod_strings['LBL_UW_COMPLIANCE_PHP_VERSION'],
    'dbVersion'                 => $mod_strings['LBL_UW_COMPLIANCE_DB'],
    'xmlStatus'					=> $mod_strings['LBL_UW_COMPLIANCE_XML'],
    'curlStatus'				=> $mod_strings['LBL_UW_COMPLIANCE_CURL'],
    'imapStatus'				=> $mod_strings['LBL_UW_COMPLIANCE_IMAP'],
    'mbstringStatus'			=> $mod_strings['LBL_UW_COMPLIANCE_MBSTRING'],
    'callTimeStatus'			=> $mod_strings['LBL_UW_COMPLIANCE_CALLTIME'],
    'memory_msg'				=> $mod_strings['LBL_UW_COMPLIANCE_MEMORY'],
    'stream_msg'                => $mod_strings['LBL_UW_COMPLIANCE_STREAM'],
    'ZipStatus'			        => $mod_strings['LBL_UW_COMPLIANCE_ZIPARCHIVE'],
    'pcreVersion'			    => $mod_strings['LBL_UW_COMPLIANCE_PCRE_VERSION'],
    //commenting mbstring overload.
    //'mbstring.func_overload'	=> $mod_strings['LBL_UW_COMPLIANCE_MBSTRING_FUNC_OVERLOAD'],
);
if ($result['error_found'] == true || !empty($result['warn_found'])) {
    if ($result['error_found']) {
        $stop = true;
    }
    $phpIniLocation = get_cfg_var("cfg_file_path");

    $sysCompliance  = "<a href='javascript:void(0); toggleNwFiles(\"sysComp\");'>{$mod_strings['LBL_UW_SHOW_COMPLIANCE']}</a>";
    $sysCompliance .= "<div id='sysComp' >";
    $sysCompliance .= "<table cellpadding='0' cellspacing='0' border='0'>";
    foreach ($result as $k => $v) {
        if ($k == 'error_found') {
            continue;
        }
        $sysCompliance .= "<tr><td valign='top'>{$checks[$k]}</td>";
        $sysCompliance .= "<td valign='top'>{$v}</td></tr>";
    }
    $sysCompliance .= "<tr><td valign='top'>{$mod_strings['LBL_UW_COMPLIANCE_PHP_INI']}</td>";
    $sysCompliance .= "<td valign='top'><b>{$phpIniLocation}</b></td></tr>";
    $sysCompliance .= "</table></div>";
} else {
    $sysCompliance = "<b>{$mod_strings['LBL_UW_COMPLIANCE_ALL_OK']}</b>";
}

////	END INSTALLER CHECKS
///////////////////////////////////////////////////////////////////////////////

////	stop on all errors
foreach ($errors as $k => $type) {
    if (is_array($type) && count($type) > 0) {
        foreach ($type as $k => $subtype) {
            if ($subtype == true) {
                $stop = true;
            }
        }
    }

    if ($type === true) {
        logThis('Found errors during system check - disabling forward movement.');
        $stop = true;
    }
}

$GLOBALS['top_message'] = (string)($mod_strings['LBL_UW_NEXT_TO_UPLOAD']);
$showBack		= true;
$showCancel		= true;
$showRecheck	= true;
$showNext		= ($stop) ? false : true;

$stepBack		= $_REQUEST['step'] - 1;
$stepNext		= $_REQUEST['step'] + 1;
$stepCancel		= -1;
$stepRecheck	= $_REQUEST['step'];

$_SESSION['step'][$steps['files'][$_REQUEST['step']]] = ($stop) ? 'failed' : 'success';


///////////////////////////////////////////////////////////////////////////////
////	OUTPUT

$uwMain =<<<eoq
<style>
.stop {
	color: #cc0000;
	}
.go {
	color: #00cc00;
	}

</style>
<table cellpadding="3" cellspacing="4" border="0">
	<tr>
		<td align="left" valign="top">
			{$mod_strings['LBL_UW_FILE_ISSUES_PERMS']}:
		</td>
		<td>
			{$filesOut}
		</td>
	</tr>
	<tr>
		<td colspan="2">
			<p>&nbsp;</p>
		</td>
	</tr>
	<tr>
		<td align="left" valign="top">
			{$mod_strings['LBL_UW_DB_ISSUES_PERMS']}:
		</td>
		<td>
			{$dbOut}
		</td>
	</tr>
	<tr>
		<td colspan="2">
			<p>&nbsp;</p>
		</td>
	</tr>
	<tr>
		<td align="left" valign="top">
			{$mod_strings['LBL_UW_COMPLIANCE_TITLE2']}:
		</td>
		<td>
			{$sysCompliance}
		</td>
	</tr>
</table>
<div id="upgradeDiv" style="display:none">
    <table border="0" cellspacing="0" cellpadding="0">
        <tr><td>
           <p><!--not_in_theme!--><img src='modules/UpgradeWizard/processing.gif' alt='Processing'> <br></p>
        </td></tr>
     </table>
 </div>
eoq;
