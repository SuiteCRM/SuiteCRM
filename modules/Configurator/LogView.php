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



global $mod_strings;
if (!is_admin($current_user)) {
    sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
}
$filter = '';
if (!empty($_REQUEST['filter'])) {
    $filter = 	$_REQUEST['filter'];
}
$ignore_self = false;
if (!empty($_REQUEST['ignore_self'])) {
    $ignore_self = 'checked';
}
$reg_ex = false;
if (!empty($_REQUEST['reg_ex'])) {
    $reg_ex = 'checked';
}
set_time_limit(180);
echo <<<EOQ
<form action='index.php' name='logview'>
<input type='hidden' name='action' value='LogView'>
<input type='hidden' name='module' value='Configurator'>
<input type='hidden' name='doaction' value=''>
<input type='button' onclick='document.logview.doaction.value="all";document.logview.submit()' name='all' value='{$mod_strings['LBL_ALL']}'>
<input type='button' onclick='document.logview.doaction.value="mark";document.logview.submit()' name='mark' value='{$mod_strings['LBL_MARK_POINT']}'>
<input type='submit' name='display' value='{$mod_strings['LBL_REFRESH_FROM_MARK']}'>
<input type='button' onclick='document.logview.doaction.value="next";document.logview.submit()' name='next' value='{$mod_strings['LBL_NEXT_']}'>
<br>
{$mod_strings['LBL_SEARCH']} <input type='text' name='filter' value='$filter'>&nbsp;{$mod_strings['LBL_REG_EXP']} <input type='checkbox' name='reg_ex' $reg_ex>
<br>
{$mod_strings['LBL_IGNORE_SELF']} <input type='checkbox' name='ignore_self' $ignore_self>
</form>
EOQ;

define('PROCESS_ID', 1);
define('LOG_LEVEL', 2);
define('LOG_NAME', 3);
define('LOG_DATA', 4);

// bug 53041 - now that we are respecting file name suffixes for log files, we need to get the log file name properly
$config = SugarConfig::getInstance();
$ext = $config->get('logger.file.ext');
$logfile = $config->get('logger.file.name');
$log_dir = $config->get('log_dir');
$log_dir = $log_dir . (empty($log_dir)?'':'/');
$file_suffix = $config->get('logger.file.suffix');
$date_suffix = "";
if (!empty($file_suffix)) {
    $date_suffix = "_" . date(str_replace("%", "", (string) $file_suffix));
}

$logFile = $log_dir . $logfile . $date_suffix . $ext;

if (!file_exists($logFile)) {
    die('No Log File');
}
$lastMatch = false;
$doaction =(!empty($_REQUEST['doaction']))?$_REQUEST['doaction']:'';

switch ($doaction) {
    case 'mark':
        echo "<h3>{$mod_strings['LBL_MARKING_WHERE_START_LOGGING']}</h3><br>";
        $_SESSION['log_file_size'] = filesize($logFile);
        break;
    case 'next':
        if (!empty($_SESSION['last_log_file_size'])) {
            $_SESSION['log_file_size'] = $_SESSION['last_log_file_size'];
        } else {
            $_SESSION['log_file_size'] = 0;
        }
        $_REQUEST['display'] = true;
        break;
    case 'all':
        $_SESSION['log_file_size'] = 0;
        $_REQUEST['display'] = true;
        break;
}


if (!empty($_REQUEST['display'])) {
    echo "<h3>{$mod_strings['LBL_DISPLAYING_LOG']}</h3>";
    $process_id =  getmypid();

    echo $mod_strings['LBL_YOUR_PROCESS_ID'].' [' . $process_id. ']';
    echo '<br>'.$mod_strings['LBL_YOUR_IP_ADDRESS'].' ' . $_SERVER['REMOTE_ADDR'];
    if ($ignore_self) {
        echo $mod_strings['LBL_IT_WILL_BE_IGNORED'];
    }
    if (empty($_SESSION['log_file_size'])) {
        $_SESSION['log_file_size'] = 0;
    }
    $cur_size = filesize($logFile);
    $_SESSION['last_log_file_size'] = $cur_size;
    $pos = 0;
    if ($cur_size >= $_SESSION['log_file_size']) {
        $pos = $_SESSION['log_file_size'] - $cur_size;
    }
    if ($_SESSION['log_file_size'] == $cur_size) {
        echo $mod_strings['LBL_LOG_NOT_CHANGED'].'<br>';
    } else {
        $fp = sugar_fopen($logFile, 'r');
        fseek($fp, $pos, SEEK_END);
        echo '<pre>';
        while ($line = fgets($fp)) {
            $line = filter_var($line, FILTER_SANITIZE_SPECIAL_CHARS);
            //preg_match('/[^\]]*\[([0-9]*)\] ([a-zA-Z]+) ([a-zA-Z0-9\.]+) - (.*)/', $line, $result);
            preg_match('/[^\]]*\[([0-9]*)\]/', $line, $result);
            ob_flush();
            flush();
            if (empty($result) && $lastMatch) {
                echo $line;
            } else {
                $lastMatch = false;
                if (empty($result) || ($ignore_self &&$result[LOG_NAME] == $_SERVER['REMOTE_ADDR'])) {
                } else {
                    if (empty($filter) || (!$reg_ex && substr_count($line, (string) $filter) > 0) || ($reg_ex && preg_match($filter, $line))) {
                        $lastMatch = true;
                        echo $line;
                    }
                }
            }
        }
        echo '</pre>';
        fclose($fp);
    }
}
