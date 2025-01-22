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


#[\AllowDynamicProperties]
class ViewBackups extends SugarView
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;
        
        return array(
           "<a href='index.php?module=Administration&action=index'>".$mod_strings['LBL_MODULE_NAME']."</a>",
           $mod_strings['LBL_BACKUPS_TITLE']
           );
    }
    
    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;
        
        if (!is_admin($current_user)) {
            sugar_die("Unauthorized access to administration.");
        }
        if (isset($GLOBALS['sugar_config']['hide_admin_backup']) && $GLOBALS['sugar_config']['hide_admin_backup']) {
            sugar_die("Unauthorized access to backups.");
        }
    }
    
    /**
     * @see SugarView::display()
     */
    public function display()
    {
        require_once('include/utils/php_zip_utils.php');

        $form_action = "index.php?module=Administration&action=Backups";
        
        $backup_dir = "";
        $backup_zip = "";
        $run        = "confirm";
        $input_disabled = "";
        global $mod_strings;
        $errors = array();
        
        // process "run" commands
        if (isset($_REQUEST['run']) && ($_REQUEST['run'] != "")) {
            $run = $_REQUEST['run'];
        
            $backup_dir = $_REQUEST['backup_dir'];
            $backup_zip = $_REQUEST['backup_zip'];
            if (strpos((string) $backup_dir, 'phar://') !== false) {
                $errors[] = $mod_strings['LBL_BACKUP_DIRECTORY_WRITABLE'];

                return $errors;
            }
            if ($run == "confirm") {
                if ($backup_dir == "") {
                    $errors[] = $mod_strings['LBL_BACKUP_DIRECTORY_ERROR'];
                }
                if ($backup_zip == "") {
                    $errors[] = $mod_strings['LBL_BACKUP_FILENAME_ERROR'];
                }
        
                if (count($errors) > 0) {
                    return($errors);
                }
        
                if (!is_dir($backup_dir)) {
                    if (!mkdir_recursive($backup_dir)) {
                        $errors[] = $mod_strings['LBL_BACKUP_DIRECTORY_EXISTS'];
                    }
                }
        
                if (!is_writable($backup_dir)) {
                    $errors[] = $mod_strings['LBL_BACKUP_DIRECTORY_NOT_WRITABLE'];
                }
        
                if (is_file("$backup_dir/$backup_zip")) {
                    $errors[] = $mod_strings['LBL_BACKUP_FILE_EXISTS'];
                }
                if (is_dir("$backup_dir/$backup_zip")) {
                    $errors[] = $mod_strings['LBL_BACKUP_FILE_AS_SUB'];
                }
                if (count($errors) == 0) {
                    $run = "confirmed";
                    $input_disabled = "readonly";
                }
            } elseif ($run == "confirmed") {
                ini_set("memory_limit", "-1");
                ini_set("max_execution_time", "0");
                zip_dir(".", "$backup_dir/$backup_zip");
                $run = "done";
            }
        }
        if (count($errors) > 0) {
            foreach ($errors as $error) {
                print("<font color=\"red\">$error</font><br>");
            }
        }
        if ($run == "done") {
            $size = filesize("$backup_dir/$backup_zip");
            print($mod_strings['LBL_BACKUP_FILE_STORED'] . " $backup_dir/$backup_zip ($size bytes).<br>\n");
            print("<a href=\"index.php?module=Administration&action=index\">" . $mod_strings['LBL_BACKUP_BACK_HOME']. "</a>\n");
        } else {
            ?>
        
            <?php
            echo getClassicModuleTitle(
                "Administration",
                array(
                    "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
                   $mod_strings['LBL_BACKUPS_TITLE'],
                   ),
                false
                );
            echo $mod_strings['LBL_BACKUP_INSTRUCTIONS_1']; ?>
            <br>
            <?php echo $mod_strings['LBL_BACKUP_INSTRUCTIONS_2']; ?><br>
            <form action="<?php print($form_action); ?>" method="post">
            <table>
            <tr>
                <td><?php echo $mod_strings['LBL_BACKUP_DIRECTORY']; ?><br><i><?php echo $mod_strings['LBL_BACKUP_DIRECTORY_WRITABLE']; ?></i></td>
                <td><input size="100" type="input" name="backup_dir" <?php print($input_disabled); ?> value="<?php print($backup_dir); ?>"/></td>
            </tr>
            <tr>
                <td><?php echo $mod_strings['LBL_BACKUP_FILENAME']; ?></td>
                <td><input type="input" name="backup_zip" <?php print($input_disabled); ?> value="<?php print($backup_zip); ?>"/></td>
            </tr>
            </table>
            <input type=hidden name="run" value="<?php print($run); ?>" />
        
        <?php
            switch ($run) {
                case "confirm":
        ?>
                    <input type="submit" value="<?php echo $mod_strings['LBL_BACKUP_CONFIRM']; ?>" />
        <?php
                    break;
                case "confirmed":
        ?>
                    <?php echo $mod_strings['LBL_BACKUP_CONFIRMED']; ?><br>
                    <input type="submit" value="<?php echo $mod_strings['LBL_BACKUP_RUN_BACKUP']; ?>" />
        <?php
                    break;
            } ?>
        
            </form>
        
        <?php
        }   // end if/else of $run options
        $GLOBALS['log']->info("Backups");
    }
}
