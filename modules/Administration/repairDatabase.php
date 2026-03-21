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



global $current_user, $beanFiles, $sugar_config;
set_time_limit(3600);


$db = DBManagerFactory::getInstance();

if (is_admin($current_user) || isset($from_sync_client) || is_admin_for_any_module($current_user)) {
    isset($_REQUEST['execute'])? $execute=$_REQUEST['execute'] : $execute= false;
    $export = false;

    if (count($_POST) && isset($_POST['raction'])) {
        if (isset($_POST['raction']) && strtolower($_POST['raction']) == "export") {
            //jc - output buffering is being used. if we do not clean the output buffer
            //the contents of the buffer up to the length of the repair statement(s)
            //will be saved in the file...
            ob_clean();

            header("Content-Disposition: attachment; filename=repairSugarDB.sql");
            header("Content-Type: text/sql; charset={$app_strings['LBL_CHARSET']}");
            header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
            header("Last-Modified: " . TimeDate::httpTime());
            header("Cache-Control: post-check=0, pre-check=0", false);
            header("Content-Length: " . strlen((string) $_POST['sql']));

            //jc:7347 - for whatever reason, html_entity_decode is choking on converting
            //the html entity &#039; to a single quote, so we will use str_replace
            //instead
            $sql = str_replace(array('&#039;', '&#96;'), array("'", "`"), (string) $_POST['sql']);
            //echo html_entity_decode($_POST['sql']);
            echo $sql;
        } elseif (isset($_POST['raction']) && strtolower($_POST['raction']) == "execute") {
            $sql = str_replace(
                array(
                    "\n",
                    '&#039;',
                    '&#96;',
                ),
                array(
                    '',
                    "'",
                    "`"
                ),
                preg_replace('#(/\*.+?\*/\n*)#', '', (string) $_POST['sql'])
            );
            foreach (explode(";", $sql) as $stmt) {
                $stmt = trim($stmt);

                if (!empty($stmt)) {
                    $db->query($stmt, true, 'Executing repair query: ');
                }
            }

            echo "<h3>{$mod_strings['LBL_REPAIR_DATABASE_SYNCED']}</h3>";
        }
    } else {
        if (!$export && empty($_REQUEST['repair_silent'])) {
            if (empty($hideModuleMenu)) {
                echo getClassicModuleTitle($mod_strings['LBL_REPAIR_DATABASE'], array($mod_strings['LBL_REPAIR_DATABASE']), true);
            }
            echo "<h1 id=\"rdloading\">{$mod_strings['LBL_REPAIR_DATABASE_PROCESSING']}</h1>";
            ob_flush();
        }

        $sql = '';

        VardefManager::clearVardef();
        $repairedTables = array();

        foreach ($beanFiles as $bean => $file) {
            if (file_exists($file)) {
                require_once($file);
                unset($GLOBALS['dictionary'][$bean]);
                $focus = new $bean();
                if (($focus instanceof SugarBean) && !isset($repairedTables[$focus->table_name])) {
                    $sql .= $db->repairTable($focus, $execute);
                    $repairedTables[$focus->table_name] = true;
                }
                //Repair Custom Fields
                if (($focus instanceof SugarBean) && $focus->hasCustomFields() && !isset($repairedTables[$focus->table_name . '_cstm'])) {
                    $df = new DynamicField($focus->module_dir);
                    //Need to check if the method exists as during upgrade an old version of Dynamic Fields may be loaded.
                    if (method_exists($df, "repairCustomFields")) {
                        $df->bean = $focus;
                        $sql .= $df->repairCustomFields($execute);
                        $repairedTables[$focus->table_name . '_cstm'] = true;
                    }
                }
            }
        }

        $olddictionary = $dictionary;

        unset($dictionary);
        include('modules/TableDictionary.php');

        foreach ($dictionary as $meta) {
            if (!isset($meta['table']) || isset($repairedTables[$meta['table']])) {
                continue;
            }

            $tablename = $meta['table'];
            $fielddefs = $meta['fields'];
            $indices = $meta['indices'] ?? [];
            $engine = isset($meta['engine'])?$meta['engine']:null;
            $sql .= $db->repairTableParams($tablename, $fielddefs, $indices, $execute, $engine);
            $repairedTables[$tablename] = true;
        }

        $dictionary = $olddictionary;



        if (empty($_REQUEST['repair_silent'])) {
            echo "<script type=\"text/javascript\">document.getElementById('rdloading').style.display = \"none\";</script>";

            if (isset($sql) && !empty($sql)) {
                $qry_str = "";
                foreach (explode("\n", $sql) as $line) {
                    if (!empty($line) && substr($line, -2) != "*/") {
                        $line .= ";";
                    }

                    $qry_str .= $line . "\n";
                }

                $ss = new Sugar_Smarty();
                $ss->assign('MOD', $GLOBALS['mod_strings']);
                $ss->assign('qry_str', $qry_str);
                echo $ss->fetch('modules/Administration/templates/RepairDatabase.tpl');
            } else {
                echo "<h3>{$mod_strings['LBL_REPAIR_DATABASE_SYNCED']}</h3>";
            }
        }
    }
} else {
    sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
}
