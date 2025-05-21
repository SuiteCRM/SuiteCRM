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
$mod_strings_users = $mod_strings;

global $current_user;
if (!$GLOBALS['current_user']->isAdminForModule('Users')
  ) {
    sugar_die("You cannot access this page.");
}

global $locale;

$return_module = isset($_REQUEST['return_module']) ? $_REQUEST['return_module'] : '';
$return_action = isset($_REQUEST['return_action']) ? $_REQUEST['return_action'] : '';
$return_id = isset($_REQUEST['return_id']) ? $_REQUEST['return_id'] : '';
if (!empty($return_module)) {
    $cancel_location = "index.php?module=".$return_module."&action=".$return_action."&record=".$return_id;
} else {
    $cancel_location = "index.php?module=Users&action=index";
}

echo "<h2 class='moduleTitle' style=\"margin-bottom:0px;\">{$mod_strings_users['LBL_REASS_SCRIPT_TITLE']}</h2>";

// Include Metadata for processing
require_once("modules/Users/metadata/reassignScriptMetadata.php");
if (file_exists("custom/modules/Users/reassignScriptMetadata_override.php")) {
    include("custom/modules/Users/reassignScriptMetadata_override.php");
}

if (!empty($_GET['record'])) {
    unset($_SESSION['reassignRecords']);
    $_SESSION['reassignRecords']['fromuser'] = $_GET['record'];
}

if (!isset($_POST['fromuser']) && !isset($_GET['execute'])) {
    ///////////////////// BEGIN STEP 1 - Select users/modules /////////////////////////
    $exclude_modules = array(
        "ImportMap",
        "UsersLastImport",
        "Dashboard",
        "SavedSearch",
        "UserPreference",
        "SugarFavorites",
        'OAuthKey',
        'OAuthToken',
    );

    if (isset($_GET['clear']) && $_GET['clear'] == 'true') {
        unset($_SESSION['reassignRecords']);
    } ?>
<form method=post action="index.php?module=Users&action=reassignUserRecords" name='EditView' id='EditView'>
<table cellspacing='1' cellpadding='1' border='0'>
<tr>
    <td><?php echo $mod_strings_users['LBL_REASS_DESC_PART1']."<BR><br>"?></td>
</tr>
<tr>
<td>
<input type=submit class="button" value="<?php echo $mod_strings_users['LBL_REASS_BUTTON_CONTINUE']; ?>" name=steponesubmit>
&nbsp;<input type=button class="button" value="<?php echo $mod_strings_users['LBL_REASS_BUTTON_CLEAR']; ?>" onclick='clearCurrentRecords();'>
<input type=button class="button" value="<?php echo $app_strings['LBL_CANCEL_BUTTON_LABEL']; ?>" onclick='document.location="<?php echo $cancel_location ?>"'>
</td>
</tr>
</table>
<table border='0' cellspacing='0' cellpadding='0'  class='edit view'>
<tr>
<td>
<BR>
<?php echo $mod_strings_users['LBL_REASS_USER_FROM']; ?>
<BR>
<select name="fromuser" id='fromuser'>
<?php
$all_users = User::getAllUsers();
    //Bug 48697 - We need to display only active users as possible reassign targets
    $active_users = User::getActiveUsers();
    echo get_select_options_with_id($all_users, isset($_SESSION['reassignRecords']['fromuser']) ? $_SESSION['reassignRecords']['fromuser'] : ''); ?>
</select>
<BR>
<BR>
<?php echo $mod_strings_users['LBL_REASS_USER_TO']; ?>
<BR>
<select name="touser" id="touser">
<?php
if (isset($_SESSION['reassignRecords']['fromuser']) && isset($all_users[$_SESSION['reassignRecords']['fromuser']])) {
        unset($all_users[$_SESSION['reassignRecords']['fromuser']]);
    }

    echo get_select_options_with_id($active_users, isset($_SESSION['reassignRecords']['touser']) ? $_SESSION['reassignRecords']['touser'] : ''); ?>
</select>
<?php
?>
<BR>
<?php echo $mod_strings_users['LBL_REASS_MOD_REASSIGN']; ?>
<BR>
<select size="6" name='modules[]' multiple="true" id='modulemultiselect' onchange="updateDivDisplay(this);">
<?php
if (!isset($_SESSION['reassignRecords']['assignedModuleListCache'])) {
        $beanListDup = $beanList;
        foreach ($beanListDup as $m => $p) {
            if (empty($beanFiles[$p])) {
                unset($beanListDup[$m]);
            } else {
                require_once($beanFiles[$p]);
                $obj = new $p();
                if (!isset($obj->field_defs['assigned_user_id']) ||
                     (
                         isset($obj->field_defs['assigned_user_id']) &&
                    isset($obj->field_defs['assigned_user_id']['source']) &&
                    $obj->field_defs['assigned_user_id']['source'] == "non-db"
                )
              ) {
                    unset($beanListDup[$m]);
                }
            }
        }
        $beanListDup = array_diff($beanListDup, $exclude_modules);

        //Leon bug 20739
        $beanListDupDisp=array() ;
        foreach ($beanListDup as $m => $p) {
            if (isset($app_list_strings['moduleList'][$m])) {
                $beanListDupDisp[$app_list_strings['moduleList'][$m]]=$p;
            }
        }
        $_SESSION['reassignRecords']['assignedModuleListCache'] = $beanListDup;
        $_SESSION['reassignRecords']['assignedModuleListCacheDisp'] = $beanListDupDisp;
    }
    $beanListDup = array_flip($_SESSION['reassignRecords']['assignedModuleListCache']);
    $beanListFlip = array_flip($_SESSION['reassignRecords']['assignedModuleListCacheDisp']);
    asort($beanListFlip);
    $selected = array();
    if (!empty($_SESSION['reassignRecords']['modules'])) {
        foreach ($_SESSION['reassignRecords']['modules'] as $mod => $arr) {
            $selected[] = $mod;
        }
    }
    echo get_select_options_with_id($beanListFlip, $selected); ?>
</select>
<BR>
</td>
</tr>
<tr>
<td>
<?php
foreach ($moduleFilters as $modFilter => $fieldArray) {
        $display = (!empty($fieldArray['display_default']) && $fieldArray['display_default'] == true ? "block" : "none");
        //Leon bug 20739
        $t_mod_strings=return_module_language($GLOBALS['current_language'], $modFilter);
        echo "<div id=\"reassign_{$GLOBALS['beanList'][$modFilter]}\" style=\"display:$display\">\n";
        echo "<h5 style=\"padding-left:0px; margin-bottom:4px;\">{$app_list_strings['moduleList'][$modFilter]} ", " {$mod_strings_users['LBL_REASS_FILTERS']}</h5>\n";
        foreach ($fieldArray['fields'] as $meta) {
            $multi = "";
            $name = (!empty($meta['name']) ? $meta['name'] : "");
            $size = (!empty($meta['size']) ? "size=\"{$meta['size']}\"" : "");
            //Leon bug 20739
            echo $t_mod_strings[$meta['vname']] ."\n<BR>\n";
            switch ($meta['type']) {
            case "text":
                $tag = "input";
                break;
            case "multiselect":
                $multi = "multiple=\"true\"";
                $name .= "[]";
                // no break - Continue into select
            case "select":
                $tag = "select";
                $sel = '';
                if (!empty($_SESSION['reassignRecords']['filters'][$meta['name']])) {
                    $sel = $_SESSION['reassignRecords']['filters'][$meta['name']];
                }
                $extra = get_select_options_with_id($meta['dropdown'], $sel);
                $extra .= "\n</select>";
                break;
            default:
                break;
        }
            echo "<$tag $size name=\"$name\" $multi>\n$extra";
            echo "<BR>\n";
        }
        echo "</div>\n";
    } ?>
</td>
</tr>
</table>
<table cellspacing='1' cellpadding='1' border='0'>
<tr>
<td>
<input type=submit class="button" value="<?php echo $mod_strings_users['LBL_REASS_BUTTON_CONTINUE']; ?>" name=steponesubmit>
&nbsp;<input type=button class="button" value="<?php echo $mod_strings_users['LBL_REASS_BUTTON_CLEAR']; ?>" onclick='clearCurrentRecords();'>
<input type=button class="button" value="<?php echo $app_strings['LBL_CANCEL_BUTTON_LABEL']; ?>" onclick='document.location="<?php echo $cancel_location ?>"'>
</td>
</tr>
</table>
</form>

<?php
///////////////////// END STEP 1 - Select users/modules /////////////////////////
} elseif (!isset($_GET['execute'])) {
    ///////////////////// BEGIN STEP 2 - Confirm Selections /////////////////////////
    if (empty($_POST['modules'])) {
        sugar_die($mod_strings_users['ERR_REASS_SELECT_MODULE']);
    }
    if ($_POST['fromuser'] == $_POST['touser']) {
        sugar_die($mod_strings_users['ERR_REASS_DIFF_USERS']);
    }

    global $current_user;
    // Set the from and to user names so that we can display them in the results
    $fromUserNameQuoted = $this->db->quote($_POST['fromuser']);
    $toUserNameQuoted = $this->db->quote($_POST['touser']);

    $query = "select user_name, id from users where id in ('$fromUserNameQuoted', '$toUserNameQuoted')";
    $res = DBManagerFactory::getInstance()->query($query, true);
    while ($row = DBManagerFactory::getInstance()->fetchByAssoc($res)) {
        if ($row['id'] == $_POST['fromuser']) {
            $fromusername = $row['user_name'];
        }
        if ($row['id'] == $_POST['touser']) {
            $tousername = $row['user_name'];
        }
    }
    echo "{$mod_strings_users['LBL_REASS_DESC_PART2']}\n";
    echo "<form action=\"index.php?module=Users&action=reassignUserRecords&execute=true\" method=post>\n";
    echo "<BR>{$mod_strings_users['LBL_REASS_NOTES_TITLE']}\n";
    echo "<ul>\n";
    echo "<li>* {$mod_strings_users['LBL_REASS_NOTES_ONE']}\n";
    echo "<li>* {$mod_strings_users['LBL_REASS_NOTES_TWO']}\n";
    echo "<li>* {$mod_strings_users['LBL_REASS_NOTES_THREE']}\n";
    echo "</ul>\n";
    require_once('include/Smarty/plugins/function.sugar_help.php');
    $sugar_smarty = new Sugar_Smarty();
    $help_img = smarty_function_sugar_help(array("text"=>$mod_strings['LBL_REASS_VERBOSE_HELP']), $sugar_smarty);
    echo "<BR><input type=checkbox name=verbose> {$mod_strings_users['LBL_REASS_VERBOSE_OUTPUT']}".$help_img."<BR>\n";

    unset($_SESSION['reassignRecords']['modules']);
    $beanListFlip = array_flip($_SESSION['reassignRecords']['assignedModuleListCache']);
    foreach ($_POST['modules'] as $module) {
        if (!array_key_exists($module, $beanListFlip)) {
            //echo "$module not found as key in \$beanListFlip. Skipping $module.<BR>";
            continue;
        }
        $p_module = $beanListFlip[$module];

        require_once($beanFiles[$module]);
        $object = new $module();
        if (empty($object->table_name)) {
            //			echo "<h5>Could not find the database table for $p_module.</h5>";
            continue;
        }

        echo "<h5>{$mod_strings_users['LBL_REASS_ASSESSING']} {$app_list_strings['moduleList'][$p_module]}</h5>";

        echo "<table border='0' cellspacing='0' cellpadding='0'  class='detail view'>\n";
        echo "<tr>\n";
        echo "<td>\n";

        $currentUserID = $this->db->quote($current_user->id);
        $tableName = $this->db->quote($object->table_name);

        $q_select = "select id";
        $q_update = "update ";
        $q_set = " set assigned_user_id = '$toUserNameQuoted', " .
            "date_modified = '" . TimeDate::getInstance()->nowDb() . "', " .
            "modified_user_id = '$currentUserID' ";
        $q_tables = " {$tableName} ";
        $q_where = "where {$tableName}.deleted=0 and {$tableName}.assigned_user_id = '$fromUserNameQuoted' ";

        // Process conditions based on metadata
        if (isset($moduleFilters[$p_module]['fields']) && is_array($moduleFilters[$p_module]['fields'])) {
            $custom_added = false;
            foreach ($moduleFilters[$p_module]['fields'] as $meta) {
                if (!empty($_POST[$meta['name']])) {
                    $_SESSION['reassignRecords']['filters'][$meta['name']] = $_POST[$meta['name']];
                }
                $is_custom = isset($meta['custom_table']) && $meta['custom_table'] == true;
                if ($is_custom && !$custom_added) {
                    $q_tables .= "inner join {$tableName}_cstm on {$tableName}.id = {$tableName}_cstm.id_c ";
                    $custom_added = true;
                }
                $addcstm = ($is_custom ? "_cstm" : "");
                $nameQuoted = $_POST[$meta['name']];
                switch ($meta['type']) {
                    case "text":
                    case "select":
                    $q_where .= " and {$tableName}{$addcstm}.{$meta['dbname']} = '{$nameQuoted}' ";
                        break;
                    case "multiselect":
                        if (empty($nameQuoted)) {
                            continue;
                        }
                        $in_string = "";
                        $empty_check = "";
                        foreach ($nameQuoted as $onevalue) {
                            if (empty($onevalue)) {
                                $empty_check .= " OR {$tableName}{$addcstm}.{$meta['dbname']} is null ";
                            }
                            $in_string .= "'$onevalue', ";
                        }
                        $in_string = substr($in_string, 0, (is_countable($in_string) ? count($in_string) : 0) - 3);
                        $q_where .= " and ({$tableName}{$addcstm}.{$meta['dbname']} in ($in_string) $empty_check)";
                        break;
                    default:
                        break;
                }
            }
        }
        $query = "$q_select from $q_tables $q_where";
        $countquery = "select count(*) AS count from $q_tables $q_where";
        $updatequery = "$q_update $q_tables $q_set $q_where";

        $_SESSION['reassignRecords']['fromuser'] = $_POST['fromuser'];
        $_SESSION['reassignRecords']['touser'] = $_POST['touser'];
        $_SESSION['reassignRecords']['fromusername'] = $fromusername;
        $_SESSION['reassignRecords']['tousername'] = $tousername;
        $_SESSION['reassignRecords']['modules'][$module]['query'] = $query;
        $_SESSION['reassignRecords']['modules'][$module]['update'] = $updatequery;

        $res = DBManagerFactory::getInstance()->query($countquery, true);
        $row = DBManagerFactory::getInstance()->fetchByAssoc($res);

        echo "{$row['count']} {$mod_strings_users['LBL_REASS_RECORDS_FROM']} {$app_list_strings['moduleList'][$p_module]} {$mod_strings_users['LBL_REASS_WILL_BE_UPDATED']}\n<BR>\n";
        echo "<input type=checkbox name={$module}_workflow> {$mod_strings_users['LBL_REASS_WORK_NOTIF_AUDIT']}<BR>\n";
        echo "</td></tr></table>\n";
    }

    echo "<BR><input type=button class=\"button\" value=\"{$mod_strings_users['LBL_REASS_BUTTON_GO_BACK']}\" onclick='document.location=\"index.php?module=Users&action=reassignUserRecords\"'>\n";
    echo "&nbsp;<input type=submit class=\"button\" value=\"{$mod_strings_users['LBL_REASS_BUTTON_CONTINUE']}\">\n";
    echo "&nbsp;<input type=button class=\"button\" value=\"{$mod_strings_users['LBL_REASS_BUTTON_RESTART']}\" onclick='document.location=\"index.php?module=Users&action=reassignUserRecords&clear=true\"'>\n";

    echo "</form>\n";

// debug
    //print_r($_SESSION['reassignRecords']);
///////////////////// END STEP 2 - Confirm Selections /////////////////////////
}
/////////////////// BEGIN STEP 3 - Execute reassignment ///////////////////////
elseif (isset($_GET['execute']) && $_GET['execute'] == true) {
    $fromuser = $_SESSION['reassignRecords']['fromuser'];
    $touser = $_SESSION['reassignRecords']['touser'];
    $fromusername = $_SESSION['reassignRecords']['fromusername'];
    $tousername = $_SESSION['reassignRecords']['tousername'];

    $beanListFlip = array_flip($_SESSION['reassignRecords']['assignedModuleListCache']);

    foreach ($_SESSION['reassignRecords']['modules'] as $module => $queries) {
        $p_module = $beanListFlip[$module];
        $workflow = false;
        if (isset($_POST[$module."_workflow"]) && $_POST[$module."_workflow"] = "on") {
            $workflow = true;
        }

        $query = $workflow ? $queries['query'] : $queries['update'];

        echo "<h5>{$mod_strings_users['LBL_PROCESSING']} {$app_list_strings['moduleList'][$p_module]}</h5>";

        $res = DBManagerFactory::getInstance()->query($query, true);

        //echo "<i>Workflow and Notifications <b>".($workflow ? "enabled" : "disabled")."</b> for this module record reassignment</i>\n<BR>\n";
        echo "<table border='0' cellspacing='0' cellpadding='0'  class='detail view'>\n";
        echo "<tr>\n";
        echo "<td>\n";
        if (! $workflow) {
            $affected_rows = DBManagerFactory::getInstance()->getAffectedRowCount($res);
            echo "{$mod_strings_users['LBL_UPDATE_FINISH']}: $affected_rows {$mod_strings_users['LBL_AFFECTED']}<BR>\n";
        } else {
            $successarr = array();
            $failarr = array();

            require_once($beanFiles[$module]);
            while ($row = DBManagerFactory::getInstance()->fetchByAssoc($res)) {
                $bean = new $module();
                if (empty($row['id'])) {
                    continue;
                }
                $bean->retrieve($row['id']);

                // So that we don't create new blank records.
                if (!isset($bean->id)) {
                    continue;
                }
                $bean->assigned_user_id = $touser;

                if ($bean->save()) {
                    $linkname = "record with id {$bean->id}";
                    if (!empty($bean->name)) {
                        $linkname = $bean->name;
                    } elseif (!empty($bean->last_name)) {
                        $linkname = $locale->getLocaleFormattedName($bean->first_name, $bean->last_name);
                    } elseif (!empty($bean->document_name)) {
                        $linkname = $bean->document_name;
                    }
                    $successstr = "{$mod_strings_users['LBL_REASS_SUCCESS_ASSIGN']} {$bean->object_name} \"<i><a href=\"index.php?module={$bean->module_dir}&action=DetailView&record={$bean->id}\">$linkname</a></i>\" {$mod_strings_users['LBL_REASS_FROM']} $fromusername {$mod_strings_users['LBL_REASS_TO']} $tousername";
                    $successarr[] = $successstr;
                } else {
                    $failarr[] = "{$mod_strings_users['LBL_REASS_FAILED_SAVE']} \"<i><a href=\"index.php?module={$bean->module_dir}&action=DetailView&record={$bean->id}\">$linkname</a></i>\".";
                }
            }

            if (isset($_POST['verbose']) && $_POST['verbose'] == "on") {
                echo "<h5>{$mod_strings_users['LBL_REASS_THE_FOLLOWING']} {$app_list_strings['moduleList'][$p_module]} {$mod_strings_users['LBL_REASS_HAVE_BEEN_UPDATED']}</h5>\n";
                foreach ($successarr as $ord) {
                    echo "$ord\n<BR>\n";
                }
                if (empty($successarr)) {
                    echo "{$mod_strings_users['LBL_REASS_NONE']}\n<BR>\n";
                }

                echo "<h5>{$mod_strings_users['LBL_REASS_THE_FOLLOWING']} {$app_list_strings['moduleList'][$p_module]} {$mod_strings_users['LBL_REASS_CANNOT_PROCESS']}</h5>\n";
                foreach ($failarr as $failure) {
                    echo $failure."\n<BR>\n";
                }
                if (empty($failarr)) {
                    echo "{$mod_strings_users['LBL_REASS_NONE']}\n<BR>\n";
                }
            } else {
                echo "{$mod_strings_users['LBL_REASS_UPDATE_COMPLETE']}\n<BR>\n";
                echo "&nbsp;&nbsp;".count($successarr)." {$mod_strings_users['LBL_REASS_SUCCESSFUL']}\n<BR>\n";
                echo "&nbsp;&nbsp;".count($failarr)." {$mod_strings_users['LBL_REASS_FAILED']}\n";
            }
            echo "<BR>\n";
        }
        echo "</td></tr></table>\n";
    }

    echo "<BR><input type=button class=\"button\" value=\"{$mod_strings_users['LBL_REASS_BUTTON_RETURN']}\" onclick='document.location=\"index.php?module=Users&action=reassignUserRecords\"'>\n";

    /////////////////// END STEP 3 - Execute reassignment ///////////////////////
}
?>
<script type="text/javascript">

function clearCurrentRecords()
{
    var callback = {
                success: function(){
                    document.getElementById('fromuser').selectedIndex = 0;
                    document.getElementById('touser').selectedIndex = 0;
                    document.getElementById('modulemultiselect').selectedIndex = -1;
                    updateDivDisplay(document.getElementById('modulemultiselect'));
                }
            };

    YAHOO.util.Connect.asyncRequest('POST', 'index.php?module=Users&action=clearreassignrecords&to_pdf=1', callback, null);
}

var allselected = [];
function updateDivDisplay(multiSelectObj){
    for(var i = 0; i < multiSelectObj.options.length; i++){
        if(multiSelectObj.options[i].selected != allselected[i]){
            allselected[i] = multiSelectObj.options[i].selected;

            if(allselected[i]){
                theElement = document.getElementById('reassign_'+multiSelectObj.options[i].value);
                if(theElement != null){
                    theElement.style.display = 'block';
                }
            }
            else{
                theElement = document.getElementById('reassign_'+multiSelectObj.options[i].value);
                if(theElement != null){
                    theElement.style.display = 'none';
                }
            }
        }
    }
}
<?php
if (!isset($_POST['fromuser']) && !isset($_GET['execute'])) {
    ?>
updateDivDisplay(document.getElementById('modulemultiselect'));
<?php
}
?>
</script>
