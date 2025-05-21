<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once('modules/SecurityGroups/SecurityGroup.php');

require_once('include/formbase.php');
global $current_user, $db;


$module = $_REQUEST['return_module'];
$sugarbean = null;

$securitygroup = $_REQUEST['massassign_group'];
if (!isset($module) || empty($securitygroup) || !isset($securitygroup)) {
    return;
}

if (!empty($GLOBALS['beanList'][$module])) {
    $class = $GLOBALS['beanList'][$module];
    if (!empty($GLOBALS['beanFiles'][$class])) {
        require_once($GLOBALS['beanFiles'][$class]);
        $sugarbean = new $class();
    }
}

$groupFocus = BeanFactory::newBean('SecurityGroups');
$groupFocus->retrieve($securitygroup);





if (!empty($_REQUEST['uid'])) {
    $_POST['mass'] = explode(',', $_REQUEST['uid']);
} // coming from listview
elseif (isset($_REQUEST['entire'])) {
    if (isset($_SESSION['export_where']) && !empty($_SESSION['export_where'])) { // bug 4679
        $where = $_SESSION['export_where'];
        $whereArr = explode(" ", trim($where));
        if ($whereArr[0] === trim('where')) {
            $whereClean = array_shift($whereArr);
        }
        $where = implode(" ", $whereArr);
    } else {
        $where = '';
    }
    $export_where = !empty($_SESSION['export_where']) ? $_SESSION['export_where'] : '';
    if (empty($_REQUEST['export_where_md5']) || $_REQUEST['export_where_md5'] !== md5($export_where)) {
        $err = translate('LBL_ERROR_EXPORT_WHERE_CHANGED', 'SecurityGroups');
        sugar_die($err);
    }
    if (empty($order_by)) {
        $order_by = '';
    }
    $query = $sugarbean->create_export_query($order_by, $where);
    $result = $db->query($query, true);

    $new_arr = array();
    while ($val = $db->fetchByAssoc($result, -1, false)) {
        array_push($new_arr, $val['id']);
    }
    $_POST['mass'] = $new_arr;
}

if (isset($_POST['mass']) && is_array($_POST['mass'])) {
    $rel_name = "";
    foreach ($_POST['mass'] as $id) {
        if (isset($_POST['Delete'])) {
            $sugarbean->retrieve($id);

            //if($sugarbean->ACLAccess('Delete')){

            $GLOBALS['log']->debug("MassAssign - deleting relationship: $groupFocus->name");
            if ($sugarbean->module_dir == 'Users') {
                $rel_name = "SecurityGroups";
            } else {
                if (empty($rel_name) || !isset($rel_name)) {
                    $rel_name = $groupFocus->getLinkName($sugarbean->module_dir, "SecurityGroups");
                }
            }
            $sugarbean->load_relationship($rel_name);
            $sugarbean->$rel_name->delete($sugarbean->id, $groupFocus->id);

        //As of 6.3.0 many-to-many requires a link field set in both modules...so lets bypass that
                //$groupFocus->removeGroupFromRecord($sugarbean->module_dir, $id, $groupFocus->id);
            //}
        } else {
            $sugarbean->retrieve($id);

            //if($sugarbean->ACLAccess('Save')){

            $GLOBALS['log']->debug("MassAssign - adding relationship: $groupFocus->name");
            if ($sugarbean->module_dir == 'Users') {
                $rel_name = "SecurityGroups";
            } else {
                if (empty($rel_name) || !isset($rel_name)) {
                    $rel_name = $groupFocus->getLinkName($sugarbean->module_dir, "SecurityGroups");
                }
            }
            $GLOBALS['log']->debug("MassAssign - adding relationship relationship name: ".$rel_name);
            $sugarbean->load_relationship($rel_name);
            $sugarbean->$rel_name->add($groupFocus->id);


            //As of 6.3.0 many-to-many requires a link field set in both modules...so lets bypass that
                /**
                //check existing
                $query = "SELECT * FROM securitygroups_records WHERE securitygroup_id='$groupFocus->id' AND record_id='$id' AND module='$sugarbean->module_dir' AND deleted=0";
                $db = DBManagerFactory::getInstance();
                $result = $db->query($query);
                $row = $db->fetchByAssoc($result);
                if (empty($row))
                {
                    $groupFocus->addGroupToRecord($sugarbean->module_dir, $id, $groupFocus->id);
                }
                */


            //}
        }
    }
}


header("Location: index.php?action={$_POST['return_action']}&module={$_POST['return_module']}");
