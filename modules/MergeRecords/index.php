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



if (isset($_REQUEST['uid'])) {
    $merge_ids = explode(',', $_REQUEST['uid']);
    // Bug 18852 - Check to make sure we have ACL Edit privledges on both records involved in the merge before proceeding
    if (($bean1 = SugarModule::get($_REQUEST['action_module'])->loadBean()) !== false
            && ($bean2 = SugarModule::get($_REQUEST['action_module'])->loadBean()) !== false) {
        $bean1->retrieve($merge_ids[0]);
        $bean2->retrieve($merge_ids[1]);
        if (!$bean1->ACLAccess('edit') || !$bean2->ACLAccess('edit')) {
            ACLController::displayNoAccess();
            sugar_die('');
        }
    }
    
    //redirect to step3.
    $_REQUEST['record']=$merge_ids[0];
    $_REQUEST['merge_module']=$_REQUEST['action_module'];
    unset($merge_ids[0]);
    $_REQUEST['mass']=$merge_ids;
} else {
    global $beanList;
    global $beanFiles;
    $merge_ids = array();
    $bean = $beanList[$_REQUEST['return_module']];
    require_once($beanFiles[$bean]);
    $focus = new $bean();
    
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
    if (empty($order_by)) {
        $order_by = '';
    }
    $query = $focus->create_export_query($order_by, $where);
    $result = $focus->db->query($query, true);
    
    /*
    $query = 'select * from '.$focus->table_name.' where deleted=0';
    $result = $focus->db->query($query, true, "");
    */
    $row = $focus->db->fetchByAssoc($result);
    
    while ($row != null) {
        //$beanObj = new $bean;
        array_push($merge_ids, $row['id']);
        $row = $focus->db->fetchByAssoc($result);
    }
    $_REQUEST['record']=$merge_ids[0];
    $_REQUEST['action']='index.php';
    $_REQUEST['merge_module']=$_REQUEST['return_module'];
    unset($merge_ids[0]);
    $_REQUEST['mass']=$merge_ids;
}
require('modules/MergeRecords/Step3.php');
