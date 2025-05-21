<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

//prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class stic_GoalsController extends SugarController {

    /**
     * Action to create custom many2many relationships between a main Goal (Detailview record) and popup selected destination Goals records.
     * This action is called from "relateDestinationGoal" javascript function (modules/stic_Goals/Utils.js)
     *
     * @return void
     */
    public function action_relateDestinationGoalFromPopUp() {

        $currentGoalId = $_REQUEST['record'];
        $relateIds = explode(' ', $_REQUEST['goalIds']);

        // If select_entire_list is used, select all stic_goals records undeleted and different that current record
        if (isset($_REQUEST['select_entire_list']) && $_REQUEST['select_entire_list'] == '1') {

            // If the popup selection was previously filtered, use custom function to generate custom where conditions
            if (isset($_REQUEST['current_query_by_page'])) {
                include 'modules/stic_Goals/Utils.php';
                $where = generateAllWhereClausesFromGoalsPopup();
            }
            $entireListSQL = "SELECT distinct id FROM stic_goals WHERE deleted=0 AND id != '{$currentGoalId}'";
            if (!empty($where)) {
                $entireListSQL = $entireListSQL . ' AND ' . $where;
            }

            $entireListSQLResults = $GLOBALS['db']->query($entireListSQL);
            unset($relateIds);
            while ($goalRow = $GLOBALS['db']->fetchByAssoc($entireListSQLResults)) {
                $relateIds[] = $goalRow['id'];
            }
        }

        foreach ($relateIds as $key => $value) {

            // If relationship really exists, skip saving
            $checkIfRelateReallyExistSQL = "SELECT count(id) FROM stic_goals_stic_goals_c where stic_goals_stic_goalsstic_goals_ida='{$currentGoalId}' AND stic_goals_stic_goalsstic_goals_idb='{$value}' AND deleted=0";
            if ($GLOBALS['db']->getOne($checkIfRelateReallyExistSQL) == 0) {
                $relateSQL = "INSERT INTO stic_goals_stic_goals_c (id,date_modified,deleted,stic_goals_stic_goalsstic_goals_ida,stic_goals_stic_goalsstic_goals_idb) VALUES (uuid(),now(),'0','{$currentGoalId}','{$value}');";
                $GLOBALS['db']->query($relateSQL);
            }
        }
        SugarApplication::redirect("index.php?module=stic_Goals&action=DetailView&record={$currentGoalId}");
    }
    
    /**
     * Action to create custom many2many relationships between main Goal (Detailview record) and popup selected origin Goals records.
     * This action is called from "relateOriginGoal" javascript function (modules/stic_Goals/Utils.js)
     *
     * @return void
     */
    public function action_relateOriginGoalFromPopUp() {
        
        $currentGoalId = $_REQUEST['record'];
        $relateIds = explode(' ', $_REQUEST['goalIds']);
        
        // If select_entire_list is used, select all stic_goals records undeleted and different that current record
        if (isset($_REQUEST['select_entire_list']) && $_REQUEST['select_entire_list'] == '1') {
            
            // If the popup selection was previously filtered, use custom function to generate custom where conditions
            if (isset($_REQUEST['current_query_by_page'])) {
                include 'modules/stic_Goals/Utils.php';
                $where = generateAllWhereClausesFromGoalsPopup();
            }
            $entireListSQL = "SELECT distinct id FROM stic_goals WHERE deleted=0 AND id != '{$currentGoalId}'";
            if (!empty($where)) {
                $entireListSQL = $entireListSQL . ' AND ' . $where;
            }
            
            $entireListSQLResults = $GLOBALS['db']->query($entireListSQL);
            unset($relateIds);
            while ($goalRow = $GLOBALS['db']->fetchByAssoc($entireListSQLResults)) {
                $relateIds[] = $goalRow['id'];
            }
        }
        
        foreach ($relateIds as $key => $value) {
            
            // If relationship really exists, skip saving
            $checkIfRelateReallyExistSQL = "SELECT count(id) FROM stic_goals_stic_goals_c where stic_goals_stic_goalsstic_goals_idb='{$currentGoalId}' AND stic_goals_stic_goalsstic_goals_ida='{$value}' AND deleted=0";
            if ($GLOBALS['db']->getOne($checkIfRelateReallyExistSQL) == 0) {
                $relateSQL = "INSERT INTO stic_goals_stic_goals_c (id,date_modified,deleted,stic_goals_stic_goalsstic_goals_ida,stic_goals_stic_goalsstic_goals_idb) VALUES (uuid(),now(),'0','{$value}','{$currentGoalId}');";
                $GLOBALS['db']->query($relateSQL);
            }

        }

        SugarApplication::redirect("index.php?module=stic_Goals&action=DetailView&record={$currentGoalId}");
    }

    /**
     * This action removes custom many2many relationships between Goals and is called from custom remove button (SubPanelRemoveButtonstic_Goals widget) in
     * Goals subpaneledefs
     *
     * @return void
     */
    public function action_removeAutoRelationGoalFromSubpanel() {

        $subpanelRecordId = $_REQUEST['subpanel_record'];
        $currentGoalId = $_REQUEST['main_record'];

        // get the source subpanel from $_REQUEST['subpanel_name']
        switch ($_REQUEST['subpanel_name']) {
        case 'stic_goals_stic_goals_origin':
            // build SQL
            $removeRelationSQL = "UPDATE stic_goals_stic_goals_c SET deleted=1 WHERE stic_goals_stic_goalsstic_goals_ida='{$subpanelRecordId}' AND stic_goals_stic_goalsstic_goals_idb='{$currentGoalId}';";
            break;

        case 'stic_goals_stic_goals_destination':
            // build SQL
            $removeRelationSQL = "UPDATE stic_goals_stic_goals_c SET deleted=1 WHERE stic_goals_stic_goalsstic_goals_ida='{$currentGoalId}' AND stic_goals_stic_goalsstic_goals_idb='{$subpanelRecordId}';";
            break;

        default:
            break;
        }

        if (isset($removeRelationSQL)) {
            // Run SQL
            $GLOBALS['db']->query($removeRelationSQL);
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': Relation bettwen Goals has been deleted with SQL: ' . $removeRelationSQL);
        }
        SugarApplication::redirect("index.php?module=stic_Goals&action=DetailView&record={$currentGoalId}");

    }

        /**
     * Action to retrieve the Contact or Familiy associated with the selected Assessments
     * This action is called from "relateDestinationGoal" javascript function (modules/stic_Goals/Utils.js)
     *
     * @return void
     */
    public function action_getContactOrFamilyFromAssessment() {

        $assessmentId = $_POST["assessmentId"];
        
        $response['code'] = 'No data';
        $db = DBManagerFactory::getInstance();

        $sql = "SELECT 'contact' AS type, stic_assessments_contactscontacts_ida AS id, concat(con.first_name, ' ', con.last_name) AS name
        FROM stic_assessments_contacts_c asscon
        JOIN contacts con ON con.id = asscon.stic_assessments_contactscontacts_ida 
        WHERE stic_assessments_contactsstic_assessments_idb = '{$assessmentId}'
        AND asscon.deleted = 0";
        $result = $db->query($sql);
        if($row = $result->fetch_assoc()) {
            $response['code'] = 'OK';
            $response['data']['stic_goals_contactscontacts_ida'] = $row['id'];
            $response['data']['stic_goals_contacts_name'] = $row['name'];
        }
        else {
            $response['data']['stic_goals_contactscontacts_ida'] = '';
            $response['data']['stic_goals_contacts_name'] = '';
        }
        
        $sql = "SELECT 'family' AS type, stic_families_stic_assessmentsstic_families_ida as id, fam.name
        FROM stic_families_stic_assessments_c famass
        JOIN stic_families fam ON fam.id = famass.stic_families_stic_assessmentsstic_families_ida 
        WHERE stic_families_stic_assessmentsstic_assessments_idb = '{$assessmentId}'
        AND famass.deleted = 0";
        $result = $db->query($sql);
        if($row = $result->fetch_assoc()) {
            $response['code'] = 'OK';
            $response['data']['stic_families_stic_goalsstic_families_ida'] = $row['id'];
            $response['data']['stic_families_stic_goals_name'] = $row['name'];
        }
        else {
            $response['data']['stic_families_stic_goalsstic_families_ida'] = '';
            $response['data']['stic_families_stic_goals_name'] = '';
        }

        echo json_encode($response);
        exit;
    }

}
