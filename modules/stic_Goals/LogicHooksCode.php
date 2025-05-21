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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


class stic_GoalsLogicHooks {    
    public function after_save(&$bean, $event, $arguments) {

        /*
         If $_REQUEST contains "relate_to" variable, it means we are creating a record through a subpanel quickcreate form. 
         In this case "relate_to" contains de source subpanel name. Depending on the sub-panel used to create the related record, we will create the relationship one way or the other. Inserting in the "_ida" side or in the "_idb" side of the related table the id of the Goal that is shown in the detail view (Destination or Origin Goal).
        */
        if (!empty($_REQUEST['relate_to'])) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . 'REQUEST : ' . print_r($_REQUEST, true));
            $relateId = $_REQUEST['relate_id'];

            $relateSQL = '';
            switch ($_REQUEST['relate_to']) {
            case 'getSticGoalsSticGoalsDestinationSide':
                $relateSQL = "INSERT INTO stic_goals_stic_goals_c (id,date_modified,deleted,stic_goals_stic_goalsstic_goals_ida,stic_goals_stic_goalsstic_goals_idb) VALUES (uuid(),now(),'0','{$relateId}','{$bean->id}');";
                break;
            case 'getSticGoalsSticGoalsOriginSide':
                $relateSQL = "INSERT INTO stic_goals_stic_goals_c (id,date_modified,deleted,stic_goals_stic_goalsstic_goals_ida,stic_goals_stic_goalsstic_goals_idb) VALUES (uuid(),now(),'0','{$bean->id}','{$relateId}');";
                break;

            default:
                
                break;
            }
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . '.OBB ' . $relateSQL);
            if (!empty($relateSQL)) {
                // Insert relationship record
                $bean->db->query($relateSQL);
                $GLOBALS['log']->info('Line '.__LINE__.': '.__METHOD__.': Relationship between Goals has been deleted using this SQL statement: '.$relateSQL);
            }
        }

    }
}
