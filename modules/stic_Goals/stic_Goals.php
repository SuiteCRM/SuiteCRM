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

#[\AllowDynamicProperties]
class stic_Goals extends Basic {
    public $new_schema = true;
    public $module_dir = 'stic_Goals';
    public $object_name = 'stic_Goals';
    public $table_name = 'stic_goals';
    public $importable = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $SecurityGroups;
    public $start_date;
    public $expected_end_date;
    public $status;
    public $origin;
    public $level;
    public $area;
    public $subarea;
    public $actual_end_date;
    public $follow_up;

    public function bean_implements($interface) {
        switch ($interface) {
        case 'ACL':
            return true;
        }

        return false;
    }

    // Set main common SQL for both custom subpanels (destination & origin goals)
    private $customSubpanelSQL =
        "SELECT
            stic_goals.id as stic_goals_id,
            concat_ws(' ', contacts.first_name, contacts.last_name) as stic_goals_contacts_name,
            contacts.id as stic_goals_contactscontacts_ida,
            stic_registrations.name as stic_goals_stic_registrations_name,
            stic_registrations.id as stic_goals_stic_registrationsstic_registrations_ida,
            stic_assessments.name as stic_goals_stic_assessments_name,
            stic_assessments.id as stic_goals_stic_assessmentsstic_assessments_ida ,
            project.name as stic_goals_project_name,
            project.id as stic_goals_projectproject_ida,
            stic_goals.*
        FROM
            stic_goals
        JOIN stic_goals_stic_goals_c on
            stic_goals.id = stic_goals_stic_goals_c.@@relationSide@@  AND stic_goals.deleted =0
            left join stic_goals_contacts_c on stic_goals_contacts_c.stic_goals_contactsstic_goals_idb = stic_goals.id AND stic_goals_contacts_c.deleted=0
            left join contacts on contacts.id=stic_goals_contacts_c.stic_goals_contactscontacts_ida AND contacts.deleted=0
            left join stic_goals_project_c on stic_goals_project_c.stic_goals_projectstic_goals_idb =stic_goals.id AND stic_goals_project_c.deleted=0
            left join project on project.id=stic_goals_project_c.stic_goals_projectproject_ida AND project.deleted=0
            left join stic_goals_stic_assessments_c on stic_goals_stic_assessments_c.stic_goals_stic_assessmentsstic_goals_idb=stic_goals.id AND stic_goals_stic_assessments_c.deleted=0
            left join stic_assessments on stic_assessments.id=stic_goals_stic_assessments_c.stic_goals_stic_assessmentsstic_assessments_ida AND stic_assessments.deleted=0
            left join stic_goals_stic_registrations_c on stic_goals_stic_registrations_c.stic_goals_stic_registrationsstic_goals_idb =stic_goals.id AND stic_goals_stic_registrations_c.deleted=0
            left join stic_registrations on stic_registrations.id=stic_goals_stic_registrations_c.stic_goals_stic_registrationsstic_registrations_ida AND stic_registrations.deleted=0
        WHERE
            stic_goals_stic_goalsstic_goals_ida != stic_goals_stic_goalsstic_goals_idb
        AND stic_goals_stic_goals_c.deleted=0";

    /**
     * Create a query string for select Goals in destination side using the where conditions for destination side
     *
     * @return string final query
     */
    public function getSticGoalsSticGoalsDestinationSide() {
        $idQuoted = $this->db->quoted($this->id);

        // Add conditions to query
        $query = $this->customSubpanelSQL . " " .
            "AND stic_goals_stic_goals_c.stic_goals_stic_goalsstic_goals_ida = $idQuoted
             AND stic_goals.id != $idQuoted";

        // Replace relationship field name side
        $query = str_replace('@@relationSide@@', 'stic_goals_stic_goalsstic_goals_idb', $query);

        return $query;
    }

    /**
     * Create a query string for select Goals in origin side using the where conditions for origin side
     * @return string final query
     */
    public function getSticGoalsSticGoalsOriginSide() {
        $idQuoted = $this->db->quoted($this->id);
        
        // Add conditions to query
        $query = $this->customSubpanelSQL . " " .
            "AND stic_goals_stic_goals_c.stic_goals_stic_goalsstic_goals_idb = $idQuoted
             AND stic_goals.id != $idQuoted";
        
        // Replace relationship field name side
        $query = str_replace('@@relationSide@@', 'stic_goals_stic_goalsstic_goals_ida', $query);
        return $query;
    }

}
