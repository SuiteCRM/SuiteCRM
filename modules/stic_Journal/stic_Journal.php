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
class stic_Journal extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Journal';
    public $object_name = 'stic_Journal';
    public $table_name = 'stic_journal';
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
    public $journal_date;
    public $type;
    public $turn;
    public $task;
    public $task_scope;
    public $task_start_date;
    public $task_end_date;
    public $task_fulfillment;
    public $task_description;
    public $infringement_seriousness;
    public $infringement_description;
	
    public function bean_implements($interface)
    {
        switch($interface)
        {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     * Overriding SugarBean save function to insert additional logic:
     * Build the name of the journal using the name of the center, the date and the type
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = false) {
        
        include_once 'SticInclude/Utils.php';
        global $app_list_strings, $timedate;

        // Create name if empty
        if(empty($this->name)) {
            // Format the date
            $formatedDateName = $timedate->to_display_date_time($this->journal_date); 

            // If there is a center selected
            if(!empty($this->stic_journal_stic_centers_name)) {
                $this->name = $formatedDateName . ' - ' . $app_list_strings['stic_journal_types_list'][$this->type] . ' - ' . $this->stic_journal_stic_centers_name;
            } else {
                $this->name = $formatedDateName . ' - ' . $app_list_strings['stic_journal_types_list'][$this->type];
            }
        }
        
        // Call the generic save() function from the SugarBean class
        parent::save();
    }

}
