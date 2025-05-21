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
class stic_Bookings extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Bookings';
    public $object_name = 'stic_Bookings';
    public $table_name = 'stic_bookings';
    public $importable = true;
    public $disable_row_level_security = true; // to ensure that modules created and deployed under CE will continue to function under team security if the instance is upgraded to PRO
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
    public $SecurityGroups;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $start_date;
    public $end_date;
    public $parent_name;
    public $parent_type;
    public $parent_id;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }
        return false;
    }

    /**
     * Override bean's save function to link all the resources chosen on the EditViewFooter and set the booking's name
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = true)
    {
        global $timedate, $current_user, $db, $current_language;

        // Retrieve the strings of the needed module
        // (otherwise when accessing from ListView mod_string would return the global app strings)
        $mod_strings = return_module_language($current_language, 'stic_Bookings');

        // Set booking name
        if (empty($this->name)) {
            // On new bookings, define booking code
            // Warning: the lines below retrieve the last existing code in the db in order to calculate
            // the code for the newly created booking because the value of the field code is not available
            // until the record is created in the db (it is an autoincrement db field). This may cause
            // problems of crossing code assignation in case of concurrent bookings creation. If this
            // proves to be a problem in the future, this section should be rethinked. Anyway, it only
            // affects the name field, which is not a critical data.
            $currentNum = $this->code ?? null;
            if (!$currentNum) {
                // Get last assigned code
                $query = "SELECT code
                FROM stic_bookings
                ORDER BY code DESC LIMIT 1";
                $result = $db->query($query, true);
                $row = $db->fetchByAssoc($result);
                $lastNum = $row['code'];
                if (!isset($lastNum) || empty($lastNum)) {
                    $lastNum = 0;
                }
                $currentNum = $lastNum + 1;
            }
            // Format code
            $currentNum = str_pad($currentNum, 5, "0", STR_PAD_LEFT);
            // Build booking name
            $this->name = $mod_strings['LBL_MODULE_NAME_SINGULAR'] . ' ' . $currentNum;
        }

        // If all_day is checked and the request is from user interface, set the proper start_date and end_date values.
        // From the API or from the import process is not necessary since the start_date and end_date values are received by the save() method in UTC and in database format.
        // Control that a FdT or an LH does not recalculate the dates more than once through the condition !$this->processed
        if (isset($this->all_day) && $this->all_day == '1' && !empty($_REQUEST['start_date']) && !empty($_REQUEST['end_date']) && (!isset($this->processed) || !$this->processed)) {
            $startDate = $timedate->fromUser($_REQUEST['start_date'], $current_user);
            $startDate = $startDate->get_day_begin();
            $startDate = $timedate->asUserDate($startDate, false, $current_user);
            $this->start_date = $startDate;
            $endDate = $timedate->fromUser($_REQUEST['end_date'], $current_user);
            $endDate = $endDate->modify("next day");
            $endDate = $timedate->asUserDate($endDate, false, $current_user);
            $this->end_date = $endDate;
        }

        // Save the bean
        parent::save($check_notify);

        // If the save function is launched by save action in editview, relationships 
        // with resources must be managed. In other cases (inline edit, etc.) will do nothing.
        if ($_REQUEST['action'] == 'Save') {
            // Remove previous relationships
            $oldRelatedResources = array();
            $oldRelatedResources = $this->get_linked_beans('stic_resources_stic_bookings', 'stic_Resources');
            foreach ($oldRelatedResources as $oldRelatedResource) {
                $this->stic_resources_stic_bookings->delete($this->id, $oldRelatedResource->id);
            }

            // Retrieve the resources selected in the EditViewFooter
            $newRelatedResources = array();
            foreach ($_REQUEST['resource_id'] as $parent => $key) {
                if (empty($_REQUEST['deleted'][$parent])) {
                    $newRelatedResources[] = $_REQUEST['resource_id'][$parent];
                }
            }
            // Set current relationships
            foreach ($newRelatedResources as $newRelatedResource) {
                $this->stic_resources_stic_bookings->add($newRelatedResource);
            }
        }

        // If return module is Booking's Calendar, redirect there
        if (isset($_REQUEST['return_module']) && $_REQUEST['return_module'] == 'stic_Bookings_Calendar') {
            SugarApplication::redirect("index.php?module=stic_Bookings_Calendar&action=index&start_date=".explode(' ', $this->start_date)[0]);
        }

    }
}
