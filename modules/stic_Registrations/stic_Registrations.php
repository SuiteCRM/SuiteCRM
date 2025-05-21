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
class stic_Registrations extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Registrations';
    public $object_name = 'stic_Registrations';
    public $table_name = 'stic_registrations';
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
    public $status;
    public $registration_date;
    public $not_participating_reason;
    public $rejection_reason;
    public $participation_type;
    public $special_needs;
    public $special_needs_description;
    public $attendance_percentage;
    public $attended_hours;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function save_relationship_changes($is_update, $exclude = array())
    {
        // STIC Custom 20250416 JBL - Fix Warnings and TypeErrors
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
        // if (!empty($this->stic_registrations_stic_eventsstic_events_ida) && (trim($this->stic_registrations_stic_eventsstic_events_ida) != trim($this->rel_fields_before_value['stic_registrations_stic_eventsstic_events_ida']))) {
        if (!empty($this->stic_registrations_stic_eventsstic_events_ida)) {
            $event_id = '';
            if (is_string($this->stic_registrations_stic_eventsstic_events_ida) || 
                (is_object($this->stic_registrations_stic_eventsstic_events_ida) && 
                 method_exists($this->stic_registrations_stic_eventsstic_events_ida, '__toString'))) {
                $event_id = (string)$this->stic_registrations_stic_eventsstic_events_ida;
            }
            $event_id_before = '';
            if (isset($this->rel_fields_before_value['stic_registrations_stic_eventsstic_events_ida'])) {
                if (is_string($this->rel_fields_before_value['stic_registrations_stic_eventsstic_events_ida']) ||
                    (is_object($this->rel_fields_before_value['stic_registrations_stic_eventsstic_events_ida']) && 
                     method_exists($this->rel_fields_before_value['stic_registrations_stic_eventsstic_events_ida'], '__toString'))) {
                    $event_id_before = (string)$this->rel_fields_before_value['stic_registrations_stic_eventsstic_events_ida'];
                }
            }
            if (trim($event_id) != trim($event_id_before)) {
        // END STIC Custom
                // On new records, inherit amount from related event
                if (empty($this->session_amount) && !$is_update) {
                    $eventBean = BeanFactory::getBean('stic_Events', $this->stic_registrations_stic_eventsstic_events_ida);
                    $this->session_amount = $eventBean->session_amount;
                }
            }
        }
        parent::save_relationship_changes($is_update, $exclude);
    }
}
