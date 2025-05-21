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
class stic_Attendances extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Attendances';
    public $object_name = 'stic_Attendances';
    public $table_name = 'stic_attendances';
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
    public $duration;
    public $status;

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
        // if (!empty($this->stic_attendances_stic_registrationsstic_registrations_ida) && !empty($this->rel_fields_before_value) && (trim($this->stic_attendances_stic_registrationsstic_registrations_ida) != trim($this->rel_fields_before_value['stic_attendances_stic_registrationsstic_registrations_ida']))) {
        if (!empty($this->stic_attendances_stic_registrationsstic_registrations_ida)) {
            $registration_id = '';
            if (is_string($this->stic_attendances_stic_registrationsstic_registrations_ida) || 
                (is_object($this->stic_attendances_stic_registrationsstic_registrations_ida) && 
                    method_exists($this->stic_attendances_stic_registrationsstic_registrations_ida, '__toString'))) {
                $registration_id = (string)$this->stic_attendances_stic_registrationsstic_registrations_ida;
            }
            $registration_id_before = '';
            if (isset($this->rel_fields_before_value['stic_attendances_stic_registrationsstic_registrations_ida'])) {
                if (is_string($this->rel_fields_before_value['stic_attendances_stic_registrationsstic_registrations_ida']) ||
                    (is_object($this->rel_fields_before_value['stic_attendances_stic_registrationsstic_registrations_ida']) && 
                        method_exists($this->rel_fields_before_value['stic_attendances_stic_registrationsstic_registrations_ida'], '__toString'))) {
                    $registration_id_before = (string)$this->rel_fields_before_value['stic_attendances_stic_registrationsstic_registrations_ida'];
                }
            }
            if (trim($registration_id) != trim($registration_id_before)) {
        // END STIC Custom
                // On new records, inherit amount from related registration
                if (empty($this->amount) && !$is_update) {
                    $registrationBean = BeanFactory::getBean('stic_Registrations', $this->stic_attendances_stic_registrationsstic_registrations_ida);
                    $this->amount = (float)$registrationBean->session_amount;
                }
            }
        }
        parent::save_relationship_changes($is_update, $exclude);
    }

}
