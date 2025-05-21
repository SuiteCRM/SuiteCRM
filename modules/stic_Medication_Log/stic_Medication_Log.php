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
class stic_Medication_Log extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Medication_Log';
    public $object_name = 'stic_Medication_Log';
    public $table_name = 'stic_medication_log';
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
    public $intake_date;
    public $medication;
    public $schedule;
    public $dosage;
    public $administered;
    public $time;
    public $stock_depletion;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }
    /**
     * Override the bean's save function to assign an auto-incrementing value to the code field when a new record is created
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = false)
    {
        include_once 'SticInclude/Utils.php';
        
        $prescriptionBean = BeanFactory::getBean('stic_Prescription', $this->stic_medication_log_stic_prescriptionstic_prescription_ida);
        if ($prescriptionBean->stic_prescription_contactscontacts_ida instanceof Link2) {
            $contactBean = SticUtils::getRelatedBeanObject($prescriptionBean, 'stic_prescription_contacts');
        } else {
            $contactBean = BeanFactory::getBean('Contacts', $prescriptionBean->stic_prescription_contactscontacts_ida);
        }
        $contactId = $contactBean->id;
        
        if ($prescriptionBean->stic_prescription_stic_medicationstic_medication_ida instanceof Link2) {
            $medicationBean = SticUtils::getRelatedBeanObject($prescriptionBean, 'stic_prescription_stic_medication');
        } else {
            $medicationBean = BeanFactory::getBean('stic_Medication', $prescriptionBean->stic_prescription_stic_medicationstic_medication_ida);
        }

        $mediationName = '';
        if($medicationBean){
            $this->medication = $medicationBean->name;
            $medicationName = $medicationBean->name;
        }

        $contactName = '';
        if($contactBean) {
            $this->stic_medication_log_contactscontacts_ida = $contactId;
            $contactName = $contactBean->first_name . ' ' . $contactBean->last_name;
        }
        if (empty($this->name)) {
            global $app_list_strings, $current_user, $timedate;

            $intakeDate = $this->intake_date;

            // $timeDate = new TimeDate();
            if ($userDate = $timedate->fromUserDate($intakeDate, false, $current_user)) {
                $intakeDate = $userDate->asDBDate();
            }

            $date = SugarDateTime::createFromFormat(TimeDate::DB_DATE_FORMAT, $intakeDate, null);
            $intakeDateFormatted = $date->format($timedate->get_date_format($current_user));

            
            $this->name = $contactName . ' - ' . $medicationName . ' - ' . $intakeDateFormatted
                . ' - ' . $app_list_strings['stic_medication_schedule_list'][$this->schedule];
        }


        
        // Save the bean
        parent::save($check_notify);
    }
    /**
     * Overriding SugarBean save_relationship_changes function to insert additional logic: 
     * 1) Remove previous relationship with contact when needed
     * 2) Get the contact from the prescription and set it in the medication log
     *
     * @param bool $is_update
     * @param array $exclude
     * @return void
     */
    public function save_relationship_changes($is_update, $exclude = array())
    {
        include_once 'SticInclude/Utils.php';

        // If parent medication has changed...
        // STIC Custom 20250416 JBL - Fix Warnings and TypeErrors
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
        // if (!empty($this->stic_prescription_stic_medicationstic_medication_ida) && (trim($this->stic_prescription_stic_medicationstic_medication_ida) != trim($this->rel_fields_before_value['stic_prescription_stic_medicationstic_medication_ida']))) {
        if (!empty($this->stic_prescription_stic_medicationstic_medication_ida)) {
            $medication_id = '';
            if (is_string($this->stic_prescription_stic_medicationstic_medication_ida) || 
                (is_object($this->stic_prescription_stic_medicationstic_medication_ida) && 
                    method_exists($this->stic_prescription_stic_medicationstic_medication_ida, '__toString'))) {
                $medication_id = (string)$this->stic_prescription_stic_medicationstic_medication_ida;
            }
            $medication_id_before = '';
            if (isset($this->rel_fields_before_value['stic_prescription_stic_medicationstic_medication_ida'])) {
                if (is_string($this->rel_fields_before_value['stic_prescription_stic_medicationstic_medication_ida']) ||
                    (is_object($this->rel_fields_before_value['stic_prescription_stic_medicationstic_medication_ida']) && 
                        method_exists($this->rel_fields_before_value['stic_prescription_stic_medicationstic_medication_ida'], '__toString'))) {
                    $medication_id_before = (string)$this->rel_fields_before_value['stic_prescription_stic_medicationstic_medication_ida'];
                }
            }
            if (trim($medication_id) != trim($medication_id_before)) {
        // END STIC Custom
                // Get new parent prescription bean
                $prescriptionBean = BeanFactory::getBean('stic_Prescription', $this->stic_prescription_stic_medicationstic_medication_ida);
                // Get prescription related contact (usual case)
                $contactId = SticUtils::getRelatedBeanObject($prescriptionBean, 'stic_prescription_contacts')->id;
                $this->stic_medication_log_contactscontacts_ida = $contactId;
            }
        }

        // Call the generic save_relationship_changes() function from the SugarBean class
        parent::save_relationship_changes($is_update, $exclude);
    }
}
