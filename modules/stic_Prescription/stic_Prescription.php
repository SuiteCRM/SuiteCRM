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
class stic_Prescription extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Prescription';
    public $object_name = 'stic_Prescription';
    public $table_name = 'stic_prescription';
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
    public $end_date;
    public $active;
    public $frequency;
    public $schedule;
    public $dosage;
    public $skip_intake;
    public $stock_depletion_date;
    public $prescription;
    public $type;
    public $contact_id_c;
    public $prescriber;

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
        $this->fillName();

        $this->setActiveField();

        // Save the bean
        parent::save($check_notify);

        if (!(isset($this->dont_create_logs) && $this->dont_create_logs == 1)) {
            $this->createMedicationLogs();
        }
    }

    protected function fillName()
    {
        // Auto name
        if (empty($this->name)) {
            global $app_list_strings;
            include_once 'SticInclude/Utils.php';

            $contactName = '';
            $contactBean = BeanFactory::getBean('Contacts', $this->stic_prescription_contactscontacts_ida);
            if ($contactBean){
                $contactName = $contactBean->first_name . ' ' . $contactBean->last_name;
            }

            $medicationName = '';
            $medicationBean = BeanFactory::getBean('stic_Medication', $this->stic_prescription_stic_medicationstic_medication_ida);
            if($medicationBean){
                $medicationName = $medicationBean->name;
            }
            $this->name = $contactName . ' - ' . $medicationName . ' - ' .
                $app_list_strings['stic_medication_frequency_list'][$this->frequency];
        }
    }

    protected function setActiveField()
    {
        global $timedate, $current_user;

        // Set active/inactive status
        $start = $this->start_date;
        $end = $this->end_date;

        if ($userDate = $timedate->fromUserDate($start, false, $current_user)) {
            $start = $userDate->asDBDate();
        }

        if ($userDate = $timedate->fromUserDate($end, false, $current_user)) {
            $end = $userDate->asDBDate();
        }

        if ((empty($start) || $start <= date("Y-m-d"))
            && (empty($end)   || $end >= date("Y-m-d"))
        ) {
            $this->active = true;
        } else {
            $this->active = false;
        }
    }

    // Create medication logs if creation date > start_date
    protected function createMedicationLogs()
    {
        // Logs are created only if periodicity is not "punctual" and the record is being created
        if ($this->frequency == 'daily' && $this->fetched_row == false) {
            list($startDate, $stopGeneratingDate) = $this->getStartAndStopDates();

            // Logs are only generated if the prescription started before creation date
            $currentDate = date('Y-m-d');
            if ($startDate < $currentDate) {
                $scheduleList = explode('^,^', trim($this->schedule, '^'));
                $iterationDate = strtotime($startDate);
                $stopGeneratingDate = strtotime($stopGeneratingDate);

                if ($this->stic_prescription_contactscontacts_ida instanceof Link2) {
                    $contactBean = SticUtils::getRelatedBeanObject($this, 'stic_prescription_stic_medication');
                    if ($contactBean) {
                        $contactId = $contactBean->id;
                    }
                }
                else {
                    $contactId = $this->stic_prescription_contactscontacts_ida;
                }

                
                if ($this->stic_prescription_stic_medicationstic_medication_ida instanceof Link2) {
                    $medicationBean = SticUtils::getRelatedBeanObject($this, 'stic_prescription_stic_medication');
                } else {
                    $medicationBean = BeanFactory::getBean('stic_Medication', $this->stic_prescription_stic_medicationstic_medication_ida);
                }

                if($contactId && $medicationBean) {
                    while ($iterationDate <= $stopGeneratingDate) {
                        $weekday = (date('N', $iterationDate) % 7);
                        $weekday = $weekday . ''; // Force conversion to string

                        // Check if weekday must be skipped
                        if (!strpos($this->skip_intake, $weekday)) { 
                            foreach ($scheduleList as $schedule) {
                                $this->createLogRecord($this, $contactId, $iterationDate, $medicationBean->name, $schedule);
                            }
                        }
    
                        $iterationDate += (24 * 60 * 60); // +1 day
                    }
                }
            }
        }
    }

    protected function getStartAndStopDates() {
        global $current_user, $timedate;

        $currentDate = date('Y-m-d');

        $startDate = $this->start_date;
        if ($userDate = $timedate->fromUserDate($startDate, false, $current_user)) {
            $startDate = $userDate->asDBDate();
        }

        $stopGeneratingDate = $currentDate;
                
        $endDate = $this->end_date;
        if ($endDate != null) {
            if ($userDate = $timedate->fromUserDate($endDate, false, $current_user)) {
                $endDate = $userDate->asDBDate();
            }
            $stopGeneratingDate = $stopGeneratingDate < $endDate ? $stopGeneratingDate : $endDate;
        }

        return array($startDate, $stopGeneratingDate);
    }
    protected function createLogRecord($prescriptionBean, $contactId, $iterationDate, $medicationName, $schedule)
    {
        $medicationLog = BeanFactory::getBean('stic_Medication_Log');
        $medicationLog->assigned_user_id = $prescriptionBean->assigned_user_id;
        $medicationLog->intake_date = date('Y-m-d', $iterationDate);
        $medicationLog->medication = $medicationName;
        $medicationLog->dosage = $prescriptionBean->dosage;
        $medicationLog->administered = 'pending';
        $medicationLog->schedule = $schedule;

        $medicationLog->stic_medication_log_stic_prescriptionstic_prescription_ida = $prescriptionBean->id;
        $medicationLog->stic_medication_log_contactscontacts_ida = $contactId;

        $medicationLog->save();
    }
}
