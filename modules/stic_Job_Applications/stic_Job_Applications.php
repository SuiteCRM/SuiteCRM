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
class stic_Job_Applications extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Job_Applications';
    public $object_name = 'stic_Job_Applications';
    public $table_name = 'stic_job_applications';
    public $importable = true;
    public $disable_row_level_security = true; // to ensure that modules created and deployed under CE will continue to function under team security if the instance is upgraded to PRO
    public $id;
    public $SecurityGroups;
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
    public $start_date;
    public $end_date;
    public $status;
    public $status_details;
    public $motivations;
    public $preinsertion_observations;
    public $contract_start_date;
    public $contract_end_date;
    public $contract_end_reason;
    public $postinsertion_observations;
    public $rejection_reason;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return true;
        }
        return false;
    }
	public function save($check_notify = true) {
        include_once 'SticInclude/Utils.php';
        include_once 'modules/stic_Job_Applications/Utils.php';

        // Call the generic save() function from the SugarBean class
        if (empty($this->name)) {
            if ($this->stic_job_applications_contactscontacts_ida) {
                $contact_name = $this->stic_job_applications_contacts_name;
            }
            if ($this->stic_job_applications_stic_job_offersstic_job_offers_ida) {
                $offer_name = $this->stic_job_applications_stic_job_offers_name;
            }
            $this->name = $contact_name .' - '.$offer_name;
        }

        parent::save($check_notify);

        if( $this->status == 'accepted'){

            stic_Job_ApplicationsUtils::createWorkExperience($this);

		}
    }

}
