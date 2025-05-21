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
class stic_Job_Offers extends Basic {
	public $new_schema = true;
	public $module_dir = 'stic_Job_Offers';
	public $object_name = 'stic_Job_Offers';
	public $table_name = 'stic_job_offers';
	public $importable = true;
	public $disable_row_level_security = true ; // to ensure that modules created and deployed under CE will continue to function under team security if the instance is upgraded to PRO
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
	public $status;
	public $contact_id_c;
	public $interlocutor;
	public $contract_duration_details;
	public $process_start_date;
	public $process_end_date;
	public $offered_positions;
	public $hours_per_week;
	public $contract_description;
	public $retribution;
	public $retribution_conditions;
	public $offer_code;
	public $applications_start_date;
	public $applications_end_date;
	public $inc_contract_start_date;
	public $inc_collective_requirements;
	public $inc_state;
	public $inc_state_code;
	public $inc_municipality;
	public $inc_municipality_code;
	public $inc_town;
	public $inc_town_code;
	public $inc_country;
	public $sepe_contract_type;
	public $inc_contract_type;
	public $offer_origin;
	public $inc_offer_origin;
	public $inc_contract_duration;
	public $inc_working_day;
	public $inc_id;
	public $inc_status;
	public $type;
	public $sepe_activation_date;
	public $sepe_covered_date;
	public $professional_profile;
	public $job_requirements;
	public $inc_register_start_date;
	public $inc_register_end_date;
	public $inc_reference_group;
	public $inc_reference_entity;
	public $inc_reference_officer;
	public $inc_checkin_date;
	public $inc_remuneration;
	public $inc_cno_n1;
	public $inc_cno_n2;
	public $inc_cno_n3;
	public $stic_incorpora_locations_id;
	public $inc_location;
	public $inc_tasks_responsabilities;
	public $inc_observations;
	public $inc_synchronization_log;
	public $inc_synchronization_errors;
	public $inc_incorpora_record;
	public $inc_synchronization_date;
	
	function bean_implements($interface){
		switch($interface){
			case 'ACL': return true;
		}
		return false;
}
		
}
?>