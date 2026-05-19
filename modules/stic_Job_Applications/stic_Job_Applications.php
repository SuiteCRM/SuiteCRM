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

    public function save($check_notify = false) 
    {
        // Call the generic save() function from the SugarBean class
        if (empty($this->name)) {
            $contact_name = '';
            $offer_name = '';
            if (!empty($this->stic_job_applications_contactscontacts_ida)) {
                $contact_name = $this->stic_job_applications_contacts_name ?? '';
            }
            if (!empty($this->stic_job_applications_stic_job_offersstic_job_offers_ida)) {
                $offer_name = $this->stic_job_applications_stic_job_offers_name ?? '';
            }
            $this->name = $contact_name .' - '.$offer_name;
        }
        
        $offerBean = BeanFactory::getBean('stic_Job_Offers', $this->stic_job_applications_stic_job_offersstic_job_offers_ida);

        if (empty($offerBean) || empty($offerBean->id)) {
            $offerId = $this->getRelatedOfferId();
            if (!empty($offerId)) {
                $offerBean = BeanFactory::getBean('stic_Job_Offers', $offerId);
            }
        }
        // If it is a new record, the assigned user of the offer is indicated in the job application
        if (!empty($offerBean) &&
            $this->assigned_user_id != $offerBean->assigned_user_id) {
            $this->assigned_user_id = $offerBean->assigned_user_id;
        }

        parent::save($check_notify);

        if( $this->status == 'accepted'){
            include_once 'modules/stic_Job_Applications/Utils.php';
            stic_Job_ApplicationsUtils::createWorkExperience($this);
		}

        include_once 'SticInclude/Utils.php';
        $contactBean = SticUtils::getRelatedBeanObject($this, 'stic_job_applications_contacts');
        if (!empty($contactBean) && !empty($offerBean) && !empty($offerBean->offer_type) && ($offerBean->offer_type == 'volunteering')) 
        {
            // If the available time field has been updated, the corresponding field of the contact related also is updated.
            if (isset($this->available_time) && (!isset($this->fetched_row['available_time']) || $this->available_time != $this->fetched_row['available_time'])) {
                $contactBean->stic_time_availability_c = $this->available_time;
                $contactBean->save();
            }

            // Get the active contact relationships, whether pre-voluntary or voluntary, related to the contact
            $query = "stic_contacts_relationships.active = 1 AND (stic_contacts_relationships.relationship_type = 'pre-volunteer' OR stic_contacts_relationships.relationship_type = 'volunteer')";
            $contactRelationshipBeans = $contactBean->get_linked_beans(
                'stic_contacts_relationships_contacts',
                '',
                '',
                0,
                0,
                0,
                $query,
            );

            // Check if there is any relationship for the same project as the offer
            $relationsCount = 0;
            foreach ($contactRelationshipBeans as $contactRelationshipBean) {
                if ($contactRelationshipBean->stic_contacts_relationships_projectproject_ida == $offerBean->project_stic_job_offersproject_ida) {
                    $relationsCount++;
                    break;  
                }
            }

            // If there is no pre-voluntary and voluntary contact relationship, create a new pre-volunteer relationship
            if ($relationsCount == 0) {
                $relationshipBean = BeanFactory::newBean('stic_Contacts_Relationships');
                $relationshipBean->relationship_type = 'pre-volunteer';
                $relationshipBean->stic_contacts_relationships_contactscontacts_ida = $contactBean->id;
                $relationshipBean->stic_contacts_relationships_projectproject_ida = $offerBean->project_stic_job_offersproject_ida;
                $relationshipBean->assigned_user_id = $offerBean->assigned_user_id;
                $relationshipBean->save();
            }
        }
    }

    /**
     * Get related offer ID
     *
     * @return string
     */
    protected function getRelatedOfferId()
    {
        $rawOfferId = $this->stic_job_applications_stic_job_offersstic_job_offers_ida ?? '';
        if (!is_object($rawOfferId) && !empty($rawOfferId)) {
            return (string)$rawOfferId;
        }

        $requestOfferId = (string)($_REQUEST['stic_job_applications_stic_job_offersstic_job_offers_ida'] ?? '');
        if (!empty($requestOfferId)) {
            return $requestOfferId;
        }

        $fetchedOfferId = (string)($this->fetched_row['stic_job_applications_stic_job_offersstic_job_offers_ida'] ?? '');
        if (!empty($fetchedOfferId)) {
            return $fetchedOfferId;
        }

        if (empty($this->id)) {
            return '';
        }

        global $db;
        $applicationId = $db->quote((string)$this->id);
        $query = "SELECT rel.stic_job_applications_stic_job_offersstic_job_offers_ida AS offer_id
            FROM stic_job_applications_stic_job_offers_c rel
            WHERE rel.deleted = 0
              AND rel.stic_job_applications_stic_job_offersstic_job_applications_idb = '{$applicationId}'
            ORDER BY rel.date_modified DESC
            LIMIT 1";
        $row = $db->fetchByAssoc($db->query($query));

        return (string)($row['offer_id'] ?? '');
    }
}