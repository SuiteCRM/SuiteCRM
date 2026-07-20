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
        $contactBean = $this->getRelatedContactBean();
        $offerBean = $this->getRelatedOfferBean();

        // Call the generic save() function from the SugarBean class
        if (empty($this->name)) {
            $contact_name = $this->getRelatedName($contactBean, 'first_name', 'last_name');
            $offer_name = $this->getRelatedName($offerBean);

            $this->name = $contact_name .' - '.$offer_name;
        }

        // If it is a new record, the assigned user of the offer is indicated in the job application
        if (!empty($offerBean) &&
            $this->assigned_user_id != $offerBean->assigned_user_id) {
            $this->assigned_user_id = $offerBean->assigned_user_id;
        }

        parent::save($check_notify);

        if (isset($this->status) && $this->status == 'accepted') {
            include_once 'modules/stic_Job_Applications/Utils.php';
            stic_Job_ApplicationsUtils::createWorkExperience($this);
        }

        include_once 'SticInclude/Utils.php';
        if (!empty($offerBean) && !empty($offerBean->id) && !empty($offerBean->offer_type) && ($offerBean->offer_type == 'volunteering')) {
            $contactBean = SticUtils::getRelatedBeanObject($this, 'stic_job_applications_contacts');

            if (!empty($contactBean) && is_object($contactBean) && !empty($contactBean->id)) {

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
                $offerProjectId = $offerBean->project_stic_job_offersproject_ida ?? '';

                // Ensure $contactRelationshipBeans is an array before looping
                if (!empty($offerProjectId) && is_array($contactRelationshipBeans)) {
                    foreach ($contactRelationshipBeans as $contactRelationshipBean) {
                        $relProjectId = $contactRelationshipBean->stic_contacts_relationships_projectproject_ida ?? '';
                        if ($relProjectId == $offerProjectId) {
                            $relationsCount++;
                            break;
                        }
                    }
                }

                // If there is no pre-voluntary and voluntary contact relationship, create a new pre-volunteer relationship
                if ($relationsCount == 0 && !empty($offerProjectId)) {
                    $relationshipBean = BeanFactory::newBean('stic_Contacts_Relationships');
                    $relationshipBean->relationship_type = 'pre-volunteer';
                    $relationshipBean->stic_contacts_relationships_contactscontacts_ida = $contactBean->id;
                    $relationshipBean->stic_contacts_relationships_projectproject_ida = $offerProjectId;
                    $relationshipBean->assigned_user_id = $offerBean->assigned_user_id ?? '';
                    $relationshipBean->save();
                }
            }
        }
    }

    /**
     * Get related contact bean
     *
     * @return SugarBean|null
     */
    protected function getRelatedContactBean()
    {
        return $this->getRelatedBean(
            $this->stic_job_applications_contactscontacts_ida ?? '',
            'stic_job_applications_contacts',
            'Contacts'
        );
    }

    /**
     * Get related offer bean
     *
     * @return SugarBean|null
     */
    protected function getRelatedOfferBean()
    {
        return $this->getRelatedBean(
            $this->stic_job_applications_stic_job_offersstic_job_offers_ida ?? '',
            'stic_job_applications_stic_job_offers',
            'stic_Job_Offers'
        );
    }

    /**
     * Get a related bean display name
     *
     * @param SugarBean|null $bean
     * @param string $firstField
     * @param string $lastField
     * @return string
     */
    protected function getRelatedName($bean, $firstField = '', $lastField = '')
    {
        if (empty($bean) || empty($bean->id)) {
            return '';
        }

        $beanName = $bean->name ?? '';
        if (!empty($beanName)) {
            return (string)$beanName;
        }

        if (!empty($firstField) || !empty($lastField)) {
            $firstName = !empty($firstField) ? (string)($bean->{$firstField} ?? '') : '';
            $lastName = !empty($lastField) ? (string)($bean->{$lastField} ?? '') : '';

            return trim($firstName . ' ' . $lastName);
        }

        return '';
    }

    /**
     * Get a related bean from id field or relationship link
     *
     * @param mixed $rawId
     * @param string $linkName
     * @param string $module
     * @return SugarBean|null
     */
    protected function getRelatedBean($rawId, $linkName, $module)
    {
        if (!is_object($rawId) && !empty($rawId)) {
            $bean = BeanFactory::getBean($module, (string)$rawId);
            if (!empty($bean) && !empty($bean->id)) {
                return $bean;
            }
        }

        if ($this->load_relationship($linkName)) {
            $relatedBeans = $this->{$linkName}->getBeans();
            if (!empty($relatedBeans)) {
                $firstBean = reset($relatedBeans);
                return !empty($firstBean) && !empty($firstBean->id) ? $firstBean : null;
            }
        }

        return null;
    }
}