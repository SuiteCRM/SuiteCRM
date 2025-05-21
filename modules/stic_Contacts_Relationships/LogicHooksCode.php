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
class stic_Contacts_RelationshipsLogicHooks
{

    public function before_save(&$bean, $event, $arguments)
    {
        // Create name if empty
        if (empty($bean->name)) {
            include_once 'SticInclude/Utils.php';
            global $app_list_strings;
            $relatedBean = SticUtils::getRelatedBeanObject($bean, 'stic_contacts_relationships_contacts');
            $bean->name = $relatedBean->first_name . ' ' . $relatedBean->last_name . ' - ' . $app_list_strings['stic_contacts_relationships_types_list'][$bean->relationship_type];
        }

         // Set active/inactive status
         include_once 'modules/stic_Contacts_Relationships/Utils.php';
         $bean->active=stic_Contacts_RelationshipsUtils::isActive($bean);
    }

    public function after_save(&$bean, $event, $arguments)
    {
        // Update active relationship types in Contacts in case of date or relationship type changes
        if (empty($bean->fetched_row) || $bean->start_date != $bean->fetched_row['start_date']
            || $bean->end_date != $bean->fetched_row['end_date']
            || $bean->relationship_type != $bean->fetched_row['relationship_type']
        ) {
            include_once 'SticInclude/Utils.php';
            if ($contactBean = SticUtils::getRelatedBeanObject($bean, 'stic_contacts_relationships_contacts')) {
                include_once 'modules/stic_Contacts_Relationships/Utils.php';
                stic_Contacts_RelationshipsUtils::setRelationshipType($contactBean->id);
                // Related with STIC#744 
                $contactBean->retrieve();
            }
            else {
                $GLOBALS['log']->error('Line '.__LINE__.': '.__METHOD__.': ' . 'The related Contact bean is empty');
            }
        }              
    }

    public function manage_relationships(&$bean, $event, $arguments)
    {
        // Update active relationship types in Contacts in case of relationship changes between Contacts and Contacts_Relationships
        if ($arguments['related_module'] == 'Contacts') {
            switch ($event) {
                case 'after_relationship_delete':
                case 'after_relationship_add':
                    if ($arguments['related_id']) {
                        $contactBean = BeanFactory::getBean('Contacts', $arguments['related_id']);
                        if ($contactBean !== false) {
                            include_once 'modules/stic_Contacts_Relationships/Utils.php';
                            stic_Contacts_RelationshipsUtils::setRelationshipType($contactBean->id);
                            // Related with STIC#744 
                            $contactBean->retrieve();
                        }                    
                    }
                    else {
                        $GLOBALS['log']->error('Line '.__LINE__.': '.__METHOD__.': ' . 'The related Contact Id is empty');
                    }
                    break;
                default:
                    break;
            }
        }
    }

}
