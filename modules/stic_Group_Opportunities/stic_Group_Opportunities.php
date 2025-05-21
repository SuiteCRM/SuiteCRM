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
class stic_Group_Opportunities extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Group_Opportunities';
    public $object_name = 'stic_Group_Opportunities';
    public $table_name = 'stic_group_opportunities';
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
    public $document_status;
    public $amount_requested;
    public $amount_awarded;
    public $amount_received;
    public $start_date;
    public $validation_date;
    public $presentation_date;
    public $resolution_date;
    public $advance_date;
    public $justification_date;
    public $payment_date;
    public $folder;
    public $contact_id;
    public $contact;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }


    public function save($check_notify = false)
    {
        include_once 'SticInclude/Utils.php';
        $id = parent::save($check_notify);

        // Update Name: Account - Opportunity
        $accountBean = SticUtils::getRelatedBeanObject($this, 'stic_group_opportunities_accounts');
        $opportunityBean = SticUtils::getRelatedBeanObject($this, 'stic_group_opportunities_opportunities');
        $this->name = $accountBean->name . ' - ' . $opportunityBean->name;

        parent::save($check_notify);

        return $id;
    }

}
