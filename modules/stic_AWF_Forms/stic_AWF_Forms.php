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

class stic_AWF_Forms extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_AWF_Forms';
    public $object_name = 'stic_AWF_Forms';
    public $table_name = 'stic_awf_forms';
    public $importable = false;

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
    public $start_date;
    public $end_date;
    public $public_url;
    public $processing_mode;
    public $configuration;
    public $custom_css;
    public $custom_js;
    public $custom_header_html;
    public $custom_footer_html;
    public $analytics_views;
    public $analytics_blocked;
    public $analytics_submissions;
    public $analytics_spam;

	
    public function bean_implements($interface)
    {
        switch($interface)
        {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function save($check_notify = false)
    {
        if ($this->new_with_id && empty($this->fetched_row['id'])) {
            $this->reset_analytics_and_public_url();
        }

        return parent::save($check_notify);
    }

    public function reset_analytics_and_public_url()
    {
        $this->analytics_views = 0;
        $this->analytics_blocked = 0;
        $this->analytics_submissions = 0;
        $this->analytics_spam = 0;
        $this->analytics_referrers = '';
        $this->public_url = '';
    }
}
