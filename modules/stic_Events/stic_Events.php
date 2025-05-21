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
class stic_Events extends Basic {
    public $new_schema = true;
    public $module_dir = 'stic_Events';
    public $object_name = 'stic_Events';
    public $table_name = 'stic_events';
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
    public $type;
    public $status;
    public $start_date;
    public $end_date;
    public $timetable;
    public $discard_reason;
    public $max_attendees;
    public $status_not_invited;
    public $status_invited;
    public $status_confirmed;
    public $status_rejected;
    public $status_maybe;
    public $status_didnt_take_part;
    public $status_took_part;
    public $budget;
    public $currency_id;
    public $expected_cost;
    public $actual_cost;
    public $expected_income;
    public $actual_income;
    public $price;
    public $attendees;
    public $total_hours;
    public $status_drop_out;

    public function bean_implements($interface) {
        switch ($interface) {
        case 'ACL':
            return true;
        }

        return false;
    }

}