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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/MVC/View/views/view.detail.php';
require_once 'SticInclude/Views.php';

class stic_BookingsViewDetail extends ViewDetail
{

    public function __construct()
    {

        parent::__construct();

    }

    public function preDisplay()
    {
        global $timedate, $current_user;

        // If all_day is checked then remove the hours and minutes
        // and apply timezone to the dates

        if ($this->bean->all_day == '1') {
            $startDate = explode(' ', $this->bean->fetched_row['start_date']);
            if ($startDate[1] > "12:00") {
                $startDate = $timedate->fromDbDate($startDate[0]);
                $startDate = $startDate->modify("next day");
                $startDate = $timedate->asUserDate($startDate, false, $current_user);
                $this->bean->start_date = $startDate;
            } else {
                $startDate = $timedate->fromDbDate($startDate[0]);
                $startDate = $timedate->asUserDate($startDate, false, $current_user);
                $this->bean->start_date = $startDate;
            }

            $endDate = explode(' ', $this->bean->fetched_row['end_date']);
            if ($endDate[1] > "12:00") {
                $endDate = $timedate->fromDbDate($endDate[0]);
                $endDate = $timedate->asUserDate($endDate, false, $current_user);
                $this->bean->end_date = $endDate;
            } else {
                $endDate = $timedate->fromDbDate($endDate[0]);
                $endDate = $endDate->modify("previous day");
                $endDate = $timedate->asUserDate($endDate, false, $current_user);
                $this->bean->end_date = $endDate;
            }

            if (!empty($this->bean->fetched_row['planned_start_date'])) {
                $plannedStartDate = explode(' ', $this->bean->fetched_row['planned_start_date']);
                if ($plannedStartDate[1] > "12:00") {
                    $plannedStartDate = $timedate->fromDbDate($plannedStartDate[0]);
                    $plannedStartDate = $plannedStartDate->modify("next day");
                    $plannedStartDate = $timedate->asUserDate($plannedStartDate, false, $current_user);
                    $this->bean->planned_start_date = $plannedStartDate;
                } else {
                    $plannedStartDate = $timedate->fromDbDate($plannedStartDate[0]);
                    $plannedStartDate = $timedate->asUserDate($plannedStartDate, false, $current_user);
                    $this->bean->planned_start_date = $plannedStartDate;
                }
            }

            if(!empty($this->bean->fetched_row['planned_end_date'])){
                $plannedEndDate = explode(' ', $this->bean->fetched_row['planned_end_date']);
                if ($plannedEndDate[1] > "12:00") {
                    $plannedEndDate = $timedate->fromDbDate($plannedEndDate[0]);
                    $plannedEndDate = $timedate->asUserDate($plannedEndDate, false, $current_user);
                    $this->bean->planned_end_date = $plannedEndDate;
                } else {
                    $plannedEndDate = $timedate->fromDbDate($plannedEndDate[0]);
                    $plannedEndDate = $plannedEndDate->modify("previous day");
                    $plannedEndDate = $timedate->asUserDate($plannedEndDate, false, $current_user);
                    $this->bean->planned_end_date = $plannedEndDate;
                }
            }
        }

        parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here you custom code

    }

    public function display()
    {

        parent::display();

        SticViews::display($this);
        echo getVersionedScript("modules/stic_Bookings/Utils.js");
    }
}
