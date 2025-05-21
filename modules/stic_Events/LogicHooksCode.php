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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class stic_EventsLogicHooks {

    public function after_save(&$bean, $event, $arguments) {

        // The percentage is only (re)calculated when total_hours changes its value
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Total hours:' . $bean->total_hours);
        if (empty($bean->fetched_row) || $bean->total_hours != $bean->fetched_row['total_hours']) {

            // Load the event bean with its related registrations
            $bean->load_relationship('stic_registrations_stic_events');

            // Calculate the attendance percentage for every related registration
            foreach ($bean->stic_registrations_stic_events->getBeans() as $registration) {
                if ($bean->total_hours != 0) {
                    $registration->attended_hours = empty($registration->attended_hours) ? 0 : $registration->attended_hours;
                    $registration->attendance_percentage = ((float)$bean->total_hours != 0) ? (float)$registration->attended_hours / (float)$bean->total_hours * 100 : 0;
                } else {
                    $registration->attendance_percentage = 0;
                }
                $registration->save();
            }
        }
    }
}
