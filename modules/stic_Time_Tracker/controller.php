<?php

use Symfony\Component\Validator\Constraints\IsNull;

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
class stic_Time_TrackerController extends SugarController {


    /**
     * Returns the information necessary for the user's browser to determine whether to display the time stamp button in the top menu
     * @return void 
     */
    public function action_getTimeTrackerMenuButtonStatus()
    {
        // Check if the user has started any time registration today
        $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Checking time tracker registration status.');
        global $current_user;
        
        // Check if time tracker module is active
        $timeTrackerModuleActive = false;
        $administration = BeanFactory::newBean('Administration');
        $administration->retrieveSettings('MySettings');
        if (isset($administration->settings) && isset($administration->settings['MySettings_tab'])) {
            $tabs = $administration->settings['MySettings_tab'];
            //make sure serialized string is not empty
            if (!empty($tabs)) {
                $tabs = base64_decode($tabs);
                $tabs = unserialize($tabs);
                $timeTrackerModuleActive = in_array('stic_Time_Tracker', $tabs) ? 1 : 0;
            }
        }

        // Check if there is a time tracker record for the employee in today
        $data = stic_Time_Tracker::getLastTodayTimeTrackerRecord($current_user->id) ?? [];
        $todayRegistrationStarted = !is_array($data) ? 0 : (empty($data["end_date"]) ? 1 : 0);

        $data = array(
            'timeTrackerModuleActive' => $timeTrackerModuleActive,
            'timeTrackerActiveInEmployee' => $current_user->stic_clock_c ? 1:0,
            'todayRegistrationStarted' => $todayRegistrationStarted,
        );
        
        // return the json result
        ob_clean();
        $json = json_encode($data);
        header('Content-Type: application/json');
        echo $json;
        ob_flush();
        die();
    }

    /**
     * Returns today's last time tracking record for the user logged in to the CRM to the user's browser
     * @return void
     */
    public function action_getLastTodayTimeTrackerRecordForEmployee()
    {
        // Check if the user has started any time registration today
        $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Checking last time tracker record for today.');
        
        // Get now in current_user timezone and format
        global $timedate, $current_user;
        $currentUserNow = $timedate->getNow(true);
        $currentUserNow = $currentUserNow->format($timedate->get_date_time_format($current_user));

        // Get record data
        $recordName = stic_Time_Tracker::getLastTodayTimeTrackerRecord($current_user->id)['name'] ?? '';
        $data = array(
            'date' => $currentUserNow,
            'recordName' => $recordName,
        );
        
        // return the json result
        ob_clean();
        $json = json_encode($data);
        header('Content-Type: application/json');
        echo $json;
        ob_flush();
        die();
    }

    /**
     * Create or update a time record for the user logged in to the CRM
     * @return void
     */
    public function action_createOrUpdateTodayRegister()
    {
        global $current_user, $timedate;
        $data = json_decode(file_get_contents('php://input'), true);
        // Check if the user has started any time registration today
        include_once 'modules/stic_Time_Tracker/stic_Time_Tracker.php';        
        $todayUserRegistrationData = stic_Time_Tracker::getLastTodayTimeTrackerRecord($current_user->id) ?? [];
        $todayRegistrationStarted =  $todayUserRegistrationData ? empty($todayUserRegistrationData["end_date"]) : false;

        $date = $timedate->fromUser($data['date'], $current_user);
        $date = $timedate->asDb($date);

        if (!$todayRegistrationStarted) {           
            // Create today's time tracker record for the current user
            $bean = BeanFactory::getBean($this->module);
            $bean->start_date = $date;
            $bean->end_date = '';            
            $bean->assigned_user_id = $current_user->id;
            $bean->assigned_user_name = $current_user->name;
            $bean->description=$data['description'];            
        } else {
            // Update the end date field of today's time tracker for the current user
            $bean = BeanFactory::getBean($this->module, $todayUserRegistrationData['id']);
            $bean->start_date = $todayUserRegistrationData['start_date'];
            $bean->end_date = $date;
            $bean->name = ''; // delete the name so that it is recalculated again
            $bean->description=$bean->description.'
            
            '.$data['description']; 
        }
        $bean->save(false);
    }
}