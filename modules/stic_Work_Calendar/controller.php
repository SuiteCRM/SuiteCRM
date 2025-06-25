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

require_once 'modules/stic_Work_Calendar/stic_Work_Calendar.php';
class stic_Work_CalendarController extends SugarController 
{
    /**
     * Check:
     * - If there is already a non-work record that takes up the entire day, in that case, it is not posible to create the record
     * - If exist a record that does not occupy the entire day, in that case, since the record to be created is an all-day record
     * and returns the result to the browser's user
     * @return void
     */
    public function action_existsOtherTypesIncompatibleRecords()
    {
        // getParams
        $data = json_decode(file_get_contents('php://input'), true);
        if (!empty($data)) 
        {
            $id = $data['id'];
            $startDate = $data["startDate"];
            $endDate = $data["endDate"];
            $type = $data['type'];
            $assignedUserId = $data['assignedUserId'];

            global $timedate, $current_user;
            $assignedUser = BeanFactory::getBean('Users', $assignedUserId);            
            $startDate = $timedate->fromUser($startDate, $assignedUser);
            $startDate = $timedate->asDb($startDate);

            require_once 'modules/stic_Work_Calendar/stic_Work_Calendar.php';
            if (in_array($type, stic_Work_Calendar::ALL_DAY_TYPES)) {
                $endDate = $timedate->fromDbFormat($startDate, TimeDate::DB_DATETIME_FORMAT);
                $endDate = $endDate->modify("next day");
                $endDate = $timedate->asDb($endDate, $current_user);     
            } else {
                $endDate = $timedate->fromUser($endDate, $assignedUser);
                $endDate = $timedate->asDb($endDate);            
            }

            require_once 'modules/stic_Work_Calendar/Utils.php';
            ob_clean();
            echo(stic_Work_CalendarUtils::existsRecordsWithIncompatibleType($id, $startDate, $endDate, $type, $assignedUserId));
            ob_flush();
        }
        die('');    
    }


    /**
     * Renders the view of periodic creation of work calendar records
     * @return void
     */
    public function action_showWorkCalendarAssistant()
    {
        $this->view = "workcalendarassistant"; //call for the view file in views dir
    }

    /**
     * Runs the periodic creation of work calendar records.
     * @return void
     */
    public function action_createPeriodicWorkCalendarRecords() {
        include_once 'Utils.php';
        stic_Work_CalendarUtils::createPeriodicWorkCalendarRecords();
    }
    
    

    /**
     * Renders the summary view with the results of the periodic creation of work calendar records
     * @return void
     */
    public function action_workCalendarAssistantSummary() {
        $this->view = "workcalendarassistantsummary";
    }



    /**
     * Renders the view to update the time of the Start date and End date fields
     * @return void
     */
    public function action_showMassUpdateDatesForm()
    {

        // If select_entire_list is used, select all stic_Work_Calendar records undeleted and different that current record
        if (isset($_REQUEST['select_entire_list']) && $_REQUEST['select_entire_list'] == '1') {

            // If the popup selection was previously filtered, use custom function to generate custom where conditions
            if (isset($_REQUEST['current_query_by_page'])) {
                include 'Utils.php';
                $where = $this->generateAllWhereClausesFromFilter($_REQUEST);
            }
            $entireListSQL = "SELECT distinct id FROM stic_work_calendar WHERE deleted=0";
            if (!empty($where)) {
                $entireListSQL = $entireListSQL . ' AND ' . $where;
            }

            $entireListSQLResults = $GLOBALS['db']->query($entireListSQL);
            unset($relateIds);
            while ($row = $GLOBALS['db']->fetchByAssoc($entireListSQLResults)) {
                $relateIds[] = $row['id'];
            }
            $_REQUEST['uid'] = empty($relateIds) ? $_REQUEST['uid'] : implode(',', $relateIds);
        }        
        $this->view = "massupdatedatesform"; //call for the view file in views dir
    }


    /**
     * Runs the time update for the Start Date and End Date fields
     * @return void
     */
    public function action_runMassUpdateDates()
    {
        global $timedate, $current_user;
        // Check if any operator has been indicated
        if (!empty($_REQUEST['start_date_operator']) || !empty($_REQUEST['end_date_operator'])) 
        {            
            // Calculate the new date on the selected records
            $selectedIds = explode(',', $_REQUEST['selectedIDs']);
            foreach ($selectedIds as $id) 
            {            
                $bean = BeanFactory::getBean('stic_Work_Calendar', $id);
                $update = false;
                if (!empty($_REQUEST['start_date_operator'])) {                
                    $startDateArray = array (
                        'field' => 'start_date',
                        'operator' => $_REQUEST['start_date_operator'],
                        'hours' => $_REQUEST['start_date_hours'],
                        'minutes' => $_REQUEST['start_date_minutes']
                    );
                    $bean->start_date = $this->calculateNewDatesInRecord($bean, $startDateArray);
                    $update = true;
                } else {
                   $startDate = $timedate->fromUser($bean->start_date, $current_user);
                   $bean->start_date = $timedate->asDb($startDate);
                }
                if (!empty($_REQUEST['end_date_operator'])) {                
                    $endDateArray = array (
                        'field' => 'end_date',                        
                        'operator' => $_REQUEST['end_date_operator'],
                        'hours' => $_REQUEST['end_date_hours'],
                        'minutes' => $_REQUEST['end_date_minutes']
                    );
                    $bean->end_date = $this->calculateNewDatesInRecord($bean, $endDateArray);
                    $update = true;
                } else {
                    $endDate = $timedate->fromUser($bean->end_date, $current_user);
                    $bean->end_date = $timedate->asDb($endDate);
                }         
                
                if ($update) $bean->save(false);  
            }
            // Redirect to the list view
            SugarApplication::redirect('index.php?module=stic_Work_Calendar');
        }
    }
    

    /**
     * Calculates new dates and times for the Start Date and End Date fields after updating the time
     * @param $bean record with the information to update
     * @param $infoDateArray information necessary to perform the update the time
     * @return String the new date
     */
    protected function calculateNewDatesInRecord($bean, $infoDateArray)
    {
        // Calculate new date
        $stringDate = '';
        if (!empty($infoDateArray['operator'])){
            $field = $infoDateArray['field'];
            if (!empty($bean->$field)){
                $infoDateArray['original'] = $bean->$field;
                $stringDate = $this->calculateNewDate($infoDateArray);
            } else {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'The new date could not be calculated in the work calendar record with id = '  . $id);                        
            }
        }
        return $stringDate;
    }

    
    /**
     * Update the time of a date
     * @param $dateInfo information necessary to perform the update the time
     * @return String the new date
     */
    protected function calculateNewDate($dateInfo) 
    {
        // User timezone and offset in hours
        global $current_user, $timedate;

         // Calculate the new date
        $format ='Y-m-d H:i:s';
        $date = $timedate->fromUser($dateInfo['original'], $current_user);
        $stringDate = $timedate->asDb($date);
        $date = DateTime::createFromFormat($format, $stringDate);
        
        switch ($dateInfo['operator']) {
            case '=':
                $userTz = $current_user->getPreference('timezone');
                $dateInUserTZ = new DateTime($stringDate, new DateTimeZone($userTz));   
                $dateOffsetInHours = $dateInUserTZ->getOffset()/3600;
                $hours = $dateInfo['hours'] - $dateOffsetInHours;       
                $date->setTime($hours, $dateInfo['minutes']);                            
                break;
            case '+':
                $date->modify("+{$dateInfo['hours']} hours");                        
                $date->modify("+{$dateInfo['minutes']} minutes");                        
                break;
            case '-':
                $date->modify("-{$dateInfo['hours']} hours");                        
                $date->modify("-{$dateInfo['minutes']} minutes");    
                break;
            default:
                break; 
        }
        return $date->format($format);
    }

    /**
     * Returns generated where SQL conditions from popup view. Fuction uses the $_REQUEST object generated by popup callback javascript function
     * The function is based in include/generic/Save2.php file, which contains the controller code for saving popup selection records when usin Select button in a "normal" subpanel throw SubPanelTopSelectButton widget.
     * @return String Generated where SQL conditions
     */
    protected function generateAllWhereClausesFromFilter() 
    {
        require_once 'include/formbase.php';

        // If the user selected "All records" from the selection menu, we pull up the list
        // based on the query they used on that popup to relate them to the parent record
        $current_query_by_page = $_REQUEST['current_query_by_page'];
        $current_query_by_page_array = json_decode(html_entity_decode($current_query_by_page), true);

        $module = $current_query_by_page_array['module'];
        $seed = BeanFactory::getBean($module);
        if (empty($seed)) {
            sugar_die($GLOBALS['app_strings']['ERROR_NO_BEAN']);
        }
        $where_clauses = '';
        require_once 'include/SearchForm/SearchForm2.php';

        if (file_exists('custom/modules/' . $module . '/metadata/metafiles.php')) {
            require 'custom/modules/' . $module . '/metadata/metafiles.php';
        } elseif (file_exists('modules/' . $module . '/metadata/metafiles.php')) {
            require 'modules/' . $module . '/metadata/metafiles.php';
        }

        if (file_exists('custom/modules/' . $module . '/metadata/searchdefs.php')) {
            require_once 'custom/modules/' . $module . '/metadata/searchdefs.php';
        } elseif (!empty($metafiles[$module]['searchdefs'])) {
            require_once $metafiles[$module]['searchdefs'];
        } elseif (file_exists('modules/' . $module . '/metadata/searchdefs.php')) {
            require_once 'modules/' . $module . '/metadata/searchdefs.php';
        }

        if (!empty($metafiles[$module]['searchfields'])) {
            require_once $metafiles[$module]['searchfields'];
        } elseif (file_exists('modules/' . $module . '/metadata/SearchFields.php')) {
            require_once 'modules/' . $module . '/metadata/SearchFields.php';
        }
        if (!empty($searchdefs) && !empty($searchFields)) {
            $searchForm = new SearchForm($seed, $module);
            $displayView = explode("_", $current_query_by_page_array["searchFormTab"])[0] ?? 'basic';               
            $searchForm->setup($searchdefs, $searchFields, 'SearchFormGeneric.tpl', $displayView);
            $searchForm->populateFromArray($current_query_by_page_array, $searchForm->displayView);
            $where_clauses_arr = $searchForm->generateSearchWhere(true, $module);
            if (count($where_clauses_arr) > 0) {
                $where_clauses = '(' . implode(' ) AND ( ', $where_clauses_arr) . ')';
            }
        }
        return $where_clauses;
    }
}