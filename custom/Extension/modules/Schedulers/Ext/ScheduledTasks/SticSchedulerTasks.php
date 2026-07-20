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
// Scheduled task that calculates the age of the contacts in "contacts" database table, daily. The age is stored in the field stic_age_c.

$job_strings[]='calculateContactsAge';

function calculateContactsAge() {
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task calculateContactsAge');
    require_once 'custom/modules/Contacts/SticUtils.php';
    return ContactsUtils::calculateContactsAge();
}


// Scheduled task that resets the value of some config_override properties, like developer_mode or log level

$job_strings[]='sticCleanConfig';

function sticCleanConfig() {
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task sticCleanConfig');
    require_once 'SticInclude/CleanConfig.php';
    return SticCleanConfig::cleanConfig();
}


// Scheduled task for attendances daily generation in multisession events.
// Only attendances for current day sessions will be created.

$job_strings[] = 'createAttendances';

function createAttendances()
{
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task createAttendances');
    require_once 'modules/stic_Attendances/Utils.php';
    return stic_AttendancesUtils::createAttendances();
}


// Scheduled task for creating monthy recurring payments.
// Only payments linked to payment commitments will be created.

$job_strings[] = 'createCurrentMonthRecurringPayments';

function createCurrentMonthRecurringPayments()
{
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task createCurrentMonthRecurringPayments');
    require_once 'modules/stic_Payments/Utils.php';
    return stic_PaymentsUtils::createCurrentMonthRecurringPayments();
}


// Scheduled task that reminds the users by Email of opportunities approaching the due date.

$job_strings[]='opportunitiesReminder';

function opportunitiesReminder() {

    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task opportunitiesReminder');
    require_once 'custom/modules/Opportunities/SticUtils.php';
    return OpportunitiesUtils::opportunitiesReminder();
}

// Scheduled task for medication logs daily generation
// Only attendances for current day will be created.

$job_strings[] = 'createMedicationLogs';

function createMedicationLogs()
{
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task createMedicationLogs');
    require_once 'modules/stic_Medication_Log/Utils.php';
    return stic_Medication_LogUtils::createLogs();
}


// Scheduled task that runs different kind of validation for the data located in the CRM.

$job_strings[] = 'validationActions';

/**
 * Data analysis process.
 * Load the list of linked validation actions,
 * retrieve the necessary data set and execute the actions.
 * @param $scheduledJob Object Bean of the scheduled task
 * @return boolean
 */
function validationActions($scheduledJob)
{
    include_once 'modules/stic_Validation_Actions/Utils.php';
    return stic_Validation_ActionsUtils::runSchedulersValidationTasks($scheduledJob);
}


// Scheduled task that deletes from database the records where deleted = 1 was set at least N days before

$job_strings[] = 'sticPurgeDatabase';

/**
 * Deletes from the database the records where deleted = 1 was set at least N days before 
 * (N depends on config value 'stic_purge_database_days').
 * @return boolean
 */
function sticPurgeDatabase()
{
    global $sugar_config;
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task purgeDatabase');

    $db = DBManagerFactory::getInstance();
    $tables = $db->getTablesArray();
    $queryString = array();

    if (!empty($tables)) {
        foreach ($tables as $kTable => $table) {
            // find tables with deleted=1
            $columns = $db->get_columns($table);
            // no deleted - won't delete
            if (empty($columns['deleted']) || empty($columns['date_modified'])) {
                continue;
            }

            $custom_columns = array();
            if (array_search($table . '_cstm', $tables)) {
                $custom_columns = $db->get_columns($table . '_cstm');
            }

            if (empty($sugar_config['stic_purge_database_days'])) {
                $GLOBALS['log']->error('Line '.__LINE__.': '.__METHOD__.':  $sugar_config["stic_purge_database_days"] is not provided.');
                return false;
            }

            $backupDays = $sugar_config['stic_purge_database_days'];             
            $today = date("Y-m-d");
            $limitDate = date("Y-m-d", strtotime($today." - ". $backupDays ."days"));

            $qDel = "SELECT * FROM $table WHERE deleted = 1 AND date_modified <= '".$limitDate."'";
            $rDel = $db->query($qDel);

            while ($aDel = $db->fetchByAssoc($rDel, false)) {
                $id = $db->quoted($aDel['id']);
                if (!empty($custom_columns) && !empty($aDel['id'])) {
                    $db->query('DELETE FROM ' . $table . '_cstm WHERE id_c = '.$id );
                }
                $db->query('DELETE FROM ' . $table . ' WHERE id = '.$id);
            }
        }
        return true;
    }
    return false;
}

// Scheduled task to process Phone messages queued
$job_strings[] = 'sticSendPhoneMessages';

/**
 * Deletes from the database the records where deleted = 1 was set at least N days before 
 * (N depends on config value 'stic_purge_database_days').
 * @return boolean
 */
function sticSendPhoneMessages() {
    require_once('modules/stic_MessagesMan/Utils.php');

    return stic_MessagesManUtils::sendQueuedMessages(false);
}

// Scheduled task to cancel expired AWF tickets 
$job_strings[] = 'sticAwfCancelExpiredTickets';

/**
 * Cleans up AWF Deferred Tickets that have expired or are blocked (zombies).
 */
function sticAwfCancelExpiredTickets() {
    require_once 'modules/stic_AWF_Forms/Utils.php';
    
    $GLOBALS['log']->debug('Line '.__LINE__.': '.__METHOD__.':  Running the task sticAwfCancelExpiredTickets');

    try {
        stic_AWF_FormsUtils::cancelExpiredTickets();
        return true;
    } catch (\Exception $e) {
        $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.':  sticAwfCancelExpiredTickets Error: ' . $e->getMessage());
        return false;
    }
}