<?php

use SuiteCRM\Utility\SuiteValidator;

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/Services/NormalizeRecords/NormalizeRecords.php';

/**
 * Set up an array of Jobs with the appropriate metadata
 * 'jobName' => array (
 *        'X' => 'name',
 * )
 * 'X' should be an increment of 1
 * 'name' should be the EXACT name of your function
 *
 * Your function should not be passed any parameters
 * Always  return a Boolean. If it does not the Job will not terminate itself
 * after completion, and the webserver will be forced to time-out that Job instance.
 * DO NOT USE sugar_cleanup(); in your function flow or includes.  this will
 * break Schedulers.  That function is called at the foot of cron.php
 */

/**
 * This array provides the Schedulers admin interface with values for its "Job"
 * dropdown menu.
 */
$job_strings = array(
    0 => 'refreshJobs',
    1 => 'pollMonitoredInboxes',
    2 => 'runMassEmailCampaign',
    3 => 'pruneDatabase',
    4 => 'trimTracker',
    5 => 'pollMonitoredInboxesForBouncedCampaignEmails',
    6 => 'pollMonitoredInboxesAOP',
    7 => 'aodIndexUnindexed',
    8 => 'aodOptimiseIndex',
    9 => 'aorRunScheduledReports',
    10 => 'processAOW_Workflow',
    12 => 'sendEmailReminders',
    14 => 'cleanJobQueue',
    15 => 'removeDocumentsFromFS',
    16 => 'trimSugarFeeds',
    17 => 'syncGoogleCalendar',
    18 => 'runElasticSearchIndexerScheduler',
);

/**
 * Job 0 refreshes all job schedulers at midnight
 * DEPRECATED
 */
function refreshJobs()
{
    return true;
}


/**
 * Job 1
 */
function pollMonitoredInboxes()
{
    $_bck_up = array('team_id' => $GLOBALS['current_user']->team_id, 'team_set_id' => $GLOBALS['current_user']->team_set_id);
    $GLOBALS['log']->info('----->Scheduler fired job of type pollMonitoredInboxes()');
    global $dictionary;
    global $app_strings;


    require_once('modules/Emails/EmailUI.php');

    $ie = BeanFactory::newBean('InboundEmail');
    $emailUI = new EmailUI();
    $r = $ie->db->query('SELECT id, name FROM inbound_email WHERE is_personal = 0 AND deleted=0 AND status=\'Active\' AND mailbox_type != \'bounce\'');
    $GLOBALS['log']->debug('Just got Result from get all Inbounds of Inbound Emails');

    while ($a = $ie->db->fetchByAssoc($r)) {
        $GLOBALS['log']->debug('In while loop of Inbound Emails');
        $ieX = BeanFactory::newBean('InboundEmail');
        $ieX->retrieve($a['id']);
        $mailboxes = $ieX->mailboxarray;
        foreach ($mailboxes as $mbox) {
            $ieX->mailbox = $mbox;
            $newMsgs = array();
            $msgNoToUIDL = array();
            $connectToMailServer = false;
            if ($ieX->isPop3Protocol()) {
                $msgNoToUIDL = $ieX->getPop3NewMessagesToDownloadForCron();
                // get all the keys which are msgnos;
                $newMsgs = array_keys($msgNoToUIDL);
            }
            if ($ieX->connectMailserver() == 'true') {
                $connectToMailServer = true;
            } // if

            $GLOBALS['log']->debug('Trying to connect to mailserver for [ ' . $a['name'] . ' ]');
            if ($connectToMailServer) {
                $GLOBALS['log']->debug('Connected to mailserver');
                if (!$ieX->isPop3Protocol()) {
                    $newMsgs = $ieX->getNewMessageIds();
                }

                $isGroupFolderExists = false;

                if (is_array($newMsgs)) {
                    $current = 1;
                    $total = count($newMsgs);
                    require_once("include/SugarFolders/SugarFolders.php");
                    $sugarFolder = new SugarFolder();
                    $groupFolderId = $ieX->groupfolder_id;
                    $users = array();
                    if ($groupFolderId != null && $groupFolderId != "") {
                        $sugarFolder->retrieve($groupFolderId);
                        $isGroupFolderExists = true;
                    } // if
                    $messagesToDelete = array();
                    if ($ieX->isMailBoxTypeCreateCase()) {
                        $users[] = $sugarFolder->assign_to_id;
                        $distributionMethod = $ieX->get_stored_options("distrib_method", "");
                        if ($distributionMethod != 'roundRobin') {
                            $counts = $emailUI->getAssignedEmailsCountForUsers($users);
                        } else {
                            $lastRobin = $emailUI->getLastRobin($ieX);
                        }
                        $GLOBALS['log']->debug('distribution method id [ ' . $distributionMethod . ' ]');
                    }
                    foreach ($newMsgs as $k => $msgNo) {
                        $uid = $msgNo;
                        if ($ieX->isPop3Protocol()) {
                            $uid = $msgNoToUIDL[$msgNo];
                        } else {
                            $uid = $ieX->getImap()->getUid($msgNo);
                        } // else
                        if ($isGroupFolderExists) {
                            if ($ieX->returnImportedEmail($msgNo, $uid)) {
                                // add to folder
                                $sugarFolder->addBean($ieX);
                                if ($ieX->isPop3Protocol()) {
                                    $messagesToDelete[] = $msgNo;
                                } else {
                                    $messagesToDelete[] = $uid;
                                }
                                if ($ieX->isMailBoxTypeCreateCase()) {
                                    $userId = "";
                                    if ($distributionMethod == 'roundRobin') {
                                        if (count($users) === 1) {
                                            $userId = $users[0];
                                            $lastRobin = $users[0];
                                        } else {
                                            $userIdsKeys = array_flip($users); // now keys are values
                                            $thisRobinKey = $userIdsKeys[$lastRobin] + 1;
                                            if (!empty($users[$thisRobinKey])) {
                                                $userId = $users[$thisRobinKey];
                                                $lastRobin = $users[$thisRobinKey];
                                            } else {
                                                $userId = $users[0];
                                                $lastRobin = $users[0];
                                            }
                                        } // else
                                    } else {
                                        if (count($users) === 1) {
                                            foreach ($users as $k => $value) {
                                                $userId = $value;
                                            } // foreach
                                        } else {
                                            asort($counts); // lowest to highest
                                            $countsKeys = array_flip($counts); // keys now the 'count of items'
                                            $leastBusy = array_shift($countsKeys); // user id of lowest item count
                                            $userId = $leastBusy;
                                            $counts[$leastBusy] = $counts[$leastBusy] + 1;
                                        }
                                    } // else
                                    $GLOBALS['log']->debug('userId [ ' . $userId . ' ]');
                                    $ieX->handleCreateCase($ieX->email, $userId);
                                } // if
                            } // if
                        } else {
                            if ($ieX->isAutoImport()) {
                                $ieX->returnImportedEmail($msgNo, $uid);
                            } else {
                                /*If the group folder doesn't exist then download only those messages
                                 which has caseid in message*/
                                $ieX->getMessagesInEmailCache($msgNo, $uid);
                                $email = BeanFactory::newBean('Emails');
                                $header = $ieX->getImap()->getHeaderInfo($msgNo);
                                $email->name = $ieX->handleMimeHeaderDecode($header->subject);
                                $email->from_addr = $ieX->convertImapToSugarEmailAddress($header->from);
                                isValidEmailAddress($email->from_addr);
                                $email->reply_to_email = $ieX->convertImapToSugarEmailAddress($header->reply_to);
                                if (!empty($email->reply_to_email)) {
                                    $contactAddr = $email->reply_to_email;
                                    isValidEmailAddress($contactAddr);
                                } else {
                                    $contactAddr = $email->from_addr;
                                    isValidEmailAddress($contactAddr);
                                }
                                $mailBoxType = $ieX->mailbox_type;
                                $ieX->handleAutoresponse($email, $contactAddr);
                            } // else
                        } // else
                        $GLOBALS['log']->debug('***** On message [ ' . $current . ' of ' . $total . ' ] *****');
                        $current++;
                    } // foreach
                    // update Inbound Account with last robin
                    if ($ieX->isMailBoxTypeCreateCase() && $distributionMethod == 'roundRobin') {
                        $emailUI->setLastRobin($ieX, $lastRobin);
                    } // if
                } // if
                if ($isGroupFolderExists) {
                    $leaveMessagesOnMailServer = $ieX->get_stored_options("leaveMessagesOnMailServer", 0);
                    if (!$leaveMessagesOnMailServer) {
                        if ($ieX->isPop3Protocol()) {
                            $ieX->deleteMessageOnMailServerForPop3(implode(",", $messagesToDelete));
                        } else {
                            $ieX->deleteMessageOnMailServer(implode($app_strings['LBL_EMAIL_DELIMITER'], $messagesToDelete));
                        }
                    }
                }
            } else {
                $GLOBALS['log']->fatal("SCHEDULERS: could not get an IMAP connection resource for ID [ {$a['id']} ]. Skipping mailbox [ {$a['name']} ].");
                // cn: bug 9171 - continue while
            } // else
        } // foreach
        $ieX->getImap()->expunge();
        $ieX->getImap()->close(CL_EXPUNGE);
    } // while;
    return true;
}

/**
 * Job 2
 */
function runMassEmailCampaign()
{
    if (!class_exists('LoggerManager')) {
    }
    $GLOBALS['log'] = LoggerManager::getLogger('emailmandelivery');
    $GLOBALS['log']->debug('Called:runMassEmailCampaign');

    if (!class_exists('DBManagerFactory')) {
        require('include/database/DBManagerFactory.php');
    }

    global $beanList;
    global $beanFiles;
    require("config.php");
    require('include/modules.php');
    if (!class_exists('AclController')) {
        require('modules/ACL/ACLController.php');
    }

    require('modules/EmailMan/EmailManDelivery.php');
    return true;
}

/**
 *  Job 3
 */
function pruneDatabase()
{
    $GLOBALS['log']->info('----->Scheduler fired job of type pruneDatabase()');
    $backupDir = sugar_cached('backups');
    $backupFile = 'backup-pruneDatabase-GMT0_' . gmdate('Y_m_d-H_i_s', strtotime('now')) . '.php';

    $db = DBManagerFactory::getInstance();
    $tables = $db->getTablesArray();
    $queryString = array();

    if (!empty($tables)) {
        foreach ($tables as $kTable => $table) {
            // find tables with deleted=1
            $columns = $db->get_columns($table);
            // no deleted - won't delete
            if (empty($columns['deleted'])) {
                continue;
            }

            $custom_columns = array();
            if (array_search($table . '_cstm', $tables, true)) {
                $custom_columns = $db->get_columns($table . '_cstm');
                if (empty($custom_columns['id_c'])) {
                    $custom_columns = array();
                }
            }

            $qDel = "SELECT * FROM $table WHERE deleted = 1";
            $rDel = $db->query($qDel);

            // make a backup INSERT query if we are deleting.
            while ($aDel = $db->fetchByAssoc($rDel, false)) {
                // build column names

                $queryString[] = $db->insertParams($table, $columns, $aDel, null, false);

                if (!empty($custom_columns) && !empty($aDel['id'])) {
                    $qDelCstm = 'SELECT * FROM ' . $table . '_cstm WHERE id_c = ' . $db->quoted($aDel['id']);
                    $rDelCstm = $db->query($qDelCstm);

                    // make a backup INSERT query if we are deleting.
                    while ($aDelCstm = $db->fetchByAssoc($rDelCstm)) {
                        $queryString[] = $db->insertParams($table, $custom_columns, $aDelCstm, null, false);
                    } // end aDel while()

                    $db->query('DELETE FROM ' . $table . '_cstm WHERE id_c = ' . $db->quoted($aDel['id']));
                }
            } // end aDel while()
            // now do the actual delete
            $db->query('DELETE FROM ' . $table . ' WHERE deleted = 1');
        } // foreach() tables

        if (!file_exists($backupDir) || !file_exists($backupDir . '/' . $backupFile)) {
            // create directory if not existent
            mkdir_recursive($backupDir, false);
        }
        // write cache file

        write_array_to_file('pruneDatabase', $queryString, $backupDir . '/' . $backupFile);
        return true;
    }
    return false;
}


///**
// * Job 4
// */

//function securityAudit() {
//	// do something
//	return true;
//}

function trimTracker()
{
    global $sugar_config, $timedate;
    $GLOBALS['log']->info('----->Scheduler fired job of type trimTracker()');
    $db = DBManagerFactory::getInstance();

    $admin = BeanFactory::newBean('Administration');
    $admin->retrieveSettings('tracker');
    require('modules/Trackers/config.php');
    $trackerConfig = $tracker_config;

    require_once('include/utils/db_utils.php');
    $prune_interval = !empty($admin->settings['tracker_prune_interval']) ? $admin->settings['tracker_prune_interval'] : 30;
    foreach ($trackerConfig as $tableName => $tableConfig) {

        //Skip if table does not exist
        if (!$db->tableExists($tableName)) {
            continue;
        }

        $timeStamp = DBManagerFactory::getInstance()->convert("'" . $timedate->asDb($timedate->getNow()->get("-" . $prune_interval . " days")) . "'", "datetime");
        if ($tableName == 'tracker_sessions') {
            $query = "DELETE FROM $tableName WHERE date_end < $timeStamp";
        } else {
            $query = "DELETE FROM $tableName WHERE date_modified < $timeStamp";
        }

        $GLOBALS['log']->info("----->Scheduler is about to trim the $tableName table by running the query $query");
        $db->query($query);
    } //foreach
    return true;
}

/* Job 5
 *
 */
function pollMonitoredInboxesForBouncedCampaignEmails()
{
    $GLOBALS['log']->info('----->Scheduler job of type pollMonitoredInboxesForBouncedCampaignEmails()');
    global $dictionary;


    $ie = BeanFactory::newBean('InboundEmail');
    $r = $ie->db->query('SELECT id FROM inbound_email WHERE deleted=0 AND status=\'Active\' AND mailbox_type=\'bounce\'');

    while ($a = $ie->db->fetchByAssoc($r)) {
        $ieX = BeanFactory::newBean('InboundEmail');
        $ieX->retrieve($a['id']);
        $ieX->connectMailserver();
        $ieX->importMessages();
    }

    return true;
}


/**
 * Job 12
 */
function sendEmailReminders()
{
    $GLOBALS['log']->info('----->Scheduler fired job of type sendEmailReminders()');
    require_once("modules/Activities/EmailReminder.php");
    $reminder = new EmailReminder();
    return $reminder->process();
}

function removeDocumentsFromFS()
{
    $GLOBALS['log']->info('Starting removal of documents if they are not present in DB');

    /**
     * @var DBManager $db
     * @var SugarBean $bean
     */
    $db = DBManagerFactory::getInstance();

    // temp table to store id of files without memory leak
    $tableName = 'cron_remove_documents';

    $resource = $db->limitQuery("SELECT * FROM cron_remove_documents WHERE 1=1 ORDER BY date_modified ASC", 0, 100);
    $return = true;
    while ($row = $db->fetchByAssoc($resource)) {
        $bean = BeanFactory::getBean($row['module']);
        $bean->retrieve($row['bean_id'], true, false);
        if (empty($bean->id)) {
            $isSuccess = true;
            $bean->id = $row['bean_id'];
            $directory = $bean->deleteFileDirectory();
            if (!empty($directory) && is_dir('upload://deleted/' . $directory)) {
                if ($isSuccess = rmdir_recursive('upload://deleted/' . $directory)) {
                    $directory = explode('/', $directory);
                    while (!empty($directory)) {
                        $path = 'upload://deleted/' . implode('/', $directory);
                        if (is_dir($path)) {
                            $directoryIterator = new DirectoryIterator($path);
                            $empty = true;
                            foreach ($directoryIterator as $item) {
                                if ($item->getFilename() == '.' || $item->getFilename() == '..') {
                                    continue;
                                }
                                $empty = false;
                                break;
                            }
                            if ($empty) {
                                rmdir($path);
                            }
                        }
                        array_pop($directory);
                    }
                }
            }
            if ($isSuccess) {
                $db->query('DELETE FROM ' . $tableName . ' WHERE id=' . $db->quoted($row['id']));
            } else {
                $return = false;
            }
        } else {
            $db->query('UPDATE ' . $tableName . ' SET date_modified=' . $db->convert($db->quoted(TimeDate::getInstance()->nowDb()), 'datetime') . ' WHERE id=' . $db->quoted($row['id']));
        }
    }

    return $return;
}


/**
 * + * Job 16
 * + * this will trim all records in sugarfeeds table that are older than 30 days or specified interval
 * + */

function trimSugarFeeds()
{
    global $sugar_config, $timedate;
    $GLOBALS['log']->info('----->Scheduler fired job of type trimSugarFeeds()');
    $db = DBManagerFactory::getInstance();

    //get the pruning interval from globals if it's specified
    $prune_interval = !empty($GLOBALS['sugar_config']['sugarfeed_prune_interval']) && is_numeric($GLOBALS['sugar_config']['sugarfeed_prune_interval']) ? $GLOBALS['sugar_config']['sugarfeed_prune_interval'] : 30;


    //create and run the query to delete the records
    $timeStamp = $db->convert("'" . $timedate->asDb($timedate->getNow()->get("-" . $prune_interval . " days")) . "'", "datetime");
    $query = "DELETE FROM sugarfeed WHERE date_modified < $timeStamp";


    $GLOBALS['log']->info("----->Scheduler is about to trim the sugarfeed table by running the query $query");
    $db->query($query);

    return true;
}


/**
 * + * Job 17
 * + * this will sync the Google Calendars of users who are configured to do so
 * + */
function syncGoogleCalendar()
{
    global $sugar_config;
    require_once 'include/GoogleSync/GoogleSync.php';
    $googleSync = new GoogleSync($sugar_config);
    $googleSync->syncAllUsers();

    return true;
}

function cleanJobQueue($job)
{
    $td = TimeDate::getInstance();
    // soft delete all jobs that are older than cutoff
    $soft_cutoff = 7;
    if (isset($GLOBALS['sugar_config']['jobs']['soft_lifetime'])) {
        $soft_cutoff = $GLOBALS['sugar_config']['jobs']['soft_lifetime'];
    }
    $soft_cutoff_date = $job->db->quoted($td->getNow()->modify("- $soft_cutoff days")->asDb());
    $job->db->query("UPDATE {$job->table_name} SET deleted=1 WHERE status='done' AND date_modified < " . $job->db->convert($soft_cutoff_date, 'datetime'));
    // hard delete all jobs that are older than hard cutoff
    $hard_cutoff = 21;
    if (isset($GLOBALS['sugar_config']['jobs']['hard_lifetime'])) {
        $hard_cutoff = $GLOBALS['sugar_config']['jobs']['hard_lifetime'];
    }
    $hard_cutoff_date = $job->db->quoted($td->getNow()->modify("- $hard_cutoff days")->asDb());
    $job->db->query("DELETE FROM {$job->table_name} WHERE status='done' AND date_modified < " . $job->db->convert($hard_cutoff_date, 'datetime'));
    return true;
}

function pollMonitoredInboxesAOP()
{
    require_once 'modules/InboundEmail/AOPInboundEmail.php';
    $GLOBALS['log']->info('----->Scheduler fired job of type pollMonitoredInboxesAOP()');
    global $dictionary;
    global $app_strings;
    global $sugar_config;

    require_once('modules/Configurator/Configurator.php');
    $aopInboundEmail = new AOPInboundEmail();

    $sqlQueryResult = $aopInboundEmail->db->query(
        'SELECT id, name FROM inbound_email WHERE is_personal = 0 AND deleted=0 AND status=\'Active\'' .
        ' AND mailbox_type != \'bounce\''
    );

    $GLOBALS['log']->debug('Just got Result from get all Inbounds of Inbound Emails');

    while ($inboundEmailRow = $aopInboundEmail->db->fetchByAssoc($sqlQueryResult)) {
        $GLOBALS['log']->debug('In while loop of Inbound Emails');

        $aopInboundEmailX = new AOPInboundEmail();

        if (!$aopInboundEmailX->retrieve($inboundEmailRow['id']) || !$aopInboundEmailX->id) {
            throw new Exception('Error retrieving AOP Inbound Email: ' . $inboundEmailRow['id']);
        }

        $mailboxes = $aopInboundEmailX->mailboxarray;

        foreach ($mailboxes as $mbox) {
            $aopInboundEmailX->mailbox = $mbox;
            $newMsgs = array();
            $msgNoToUIDL = array();
            $connectToMailServer = false;

            if ($aopInboundEmailX->isPop3Protocol()) {
                $msgNoToUIDL = $aopInboundEmailX->getPop3NewMessagesToDownloadForCron();
                // get all the keys which are msgnos;
                $newMsgs = array_keys($msgNoToUIDL);
            }

            if ($aopInboundEmailX->connectMailserver() == 'true') {
                $connectToMailServer = true;
            } // if

            $GLOBALS['log']->debug('Trying to connect to mailserver for [ ' . $inboundEmailRow['name'] . ' ]');
            if ($connectToMailServer) {
                $GLOBALS['log']->debug('Connected to mailserver');

                if (!$aopInboundEmailX->isPop3Protocol()) {
                    $newMsgs = $aopInboundEmailX->getNewMessageIds();
                }

                if (is_array($newMsgs)) {
                    $current = 1;
                    $total = count($newMsgs);
                    require_once("include/SugarFolders/SugarFolders.php");
                    $sugarFolder = new SugarFolder();
                    $groupFolderId = $aopInboundEmailX->groupfolder_id;
                    $isGroupFolderExists = false;
                    $users = array();
                    if ($groupFolderId != null && $groupFolderId != "") {
                        // FIX #6994 - Unable to retrieve Sugar Folder due to incorrect groupFolderId
                        $sugarFolder->retrieve($groupFolderId);
                        if (empty($sugarFolder->id)) {
                            $sugarFolder->retrieve($aopInboundEmailX->id);
                        }
                        if (!empty($sugarFolder->id)) {
                            $isGroupFolderExists = true;
                        }
                    } // if
                    $messagesToDelete = array();
                    if ($aopInboundEmailX->isMailBoxTypeCreateCase()) {
                        require_once 'modules/AOP_Case_Updates/AOPAssignManager.php';
                        $assignManager = new AOPAssignManager($aopInboundEmailX);
                    }
                    foreach ($newMsgs as $k => $msgNo) {
                        $uid = $msgNo;
                        if ($aopInboundEmailX->isPop3Protocol()) {
                            $uid = $msgNoToUIDL[$msgNo];
                        } else {
                            $uid = $aopInboundEmailX->getImap()->getUid($msgNo);
                        } // else
                        if ($isGroupFolderExists) {
                            $emailId = $aopInboundEmailX->returnImportedEmail($msgNo, $uid, false, true, $isGroupFolderExists);

                            if (!empty($emailId)) {
                                // add to folder

                                $sugarFolder->addBean($aopInboundEmailX);
                                if ($aopInboundEmailX->isPop3Protocol()) {
                                    $messagesToDelete[] = $msgNo;
                                } else {
                                    $messagesToDelete[] = $uid;
                                }
                                if ($aopInboundEmailX->isMailBoxTypeCreateCase()) {
                                    $userId = $assignManager->getNextAssignedUser();
                                    $GLOBALS['log']->debug('userId [ ' . $userId . ' ]');
                                    $validatior = new SuiteValidator();
                                    if ((!isset($aopInboundEmailX->email) || !$aopInboundEmailX->email ||
                                        !isset($aopInboundEmailX->email->id) || !$aopInboundEmailX->email->id) &&
                                        $validatior->isValidId($emailId)
                                    ) {
                                        $aopInboundEmailX->email = BeanFactory::newBean('Emails');
                                        if (!$aopInboundEmailX->email->retrieve($emailId)) {
                                            throw new Exception('Email retrieving error to handle case create, email id was: ' . $emailId);
                                        }
                                    }
                                    if (empty($aopInboundEmailX->email)) {
                                        throw new Exception('Invalid type for email id ' . $emailId);
                                    }
                                    $aopInboundEmailX->handleCreateCase($aopInboundEmailX->email, $userId);
                                } // if
                            } // if
                        } else {
                            if ($aopInboundEmailX->isAutoImport()) {
                                $aopInboundEmailX->returnImportedEmail($msgNo, $uid);
                            } else {
                                /*If the group folder doesn't exist then download only those messages
                                 which has caseid in message*/

                                $aopInboundEmailX->getMessagesInEmailCache($msgNo, $uid);
                                $email = BeanFactory::newBean('Emails');
                                $header = $aopInboundEmailX->getImap()->getHeaderInfo($msgNo);
                                $email->name = $aopInboundEmailX->handleMimeHeaderDecode($header->subject);
                                $email->from_addr = $aopInboundEmailX->convertImapToSugarEmailAddress($header->from);
                                isValidEmailAddress($email->from_addr);
                                $email->reply_to_email = $aopInboundEmailX->convertImapToSugarEmailAddress($header->reply_to);
                                if (!empty($email->reply_to_email)) {
                                    $contactAddr = $email->reply_to_email;
                                    isValidEmailAddress($contactAddr);
                                } else {
                                    $contactAddr = $email->from_addr;
                                    isValidEmailAddress($contactAddr);
                                }
                                $mailBoxType = $aopInboundEmailX->mailbox_type;
                                $aopInboundEmailX->handleAutoresponse($email, $contactAddr);
                            } // else
                        } // else
                        $GLOBALS['log']->debug('***** On message [ ' . $current . ' of ' . $total . ' ] *****');
                        $current++;
                    } // foreach
                    // update Inbound Account with last robin
                } // if

                if (!empty($isGroupFolderExists)) {
                    $leaveMessagesOnMailServer = $aopInboundEmailX->get_stored_options("leaveMessagesOnMailServer", 0);
                    if (!$leaveMessagesOnMailServer) {
                        if ($aopInboundEmailX->isPop3Protocol()) {
                            $aopInboundEmailX->deleteMessageOnMailServerForPop3(implode(",", $messagesToDelete));
                        } else {
                            $aopInboundEmailX->deleteMessageOnMailServer(implode($app_strings['LBL_EMAIL_DELIMITER'], $messagesToDelete));
                        }
                    }
                }
            } else {
                $GLOBALS['log']->fatal("SCHEDULERS: could not get an IMAP connection resource for ID [ {$inboundEmailRow['id']} ]. Skipping mailbox [ {$inboundEmailRow['name']} ].");
                // cn: bug 9171 - continue while
            } // else
        } // foreach
        $aopInboundEmailX->getImap()->expunge();
        $aopInboundEmailX->getImap()->close(CL_EXPUNGE);
    } // while
    return true;
}

/**
 * Scheduled job function to index any unindexed beans.
 * @deprecated since v7.12.0
 * @return bool
 */
function aodIndexUnindexed()
{
    $total = 1;
    $sanityCount = 0;
    while ($total > 0) {
        $total = performLuceneIndexing();
        $sanityCount++;
        if ($sanityCount > 100) {
            return true;
        }
    }
    return true;
}

/**
 * @deprecated since v7.12.0
 * @return bool
 */
function aodOptimiseIndex()
{
    $index = BeanFactory::getBean("AOD_Index")->getIndex();
    $index->optimise();
    return true;
}

/**
 * @deprecated since v7.12.0
 * @return int|void
 */
function performLuceneIndexing()
{
    global $sugar_config;
    $db = DBManagerFactory::getInstance();

    if (empty($sugar_config['aod']['enable_aod'])) {
        return;
    }
    $index = BeanFactory::getBean("AOD_Index")->getIndex();

    $beanList = $index->getIndexableModules();
    $total = 0;
    foreach ($beanList as $beanModule => $beanName) {
        $bean = BeanFactory::getBean($beanModule);
        if (!$bean || !method_exists($bean, "getTableName") || !$bean->getTableName()) {
            continue;
        }
        $query = "SELECT b.id FROM ".$bean->getTableName()." b LEFT JOIN aod_indexevent ie ON (ie.record_id = b.id AND ie.record_module = '".$beanModule."') WHERE b.deleted = 0 AND (ie.id IS NULL OR ie.date_modified < b.date_modified) ORDER BY b.date_modified ASC";
        $res = $db->limitQuery($query, 0, 500);
        $c = 0;
        while ($row = $db->fetchByAssoc($res)) {
            $suc = $index->index($beanModule, $row['id']);
            if ($suc) {
                $c++;
                $total++;
            }
        }
        if ($c) {
            $index->commit();
            $index->optimise();
        }
    }
    $index->optimise();
    return $total;
}

function aorRunScheduledReports()
{
    require_once 'include/SugarQueue/SugarJobQueue.php';
    $db = DBManagerFactory::getInstance();
    $date = new DateTime();//Ensure we check all schedules at the same instant
    foreach (BeanFactory::getBean('AOR_Scheduled_Reports')->get_full_list() as $scheduledReport) {
        if ($scheduledReport->status != 'active') {
            continue;
        }
        try {
            $shouldRun = $scheduledReport->shouldRun($date);
        } catch (Exception $ex) {
            LoggerManager::getLogger()->warn('aorRunScheduledReports: id: ' . $scheduledReport->id . ' got exception. code: ' . $ex->getCode() . ', message: ' . $ex->getMessage());
            $shouldRun = false;
        }
        if ($shouldRun) {
            if (empty($scheduledReport->aor_report_id)) {
                continue;
            }
            $queued = $db->fetchOne("SELECT count(*) cnt FROM job_queue WHERE data=".$db->quoted($scheduledReport->id)." and deleted=0 and status = 'running' and execute_time >= " . $db->quoted(date("Y-m-d H:i:s", strtotime("-2 hours"))));
            if(!empty($queued) && $queued['cnt'] > 0) {
                LoggerManager::getLogger()->warn('aorRunScheduledReports: id: ' . $scheduledReport->id . ' is already running. Postpone creating new job.');
                continue;
            }
            $job = BeanFactory::newBean('SchedulersJobs');
            $job->name = "Scheduled report - {$scheduledReport->name} on {$date->format('c')}";
            $job->data = $scheduledReport->id;
            $job->target = "class::AORScheduledReportJob";
            $job->assigned_user_id = 1;
            $jq = new SugarJobQueue();
            $jq->submitJob($job);
        }
    }
    return true;
}

function processAOW_Workflow()
{
    require_once('modules/AOW_WorkFlow/AOW_WorkFlow.php');
    $workflow = BeanFactory::newBean('AOW_WorkFlow');
    return $workflow->run_flows();
}

#[\AllowDynamicProperties]
class AORScheduledReportJob implements RunnableSchedulerJob
{
    public function setJob(SchedulersJob $job)
    {
        $this->job = $job;
    }

    public function run($data)
    {
        global $timedate;

        $bean = BeanFactory::getBean('AOR_Scheduled_Reports', $data);
        $report = $bean->get_linked_beans('aor_report', 'AOR_Reports');
        if ($report) {
            $report = $report[0];
        } else {
            return false;
        }
        $html = "<h1>{$report->name}</h1>" . $report->build_group_report();
        $html .= <<<EOF
        <style>
        h1{
            color: black;
        }
        .list
        {
            font-family: "Lucida Sans Unicode", "Lucida Grande", Sans-Serif;font-size: 12px;
            background: #fff;margin: 45px;width: 480px;border-collapse: collapse;text-align: left;
        }
        .list th
        {
            font-size: 14px;
            font-weight: normal;
            color: black;
            padding: 10px 8px;
            border-bottom: 2px solid black;
        }
        .list td
        {
            padding: 9px 8px 0px 8px;
        }
        </style>
EOF;
        $emailObj = BeanFactory::newBean('Emails');
        $defaults = $emailObj->getSystemDefaultEmail();
        $mail = new SugarPHPMailer();

        $mail->setMailerForSystem();
        $mail->IsHTML(true);
        $mail->From = $defaults['email'];
        isValidEmailAddress($mail->From);
        $mail->FromName = $defaults['name'];
        $mail->Subject = from_html($bean->name);
        $mail->Body = $html;
        $mail->prepForOutbound();
        $success = true;
        $emails = $bean->get_email_recipients();
        foreach ($emails as $email_address) {
            $mail->ClearAddresses();
            $mail->AddAddress($email_address);
            $success = $mail->Send() && $success;
        }
        $bean->last_run = $timedate->getNow()->asDb(false);
        $bean->save();
        return true;
    }
}

function runElasticSearchIndexerScheduler($job, $data = '{}')
{
    return \SuiteCRM\Search\ElasticSearch\ElasticSearchIndexer::schedulerJob(json_decode(html_entity_decode($data), true));
}

if (file_exists('custom/modules/Schedulers/_AddJobsHere.php')) {
    require('custom/modules/Schedulers/_AddJobsHere.php');
}

if (file_exists('custom/modules/Schedulers/Ext/ScheduledTasks/scheduledtasks.ext.php')) {
    require('custom/modules/Schedulers/Ext/ScheduledTasks/scheduledtasks.ext.php');
}
