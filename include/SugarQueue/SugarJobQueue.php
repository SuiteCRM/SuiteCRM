<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


require_once 'modules/SchedulersJobs/SchedulersJob.php';

/**
 * Job queue driver
 * @api
 */
class SugarJobQueue
{
    /**
     * Max number of failures for job
     * @var int
     */
    public $jobTries = 5;

    /**
     * Job running timeout - longer than that, job is failed by force
     * @var int
     */
    public $timeout = 86400; // 24 hours

    /**
     * Table in the DB that stores jobs
     * @var string
     */
    protected $job_queue_table;

    /**
     * Success History Lifetime
     * Defines the time in days for successful cron jobs to remain before deletion
     * @var int
     */

    public $success_lifetime = 30; // 30 days
    /**
     * Failure History Lifetime
     * Defines the time in days for failed cron jobs to remain before deletion
     * @var int
     */
    public $failure_lifetime = 180; // 180 days

    /**
     * DB connection
     * @var DBManager
     */
    public $db;

    public function __construct()
    {
        $this->db = DBManagerFactory::getInstance();
        $job = BeanFactory::newBean('SchedulersJobs');
        $this->job_queue_table = $job->table_name;
        if (!empty($GLOBALS['sugar_config']['jobs']['max_retries'])) {
            $this->jobTries = $GLOBALS['sugar_config']['jobs']['max_retries'];
        }
        if (!empty($GLOBALS['sugar_config']['jobs']['timeout'])) {
            $this->timeout = $GLOBALS['sugar_config']['jobs']['timeout'];
        }
        if (!empty($GLOBALS['sugar_config']['jobs']['failure_lifetime'])) {
            $this->failure_lifetime = $GLOBALS['sugar_config']['jobs']['failure_lifetime'];
        }
        if (!empty($GLOBALS['sugar_config']['jobs']['success_lifetime'])) {
            $this->success_lifetime = $GLOBALS['sugar_config']['jobs']['success_lifetime'];
        }
    }

    /**
     * Submit a new job to the queue
     * @param SugarJob $job
     * @param User $user User to run the job under
     */
    public function submitJob($job)
    {
        $job->id = create_guid();
        $job->new_with_id = true;
        $job->status = SchedulersJob::JOB_STATUS_QUEUED;
        $job->resolution = SchedulersJob::JOB_PENDING;
        if (empty($job->execute_time)) {
            $job->execute_time = $GLOBALS['timedate']->nowDb();
        }
        $job->save();

        return $job->id;
    }

    /**
     * Get Job object by ID
     * @param string $jobId
     * @return SugarJob
     */
    protected function getJob($jobId)
    {
        $job = BeanFactory::newBean('SchedulersJobs');
        $job->retrieve($jobId);
        if (empty($job->id)) {
            $GLOBALS['log']->info("Job $jobId not found!");
            return null;
        }
        return $job;
    }

    /**
     * Resolve job as success or failure
     * @param string $jobId
     * @param string $resolution One of JOB_ constants that define job status
     * @param string $message
     * @return bool
     */
    public function resolveJob($jobId, $resolution, $message = null)
    {
        $job = $this->getJob($jobId);
        if (empty($job)) {
            return false;
        }
        return $job->resolveJob($resolution, $message);
    }

    /**
     * Rerun this job again
     * @param string $jobId
     * @param string $message
     * @param string $delay how long to delay (default is job's delay)
     * @return bool
     */
    public function postponeJob($jobId, $message = null, $delay = null)
    {
        $job = $this->getJob($jobId);
        if (empty($job)) {
            return false;
        }
        return $job->postponeJob($message, $delay);
    }

    /**
     * Delete a job
     * @param string $jobId
     */
    public function deleteJob($jobId)
    {
        $job = BeanFactory::newBean('SchedulersJobs');
        if (empty($job)) {
            return false;
        }
        return $job->mark_deleted($jobId);
    }

    /**
     * Cleanup old, failed or long running jobs
     * Forcefully remove jobs that are marked as running when they exceed the timeout value
     * @return bool true if no failed job discovered, false if some job were failed
     */
    public function cleanup()
    {
        $ret = true;
        $date = $this->db->convert($GLOBALS['timedate']->getNow()->modify("-{$this->timeout} seconds")->asDb(), 'datetime');
        $sql = "SELECT id FROM {$this->job_queue_table} WHERE status='".SchedulersJob::JOB_STATUS_RUNNING."' AND date_modified <= '{$date}'";
        $res = $this->db->query($sql);
        while ($row = $this->db->fetchByAssoc($res)) {
            $this->resolveJob($row["id"], SchedulersJob::JOB_FAILURE, translate('ERR_TIMEOUT', 'SchedulersJobs'));
            $ret = false;
        }
        return $ret;
    }

    /**
     * Marks jobs for deletion that have exceeded their history lifetime
     * Uses different values for successful and failed jobs
     */
    public function clearHistoricJobs()
    {
        // Process successful jobs
        $date = $this->db->convert($GLOBALS['timedate']->getNow()->modify("-{$this->success_lifetime} days")->asDb(), 'datetime');
        $sql = "SELECT id FROM {$this->job_queue_table} WHERE status='".SchedulersJob::JOB_STATUS_DONE."' AND resolution='".SchedulersJob::JOB_SUCCESS."' AND date_modified <= '{$date}'";
        $res = $this->db->query($sql);
        while ($row = $this->db->fetchByAssoc($res)) {
            $this->deleteJob($row["id"]);
        }

        // Process failed jobs
        $date = $this->db->convert($GLOBALS['timedate']->getNow()->modify("-{$this->failure_lifetime} days")->asDb(), 'datetime');
        $sql = "SELECT id FROM {$this->job_queue_table} WHERE status='".SchedulersJob::JOB_STATUS_DONE."' AND resolution!='".SchedulersJob::JOB_SUCCESS."' AND date_modified <= '{$date}'";
        $res = $this->db->query($sql);
        while ($row = $this->db->fetchByAssoc($res)) {
            $this->deleteJob($row["id"]);
        }
    }

    /**
     * Nuke all jobs from the queue
     */
    public function cleanQueue()
    {
        $this->db->query("DELETE FROM {$this->job_queue_table}");
    }

    /**
     * Fetch the next job in the queue and mark it running
     * @param string $clientID ID of the client requesting the job
     * @return SugarJob
     */
    public function nextJob($clientID)
    {
        $now = $this->db->now();
        $queued = SchedulersJob::JOB_STATUS_QUEUED;
        $try = $this->jobTries;
        while ($try--) {
            // TODO: tranaction start?
            $id = $this->db->getOne("SELECT id FROM {$this->job_queue_table} WHERE execute_time <= $now AND status = '$queued' ORDER BY date_entered ASC");
            if (empty($id)) {
                return null;
            }
            $job = BeanFactory::newBean('SchedulersJobs');
            $job->retrieve($id);
            if (empty($job->id)) {
                return null;
            }
            $job->status = SchedulersJob::JOB_STATUS_RUNNING;
            $job->client = $clientID;
            $client = $this->db->quote($clientID);
            // using direct query here to be able to fetch affected count
            // if count is 0 this means somebody changed the job status and we have to try again
            $res = $this->db->query("UPDATE {$this->job_queue_table} SET status='{$job->status}', date_modified=$now, client='$client' WHERE id='{$job->id}' AND status='$queued'");
            if ($this->db->getAffectedRowCount($res) == 0) {
                // somebody stole our job, try again
                continue;
            } else {
                // to update dates & possible hooks
                $job->save();
                break;
            }
            // TODO: commit/check?
        }
        return $job;
    }

    /**
     * Run schedulers to instantiate scheduled jobs
     */
    public function runSchedulers()
    {
        $sched = BeanFactory::newBean('Schedulers');
        $sched->checkPendingJobs($this);
    }
}
