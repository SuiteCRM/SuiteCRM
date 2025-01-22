<?php

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

require_once __DIR__ . '/../../../modules/SchedulersJobs/SchedulersJob.php';

abstract class BatchJob implements RunnableSchedulerJob
{
    /**
     * @var $job SchedulersJob
     */
    protected $job;

    /**
     * @var $batchSize int
     */
    protected $batchSize = 100;


    /**
     * @inheritDoc
     */
    public function setJob(SchedulersJob $job)
    {
        $this->job = $job;
    }

    /**
     * @inheritDoc
     */
    public function run($data)
    {
        $data = $data ?? [];

        if (is_string($data)) {
            $data = json_decode(html_entity_decode($data), true, 512, JSON_THROW_ON_ERROR);
        }

        if (!$this->shouldRun($data)) {
            return false;
        }

        $this->init();

        if (empty($data['tracking'])) {
            $data['tracking'] = $this->setupTracking();
        }

        $batchSize = $this->getBatchSize($data);
        [$type, $recordIds, $data] = $this->getNextBatch($data, $batchSize);

        if (empty($recordIds)) {
            $this->markComplete();

            return true;
        }

        $data = $this->processBatch($type, $data, $recordIds);

        $this->updateTrackingTable($type, $data, $recordIds);
        $this->updateJobStatus($data);

        return true;
    }

    /**
     * Get job key
     * @return string
     */
    abstract public function getJobKey(): string;

    /**
     * Get job key
     * @return array
     */
    abstract protected function setupTracking(): array;

    /**
     * Should run
     * @return bool
     */
    abstract protected function shouldRun(): bool;

    /**
     * Do initial setup
     */
    abstract protected function init(): void;

    /**
     * @param string $type
     * @param array $data
     * @return array
     */
    abstract protected function getAllIdsInGroup(string $type, array $data): array;

    /**
     * @param string $type
     * @param array $data
     * @param array $ids
     * @return array
     */
    abstract protected function processBatch(string $type, array $data, array $ids): array;

    /**
     * Get tracking table name
     * @param string $type
     * @return string
     */
    protected function getTrackingTableName(string $type): string
    {
        return 'tmp_' . strtolower(str_replace('-', '_', $this->getJobKey())) . '_' . strtolower($type);
    }

    /**
     * Create and initialize tracking table with all ids to process
     * @param string $table
     * @param array $ids
     */
    protected function initTrackingTable(string $table, array $ids): void
    {
        global $db;

        $fields = [
            'id' => [
                'name' => 'id',
                'type' => 'varchar',
            ],
            'status' => [
                'name' => 'status',
                'type' => 'varchar',
                'isnull' => true
            ],
        ];

        $db->createTableParams($table, $fields, []);

        $chunkSize = 2500;
        $recordIds = array_chunk($ids, $chunkSize, true);

        $baseQuery = "INSERT INTO " . $table . " (id) VALUES ";

        foreach ($recordIds as $chunk) {

            $query = $baseQuery . '("' . implode('") , ("', $chunk) . '")';

            $db->query($query);
        }
    }

    /**
     * Delete ids table
     * @param string $table
     */
    protected function deleteTrackingTable(string $table): void
    {
        global $db;

        $db->dropTableName($table);
    }

    /**
     * Update tracking table with ids that have been processed
     * @param string $type
     * @param array $data
     * @param array $recordIds
     */
    protected function updateTrackingTable(string $type, array $data, array $recordIds): void
    {
        global $db;

        $trackingTable = $data['tracking'][$type]['table'] ?? '';

        $sql = "UPDATE $trackingTable SET status = 'processed' WHERE id IN ('" . implode("' , '", $recordIds) . "') ";

        $db->query($sql);
    }

    /**
     * Get Batch of ids to process
     * @param string $trackingTable
     * @param array $data
     * @return array
     */
    protected function getIdsBatch(string $trackingTable, array $data): array
    {
        global $db;

        $batchSize = $this->getBatchSize($data);

        $query = "SELECT id FROM $trackingTable WHERE status is null";
        $start = 0;
        $count = $batchSize;

        $result = $db->limitQuery($query, $start, $count);

        $ids = [];
        $row = $db->fetchByAssoc($result, false);

        while (!empty($row)) {
            if (!empty($row['id'])) {
                $ids[] = $row['id'];
            }

            $row = $db->fetchByAssoc($result);

        }

        return $ids;
    }

    /**
     * @param array $data
     * @param string $group
     * @return array
     */
    protected function getGroupBatchInfo(array $data, string $group): array
    {
        return $data['tracking'][$group] ?? [];
    }

    /**
     * @param array $data
     * @return int
     */
    protected function getBatchSize(array $data): int
    {
        $batchSize = $this->batchSize;
        if (!empty($data['batchSize']) && is_numeric($data['batchSize'])) {
            $batchSize = (int)$data['batchSize'];
        }

        return $batchSize;
    }

    /**
     * @param $data
     * @param int $batchSize
     * @return array
     */
    protected function getNextBatch($data, int $batchSize): array
    {
        $groups = array_keys($data['tracking']);
        $type = '';
        $recordIds = [];

        foreach ($groups as $group) {
            $groupStatus = $data['tracking'][$group]['status'] ?? 'queued';

            if ($groupStatus === 'done' || $groupStatus === 'no-records') {
                continue;
            }

            $currentDataSet = $this->getGroupBatchInfo($data, $group);

            $trackingTable = $currentDataSet['table'] ?? '';
            $updatedCount = $this->getUpdatedCount($currentDataSet);

            if ($trackingTable === '') {
                $ids = $this->getAllIdsInGroup($group, $data);

                if (empty($ids)) {
                    $data['tracking'][$group]['status'] = 'no-records';
                    continue;
                }

                $trackingTable = $this->getTrackingTableName($group);
                $this->initTrackingTable($trackingTable, $ids);

                $data['tracking'][$group]['total'] = count($ids);
                $data['tracking'][$group]['table'] = $trackingTable;

                $idChunks = array_chunk($ids, $batchSize);
                $recordIds = $idChunks[0];
            }

            if (empty($recordIds)) {
                $recordIds = $this->getIdsBatch($trackingTable, $data);
            }

            if (!empty($recordIds)) {
                $data['tracking'][$group]['status'] = 'in-progress';
                $data['tracking'][$group]['updated_count'] = $updatedCount + count($recordIds);
                $type = $group;
                break;
            }

            // no more ids for current group, delete table
            if (!empty($data['tracking'][$group]['table']) && empty($data['keepTracking'])) {
                $this->deleteTrackingTable($data['tracking'][$group]['table']);
            }

            $data['tracking'][$group]['status'] = 'done';
        }

        return [$type, $recordIds, $data];
    }

    /**
     * Get updated count
     * @param $currentDataSet
     * @return int
     */
    protected function getUpdatedCount($currentDataSet): int
    {
        $updatedCount = 0;
        if (!empty($currentDataSet['updated_count']) && is_numeric($currentDataSet['updated_count'])) {
            $updatedCount = (int)$currentDataSet['updated_count'];
        }

        return $updatedCount;
    }

    /**
     * Update job status and data
     * @param $data
     */
    protected function updateJobStatus($data): void
    {
        $this->job->data = json_encode($data, JSON_THROW_ON_ERROR);
        $this->job->status = 'queued';
        $this->job->postponeJob(null, 5);
    }

    /**
     * Mark complete
     */
    protected function markComplete(): void
    {
        $this->job->resolveJob(SchedulersJob::JOB_SUCCESS);
    }
}
