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
require_once __DIR__ . '/../Batch/BatchJob.php';


class NormalizeRecords extends BatchJob
{
    public const UTF_REPAIR_FROM = '2021-04-27 00:00:01';
    public const SYSTEM_SETTING_KEY = 'utf_normalization_executed';
    public const SYSTEM_SETTING_MODE_KEY = 'utf_normalization_execution_mode';
    public const SYSTEM_SETTING_CATEGORY = 'system';
    public const REPAIR_STATUS_IN_PROGRESS = 'in_progress';
    public const REPAIR_STATUS_REPAIRED = 'repaired';
    public const REPAIR_STATUS_FAILURE = 'failed';
    public const EXECUTION_MODE_SYNC = 'sync';
    public const EXECUTION_MODE_ASYNC = 'async';

    /**
     * Run record normalization process
     * @param array $data $repairFrom
     * @param bool $echo
     * @return array
     */
    public function runAll($data, $echo = false): array
    {
        $this->init();

        $repairStatus = self::getRepairStatus();

        if ($repairStatus === self::REPAIR_STATUS_REPAIRED) {
            return [
                'success' => true,
                'messages' => [
                    'Repair already executed'
                ]
            ];
        }

        if ($repairStatus === self::REPAIR_STATUS_IN_PROGRESS) {
            return [
                'success' => true,
                'messages' => [
                    'Repair already in progress'
                ]
            ];
        }

        self::setRepairStatus(self::REPAIR_STATUS_IN_PROGRESS);
        self::setExecutionMode(self::EXECUTION_MODE_SYNC);

        $tracking = $this->setupTracking();
        $moduleList = array_keys($tracking);

        $chunkSize = 2500;
        $time_start = microtime(true);

        $repairFrom = $data['repair_from'] ?? '';
        $messages = [];
        foreach ($moduleList as $type) {

            $this->debugLog("Starting repair module $type");
            $this->echoOutput($echo, "Starting repair module $type");

            $ids = $this->getAllIdsInRange($type, $repairFrom);

            if (empty($ids)) {
                $this->debugLog("No ids found for module $type");
                $this->echoOutput($echo, "No ids found for module $type");
                $this->echoOutput($echo, "$type 0 records normalized");
                $messages[] = "$type 0 records normalized";
                $this->debugLog("$type 0 records normalized");
                continue;
            }

            $idChunks = array_chunk($ids, $chunkSize, true);

            foreach ($idChunks as $key => $idChunk) {
                $this->debugLog("Repair $type - chunk $key");
                $this->echoOutput($echo, "Repair $type - chunk $key");
                $result = $this->repairChunk($type, $idChunk, $messages);
                $messages = $result['messages'] ?? [];
            }

            $this->debugLog("Finished repair for module $type");
            $this->echoOutput($echo, "Finished repair for module $type");

        }

        self::setRepairStatus(self::REPAIR_STATUS_REPAIRED);

        $mem_peak = round(memory_get_peak_usage() / 1024 / 1024);
        $this->debugLog("$mem_peak MB memory usage peak");
        $messages[] = "Memory usage peak: $mem_peak MB";

        $time_end = microtime(true);
        $execution_time = ($time_end - $time_start) / 60;
        $this->debugLog("Total Execution Time: $execution_time Mins");
        $messages[] = "Total Execution Time: $execution_time Mins";

        return [
            'success' => true,
            'messages' => $messages
        ];
    }

    /**
     * @inheritDoc
     */
    public function getJobKey(): string
    {
        return 'utf-normalize';
    }

    /**
     * @inheritDoc
     */
    protected function init(): void
    {
        global $current_language, $sugar_config, $current_user, $db;

        if (empty($current_language)) {
            $current_language = $sugar_config['default_language'];
        }

        $app_list_strings = return_app_list_strings_language($current_language);
        $app_strings = return_application_language($current_language);

        $current_user = BeanFactory::newBean('Users');
        $current_user->getSystemUser();

        $db::setQueryLimit(0);
    }

    /**
     * @inheritDoc
     */
    protected function shouldRun(): bool
    {
        $repairStatus = self::getRepairStatus() ?? '';
        if ($repairStatus === self::REPAIR_STATUS_REPAIRED) {
            $this->job->resolveJob(SchedulersJob::JOB_FAILURE, 'Normalization can only be executed once');

            return false;
        }

        self::setRepairStatus(self::REPAIR_STATUS_IN_PROGRESS);
        self::setExecutionMode(self::EXECUTION_MODE_ASYNC);

        return true;
    }

    /**
     * Get repair status
     * @return string
     */
    public static function getRepairStatus(): string
    {
        return self::getConfigEntry(self::SYSTEM_SETTING_KEY);
    }


    /**
     * Set repair status
     * @param string $status
     */
    public static function setRepairStatus(string $status): void
    {
        self::setConfigEntry(self::SYSTEM_SETTING_KEY, $status);
    }

    /**
     * Get execution mode
     * @return string
     */
    public static function getExecutionMode(): string
    {
        return self::getConfigEntry(self::SYSTEM_SETTING_MODE_KEY);
    }

    /**
     * Set repair status
     * @param string $mode
     */
    public static function setExecutionMode(string $mode): void
    {
        self::setConfigEntry(self::SYSTEM_SETTING_MODE_KEY, $mode);
    }

    /**
     * Check if repair from is valid
     * @param string $repairFrom
     * @return bool
     */
    public static function isValidRepairFrom(string $repairFrom): bool
    {
        global $timedate;

        if (!preg_match("/^\d\d\d\d-\d\d-\d\d$/", $repairFrom)) {
            return false;
        }

        $repairFromDateTime = $timedate->fromDbDate($repairFrom);

        if (empty($repairFromDateTime)) {
            return false;
        }

        return true;
    }

    /**
     * Get entry
     * @param string $key
     * @return mixed|string
     */
    protected static function getConfigEntry(string $key)
    {
        /** @var Administration $administration */
        $administration = BeanFactory::getBean('Administration');
        $result = $administration->retrieveSettings('system');

        if ($result === null) {
            throw new RuntimeException('Problem while fetching admin settings');
        }

        $settingKey = self::SYSTEM_SETTING_CATEGORY . '_' . $key;

        if (empty($administration->settings[$settingKey])) {
            return '';
        }

        return $administration->settings[$settingKey] ?? '';
    }

    /**
     * Set config entry
     * @param string $key
     * @param string $value
     */
    protected static function setConfigEntry(string $key, string $value): void
    {
        /** @var Administration $administration */
        $administration = BeanFactory::getBean('Administration');
        $administration->saveSetting(self::SYSTEM_SETTING_CATEGORY, $key, $value);
    }

    /**
     * @inheritDoc
     */
    protected function setupTracking(): array
    {
        // Populate the $moduleList variable to target only specific modules
        $moduleList = $this->getModulesToNormalize();

        $tracking = [];

        foreach ($moduleList as $module) {

            if (empty($GLOBALS["beanList"][$module])) {
                continue;
            }

            if ($module === 'Home') {
                continue;
            }

            $tracking[$module] = [
                'table' => '',
                'total' => '',
                'status' => 'queued',
                'updated_count' => ''
            ];
        }

        return $tracking;
    }

    /**
     * @inheritDoc
     */
    protected function getAllIdsInGroup(string $type, array $data): array
    {
        $repairFrom = $data['repair_from'] ?? '';

        return $this->getAllIdsInRange($type, $repairFrom);
    }

    /**
     * @inheritDoc
     */
    protected function processBatch(string $type, array $data, array $ids): array
    {
        $result = $this->repairChunk($type, $ids);

        $currentCount = $data['tracking'][$type]['normalize_count'] ?? 0;
        $count = $result['normalize_count'] ?? 0;
        $data['tracking'][$type]['normalize_count'] = $currentCount + $count;

        return $data;
    }

    /**
     * @inheritDoc
     */
    protected function markComplete(): void
    {
        self::setRepairStatus(self::REPAIR_STATUS_REPAIRED);
        parent::markComplete();
    }

    /**
     * Get all potentially affected records as an array of Ids
     *
     * @param $type
     * @param string $repairFrom
     * @return array
     */
    protected function getAllIdsInRange($type, string $repairFrom): array
    {
        global $db;
        $bean = BeanFactory::getBean($type);
        if ($bean === false) {
            return [];
        }

        $tableName = $bean->table_name;

        if (empty($repairFrom)) {
            $repairFrom = self::UTF_REPAIR_FROM;
        }

        $sql = "SELECT id FROM $tableName WHERE (date_entered >= '$repairFrom') OR (date_modified >= '$repairFrom') AND ( deleted != 1 );";

        $result = $db->query($sql);

        $ids = [];
        while ($row = $db->fetchByAssoc($result)) {
            $ids[] = $row['id'];
        }

        return $ids;
    }

    /**
     * @param string $type
     * @param array $ids
     * @param array $messages
     * @return array
     */
    protected function repairChunk(string $type, array $ids, array $messages = []): array
    {
        $result = [
            'normalize_count' => 0
        ];
        $fieldList = $this->getRepairableFieldNames($type);
        $this->debugLog("$type " . count($ids) . " records checked");
        $this->debugLog("Processing $type");

        $i = 0;

        $records = $this->getRecordChunk($ids, $type);
        foreach ($records as $row) {

            $normalized = $this->repairStringValues($type, $row, $fieldList);
            if (empty($normalized)) {
                continue;
            }

            $bean = BeanFactory::getBean($type);
            $bean->populateFromRow($row);
            $bean->update_date_modified = false;
            $bean->update_modified_by = false;
            $bean->processed = true;
            $bean->notify_inworkflow = false;
            $bean->saveFields($normalized);
            $this->debugLog("$type - " . $bean->id . " normalized");
            ++$i;
            if ($i % 100 === 0) {
                $this->debugLog("$i records have been saved");
            }
        }
        $messages[] = "$type " . $i . " records normalized";
        $result['normalize_count'] = $i;
        $this->debugLog("$type " . $i . " records normalized");

        $result['messages'] = $messages;

        return $result;
    }

    /**
     * Check and Normalize values from database row
     * which exist in the $fieldList array
     *
     * @param string $type
     * @param array $row
     * @param array $fieldList
     * @return array
     */
    protected function repairStringValues(string $type, &$row, array $fieldList): array
    {
        $fieldsToExclude = [
            'Users' => [
                'user_hash' => true
            ],
        ];

        $normalized = [];
        foreach ($fieldList as $fieldName) {

            $exclude = $fieldsToExclude[$type][$fieldName] ?? false;

            if ($exclude === true) {
                $this->debugLog("Excluded $fieldName");
                continue;
            }

            if (!empty($row[$fieldName])) {

                // Check if normalization is required
                if (normalizer_is_normalized($row[$fieldName], Normalizer::FORM_C)) {
                    continue;
                }

                //debugLog("Pre : $row[$fieldName]");
                $row[$fieldName] = Normalizer::normalize($row[$fieldName], Normalizer::FORM_C);
                //debugLog("Post: $row[$fieldName]");
                $normalized[] = $fieldName;
            }
        }

        return $normalized;
    }

    /**
     * Build an array of text fields to be repaired
     * from a specific bean
     *
     * @param $type
     * @return array
     */
    protected function getRepairableFieldNames($type): array
    {
        $bean = BeanFactory::getBean($type);
        if ($bean === false) {
            return [];
        }

        $repairableTypes = ['enum', 'longtext', 'name', 'text', 'varchar'];
        $varDefFields = $bean->getFieldDefinitions();

        if ($type === 'Users') {
            $repairableTypes[] = 'user_name';
        }

        $fieldList = array();
        foreach ($varDefFields as $field) {
            if (in_array($field['type'], $repairableTypes, true)) {
                $fieldList[] = $field['name'];
            }
        }

        return $fieldList;
    }

    /**
     * Get a chunk of records in an array
     * $chunk is an array of record ids to retrive
     *
     * @param array $chunk
     * @param $type
     * @return array
     */
    protected function getRecordChunk(array $chunk, $type): array
    {
        global $db;

        $bean = BeanFactory::getBean($type);
        $join = $bean->getCustomJoin();

        $sql = "SELECT * FROM $bean->table_name ";
        $sql .= $join['join'];
        $sql .= "WHERE id IN ('" . implode("','", $chunk) . "');";

        $result = $db->query($sql);
        $records = [];
        $row = $db->fetchByAssoc($result);
        while (!empty($row)) {
            $records[] = $row;
            $row = $db->fetchByAssoc($result);
        }

        if (empty($records)) {
            $this->debugLog("No records retrieved for bean type " . $type);
            $this->debugLog("" . $sql);
        }

        return $records;
    }

    /**
     * Log debug message
     * @param $string
     */
    protected function debugLog($string): void
    {
        global $log;

        $log->info("[utf-normalize] " . $string);
    }

    /**
     * Echo output
     * @param bool $echo
     * @param string $string
     */
    protected function echoOutput(bool $echo, string $string): void
    {
        if (empty($echo)) {
            return;
        }

        echo '<div>' . $string . '</div>';
        ob_flush();
        flush();
    }

    /**
     * @return array|mixed
     */
    protected function getModulesToNormalize()
    {
        $moduleList = [];
        if (empty($moduleList)) {
            $moduleList = $GLOBALS["moduleList"];
            $moduleList[] = 'Users';
        }

        $modInvisList = $GLOBALS["modInvisList"] ?? [];

        $toExlude = [
            'Calendar',
            'Administration',
            'CustomFields',
            'Connectors',
            'Dropdown',
            'Dynamic',
            'DynamicFields',
            'DynamicLayout',
            'EditCustomFields',
            'Help',
            'Import',
            'MySettings',
            'EditCustomFields',
            'FieldsMetaData',
            'UpgradeWizard',
            'Trackers',
            'Connectors',
            'Employees',
            'Calendar',
            'Sync',
            'Versions',
            'LabelEditor',
            'Roles',
            'EmailMarketing',
            'OptimisticLock',
            'TeamMemberships',
            'TeamSets',
            'TeamSetModule',
            'Audit',
            'MailMerge',
            'MergeRecords',
            'EmailText',
            'Schedulers',
            'Schedulers_jobs',
            'CampaignTrackers',
            'CampaignLog',
            'EmailMan',
            'Groups',
            'InboundEmail',
            'ACLActions',
            'ACLRoles',
            'DocumentRevisions',
            'ModuleBuilder',
            'Alert',
            'ResourceCalendar',
            'ACL',
            'Configurator',
            'UserPreferences',
            'SavedSearch',
            'Studio',
            'Connectors',
            'SugarFeed',
            'EAPM',
            'OAuthKeys',
            'OAuthTokens',
            'AM_TaskTemplates',
            'Reminders',
            'Reminders_Invitees',
            'AOD_IndexEvent',
            'AOD_Index',
            'AOR_Fields',
            'AOR_Charts',
            'AOR_Conditions',
            'AOS_Line_Item_Groups',
            'AOW_Processed',
            'Calls_Reschedule',
            'OutboundEmailAccounts',
            'TemplateSectionLine',
            'OAuth2Tokens',
            'OAuth2Clients',
        ];

        $modInvisList = array_diff($modInvisList, $toExlude);
        $moduleList = array_merge($moduleList, $modInvisList);

        return $moduleList;
    }

}
