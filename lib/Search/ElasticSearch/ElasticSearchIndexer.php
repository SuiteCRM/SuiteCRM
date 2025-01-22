<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\Search\ElasticSearch;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Carbon\Carbon;
use Elasticsearch\Client;
use Elasticsearch\Common\Exceptions\Missing404Exception;
use Exception;
use JsonSchema\Exception\RuntimeException;
use LoggerManager;
use Monolog\Logger;
use SugarBean;
use SuiteCRM\Search\Index\AbstractIndexer;
use SuiteCRM\Search\Index\Documentify\AbstractDocumentifier;
use SuiteCRM\Search\Index\Documentify\SearchDefsDocumentifier;
use SuiteCRM\Search\Index\IndexingLockFileTrait;
use SuiteCRM\Search\Index\IndexingSchedulerTrait;
use SuiteCRM\Search\Index\IndexingStatisticsTrait;

/**
 * Class ElasticSearchIndexer takes care of creating a search index for the database.
 */
#[\AllowDynamicProperties]
class ElasticSearchIndexer extends AbstractIndexer
{
    use IndexingStatisticsTrait;
    use IndexingLockFileTrait;
    use IndexingSchedulerTrait;

    /** @var string The name of the Elasticsearch index to use. */
    private $index;
    /** @var Client */
    private $client;
    /** @var int the size of the batch to be sent to the Elasticsearch while batch indexing */
    private $batchSize = 1000;
    /** @var Carbon|false the timestamp of the last indexing. false if unknown */
    private $lastRunTimestamp = false;

    /**
     * ElasticSearchIndexer constructor.
     *
     * @param Client|null $client
     */
    public function __construct(Client $client = null)
    {
        parent::__construct();

        $this->client = $client ?? ElasticSearchClientBuilder::getClient();
    }

    /**
     * Returns whether the Elasticsearch is enabled by user configuration or not.
     *
     * @return bool|null
     */
    public static function isEnabled(): ?bool
    {
        /** @noinspection PhpVariableNamingConventionInspection */
        global $sugar_config;

        try {
            return !empty($sugar_config['search']['ElasticSearch']['enabled']);
        } catch (Exception $exception) {
            LoggerManager::getLogger()->fatal("Failed to retrieve ElasticSearch options");

            return false;
        }
    }

    /** @inheritdoc */
    public function index()
    {
        $this->logger->info('Starting indexing procedures');

        $this->resetCounts();

        $this->logger->debug('Indexing is performed using ' . $this->getDocumentifierName());

        if ($this->differentialIndexing) {
            $this->lastRunTimestamp = $this->readLockFile();
        }

        $modules = $this->getModulesToIndex();
        $start = microtime(true);

        if ($this->differentialIndexing()) {
            $this->logger->debug('A differential indexing will be performed');
        } else {
            $this->logger->debug('A full indexing will be performed');

            foreach ($modules as $module) {
                try {
                    $lowercaseModule = strtolower($module);
                    $this->removeIndex($lowercaseModule);
                    $this->createIndex($lowercaseModule);
                } catch (Exception $exception) {
                    $message = "Failed to create index $module! Exception details follow";
                    $this->logger->error($message);
                    $this->logger->error($exception);
                }
            }
        }

        foreach ($modules as $module) {
            try {
                $this->indexModule($module);
            } catch (Exception $exception) {
                $message = "Failed to index module $module! Exception details follow";
                $this->logger->error($message);
                $this->logger->error($exception);
            }
        }

        $end = microtime(true);

        if ($this->differentialIndexing) {
            $this->writeLockFile();
        }

        $this->statistics($end, $start);

        $this->logger->info("Indexing complete");
    }

    /**
     * @param string $index
     */
    public function removeIndex(string $index): void
    {
        $params = ['index' => $index];
        $params['client'] = ['ignore' => [404]];

        $this->client->indices()->delete($params);

        $this->logger->debug("Removed index '$index'");
    }

    /**
     * Creates a new index in the Elasticsearch server.
     *
     * The optional $body can be used to set up the index settings, mappings, etc.
     *
     * @param string $index name of the index
     * @param array|null $body options of the index
     */
    public function createIndex(string $index, array $body = null): void
    {
        $params = ['index' => $index];

        if (!empty($body) && is_array($body)) {
            $params['body'] = $body;
        }

        $this->client->indices()->create($params);

        $this->logger->debug("Created new index '$index'");
    }

    /** @inheritdoc */
    public function indexModule($module)
    {
        $isDifferential = $this->differentialIndexing();
        $dataPuller = new ElasticSearchModuleDataPuller($module, $isDifferential, $this->logger);

        $this->buildWhereClause($dataPuller, $isDifferential, $module);

        $this->logger->debug(sprintf('Indexing module %s...', $module));

        try {
            $beanTime = Carbon::now()->toDateTimeString();

            while ($beans = $dataPuller->pullNextBatch()) {
                $this->indexBeans($module, $beans);
            }
            $this->logger->debug(sprintf('Finished %s. Processed %d Records', $module, $dataPuller->recordsPulled));

        } catch (RuntimeException $exception) {
            $this->logger->error("Failed to index module $module");
            $this->logger->error($exception);

            return;
        }

        if ($dataPuller->recordsPulled === 0) {
            if (!$isDifferential) {
                $this->logger->notice(sprintf('Skipped %s because $beans was null. The table is probably empty',
                    $module));
            }

            return;
        }

        $this->putMeta($module, [
            'last_index' => $beanTime,
            'module_name' => $module
        ]);
        $this->indexedModulesCount++;
    }

    /**
     * For differential runs, attempt to pull the Last Index time and set on dataPuller
     *
     * @param ElasticSearchModuleDataPuller $dataPuller
     * @param bool $isDifferential
     * @param string $module
     * @return void
     */
    protected function buildWhereClause($dataPuller, $isDifferential, $module): void
    {
        if ($isDifferential) {
            try {
                $datetime = $this->getModuleLastIndexed($module);
                $dataPuller->setLastIndexTime($datetime)->setShowDeleted(-1);
            } catch (Exception $exception) {
                $this->logger->notice("Time metadata not found for $module, performing full index for this module");
                $dataPuller->setDifferential(false);
            }
        }
    }

    /** @inheritdoc */
    public function indexBeans($module, array $beans)
    {
        $oldCount = $this->indexedRecordsCount;
        $oldRemCount = $this->removedRecordsCount;
        $this->indexBatch($module, $beans);
        $diff = $this->indexedRecordsCount - $oldCount;
        $remDiff = $this->removedRecordsCount - $oldRemCount;
        $total = count($beans) - $remDiff;
        $type = $total === $diff ? Logger::DEBUG : Logger::WARNING;
        $this->logger->log($type, sprintf('Indexed %d', $diff));
    }

    /** Removes all the indexes from Elasticsearch, effectively nuking all data. */
    public function removeAllIndices(): void
    {
        $this->logger->debug("Deleting all indices");
        try {
            $this->client->indices()->delete(['index' => '_all']);
        } /** @noinspection PhpRedundantCatchClauseInspection */
        catch (Missing404Exception $ignore) {
            // Index not there, not big deal since we meant to delete it anyway.
            $this->logger->warn('Index not found, no index has been deleted.');
        }
    }

    /** @inheritdoc */
    public function indexBean(SugarBean $bean)
    {
        $this->logger->debug("Indexing {$bean->module_name}($bean->name)");

        $args = $this->makeIndexParamsFromBean($bean);

        $this->client->index($args);
    }

    /** @inheritdoc */
    public function removeBean(SugarBean $bean)
    {
        $this->logger->debug("Removing {$bean->module_name}($bean->name)");

        $args = $this->makeParamsHeaderFromBean($bean);
        $this->client->delete($args);
    }

    /**
     * @inheritdoc
     *
     * @param bool $ignore404 deleting something that is not there won't throw an error
     */
    public function removeBeans(array $beans, $ignore404 = true)
    {
        $params = [];

        if ($ignore404) {
            $params['client']['ignore'] = [404];
        }

        foreach ($beans as $bean) {
            $params['body'][] = ['delete' => $this->makeParamsHeaderFromBean($bean)];
        }

        $this->sendBatch($params);
    }

    /**
     * Attempts to contact the Elasticsearch server and perform a simple request.
     *
     * Returns `false` in case of failure or the time it took to perform the operation in microseconds.
     *
     * @return int|false
     */
    public function ping()
    {
        $start = Carbon::now()->micro;
        $status = $this->client->ping();
        $elapsed = Carbon::now()->micro - $start;

        if ($status === false) {
            $this->logger->error("Failed to ping server");

            return false;
        }

        $this->logger->debug("Ping performed in $elapsed µs");

        return $elapsed;
    }

    /**
     * Writes the metadata fields for one index.
     *
     * @param string $module name of the module
     * @param array $meta an associative array with the fields to populate
     */
    public function putMeta(string $module, array $meta): void
    {
        $params = [
            'index' => strtolower($module),
            'body' => ['_meta' => $meta],
            'ignore_unavailable' => true
        ];

        $this->client->indices()->putMapping($params);
    }

    /**
     * Returns the metadata fields for one index.
     *
     * @param string $module name of the module
     *
     * @return mixed[]|null an associative array with the metadata
     */
    public function getMeta(string $module): ?array
    {
        $lowercaseModule = strtolower($module);
        $params = ['index' => $lowercaseModule];
        $results = $this->client->indices()->getMapping($params);

        if (!isset($results[$lowercaseModule])) {
            return null;
        }

        return $results[$lowercaseModule]['mappings']['_meta'] ?? array();
    }

    /**
     * @return int
     */
    public function getBatchSize(): int
    {
        return $this->batchSize;
    }

    /**
     * @param int $batchSize
     */
    public function setBatchSize(int $batchSize): void
    {
        $this->batchSize = $batchSize;
    }

    /** @return string */
    public function getIndex(): string
    {
        return $this->index;
    }

    /**
     * Sets the name of the Elasticsearch index to send requests to.
     *
     * @param string $index
     */
    public function setIndex(string $index): void
    {
        $this->logger->debug("Setting index to $index");
        $this->index = $index;
    }

    /**
     * Creates a batch indexing request for Elasticsearch.
     *
     * The size of the batch is defined by `this::batchSize`.
     *
     * Additionally, Beans marked as deleted will be remove from the index.
     *
     * @param string $module
     * @param SugarBean[] $beans
     *
     * @see batchSize
     */
    private function indexBatch(string $module, array $beans): void
    {
        $params = ['body' => []];

        foreach ($beans as $key => $bean) {
            $head = ['_index' => strtolower($module), '_id' => $bean->id];

            if ($bean->deleted) {
                $params['body'][] = ['delete' => $head];
                $this->removedRecordsCount++;
            } else {
                $body = $this->makeIndexParamsBodyFromBean($bean);
                $params['body'][] = ['index' => $head];
                $params['body'][] = $body;
                $this->indexedRecordsCount++;
                $this->indexedFieldsCount += count($body);
            }

            // Send a batch of $this->batchSize elements to the server
            if ($key % $this->batchSize == 0) {
                $this->sendBatch($params);
            }
        }

        // Send the last batch if it exists
        if (!empty($params['body'])) {
            $this->sendBatch($params);
        }
    }

    /**
     * Returns true if differentialIndexing is enabled and a previous run timestamp was found.
     *
     * @return bool
     */
    private function differentialIndexing(): bool
    {
        return $this->differentialIndexing && $this->lastRunTimestamp !== false;
    }

    /**
     * Creates the body of a Elasticsearch request for a given bean.
     *
     * It uses a Documentifier to make a document out of a SugarBean.
     *
     * @param SugarBean $bean
     *
     * @return array
     * @see AbstractDocumentifier
     */
    private function makeIndexParamsBodyFromBean(SugarBean $bean): array
    {
        return $this->documentifier->documentify($bean);
    }

    /**
     * Sends a batch request with the given params.
     *
     * Sends the requests and attempts to parse errors and fix the number of indexed records in case of errors.
     *
     * @param array $params
     */
    private function sendBatch(array &$params): void
    {
        // sends the batch over to the server
        $responses = $this->client->bulk($params);

        if (isset($responses['errors']) && $responses['errors'] === true) {
            // logs the errors
            foreach ($responses['items'] as $item) {
                $action = array_keys($item)[0];
                if(isset($item[$action]['error'])) {
                    $type = $item[$action]['error']['type'];
                    $reason = $item[$action]['error']['reason'];
                    $this->logger->error("[$action] [$type] $reason");
                    
                    if ($action === 'index') {
                        $this->indexedRecordsCount--;
                    }

                    if ($action === 'delete') {
                        $this->removedRecordsCount--;
                    }
                }
            }
        }

        // erase the old bulk request
        $params = ['body' => []];

        // unset the bulk response when you are done to save memory
        unset($responses);
    }

    /**
     * Generates the params for indexing a bean from the bean itself.
     *
     * @param SugarBean $bean
     *
     * @return array
     */
    private function makeIndexParamsFromBean(SugarBean $bean): array
    {
        $args = $this->makeParamsHeaderFromBean($bean);
        $args['body'] = $this->makeIndexParamsBodyFromBean($bean);

        return $args;
    }

    /**
     * Generates the headers for indexing a bean from the bean itself.
     *
     * @param SugarBean $bean
     *
     * @return array
     */
    private function makeParamsHeaderFromBean(SugarBean $bean): array
    {
        return [
            'index' => strtolower($bean->module_name),
            'id' => $bean->id,
        ];
    }

    /**
     * Retrieves the last time a module was indexed from a metadata stored in the Elasticsearch index.
     *
     * @param string $module
     *
     * @return string a datetime string
     */
    private function getModuleLastIndexed(string $module): string
    {
        $meta = $this->getMeta($module);

        if (!isset($meta['last_index'])) {
            throw new RuntimeException("Last index metadata not found.");
        }

        return $meta['last_index'];
    }

    /**
     * @param bool $differential
     * @param int $searchdefs
     */
    public static function repairElasticsearchIndex(bool $differential = true, int $searchdefs = 0): void
    {
        $indexer = new ElasticSearchIndexer();
        if (!$indexer::isEnabled()) {
            return;
        }
        $indexer->setDifferentialIndexing($differential);
        if ($searchdefs) {
            $indexer->setDocumentifier(new SearchDefsDocumentifier());
        }
        $indexer->index();
    }
}
