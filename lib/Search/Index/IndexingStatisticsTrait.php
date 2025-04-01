<?php
/**
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

namespace SuiteCRM\Search\Index;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Carbon\CarbonInterval;
use Monolog\Logger;

/**
 * Trait IndexingStatisticsTrait.
 *
 * This is intended to be use with an instance of AbstractIndexer.
 *
 * @property Logger logger
 */
trait IndexingStatisticsTrait
{
    // stats
    /** @var int number of modules indexed */
    private $indexedModulesCount;
    /** @var int number of records (beans) indexed */
    private $indexedRecordsCount;
    /** @var int number of record fields indexed */
    private $indexedFieldsCount;
    /** @var int number of records (beans) removed */
    private $removedRecordsCount;

    /** @return int */
    public function getRemovedRecordsCount()
    {
        return $this->removedRecordsCount;
    }

    /** @return int */
    public function getIndexedRecordsCount()
    {
        return $this->indexedRecordsCount;
    }

    /** @return int */
    public function getIndexedFieldsCount()
    {
        return $this->indexedFieldsCount;
    }

    /** @return int */
    public function getIndexedModulesCount()
    {
        return $this->indexedModulesCount;
    }

    /**
     * Shows statistics for the past run.
     *
     * @param float $end
     * @param float $start
     */
    private function statistics($end, $start)
    {
        if ($this->removedRecordsCount) {
            $this->logger->debug(sprintf('%s records have been removed', $this->removedRecordsCount));
        }

        if ($this->indexedRecordsCount === 0) {
            $this->logger->debug('No record has been indexed');
            return;
        }

        $elapsed = ($end - $start); // seconds
        $this->logger->debug(sprintf('%d modules, %d records and %d fields indexed in %01.3F s', $this->indexedModulesCount, $this->indexedRecordsCount, $this->indexedFieldsCount, $elapsed));

        if ($this->indexedRecordsCount > 100) {
            $estimation = $elapsed / $this->indexedRecordsCount * 200000;
            CarbonInterval::setLocale('en');
            $estimationString = CarbonInterval::seconds((int) round($estimation))->cascade()->forHumans(true);
            $fieldsSpeed = $this->indexedFieldsCount / $elapsed;
            $this->logger->debug(sprintf('Average speed is %01.3F fields/s', $fieldsSpeed));
            $this->logger->debug("It would take ~$estimationString for 200,000 records, assuming a linear expansion");
        }
    }

    /** Resets the counters to zero. */
    private function resetCounts()
    {
        $this->indexedModulesCount = 0;
        $this->indexedRecordsCount = 0;
        $this->indexedFieldsCount = 0;
        $this->removedRecordsCount = 0;
    }
}
