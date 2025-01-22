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

namespace SuiteCRM\Search\ElasticSearch;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use InvalidArgumentException;
use LoggerManager;
use SugarBean;
use SuiteCRM\Search\Exceptions\SearchException;
use SuiteCRM\Utility\SuiteLogger;
use Throwable;

/**
 * Class ElasticSearchHooks handles logic hooks to keep the elasticsearch index synchronised.
 */
#[\AllowDynamicProperties]
class ElasticSearchHooks
{
    /** @var SugarBean */
    private $bean;
    /** @var string */
    private $action;
    /** @var ElasticSearchIndexer */
    private $indexer;

    /**
     * Callback for save beans.
     *
     * @param SugarBean $bean
     * @param           $event
     * @param           $arguments
     */
    public function beanSaved(SugarBean $bean, $event, $arguments)
    {
        $this->action = 'index';

        if (ElasticSearchIndexer::isEnabled() !== false) {
            $this->reIndexSafe($bean);
        }
    }

    /**
     * Callback for deleted beans.
     *
     * @param SugarBean $bean
     * @param           $event
     * @param           $arguments
     */
    public function beanDeleted(SugarBean $bean, $event, $arguments)
    {
        $this->action = 'remove';

        if (ElasticSearchIndexer::isEnabled() !== false) {
            $this->reIndexSafe($bean);
        }
    }

    // ~ ~ ~ ~ ~ ~
    // Private Methods
    // ~ ~ ~ ~ ~ ~

    /**
     * @param SugarBean $bean
     */
    private function reIndexSafe(SugarBean $bean)
    {
        try {
            $this->reIndex($bean);
        } catch (SearchException $exception) {
            $this->handleError($exception);
        } catch (\Exception $exception) {
            $this->handleError($exception);
        } catch (\Throwable $throwable) {
            $this->handleError($throwable);
        }
    }

    /**
     * @param SugarBean $bean
     *
     * @return void
     */
    private function reIndex(SugarBean $bean)
    {
        if (ElasticSearchIndexer::isEnabled() === false) {
            throw new SearchException(
                'Elasticsearch trying to re-indexing a bean but indexer is disabled in configuration.',
                SearchException::ES_DISABLED
            );
        }

        $this->bean = $bean;

        $this->getIndexer();

        if (!$this->isBlacklisted()) {
            $this->correctAction();
            $this->performAction($bean);
        } else {
            LoggerManager::getLogger()->warn(
                'Elasticsearch trying to re-indexing a bean but this module is blacklisted: ' .
                $bean->module_name
            );
        }
    }

    /**
     * @return void
     */
    private function getIndexer()
    {
        /** @noinspection PhpUndefinedFieldInspection */
        $indexer = !isset($this->bean->indexer)
            ? new ElasticSearchIndexer()
            : $this->bean->indexer;

        $this->indexer = $indexer;
    }

    /**
     * @return bool
     */
    private function isBlacklisted()
    {
        return !in_array($this->bean->module_name, $this->indexer->getModulesToIndex());
    }

    private function correctAction()
    {
        if ($this->bean->deleted) {
            $this->action = 'remove';
        }
    }

    /**
     * @param SugarBean $bean
     */
    private function performAction(SugarBean $bean)
    {
        switch ($this->action) {
            case 'index':
                $this->indexer->indexBean($bean);
                break;
            case 'remove':
                $this->indexer->removeBean($bean);
                break;
            default:
                throw new InvalidArgumentException('Wrong action provided');
        }
    }

    /**
     * @param Throwable $exception
     */
    private function handleError($exception)
    {
        $message = "Failed to $this->action bean to index";

        if (isset($this->indexer)) {
            $this->indexer->getLogger()->error($message);
            $this->indexer->getLogger()->error($exception);
            return;
        }

        $logger = new SuiteLogger();
        $logger->error($message);
        $logger->error($exception);
    }
}
