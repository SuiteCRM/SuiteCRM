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

use BeanFactory;
use RuntimeException;
use SugarBean;

/**
 * This class facilitates pulling Sugarbeans from a module
 */
#[\AllowDynamicProperties]
class ElasticSearchModuleDataPuller
{
    /** 
     * The source module
     * 
     * @var string 
     */
    protected $module;

    /** 
     * The Sugarbean seed
     * 
     * @var SugarBean 
     */
    protected $seed;

    /** 
     * Whether this is a differential run or not
     * 
     * @var bool 
     */
    protected $isDifferential;

    /** 
     * Whether to pull Deleted records
     *  
     * @var int *
     */
    protected $showDeleted = 0;

    /** 
     * The pagination offset
     *  
     * @var int *
     */
    protected $offset = 0;

    /** 
     * The size of batches pulled from the database.
     *  
     * @var int *
     */
    protected $batchSize = 1000;

    /**
     * The last index time
     *
     * @var string
     */
    protected $lastIndexTime;

    /**
     * The number of records pulled from the DB
     *
     * @var integer
     */
    protected $recordsPulled = 0;


    public function __construct($module, $isDifferential, $logger)
    {
        $this->module = $module;
        $this->seed = BeanFactory::getBean($module);
        $this->isDifferential = $isDifferential;  
        $this->logger = $logger;   

    }

    /**
     * Set the LastIndexTime
     *
     * @param string $lastIndexTime
     * @return this
     */
    public function setLastIndexTime($lastIndexTime)
    {
        $this->lastIndexTime = $lastIndexTime;
        return $this;
    }

    /**
     * Set the ShowDeleted flag. 
     * Set to 1 to return deleted records in the results
     *
     * @param int $showDeleted
     * @return this
     */
    public function setShowDeleted($showDeleted)
    {
        $this->showDeleted = (int) $showDeleted;
        return $this;
    }

    /**
     * Set whether this run should be differential 
     * This influences whether the where clause is used
     *
     * @param bool $isDifferential
     * @return this
     */
    public function setDifferential($isDifferential)
    {
        $this->isDifferential = $isDifferential;
        return $this;
    }

    /**
     * Pull the next batch of records from the database
     *
     * @return array | null
     */
    public function pullNextBatch()
    {
        $results = $this->seed->get_list('id', $this->generateWhere(), $this->offset, $this->batchSize, $this->batchSize, $this->showDeleted);

        $this->offset = $results['next_offset'];
        $this->recordsPulled += is_countable($results['list']) ? count($results['list']) : 0;

        return $results['row_count'] ? $results['list'] : null;
    }

    /**
     * Generates the where clause used in the get_list query
     *
     * @return string
     */
    protected function generateWhere()
    {
        if($this->isDifferential && empty($this->lastIndexTime)){
            throw new RuntimeException("A differential search must have a lastIndexTime to filter off of");
        }

        if($this->isDifferential){
            $tableName = $this->seed->table_name;
            $lastIndexTime = $this->lastIndexTime;
            return "$tableName.date_modified > '$lastIndexTime' OR $tableName.date_entered > '$lastIndexTime'";    
        }

        return '';
    }

    /**
     * Magic getter
     *
     * @param string $field
     * @return mixed
     */
    public function __get($field)
    {
        return $this->$field;
    }
}
