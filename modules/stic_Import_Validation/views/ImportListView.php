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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
require_once('include/ListView/ListViewSmarty.php');


#[\AllowDynamicProperties]
class ImportListView
{
    /**
     * @var array
     */
    protected $data = array();

    /**
     * @var array
     */
    protected $headerColumns = array();

    /**
     * @var Sugar_Smarty
     */
    private $ss;

    /**
     * @var string
     */
    private $tableID;

    /**
     * @var Paginatable
     */
    private $dataSource;

    /**
     * @var int
     */
    private $recordsPerPage;

    /**
     * @var int
     */
    private $maxColumns;

    /**
     * Create a list view object that can display a data source which implements the Paginatable interface.
     *
     * @throws Exception
     * @param  Paginatable $dataSource
     * @param  array $params
     * @param string $tableIdentifier
     */
    public function __construct($dataSource, $params, $tableIdentifier = '')
    {
        global $sugar_config;

        $this->ss = new Sugar_Smarty();
        $this->tableID = $tableIdentifier;

        $this->dataSource = $dataSource;
        $this->headerColumns = $this->dataSource->getHeaderColumns();

        if (!isset($params['offset'])) {
            throw new Exception("Missing required parameter offset for stic_Import_ValidationListView");
        } else {
            $this->dataSource->setCurrentOffset($params['offset']);
        }

        $this->recordsPerPage = isset($params['totalRecords']) ? $params['totalRecords'] : ($sugar_config['list_max_entries_per_page'] + 0);
        $this->data = $this->dataSource->loadDataSet($this->recordsPerPage)->getDataSet();
        $this->maxColumns = $this->getMaxColumnsForDataSet();
    }

    /**
     * Display the list view like table.
     *
     * @param bool $return True if we should return the content rather than echoing.
     * @return
     */
    public function display($return = false)
    {
        global $app_strings,$mod_strings;

        $navStrings = array('next' => $app_strings['LNK_LIST_NEXT'],'previous' => $app_strings['LNK_LIST_PREVIOUS'],'end' => $app_strings['LNK_LIST_END'],
                            'start' => $app_strings['LNK_LIST_START'],'of' => $app_strings['LBL_LIST_OF']);
        $this->ss->assign('navStrings', $navStrings);
        $this->ss->assign('pageData', $this->generatePaginationData());
        $this->ss->assign('tableID', $this->tableID);
        $this->ss->assign('colCount', count($this->headerColumns));
        $this->ss->assign('APP', $app_strings);
        $this->ss->assign('rowColor', array('oddListRow', 'evenListRow'));
        $this->ss->assign('displayColumns', $this->headerColumns);
        $this->ss->assign('data', $this->data);
        $this->ss->assign('maxColumns', $this->maxColumns);
        $this->ss->assign('MOD', $mod_strings);
        $contents = $this->ss->fetch('modules/stic_Import_Validation/tpls/listview.tpl');
        if ($return) {
            return $contents;
        } else {
            echo $contents;
        }
    }

    /**
     * For the data set that was loaded, find the max count of entries per row.
     *
     * @return int
     */
    protected function getMaxColumnsForDataSet()
    {
        $maxColumns = 0;
        foreach ($this->data as $data) {
            if ((is_countable($data) ? count($data) : 0) > $maxColumns) {
                $maxColumns = is_countable($data) ? count($data) : 0;
            }
        }
        return $maxColumns;
    }

    /**
     * Generate the pagination data.
     *
     * @return array
     */
    protected function generatePaginationData()
    {
        $currentOffset = $this->dataSource->getCurrentOffset();
        $totalRecordsCount = $this->dataSource->getTotalRecordCount();
        $nextOffset =  $currentOffset+ $this->recordsPerPage;
        $nextOffset = $nextOffset > $totalRecordsCount ? 0 : $nextOffset;
        $lastOffset = floor($totalRecordsCount / $this->recordsPerPage) * $this->recordsPerPage;
        $previousOffset = $currentOffset - $this->recordsPerPage;
        $offsets = array('totalCounted'=> true, 'total' => $totalRecordsCount, 'next' => $nextOffset,
                         'last' => $lastOffset, 'previous' => $previousOffset,
                         'current' => $currentOffset, 'lastOffsetOnPage' => count($this->data) + $this->dataSource->getCurrentOffset() );

        $pageData = array('offsets' => $offsets);
        return $pageData;
    }
}
