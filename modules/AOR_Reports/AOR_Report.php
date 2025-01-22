<?php
/**
 *
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

use SuiteCRM\CleanCSV;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class AOR_Report extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOR_Reports';
    public $object_name = 'AOR_Report';
    public $table_name = 'aor_reports';
    public $importable = true;
    public $disable_row_level_security = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $report_module;

    public function __construct()
    {
        parent::__construct();
        require_once('modules/AOW_WorkFlow/aow_utils.php');
        require_once('modules/AOR_Reports/aor_utils.php');
    }




    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function save($check_notify = false)
    {

        // TODO: process of saveing the fields and conditions is too long so we will have to make some optimization on save_lines functions
        set_time_limit(3600);

        if (empty($this->id) || (isset($_POST['duplicateSave']) && $_POST['duplicateSave'] == 'true')) {
            unset($_POST['aor_conditions_id']);
            unset($_POST['aor_fields_id']);
        }

        $return_id = parent::save($check_notify);

        require_once('modules/AOR_Fields/AOR_Field.php');
        $field = BeanFactory::newBean('AOR_Fields');
        $field->save_lines($_POST, $this, 'aor_fields_');

        require_once('modules/AOR_Conditions/AOR_Condition.php');
        $condition = BeanFactory::newBean('AOR_Conditions');
        $condition->save_lines($_POST, $this, 'aor_conditions_');

        require_once('modules/AOR_Charts/AOR_Chart.php');
        $chart = BeanFactory::newBean('AOR_Charts');
        $chart->save_lines($_POST, $this, 'aor_chart_');

        return $return_id;
    }

    /**
     * @param string $view
     * @param string $is_owner
     * @param string $in_group
     * @return bool
     */
    public function ACLAccess($view, $is_owner = 'not_set', $in_group = 'not_set')
    {
        $result = parent::ACLAccess($view, $is_owner, $in_group);
        if ($result && $this->report_module !== '') {
            $result = ACLController::checkAccess($this->report_module, 'list', true);
        }

        return $result;
    }

    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_detail_fields();
        $this->load_report_beans();
    }

    public function load_report_beans()
    {
        global $beanList, $app_list_strings;

        $app_list_strings['aor_moduleList'] = $app_list_strings['moduleList'];

        foreach ($app_list_strings['aor_moduleList'] as $mkey => $mvalue) {
            if (!isset($beanList[$mkey]) || str_begin($mkey, 'AOR_') || str_begin($mkey, 'AOW_')) {
                unset($app_list_strings['aor_moduleList'][$mkey]);
            }
            if (!ACLController::checkAccess($mkey, 'list', true)) {
                unset($app_list_strings['aor_moduleList'][$mkey]);
            }
        }

        $app_list_strings['aor_moduleList'] = array_merge(
            array('' => ''),
            (array)$app_list_strings['aor_moduleList']
        );

        asort($app_list_strings['aor_moduleList']);
    }


    public function getReportFields()
    {
        $fields = array();
        foreach ($this->get_linked_beans('aor_fields', 'AOR_Fields') as $field) {
            $fields[] = $field;
        }
        usort($fields, function ($a, $b) {
            return $a->field_order - $b->field_order;
        });

        return $fields;
    }

    public const CHART_TYPE_PCHART = 'pchart';
    public const CHART_TYPE_CHARTJS = 'chartjs';
    public const CHART_TYPE_RGRAPH = 'rgraph';


    public function build_report_chart($chartIds = null, $chartType = self::CHART_TYPE_PCHART)
    {
        global $beanList;
        $linkedCharts = $this->get_linked_beans('aor_charts', 'AOR_Charts');
        if (!$linkedCharts) {
            //No charts to display
            LoggerManager::getLogger()->warn('No charts to display to build report chart for AOR Report.');
            return '';
        }

        $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '" . $this->id . "' AND deleted = 0 ORDER BY field_order ASC";
        $result = $this->db->query($sql);

        $fields = array();
        $i = 0;

        $mainGroupField = null;

        while ($row = $this->db->fetchByAssoc($result)) {
            $field = BeanFactory::newBean('AOR_Fields');
            $field->retrieve($row['id']);

            $path = unserialize(base64_decode($field->module_path));

            $field_bean = new $beanList[$this->report_module]();

            $field_module = $this->report_module;
            $field_alias = $field_bean->table_name;
            if ($path[0] != $this->report_module) {
                foreach ($path as $rel) {
                    if (empty($rel)) {
                        continue;
                    }
                    $field_module = getRelatedModule($field_module, $rel);
                    $field_alias = $field_alias . ':' . $rel;
                }
            }
            $label = str_replace(' ', '_', (string) $field->label) . $i;
            $fields[$label]['field'] = $field->field;
            $fields[$label]['label'] = $field->label;
            $fields[$label]['display'] = $field->display;
            $fields[$label]['function'] = $field->field_function;
            $fields[$label]['module'] = $field_module;
            $fields[$label]['alias'] = $field_alias;
            $fields[$label]['link'] = $field->link;
            $fields[$label]['total'] = $field->total;


            $fields[$label]['format'] = $field->format;

            // get the main group

            if ($field->group_display) {

                // if we have a main group already thats wrong cause only one main grouping field possible
                if (!is_null($mainGroupField)) {
                    $GLOBALS['log']->fatal('main group already found');
                }

                $mainGroupField = $field;
            }

            ++$i;
        }


        $query = $this->build_report_query();
        $result = $this->db->query($query);
        $data = array();
        while ($row = $this->db->fetchByAssoc($result, false)) {
            foreach ($fields as $name => $att) {
                $currency_id = isset($row[$att['alias'] . '_currency_id']) ? $row[$att['alias'] . '_currency_id'] : '';

                if ($att['function'] != 'COUNT' && empty($att['format']) && !is_numeric($row[$name])) {
                    $row[$name] = trim(strip_tags(getModuleField(
                        $att['module'],
                        $att['field'],
                        $att['field'],
                        'DetailView',
                        $row[$name],
                        '',
                        $currency_id
                    )));
                }
            }
            $data[] = $row;
        }
        $fields = $this->getReportFields();
        $html = '';
        switch ($chartType) {
            case self::CHART_TYPE_PCHART:
                $html = '<script src="modules/AOR_Charts/lib/pChart/imagemap.js"></script>';
                break;
            case self::CHART_TYPE_CHARTJS:
                $html = '<script src="modules/AOR_Reports/js/Chart.js"></script>';
                break;
            case self::CHART_TYPE_RGRAPH:
                if ($_REQUEST['module'] != 'Home') {
                    require_once('include/SuiteGraphs/RGraphIncludes.php');
                }

                break;
        }
        $x = 0;
        foreach ($linkedCharts as $chart) {
            if ($chartIds !== null && !in_array($chart->id, $chartIds)) {
                continue;
            }
            $html .= $chart->buildChartHTML($data, $fields, $x, $chartType, $mainGroupField);
            $x++;
        }

        return $html;
    }


    public function buildMultiGroupReport($offset = -1, $links = true, $level = 2, $path = array())
    {
        global $beanList;

        $rows = $this->getGroupDisplayFieldByReportId($this->id, $level);

        $rowsCount = is_countable($rows) ? count($rows) : 0;

        if ($rowsCount > 1) {
            $GLOBALS['log']->fatal('ambiguous group display for report ' . $this->id);
        } else {
            if ($rowsCount == 1) {
                $rows[0]['module_path'] = unserialize(base64_decode($rows[0]['module_path']));
                if (!$rows[0]['module_path'][0]) {
                    $module = new $beanList[$this->report_module]();
                    $rows[0]['field_id_name'] = $module->field_defs[$rows[0]['field']]['id_name'] ? $module->field_defs[$rows[0]['field']]['id_name'] : $module->field_defs[$rows[0]['field']]['name'];
                    $rows[0]['module_path'][0] = $module->table_name;
                } else {
                    $rows[0]['field_id_name'] = $rows[0]['field'];
                }
                $path[] = $rows[0];

                if ($level > 10) {
                    $msg = 'Too many nested groups';
                    $GLOBALS['log']->fatal($msg);

                    return null;
                }

                return $this->buildMultiGroupReport($offset, $links, $level + 1, $path);
            }
            if (!$rows) {
                if ($path) {
                    $html = '';
                    foreach ($path as $pth) {
                        $_fieldIdName = $this->db->quoteIdentifier($pth['field_id_name']);
                        $query = "SELECT $_fieldIdName FROM " . $this->db->quoteIdentifier($pth['module_path'][0]) . " GROUP BY $_fieldIdName;";
                        $values = $this->dbSelect($query);

                        foreach ($values as $value) {
                            $moduleFieldByGroupValue = $this->getModuleFieldByGroupValue(
                                $beanList,
                                $value[$pth['field_id_name']]
                                );
                            $moduleFieldByGroupValue = $this->addDataIdValueToInnertext($moduleFieldByGroupValue);
                            $html .= $this->getMultiGroupFrameHTML(
                                $moduleFieldByGroupValue,
                                $this->build_group_report($offset, $links)
                                );
                        }
                    }

                    return $html;
                }
                return $this->build_group_report($offset, $links, array());
            }
            throw new Exception('incorrect results');
        }
        throw new Exception('incorrect state');
    }

    private function getGroupDisplayFieldByReportId($reportId = null, $level = 1)
    {

        // set the default values

        if (is_null($reportId)) {
            $reportId = $this->id;
        }

        if (!$level) {
            $level = 1;
        }

        // escape values for query

        $_id = $this->db->quote($reportId);
        $_level = (int)$level;

        // get results array

        $query = "SELECT id, field, module_path FROM aor_fields WHERE aor_report_id = '$_id' AND group_display = $_level AND deleted = 0;";
        $rows = $this->dbSelect($query);

        return $rows;
    }


    private function dbSelect($query)
    {
        $results = $this->db->query($query);

        $rows = array();
        while ($row = $this->db->fetchByAssoc($results)) {
            $rows[] = $row;
        }

        return $rows;
    }

    private function getMultiGroupFrameHTML($header, $body)
    {
        $html = '<div class="multi-group-list" style="border: 1px solid black; padding: 10px;">
                    <h3>' . $header . '</h3>
                    <div class="multi-group-list-inner">' . $body . '</div>
                </div>';

        return $html;
    }

    private function addDataIdValueToInnertext($html)
    {
        preg_match('/\sdata-id-value\s*=\s*"([^"]*)"/', (string) $html, $match);
        $html = preg_replace('/(>)([^<]*)(<\/\w+>$)/', '$1$2' . $match[1] . '$3', (string) $html);

        return $html;
    }


    public function build_group_report($offset = -1, $links = true, $extra = array(), $subgroup = '')
    {
        global $beanList, $timedate, $app_strings;

        $html = '';
        $query = '';
        $query_array = array();
        $module = new $beanList[$this->report_module]();

        $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '" . $this->id . "' AND group_display = 1 AND deleted = 0 ORDER BY field_order ASC";
        $field_id = $this->db->getOne($sql);

        if (!$field_id) {
            $query_array['select'][] = $module->table_name . ".id AS '" . $module->table_name . "_id'";
        }

        if ($field_id != '' && empty($subgroup)) {
            $field = BeanFactory::newBean('AOR_Fields');
            $field->retrieve($field_id);

            $field_label = str_replace(' ', '_', (string) $field->label);

            $path = unserialize(base64_decode($field->module_path));

            $field_module = $module;
            $table_alias = $field_module->table_name;
            if (!empty($path[0]) && $path[0] != $module->module_dir) {
                foreach ($path as $rel) {
                    $new_field_module = new $beanList[getRelatedModule($field_module->module_dir, $rel)];
                    $oldAlias = $table_alias;
                    $table_alias = $table_alias . ":" . $rel;

                    $query_array = $this->build_report_query_join(
                        $rel,
                        $table_alias,
                        $oldAlias,
                        $field_module,
                        'relationship',
                        $query_array,
                        $new_field_module
                    );
                    $field_module = $new_field_module;
                }
            }

            $data = $field_module->field_defs[$field->field];

            if ($data['type'] == 'relate' && isset($data['id_name'])) {
                $field->field = $data['id_name'];
            }

            if ($data['type'] == 'currency' && !stripos(
                (string) $field->field,
                '_USD'
            ) && isset($field_module->field_defs['currency_id'])
            ) {
                if ((isset($field_module->field_defs['currency_id']['source']) && $field_module->field_defs['currency_id']['source'] == 'custom_fields')) {
                    $query_array['select'][$table_alias . '_currency_id'] = $table_alias . '_cstm' . ".currency_id AS '" . $table_alias . "_currency_id'";
                } else {
                    $query_array['select'][$table_alias . '_currency_id'] = $table_alias . ".currency_id AS '" . $table_alias . "_currency_id'";
                }
            }

            if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                $select_field = $this->db->quoteIdentifier($table_alias . '_cstm') . '.' . $field->field;
                // Fix for #1251 - added a missing parameter to the function call
                $query_array = $this->build_report_query_join(
                    $table_alias . '_cstm',
                    $table_alias . '_cstm',
                    $table_alias,
                    $field_module,
                    'custom',
                    $query_array
                );
            } else {
                $select_field = $this->db->quoteIdentifier($table_alias) . '.' . $field->field;
            }

            if ($field->sort_by != '') {
                $query_array['sort_by'][] = $field_label . ' ' . $field->sort_by;
            }

            if ($field->format && in_array($data['type'], array('date', 'datetime', 'datetimecombo'))) {
                if (in_array($data['type'], array('datetime', 'datetimecombo'))) {
                    $select_field = $this->db->convert($select_field, 'add_tz_offset');
                }
                $select_field = $this->db->convert(
                    $select_field,
                    'date_format',
                    array($timedate->getCalFormat($field->format))
                );
            }

            if ($field->field_function != null) {
                $select_field = $field->field_function . '(' . $select_field . ')';
            }

            $query_array['group_by'][] = $select_field;

            $query_array['select'][] = $select_field . " AS '" . $field_label . "'";
            if (isset($extra['select']) && $extra['select']) {
                foreach ($extra['select'] as $selectField => $selectAlias) {
                    if ($selectAlias) {
                        $query_array['select'][] = $selectField . " AS " . $selectAlias;
                    } else {
                        $query_array['select'][] = $selectField;
                    }
                }
            }
            $query_array['where'][] = $select_field . " IS NOT NULL AND ";
            if (isset($extra['where']) && $extra['where']) {
                $query_array['where'][] = implode(' AND ', $extra['where']) . ' AND ';
            }

            $query_array = $this->build_report_query_where($query_array);

            foreach ($query_array['select'] as $select) {
                $query .= ($query == '' ? 'SELECT ' : ', ') . $select;
            }

            $query .= ' FROM ' . $module->table_name . ' ';

            if (isset($query_array['join'])) {
                foreach ($query_array['join'] as $join) {
                    $query .= $join;
                }
            }
            if (isset($query_array['where'])) {
                $query_where = '';
                foreach ($query_array['where'] as $where) {
                    $query_where .= ($query_where == '' ? 'WHERE ' : ' ') . $where;
                }

                $query_where = $this->queryWhereRepair($query_where);

                $query .= ' ' . $query_where;
            }

            if (isset($query_array['group_by'])) {
                $query_group_by = '';
                foreach ($query_array['group_by'] as $group_by) {
                    $query_group_by .= ($query_group_by == '' ? 'GROUP BY ' : ', ') . $group_by;
                }
                $query .= ' ' . $query_group_by;
            }

            if (isset($query_array['sort_by'])) {
                $query_sort_by = '';
                foreach ($query_array['sort_by'] as $sort_by) {
                    $query_sort_by .= ($query_sort_by == '' ? 'ORDER BY ' : ', ') . $sort_by;
                }
                $query .= ' ' . $query_sort_by;
            }
            $result = $this->db->query($query);

            while ($row = $this->db->fetchByAssoc($result)) {
                if ($html !== '') {
                    $html .= '<br />';
                }
                $groupValue = $row[$field_label];
                $groupDisplay = $this->getModuleFieldByGroupValue($beanList, $groupValue);
                if (empty(trim($groupValue))) {
                    $groupValue = '_empty';
                    $groupDisplay = $app_strings['LBL_NONE'];
                }

                // Fix #5427 If download pdf then not use tab-content and add css inline to work with mpdf
                $pdf_style = "";
                $action = $_REQUEST['action'];
                if ($action == 'DownloadPDF') {
                    $pdf_style = "background: #333 !important; color: #fff !important; margin-bottom: 0px;";
                }

                $html .= '<div class="panel panel-default">
                            <div class="panel-heading" style="' . $pdf_style . '">
                                <a class="" role="button" data-toggle="collapse" href="#detailpanel_report_group_' . $groupValue . '" aria-expanded="false">
                                    <div class="col-xs-10 col-sm-11 col-md-11">
                                        ' . $groupDisplay . '
                                    </div>
                                </a>
                            </div>';
                if ($action != 'DownloadPDF') {
                    $html .= '<div class="panel-body panel-collapse collapse in" id="detailpanel_report_group_' . $groupValue . '">
                                <div class="tab-content">';
                } else {
                    $html .= '</div>';
                }


                $html .= $this->build_report_html($offset, $links, $groupValue, create_guid(), $extra);
                $html .= ($action == 'downloadPDF') ? '' : '</div></div></div>';
                // End
            }
        }

        if ($html == '') {
            $html = $this->build_report_html($offset, $links, $subgroup, create_guid(), $extra);
        }

        return $html;
    }


    public function build_report_html($offset = -1, $links = true, $group_value = '', $tableIdentifier = '', $extra = array())
    {
        global $beanList, $sugar_config;

        $_group_value = $this->db->quote($group_value);

        $report_sql = $this->build_report_query($_group_value, $extra);

        // Fix for issue 1232 - items listed in a single report, should adhere to the same standard as ListView items.
        if ($sugar_config['list_max_entries_per_page'] != '') {
            $max_rows = $sugar_config['list_max_entries_per_page'];
        } else {
            $max_rows = 20;
        }

        // See if the report actually has any fields, if not we don't want to run any queries since we can't show anything
        $fieldCount = is_countable($this->getReportFields()) ? count($this->getReportFields()) : 0;
        if (!$fieldCount) {
            $GLOBALS['log']->info('Running report "' . $this->name . '" with 0 fields');
        }

        $total_rows = 0;
        if ($fieldCount) {
            $count_sql = explode('ORDER BY', $report_sql);
            $count_query = 'SELECT count(*) c FROM (' . $count_sql[0] . ') as n';

            // We have a count query.  Run it and get the results.
            $result = $this->db->query($count_query);
            $assoc = $this->db->fetchByAssoc($result);
            if (!empty($assoc['c'])) {
                $total_rows = $assoc['c'];
            }
        }

        $html = '<div class="list-view-rounded-corners">';
        $html.='<table id="report_table_'.$tableIdentifier.$group_value.'" width="100%" border="0" class="list view table-responsive aor_reports">';

        $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '" . $this->id . "' AND deleted = 0 ORDER BY field_order ASC";
        $result = $this->db->query($sql);

        $html .= '<thead>';
        $html .= '<tr>';

        $fields = array();
        $i = 0;
        while ($row = $this->db->fetchByAssoc($result)) {
            $field = BeanFactory::newBean('AOR_Fields');
            $field->retrieve($row['id']);

            $path = unserialize(base64_decode($field->module_path));

            $field_bean = new $beanList[$this->report_module]();

            $field_module = $this->report_module;
            $field_alias = $field_bean->table_name;
            if ($path[0] != $this->report_module) {
                foreach ($path as $rel) {
                    if (empty($rel)) {
                        continue;
                    }
                    $field_module = getRelatedModule($field_module, $rel);
                    $field_alias = $field_alias . ':' . $rel;
                }
            }
            $label = str_replace(' ', '_', (string) $field->label) . $i;
            $fields[$label]['field'] = $field->field;
            $fields[$label]['label'] = $field->label;
            $fields[$label]['display'] = $field->display;
            $fields[$label]['function'] = $field->field_function;
            $fields[$label]['module'] = $field_module;
            $fields[$label]['alias'] = $field_alias;
            $fields[$label]['link'] = $field->link;
            $fields[$label]['total'] = $field->total;
            $fields[$label]['format'] = $field->format;
            $fields[$label]['params'] = [];


            if ($fields[$label]['display']) {
                // Fix #5427
                $html .= "<th scope='col'>";
                // End
                $html .= "<div>";
                $html .= $field->label;
                $html .= "</div></th>";
            }
            ++$i;
        }

        $html .= '</tr>';

        if ($offset >= 0) {
            $start = 0;
            $end = 0;
            $previous_offset = 0;
            $next_offset = 0;
            $last_offset = 0;

            if ($total_rows > 0) {
                $start = $offset + 1;
                $end = (($offset + $max_rows) < $total_rows) ? $offset + $max_rows : $total_rows;
                $previous_offset = ($offset - $max_rows) < 0 ? 0 : $offset - $max_rows;
                $next_offset = $offset + $max_rows;
                if (is_int($total_rows / $max_rows)) {
                    $last_offset = $max_rows * ($total_rows / $max_rows - 1);
                } else {
                    $last_offset = $max_rows * floor($total_rows / $max_rows);
                }
            }

            $html .= '<tr id="pagination" class="pagination-unique" role="presentation">';

            $html .= "<td colspan='$i'>
                       <table class='paginationTable' border='0' cellpadding='0' cellspacing='0' width='100%'>
                        <td nowrap=\"nowrap\" class=\"paginationActionButtons\" ></td>";

            $html .= '<td nowrap="nowrap" align="right" class="paginationChangeButtons" width="1%">';
            if ($offset == 0) {
                $html .= "<button type='button' id='listViewStartButton_top' name='listViewStartButton' title='Start' class='list-view-pagination-button' disabled='disabled'>
                    <span class='suitepicon suitepicon-action-first'></span>
                </button>
                <button type='button' id='listViewPrevButton_top' name='listViewPrevButton' class='list-view-pagination-button' title='Previous' disabled='disabled'>
                    <span class='suitepicon suitepicon-action-left'></span>
                </button>";
            } else {
                $html .= "<button type='button' id='listViewStartButton_top' name='listViewStartButton' title='Start' class='list-view-pagination-button' onclick='changeReportPage(\"" . $this->id . '",0,"' . $group_value . '","' . $tableIdentifier . "\")'>
                    <span class='suitepicon suitepicon-action-first'></span>
                </button>
                <button type='button' id='listViewPrevButton_top' name='listViewPrevButton' class='list-view-pagination-button' title='Previous' onclick='changeReportPage(\"" . $this->id . '",' . $previous_offset . ',"' . $group_value . '","' . $tableIdentifier . "\")'>
                    <span class='suitepicon suitepicon-action-left'></span>
                </button>";
            }
            $html .= '</td><td style="vertical-align:middle" nowrap="nowrap" width="1%" class="paginationActionButtons">';
            $html .= ' <div class="pageNumbers">(' . $start . ' - ' . $end . ' of ' . $total_rows . ')</div>';
            $html .= '</td><td nowrap="nowrap" align="right" class="paginationActionButtons" width="1%">';
            if ($next_offset < $total_rows) {
                $html .= "<button type='button' id='listViewNextButton_top' name='listViewNextButton' title='Next' class='list-view-pagination-button' onclick='changeReportPage(\"" . $this->id . '",' . $next_offset . ',"' . $group_value . '","' . $tableIdentifier . "\")'>
                       <span class='suitepicon suitepicon-action-right'></span>
                    </button>
                     <button type='button' id='listViewEndButton_top' name='listViewEndButton' title='End' class='list-view-pagination-button' onclick='changeReportPage(\"" . $this->id . '",' . $last_offset . ',"' . $group_value . '","' . $tableIdentifier . "\")'>
                        <span class='suitepicon suitepicon-action-last'></span>
                    </button>";
            } else {
                $html .= "<button type='button' id='listViewNextButton_top' name='listViewNextButton' title='Next' class='list-view-pagination-button'  disabled='disabled'>
                        <span class='suitepicon suitepicon-action-right'></span>
                    </button>
                     <button type='button' id='listViewEndButton_top' name='listViewEndButton' title='End' class='list-view-pagination-button'  disabled='disabled'>
                       <span class='suitepicon suitepicon-action-last'></span>
                    </button>";
            }

            $html .= '</td><td nowrap="nowrap" width="4px" class="paginationActionButtons"></td>
                       </table>
                      </td>';

            $html .= '</tr>';
        }

        $html .= '</thead>';
        $html .= '<tbody>';

        if ($fieldCount) {
            if ($offset >= 0) {
                $result = $this->db->limitQuery($report_sql, $offset, $max_rows);
            } else {
                $result = $this->db->query($report_sql);
            }
        }

        $row_class = 'oddListRowS1';


        $totals = array();
        while ($fieldCount && $row = $this->db->fetchByAssoc($result)) {
            $html .= "<tr class='" . $row_class . "' height='20'>";

            foreach ($fields as $name => $att) {
                if ($att['display']) {
                    $html .= "<td class='' valign='top' align='left'>";
                    if ($att['link'] && $links) {
                        $html .= "<a href='" . $sugar_config['site_url'] . "/index.php?module=" . $att['module'] . "&action=DetailView&record=" . $row[$att['alias'] . '_id'] . "'>";
                    }

                    $currency_id = isset($row[$att['alias'] . '_currency_id']) ? $row[$att['alias'] . '_currency_id'] : '';

                    if ($att['function'] == 'COUNT' || !empty($att['format'])) {
                        $html .= $row[$name];
                    } else {
                        // Make sure the `{$module}_id` key exists on $row, to prevent PHP notices.
                        if (isset($row[$att['alias'] . '_id'])) {
                            $params = array('record_id' => $row[$att['alias'] . '_id']);
                        } else {
                            $params = [];
                        }
                        $html .= trim(getModuleField(
                            $att['module'],
                            $att['field'],
                            $att['field'],
                            'DetailView',
                            $row[$name],
                            '',
                            $currency_id,
                            $params
                        ));
                    }

                    if ($att['total']) {
                        $totals[$name][] = $row[$name];
                    }
                    if ($att['link'] && $links) {
                        $html .= "</a>";
                    }
                    $html .= "</td>";
                }
            }
            $html .= "</tr>";

            $row_class = $row_class == 'oddListRowS1' ? 'evenListRowS1' : 'oddListRowS1';
        }
        $html .= "</tbody>";

        $html .= $this->getTotalHTML($fields, $totals);

        $html .= '</table>';

        $html .= '</div>';

        $currentTheme = SugarThemeRegistry::current();

        if (empty($_REQUEST['action']) || $_REQUEST['action'] !== 'DownloadPDF') {
            $html .= "    <script type=\"text/javascript\">
                            groupedReportToggler = {

                                toggleList: function(elem) {
                                    $(elem).closest('table.list').find('thead, tbody').each(function(i, e){
                                        if(i>1) {
                                            $(e).toggle();
                                        }
                                    });
                                    if($(elem).find('img').first().attr('src') == '" . $currentTheme->getImageURL('basic_search.gif') . "') {
                                        $(elem).find('img').first().attr('src', '" . $currentTheme->getImageURL('advanced_search.gif') . "');
                                    }
                                    else {
                                        $(elem).find('img').first().attr('src', '" . $currentTheme->getImageURL('basic_search.gif') . "');
                                    }
                                }

                            };
                        </script>";
        }

        return $html;
    }

    private function getModuleFieldByGroupValue($beanList, $group_value)
    {
        global $app_list_strings;
        $moduleFieldByGroupValues = array();

        $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '" . $this->id . "' AND group_display = 1 AND deleted = 0 ORDER BY field_order ASC";
        $result = $this->db->limitQuery($sql, 0, 1);
        while ($row = $this->db->fetchByAssoc($result)) {
            $field = BeanFactory::newBean('AOR_Fields');
            $field->retrieve($row['id']);

            if ($field->field_function != 'COUNT' || $field->format != '') {
                // Fix grouping on assignment displays ID and not name #5427
                $report_bean = BeanFactory::getBean($this->report_module);
                $field_def = $report_bean->field_defs[$field->field];
                if ($field_def['type'] == 'relate' && isset($field_def['id_name'])) {
                    $related_bean = BeanFactory::getBean($field_def['module']);
                    $related_bean->retrieve($group_value);
                    $moduleFieldByGroupValues[] = ($related_bean instanceof Person) ? $related_bean->full_name : $related_bean->name;
                } elseif ($field_def['type'] == 'enum') {
                    $moduleFieldByGroupValues[] = $app_list_strings[$field_def['options']][$group_value];
                } else {
                     $moduleFieldByGroupValues[] = $group_value;
                }
                continue;
                // End
            }

            $path = unserialize(base64_decode($field->module_path));

            $field_bean = new $beanList[$this->report_module]();

            $field_module = $this->report_module;
            $field_alias = $field_bean->table_name;
            if ($path[0] != $this->report_module) {
                foreach ($path as $rel) {
                    if (empty($rel)) {
                        continue;
                    }
                    $field_module = getRelatedModule($field_module, $rel);
                    $field_alias = $field_alias . ':' . $rel;
                }
            }

            $currency_id = isset($row[$field_alias . '_currency_id']) ? $row[$field_alias . '_currency_id'] : '';
            $moduleFieldByGroupValues[] = getModuleField(
                $this->report_module,
                $field->field,
                $field->field,
                'DetailView',
                $group_value,
                '',
                $currency_id
            );
        }

        $moduleFieldByGroupValue = implode(', ', $moduleFieldByGroupValues);

        return $moduleFieldByGroupValue;
    }

    public function getTotalHTML($fields, $totals)
    {
        global $app_list_strings;

        $currency = BeanFactory::newBean('Currencies');
        $currency->retrieve($GLOBALS['current_user']->getPreference('currency'));

        $showTotal = false;
        foreach ($fields as $label => $field) {
            if (!$field['display']) {
                continue;
            }

            if (!empty($field['total'])) {
                $showTotal = true;
            }
        }

        if (!$showTotal) {
            return '';
        }

        $html = "<tr class='totalReportRow oddListRowS1'>";
        foreach ($fields as $label => $field) {
            if (!$field['display']) {
                continue;
            }
            if ($field['total'] && isset($totals[$label])) {
                $type = $field['total'];
                $total = $this->calculateTotal($type, $totals[$label]);
                $params = isset($field['params']) ? $field['params'] : [];
                switch ($type) {
                    case 'SUM':
                    case 'AVG':
                        $total = getModuleField(
                            $field['module'],
                            $field['field'],
                            $field['field'],
                            'DetailView',
                            $total,
                            '',
                            $currency->id,
                            $params
                        );
                        break;
                    case 'COUNT':
                    default:
                        break;
                }
                $html .= '<td>' . $total . '</td>';
            } else {
                $html .= '<td></td>';
            }
        }
        $html .= '</tr>';

        return $html;
    }

    public function calculateTotal($type, $totals)
    {
        $totalCount = is_countable($totals) ? count($totals) : 0;

        switch ($type) {
            case 'SUM':
                return array_sum($totals);
            case 'COUNT':
                return $totalCount;
            case 'AVG':
                return array_sum($totals) / $totalCount;
            default:
                return '';
        }
    }

    /**
     * @param string $field
     * @return string
     */
    private function encloseForCSV($field)
    {
        $cleanCSV = new CleanCSV();

        return '"' . $cleanCSV->escapeField($field) . '"';
    }

    public function build_report_csv()
    {

        global $beanList;
        ini_set('zlib.output_compression', 'Off');

        ob_start();
        require_once('include/export_utils.php');

        $delimiter = getDelimiter();
        $csv = '';
        //text/comma-separated-values

        $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '" . $this->id . "' AND deleted = 0 ORDER BY field_order ASC";
        $result = $this->db->query($sql);

        $fields = array();
        $field = null;
        $i = 0;
        while ($row = $this->db->fetchByAssoc($result)) {
            $field = BeanFactory::newBean('AOR_Fields');
            $field->retrieve($row['id']);

            $path = unserialize(base64_decode($field->module_path));
            $field_bean = new $beanList[$this->report_module]();
            $field_module = $this->report_module;
            $field_alias = $field_bean->table_name;

            if ($path[0] != $this->report_module) {
                foreach ($path as $rel) {
                    if (empty($rel)) {
                        continue;
                    }
                    $field_module = getRelatedModule($field_module, $rel);
                    $field_alias = $field_alias . ':' . $rel;
                }
            }
            $label = str_replace(' ', '_', (string) $field->label) . $i;
            $fields[$label]['field'] = $field->field;
            $fields[$label]['display'] = $field->display;
            $fields[$label]['function'] = $field->field_function;
            $fields[$label]['module'] = $field_module;
            $fields[$label]['alias'] = $field_alias;
            $fields[$label]['format'] = $field->format;

            if ($field->display) {
                $csv .= $this->encloseForCSV($field->label);
                $csv .= $delimiter;
            }
            ++$i;
        }

        // Remove last delimiter of the line
        if ($field->display) {
            $csv = substr($csv, 0, strlen($csv) - strlen((string) $delimiter));
        }

        $sql = $this->build_report_query();
        $result = $this->db->query($sql);

        while ($row = $this->db->fetchByAssoc($result, false)) {
            $csv .= "\r\n";
            foreach ($fields as $name => $att) {
                $currency_id = isset($row[$att['alias'] . '_currency_id']) ? $row[$att['alias'] . '_currency_id'] : '';
                if ($att['display']) {
                    if ($att['function'] != '' || $att['format'] != '') {
                        $csv .= $this->encloseForCSV($row[$name]);
                    } else {
                        $t = getModuleField(
                            $att['module'],
                            $att['field'],
                            $att['field'],
                            'DetailView',
                            $row[$name],
                            '',
                            $currency_id
                        );
                        if (false !== strpos((string) $t, 'checkbox')) {
                            $csv .= $row[$name];
                        } else {
                            $csv .= $this->encloseForCSV(trim(strip_tags($t)));
                        }
                    }
                    $csv .= $delimiter;
                }
            }
            // Remove last delimiter of the line
            $csv = substr($csv, 0, strlen($csv) - strlen((string) $delimiter));
        }

        ob_clean();
        printCSV($csv, $this->name);
        sugar_cleanup(true);
    }


    public function build_report_query($group_value = '', $extra = array())
    {
        global $beanList;

        $module = new $beanList[$this->report_module]();

        $query = '';
        $query_array = array();

        //Check if the user has access to the target module
        if (!(ACLController::checkAccess($this->report_module, 'list', true))) {
            return false;
        }

        $query_array = $this->build_report_query_select($query_array, $group_value);
        if (isset($extra['where']) && $extra['where']) {
            $query_array['where'][] = implode(' AND ', $extra['where']) . ' AND ';
        }
        $query_array = $this->build_report_query_where($query_array);

        if (!isset($query_array['select'])) {
            LoggerManager::getLogger()->warn('Trying to build report query without database select definition.');
        } else {
            foreach ($query_array['select'] as $select) {
                $query .= ($query == '' ? 'SELECT ' : ', ') . $select;
            }
        }

        if (empty($query_array['group_by'])) {
            foreach ($query_array['id_select'] as $select) {
                if (!$query) {
                    $query = 'SELECT ' . $select;
                } else {
                    $query .= ', ' . $select;
                }
            }
        }

        $query .= ' FROM ' . $this->db->quoteIdentifier($module->table_name) . ' ';

        if (isset($query_array['join'])) {
            foreach ($query_array['join'] as $join) {
                $query .= $join;
            }
        }
        if (isset($query_array['where'])) {
            $query_where = '';
            foreach ($query_array['where'] as $where) {
                $query_where .= ($query_where == '' ? 'WHERE ' : ' ') . $where;
            }

            $query_where = $this->queryWhereRepair($query_where);

            $query .= ' ' . $query_where;
        }

        if (isset($query_array['group_by'])) {
            $query_group_by = '';
            foreach ($query_array['group_by'] as $group_by) {
                $query_group_by .= ($query_group_by == '' ? 'GROUP BY ' : ', ') . $group_by;
            }
            if (isset($query_array['second_group_by']) && $query_group_by != '') {
                foreach ($query_array['second_group_by'] as $group_by) {
                    $query_group_by .= ', ' . $group_by;
                }
            }
            $query .= ' ' . $query_group_by;
        }

        if (isset($query_array['sort_by'])) {
            $query_sort_by = '';
            foreach ($query_array['sort_by'] as $sort_by) {
                $query_sort_by .= ($query_sort_by == '' ? 'ORDER BY ' : ', ') . $sort_by;
            }
            $query .= ' ' . $query_sort_by;
        }

        return $query;
    }

    private function queryWhereRepair($query_where)
    {

        // remove empty parenthesis and fix query syntax

        $safe = 0;
        $query_where_clean = '';
        while ($query_where_clean != $query_where) {
            $query_where_clean = $query_where;
            $query_where = preg_replace('/\b(AND|OR)\s*\(\s*\)|[^\w+\s*]\(\s*\)/i', '', (string) $query_where_clean);
            $safe++;
            if ($safe > 100) {
                $GLOBALS['log']->fatal('Invalid report query conditions');
                break;
            }
        }

        return $query_where;
    }

    public function build_report_query_select($query = array(), $group_value = '')
    {
        global $beanList, $timedate;

        if ($beanList[$this->report_module]) {
            $module = new $beanList[$this->report_module]();

            $query['id_select'][$module->table_name] = $this->db->quoteIdentifier($module->table_name) . ".id AS '" . $module->table_name . "_id'";
            $query['id_select_group'][$module->table_name] = $this->db->quoteIdentifier($module->table_name) . ".id";

            $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '" . $this->id . "' AND deleted = 0 ORDER BY field_order ASC";

            $result = $this->db->query($sql);
            $i = 0;
            while ($row = $this->db->fetchByAssoc($result)) {
                $field = BeanFactory::newBean('AOR_Fields');
                $field->retrieve($row['id']);

                $field->label = str_replace(' ', '_', (string) $field->label) . $i;

                $path = unserialize(base64_decode($field->module_path));

                $field_module = $module;
                $table_alias = $field_module->table_name;
                $oldAlias = $table_alias;
                if (!empty($path[0]) && $path[0] != $module->module_dir) {
                    foreach ($path as $rel) {
                        $new_field_module = new $beanList[getRelatedModule($field_module->module_dir, $rel)];
                        $oldAlias = $table_alias;
                        $table_alias = $table_alias . ":" . $rel;
                        $query =
                            $this->build_report_query_join(
                                $rel,
                                $table_alias,
                                $oldAlias,
                                $field_module,
                                'relationship',
                                $query,
                                $new_field_module
                            );
                        $field_module = $new_field_module;
                    }
                }
                $data = $field_module->field_defs[$field->field];

                if ($data['type'] == 'relate' && isset($data['id_name'])) {
                    $field->field = $data['id_name'];
                    $data_new = $field_module->field_defs[$field->field];
                    if (isset($data_new['source']) && $data_new['source'] == 'non-db' && $data_new['type'] != 'link' && isset($data['link'])) {
                        $data_new['type'] = 'link';
                        $data_new['relationship'] = $data['link'];
                    }
                    $data = $data_new;
                }

                if ($data['type'] == 'link' && $data['source'] == 'non-db') {
                    $new_field_module = new $beanList[getRelatedModule(
                        $field_module->module_dir,
                        $data['relationship']
                    )];
                    $table_alias = $data['relationship'];
                    $query = $this->build_report_query_join(
                        $data['relationship'],
                        $table_alias,
                        $oldAlias,
                        $field_module,
                        'relationship',
                        $query,
                        $new_field_module
                    );
                    $field_module = $new_field_module;
                    $field->field = 'id';
                }

                if ($data['type'] == 'currency' && isset($field_module->field_defs['currency_id']) && !stripos((string) $field->field,'_USD')) {
                    if ((isset($field_module->field_defs['currency_id']['source']) && $field_module->field_defs['currency_id']['source'] == 'custom_fields')) {
                        $query['select'][$table_alias . '_currency_id'] = $this->db->quoteIdentifier($table_alias . '_cstm') . ".currency_id AS '" . $table_alias . "_currency_id'";
                        $query['second_group_by'][] = $this->db->quoteIdentifier($table_alias . '_cstm') . ".currency_id";
                    } else {
                        $query['select'][$table_alias . '_currency_id'] = $this->db->quoteIdentifier($table_alias) . ".currency_id AS '" . $table_alias . "_currency_id'";
                        $query['second_group_by'][] = $this->db->quoteIdentifier($table_alias) . ".currency_id";
                    }
                }

                if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                    $select_field = $this->db->quoteIdentifier($table_alias . '_cstm') . '.' . $field->field;
                    $query = $this->build_report_query_join(
                        $table_alias . '_cstm',
                        $table_alias . '_cstm',
                        $table_alias,
                        $field_module,
                        'custom',
                        $query
                    );
                } else {
                    $select_field = $this->db->quoteIdentifier($table_alias) . '.' . $field->field;
                }
                $select_field_db = $select_field;

                if ($field->format && in_array($data['type'], array('date', 'datetime', 'datetimecombo'))) {
                    if (in_array($data['type'], array('datetime', 'datetimecombo'))) {
                        $select_field = $this->db->convert($select_field, 'add_tz_offset');
                    }
                    $select_field = $this->db->convert(
                        $select_field,
                        'date_format',
                        array($timedate->getCalFormat($field->format))
                    );
                }

                if ($field->link && isset($query['id_select'][$table_alias])) {
                    $query['select'][] = $query['id_select'][$table_alias];
                    $query['second_group_by'][] = $query['id_select_group'][$table_alias];
                    unset($query['id_select'][$table_alias]);
                }

                if ($field->group_by == 1) {
                    $query['group_by'][] = $select_field;
                } elseif ($field->field_function != null) {
                    $select_field = $field->field_function . '(' . $select_field . ')';
                } else {
                    $query['second_group_by'][] = $select_field;
                }

                if ($field->sort_by != '') {
                    // If the field is a date, sort by the natural date and not the user-formatted date
                    if ($data['type'] == 'date' || $data['type'] == 'datetime') {
                        $query['sort_by'][] = $select_field_db . " " . $field->sort_by;
                    } else {
                        $query['sort_by'][] = $select_field . " " . $field->sort_by;
                    }
                }

                $query['select'][] = $select_field . " AS '" . $field->label . "'";

                if ($field->group_display == 1 && $group_value) {
                    if ($group_value === '_empty') {
                        $query['where'][] = '(' . $select_field . " = '' OR " . $select_field . ' IS NULL) AND ';
                    } else {
                        $query['where'][] = $select_field . " = '" . $group_value . "' AND ";
                    }
                }

                ++$i;
            }
        }

        return $query;
    }

    public function build_report_query_join(
        $name,
        $alias,
        $parentAlias,
        SugarBean $module,
        $type,
        $query = array(),
        SugarBean $rel_module = null
    ) {

        // Alias to keep lines short
        $db = $this->db;
        $params = [];

        if (!isset($query['join'][$alias])) {
            switch ($type) {
                case 'custom':
                    $customTable = $module->get_custom_table_name();
                    $query['join'][$alias] =
                        'LEFT JOIN ' .
                        $db->quoteIdentifier($customTable) .' '. $db->quoteIdentifier($alias) .
                        ' ON ' .
                        $db->quoteIdentifier($parentAlias) . '.id = ' . $db->quoteIdentifier($name) . '.id_c ';
                    break;

                case 'relationship':
                    if ($module->load_relationship($name)) {
                        $params['join_type'] = 'LEFT JOIN';
                        if ($module->$name->relationship_type != 'one-to-many') {
                            if ($module->$name->getSide() == REL_LHS) {
                                $params['right_join_table_alias'] = $db->quoteIdentifier($alias);
                                $params['join_table_alias'] = $db->quoteIdentifier($alias);
                                $params['left_join_table_alias'] = $db->quoteIdentifier($parentAlias);
                            } else {
                                $params['right_join_table_alias'] = $db->quoteIdentifier($parentAlias);
                                $params['join_table_alias'] = $db->quoteIdentifier($alias);
                                $params['left_join_table_alias'] = $db->quoteIdentifier($alias);
                            }
                        } else {
                            $params['right_join_table_alias'] = $db->quoteIdentifier($parentAlias);
                            $params['join_table_alias'] = $db->quoteIdentifier($alias);
                            $params['left_join_table_alias'] = $db->quoteIdentifier($parentAlias);
                        }
                        $linkAlias = $parentAlias . "|" . $alias;
                        $params['join_table_link_alias'] = $db->quoteIdentifier($linkAlias);
                        $join = $module->$name->getJoin($params, true);
                        $query['join'][$alias] = $join['join'];
                        if ($rel_module != null) {
                            $query['join'][$alias] .= $this->build_report_access_query(
                                $rel_module,
                                $db->quoteIdentifier($alias)
                            );
                        }
                        $query['id_select'][$alias] = $join['select'] . " AS '" . $alias . "_id'";
                        $query['id_select_group'][$alias] = $join['select'];
                    }
                    break;
                default:
                    break;

            }
        }

        return $query;
    }

    public function build_report_access_query(SugarBean $module, $alias)
    {
        $tempTableName = $module->table_name;
        $module->table_name = $alias;
        $where = $module->buildAccessWhere('list');
        if (!empty($where)) {
            $where = ' AND ' . $where;
        }
        $module->table_name = $tempTableName;

        return $where;
    }

    /**
     * @param array $query
     * @return array
     */
    public function build_report_query_where($query = array())
    {
        global $beanList, $app_list_strings, $sugar_config, $timedate;

        $aor_sql_operator_list = [];
        $aor_sql_operator_list['Equal_To'] = '=';
        $aor_sql_operator_list['Not_Equal_To'] = '!=';
        $aor_sql_operator_list['Greater_Than'] = '>';
        $aor_sql_operator_list['Less_Than'] = '<';
        $aor_sql_operator_list['Greater_Than_or_Equal_To'] = '>=';
        $aor_sql_operator_list['Less_Than_or_Equal_To'] = '<=';
        $aor_sql_operator_list['Contains'] = 'LIKE';
        $aor_sql_operator_list['Starts_With'] = 'LIKE';
        $aor_sql_operator_list['Ends_With'] = 'LIKE';

        $closure = false;
        if (!empty($query['where'])) {
            $query['where'][] = '(';
            $closure = true;
        }

        if ($beanList[$this->report_module]) {
            $module = new $beanList[$this->report_module]();

            $sql = "SELECT id FROM aor_conditions WHERE aor_report_id = '" . $this->id . "' AND deleted = 0 ORDER BY condition_order ASC";
            $result = $this->db->query($sql);

            $tiltLogicOp = true;

            while ($row = $this->db->fetchByAssoc($result)) {
                $condition = BeanFactory::newBean('AOR_Conditions');
                $condition->retrieve($row['id']);

                $path = unserialize(base64_decode($condition->module_path));

                $condition_module = $module;
                $table_alias = $condition_module->table_name;
                $oldAlias = $table_alias;
                if (!empty($path[0]) && $path[0] != $module->module_dir) {
                    foreach ($path as $rel) {
                        if (empty($rel)) {
                            continue;
                        }
                        // Bug: Prevents relationships from loading.
                        $new_condition_module = new $beanList[getRelatedModule($condition_module->module_dir, $rel)];
                        //Check if the user has access to the related module
                        if (!(ACLController::checkAccess($new_condition_module->module_name, 'list', true))) {
                            return false;
                        }
                        $oldAlias = $table_alias;
                        $table_alias = $table_alias . ":" . $rel;
                        $query = $this->build_report_query_join(
                            $rel,
                            $table_alias,
                            $oldAlias,
                            $condition_module,
                            'relationship',
                            $query,
                            $new_condition_module
                        );
                        $condition_module = $new_condition_module;
                    }
                }
                if (isset($aor_sql_operator_list[$condition->operator])) {
                    $where_set = false;

                    $data = $condition_module->field_defs[$condition->field];

                    if ($data['type'] == 'relate' && isset($data['id_name'])) {
                        $condition->field = $data['id_name'];
                        $data_new = $condition_module->field_defs[$condition->field];
                        if (!empty($data_new['source']) && $data_new['source'] == 'non-db' && $data_new['type'] != 'link' && isset($data['link'])) {
                            $data_new['type'] = 'link';
                            $data_new['relationship'] = $data['link'];
                        }
                        $data = $data_new;
                    }

                    if ($data['type'] == 'link' && $data['source'] == 'non-db') {
                        $new_field_module = new $beanList[getRelatedModule(
                            $condition_module->module_dir,
                            $data['relationship']
                        )];
                        $table_alias = $data['relationship'];
                        $query = $this->build_report_query_join(
                            $data['relationship'],
                            $table_alias,
                            $oldAlias,
                            $condition_module,
                            'relationship',
                            $query,
                            $new_field_module
                        );
                        $condition_module = $new_field_module;

                        // Debugging: security groups conditions - It's a hack to just get the query working
                        if ($condition_module->module_dir = 'SecurityGroups' && (is_countable($path) ? count($path) : 0) > 1) {
                            $table_alias = $oldAlias . ':' . $rel;
                        }
                        $condition->field = 'id';
                    }
                    if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                        $field = $this->db->quoteIdentifier($table_alias . '_cstm') . '.' . $condition->field;
                        $query = $this->build_report_query_join(
                            $table_alias . '_cstm',
                            $table_alias . '_cstm',
                            $table_alias,
                            $condition_module,
                            'custom',
                            $query
                        );
                    } else {
                        $field = $this->db->quoteIdentifier($table_alias) . '.' . $condition->field;
                    }

                    if (!empty($this->user_parameters[$condition->id]) && $condition->parameter) {
                        $condParam = $this->user_parameters[$condition->id];
                        $condition->value = $condParam['value'];
                        $condition->operator = $condParam['operator'];
                        $condition->value_type = $condParam['type'];
                    }

                    $value = '';

                    switch ($condition->value_type) {
                        case 'Field':
                            $data = $condition_module->field_defs[$condition->value];

                            if ($data['type'] == 'relate' && isset($data['id_name'])) {
                                $condition->value = $data['id_name'];
                                $data_new = $condition_module->field_defs[$condition->value];
                                if ($data_new['source'] == 'non-db' && $data_new['type'] != 'link' && isset($data['link'])) {
                                    $data_new['type'] = 'link';
                                    $data_new['relationship'] = $data['link'];
                                }
                                $data = $data_new;
                            }

                            if ($data['type'] == 'link' && $data['source'] == 'non-db') {
                                $new_field_module = new $beanList[getRelatedModule(
                                    $condition_module->module_dir,
                                    $data['relationship']
                                )];
                                $table_alias = $data['relationship'];
                                $query = $this->build_report_query_join(
                                    $data['relationship'],
                                    $table_alias,
                                    $oldAlias,
                                    $condition_module,
                                    'relationship',
                                    $query,
                                    $new_field_module
                                );
                                $condition_module = $new_field_module;
                                $condition->field = 'id';
                            }
                            if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                                $value = $condition_module->table_name . '_cstm.' . $condition->value;
                                $query = $this->build_report_query_join(
                                    $condition_module->table_name . '_cstm',
                                    $table_alias . '_cstm',
                                    $table_alias,
                                    $condition_module,
                                    'custom',
                                    $query
                                );
                            } else {
                                $value = ($table_alias ? $this->db->quoteIdentifier($table_alias) : $condition_module->table_name) . '.' . $condition->value;
                            }
                            break;

                        case 'Date':
                            $params = unserialize(base64_decode($condition->value));

                            // Fix for issue #1272 - AOR_Report module cannot update Date type parameter.
                            if ($params == false) {
                                $params = $condition->value;
                            }

                            if ($params[0] == 'now') {
                                if ($sugar_config['dbconfig']['db_type'] == 'mssql') {
                                    $value = 'GetDate()';
                                } else {
                                    $value = 'NOW()';
                                }
                            } else {
                                if ($params[0] == 'today') {
                                    if ($sugar_config['dbconfig']['db_type'] == 'mssql') {
                                        //$field =
                                        $value = 'CAST(GETDATE() AS DATE)';
                                    } else {
                                        $field = 'DATE(' . $field . ')';
                                        $value = 'Curdate()';
                                    }
                                } else {
                                    $data = $condition_module->field_defs[$params[0]];
                                    if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                                        $value = $condition_module->table_name . '_cstm.' . $params[0];
                                        $query = $this->build_report_query_join(
                                            $condition_module->table_name . '_cstm',
                                            $table_alias . '_cstm',
                                            $table_alias,
                                            $condition_module,
                                            'custom',
                                            $query
                                        );
                                    } else {
                                        $value = $condition_module->table_name . '.' . $params[0];
                                    }
                                }
                            }

                            if ($params[1] !== 'now') {
                                switch ($params[3]) {
                                    case 'business_hours';
                                        if ($params[0] === 'now') {
                                            $businessHours = BeanFactory::getBean('AOBH_BusinessHours');
                                            $amount = $params[2];

                                            if ($params[1] !== 'plus') {
                                                $amount = 0 - $amount;
                                            }
                                            $value = $businessHours->addBusinessHours($amount);
                                            $value = "'" . $timedate->asDb($value) . "'";
                                            break;
                                        }
                                        $params[3] = 'hour';
                                    default:
                                        if ($sugar_config['dbconfig']['db_type'] === 'mssql') {
                                            $value = "DATEADD(" . $params[3] . ",  " . $app_list_strings['aor_date_operator'][$params[1]] . " $params[2], $value)";
                                        } else {
                                            $value = "DATE_ADD($value, INTERVAL " . $app_list_strings['aor_date_operator'][$params[1]] . " $params[2] " . $params[3] . ")";
                                        }
                                        break;
                                }
                            }
                            break;

                        case 'Multi':
                            $sep = ' AND ';
                            if ($condition->operator == 'Equal_To') {
                                $sep = ' OR ';
                            }
                            $multi_values = unencodeMultienum($condition->value);
                            if (!empty($multi_values)) {
                                $value = '(';
                                if ($data['type'] == 'multienum') {
                                    $multi_operator =  $condition->operator == 'Equal_To' ? 'LIKE' : 'NOT LIKE';
                                    foreach ($multi_values as $multi_value) {
                                        if ($value != '(') {
                                            $value .= $sep;
                                        }
                                        $value .= $field . ' ' . $multi_operator . ' \'%' . $this->db->quote(encodeMultienumValue(array($multi_value))) . '%\'';
                                    }
                                } else {
                                    foreach ($multi_values as $multi_value) {
                                        if ($value != '(') {
                                            $value .= $sep;
                                        }
                                        $value .= $field . ' ' . $aor_sql_operator_list[$condition->operator] . " '" . $multi_value . "'";
                                    }
                                }
                                $value .= ')';
                                $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $value;
                            }
                            $where_set = true;
                            break;
                        case "Period":
                            if (array_key_exists($condition->value, $app_list_strings['date_time_period_list'])) {
                                $params = $condition->value;
                            } else {
                                $params = base64_decode($condition->value);
                            }
                            $value = '"' . getPeriodDate($params, $data['type'])->format('Y-m-d H:i:s') . '"';
                            break;
                        case "CurrentUserID":
                            global $current_user;
                            $value = '"' . $current_user->id . '"';
                            break;
                        case 'Value':
                            $utc = new DateTimeZone("UTC");
                            $dateTime = DateTime::createFromFormat('Y-m-d H:i:s', $condition->value, $utc);

                            if ($condition->operator === 'Equal_To') {
                                if ($dateTime !== false) {
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $equal_query = "( $field  BETWEEN '" . $this->db->quote($condition->value) . "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $equal_query;
                                } elseif ($dateTime === false && $data['type'] === 'datetime') { // check for incorrectly converted dateTime
                                    $dateTime = convertToDateTime($condition->value);

                                    $query_date = $dateTime->format('Y-m-d H:i:s');
                                    $equal_query = "( $field  BETWEEN '" . $this->db->quote($query_date);
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $equal_query .= "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $equal_query;
                                } else {
                                    $value = "'" . $this->db->quote($condition->value) . "'";
                                    break;
                                }
                                $where_set = true;
                            } elseif ($condition->operator === 'Not_Equal_To') {
                                if ($dateTime !== false) {
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $not_equal_query = "( $field NOT BETWEEN '" . $this->db->quote($condition->value) . "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $not_equal_query;
                                } elseif ($dateTime === false && $data['type'] === 'datetime') { // check for incorrectly converted dateTime
                                    $dateTime = convertToDateTime($condition->value);

                                    $query_date = $dateTime->format('Y-m-d H:i:s');
                                    $not_equal_query = "( $field NOT BETWEEN '" . $this->db->quote($query_date);
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $not_equal_query .= "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $not_equal_query;
                                } else {
                                    $value = "'" . $this->db->quote($condition->value) . "'";
                                    break;
                                }
                                $where_set = true;
                            } elseif ($condition->operator === 'Greater_Than') {
                                if ($dateTime !== false) {
                                    $greater_than_query = "( $field > '" . $this->db->quote($condition->value) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $greater_than_query;
                                } elseif ($dateTime === false && $data['type'] === 'datetime') { // check for incorrectly converted dateTime
                                    $dateTime = convertToDateTime($condition->value);

                                    $query_date = $dateTime->format('Y-m-d H:i:s');
                                    $greater_than_query = "( $field > '" . $this->db->quote($query_date) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $greater_than_query;
                                } else {
                                    $value = "'" . $this->db->quote($condition->value) . "'";
                                    break;
                                }
                                $where_set = true;
                            } elseif ($condition->operator === 'Less_Than') {
                                if ($dateTime !== false) {
                                    $less_than_query = "( $field < '" . $this->db->quote($condition->value) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $less_than_query;
                                } elseif ($dateTime === false && $data['type'] === 'datetime') { // check for incorrectly converted dateTime
                                    $dateTime = convertToDateTime($condition->value);

                                    $query_date = $dateTime->format('Y-m-d H:i:s');
                                    $less_than_query = "( $field < '" . $this->db->quote($query_date) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $less_than_query;
                                } else {
                                    $value = "'" . $this->db->quote($condition->value) . "'";
                                    break;
                                }
                                $where_set = true;
                            } elseif ($condition->operator === 'Greater_Than_or_Equal_To') {
                                if ($dateTime !== false) {
                                    $equal_greater_than_query = "( $field > '" . $this->db->quote($condition->value) . "'";
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $equal_greater_than_query .= " OR $field  BETWEEN '" . $this->db->quote($condition->value) . "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $equal_greater_than_query;
                                } elseif ($dateTime === false && $data['type'] === 'datetime') { // check for incorrectly converted dateTime
                                    $dateTime = convertToDateTime($condition->value);

                                    $query_date = $dateTime->format('Y-m-d H:i:s');
                                    $equal_greater_than_query = "( $field > '" . $this->db->quote($query_date) . "'";
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $equal_greater_than_query .= " OR $field  BETWEEN '" . $this->db->quote($query_date) . "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $equal_greater_than_query;
                                } else {
                                    $value = "'" . $this->db->quote($condition->value) . "'";
                                    break;
                                }
                                $where_set = true;
                            } elseif ($condition->operator === 'Less_Than_or_Equal_To') {
                                if ($dateTime !== false) {
                                    $equal_less_than_query = "( $field < '" . $this->db->quote($condition->value) . "'";
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $equal_less_than_query .= " OR $field  BETWEEN '" . $this->db->quote($condition->value) . "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $equal_less_than_query;
                                } elseif ($dateTime === false && $data['type'] === 'datetime') { // check for incorrectly converted dateTime
                                    $dateTime = convertToDateTime($condition->value);

                                    $query_date = $dateTime->format('Y-m-d H:i:s');
                                    $equal_less_than_query = "( $field < '" . $this->db->quote($query_date) . "'";
                                    $day_ahead = $dateTime->modify('+1 day');
                                    $equal_less_than_query .= " OR $field  BETWEEN '" . $this->db->quote($query_date) . "' AND '" . $this->db->quote($day_ahead->format('Y-m-d H:i:s')) . "' ) ";
                                    $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $equal_less_than_query;
                                } else {
                                    $value = "'" . $this->db->quote($condition->value) . "'";
                                    break;
                                }
                                $where_set = true;
                            } else {
                                $value = "'" . $this->db->quote($condition->value) . "'";
                            }
                            break;
                        default:
                            $value = "'" . $this->db->quote($condition->value) . "'";
                            break;
                    }

                    //handle like conditions
                    switch ($condition->operator) {
                        case 'Contains':
                            $value = "CONCAT('%', " . $value . " ,'%')";
                            break;
                        case 'Starts_With':
                            $value = "CONCAT(" . $value . " ,'%')";
                            break;
                        case 'Ends_With':
                            $value = "CONCAT('%', " . $value . ")";
                            break;
                    }

                    if ($condition->value_type == 'Value' && !$condition->value && $condition->operator == 'Equal_To') {
                        if (!isset($value)) {
                            $GLOBALS['log']->warn(
                                $condition->field
                                . ' value is not set, assuming empty string value'
                            );
                            $value = '';
                        }

                        $value = "{$value} OR {$field} IS NULL)";
                        $field = "(" . $field;
                    }

                    if (!$where_set) {
                        if ($condition->value_type == "Period") {
                            if (array_key_exists($condition->value, $app_list_strings['date_time_period_list'])) {
                                $params = $condition->value;
                            } else {
                                $params = base64_decode($condition->value);
                            }
                            $date = getPeriodEndDate($params, $data['type'])->format('Y-m-d H:i:s');
                            $value = "'" . $this->db->quote(getPeriodDate($params, $data['type'])->format('Y-m-d H:i:s')) . "'";

                            $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND '));
                            $tiltLogicOp = false;

                            switch ($aor_sql_operator_list[$condition->operator]) {
                                case "=":
                                    $query['where'][] = $field . " BETWEEN " . $value . " AND " . "'" . $this->db->quote($date) . "'";
                                    break;
                                case "!=":
                                    $query['where'][] = $field . " NOT BETWEEN " . $value . " AND " . "'" . $this->db->quote($date)  . "'";
                                    break;
                                case ">":
                                case "<":
                                case ">=":
                                case "<=":
                                    $query['where'][] = $field . ' ' . $aor_sql_operator_list[$condition->operator] . ' ' . $value;
                                    break;
                            }
                        } else {
                            if (!$where_set) {
                                $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . $field . ' ' . $aor_sql_operator_list[$condition->operator] . ' ' . $value;
                            }
                        }
                    }
                    $tiltLogicOp = false;
                } else {
                    if ($condition->parenthesis) {
                        if ($condition->parenthesis == 'START') {
                            $query['where'][] = ($tiltLogicOp ? '' : ($condition->logic_op ? $condition->logic_op . ' ' : 'AND ')) . '(';
                            $tiltLogicOp = true;
                        } else {
                            $query['where'][] = ')';
                            $tiltLogicOp = false;
                        }
                    } else {
                        $GLOBALS['log']->debug('illegal condition');
                    }
                }
            }

            if (isset($query['where']) && $query['where']) {
                array_unshift($query['where'], '(');
                $query['where'][] = ') AND ';
            }
            $query['where'][] = $module->table_name . ".deleted = 0 " . $this->build_report_access_query(
                $module,
                $module->table_name
            );
        }

        if ($closure) {
            $query['where'][] = ')';
        }

        return $query;
    }
}
