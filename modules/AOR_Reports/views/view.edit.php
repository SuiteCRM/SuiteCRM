<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

require_once 'modules/AOW_WorkFlow/aow_utils.php';
require_once 'modules/AOR_Reports/aor_utils.php';
#[\AllowDynamicProperties]
class AOR_ReportsViewEdit extends ViewEdit
{
    public function __construct()
    {
        parent::__construct();
    }

    public function preDisplay()
    {
        global $app_list_strings;
        echo "<style type='text/css'>";
        //readfile('modules/AOR_Reports/css/edit.css');
        readfile('modules/AOR_Reports/js/jqtree/jqtree.css');
        echo "</style>";
        if (!is_file('cache/jsLanguage/AOR_Fields/' . $GLOBALS['current_language'] . '.js')) {
            require_once('include/language/jsLanguage.php');
            jsLanguage::createModuleStringsCache('AOR_Fields', $GLOBALS['current_language']);
        }
        echo '<script src="cache/jsLanguage/AOR_Fields/'. $GLOBALS['current_language'] . '.js"></script>';

        if (!is_file('cache/jsLanguage/AOR_Conditions/' . $GLOBALS['current_language'] . '.js')) {
            require_once('include/language/jsLanguage.php');
            jsLanguage::createModuleStringsCache('AOR_Conditions', $GLOBALS['current_language']);
        }
        echo '<script src="cache/jsLanguage/AOR_Conditions/'. $GLOBALS['current_language'] . '.js"></script>';

        echo "<script>";
        echo "sort_by_values = \"".trim(preg_replace('/\s+/', ' ', (string) get_select_options_with_id($app_list_strings['aor_sort_operator'], '')))."\";";
        echo "total_values = \"".trim(preg_replace('/\s+/', ' ', (string) get_select_options_with_id($app_list_strings['aor_total_options'], '')))."\";";
        echo "format_values = \"".trim(preg_replace('/\s+/', ' ', (string) get_select_options_with_id($app_list_strings['aor_format_options'], '')))."\";";
        echo "</script>";

        $fields = $this->getFieldLines();
        echo "<script>var fieldLines = ".json_encode($fields)."</script>";

        $conditions = $this->getConditionLines();
        echo "<script>var conditionLines = ".json_encode($conditions)."</script>";

        $charts = $this->getChartLines();
        echo "<script>var chartLines = ".json_encode($charts).";</script>";

        parent::preDisplay();
    }

    private function getConditionLines()
    {
        if (!$this->bean->id) {
            return array();
        }
        $sql = "SELECT id FROM aor_conditions WHERE aor_report_id = '".$this->bean->id."' AND deleted = 0 ORDER BY condition_order ASC";
        $result = $this->bean->db->query($sql);
        $conditions = array();
        while ($row = $this->bean->db->fetchByAssoc($result)) {
            $condition_name = BeanFactory::newBean('AOR_Conditions');
            $condition_name->retrieve($row['id']);
            if (!$condition_name->parenthesis) {
                $condition_name->module_path = implode(":", unserialize(base64_decode($condition_name->module_path),['allowed_classes' => false]));
            }
            if ($condition_name->value_type == 'Date') {
                $condition_name->value = unserialize(base64_decode($condition_name->value),['allowed_classes' => false]);
            }
            $condition_item = $condition_name->toArray();

            if (!$condition_name->parenthesis) {
                $display = getDisplayForField($condition_name->module_path, $condition_name->field, $this->bean->report_module);
                $condition_item['module_path_display'] = $display['module'];
                $condition_item['field_label'] = $display['field'];
            }
            if (isset($conditions[$condition_item['condition_order']])) {
                $conditions[] = $condition_item;
            } else {
                $conditions[$condition_item['condition_order']] = $condition_item;
            }
        }
        return $conditions;
    }

    private function getFieldLines()
    {
        if (!$this->bean->id) {
            return array();
        }
        $sql = "SELECT id FROM aor_fields WHERE aor_report_id = '".$this->bean->id."' AND deleted = 0 ORDER BY field_order ASC";
        $result = $this->bean->db->query($sql);

        $fields = array();
        while ($row = $this->bean->db->fetchByAssoc($result)) {
            $field_name = BeanFactory::newBean('AOR_Fields');
            $field_name->retrieve($row['id']);
            $field_name->module_path = implode(":", unserialize(base64_decode($field_name->module_path),['allowed_classes' => false]));
            $arr = $field_name->toArray();


            $display = getDisplayForField($field_name->module_path, $field_name->field, $this->bean->report_module);
            $arr['field_type'] = $display['type'];

            $arr['module_path_display'] = $display['module'];
            $arr['field_label'] = $display['field'];
            $fields[] = $arr;
        }
        return $fields;
    }

    private function getChartLines()
    {
        $charts = array();
        if (!$this->bean->id) {
            return array();
        }
        foreach ($this->bean->get_linked_beans('aor_charts', 'AOR_Charts') as $chart) {
            $charts[] = $chart->toArray();
        }
        return $charts;
    }
}
