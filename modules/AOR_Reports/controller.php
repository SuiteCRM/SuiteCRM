<?php
/**
 * Advanced OpenReports, SugarCRM Reporting.
 * @package Advanced OpenReports for SugarCRM
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility <info@salesagility.com>
 */

use SuiteCRM\PDF\Exceptions\PDFException;
use SuiteCRM\PDF\PDFWrapper;

require_once __DIR__ . '/../../modules/AOW_WorkFlow/aow_utils.php';
require_once __DIR__ . '/../../modules/AOR_Reports/aor_utils.php';

/**
 * Class AOR_ReportsController
 */
#[\AllowDynamicProperties]
class AOR_ReportsController extends SugarController
{
    protected function action_getModuleFields()
    {
        if (!empty($_REQUEST['aor_module']) && $_REQUEST['aor_module'] != '') {
            if (isset($_REQUEST['rel_field']) && $_REQUEST['rel_field'] != '') {
                $module = getRelatedModule($_REQUEST['aor_module'], $_REQUEST['rel_field']);
            } else {
                $module = $_REQUEST['aor_module'];
            }
            $val = !empty($_REQUEST['aor_value']) ? $_REQUEST['aor_value'] : '';
            echo getModuleFields($module, $_REQUEST['view'], $val);
        }
        die;
    }

    public function action_getVarDefs()
    {
        if ($_REQUEST['aor_module']) {
            $bean = BeanFactory::getBean($_REQUEST['aor_module']);
            echo json_encode((array)$bean->field_defs[$_REQUEST['aor_request']]);
            die();
        }
    }

    protected function action_getModuleTreeData()
    {
        if (!empty($_REQUEST['aor_module']) && $_REQUEST['aor_module'] != '') {
            ob_start();
            $data = getModuleTreeData($_REQUEST['aor_module']);
            ob_clean();
            echo $data;
        }
        die;
    }

    protected function action_getModuleRelationships()
    {
        if (!empty($_REQUEST['aor_module']) && $_REQUEST['aor_module'] != '') {
            echo getModuleRelationships($_REQUEST['aor_module']);
        }
        die;
    }

    protected function action_changeReportPage()
    {
        $offset = !empty($_REQUEST['offset']) ? $_REQUEST['offset'] : 0;
        $group = !empty($_REQUEST['group']) ? $_REQUEST['group'] : '';
        if (!empty($this->bean->id)) {
            $this->bean->user_parameters = requestToUserParameters();
            echo $this->bean->build_group_report($offset, true, array(), $group);
        }

        die();
    }

    protected function action_getParametersForReport()
    {
        if (empty($_REQUEST['record'])) {
            echo json_encode(array());

            return;
        }
        $report = BeanFactory::getBean('AOR_Reports', $_REQUEST['record']);
        if (!$report) {
            echo json_encode(array());

            return;
        }
        if (empty($report->id)) {
            echo json_encode(array());

            return;
        }
        $conditions = getConditionsAsParameters($report);
        echo json_encode($conditions);
    }

    protected function action_getChartsForReport()
    {
        if (empty($_REQUEST['record'])) {
            echo json_encode(array());

            return;
        }
        $report = BeanFactory::getBean('AOR_Reports', $_REQUEST['record']);
        if (!$report) {
            echo json_encode(array());

            return;
        }
        $charts = array();
        foreach ($report->get_linked_beans('aor_charts', 'AOR_Charts') as $chart) {
            $charts[$chart->id] = $chart->name;
        }
        echo json_encode($charts);
    }

    protected function action_addToProspectList()
    {
        global $beanList;

        require_once('modules/Relationships/Relationship.php');
        require_once('modules/ProspectLists/ProspectList.php');

        $prospectList = BeanFactory::newBean('ProspectLists');
        $prospectList->retrieve($_REQUEST['prospect_id']);

        $module = new $beanList[$this->bean->report_module]();

        $key = Relationship::retrieve_by_modules($this->bean->report_module, 'ProspectLists', $GLOBALS['db']);
        if (!empty($key)) {
            $this->bean->user_parameters = requestToUserParameters();
            $sql = $this->bean->build_report_query();
            $result = $this->bean->db->query($sql);
            $beans = array();
            while ($row = $this->bean->db->fetchByAssoc($result)) {
                if (isset($row[$module->table_name . '_id'])) {
                    $beans[] = $row[$module->table_name . '_id'];
                }
            }
            if (!empty($beans)) {
                foreach ($prospectList->field_defs as $field => $def) {
                    if ($def['type'] == 'link' && !empty($def['relationship']) && $def['relationship'] == $key) {
                        $prospectList->load_relationship($field);
                        $prospectList->$field->add($beans);
                    }
                }
            }
        }
        die;
    }

    protected function action_chartReport()
    {
        $this->bean->build_report_chart(null, AOR_Report::CHART_TYPE_CHARTJS);

        die;
    }

    protected function action_export()
    {
        set_time_limit(0);
        if (!$this->bean->ACLAccess('Export')) {
            SugarApplication::appendErrorMessage(translate('LBL_NO_ACCESS', 'ACL'));
            SugarApplication::redirect("index.php?module=AOR_Reports&action=DetailView&record=" . $this->bean->id);
            sugar_die('');
        }
        $this->bean->user_parameters = requestToUserParameters($this->bean);
        $this->bean->build_report_csv();
        die;
    }

    /**
     * @throws Exception
     * @noinspection PhpMethodParametersCountMismatchInspection
     */
    protected function action_downloadPDF(): void
    {
        if (!$this->bean->ACLAccess('Export')) {
            SugarApplication::appendErrorMessage(translate('LBL_NO_ACCESS', 'ACL'));
            SugarApplication::redirect('index.php?module=AOR_Reports&action=DetailView&record=' . $this->bean->id);
            sugar_die('');
        }

        $companyLogo = explode('?', SugarThemeRegistry::current()->getImageURL('company_logo.png'), 2);
        $reportName = strtoupper($this->bean->name);
        $graphs = $_POST["graphsForPDF"] ?? '';
        $graphHtml = '';
        $chartsPerRow = $this->bean->graphs_per_row;

        if (is_countable($graphs)) {
            $countOfCharts = count($graphs);
        }
        if (!empty($countOfCharts) && $countOfCharts > 0) {
            $graphHtml = "<div class='reportGraphs' style='width:100%; text-align:center;'>";

            $width = (100 / $chartsPerRow);

            $modulusRemainder = $countOfCharts % $chartsPerRow;

            if ($modulusRemainder > 0) {
                $modulusWidth = (100 / $modulusRemainder);
                $itemsWithModulus = $countOfCharts - $modulusRemainder;
            }

            foreach ($graphs as $x => $xValue) {
                if (is_null($itemsWithModulus) || $x < $itemsWithModulus) {
                    $graphHtml .= "<img src='data:image/png;base64, @$xValue' style='width:$width%;' />";
                } else {
                    $graphHtml .= "<img src='data:image/png;base64, @$xValue' style='width:$modulusWidth%;' />";
                }
            }

            $graphHtml .= "</div>";
        }

        $stylesheet = file_get_contents(get_custom_file_if_exists('modules/AOR_Reports/pdf/pdf.css'));
        $footer = '{PAGENO}';
        $head = '<table style="width: 100%; text-align: center;" border="0" cellpadding="2" cellspacing="2">
                <tbody style="text-align: left;">
                <tr style="text-align: left;">
                <td style="text-align: left;">
                <p><img src="' . $companyLogo[0] . '" style="float: left;"/>&nbsp;</p>
                </td>
                </tr>
                <tr style="text-align: left;">
                <td style="text-align: left;">
                <h2>' . $reportName . '</h2>
                </td>
                </tr>
                </tbody>
                </table>';


        if (!empty($graphHtml)) {
            $head .= '<br />' . $graphHtml;
        }

        $this->bean->user_parameters = requestToUserParameters($this->bean);
        $report = $this->bean->build_group_report(-1, false);

        ob_clean();
        try {
            $pdf = PDFWrapper::getPDFEngine();
            $pdf->configurePDF([
                'mode' => 'en',
                'font' => 'DejaVuSansCondensed',
            ]);
            $pdf->addCSS($stylesheet);
            $pdf->writeFooter($footer);
            $pdf->writeHTML($head, 2);
            $pdf->writeHTML($report, 3);
            $pdf->outputPDF($this->bean->name . '.pdf', 'D');
        } catch (PDFException $e) {
            LoggerManager::getLogger()->warn('PDFException: ' . $e->getMessage());
        }
    }

    protected function action_getModuleFunctionField()
    {
        global $app_list_strings;

        $view = $_REQUEST['view'];
        $value = $_REQUEST['aor_value'];
        $module = $_REQUEST['aor_module'];
        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if ($view == 'EditView') {
            echo "<select type='text' style='width:100px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . get_select_options_with_id(
                    $app_list_strings['aor_function_list'],
                    $value
                ) . "</select>";
        } else {
            echo $app_list_strings['aor_function_list'][$value];
        }
        die;
    }


    protected function action_getModuleOperatorField()
    {
        global $app_list_strings, $beanFiles, $beanList;

        if (isset($_REQUEST['rel_field']) && $_REQUEST['rel_field'] != '') {
            $module = getRelatedModule($_REQUEST['aor_module'], $_REQUEST['rel_field']);
        } else {
            $module = $_REQUEST['aor_module'];
        }
        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);

        switch ($vardef['type']) {
            case 'double':
            case 'decimal':
            case 'float':
            case 'currency':
                $valid_opp = array(
                    'Equal_To',
                    'Not_Equal_To',
                    'Greater_Than',
                    'Less_Than',
                    'Greater_Than_or_Equal_To',
                    'Less_Than_or_Equal_To'
                );
                break;
            case 'uint':
            case 'ulong':
            case 'long':
            case 'short':
            case 'tinyint':
            case 'int':
                $valid_opp = array(
                    'Equal_To',
                    'Not_Equal_To',
                    'Greater_Than',
                    'Less_Than',
                    'Greater_Than_or_Equal_To',
                    'Less_Than_or_Equal_To'
                );
                break;
            case 'date':
            case 'datetime':
            case 'datetimecombo':
                $valid_opp = array(
                    'Equal_To',
                    'Not_Equal_To',
                    'Greater_Than',
                    'Less_Than',
                    'Greater_Than_or_Equal_To',
                    'Less_Than_or_Equal_To'
                );
                break;
            case 'enum':
                $valid_opp = array('Equal_To', 'Not_Equal_To');
                break;
            case 'multienum':
                $valid_opp = array('Equal_To', 'Not_Equal_To', 'Contains');
                break;
            default:
                $valid_opp = array('Equal_To', 'Not_Equal_To', 'Contains', 'Starts_With', 'Ends_With',);
                break;
        }

        foreach ($app_list_strings['aor_operator_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aor_operator_list'][$key]);
            }
        }


        $app_list_strings['aor_operator_list'];
        if ($view == 'EditView') {
            echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . get_select_options_with_id(
                    $app_list_strings['aor_operator_list'],
                    $value
                ) . "</select>";
        } else {
            echo $app_list_strings['aor_operator_list'][$value];
        }
        die;
    }

    protected function action_getFieldTypeOptions()
    {
        global $app_list_strings, $beanFiles, $beanList;

        if (isset($_REQUEST['rel_field']) && $_REQUEST['rel_field'] != '') {
            $module = getRelatedModule($_REQUEST['aor_module'], $_REQUEST['rel_field']);
        } else {
            $module = $_REQUEST['aor_module'];
        }
        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);

        switch ($vardef['type']) {
            case 'double':
            case 'decimal':
            case 'float':
            case 'currency':
                $valid_opp = array('Value', 'Field');
                break;
            case 'uint':
            case 'ulong':
            case 'long':
            case 'short':
            case 'tinyint':
            case 'int':
                $valid_opp = array('Value', 'Field');
                break;
            case 'date':
            case 'datetime':
            case 'datetimecombo':
                $valid_opp = array('Value', 'Field', 'Date', 'Period');
                break;
            case 'enum':
            case 'dynamicenum':
            case 'multienum':
                $valid_opp = array('Value', 'Field', 'Multi');
                break;
            default:
                // Added to compare fields like assinged to with the current user
                if ((isset($vardef['module']) && $vardef['module'] == "Users") || $vardef['name'] = 'id') {
                    $valid_opp = array('Value', 'Field', 'CurrentUserID');
                } else {
                    $valid_opp = array('Value', 'Field');
                }

                break;
        }

        foreach ($app_list_strings['aor_condition_type_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aor_condition_type_list'][$key]);
            }
        }

        if ($view == 'EditView') {
            echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . get_select_options_with_id(
                    $app_list_strings['aor_condition_type_list'],
                    $value
                ) . "</select>";
        } else {
            echo $app_list_strings['aor_condition_type_list'][$value];
        }
        die;
    }

    protected function action_getActionFieldTypeOptions()
    {
        global $app_list_strings, $beanFiles, $beanList;

        if (isset($_REQUEST['rel_field']) && $_REQUEST['rel_field'] != '') {
            $module = getRelatedModule($_REQUEST['aor_module'], $_REQUEST['rel_field']);
        } else {
            $module = $_REQUEST['aor_module'];
        }

        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);

        switch ($vardef['type']) {
            case 'double':
            case 'decimal':
            case 'float':
            case 'currency':
                $valid_opp = array('Value', 'Field');
                break;
            case 'uint':
            case 'ulong':
            case 'long':
            case 'short':
            case 'tinyint':
            case 'int':
                $valid_opp = array('Value', 'Field');
                break;
            case 'date':
            case 'datetime':
            case 'datetimecombo':
                $valid_opp = array('Value', 'Field', 'Date');
                break;
            case 'enum':
            case 'multienum':
                $valid_opp = array('Value', 'Field');
                break;
            case 'relate':
                $valid_opp = array('Value', 'Field');
                if ($vardef['module'] == 'Users') {
                    $valid_opp = array('Value', 'Field', 'Round_Robin', 'Least_Busy', 'Random');
                }
                break;
            default:
                $valid_opp = array('Value', 'Field');
                break;
        }

        foreach ($app_list_strings['aor_action_type_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aor_action_type_list'][$key]);
            }
        }

        if ($view == 'EditView') {
            echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . get_select_options_with_id(
                    $app_list_strings['aor_action_type_list'],
                    $value
                ) . "</select>";
        } else {
            echo $app_list_strings['aor_action_type_list'][$value];
        }
        die;
    }

    protected function action_getModuleFieldType()
    {
        if (isset($_REQUEST['rel_field']) && $_REQUEST['rel_field'] != '') {
            $rel_module = getRelatedModule($_REQUEST['aor_module'], $_REQUEST['rel_field']);
        } else {
            $rel_module = $_REQUEST['aor_module'];
        }
        $module = $_REQUEST['aor_module'];

        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aor_type']) {
            case 'Field':
                if (isset($_REQUEST['alt_module'])
                    && $_REQUEST['alt_module'] != ''
                ) {
                    $module = $_REQUEST['alt_module'];
                }
                if ($view == 'EditView') {
                    echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . getModuleFields(
                            $module,
                            $view,
                            $value
                        ) . "</select>";
                } else {
                    echo getModuleFields($module, $view, $value);
                }
                break;
            case 'Date':
                echo getDateField($module, $aor_field, $view, $value, false);
                break;
            case 'Multi':
                echo getModuleField($rel_module, $fieldname, $aor_field, $view, $value, 'multienum');
                break;
            case 'Period':
                if ($view == 'EditView') {
                    echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . getDropdownList(
                            'date_time_period_list',
                            $_REQUEST['aor_value']
                        ) . "</select>";
                } else {
                    echo getDropdownList('date_time_period_list', $_REQUEST['aor_value']);
                }

                break;
            case 'CurrentUserID':
                break;
            case 'Value':
            default:
                echo getModuleField($rel_module, $fieldname, $aor_field, $view, $value);
                break;
        }
        die;
    }

    protected function action_getModuleFieldTypeSet()
    {
        $module = $_REQUEST['aor_module'];
        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aor_type']) {
            case 'Field':
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                if ($view == 'EditView') {
                    echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . getModuleFields(
                            $module,
                            $view,
                            $value
                        ) . "</select>";
                } else {
                    echo getModuleFields($module, $view, $value);
                }
                break;
            case 'Date':
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                echo getDateField($module, $aor_field, $view, $value);
                break;
            case 'Round_Robin':
            case 'Least_Busy':
            case 'Random':
                echo getAssignField($aor_field, $view, $value);
                break;
            case 'Value':
            default:
                echo getModuleField($module, $fieldname, $aor_field, $view, $value);
                break;
        }
        die;
    }

    protected function action_getModuleField()
    {
        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }

        echo getModuleField(
            $_REQUEST['aor_module'],
            $_REQUEST['aor_fieldname'],
            $_REQUEST['aor_newfieldname'],
            $view,
            $value
        );
        die;
    }

    protected function action_getRelFieldTypeSet()
    {
        $module = $_REQUEST['aor_module'];
        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aor_type']) {
            case 'Field':
                if (isset($_REQUEST['alt_module']) && $_REQUEST['alt_module'] != '') {
                    $module = $_REQUEST['alt_module'];
                }
                if ($view == 'EditView') {
                    echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . getModuleFields(
                            $module,
                            $view,
                            $value
                        ) . "</select>";
                } else {
                    echo getModuleFields($module, $view, $value);
                }
                break;
            case 'Value':
            default:
                echo getModuleField($module, $fieldname, $aor_field, $view, $value);
                break;
        }
        die;
    }

    protected function action_getRelActionFieldTypeOptions()
    {
        global $app_list_strings, $beanFiles, $beanList;

        $module = $_REQUEST['aor_module'];
        $alt_module = $_REQUEST['alt_module'];
        $fieldname = $_REQUEST['aor_fieldname'];
        $aor_field = $_REQUEST['aor_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view = 'EditView';
        }

        if (isset($_REQUEST['aor_value'])) {
            $value = $_REQUEST['aor_value'];
        } else {
            $value = '';
        }


        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];
        $vardef = $focus->getFieldDefinition($fieldname);


        /*if($vardef['module'] == $alt_module){
            $valid_opp = array('Value','Field');
        }
        else{
            $valid_opp = array('Value');
        }*/
        $valid_opp = array('Value');

        foreach ($app_list_strings['aor_rel_action_type_list'] as $key => $keyValue) {
            if (!in_array($key, $valid_opp)) {
                unset($app_list_strings['aor_rel_action_type_list'][$key]);
            }
        }

        if ($view == 'EditView') {
            echo "<select type='text' style='width:178px;' name='$aor_field' id='$aor_field' title='' tabindex='116'>" . get_select_options_with_id(
                    $app_list_strings['aor_rel_action_type_list'],
                    $value
                ) . "</select>";
        } else {
            echo $app_list_strings['aor_rel_action_type_list'][$value];
        }
        die;
    }
}
