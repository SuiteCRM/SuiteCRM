<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once('include/Dashlets/Dashlet.php');
require_once 'modules/AOR_Reports/aor_utils.php';

#[\AllowDynamicProperties]
class AORReportsDashlet extends Dashlet
{
    public $def;
    public $report;
    public $charts;
    public $onlyCharts;

    public function __construct($id, $def = array())
    {
        global $current_user, $app_strings;

        parent::__construct($id);
        $this->isConfigurable = true;
        $this->def = $def;
        if (empty($def['dashletTitle'])) {
            $this->title = translate('LBL_AOR_REPORTS_DASHLET', 'AOR_Reports');
        } else {
            $this->title = $def['dashletTitle'];
        }

        $this->params = array();
        if (!empty($def['parameter_id'])) {
            foreach ($def['parameter_id'] as $key => $parameterId) {
                $this->params[$parameterId] = array(
                    'id' => $parameterId,
                    'operator' => $def['parameter_operator'][$key],
                    'type' => $def['parameter_type'][$key],
                    'value' => $def['parameter_value'][$key]
                );
            }
        }
        if (!empty($def['aor_report_id'])) {
            $this->report = BeanFactory::getBean('AOR_Reports', $def['aor_report_id']);
            if($this->report !== false) {
                $this->report->user_parameters = $this->params;
            }
        }
        $this->onlyCharts = !empty($def['onlyCharts']);
        $this->charts = !empty($def['charts']) ? $def['charts'] : array();
    }

    public function display()
    {
        global $current_language,$mod_strings;
        $mod_strings = return_module_language($current_language, 'AOR_Reports');
        $dashletSmarty = new Sugar_Smarty();
        $dashletTemplate = get_custom_file_if_exists('modules/AOR_Reports/Dashlets/AORReportsDashlet/dashlet.tpl');
        $dashletSmarty->assign('MOD', $mod_strings);
        $dashletSmarty->assign('dashlet_id', $this->id);
        $dashletSmarty->assign('report_id', $this->report->id);
        $dashletSmarty->assign('chartHTML', $this->getChartHTML());
        $dashletSmarty->assign('onlyCharts', $this->onlyCharts);
        $dashletSmarty->assign('parameters', json_encode(array(
            'ids' => isset($this->def['parameter_id']) ? $this->def['parameter_id'] : null,
            'operators' => isset($this->def['parameter_operator']) ? $this->def['parameter_operator'] : null,
            'types' => isset($this->def['parameter_type']) ? $this->def['parameter_type'] : null,
            'values' => isset($this->def['parameter_value']) ? $this->def['parameter_value'] : null
        )));

        return $dashletSmarty->fetch($dashletTemplate);
    }

    public function getChartHTML()
    {
        if (!empty($this->report->id)) {
            //return $this->report->build_report_chart($this->charts, AOR_Report::CHART_TYPE_CHARTJS);
            return $this->report->build_report_chart($this->charts, AOR_Report::CHART_TYPE_RGRAPH);
        } else {
            return '';
        }
    }

    public function process()
    {
    }

    public function displayOptions()
    {
        ob_start();
        global $current_language, $app_list_strings, $datetime,$mod_strings;
        $mod_strings = return_module_language($current_language, 'AOR_Reports');
        $optionsSmarty = new Sugar_Smarty();
        $optionsSmarty->assign('MOD', $mod_strings);
        $optionsSmarty->assign('id', $this->id);
        $optionsSmarty->assign('dashletTitle', $this->title);
        $optionsSmarty->assign('aor_report_id', $this->report->id);
        $optionsSmarty->assign('aor_report_name', $this->report->name);
        $optionsSmarty->assign('onlyCharts', $this->onlyCharts);
        $optionsSmarty->assign('aor_date_options', $app_list_strings['aor_date_options']);
        $optionsSmarty->assign('aor_condition_type_list', $app_list_strings['aor_condition_type_list']);
        $optionsSmarty->assign('aor_date_operator', $app_list_strings['aor_date_operator']);
        $optionsSmarty->assign('aor_date_type_list', $app_list_strings['aor_date_type_list']);
        $optionsSmarty->assign('date_time_period_list', $app_list_strings['date_time_period_list']);

        $charts = array();
        if (!empty($this->report->id)) {
            foreach ($this->report->get_linked_beans('aor_charts', 'AOR_Charts') as $chart) {
                $charts[$chart->id] = $chart->name;
            }
        }
        $conditions = getConditionsAsParameters($this->report, $this->params);
        $i = 0;
        foreach ($conditions as $condition) {
            if ($condition["value_type"] == "Date") {
                if ($condition["additionalConditions"][0] == "now") {
                    $conditions[$i]["value"] = date("d/m/Y");
                }
            }

            $i++;
        }
        $optionsSmarty->assign('parameters', $conditions);
        $chartOptions = get_select_options_with_id($charts, $this->charts);
        $optionsSmarty->assign('chartOptions', $chartOptions);
        $optionsTemplate = get_custom_file_if_exists('modules/AOR_Reports/Dashlets/AORReportsDashlet/dashletConfigure.tpl');
        ob_clean();

        return $optionsSmarty->fetch($optionsTemplate);
    }

    public function saveOptions($req)
    {
        $allowedKeys = array_flip(array(
            'aor_report_id',
            'dashletTitle',
            'charts',
            'onlyCharts',
            'parameter_id',
            'parameter_value',
            'parameter_type',
            'parameter_operator'
        ));
        // Fix for issue #1700 - save value as db type
        $itemsCount = is_countable($req['parameter_value']) ? count($req['parameter_value']) : 0;

        // Fix for issue #1700 - save value as db type
        for ($i = 0; $i < $itemsCount; $i++) {
            if (isset($req['parameter_value'][$i]) && $req['parameter_value'][$i] != '') {
                global $current_user, $timedate;
                $user_date_format = $timedate->get_date_format($current_user);

                if (DateTime::createFromFormat($user_date_format, $req['parameter_value'][$i]) !== false) {
                    $date = DateTime::createFromFormat($user_date_format, $req['parameter_value'][$i]);
                    $date->setTime(00, 00, 00);
                    $req['parameter_value'][$i] = $timedate->asDb($date);
                }
            }
        }

        $intersected = array_intersect_key($req, $allowedKeys);

        return $intersected;
    }

    public function hasAccess()
    {
        return true;
    }
}
