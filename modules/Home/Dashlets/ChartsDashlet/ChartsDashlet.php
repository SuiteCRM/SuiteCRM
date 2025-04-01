<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


require_once('include/Dashlets/Dashlet.php');


#[\AllowDynamicProperties]
class ChartsDashlet extends Dashlet
{
    public $width = '400';
    public $height = '480';
    public $report_id;

    /**
     * Constructor
     *
     * @global string current language
     * @param guid $id id for the current dashlet (assigned from Home module)
     * @param report_id $report_id id of the saved report
     * @param array $def options saved for this dashlet
     */
    public function __construct($id, $report_id, $def)
    {
        $this->report_id = $report_id;

        $this->loadLanguage('ChartsDashlet'); // load the language strings here

        parent::__construct($id); // call parent constructor

        $this->searchFields = array();
        $this->isConfigurable = true; // dashlet is configurable
        $this->hasScript = true;  // dashlet has javascript attached to it
    }

    /**
     * Displays the dashlet
     *
     * @return string html to display dashlet
     */
    public function display()
    {
        require_once("modules/Reports/Report.php");

        //		ini_set('display_errors', 'false');

        $chartReport = new SavedReport();
        $chartExists = $chartReport->retrieve($this->report_id, false);

        if (!is_null($chartExists)) {
            $title = getReportNameTranslation($chartReport->name);
            $this->title = $title;

            $reporter = new Report($chartReport->content);
            $reporter->is_saved_report = true;
            $reporter->saved_report_id = $chartReport->id;
            $reporter->get_total_header_row();
            $reporter->run_chart_queries();

            require_once("modules/Reports/templates/templates_chart.php");

            ob_start();
            template_chart($reporter, true, true, $this->id);
            $str = ob_get_contents();
            ob_end_clean();

            $xmlFile = get_cache_file_name($reporter);

            $html = parent::display() . "<div align='center'>" . $str . "</div>" . "<br />"; // return parent::display for title and such

            $ss = new Sugar_Smarty();
            $ss->assign('chartName', $this->id);
            $ss->assign('chartXMLFile', $xmlFile);
            $script = $ss->fetch('modules/Home/Dashlets/ChartsDashlet/ChartsDashletScript.tpl');
            $json = getJSONobj();

            return parent::display() . "<div align='center'>" . $str . "</div>" . "<br />"; // return parent::display for title and such
        }
    }

    /**
     * Displays the javascript for the dashlet
     *
     * @return string javascript to use with this dashlet
     */
    public function displayScript()
    {
        require_once("modules/Reports/Report.php");


        $chartReport = new SavedReport();
        $chartExists = $chartReport->retrieve($this->report_id, false);

        if (!is_null($chartExists)) {
            $this->title = $chartReport->name;

            require_once("modules/Reports/templates/templates_chart.php");
            require_once('include/SugarCharts/SugarChartFactory.php');

            $sugarChart = SugarChartFactory::getInstance();


            $reporter = new Report($chartReport->content);
            $reporter->is_saved_report = true;
            $reporter->saved_report_id = $chartReport->id;
            $xmlFile = get_cache_file_name($reporter);

            $str = $sugarChart ? $sugarChart->getDashletScript($this->id, $xmlFile) : '';
            return $str;
        }
    }

    /**
     * Displays the configuration form for the dashlet
     *
     * @return string html to display form
     */
    public function displayOptions()
    {
    }

    /**
     * called to filter out $_REQUEST object when the user submits the configure dropdown
     *
     * @param array $req $_REQUEST
     * @return array filtered options to save
     */
    public function saveOptions($req)
    {
    }

    public function setConfigureIcon()
    {
        if ($this->isConfigurable) {
            $additionalTitle = '<td nowrap width="1%" style="padding-right: 0px;"><div class="dashletToolSet"><a href="index.php?module=Reports&record=' . $this->report_id . '&action=ReportCriteriaResults&page=report">'
                               . SugarThemeRegistry::current()->getImage('dashlet-header-edit', 'title="' . translate('LBL_DASHLET_EDIT', 'Home') . '" border="0"  align="absmiddle"', null, null, '.gif', translate('LBL_DASHLET_EDIT', 'Home')).'</a>'

                               . '';
        } else {
            $additionalTitle = '<td nowrap width="1%" style="padding-right: 0px;"><div class="dashletToolSet">';
        }

        return $additionalTitle;
    }

    public function setRefreshIcon()
    {
        $additionalTitle = '';
        if ($this->isRefreshable) {
            $additionalTitle .= '<a href="#" onclick="SUGAR.mySugar.retrieveDashlet(\''
                                . $this->id . '\', \'chart\'); return false;"><!--not_in_theme!-->'
                                . SugarThemeRegistry::current()->getImage(
                                    'dashlet-header-refresh',
                                    'border="0" align="absmiddle" title="'. translate('LBL_DASHLET_REFRESH', 'Home') . '"',
                                    null,
                                    null,
                                    '.gif',
                                    translate('LBL_DASHLET_REFRESH', 'Home')
                ) .'</a>';
        }
        return $additionalTitle;
    }
}
