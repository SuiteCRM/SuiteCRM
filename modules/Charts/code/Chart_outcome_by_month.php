<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/charts/Charts.php');

#[\AllowDynamicProperties]
class Chart_outcome_by_month
{
    public $modules = array('Opportunities');
    public $order = 0;
    public function __construct()
    {
    }




    public function draw($extra_tools)
    {
        global $app_list_strings, $current_language, $sugar_config, $currentModule, $action, $theme;
        $current_module_strings = return_module_language($current_language, 'Charts');


        if (isset($_REQUEST['obm_refresh'])) {
            $refresh = $_REQUEST['obm_refresh'];
        } else {
            $refresh = false;
        }

        $date_start = array();
        $datax = array();
        //get the dates to display
        global $current_user;
        $user_date_start = $current_user->getPreference('obm_date_start');
        if (!empty($user_date_start)  && !isset($_REQUEST['obm_date_start'])) {
            $date_start =$user_date_start;
            $GLOBALS['log']->debug("USER PREFERENCES['obm_date_start'] is:");
            $GLOBALS['log']->debug($user_date_start);
        } elseif (isset($_REQUEST['obm_year']) && $_REQUEST['obm_year'] != '') {
            $date_start = $_REQUEST['obm_year'].'-01-01';
            $current_user->setPreference('obm_date_start', $date_start);
            $GLOBALS['log']->debug("_REQUEST['obm_date_start'] is:");
            $GLOBALS['log']->debug($_REQUEST['obm_date_start']);
            $GLOBALS['log']->debug("_SESSION['obm_date_start'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('obm_date_start'));
        } else {
            $date_start = date('Y').'-01-01';
        }
        $user_date_end = $current_user->getPreference('obm_date_end');
        if (!empty($user_date_end) && !isset($_REQUEST['obm_date_end'])) {
            $date_end =$user_date_end;
            $GLOBALS['log']->debug("USER PREFERENCES['obm_date_end'] is:");
            $GLOBALS['log']->debug($date_end);
        } elseif (isset($_REQUEST['obm_year']) && $_REQUEST['obm_year'] != '') {
            $date_end = $_REQUEST['obm_year'].'-12-31';
            $current_user->setPreference('obm_date_end', $date_end);
            $GLOBALS['log']->debug("_REQUEST['obm_date_end'] is:");
            $GLOBALS['log']->debug($_REQUEST['obm_date_end']);
            $GLOBALS['log']->debug("USER PREFERENCES['obm_date_end'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('obm_date_end'));
        } else {
            $date_end = date('Y').'-12-31';
        }

        $ids = array();
        //get list of user ids for which to display data
        $user_ids = $current_user->getPreference('obm_ids');
        if (!empty($user_ids) && (is_countable($user_ids) ? count($user_ids) : 0) != 0 && !isset($_REQUEST['obm_ids'])) {
            $ids = $user_ids;
            $GLOBALS['log']->debug("USER PREFERENCES['obm_ids'] is:");
            $GLOBALS['log']->debug($user_ids);
        } elseif (isset($_REQUEST['obm_ids']) && (is_countable($_REQUEST['obm_ids']) ? count($_REQUEST['obm_ids']) : 0) > 0) {
            $ids = $_REQUEST['obm_ids'];
            $current_user->setPreference('obm_ids', $_REQUEST['obm_ids']);
            $GLOBALS['log']->debug("_REQUEST['obm_ids'] is:");
            $GLOBALS['log']->debug($_REQUEST['obm_ids']);
            $GLOBALS['log']->debug("USER PREFRENCES['obm_ids'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('obm_ids'));
        } else {
            $ids = get_user_array(false);
            $ids = array_keys($ids);
        }

        //create unique prefix based on selected users for image files
        $id_hash = '1';
        if (isset($ids)) {
            sort($ids);
            $id_hash = crc32(implode('', $ids));
            if ($id_hash < 0) {
                $id_hash = $id_hash * -1;
            }
        }
        $GLOBALS['log']->debug("ids is:");
        $GLOBALS['log']->debug($ids);
        $id_md5 = substr(md5($current_user->id), 0, 9);


        // cn: format date_start|end to user's preferred
        global $timedate;
        $dateDisplayStart	= strftime($timedate->get_user_date_format(), strtotime($date_start));
        $dateDisplayEnd   	= strftime($timedate->get_user_date_format(), strtotime($date_end));
        $seps				= array("-", "/");
        $dates				= array($date_start, $date_end);
        $dateFileNameSafe	= str_replace($seps, "_", $dates);

        $cache_file_name = sugar_cached("xml/").$current_user->getUserPrivGuid()."_outcome_by_month_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";

        $GLOBALS['log']->debug("cache file name is: $cache_file_name");


        global $app_strings;
        $tools='<div align="right"><a href="index.php?module='.$currentModule.'&action='. $action .'&obm_refresh=true" class="tabFormAdvLink">'.SugarThemeRegistry::current()->getImage('refresh', 'border="0" align="absmiddle"', null, null, '.gif', $mod_strings['LBL_REFRESH']).'&nbsp;'.$current_module_strings['LBL_REFRESH'].'</a>&nbsp;&nbsp;<a href="javascript: toggleDisplay(\'outcome_by_month_edit\');" class="tabFormAdvLink">'.SugarThemeRegistry::current()->getImage('edit', 'border="0" align="absmiddle"', null, null, '.gif', $mod_strings['LBL_EDIT']).'&nbsp;'. $current_module_strings['LBL_EDIT'].'</a>&nbsp;&nbsp;'.$extra_tools.'</div>'; ?>
	<?php echo '<span onmouseover="this.style.cursor=\'move\'" id="chart_handle_' . $this->order . '">' . get_form_header($current_module_strings['LBL_YEAR_BY_OUTCOME'], $tools, false) . '</span>'; ?>

<?php
    $cal_lang = "en";
        $cal_dateformat = parse_calendardate($app_strings['NTC_DATE_FORMAT']);

        if (empty($_SESSION['obm_ids'])) {
            $_SESSION['obm_ids'] = "";
        } ?>
<p>
<div id='outcome_by_month_edit' style='display: none;'>
<form name="outcome_by_month" action="index.php" method="post" >
<input type="hidden" name="module" value="<?php echo $currentModule; ?>">
<input type="hidden" name="action" value="<?php echo $action; ?>">
<input type="hidden" name="obm_refresh" value="true">
<input type="hidden" name="obm_date_start" value="<?php if (isset($_SESSION['obm_date_start'])) {
            echo $_SESSION['obm_date_start'];
        } ?>">
<input type="hidden" name="obm_date_end" value="<?php if (isset($_SESSION['obm_date_end'])) {
            echo $_SESSION['obm_date_end'];
        } ?>">
<table cellpadding="0" cellspacing="0" border="0" class="edit view" align="center">
<tr>
	<td valign='top' nowrap ><b><?php echo $current_module_strings['LBL_YEAR']?></b><br><span class="dateFormat"><?php echo $app_strings['NTC_YEAR_FORMAT']?></span></td>
	<td valign='top' ><input class="text" name="obm_year" size='12' maxlength='10' id='obm_year'  value='<?php if (isset($date_start)) {
            echo substr((string) $date_start, 0, 4);
        } ?>'>&nbsp;&nbsp;</td>
	<td valign='top'><b><?php echo $current_module_strings['LBL_USERS']; ?></b></td>
	<td valign='top'><select name="obm_ids[]" multiple size='3'><?php echo get_select_options_with_id(get_user_array(false), $ids); ?></select></td>
	<td align="right" valign="top"><input class="button" onclick="return verify_chart_data_outcome_by_month();" type="submit" title="<?php echo $app_strings['LBL_SELECT_BUTTON_TITLE']; ?>" value="<?php echo $app_strings['LBL_SELECT_BUTTON_LABEL']?>" /><input class="button" onClick="javascript: toggleDisplay('outcome_by_month_edit');" type="button" title="<?php echo $app_strings['LBL_CANCEL_BUTTON_TITLE']; ?>" accessKey="<?php echo $app_strings['LBL_CANCEL_BUTTON_KEY']; ?>" value="<?php echo $app_strings['LBL_CANCEL_BUTTON_LABEL']?>"/></td>
</tr>
</table>
</form>

</div>
</p>
<?php
// draw chart
echo "<p align='center'>".$this->gen_xml($date_start, $date_end, $ids, $cache_file_name, $refresh, $current_module_strings)."</p>";
        echo "<P align='center'><span class='chartFootnote'>".$current_module_strings['LBL_MONTH_BY_OUTCOME_DESC']."</span></P>"; ?>


<?php
    if (file_exists($cache_file_name)) {
        $file_date = $timedate->asUser($timedate->fromTimestamp(filemtime($cache_file_name)));
    } else {
        $file_date = '';
    } ?>

<span class='chartFootnote'>
<p align="right"><i><?php  echo $current_module_strings['LBL_CREATED_ON'].' '.$file_date; ?></i></p>
</span>
<?php
echo get_validate_chart_js();
    }

    /**
    * Creates opportunity pipeline image as a VERTICAL accumlated bar graph for multiple users.
    * param $datax- the month data to display in the x-axis
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
    * All Rights Reserved..
    * Contributor(s): ______________________________________..
    */
    public function gen_xml(
        $date_start = '1971-10-15',
        $date_end = '2010-10-15',
        $user_id = array('1'),
        $cache_file_name = 'a_file',
        $refresh = false,
        $current_module_strings = null
    ) {
        global $app_strings, $app_list_strings, $charset, $lang, $barChartColors, $current_user, $current_language;

        // set $current_module_strings to 'Charts' module strings by default
        if (empty($current_module_strings)) {
            $current_module_strings = return_module_language($current_language, 'Charts');
        }

        $kDelim = $current_user->getPreference('num_grp_sep');
        global $timedate;

        if (!file_exists($cache_file_name) || $refresh == true) {
            $GLOBALS['log']->debug("date_start is: $date_start");
            $GLOBALS['log']->debug("date_end is: $date_end");
            $GLOBALS['log']->debug("user_id is: ");
            $GLOBALS['log']->debug($user_id);
            $GLOBALS['log']->debug("cache_file_name is: $cache_file_name");

            $where = "";
            //build the where clause for the query that matches $user
            $count = is_countable($user_id) ? count($user_id) : 0;
            $id = array();
            if ($count>0) {
                foreach ($user_id as $the_id) {
                    $id[] = "'".$the_id."'";
                }
                $ids = implode(",", $id);
                $where .= "opportunities.assigned_user_id IN ($ids) ";
            }

            // cn: adding user-pref date handling
            $dateStartDisplay = $timedate->asUserDate($timedate->fromString($date_start));
            $dateEndDisplay = $timedate->asUserDate($timedate->fromString($date_end));

            $opp = BeanFactory::newBean('Opportunities');
            //build the where clause for the query that matches $date_start and $date_end
            $where .= "AND opportunities.date_closed >= ".DBManagerFactory::getInstance()->convert("'".$date_start."'", 'date')." AND opportunities.date_closed <= ".DBManagerFactory::getInstance()->convert("'".$date_end."'", 'date')." AND opportunities.deleted=0";
            $query = "SELECT sales_stage,".DBManagerFactory::getInstance()->convert('opportunities.date_closed', 'date_format', array("'%Y-%m'"), array("'YYYY-MM'"))." as m, sum(amount_usdollar/1000) as total, count(*) as opp_count FROM opportunities ";
            $query .= "WHERE ".$where;
            $query .= " GROUP BY sales_stage,".DBManagerFactory::getInstance()->convert('opportunities.date_closed', 'date_format', array("'%Y-%m'"), array("'YYYY-MM'"))."ORDER BY m";
            //Now do the db queries
            //query for opportunity data that matches $datay and $user

            $result = $opp->db->query($query, true);
            //build pipeline by sales stage data
            $total = 0;
            $div = 1;
            global $sugar_config;
            $symbol = $sugar_config['default_currency_symbol'];
            $other = $current_module_strings['LBL_LEAD_SOURCE_OTHER'];
            $rowTotalArr = array();
            $rowTotalArr[] = 0;
            global $current_user;
            $salesStages = array("Closed Lost"=>$app_list_strings['sales_stage_dom']["Closed Lost"],"Closed Won"=>$app_list_strings['sales_stage_dom']["Closed Won"],"Other"=>$other);
            if ($current_user->getPreference('currency')) {
                $currency = BeanFactory::newBean('Currencies');
                $currency->retrieve($current_user->getPreference('currency'));
                $div = $currency->conversion_rate;
                $symbol = $currency->symbol;
            }
            $months = array();
            $monthArr = array();
            while ($row = $opp->db->fetchByAssoc($result, false)) {
                if ($row['total']*$div<=100) {
                    $sum = round($row['total']*$div, 2);
                } else {
                    $sum = round($row['total']*$div);
                }
                if ($row['sales_stage'] == 'Closed Won' || $row['sales_stage'] == 'Closed Lost') {
                    $salesStage = $row['sales_stage'];
                    $salesStageT = $app_list_strings['sales_stage_dom'][$row['sales_stage']];
                } else {
                    $salesStage = "Other";
                    $salesStageT = $other;
                }

                $months[$row['m']] = $row['m'];
                if (!isset($monthArr[$row['m']]['row_total'])) {
                    $monthArr[$row['m']]['row_total']=0;
                }
                $monthArr[$row['m']][$salesStage]['opp_count'][] = $row['opp_count'];
                $monthArr[$row['m']][$salesStage]['total'][] = $sum;
                $monthArr[$row['m']]['outcome'][$salesStage]=$salesStageT;
                $monthArr[$row['m']]['row_total'] += $sum;

                $total += $sum;
            }

            $fileContents = '     <xData length="20">'."\n";
            if (!empty($months)) {
                foreach ($months as $month) {
                    $rowTotalArr[]=$monthArr[$month]['row_total'];
                    if ($monthArr[$month]['row_total']>100) {
                        $monthArr[$month]['row_total']=round($monthArr[$month]['row_total']);
                    }
                    $fileContents .= '          <dataRow title="'.$month.'" endLabel="'.currency_format_number($monthArr[$month]['row_total'], array('currency_symbol' => true)).'">'."\n";
                    arsort($salesStages);
                    foreach ($salesStages as $outcome=>$outcome_translation) {
                        if (isset($monthArr[$month][$outcome])) {
                            $fileContents .= '               <bar id="'.$outcome.'" totalSize="'.array_sum($monthArr[$month][$outcome]['total']).'" altText="'.$month.': '.format_number(array_sum($monthArr[$month][$outcome]['opp_count']), 0, 0).' '.$current_module_strings['LBL_OPPS_WORTH'].' '.currency_format_number(array_sum($monthArr[$month][$outcome]['total']), array('currency_symbol' => true)).$current_module_strings['LBL_OPP_THOUSANDS'].' '.$current_module_strings['LBL_OPPS_OUTCOME'].' '.$outcome_translation.'" url="index.php?module=Opportunities&action=index&date_closed='.$month.'&sales_stage='.urlencode($outcome).'&query=true&searchFormTab=advanced_search"/>'."\n";
                        }
                    }
                    $fileContents .= '          </dataRow>'."\n";
                }
            } else {
                $fileContents .= '          <dataRow title="" endLabel="">'."\n";
                $fileContents .= '               <bar id="" totalSize="0" altText="" url=""/>'."\n";
                $fileContents .= '          </dataRow>'."\n";
                $rowTotalArr[] = 1000;
            }
            $fileContents .= '     </xData>'."\n";
            $max = get_max($rowTotalArr);
            $fileContents .= '     <yData min="0" max="'.$max.'" length="10" prefix="'.$symbol.'" suffix="" kDelim="'.$kDelim.'" defaultAltText="'.$current_module_strings['LBL_ROLLOVER_DETAILS'].'"/>'."\n";
            $fileContents .= '     <colorLegend status="on">'."\n";
            $i=0;
            asort($salesStages);
            foreach ($salesStages as $outcome=>$outcome_translation) {
                $color = generate_graphcolor($outcome, $i);
                $fileContents .= '          <mapping id="'.$outcome.'" name="'.$outcome_translation.'" color="'.$color.'"/>'."\n";
                $i++;
            }
            $fileContents .= '     </colorLegend>'."\n";
            $fileContents .= '     <graphInfo>'."\n";
            $fileContents .= '          <![CDATA['.$current_module_strings['LBL_DATE_RANGE']." ".$dateStartDisplay." ".$current_module_strings['LBL_DATE_RANGE_TO']." ".$dateEndDisplay."<br/>".$current_module_strings['LBL_OPP_SIZE'].' '.$symbol.'1'.$current_module_strings['LBL_OPP_THOUSANDS'].']]>'."\n";
            $fileContents .= '     </graphInfo>'."\n";
            $fileContents .= '     <chartColors ';
            foreach ($barChartColors as $key => $value) {
                $fileContents .= ' '.$key.'='.'"'.$value.'" ';
            }
            $fileContents .= ' />'."\n";
            $fileContents .= '</graphData>'."\n";
            $total = round($total, 2);
            $title = '<graphData title="'.$current_module_strings['LBL_TOTAL_PIPELINE'].currency_format_number($total, array('currency_symbol' => true)).$app_strings['LBL_THOUSANDS_SYMBOL'].'">'."\n";
            $fileContents = $title.$fileContents;

            //echo $fileContents;
            save_xml_file($cache_file_name, $fileContents);
        }
        $return = create_chart('vBarF', $cache_file_name);
        return $return;
    }

    public function constructQuery()
    {
        global $current_user;
        global $timedate;

        $user_date_start = $current_user->getPreference('obm_date_start');
        if (!empty($user_date_start)  && !isset($_REQUEST['obm_date_start'])) {
            $date_start =$user_date_start;
            $GLOBALS['log']->debug("USER PREFERENCES['obm_date_start'] is:");
            $GLOBALS['log']->debug($user_date_start);
        } elseif (isset($_REQUEST['obm_year']) && $_REQUEST['obm_year'] != '') {
            $date_start = $_REQUEST['obm_year'].'-01-01';
            $current_user->setPreference('obm_date_start', $date_start);
            $GLOBALS['log']->debug("_REQUEST['obm_date_start'] is:");
            $GLOBALS['log']->debug($_REQUEST['obm_date_start']);
            $GLOBALS['log']->debug("_SESSION['obm_date_start'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('obm_date_start'));
        } else {
            $date_start = date('Y').'-01-01';
        }
        $user_date_end = $current_user->getPreference('obm_date_end');
        if (!empty($user_date_end) && !isset($_REQUEST['obm_date_end'])) {
            $date_end =$user_date_end;
            $GLOBALS['log']->debug("USER PREFERENCES['obm_date_end'] is:");
            $GLOBALS['log']->debug($date_end);
        } elseif (isset($_REQUEST['obm_year']) && $_REQUEST['obm_year'] != '') {
            $date_end = $_REQUEST['obm_year'].'-12-31';
            $current_user->setPreference('obm_date_end', $date_end);
            $GLOBALS['log']->debug("_REQUEST['obm_date_end'] is:");
            $GLOBALS['log']->debug($_REQUEST['obm_date_end']);
            $GLOBALS['log']->debug("USER PREFERENCES['obm_date_end'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('obm_date_end'));
        } else {
            $date_end = date('Y').'-12-31';
        }

        $ids = array();
        //get list of user ids for which to display data
        $user_ids = $current_user->getPreference('obm_ids');
        if (!empty($user_ids) && (is_countable($user_ids) ? count($user_ids) : 0) != 0 && !isset($_REQUEST['obm_ids'])) {
            $ids = $user_ids;
            $GLOBALS['log']->debug("USER PREFERENCES['obm_ids'] is:");
            $GLOBALS['log']->debug($user_ids);
        } elseif (isset($_REQUEST['obm_ids']) && (is_countable($_REQUEST['obm_ids']) ? count($_REQUEST['obm_ids']) : 0) > 0) {
            $ids = $_REQUEST['obm_ids'];
            $current_user->setPreference('obm_ids', $_REQUEST['obm_ids']);
            $GLOBALS['log']->debug("_REQUEST['obm_ids'] is:");
            $GLOBALS['log']->debug($_REQUEST['obm_ids']);
            $GLOBALS['log']->debug("USER PREFRENCES['obm_ids'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('obm_ids'));
        } else {
            $ids = get_user_array(false);
            $ids = array_keys($ids);
        }

        $user_id = $ids;

        $where = "";
        //build the where clause for the query that matches $user
        $count = is_countable($user_id) ? count($user_id) : 0;
        $id = array();
        if ($count>0) {
            foreach ($user_id as $the_id) {
                $id[] = "'".$the_id."'";
            }
            $ids = implode(",", $id);
            $where .= "opportunities.assigned_user_id IN ($ids) ";
        }

        // cn: adding user-pref date handling
        $dateStartDisplay = $timedate->asUserDate($timedate->fromString($date_start));
        $dateEndDisplay = $timedate->asUserDate($timedate->fromString($date_end));

        $opp = BeanFactory::newBean('Opportunities');
        //build the where clause for the query that matches $date_start and $date_end
        $where .= "AND opportunities.date_closed >= ".DBManagerFactory::getInstance()->convert("'".$date_start."'", 'date')." AND opportunities.date_closed <= ".DBManagerFactory::getInstance()->convert("'".$date_end."'", 'date')." AND opportunities.deleted=0";
        $query = "SELECT sales_stage,".DBManagerFactory::getInstance()->convert('opportunities.date_closed', 'date_format', array("'%Y-%m'"), array("'YYYY-MM'"))." as m, sum(amount_usdollar/1000) as total, count(*) as opp_count FROM opportunities ";
        $query .= "WHERE ".$where;
        $query .= " GROUP BY sales_stage,".DBManagerFactory::getInstance()->convert('opportunities.date_closed', 'date_format', array("'%Y-%m'"), array("'YYYY-MM'"))."ORDER BY m";
        return $query;
    }

    public function constructGroupBy()
    {
        return array( 'm', 'sales_stage', );
    }
}
