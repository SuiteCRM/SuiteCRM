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
class Chart_lead_source_by_outcome
{
    public $modules = array('Opportunities');
    public $order = 0;
    public function __construct()
    {
    }




    public function draw($extra_tools)
    {
        global $app_list_strings, $current_language, $sugar_config, $currentModule, $action,$theme;
        $current_module_strings = return_module_language($current_language, 'Charts');


        if (isset($_REQUEST['lsbo_refresh'])) {
            $refresh = $_REQUEST['lsbo_refresh'];
        } else {
            $refresh = false;
        }

        $tempx = array();
        $datax = array();
        $selected_datax = array();
        //get list of sales stage keys to display

        global $current_user;
        $tempx = $current_user->getPreference('lsbo_lead_sources');
        if (!empty($lsbo_lead_sources) && (is_countable($lsbo_lead_sources) ? count($lsbo_lead_sources) : 0) > 0 && !isset($_REQUEST['lsbo_lead_sources'])) {
            $GLOBALS['log']->fatal("user->getPreference('lsbo_lead_sources') is:");
            $GLOBALS['log']->fatal($tempx);
        } elseif (isset($_REQUEST['lsbo_lead_sources']) && (is_countable($_REQUEST['lsbo_lead_sources']) ? count($_REQUEST['lsbo_lead_sources']) : 0) > 0) {
            $tempx = $_REQUEST['lsbo_lead_sources'];
            $current_user->setPreference('lsbo_lead_sources', $_REQUEST['lsbo_lead_sources']);
            $GLOBALS['log']->fatal("_REQUEST['lsbo_lead_sources'] is:");
            $GLOBALS['log']->fatal($_REQUEST['lsbo_lead_sources']);
            $GLOBALS['log']->fatal("user->getPreference('lsbo_lead_sources') is:");
            $GLOBALS['log']->fatal($current_user->getPreference('lsbo_lead_sources'));
        }
        //set $datax using selected sales stage keys
        if (!empty($tempx) && (is_countable($tempx) ? count($tempx) : 0) > 0) {
            foreach ($tempx as $key) {
                $datax[$key] = $app_list_strings['lead_source_dom'][$key];
                array_push($selected_datax, $key);
            }
        } else {
            $datax = $app_list_strings['lead_source_dom'];
            $selected_datax = array_keys($app_list_strings['lead_source_dom']);
        }

        $ids =$current_user->getPreference('lsbo_ids');
        //get list of user ids for which to display data
        if (!empty($ids) && (is_countable($ids) ? count($ids) : 0) != 0 && !isset($_REQUEST['lsbo_ids'])) {
            $GLOBALS['log']->debug("_SESSION['lsbo_ids'] is:");
            $GLOBALS['log']->debug($ids);
        } elseif (isset($_REQUEST['lsbo_ids']) && (is_countable($_REQUEST['lsbo_ids']) ? count($_REQUEST['lsbo_ids']) : 0) > 0) {
            $ids = $_REQUEST['lsbo_ids'];
            $current_user->setPreference('lsbo_ids', $_REQUEST['lsbo_ids']);
            $GLOBALS['log']->debug("_REQUEST['lsbo_ids'] is:");
            $GLOBALS['log']->debug($_REQUEST['lsbo_ids']);
            $GLOBALS['log']->debug("user->getPreference('lsbo_ids') is:");
            $GLOBALS['log']->debug($current_user->getPreference('lsbo_ids'));
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


        $seps				= array("-", "/");
        $dates				= array(date($GLOBALS['timedate']->dbDayFormat), $GLOBALS['timedate']->dbDayFormat);
        $dateFileNameSafe	= str_replace($seps, "_", $dates);
        $cache_file_name	= sugar_cached("xml/").$current_user->getUserPrivGuid()."_lead_source_by_outcome_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";
        $GLOBALS['log']->debug("cache file name is: $cache_file_name");


        $tools='<div align="right"><a href="index.php?module='.$currentModule.'&action='. $action .'&lsbo_refresh=true" class="tabFormAdvLink">'.SugarThemeRegistry::current()->getImage('refresh', 'border="0" align="absmiddle"', null, null, '.gif', $mod_strings['LBL_REFRESH']).'&nbsp;'.$current_module_strings['LBL_REFRESH'].'</a>&nbsp;&nbsp;<a href="javascript: toggleDisplay(\'lsbo_edit\');" class="tabFormAdvLink">'.SugarThemeRegistry::current()->getImage('edit', 'border="0"  align="absmiddle"', null, null, '.gif', $mod_strings['LBL_EDIT']).'&nbsp;'. $current_module_strings['LBL_EDIT'].'</a>&nbsp;&nbsp;'.$extra_tools.'</div>'; ?>

<?php
echo '<span onmouseover="this.style.cursor=\'move\'" id="chart_handle_' . $this->order . '">' . get_form_header($current_module_strings['LBL_LEAD_SOURCE_BY_OUTCOME'], $tools, false) . '</span>';

        if (empty($_SESSION['lsbo_ids'])) {
            $_SESSION['lsbo_ids'] = "";
        } ?>

<p>
<div id='lsbo_edit' style='display: none;'>
<form action="index.php" method="post" >
<input type="hidden" name="module" value="<?php echo $currentModule; ?>">
<input type="hidden" name="action" value="<?php echo $action; ?>">
<input type="hidden" name="lsbo_refresh" value="true">
<table cellpadding="0" cellspacing="0" border="0" class="edit view" align="center">
<tr>
	<td valign='top' nowrap><b><?php echo $current_module_strings['LBL_LEAD_SOURCES']; ?></b></td>
	<td valign='top'><select name="lsbo_lead_sources[]" multiple size='3'><?php echo get_select_options_with_id($app_list_strings['lead_source_dom'], $selected_datax); ?></select></td>
</tr>

<tr>
	<td valign='top' nowrap><b><?php echo $current_module_strings['LBL_USERS']; ?></b></td>
	<td valign='top'><select name="lsbo_ids[]" multiple size='3'><?php echo get_select_options_with_id(get_user_array(false), $ids); ?></select></td>
</tr>

<tr>
<?php
global $app_strings; ?>
	<td align="right" colspan="2"> <input class="button" type="submit" title="<?php echo $app_strings['LBL_SELECT_BUTTON_TITLE']; ?>" value="<?php echo $app_strings['LBL_SELECT_BUTTON_LABEL']?>" /><input class="button" onClick="javascript: toggleDisplay('lsbo_edit');" type="button" title="<?php echo $app_strings['LBL_CANCEL_BUTTON_TITLE']; ?>" accessKey="<?php echo $app_strings['LBL_CANCEL_BUTTON_KEY']; ?>" value="<?php echo $app_strings['LBL_CANCEL_BUTTON_LABEL']?>"/></td>
	</tr>
</table>
</form>
</div>
</p>
<?php

echo "<p align='center'>".$this->gen_xml($datax, $ids, $cache_file_name, $refresh, $current_module_strings)."</p>";
        echo "<P align='center'><span class='chartFootnote'>".$current_module_strings['LBL_LEAD_SOURCE_BY_OUTCOME_DESC']."</span></P>";


        if (file_exists($cache_file_name)) {
            global  $timedate;
            $file_date = $timedate->asUser($timedate->fromTimestamp(filemtime($cache_file_name)));
        } else {
            $file_date = '';
        } ?>
<span class='chartFootnote'>
<p align="right"><i><?php  echo $current_module_strings['LBL_CREATED_ON'].' '.$file_date; ?></i></p>
</span>
<?php
    }




    /**
    * Creates lead_source_by_outcome pipeline image as a HORIZONAL accumlated bar graph for multiple users.
    * param $datay- the lead source data to display in the x-axis
    * param $ids - list of assigned users of opps to find
    * param $cache_file_name - file name to write image to
    * param $refresh - boolean whether to rebuild image if exists
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
    * All Rights Reserved..
    * Contributor(s): ______________________________________..
    */
    public function gen_xml(
        $datay = array('foo', 'bar'),
        $user_id = array('1'),
        $cache_file_name = 'a_file',
        $refresh = false,
        $current_module_strings = null
    ) {
        global $app_strings, $charset, $lang, $barChartColors,$app_list_strings, $current_user, $current_language;

        // set $current_module_strings to 'Charts' module strings by default
        if (empty($current_module_strings)) {
            $current_module_strings = return_module_language($current_language, 'Charts');
        }

        $kDelim = $current_user->getPreference('num_grp_sep');

        if (!file_exists($cache_file_name) || $refresh == true) {
            $GLOBALS['log']->debug("datay is:");
            $GLOBALS['log']->debug($datay);
            $GLOBALS['log']->debug("user_id is: ");
            $GLOBALS['log']->debug($user_id);
            $GLOBALS['log']->debug("cache_file_name is: $cache_file_name");
            $opp = BeanFactory::newBean('Opportunities');
            $where="";
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

            //build the where clause for the query that matches $datay
            $count = is_countable($datay) ? count($datay) : 0;
            $datayArr = array();
            if ($count>0) {
                foreach ($datay as $key=>$value) {
                    $datayArr[] = "'".$key."'";
                }
                $datayArr = implode(",", $datayArr);
                $where .= "AND opportunities.lead_source IN	($datayArr) ";
            }
            $query = "SELECT lead_source,sales_stage,sum(amount_usdollar/1000) as total,count(*) as opp_count FROM opportunities ";
            $query .= "WHERE " .$where." AND opportunities.deleted=0 ";
            $query .= " GROUP BY sales_stage,lead_source ORDER BY lead_source,sales_stage";
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
            $fileContents = '     <yData defaultAltText="'.$current_module_strings['LBL_ROLLOVER_DETAILS'].'">'."\n";
            $leadSourceArr = array();
            while ($row = $opp->db->fetchByAssoc($result, false)) {
                if ($row['total']*$div<=100) {
                    $sum = round($row['total']*$div, 2);
                } else {
                    $sum = round($row['total']*$div);
                }
                if ($row['lead_source'] == '') {
                    $row['lead_source'] = $current_module_strings['NTC_NO_LEGENDS'];
                }
                if ($row['sales_stage'] == 'Closed Won' || $row['sales_stage'] == 'Closed Lost') {
                    $salesStage = $row['sales_stage'];
                    $salesStageT = $app_list_strings['sales_stage_dom'][$row['sales_stage']];
                } else {
                    $salesStage = "Other";
                    $salesStageT = $other;
                }
                if (!isset($leadSourceArr[$row['lead_source']]['row_total'])) {
                    $leadSourceArr[$row['lead_source']]['row_total']=0;
                }
                $leadSourceArr[$row['lead_source']][$salesStage]['opp_count'][] = $row['opp_count'];
                $leadSourceArr[$row['lead_source']][$salesStage]['total'][] = $sum;
                $leadSourceArr[$row['lead_source']]['outcome'][$salesStage]=$salesStageT;
                $leadSourceArr[$row['lead_source']]['row_total'] += $sum;

                $total += $sum;
            }
            foreach ($datay as $key=>$translation) {
                if ($key == '') {
                    $key = $current_module_strings['NTC_NO_LEGENDS'];
                    $translation = $current_module_strings['NTC_NO_LEGENDS'];
                }
                if (!isset($leadSourceArr[$key])) {
                    $leadSourceArr[$key] = $key;
                }
                if (isset($leadSourceArr[$key]['row_total'])) {
                    $rowTotalArr[]=$leadSourceArr[$key]['row_total'];
                }
                if (isset($leadSourceArr[$key]['row_total']) && $leadSourceArr[$key]['row_total']>100) {
                    $leadSourceArr[$key]['row_total'] = round($leadSourceArr[$key]['row_total']);
                }
                $fileContents .= '          <dataRow title="'.$translation.'" endLabel="'.currency_format_number($leadSourceArr[$key]['row_total'], array('currency_symbol' => true)) . '">'."\n";
                if (is_array($leadSourceArr[$key]['outcome'])) {
                    foreach ($leadSourceArr[$key]['outcome'] as $outcome=>$outcome_translation) {
                        $fileContents .= '               <bar id="'.$outcome.'" totalSize="'.array_sum($leadSourceArr[$key][$outcome]['total']).'" altText="'.format_number(array_sum($leadSourceArr[$key][$outcome]['opp_count']), 0, 0).' '.$current_module_strings['LBL_OPPS_WORTH'].' '.currency_format_number(array_sum($leadSourceArr[$key][$outcome]['total']), array('currency_symbol' => true)).$current_module_strings['LBL_OPP_THOUSANDS'].' '.$current_module_strings['LBL_OPPS_OUTCOME'].' '.$outcome_translation.'" url="index.php?module=Opportunities&action=index&lead_source='.$key.'&sales_stage='.urlencode($outcome).'&query=true&searchFormTab=advanced_search"/>'."\n";
                    }
                }
                $fileContents .= '          </dataRow>'."\n";
            }
            $fileContents .= '     </yData>'."\n";
            $max = get_max($rowTotalArr);
            $fileContents .= '     <xData min="0" max="'.$max.'" length="10" kDelim="'.$kDelim.'" prefix="'.$symbol.'" suffix=""/>' . "\n";
            $fileContents .= '     <colorLegend status="on">'."\n";
            $i=0;

            foreach ($salesStages as $outcome=>$outcome_translation) {
                $color = generate_graphcolor($outcome, $i);
                $fileContents .= '          <mapping id="'.$outcome.'" name="'.$outcome_translation.'" color="'.$color.'"/>'."\n";
                $i++;
            }
            $fileContents .= '     </colorLegend>'."\n";
            $fileContents .= '     <graphInfo>'."\n";
            $fileContents .= '          <![CDATA['.$current_module_strings['LBL_OPP_SIZE'].' '.$symbol.'1'.$current_module_strings['LBL_OPP_THOUSANDS'].']]>'."\n";
            $fileContents .= '     </graphInfo>'."\n";
            $fileContents .= '     <chartColors ';
            foreach ($barChartColors as $key => $value) {
                $fileContents .= ' '.$key.'='.'"'.$value.'" ';
            }
            $fileContents .= ' />'."\n";
            $fileContents .= '</graphData>'."\n";
            $total = round($total, 2);
            $title = '<graphData title="'.$current_module_strings['LBL_ALL_OPPORTUNITIES'].currency_format_number($total, array('currency_symbol' => true)).$app_strings['LBL_THOUSANDS_SYMBOL'].'">'."\n";
            $fileContents = $title.$fileContents;

            save_xml_file($cache_file_name, $fileContents);
        }
        $return = create_chart('hBarF', $cache_file_name);
        return $return;
    }


    public function constructQuery()
    {
        global $current_user;
        global $app_list_strings;

        $tempx = array();
        $datax = array();
        $selected_datax = array();
        //get list of sales stage keys to display

        $tempx = $current_user->getPreference('lsbo_lead_sources');
        if (!empty($lsbo_lead_sources) && (is_countable($lsbo_lead_sources) ? count($lsbo_lead_sources) : 0) > 0 && !isset($_REQUEST['lsbo_lead_sources'])) {
            $GLOBALS['log']->fatal("user->getPreference('lsbo_lead_sources') is:");
            $GLOBALS['log']->fatal($tempx);
        } elseif (isset($_REQUEST['lsbo_lead_sources']) && (is_countable($_REQUEST['lsbo_lead_sources']) ? count($_REQUEST['lsbo_lead_sources']) : 0) > 0) {
            $tempx = $_REQUEST['lsbo_lead_sources'];
            $current_user->setPreference('lsbo_lead_sources', $_REQUEST['lsbo_lead_sources']);
            $GLOBALS['log']->fatal("_REQUEST['lsbo_lead_sources'] is:");
            $GLOBALS['log']->fatal($_REQUEST['lsbo_lead_sources']);
            $GLOBALS['log']->fatal("user->getPreference('lsbo_lead_sources') is:");
            $GLOBALS['log']->fatal($current_user->getPreference('lsbo_lead_sources'));
        }
        //set $datax using selected sales stage keys
        if (!empty($tempx) && (is_countable($tempx) ? count($tempx) : 0) > 0) {
            foreach ($tempx as $key) {
                $datax[$key] = $app_list_strings['lead_source_dom'][$key];
                array_push($selected_datax, $key);
            }
        } else {
            $datax = $app_list_strings['lead_source_dom'];
            $selected_datax = array_keys($app_list_strings['lead_source_dom']);
        }

        $datay = $datax;

        $ids =$current_user->getPreference('lsbo_ids');
        //get list of user ids for which to display data
        if (!empty($ids) && (is_countable($ids) ? count($ids) : 0) != 0 && !isset($_REQUEST['lsbo_ids'])) {
            $GLOBALS['log']->debug("_SESSION['lsbo_ids'] is:");
            $GLOBALS['log']->debug($ids);
        } elseif (isset($_REQUEST['lsbo_ids']) && (is_countable($_REQUEST['lsbo_ids']) ? count($_REQUEST['lsbo_ids']) : 0) > 0) {
            $ids = $_REQUEST['lsbo_ids'];
            $current_user->setPreference('lsbo_ids', $_REQUEST['lsbo_ids']);
            $GLOBALS['log']->debug("_REQUEST['lsbo_ids'] is:");
            $GLOBALS['log']->debug($_REQUEST['lsbo_ids']);
            $GLOBALS['log']->debug("user->getPreference('lsbo_ids') is:");
            $GLOBALS['log']->debug($current_user->getPreference('lsbo_ids'));
        } else {
            $ids = get_user_array(false);
            $ids = array_keys($ids);
        }

        $user_id = $ids;

        $opp = BeanFactory::newBean('Opportunities');
        $where="";
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

        //build the where clause for the query that matches $datay
        $count = is_countable($datay) ? count($datay) : 0;
        $datayArr = array();
        if ($count>0) {
            foreach ($datay as $key=>$value) {
                $datayArr[] = "'".$key."'";
            }
            $datayArr = implode(",", $datayArr);
            $where .= "AND opportunities.lead_source IN	($datayArr) ";
        }
        $query = "SELECT lead_source,sales_stage,sum(amount_usdollar/1000) as total,count(*) as opp_count FROM opportunities ";
        $query .= "WHERE " .$where." AND opportunities.deleted=0 ";
        $query .= " GROUP BY sales_stage,lead_source ORDER BY lead_source,sales_stage";

        return $query;
    }

    public function constructGroupBy()
    {
        return array( 'lead_source', 'sales_stage' );
    }
}
