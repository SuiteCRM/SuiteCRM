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

require_once('include/SugarCharts/SugarChartFactory.php');

#[\AllowDynamicProperties]
class campaign_charts
{
    /**
    * Creates opportunity pipeline image as a VERTICAL accumlated bar graph for multiple users.
    * param $datax- the month data to display in the x-axis
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
    * All Rights Reserved..
    * Contributor(s): ______________________________________..
    */

    /**
     * @param array $datay
     * @param array $targets
     * @param null|string $campaign_id
     * @param string $cache_file_name
     * @param bool $refresh
     * @param string $marketing_id
     * @return string
     */
    public function campaign_response_by_activity_type($datay= array(), $targets=array(), $campaign_id= null, $cache_file_name='a_file', $refresh=false, $marketing_id='')
    {

        global $app_strings, $mod_strings, $charset, $lang, $barChartColors,$app_list_strings;

        $targeted = [];

        if ($campaign_id) {
            $sugarChart = SugarChartFactory::getInstance('', 'Reports');
            if (!$sugarChart) {
                return false;
            }
            $xmlFile = $sugarChart->getXMLFileName($campaign_id);

            if (!file_exists($xmlFile) || $refresh == true) {
                $GLOBALS['log']->debug("datay is:");
                $GLOBALS['log']->debug($datay);
                $GLOBALS['log']->debug("user_id is: ");
                $GLOBALS['log']->debug("cache_file_name is: $xmlFile");

                $focus = BeanFactory::newBean('Campaigns');

                $query = "SELECT activity_type,target_type, count(*) hits ";
                $query .= " FROM campaign_log ";
                $query .= " WHERE campaign_id = '$campaign_id' AND archived=0 AND deleted=0";

                //if $marketing id is specified, then lets filter the chart by the value
                if (!empty($marketing_id)) {
                    $query .= " AND marketing_id ='$marketing_id'";
                }

                $query .= " GROUP BY  activity_type, target_type";
                $query .= " ORDER BY  activity_type, target_type";
                $result = $focus->db->query($query);
                //$camp_data=$focus->db->fetchByAssoc($result);
                $camp_data = array();
                $leadSourceArr = array();
                $total = 0;
                $total_targeted = 0;
                $rowTotalArr = array();
                $rowTotalArr[] = 0;
                while ($row = $focus->db->fetchByAssoc($result)) {
                    if (!isset($leadSourceArr[$row['activity_type']]['row_total'])) {
                        $leadSourceArr[$row['activity_type']]['row_total'] = 0;
                    }

                    $leadSourceArr[$row['activity_type']][$row['target_type']]['hits'][] = $row['hits'];
                    $leadSourceArr[$row['activity_type']][$row['target_type']]['total'][] = $row['hits'];
                    $leadSourceArr[$row['activity_type']]['outcome'][$row['target_type']] = $row['target_type'];
                    $leadSourceArr[$row['activity_type']]['row_total'] += $row['hits'];

                    if (!isset($leadSourceArr['all_activities'][$row['target_type']])) {
                        $leadSourceArr['all_activities'][$row['target_type']] = array('total' => 0);
                    }

                    $leadSourceArr['all_activities'][$row['target_type']]['total'] += $row['hits'];

                    $total += $row['hits'];
                    if ($row['activity_type'] == 'targeted') {
                        $targeted[$row['target_type']] = $row['hits'];
                        $total_targeted += $row['hits'];
                    }
                }

                foreach ($datay as $key => $translation) {
                    if ($key == '') {
                        //$key = $mod_strings['NTC_NO_LEGENDS'];
                        $key = 'None';
                        $translation = $mod_strings['NTC_NO_LEGENDS'];
                    }
                    if (!isset($leadSourceArr[$key])) {
                        $leadSourceArr[$key] = $key;
                    }


                    if (is_array($leadSourceArr[$key]) && isset($leadSourceArr[$key]['row_total'])) {
                        $rowTotalArr[] = $leadSourceArr[$key]['row_total'];
                    }
                    if (is_array($leadSourceArr[$key]) && isset($leadSourceArr[$key]['row_total']) && $leadSourceArr[$key]['row_total'] > 100) {
                        $leadSourceArr[$key]['row_total'] = round($leadSourceArr[$key]['row_total']);
                    }
                    $camp_data[$translation] = array();
                    foreach ($targets as $outcome => $outcome_translation) {
                        //create alternate text.
                        $alttext = ' ';
                        if (isset($targeted) && isset($targeted[$outcome]) && !empty($targeted[$outcome])) {
                            $alttext = $targets[$outcome] . ': ' . $mod_strings['LBL_TARGETED'] . ' ' . $targeted[$outcome] . ', ' . $mod_strings['LBL_TOTAL_TARGETED'] . ' ' . $total_targeted . ".";
                        }
                        if ($key != 'targeted') {
                            $hits = (isset($leadSourceArr[$key][$outcome]) && is_array($leadSourceArr[$key][$outcome]) && is_array($leadSourceArr[$key][$outcome]['hits'])) ? array_sum($leadSourceArr[$key][$outcome]['hits']) : 0;
                            $alttext .= " $translation " . $hits;
                        }
                        $count = (isset($leadSourceArr[$key][$outcome]) && is_array($leadSourceArr[$key][$outcome]) && is_array($leadSourceArr[$key][$outcome]['total'])) ? array_sum($leadSourceArr[$key][$outcome]['total']) : 0;
                        $camp_data[$translation][$outcome] =
                            array(
                                "numerical_value" => $count,
                                "group_text" => $translation,
                                "group_key" => "",
                                "count" => (string)($count),
                                "group_label" => $alttext,
                                "numerical_label" => "Hits",
                                "numerical_key" => "hits",
                                "module" => 'Campaigns',
                                "group_base_text" => $outcome,
                                "link" => $key
                            );
                    }
                }

                // Since this isn't a standard report chart (with report defs), set the group_by manually so the chart bars show properly
                $sugarChart->group_by = array('activity_type', 'target_type');

                if ($camp_data) {
                    $sugarChart->setData($camp_data);
                } else {
                    $sugarChart->setData(array());
                }

                $sugarChart->setProperties($mod_strings['LBL_CAMPAIGN_RESPONSE_BY_RECIPIENT_ACTIVITY'], "", 'horizontal group by chart');
                $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());
            }

            $width = '100%';
            $return = '';
            $return .= $sugarChart->display($campaign_id, $xmlFile, $width, '480', "");
        } else {
            $GLOBALS['log']->fatal('no campaign id');
            $return = false;
        }
        return $return;
    }

    /**
     * Campaign roi computations.
    *
     * @param array $datay
     * @param array $targets
     * @param string|null $campaign_id
     * @param string $cache_file_name
     * @param bool $refresh
     * @param string $marketing_id
     * @param bool $is_dashlet
     * @param string $dashlet_id
     * @return string
     */public function campaign_response_roi(
        $datay= array(),
        $targets=array(),
        $campaign_id= null,
        $cache_file_name='a_file',
        $refresh=false,
        $marketing_id='',
        $is_dashlet=false,
        $dashlet_id=''
        ) {

        global $app_strings,$mod_strings, $current_module_strings, $charset, $lang, $app_list_strings, $current_language,$sugar_config;

        $not_empty = false;
        $focus = null;
        $opp_data1 = [];


        if ($is_dashlet) {
            $mod_strings = return_module_language($current_language, 'Campaigns');
        }

        if ((!file_exists($cache_file_name) || $refresh == true) && $campaign_id) {
            $GLOBALS['log']->debug("datay is:");
            $GLOBALS['log']->debug($datay);
            $GLOBALS['log']->debug("user_id is: ");
            $GLOBALS['log']->debug("cache_file_name is: $cache_file_name");

            $focus = BeanFactory::newBean('Campaigns');
            $focus->retrieve($campaign_id);
            $opp_count=0;
            $opp_query  = "select count(*) opp_count,sum(" . DBManagerFactory::getInstance()->convert("amount_usdollar", "IFNULL", array(0)).")  total_value";
            $opp_query .= " from opportunities";
            $opp_query .= " where campaign_id='$campaign_id'";
            $opp_query .= " and sales_stage='Prospecting'";
            $opp_query .= " and deleted=0";

            $opp_result=$focus->db->query($opp_query);
            $opp_data=$focus->db->fetchByAssoc($opp_result);
//            if (empty($opp_data['opp_count'])) $opp_data['opp_count']=0;
            if (empty($opp_data['total_value'])) {
                $opp_data['total_value']=0;
            }

            //report query
            $opp_query1  = "select SUM(opp.amount) as revenue";
            $opp_query1 .= " from opportunities opp";
            $opp_query1 .= " right join campaigns camp on camp.id = opp.campaign_id";
            $opp_query1 .= " where opp.sales_stage = 'Closed Won'and camp.id='$campaign_id' and opp.deleted=0";
            $opp_query1 .= " group by camp.name";

            $opp_result1=$focus->db->query($opp_query1);
            $opp_data1=$focus->db->fetchByAssoc($opp_result1);

            //if (empty($opp_data1[]))
            if (empty($opp_data1['revenue'])) {
                $opp_data1[$mod_strings['LBL_ROI_CHART_REVENUE']] = 0;
                unset($opp_data1['revenue']);
            } else {
                $opp_data1[$mod_strings['LBL_ROI_CHART_REVENUE']] = $opp_data1['revenue'];
                unset($opp_data1['revenue']);
                $not_empty = true;
            }

            $camp_query1  = "select camp.name, SUM(camp.actual_cost) as investment,SUM(camp.budget) as budget,SUM(camp.expected_revenue) as expected_revenue";
            $camp_query1 .= " from campaigns camp";
            $camp_query1 .= " where camp.id='$campaign_id'";
            $camp_query1 .= " group by camp.name";

            $camp_result1=$focus->db->query($camp_query1);
            $camp_data1=$focus->db->fetchByAssoc($camp_result1);
            //query needs to be lowercase, but array keys need to be propercased, as these are used in
            //chart to display legend

            if (empty($camp_data1['investment'])) {
                $camp_data1['investment'] = 0;
            } else {
                $not_empty = true;
            }
            if (empty($camp_data1['budget'])) {
                $camp_data1['budget'] = 0;
            } else {
                $not_empty = true;
            }
            if (empty($camp_data1['expected_revenue'])) {
                $camp_data1['expected_revenue'] = 0;
            } else {
                $not_empty = true;
            }

            $opp_data1[$mod_strings['LBL_ROI_CHART_INVESTMENT']]=$camp_data1['investment'];
            $opp_data1[$mod_strings['LBL_ROI_CHART_BUDGET']]=$camp_data1['budget'];
            $opp_data1[$mod_strings['LBL_ROI_CHART_EXPECTED_REVENUE']]=$camp_data1['expected_revenue'];


            $query = "SELECT activity_type,target_type, count(*) hits ";
            $query.= " FROM campaign_log ";
            $query.= " WHERE campaign_id = '$campaign_id' AND archived=0 AND deleted=0";
            //if $marketing id is specified, then lets filter the chart by the value
            if (!empty($marketing_id)) {
                $query.= " AND marketing_id ='$marketing_id'";
            }
            $query.= " GROUP BY  activity_type, target_type";
            $query.= " ORDER BY  activity_type, target_type";
            $result = $focus->db->query($query);

            $leadSourceArr = array();
            $total=0;
            $total_targeted=0;
        } elseif (!$campaign_id) {
            $GLOBALS['log']->fatal('no campaign id');
        }

        global $current_user;
        $user_id = $current_user->id;


        $width = '100%';

        $return = '';
        if (!$is_dashlet) {
            $return .= '<br />';
        }


        $currency_id = $focus->currency_id;
        $currency_symbol = $sugar_config['default_currency_symbol'];
        if (!empty($currency_id)) {
            $cur = BeanFactory::newBean('Currencies');
            $cur->retrieve($currency_id);
            $currency_symbol = $cur->symbol;
        }


        $sugarChart = SugarChartFactory::getInstance();
        if (!$sugarChart) {
            return false;
        }
        $sugarChart->is_currency = true;
        $sugarChart->currency_symbol = $currency_symbol;

        if ($not_empty) {
            $sugarChart->setData($opp_data1);
        } else {
            $sugarChart->setData(array());
        }
        $sugarChart->setProperties($mod_strings['LBL_CAMPAIGN_RETURN_ON_INVESTMENT'], $mod_strings['LBL_AMOUNT_IN'].$currency_symbol, 'bar chart');

        if (!$is_dashlet) {
            $xmlFile = $sugarChart->getXMLFileName('roi_details_chart');
            $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());
            $return .= $sugarChart->display('roi_details_chart', $xmlFile, $width, '480');
        } else {
            $xmlFile = $sugarChart->getXMLFileName($dashlet_id);
            $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());
            $return .= $sugarChart->display($dashlet_id, $xmlFile, $width, '480');
        }

        return $return;
    }

    //THis is a copy of the campaign_response_roi for rgraph so that the data is separate from the chart / presentation
    //this will need refactored later rather than a cut n paste job like this
    //perhaps add another boolean to the parameter list to return just the data or the chart
    public function campaign_response_roi_data($datay= array(), $targets=array(), $campaign_id = null, $cache_file_name='a_file', $refresh=false, $marketing_id='', $is_dashlet=false, $dashlet_id='')
    {

        global $app_strings,$mod_strings, $current_module_strings, $charset, $lang, $app_list_strings, $current_language,$sugar_config;

        $campaign_id = (int) $campaign_id;
        if (!$campaign_id) {
            $GLOBALS['log']->debug('campaign response rio data hasn\'t got campaign id');
            return false;
        }

        $not_empty = false;
        $focus = null;
        $opp_data1 = [];

        if ($is_dashlet) {
            $mod_strings = return_module_language($current_language, 'Campaigns');
        }

        if (!file_exists($cache_file_name) || $refresh == true) {
            $GLOBALS['log']->debug("datay is:");
            $GLOBALS['log']->debug($datay);
            $GLOBALS['log']->debug("user_id is: ");
            $GLOBALS['log']->debug("cache_file_name is: $cache_file_name");

            $focus = BeanFactory::newBean('Campaigns');
            $focus->retrieve($campaign_id);
            $opp_count=0;
            $opp_query  = "select count(*) opp_count,sum(" . DBManagerFactory::getInstance()->convert("amount_usdollar", "IFNULL", array(0)).")  total_value";
            $opp_query .= " from opportunities";
            $opp_query .= " where campaign_id='$campaign_id'";
            $opp_query .= " and sales_stage='Prospecting'";
            $opp_query .= " and deleted=0";

            $opp_result=$focus->db->query($opp_query);
            $opp_data=$focus->db->fetchByAssoc($opp_result);
//            if (empty($opp_data['opp_count'])) $opp_data['opp_count']=0;
            if (empty($opp_data['total_value'])) {
                $opp_data['total_value']=0;
            }

            //report query
            $opp_query1  = "select SUM(opp.amount) as revenue";
            $opp_query1 .= " from opportunities opp";
            $opp_query1 .= " right join campaigns camp on camp.id = opp.campaign_id";
            $opp_query1 .= " where opp.sales_stage = 'Closed Won'and camp.id='$campaign_id' and opp.deleted=0";
            $opp_query1 .= " group by camp.name";

            $opp_result1=$focus->db->query($opp_query1);
            $opp_data1=$focus->db->fetchByAssoc($opp_result1);

            //if (empty($opp_data1[]))
            if (empty($opp_data1['revenue'])) {
                $opp_data1[$mod_strings['LBL_ROI_CHART_REVENUE']] = 0;
                unset($opp_data1['revenue']);
            } else {
                $opp_data1[$mod_strings['LBL_ROI_CHART_REVENUE']] = $opp_data1['revenue'];
                unset($opp_data1['revenue']);
                $not_empty = true;
            }

            $camp_query1  = "select camp.name, SUM(camp.actual_cost) as investment,SUM(camp.budget) as budget,SUM(camp.expected_revenue) as expected_revenue";
            $camp_query1 .= " from campaigns camp";
            $camp_query1 .= " where camp.id='$campaign_id'";
            $camp_query1 .= " group by camp.name";

            $camp_result1=$focus->db->query($camp_query1);
            $camp_data1=$focus->db->fetchByAssoc($camp_result1);
            //query needs to be lowercase, but array keys need to be propercased, as these are used in
            //chart to display legend

            if (empty($camp_data1['investment'])) {
                $camp_data1['investment'] = 0;
            } else {
                $not_empty = true;
            }
            if (empty($camp_data1['budget'])) {
                $camp_data1['budget'] = 0;
            } else {
                $not_empty = true;
            }
            if (empty($camp_data1['expected_revenue'])) {
                $camp_data1['expected_revenue'] = 0;
            } else {
                $not_empty = true;
            }

            $opp_data1[$mod_strings['LBL_ROI_CHART_INVESTMENT']]=$camp_data1['investment'];
            $opp_data1[$mod_strings['LBL_ROI_CHART_BUDGET']]=$camp_data1['budget'];
            $opp_data1[$mod_strings['LBL_ROI_CHART_EXPECTED_REVENUE']]=$camp_data1['expected_revenue'];


            $query = "SELECT activity_type,target_type, count(*) hits ";
            $query.= " FROM campaign_log ";
            $query.= " WHERE campaign_id = '$campaign_id' AND archived=0 AND deleted=0";
            //if $marketing id is specified, then lets filter the chart by the value
            if (!empty($marketing_id)) {
                $query.= " AND marketing_id ='$marketing_id'";
            }
            $query.= " GROUP BY  activity_type, target_type";
            $query.= " ORDER BY  activity_type, target_type";
            $result = $focus->db->query($query);

            $leadSourceArr = array();
            $total=0;
            $total_targeted=0;
        }

        global $current_user;
        $user_id = $current_user->id;


        $width = '100%';

        $return = '';
        if (!$is_dashlet) {
            $return .= '<br />';
        }


        $currency_id = $focus->currency_id;
        $currency_symbol = $sugar_config['default_currency_symbol'];
        if (!empty($currency_id)) {
            $cur = BeanFactory::newBean('Currencies');
            $cur->retrieve($currency_id);
            $currency_symbol = $cur->symbol;
        }


        $sugarChart = SugarChartFactory::getInstance();
        if (!$sugarChart) {
            return false;
        }
        $sugarChart->is_currency = true;
        $sugarChart->currency_symbol = $currency_symbol;

        if ($not_empty) {
            $sugarChart->setData($opp_data1);
        } else {
            $sugarChart->setData(array());
        }
        $sugarChart->setProperties($mod_strings['LBL_CAMPAIGN_RETURN_ON_INVESTMENT'], $mod_strings['LBL_AMOUNT_IN'].$currency_symbol, 'bar chart');

        if (!$is_dashlet) {
            $xmlFile = $sugarChart->getXMLFileName('roi_details_chart');
            $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());
            $return .= $sugarChart->display('roi_details_chart', $xmlFile, $width, '480');
        } else {
            $xmlFile = $sugarChart->getXMLFileName($dashlet_id);
            $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());
            $return .= $sugarChart->display($dashlet_id, $xmlFile, $width, '480');
        }

        return $return;
    }
}// end charts class
