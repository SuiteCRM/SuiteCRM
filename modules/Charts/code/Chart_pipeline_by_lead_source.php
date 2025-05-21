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
class Chart_pipeline_by_lead_source
{
    public $order = 0;
    public $modules = array('Opportunities');

    public function __construct()
    {
    }




    public function draw($extra_tools)
    {
        global $app_list_strings, $current_language, $ids, $sugar_config ,$theme;
        $current_module_strings = return_module_language($current_language, 'Charts');


        if (isset($_REQUEST['pbls_refresh'])) {
            $refresh = $_REQUEST['pbls_refresh'];
        } else {
            $refresh = false;
        }

        $tempx = array();
        $datax = array();
        $selected_datax = array();
        //get list of sales stage keys to display
        global $current_user;
        $user_tempx = $current_user->getPreference('pbls_lead_sources');
        if (!empty($user_tempx) && (is_countable($user_tempx) ? count($user_tempx) : 0) > 0 && !isset($_REQUEST['pbls_lead_sources'])) {
            $tempx = $user_tempx;
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_lead_sources'] is:");
            $GLOBALS['log']->debug($user_tempx);
        } elseif (isset($_REQUEST['pbls_lead_sources']) && (is_countable($_REQUEST['pbls_lead_sources']) ? count($_REQUEST['pbls_lead_sources']) : 0) > 0) {
            $tempx = $_REQUEST['pbls_lead_sources'];
            $current_user->setPreference('pbls_lead_sources', $_REQUEST['pbls_lead_sources']);
            $GLOBALS['log']->debug("_REQUEST['pbls_lead_sources'] is:");
            $GLOBALS['log']->debug($_REQUEST['pbls_lead_sources']);
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_lead_sources'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('pbls_lead_sources'));
        }

        //set $datax using selected sales stage keys
        if ((is_countable($tempx) ? count($tempx) : 0) > 0) {
            foreach ($tempx as $key) {
                $datax[$key] = $app_list_strings['lead_source_dom'][$key];
                array_push($selected_datax, $key);
            }
        } else {
            $datax = $app_list_strings['lead_source_dom'];
            $selected_datax = array_keys($app_list_strings['lead_source_dom']);
        }
        $GLOBALS['log']->debug("datax is:");
        $GLOBALS['log']->debug($datax);

        $ids = array();
        $user_ids = $current_user->getPreference('pbls_ids');
        //get list of user ids for which to display data
        if (!empty($user_ids) && (is_countable($user_ids) ? count($user_ids) : 0) != 0 && !isset($_REQUEST['pbls_ids'])) {
            if (isset($_SESSION['pbls_ids'])) {
                $ids = $_SESSION['pbls_ids'];
            }
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_ids'] is:");
            $GLOBALS['log']->debug($user_ids);
        } elseif (isset($_REQUEST['pbls_ids']) && (is_countable($_REQUEST['pbls_ids']) ? count($_REQUEST['pbls_ids']) : 0) > 0) {
            $ids = $_REQUEST['pbls_ids'];
            $current_user->setPreference('pbls_ids', $ids);
            $GLOBALS['log']->debug("_REQUEST['pbls_ids'] is:");
            $GLOBALS['log']->debug($_REQUEST['pbls_ids']);
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_ids'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('pbls_ids'));
        } else {
            $ids = get_user_array(false);
            $ids = array_keys($ids);
        }

        //create unique prefix based on selected users for image files
        $id_hash = '1';
        if (isset($ids) && is_array($ids)) {
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
        $cache_file_name	= sugar_cached("xml/").$current_user->getUserPrivGuid()."_pipeline_by_lead_source_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";

        $GLOBALS['log']->debug("cache file name is: $cache_file_name");
        global $currentModule,$action;
        $tools='<div align="right"><a href="index.php?module='.$currentModule.'&action='. $action .'&pbls_refresh=true" class="tabFormAdvLink">'.SugarThemeRegistry::current()->getImage('refresh', 'border="0" align="absmiddle"', null, null, '.gif', $mod_strings['LBL_REFRESH']).'&nbsp;'.$current_module_strings['LBL_REFRESH'].'</a>&nbsp;&nbsp;<a href="javascript: toggleDisplay(\'pbls_edit\');" class="tabFormAdvLink">'.SugarThemeRegistry::current()->getImage('edit', 'border="0"  align="absmiddle"', null, null, '.gif', $mod_strings['LBL_EDIT']).'&nbsp;'. $current_module_strings['LBL_EDIT'].'</a>&nbsp;&nbsp;'.$extra_tools.'</div>'; ?>

<?php
echo '<span onmouseover="this.style.cursor=\'move\'" id="chart_handle_' . $this->order . '">' . get_form_header($current_module_strings['LBL_LEAD_SOURCE_FORM_TITLE'], $tools, false) . '</span>';
        if (empty($_SESSION['pbls_lead_sources'])) {
            $_SESSION['pbls_lead_sources'] = "";
        }
        if (empty($_SESSION['pbls_ids'])) {
            $_SESSION['pbls_ids'] = "";
        } ?>

<p>
<div id='pbls_edit' style='display: none;'>
<form action="index.php" method="post" >
<input type="hidden" name="module" value="<?php echo $currentModule; ?>">
<input type="hidden" name="action" value="<?php echo $action; ?>">
<input type="hidden" name="pbls_refresh" value="true">
<table cellpadding="0" cellspacing="0" border="0" class="edit view" align="center">
<tr>
	<td valign='top' nowrap><b><?php echo $current_module_strings['LBL_LEAD_SOURCES']; ?></b></td>
	<td valign='top'><select name="pbls_lead_sources[]" multiple size='3'><?php echo get_select_options_with_id($app_list_strings['lead_source_dom'], $selected_datax); ?></select></td>
	<td valign='top' nowrap><b><?php echo $current_module_strings['LBL_USERS']; ?></b></td>
	<td valign='top'><select name="pbls_ids[]" multiple size='3'><?php $allUsers = get_user_array(false);
        echo get_select_options_with_id($allUsers, $ids); ?></select></td>
<?php
global $app_strings; ?>
	<td align="right" valign="top"><input class="button" type="submit" title="<?php echo $app_strings['LBL_SELECT_BUTTON_TITLE']; ?>" value="<?php echo $app_strings['LBL_SELECT_BUTTON_LABEL']?>" /><input class="button" onClick="javascript: toggleDisplay('pbls_edit');" type="button" title="<?php echo $app_strings['LBL_CANCEL_BUTTON_TITLE']; ?>" accessKey="<?php echo $app_strings['LBL_CANCEL_BUTTON_KEY']; ?>" value="<?php echo $app_strings['LBL_CANCEL_BUTTON_LABEL']?>"/></td>
</tr>
</table>
</form>
</div>
</p>
<?php
// draw table
echo "<p align='center'>".$this->gen_xml($datax, $ids, $cache_file_name, $refresh, $current_module_strings)."</p>";
        echo "<P align='center'><span class='chartFootnote'>".$current_module_strings['LBL_LEAD_SOURCE_FORM_DESC']."</span></P>";

        if (file_exists($cache_file_name)) {
            global $timedate;
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
    * Creates PIE CHART image of opportunities by lead_source.
    * param $datax- the sales stage data to display in the x-axis
    * param $datay- the sum of opportunity amounts for each opportunity in each sales stage
    * to display in the y-axis
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
    * All Rights Reserved..
    * Contributor(s): ______________________________________..
    */
    public function gen_xml(
        $legends = array('foo', 'bar'),
        $user_id = array('1'),
        $cache_file_name = 'a_file',
        $refresh = true,
        $current_module_strings = null
    ) {
        global $app_strings, $charset, $lang, $pieChartColors, $current_user, $current_language;

        // set $current_module_strings to 'Charts' module strings by default
        if (empty($current_module_strings)) {
            $current_module_strings = return_module_language($current_language, 'Charts');
        }

        $kDelim = $current_user->getPreference('num_grp_sep');

        if (!file_exists($cache_file_name) || $refresh == true) {
            ;
            $GLOBALS['log']->debug("starting pipeline chart");
            $GLOBALS['log']->debug("legends is:");
            $GLOBALS['log']->debug($legends);
            $GLOBALS['log']->debug("user_id is: ");
            $GLOBALS['log']->debug($user_id);
            $GLOBALS['log']->debug("cache_file_name is: $cache_file_name");

            $opp = new Opportunity;
            //Now do the db queries
            //query for opportunity data that matches $legends and $user
            $where="";
            //build the where clause for the query that matches $user

            $count = is_countable($user_id) ? count($user_id) : 0;
            $id = array();
            if ($count > 0 && !empty($user_id)) {
                foreach ($user_id as $the_id) {
                    $id[] = "'".$the_id."'";
                }
                $ids = implode(",", $id);
                $where .= "opportunities.assigned_user_id IN ($ids) ";
            }
            if (!empty($where)) {
                $where .= 'AND';
            }
            //build the where clause for the query that matches $datax
            $count = is_countable($legends) ? count($legends) : 0;
            $legendItem = array();
            if ($count > 0 && !empty($legends)) {
                foreach ($legends as $key=>$value) {
                    $legendItem[] = "'".$key."'";
                }
                $legendItems = implode(",", $legendItem);
                $where .= " opportunities.lead_source IN	($legendItems) ";
            }
            $query = "SELECT lead_source,sum(amount_usdollar/1000) as total,count(*) as opp_count FROM opportunities ";
            $query .= "WHERE ".$where." AND opportunities.deleted=0 ";
            $query .= "GROUP BY lead_source ORDER BY total DESC";

            //build pipeline by lead source data
            $total = 0;
            $div = 1;
            global $sugar_config;
            $symbol = $sugar_config['default_currency_symbol'];
            global $current_user;
            if ($current_user->getPreference('currency')) {
                $currency = BeanFactory::newBean('Currencies');
                $currency->retrieve($current_user->getPreference('currency'));
                $div = $currency->conversion_rate;
                $symbol = $currency->symbol;
            }
            $subtitle = $current_module_strings['LBL_OPP_SIZE'].' '.$symbol.'1'.$current_module_strings['LBL_OPP_THOUSANDS'];
            $fileContents = '';
            $fileContents .= '     <pie defaultAltText="'.$current_module_strings['LBL_ROLLOVER_WEDGE_DETAILS'].'" legendStatus="on">'."\n";
            $result = $opp->db->query($query, true);
            $leadSourceArr =  array();
            while ($row = $opp->db->fetchByAssoc($result, false)) {
                if ($row['lead_source'] == '') {
                    $leadSource = $current_module_strings['NTC_NO_LEGENDS'];
                } else {
                    $leadSource = $row['lead_source'];
                }
                if ($row['total']*$div<=100) {
                    $sum = round($row['total']*$div, 2);
                } else {
                    $sum = round($row['total']*$div);
                }

                $leadSourceArr[$leadSource]['opp_count'] = $row['opp_count'];
                $leadSourceArr[$leadSource]['sum'] = $sum;
            }
            $i=0;
            foreach ($legends as $lead_source_key=>$translation) {
                if ($lead_source_key == '') {
                    $lead_source_key = $current_module_strings['NTC_NO_LEGENDS'];
                    $translation = $current_module_strings['NTC_NO_LEGENDS'];
                }
                if (!isset($leadSourceArr[$lead_source_key])) {
                    $leadSourceArr[$lead_source_key] = $lead_source_key;
                    $leadSourceArr[$lead_source_key]['sum'] = 0;
                }
                $color = generate_graphcolor($lead_source_key, $i);
                $fileContents .= '          <wedge title="'.$translation.'" kDelim="'.$kDelim.'" value="'.$leadSourceArr[$lead_source_key]['sum'].'" color="'.$color.'" labelText="'.currency_format_number($leadSourceArr[$lead_source_key]['sum'], array('currency_symbol' => true)).'" url="index.php?module=Opportunities&action=index&lead_source='.urlencode($lead_source_key).'&query=true&searchFormTab=advanced_search" altText="'.format_number($leadSourceArr[$lead_source_key]['opp_count'], 0, 0).' '.$current_module_strings['LBL_OPPS_IN_LEAD_SOURCE'].' '.$translation.'"/>'."\n";
                if (isset($leadSourceArr[$lead_source_key])) {
                    $total += $leadSourceArr[$lead_source_key]['sum'];
                }
                $i++;
            }

            $fileContents .= '     </pie>'."\n";
            $fileContents .= '     <graphInfo>'."\n";
            $fileContents .= '          <![CDATA[]]>'."\n";
            $fileContents .= '     </graphInfo>'."\n";
            $fileContents .= '     <chartColors ';
            foreach ($pieChartColors as $key => $value) {
                $fileContents .= ' '.$key.'='.'"'.$value.'" ';
            }
            $fileContents .= ' />'."\n";
            $fileContents .= '</graphData>'."\n";
            $total = round($total, 2);
            $title = $current_module_strings['LBL_TOTAL_PIPELINE'].currency_format_number($total, array('currency_symbol' => true)).$app_strings['LBL_THOUSANDS_SYMBOL'];
            $fileContents = '<graphData title="'.$title.'" subtitle="'.$subtitle.'">'."\n" . $fileContents;
            $GLOBALS['log']->debug("total is: $total");
            if ($total == 0) {
                return ($current_module_strings['ERR_NO_OPPS']);
            }

            save_xml_file($cache_file_name, $fileContents);
        }

        $return = create_chart('pieF', $cache_file_name);
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
        global $current_user;
        $user_tempx = $current_user->getPreference('pbls_lead_sources');
        if (!empty($user_tempx) && (is_countable($user_tempx) ? count($user_tempx) : 0) > 0 && !isset($_REQUEST['pbls_lead_sources'])) {
            $tempx = $user_tempx;
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_lead_sources'] is:");
            $GLOBALS['log']->debug($user_tempx);
        } elseif (isset($_REQUEST['pbls_lead_sources']) && (is_countable($_REQUEST['pbls_lead_sources']) ? count($_REQUEST['pbls_lead_sources']) : 0) > 0) {
            $tempx = $_REQUEST['pbls_lead_sources'];
            $current_user->setPreference('pbls_lead_sources', $_REQUEST['pbls_lead_sources']);
            $GLOBALS['log']->debug("_REQUEST['pbls_lead_sources'] is:");
            $GLOBALS['log']->debug($_REQUEST['pbls_lead_sources']);
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_lead_sources'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('pbls_lead_sources'));
        }

        //set $datax using selected sales stage keys
        if ((is_countable($tempx) ? count($tempx) : 0) > 0) {
            foreach ($tempx as $key) {
                $datax[$key] = $app_list_strings['lead_source_dom'][$key];
                array_push($selected_datax, $key);
            }
        } else {
            $datax = $app_list_strings['lead_source_dom'];
            $selected_datax = array_keys($app_list_strings['lead_source_dom']);
        }

        $legends = $datax;

        $ids = array();
        $user_ids = $current_user->getPreference('pbls_ids');
        //get list of user ids for which to display data
        if (!empty($user_ids) && (is_countable($user_ids) ? count($user_ids) : 0) != 0 && !isset($_REQUEST['pbls_ids'])) {
            if (isset($_SESSION['pbls_ids'])) {
                $ids = $_SESSION['pbls_ids'];
            }
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_ids'] is:");
            $GLOBALS['log']->debug($user_ids);
        } elseif (isset($_REQUEST['pbls_ids']) && (is_countable($_REQUEST['pbls_ids']) ? count($_REQUEST['pbls_ids']) : 0) > 0) {
            $ids = $_REQUEST['pbls_ids'];
            $current_user->setPreference('pbls_ids', $ids);
            $GLOBALS['log']->debug("_REQUEST['pbls_ids'] is:");
            $GLOBALS['log']->debug($_REQUEST['pbls_ids']);
            $GLOBALS['log']->debug("USER PREFERENCES['pbls_ids'] is:");
            $GLOBALS['log']->debug($current_user->getPreference('pbls_ids'));
        } else {
            $ids = get_user_array(false);
            $ids = array_keys($ids);
        }

        $user_id = $ids;

        $opp = new Opportunity;
        //Now do the db queries
        //query for opportunity data that matches $legends and $user
        $where="";
        //build the where clause for the query that matches $user

        $count = is_countable($user_id) ? count($user_id) : 0;
        $id = array();
        if ($count > 0 && !empty($user_id)) {
            foreach ($user_id as $the_id) {
                $id[] = "'".$the_id."'";
            }
            $ids = implode(",", $id);
            $where .= "opportunities.assigned_user_id IN ($ids) ";
        }
        if (!empty($where)) {
            $where .= 'AND';
        }
        //build the where clause for the query that matches $datax
        $count = is_countable($legends) ? count($legends) : 0;
        $legendItem = array();
        if ($count > 0 && !empty($legends)) {
            foreach ($legends as $key=>$value) {
                $legendItem[] = "'".$key."'";
            }
            $legendItems = implode(",", $legendItem);
            $where .= " opportunities.lead_source IN	($legendItems) ";
        }
        $query = "SELECT lead_source,sum(amount_usdollar/1000) as total,count(*) as opp_count FROM opportunities ";
        $query .= "WHERE ".$where." AND opportunities.deleted=0 ";
        $query .= "GROUP BY lead_source ORDER BY total DESC";

        return $query;
    }

    public function constructGroupBy()
    {
        return array( 'lead_source', );
    }
}
