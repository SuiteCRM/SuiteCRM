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

/**

* Description:  Includes the functions for Customer module specific charts.
********************************************************************************/
//todo: experimental class for chart data handling..not used in the application at this time.



require_once('include/charts/Charts.php');




#[\AllowDynamicProperties]
class charts
{

    /* @function:
     *
     * @param array targets: translated list of all activity types, targeted, bounced etc..
     * @param string campaign_id: chart for this campaign.
     */
    public function campaign_response_chart($targets, $campaign_id)
    {
        $focus = BeanFactory::newBean('Campaigns');
        $leadSourceArr = array();

        $query = "SELECT activity_type,target_type, count(*) hits ";
        $query.= " FROM campaign_log ";
        $query.= " WHERE campaign_id = '$campaign_id' AND archived=0 AND deleted=0";
        $query.= " GROUP BY  activity_type, target_type";
        $query.= " ORDER BY  activity_type, target_type";

        $result = $focus->db->query($query);
        while ($row = $focus->db->fetchByAssoc($result, false)) {
            if (isset($leadSourceArr[$row['activity_type']]['value'])) {
                $leadSourceArr[$row['activity_type']]['value']=0;
            }

            $leadSourceArr[$row['activity_type']]['value']=  $leadSourceArr[$row['activity_type']]['value'] + $row['hits'];

            if (!empty($row['target_type'])) {
                $leadSourceArr[$row['activity_type']]['bars'][$row['target_type']]['value']=$row['hits'];
            }
        }

        foreach ($targets as $key=>$value) {
            if (!isset($leadSourceArr[$key])) {
                $leadSourceArr[$key]['value']=0;
            }
        }

        //use the new template.
        $xtpl=new XTemplate('modules/Campaigns/chart.tpl');
        $xtpl->assign("GRAPHTITLE", 'Campaign Response by Recipient Activity');
        $xtpl->assign("Y_DEFAULT_ALT_TEXT", 'Rollover a bar to view details.');

        //process rows
        foreach ($leadSourceArr as $key=>$values) {
            if (isset($values['bars'])) {
                foreach ($values['bars'] as $bar_id=>$bar_value) {
                    $xtpl->assign("Y_BAR_ID", $bar_id);
                }
            }
        }
    }
}// end charts class
