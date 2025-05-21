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
 * DeleteTestCampaigns.php
 *
 * This is a class to encapsulate deleting test campaigns
 * @author Collin Lee
 */
#[\AllowDynamicProperties]
class DeleteTestCampaigns
{

/**
 * deleteTestRecords
 *
 * This method deletes the test records for a given Campaign instance
 * @param Campaign $focus The Campaign instance
 */
    public function deleteTestRecords($focus)
    {
        if (empty($focus)  || empty($focus->id)) {
            return;
        }

        $res = $focus->db->query("SELECT DISTINCT campaign_log.related_id emailid, prospect_lists.id as listid FROM campaign_log
            JOIN prospect_lists on campaign_log.list_id = prospect_lists.id
            WHERE campaign_log.campaign_id = '{$focus->id}' AND prospect_lists.list_type='test'");
        $test_ids = array();
        $test_list_ids = array();
        while ($row = $focus->db->fetchByAssoc($res)) {
            $test_ids[] = $row['emailid'];
            $test_list_ids[$row['listid']] = true;
        }
        $test_list_ids = array_keys($test_list_ids);
        unset($res);
        if (!empty($test_ids)) {
            $focus->db->query("UPDATE emails SET deleted=1 WHERE id IN ('".implode("','", $test_ids)."')");
        }

        if (!empty($test_list_ids)) {
            $query = "DELETE FROM emailman WHERE campaign_id = '{$focus->id}' AND list_id IN ('".implode("','", $test_list_ids)."')";
            $focus->db->query($query);

            $query = "UPDATE campaign_log SET deleted=1 WHERE campaign_id = '{$focus->id}' AND list_id IN ('".implode("','", $test_list_ids)."')";

            $focus->db->query($query);
        }
    }
}
