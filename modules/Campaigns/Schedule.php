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

/*********************************************************************************

 * Description:
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/
 






    $test=false;
//account for case when called from marketing wizard
if (isset($_REQUEST['return_action']) && $_REQUEST['return_action'] == 'WizardMarketing') {
    $_POST['return_module'] = $_REQUEST['return_module'];
    $_POST['return_action'] = 'TrackDetailView';
    $_POST['record'] = $_REQUEST['record'];
}


if (isset($_REQUEST['mode']) && $_REQUEST['mode'] == 'test') {
    $test=true;
    $_POST['mode'] = 'test';
}


global $app_strings;
global $app_list_strings;
global $current_language;
global $current_user;
global $urlPrefix;
global $currentModule;

$current_module_strings = return_module_language($current_language, 'EmailMarketing');
if ($test) {
    echo getClassicModuleTitle('Campaigns', array($current_module_strings['LBL_MODULE_SEND_TEST']), false);
} else {
    echo getClassicModuleTitle('Campaigns', array($current_module_strings['LBL_MODULE_SEND_EMAILS']), false);
}

$campaign_id = isset($_REQUEST['record']) ? $_REQUEST['record'] : false;

if (!empty($campaign_id)) {
    $campaign = BeanFactory::newBean('Campaigns');
    $campaign->retrieve($campaign_id);
}

if ($campaign_id && isset($campaign) && $campaign->status == 'Inactive') {
    $ss = new Sugar_Smarty();

    $data = array($campaign->name);
    $ss->assign('campaignInactive', string_format(translate('LBL_CAMPAIGN_INACTIVE_SCHEDULE', 'Campaigns'), $data));

    $ss->display('modules/Campaigns/tpls/campaign-inactive.tpl');
} else {
    $focus = BeanFactory::newBean('EmailMarketing');
    if ($campaign_id) {
        $where_clauses = array();

        if (!empty($campaign_id)) {
            array_push($where_clauses, "campaign_id = '".DBManagerFactory::getInstance()->quote($campaign_id)."'");
        }

        $where = "";
        foreach ($where_clauses as $clause) {
            if ($where != "") {
                $where .= " and ";
            }
            $where .= $clause;
        }

        $GLOBALS['log']->info("Here is the where clause for the list view: $where");
    }

    $ListView = new ListView();
    $ListView->initNewXTemplate('modules/Campaigns/Schedule.html', $current_module_strings);

    if ($test) {
        $ListView->xTemplateAssign("SCHEDULE_MESSAGE_HEADER", $current_module_strings['LBL_SCHEDULE_MESSAGE_TEST']);
    } else {
        $ListView->xTemplateAssign("SCHEDULE_MESSAGE_HEADER", $current_module_strings['LBL_SCHEDULE_MESSAGE_EMAILS']);
    }

    //force multi-select popup
    $ListView->process_for_popups=true;
    $ListView->multi_select_popup=true;
    //end
    $ListView->mergeduplicates = false;
    $ListView->show_export_button = false;
    $ListView->show_select_menu = false;
    $ListView->show_delete_button = false;
    $ListView->setDisplayHeaderAndFooter(false);
    $ListView->xTemplateAssign("RETURN_MODULE", $_POST['return_module']);
    $ListView->xTemplateAssign("RETURN_ACTION", $_POST['return_action']);
    $ListView->xTemplateAssign("RETURN_ID", $_POST['record']);
    $ListView->setHeaderTitle($current_module_strings['LBL_LIST_FORM_TITLE']);
    $ListView->setQuery($where, "", "date_modified desc", "EMAILMARKETING", false);

    if ($test) {
        $ListView->xTemplateAssign("MODE", $_POST['mode']);
        //finds all marketing messages that have an association with prospect list of the test.
        //this query can be siplified using sub-selects.
        $query="select distinct email_marketing.id email_marketing_id from email_marketing ";
        $query.=" inner join email_marketing_prospect_lists empl on empl.email_marketing_id = email_marketing.id ";
        $query.=" inner join prospect_lists on prospect_lists.id = empl.prospect_list_id ";
        $query.=" inner join prospect_list_campaigns plc on plc.prospect_list_id = empl.prospect_list_id ";
        $query.=" where empl.deleted=0  ";
        $query.=" and prospect_lists.deleted=0 ";
        $query.=" and prospect_lists.list_type='test' ";
        $query.=" and plc.deleted=0 ";
        $query.=" and plc.campaign_id='$campaign_id'";
        $query.=" and email_marketing.campaign_id='$campaign_id'";
        $query.=" and email_marketing.deleted=0 ";
        $query.=" and email_marketing.all_prospect_lists=0 ";

        $seed=array();

        $result=$focus->db->query($query);
        while (($row=$focus->db->fetchByAssoc($result)) != null) {
            $bean = BeanFactory::newBean('EmailMarketing');
            $bean->retrieve($row['email_marketing_id']);
            $bean->mode='test';
            $seed[]=$bean;
        }
        $query=" select email_marketing.id email_marketing_id from email_marketing ";
        $query.=" WHERE email_marketing.campaign_id='$campaign_id'";
        $query.=" and email_marketing.deleted=0 ";
        $query.=" and email_marketing.all_prospect_lists=1 ";
        $query.=" ORDER BY date_modified desc";

        $result=$focus->db->query($query);
        while (($row=$focus->db->fetchByAssoc($result)) != null) {
            $bean = BeanFactory::newBean('EmailMarketing');
            $bean->retrieve($row['email_marketing_id']);
            $bean->mode='test';
            $seed[]=$bean;
        }

        $ListView->processListView($seed, "main", "EMAILMARKETING");
    } else {
        $ListView->processListView($focus, "main", "EMAILMARKETING");
    }
}
