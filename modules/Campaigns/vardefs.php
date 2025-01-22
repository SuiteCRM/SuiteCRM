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

$dictionary['Campaign'] = array('audited' => true,
    'comment' => 'Campaigns are a series of operations undertaken to accomplish a purpose, usually acquiring leads',
    'table' => 'campaigns',
    'unified_search' => true,
    'full_text_search' => true,
    'fields' => array(
        'tracker_key' => array(
            'name' => 'tracker_key',
            'vname' => 'LBL_TRACKER_KEY',
            'type' => 'int',
            'required' => true,
            'studio' => array('editview' => false),
            'len' => '11',
            'auto_increment' => true,
            'comment' => 'The internal ID of the tracker used in a campaign; no longer used as of 4.2 (see campaign_trkrs)'
        ),
        'tracker_count' => array(
            'name' => 'tracker_count',
            'vname' => 'LBL_TRACKER_COUNT',
            'type' => 'int',
            'len' => '11',
            'default' => '0',
            'comment' => 'The number of accesses made to the tracker URL; no longer used as of 4.2 (see campaign_trkrs)'
        ),
        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_CAMPAIGN_NAME',
            'dbType' => 'varchar',
            'type' => 'name',
            'len' => '255',
            'comment' => 'The name of the campaign',
            'importable' => 'required',
            'required' => true,
            'unified_search' => true,
            'full_text_search' => array('boost' => 3),
        ),
        'refer_url' => array(
            'name' => 'refer_url',
            'vname' => 'LBL_REFER_URL',
            'type' => 'varchar',
            'len' => '255',
            'default' => 'http://',
            'comment' => 'The URL referenced in the tracker URL; no longer used as of 4.2 (see campaign_trkrs)'
        ),
        'description' => array('name' => 'description', 'type' => 'none', 'comment' => 'inhertied but not used', 'source' => 'non-db'),
        'tracker_text' => array(
            'name' => 'tracker_text',
            'vname' => 'LBL_TRACKER_TEXT',
            'type' => 'varchar',
            'len' => '255',
            'comment' => 'The text that appears in the tracker URL; no longer used as of 4.2 (see campaign_trkrs)'
        ),

        'start_date' => array(
            'name' => 'start_date',
            'vname' => 'LBL_CAMPAIGN_START_DATE',
            'type' => 'date',
            'audited' => true,
            'comment' => 'Starting date of the campaign',
            'validation' => array('type' => 'isbefore', 'compareto' => 'end_date'),
            'enable_range_search' => true,
            'options' => 'date_range_search_dom',
        ),
        'end_date' => array(
            'name' => 'end_date',
            'vname' => 'LBL_CAMPAIGN_END_DATE',
            'type' => 'date',
            'audited' => true,
            'comment' => 'Ending date of the campaign',
            'importable' => 'required',
            'required' => true,
            'enable_range_search' => true,
            'options' => 'date_range_search_dom',
        ),
        'status' => array(
            'name' => 'status',
            'vname' => 'LBL_CAMPAIGN_STATUS',
            'type' => 'enum',
            'options' => 'campaign_status_dom',
            'len' => 100,
            'audited' => true,
            'comment' => 'Status of the campaign',
            'importable' => 'required',
            'required' => true,
        ),
        'impressions' => array(
            'name' => 'impressions',
            'vname' => 'LBL_CAMPAIGN_IMPRESSIONS',
            'type' => 'int',
            'default' => 0,
            'reportable' => true,
            'comment' => 'Expected Click throughs manually entered by Campaign Manager'
        ),
        'currency_id' =>
            array(
                'name' => 'currency_id',
                'vname' => 'LBL_CURRENCY',
                'type' => 'id',
                'group' => 'currency_id',
                'function' => array('name' => 'getCurrencyDropDown', 'returns' => 'html','onListView' => true, 'onListView' => true),
                'required' => false,
                'do_report' => false,
                'reportable' => false,
                'comment' => 'Currency in use for the campaign'
            ),
        'budget' => array(
            'name' => 'budget',
            'vname' => 'LBL_CAMPAIGN_BUDGET',
            'type' => 'currency',
            'dbType' => 'double',
            'comment' => 'Budgeted amount for the campaign'
        ),
        'expected_cost' => array(
            'name' => 'expected_cost',
            'vname' => 'LBL_CAMPAIGN_EXPECTED_COST',
            'type' => 'currency',
            'dbType' => 'double',
            'comment' => 'Expected cost of the campaign'
        ),
        'actual_cost' => array(
            'name' => 'actual_cost',
            'vname' => 'LBL_CAMPAIGN_ACTUAL_COST',
            'type' => 'currency',
            'dbType' => 'double',
            'comment' => 'Actual cost of the campaign'
        ),
        'expected_revenue' => array(
            'name' => 'expected_revenue',
            'vname' => 'LBL_CAMPAIGN_EXPECTED_REVENUE',
            'type' => 'currency',
            'dbType' => 'double',
            'comment' => 'Expected revenue stemming from the campaign'
        ),
        'campaign_type' => array(
            'name' => 'campaign_type',
            'vname' => 'LBL_CAMPAIGN_TYPE',
            'type' => 'enum',
            'options' => 'campaign_type_dom',
            'len' => 100,
            'audited' => true,
            'comment' => 'The type of campaign',
            'importable' => 'required',
            'required' => true,
        ),
        'objective' => array(
            'name' => 'objective',
            'vname' => 'LBL_CAMPAIGN_OBJECTIVE',
            'type' => 'text',
            'comment' => 'The objective of the campaign'
        ),
        'content' => array(
            'name' => 'content',
            'vname' => 'LBL_CAMPAIGN_CONTENT',
            'type' => 'text',
            'comment' => 'The campaign description',
            'inline_edit' => false
        ),
        'prospectlists' => array(
            'name' => 'prospectlists',
            'vname' => 'LBL_PROSPECT_LISTS',
            'type' => 'link',
            'relationship' => 'prospect_list_campaigns',
            'source' => 'non-db',
        ),
        'emailmarketing' => array(
            'name' => 'emailmarketing',
            'vname' => 'LBL_EMAIL_MARKETING',
            'type' => 'link',
            'relationship' => 'campaign_email_marketing',
            'source' => 'non-db',
        ),
        'queueitems' => array(
            'name' => 'queueitems',
            'vname' => 'LBL_QUEUE_ITEMS',
            'type' => 'link',
            'relationship' => 'campaign_emailman',
            'source' => 'non-db',
        ),
        'log_entries' => array(
            'name' => 'log_entries',
            'type' => 'link',
            'relationship' => 'campaign_campaignlog',
            'source' => 'non-db',
            'vname' => 'LBL_LOG_ENTRIES',
        ),
        'tracked_urls' => array(
            'name' => 'tracked_urls',
            'type' => 'link',
            'relationship' => 'campaign_campaigntrakers',
            'source' => 'non-db',
            'vname' => 'LBL_TRACKED_URLS',
        ),
        'frequency' => array(
            'name' => 'frequency',
            'vname' => 'LBL_CAMPAIGN_FREQUENCY',
            'type' => 'enum',
            //'options' => 'campaign_status_dom',
            'len' => 100,
            'comment' => 'Frequency of the campaign',
            'options' => 'newsletter_frequency_dom',
            'len' => 100,
        ),
        'leads' => array(
            'name' => 'leads',
            'type' => 'link',
            'relationship' => 'campaign_leads',
            'source' => 'non-db',
            'vname' => 'LBL_LEADS',
            'link_class' => 'ProspectLink',
            'link_file' => 'modules/Campaigns/ProspectLink.php'
        ),

        'opportunities' => array(
            'name' => 'opportunities',
            'type' => 'link',
            'relationship' => 'campaign_opportunities',
            'source' => 'non-db',
            'vname' => 'LBL_OPPORTUNITIES',
        ),
        'contacts' => array(
            'name' => 'contacts',
            'type' => 'link',
            'relationship' => 'campaign_contacts',
            'source' => 'non-db',
            'vname' => 'LBL_CONTACTS',
            'link_class' => 'ProspectLink',
            'link_file' => 'modules/Campaigns/ProspectLink.php'
        ),
        'accounts' => array(
            'name' => 'accounts',
            'type' => 'link',
            'relationship' => 'campaign_accounts',
            'source' => 'non-db',
            'vname' => 'LBL_ACCOUNTS',
            'link_class' => 'ProspectLink',
            'link_file' => 'modules/Campaigns/ProspectLink.php'
        ),
        'notes' =>
            array(
                'name' => 'notes',
                'type' => 'link',
                'relationship' => 'campaign_notes',
                'source' => 'non-db',
                'vname' => 'LBL_NOTES',
            ),

        "survey" => array(
                'name' => 'survey',
                'type' => 'link',
                'relationship' => 'surveys_campaigns',
                'source' => 'non-db',
                'module' => 'Surveys',
                'bean_name' => 'Surveys',
                'vname' => 'LBL_CAMPAIGN_SURVEYS',
                'id_name' => 'survey_id',
                'link_type'=>'one',
                'side' => 'left',
        ),
        "survey_name" => array(
                'name' => 'survey_name',
                'type' => 'relate',
                'source' => 'non-db',
                'vname' => 'LBL_CAMPAIGN_SURVEYS',
                'save' => true,
                'id_name' => 'survey_id',
                'link' => 'survey',
                'table' => 'surveys',
                'module' => 'Surveys',
                'rname' => 'name',
        ),
        "survey_id" => array(
                'name' => 'survey_id',
                'type' => 'id',
                'reportable' => false,
                'vname' => 'LBL_CAMPAIGN_SURVEYS',
        ),
        "surveyresponses_campaigns" => array(
                'name' => 'surveyresponses_campaigns',
                'type' => 'link',
                'relationship' => 'surveyresponses_campaigns',
                'source' => 'non-db',
                'module' => 'SurveyResponses',
                'bean_name' => 'SurveyResponses',
                'side' => 'right',
                'vname' => 'LBL_SURVEYRESPONSES_CAMPAIGNS_FROM_SURVEYRESPONSES_TITLE',
        ),
    ),
    'indices' => array(
        array(
            'name' => 'camp_auto_tracker_key',
            'type' => 'index',
            'fields' => array('tracker_key')
        ),
        array(
            'name' => 'idx_campaign_name',
            'type' => 'index',
            'fields' => array('name')
        ),
        array(
            'name' => 'idx_survey_id',
            'type' => 'index',
            'fields' => array('survey_id')
        ),
    ),

    'relationships' => array(
        'campaign_accounts' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'Accounts', 'rhs_table' => 'accounts', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_contacts' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'Contacts', 'rhs_table' => 'contacts', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_leads' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'Leads', 'rhs_table' => 'leads', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_prospects' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'Prospects', 'rhs_table' => 'prospects', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_opportunities' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'Opportunities', 'rhs_table' => 'opportunities', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_notes' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'Notes', 'rhs_table' => 'notes', 'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many', 'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Campaigns'),

        'campaign_email_marketing' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'EmailMarketing', 'rhs_table' => 'email_marketing', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_emailman' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'EmailMan', 'rhs_table' => 'emailman', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_campaignlog' => array('lhs_module' => 'Campaigns', 'lhs_table' => 'campaigns', 'lhs_key' => 'id',
            'rhs_module' => 'CampaignLog', 'rhs_table' => 'campaign_log', 'rhs_key' => 'campaign_id',
            'relationship_type' => 'one-to-many'),

        'campaign_assigned_user' => array('lhs_module' => 'Users', 'lhs_table' => 'users', 'lhs_key' => 'id',
            'rhs_module' => 'Campaigns', 'rhs_table' => 'campaigns', 'rhs_key' => 'assigned_user_id',
            'relationship_type' => 'one-to-many'),

        'campaign_modified_user' => array('lhs_module' => 'Users', 'lhs_table' => 'users', 'lhs_key' => 'id',
            'rhs_module' => 'Campaigns', 'rhs_table' => 'campaigns', 'rhs_key' => 'modified_user_id',
            'relationship_type' => 'one-to-many'),
        'surveyresponses_campaigns' => array(
                'rhs_module' => 'SurveyResponses',
                'rhs_table' => 'surveyresponses',
                'rhs_key' => 'campaign_id',
                'lhs_module' => 'Campaigns',
                'lhs_table' => 'campaigns',
                'lhs_key' => 'id',
                'relationship_type' => 'one-to-many',
        ),
    )
);
VardefManager::createVardef('Campaigns', 'Campaign', array('default', 'assignable', 'security_groups',
));
