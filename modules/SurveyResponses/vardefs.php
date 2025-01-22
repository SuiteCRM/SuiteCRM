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

$dictionary['SurveyResponses'] = array(
    'table'              => 'surveyresponses',
    'audited'            => true,
    'inline_edit'        => true,
    'duplicate_merge'    => true,
    'fields'             => array(
        'question_responses_display'              => array(
            'name'                      => 'question_responses_display',
            'vname'                     => 'LBL_QUESTION_RESPONSES_DISPLAY',
            'type'                      => 'function',
            'source'                    => 'non-db',
            'massupdate'                => 0,
            'importable'                => 'false',
            'duplicate_merge'           => 'disabled',
            'duplicate_merge_dom_value' => 0,
            'audited'                   => false,
            'reportable'                => false,
            'inline_edit'               => false,
            'function'                  => array(
                'name'    => 'question_responses_display',
                'returns' => 'html',
                'include' => 'modules/SurveyResponses/Lines/Lines.php'
            ),
        ),
        'happiness'                               => array(
            'name'  => 'happiness',
            'type'  => 'int',
            'label' => 'LBL_HAPPINESS',
        ),
        'email_response_sent'                     => array(
            'name'  => 'email_response_sent',
            'type'  => 'bool',
            'label' => 'LBL_EMAIL_RESPONSE_SENT',
        ),
        "account"                                 => array(
            'name'         => 'account',
            'type'         => 'link',
            'relationship' => 'surveyresponses_accounts',
            'source'       => 'non-db',
            'module'       => 'Accounts',
            'bean_name'    => 'Account',
            'vname'        => 'LBL_SURVEYRESPONSES_ACCOUNTS_FROM_ACCOUNTS_TITLE',
            'id_name'      => 'account_id',
            'link_type'    => 'one',
            'side'         => 'left',
        ),
        "account_name"                            => array(
            'name'    => 'account_name',
            'type'    => 'relate',
            'source'  => 'non-db',
            'vname'   => 'LBL_SURVEYRESPONSES_ACCOUNTS_FROM_ACCOUNTS_TITLE',
            'save'    => true,
            'id_name' => 'account_id',
            'link'    => 'account',
            'table'   => 'accounts',
            'module'  => 'Accounts',
            'rname'   => 'name',
        ),
        "account_id"                              => array(
            'name'       => 'account_id',
            'type'       => 'id',
            'reportable' => false,
            'vname'      => 'LBL_SURVEYRESPONSES_ACCOUNTS_FROM_SURVEYRESPONSES_TITLE',
        ),
        "campaign"                                => array(
            'name'         => 'campaign',
            'type'         => 'link',
            'relationship' => 'surveyresponses_campaigns',
            'source'       => 'non-db',
            'module'       => 'Campaigns',
            'bean_name'    => 'Campaign',
            'vname'        => 'LBL_SURVEYRESPONSES_CAMPAIGNS_FROM_CAMPAIGNS_TITLE',
            'id_name'      => 'campaign_id',
            'link_type'    => 'one',
            'side'         => 'left',
        ),
        "campaign_name"                           => array(
            'name'    => 'campaign_name',
            'type'    => 'relate',
            'source'  => 'non-db',
            'vname'   => 'LBL_SURVEYRESPONSES_CAMPAIGNS_FROM_CAMPAIGNS_TITLE',
            'save'    => true,
            'id_name' => 'campaign_id',
            'link'    => 'campaign',
            'table'   => 'campaigns',
            'module'  => 'Campaigns',
            'rname'   => 'name',
        ),
        "campaign_id"                             => array(
            'name'       => 'campaign_id',
            'type'       => 'id',
            'reportable' => false,
            'vname'      => 'LBL_SURVEYRESPONSES_CAMPAIGNS_FROM_SURVEYRESPONSES_TITLE',
        ),
        "contact"                                 => array(
            'name'         => 'contact',
            'type'         => 'link',
            'relationship' => 'surveyresponses_contacts',
            'source'       => 'non-db',
            'module'       => 'Contacts',
            'bean_name'    => 'Contact',
            'vname'        => 'LBL_SURVEYRESPONSES_CONTACTS_FROM_CONTACTS_TITLE',
            'id_name'      => 'contact',
            'link_type'    => 'one',
            'side'         => 'left',
        ),
        "contact_name"                            => array(
            'name'             => 'contact_name',
            'type'             => 'relate',
            'source'           => 'non-db',
            'vname'            => 'LBL_SURVEYRESPONSES_CONTACTS_FROM_CONTACTS_TITLE',
            'save'             => true,
            'id_name'          => 'contact_id',
            'link'             => 'contact',
            'table'            => 'contacts',
            'module'           => 'Contacts',
            'rname'            => 'name',
            'db_concat_fields' => array(
                0 => 'first_name',
                1 => 'last_name',
            ),
        ),
        "contact_id"                              => array(
            'name'       => 'contact_id',
            'type'       => 'id',
            'reportable' => false,
            'vname'      => 'LBL_SURVEYRESPONSES_CONTACTS_FROM_SURVEYRESPONSES_TITLE',
        ),
        "surveyresponses_surveyquestionresponses" => array(
            'name'         => 'surveyresponses_surveyquestionresponses',
            'type'         => 'link',
            'relationship' => 'surveyresponses_surveyquestionresponses',
            'source'       => 'non-db',
            'module'       => 'SurveyQuestionResponses',
            'bean_name'    => 'SurveyQuestionResponses',
            'side'         => 'right',
            'vname'        => 'LBL_SURVEYRESPONSES_SURVEYQUESTIONRESPONSES_FROM_SURVEYQUESTIONRESPONSES_TITLE',
        ),
        "survey"                                  => array(
            'name'         => 'survey',
            'type'         => 'link',
            'relationship' => 'surveys_surveyresponses',
            'source'       => 'non-db',
            'module'       => 'Surveys',
            'bean_name'    => 'Surveys',
            'vname'        => 'LBL_SURVEYS_SURVEYRESPONSES_FROM_SURVEYS_TITLE',
            'id_name'      => 'survey_id',
            'link_type'    => 'one',
            'side'         => 'left',
        ),
        "survey_name"                             => array(
            'name'    => 'survey_name',
            'type'    => 'relate',
            'source'  => 'non-db',
            'vname'   => 'LBL_SURVEYS_SURVEYRESPONSES_FROM_SURVEYS_TITLE',
            'save'    => true,
            'id_name' => 'survey_id',
            'link'    => 'survey',
            'table'   => 'surveys',
            'module'  => 'Surveys',
            'rname'   => 'name',
        ),
        "survey_id"                               => array(
            'name'       => 'survey_id',
            'type'       => 'id',
            'reportable' => false,
            'vname'      => 'LBL_SURVEYS_SURVEYRESPONSES_FROM_SURVEYRESPONSES_TITLE',
        ),
    ),
    'relationships'      => array(
        'surveyresponses_surveyquestionresponses' => array(
            'rhs_module'        => 'SurveyQuestionResponses',
            'rhs_table'         => 'surveyquestionresponses',
            'rhs_key'           => 'surveyresponse_id',
            'lhs_module'        => 'SurveyResponses',
            'lhs_table'         => 'surveyresponses',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
        // Fix Issue 8897 - Adding missing relationships to SurveyResponses and Contacts/Accounts
        'surveyresponses_contacts' => array(
            'rhs_module'        => 'SurveyResponses',
            'rhs_table'         => 'surveyresponses',
            'rhs_key'           => 'contact_id',
            'lhs_module'        => 'Contacts',
            'lhs_table'         => 'contacts',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
        'surveyresponses_accounts' => array (
            'rhs_module'        => 'SurveyResponses',
            'rhs_table'         => 'surveyresponses',
            'rhs_key'           => 'account_id',
            'lhs_module'        => 'Accounts',
            'lhs_table'         => 'accounts',
            'lhs_key'           => 'id',
            'relationship_type' => 'one-to-many',
        ),
    ),
    'optimistic_locking' => true,
    'unified_search'     => true,
);
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('SurveyResponses', 'SurveyResponses', array('basic', 'assignable', 'security_groups'));
