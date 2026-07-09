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

$dictionary['stic_AWF_Responses'] = array(
    'table' => 'stic_awf_responses',
    'audited' => false,
    'inline_edit' => false,
    'duplicate_merge' => false,
    'fields' => array (
  'form_url' =>
  array (
    'required' => false,
    'name' => 'form_url',
    'vname' => 'LBL_FORM_URL',
    'type' => 'text',
    'massupdate' => false,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'size' => '20',
    'studio' => 'visible',
  ),
  'clean_referrer' => array(
    'name' => 'clean_referrer',
    'vname' => 'LBL_CLEAN_REFERRER',
    'type' => 'text',
    'studio' => 'visible',
    'reportable' => true,
    'duplicate_merge' => 'disabled',
    'inline_edit' => false,
    'reportable' => true,
    'massupdate' => false,
  ),
  'user_agent' => 
  array (
    'required' => false,
    'name' => 'user_agent',
    'vname' => 'LBL_USER_AGENT',
    'type' => 'text',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'size' => '20',
    'studio' => 'visible',
  ),
  'remote_ip' => 
  array (
    'required' => false,
    'name' => 'remote_ip',
    'vname' => 'LBL_REMOTE_IP',
    'type' => 'text',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'size' => '20',
    'studio' => 'visible',
  ),
  'raw_payload' => 
  array (
    'required' => false,
    'name' => 'raw_payload',
    'vname' => 'LBL_RAW_PAYLOAD',
    'type' => 'text',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'size' => '20',
    'studio' => 'visible',
  ),
  'response_hash' => 
  array (
    'required' => false,
    'name' => 'response_hash',
    'vname' => 'LBL_RESPONSE_HASH',
    'type' => 'varchar',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => '255',
    'size' => '20',
  ),
  'html_summary' => 
  array(
    'name' => 'html_summary',
    'vname' => 'LBL_HTML_SUMMARY',
    'type' => 'longtext',
    'dbType' => 'longtext',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'studio' => false,
    'group' => 'html_summary_group',
    'inline_edit' => false,
  ),
  'status' => 
  array (
    'required' => true,
    'name' => 'status',
    'vname' => 'LBL_STATUS',
    'type' => 'enum',
    'massupdate' => 0,
    'default' => 'pending',
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => 100,
    'size' => '20',
    'options' => 'stic_awf_responses_status_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'execution_log' => 
  array (
    'required' => false,
    'name' => 'execution_log',
    'vname' => 'LBL_EXECUTION_LOG',
    'type' => 'text',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'studio' => 'visible',
    'rows' => '6',
    'cols' => '80',
  ),
),
  'indices' => array(
    array('name' => 'idx_awf_responses_hash', 'type' => 'index', 'fields' => array('response_hash', 'deleted')),
),
    'relationships' => array (
),
    'optimistic_locking' => true,
    'unified_search' => true,
);

$dictionary["stic_AWF_Responses"]["fields"]["stic_1c31forms_links"] = array (
  'name' => 'stic_1c31forms_links',
  'type' => 'link',
  'relationship' => 'stic_awf_responses_stic_awf_links',
  'source' => 'non-db',
  'module' => 'stic_AWF_Links',
  'bean_name' => false,
  'side' => 'right',
  'vname' => 'LBL_STIC_AWF_RESPONSES_STIC_AWF_LINKS_FROM_STIC_AWF_LINKS_TITLE',
);

$dictionary["stic_AWF_Responses"]["fields"]["stic_69c1s_responses"] = array (
  'name' => 'stic_69c1s_responses',
  'type' => 'link',
  'relationship' => 'stic_awf_forms_stic_awf_responses',
  'source' => 'non-db',
  'module' => 'stic_AWF_Forms',
  'bean_name' => false,
  'vname' => 'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_FORMS_TITLE',
  'id_name' => 'stic_awf_forms_stic_awf_responsesforms_ida',
);
$dictionary["stic_AWF_Responses"]["fields"]["stic_1bb8ponses_name"] = array (
  'name' => 'stic_1bb8ponses_name',
  'type' => 'relate',
  'source' => 'non-db',
  'inline_edit' => false,
  'vname' => 'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_FORMS_TITLE',
  'save' => true,
  'id_name' => 'stic_awf_forms_stic_awf_responsesforms_ida',
  'link' => 'stic_69c1s_responses',
  'table' => 'stic_awf_forms',
  'module' => 'stic_AWF_Forms',
  'rname' => 'name',
);
$dictionary["stic_AWF_Responses"]["fields"]["stic_awf_forms_stic_awf_responsesforms_ida"] = array (
  'name' => 'stic_awf_forms_stic_awf_responsesforms_ida',
  'type' => 'link',
  'relationship' => 'stic_awf_forms_stic_awf_responses',
  'source' => 'non-db',
  'reportable' => false,
  'side' => 'right',
  'vname' => 'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_RESPONSES_TITLE',
);

$dictionary['stic_AWF_Responses']['fields']['details_link'] = array(
    'name' => 'details_link',
    'type' => 'link',
    'relationship' => 'stic_awf_responses_details', 
    'source' => 'non-db',
    'module' => 'stic_AWF_Response_Details',
    'bean_name' => 'stic_AWF_Response_Details',
    'vname' => 'LBL_ANSWERS_SUBPANEL_TITLE',
    'side' => 'right',
);

if (!class_exists('VardefManager')) {
        require_once('include/SugarObjects/VardefManager.php');
}

VardefManager::createVardef('stic_AWF_Responses', 'stic_AWF_Responses', array('basic','assignable','security_groups'));

// Set special values for SuiteCRM base fields
$dictionary['stic_AWF_Responses']['fields']['description']['rows'] = '2'; // Make textarea fields shorter

$dictionary['stic_AWF_Responses']['fields']['name']['massupdate'] = false;
$dictionary['stic_AWF_Responses']['fields']['name']['inline_edit'] = false;

$dictionary['stic_AWF_Responses']['fields']['assigned_user_name']['massupdate'] = false;
$dictionary['stic_AWF_Responses']['fields']['assigned_user_name']['inline_edit'] = false;
