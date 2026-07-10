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

$dictionary['stic_AWF_Forms'] = array(
    'table' => 'stic_awf_forms',
    'audited' => true,
    'inline_edit' => true,
    'duplicate_merge' => true,
    'fields' => array (
  'status' => 
  array (
    'required' => true,
    'name' => 'status',
    'vname' => 'LBL_STATUS',
    'type' => 'enum',
    'massupdate' => true,
    'default' => 'draft',
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => true,
    'inline_edit' => true,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => 100,
    'size' => '20',
    'options' => 'stic_awf_forms_status_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'start_date' => 
  array(
    'required' => false,
    'name' => 'start_date',
    'vname' => 'LBL_START_DATE',
    'type' => 'datetimecombo',
    'massupdate' => true,
    'no_default' => false,
    'display_default' => '',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'enabled',
    'duplicate_merge_dom_value' => '2',
    'inline_edit' => true,
    'audited' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'enabled',
    'size' => '20',
    'enable_range_search' => true,
    'options' => 'date_range_search_dom',
    'dbType' => 'datetime',
  ),
  'end_date' => 
  array(
    'required' => false,
    'name' => 'end_date',
    'vname' => 'LBL_END_DATE',
    'type' => 'datetimecombo',
    'massupdate' => true,
    'no_default' => false,
    'display_default' => '',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'enabled',
    'duplicate_merge_dom_value' => '2',
    'inline_edit' => true,
    'audited' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'enabled',
    'size' => '20',
    'enable_range_search' => true,
    'options' => 'date_range_search_dom',
    'dbType' => 'datetime',
  ),  
  'public_url' => 
  array (
    'required' => false,
    'name' => 'public_url',
    'vname' => 'LBL_PUBLIC_URL',
    'type' => 'varchar',
    'massupdate' => 0,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'false',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'inline_edit' => '',
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => '255',
    'size' => '20',
  ),
  'processing_mode' => 
  array (
    'required' => true,
    'name' => 'processing_mode',
    'vname' => 'LBL_PROCESSING_MODE',
    'type' => 'enum',
    'massupdate' => false,
    'default' => 'sync',
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
    'options' => 'stic_awf_forms_processing_mode_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'form_type' => 
  array (
    'required' => true,
    'name' => 'form_type',
    'vname' => 'LBL_FORM_TYPE',
    'type' => 'enum',
    'massupdate' => false,
    'default' => 'web',
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
    'options' => 'stic_awf_forms_types_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'configuration' => 
  array (
    'required' => false,
    'name' => 'configuration',
    'vname' => 'LBL_CONFIGURATION',
    'type' => 'longtext',
    'dbType' => 'longtext',
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
    'rows' => '4',
    'cols' => '20',
  ),
'analytics_views' => array(
    'name' => 'analytics_views',
    'vname' => 'LBL_ANALYTICS_VIEWS',
    'type' => 'int',
    'default' => 0,
    'studio' => 'visible',
    'duplicate_merge' => 'disabled',
    'inline_edit' => false,
    'reportable' => true,
    'massupdate' => false,
  ),
  'analytics_blocked' => array(
    'name' => 'analytics_blocked',
    'vname' => 'LBL_ANALYTICS_BLOCKED',
    'type' => 'int',
    'default' => 0,
    'studio' => 'visible',
    'duplicate_merge' => 'disabled',
    'inline_edit' => false,
    'reportable' => true,
    'massupdate' => false,
  ),
  'analytics_submissions' => array(
    'name' => 'analytics_submissions',
    'vname' => 'LBL_ANALYTICS_SUBMISSIONS',
    'type' => 'int',
    'default' => 0,
    'studio' => 'visible',
    'duplicate_merge' => 'disabled',
    'inline_edit' => false,
    'reportable' => true,
    'massupdate' => false,
  ),
  'analytics_spam' => array(
    'name' => 'analytics_spam',
    'vname' => 'LBL_ANALYTICS_SPAM',
    'type' => 'int',
    'default' => 0,
    'studio' => 'visible',
    'duplicate_merge' => 'disabled',
    'inline_edit' => false,
    'reportable' => true,
    'massupdate' => false,
  ),  
  'analytics_referrers' => array(
    'name' => 'analytics_referrers',
    'vname' => 'LBL_ANALYTICS_REFERRERS',
    'type' => 'text',
    'studio' => 'visible',
    'reportable' => true,
    'duplicate_merge' => 'disabled',
    'inline_edit' => false,
    'massupdate' => false,
  ),
),
    'relationships' => array (
),
    'optimistic_locking' => true,
    'unified_search' => true,
);

$dictionary["stic_AWF_Forms"]["fields"]["stic_69c1s_responses"] = array (
  'name' => 'stic_69c1s_responses',
  'type' => 'link',
  'relationship' => 'stic_awf_forms_stic_awf_responses',
  'source' => 'non-db',
  'module' => 'stic_AWF_Responses',
  'bean_name' => false,
  'side' => 'right',
  'vname' => 'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_RESPONSES_TITLE',
);

if (!class_exists('VardefManager')) {
        require_once('include/SugarObjects/VardefManager.php');
}

VardefManager::createVardef('stic_AWF_Forms', 'stic_AWF_Forms', array('basic','assignable','security_groups'));

// Set special values for SuiteCRM base fields
$dictionary['stic_AWF_Forms']['fields']['description']['rows'] = '2'; // Make textarea fields shorter
$dictionary['stic_AWF_Forms']['fields']['assigned_user_id']['required'] = 1;
$dictionary['stic_AWF_Forms']['fields']['assigned_user_id']['massupdate'] = 1;