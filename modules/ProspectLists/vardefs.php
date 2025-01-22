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

$dictionary['ProspectList'] = array(
    'table' => 'prospect_lists',
    'unified_search' => true,
    'full_text_search' => true,
    'fields' => array(
        'id' => array(
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
            'reportable' => false,
        ),
        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_NAME',
            'type' => 'varchar',
            'len' => '255',
            'importable' => 'required',
            'unified_search' => true,
            'full_text_search' => array('boost' => 3),
        ),
        'list_type' => array(
            'name' => 'list_type',
            'vname' => 'LBL_TYPE',
            'type' => 'enum',
            'options' => 'prospect_list_type_dom',
            'len' => 100,
            'importable' => 'required',
        ),
        'date_entered' => array(
            'name' => 'date_entered',
            'vname' => 'LBL_DATE_ENTERED',
            'type' => 'datetime',
        ),
        'date_modified' => array(
            'name' => 'date_modified',
            'vname' => 'LBL_DATE_MODIFIED',
            'type' => 'datetime',
        ),
        'modified_user_id' => array(
            'name' => 'modified_user_id',
            'rname' => 'user_name',
            'id_name' => 'modified_user_id',
            'vname' => 'LBL_MODIFIED',
            'type' => 'assigned_user_name',
            'table' => 'modified_user_id_users',
            'isnull' => 'false',
            'dbType' => 'id',
            'reportable' => true,
        ),
        'modified_by_name' => array(
            'name' => 'modified_by_name',
            'vname' => 'LBL_MODIFIED',
            'type' => 'relate',
            'reportable' => false,
            'source' => 'non-db',
            'table' => 'users',
            'id_name' => 'modified_user_id',
            'module' => 'Users',
            'duplicate_merge' => 'disabled',
        ),
        'created_by' => array(
            'name' => 'created_by',
            'rname' => 'user_name',
            'id_name' => 'created_by',
            'vname' => 'LBL_CREATED',
            'type' => 'assigned_user_name',
            'table' => 'created_by_users',
            'isnull' => 'false',
            'dbType' => 'id'
        ),
        'created_by_name' => array(
            'name' => 'created_by_name',
            'vname' => 'LBL_CREATED',
            'type' => 'relate',
            'reportable' => false,
            'source' => 'non-db',
            'table' => 'users',
            'id_name' => 'created_by',
            'module' => 'Users',
            'duplicate_merge' => 'disabled',
        ),
        'deleted' => array(
            'name' => 'deleted',
            'vname' => 'LBL_CREATED_BY',
            'type' => 'bool',
            'required' => false,
            'reportable' => false,
        ),
        'description' => array(
            'name' => 'description',
            'vname' => 'LBL_DESCRIPTION',
            'type' => 'text',
        ),
        'domain_name' => array(
            'name' => 'domain_name',
            'vname' => 'LBL_DOMAIN_NAME',
            'type' => 'varchar',
            'len' => '255',
        ),
        'entry_count' =>
            array(
                'name' => 'entry_count',
                'type' => 'int',
                'source' => 'non-db',
                'vname' => 'LBL_LIST_ENTRIES',
            ),
        'prospects' =>
            array(
                'name' => 'prospects',
                'vname' => 'LBL_PROSPECTS',
                'type' => 'link',
                'relationship' => 'prospect_list_prospects',
                'source' => 'non-db',
            ),
        'contacts' =>
            array(
                'name' => 'contacts',
                'vname' => 'LBL_CONTACTS',
                'type' => 'link',
                'relationship' => 'prospect_list_contacts',
                'source' => 'non-db',
            ),
        'leads' =>
            array(
                'name' => 'leads',
                'vname' => 'LBL_LEADS',
                'type' => 'link',
                'relationship' => 'prospect_list_leads',
                'source' => 'non-db',
            ),
        'accounts' =>
            array(
                'name' => 'accounts',
                'vname' => 'LBL_ACCOUNTS',
                'type' => 'link',
                'relationship' => 'prospect_list_accounts',
                'source' => 'non-db',
            ),
        'campaigns' => array(
            'name' => 'campaigns',
            'vname' => 'LBL_CAMPAIGNS',
            'type' => 'link',
            'relationship' => 'prospect_list_campaigns',
            'source' => 'non-db',
        ),
        'users' =>
            array(
                'name' => 'users',
                'vname' => 'LBL_USERS',
                'type' => 'link',
                'relationship' => 'prospect_list_users',
                'source' => 'non-db',
            ),
        'email_marketing' => array(
            'name' => 'email_marketing',
            'vname' => 'LBL_EMAIL_MARKETING',
            'type' => 'link',
            'relationship' => 'email_marketing_prospect_lists',
            'source' => 'non-db',
        ),
        'marketing_id' => array(
            'name' => 'marketing_id',
            'vname' => 'LBL_MARKETING_ID',
            'type' => 'varchar',
            'len' => '36',
            'source' => 'non-db',
        ),
        'marketing_name' => array(
            'name' => 'marketing_name',
            'vname' => 'LBL_MARKETING_NAME',
            'type' => 'varchar',
            'len' => '255',
            'source' => 'non-db',
        ),
    ),

    'indices' => array(
        array(
            'name' => 'prospectlistsspk',
            'type' => 'primary',
            'fields' => array('id')
        ),
        array(
            'name' => 'idx_prospect_list_name',
            'type' => 'index',
            'fields' => array('name')
        ),
    ),
    'relationships' => array(
        'prospectlists_assigned_user' =>
            array('lhs_module' => 'Users', 'lhs_table' => 'users', 'lhs_key' => 'id',
                'rhs_module' => 'ProspectLists', 'rhs_table' => 'prospect_lists', 'rhs_key' => 'assigned_user_id',
                'relationship_type' => 'one-to-many')
    )
);

VardefManager::createVardef('ProspectLists', 'ProspectList', array(
    'assignable', 'security_groups',
));
