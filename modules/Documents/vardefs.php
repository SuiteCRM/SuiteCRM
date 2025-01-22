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

$dictionary['Document'] = array('table' => 'documents',
    'unified_search' => true,
    'full_text_search' => true,
    'unified_search_default_enabled' => true,
    'fields' => array(

        'document_name' =>
            array(
                'name' => 'document_name',
                'vname' => 'LBL_NAME',
                'type' => 'varchar',
                'len' => '255',
                'required' => true,
                'importable' => 'required',
                'unified_search' => true,
                'full_text_search' => array('boost' => 3),
            ),
        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_NAME',
            'source' => 'non-db',
            'type' => 'varchar'
        ),
        'doc_id' =>
            array(
                'name' => 'doc_id',
                'vname' => 'LBL_DOC_ID',
                'type' => 'varchar',
                'len' => '100',
                'comment' => 'Document ID from documents web server provider',
                'importable' => false,
                'studio' => 'false',
            ),
        'doc_type' =>
            array(
                'name' => 'doc_type',
                'vname' => 'LBL_DOC_TYPE',
                'type' => 'enum',
                'function' => 'getDocumentsExternalApiDropDown',
                'len' => '100',
                'comment' => 'Document type (ex: Google, box.net, IBM SmartCloud)',
                'popupHelp' => 'LBL_DOC_TYPE_POPUP',
                'massupdate' => false,
                'options' => 'eapm_list',
                'default' => 'Sugar',
                'studio' => 'false',
            ),
        'doc_url' =>
            array(
                'name' => 'doc_url',
                'vname' => 'LBL_DOC_URL',
                'type' => 'varchar',
                'len' => '255',
                'comment' => 'Document URL from documents web server provider',
                'importable' => false,
                'massupdate' => false,
                'studio' => 'false',
            ),
        'filename' =>
            array(
                'name' => 'filename',
                'vname' => 'LBL_FILENAME',
                'type' => 'file',
                'source' => 'non-db',
                'comment' => 'The filename of the document attachment',
                'required' => true,
                'noChange' => true,
                'allowEapm' => true,
                'fileId' => 'document_revision_id',
                'docType' => 'doc_type',
                'docUrl' => 'doc_url',
                'docId' => 'doc_id',
            ),

        'active_date' =>
            array(
                'name' => 'active_date',
                'vname' => 'LBL_DOC_ACTIVE_DATE',
                'type' => 'date',
                'importable' => 'required',
                'required' => true,
                'display_default' => 'now',
            ),

        'exp_date' =>
            array(
                'name' => 'exp_date',
                'vname' => 'LBL_DOC_EXP_DATE',
                'type' => 'date',
            ),

        'category_id' =>
            array(
                'name' => 'category_id',
                'vname' => 'LBL_SF_CATEGORY',
                'type' => 'enum',
                'len' => 100,
                'options' => 'document_category_dom',
                'reportable' => true,
            ),

        'subcategory_id' =>
            array(
                'name' => 'subcategory_id',
                'vname' => 'LBL_SF_SUBCATEGORY',
                'type' => 'enum',
                'len' => 100,
                'options' => 'document_subcategory_dom',
                'reportable' => true,
            ),

        'status_id' =>
            array(
                'name' => 'status_id',
                'vname' => 'LBL_DOC_STATUS',
                'type' => 'enum',
                'len' => 100,
                'options' => 'document_status_dom',
                'reportable' => false,
            ),

        'status' =>
            array(
                'name' => 'status',
                'vname' => 'LBL_DOC_STATUS',
                'type' => 'varchar',
                'source' => 'non-db',
                'comment' => 'Document status for Meta-Data framework',
            ),

        'document_revision_id' =>
            array(
                'name' => 'document_revision_id',
                'vname' => 'LBL_LATEST_REVISION',
                'type' => 'varchar',
                'len' => '36',
                'reportable' => false,
            ),

        'revisions' =>
            array(
                'name' => 'revisions',
                'type' => 'link',
                'relationship' => 'document_revisions',
                'source' => 'non-db',
                'vname' => 'LBL_REVISIONS',
            ),

        'revision' =>
            array(
                'name' => 'revision',
                'vname' => 'LBL_DOC_VERSION',
                'type' => 'varchar',
                'reportable' => false,
                'required' => true,
                'source' => 'non-db',
                'importable' => 'required',
                'required' => true,
                'default' => '1',
            ),

        'last_rev_created_name' =>
            array(
                'name' => 'last_rev_created_name',
                'vname' => 'LBL_LAST_REV_CREATOR',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'last_rev_mime_type' =>
            array(
                'name' => 'last_rev_mime_type',
                'vname' => 'LBL_LAST_REV_MIME_TYPE',
                'type' => 'varchar',
                'reportable' => false,
                'studio' => 'false',
                'source' => 'non-db'
            ),
        'latest_revision' =>
            array(
                'name' => 'latest_revision',
                'vname' => 'LBL_LATEST_REVISION',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'last_rev_create_date' =>
            array(
                'name' => 'last_rev_create_date',
                'type' => 'date',
                'table' => 'document_revisions',
                'link' => 'revisions',
                'join_name' => 'document_revisions',
                'vname' => 'LBL_LAST_REV_CREATE_DATE',
                'rname' => 'date_entered',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'contracts' => array(
            'name' => 'contracts',
            'type' => 'link',
            'relationship' => 'contracts_documents',
            'source' => 'non-db',
            'vname' => 'LBL_CONTRACTS',
        ),
        //todo remove
        'leads' => array(
            'name' => 'leads',
            'type' => 'link',
            'relationship' => 'leads_documents',
            'source' => 'non-db',
            'vname' => 'LBL_LEADS',
        ),
        // Links around the world
        'accounts' =>
            array(
                'name' => 'accounts',
                'type' => 'link',
                'relationship' => 'documents_accounts',
                'source' => 'non-db',
                'vname' => 'LBL_ACCOUNTS_SUBPANEL_TITLE',
            ),
        'contacts' =>
            array(
                'name' => 'contacts',
                'type' => 'link',
                'relationship' => 'documents_contacts',
                'source' => 'non-db',
                'vname' => 'LBL_CONTACTS_SUBPANEL_TITLE',
            ),
        'opportunities' =>
            array(
                'name' => 'opportunities',
                'type' => 'link',
                'relationship' => 'documents_opportunities',
                'source' => 'non-db',
                'vname' => 'LBL_OPPORTUNITIES_SUBPANEL_TITLE',
            ),
        'cases' =>
            array(
                'name' => 'cases',
                'type' => 'link',
                'relationship' => 'documents_cases',
                'source' => 'non-db',
                'vname' => 'LBL_CASES_SUBPANEL_TITLE',
            ),
        'bugs' =>
            array(
                'name' => 'bugs',
                'type' => 'link',
                'relationship' => 'documents_bugs',
                'source' => 'non-db',
                'vname' => 'LBL_BUGS_SUBPANEL_TITLE',
            ),
        'related_doc_id' =>
            array(
                'name' => 'related_doc_id',
                'vname' => 'LBL_RELATED_DOCUMENT_ID',
                'reportable' => false,
                'dbType' => 'id',
                'type' => 'varchar',
                'len' => '36',
            ),

        'related_doc_name' =>
            array(
                'name' => 'related_doc_name',
                'vname' => 'LBL_DET_RELATED_DOCUMENT',
                'type' => 'relate',
                'table' => 'documents',
                'id_name' => 'related_doc_id',
                'module' => 'Documents',
                'source' => 'non-db',
                'comment' => 'The related document name for Meta-Data framework',
            ),

        'related_doc_rev_id' =>
            array(
                'name' => 'related_doc_rev_id',
                'vname' => 'LBL_RELATED_DOCUMENT_REVISION_ID',
                'reportable' => false,
                'dbType' => 'id',
                'type' => 'varchar',
                'len' => '36',
            ),

        'related_doc_rev_number' =>
            array(
                'name' => 'related_doc_rev_number',
                'vname' => 'LBL_DET_RELATED_DOCUMENT_VERSION',
                'type' => 'varchar',
                'source' => 'non-db',
                'comment' => 'The related document version number for Meta-Data framework',
            ),

        'is_template' =>
            array(
                'name' => 'is_template',
                'vname' => 'LBL_IS_TEMPLATE',
                'type' => 'bool',
                'default' => 0,
                'reportable' => false,
            ),
        'template_type' =>
            array(
                'name' => 'template_type',
                'vname' => 'LBL_TEMPLATE_TYPE',
                'type' => 'enum',
                'len' => 100,
                'options' => 'document_template_type_dom',
                'reportable' => false,
            ),
//BEGIN field used for contract document subpanel.
        'latest_revision_name' =>
            array(
                'name' => 'latest_revision_name',
                'vname' => 'LBL_LASTEST_REVISION_NAME',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),

        'selected_revision_name' =>
            array(
                'name' => 'selected_revision_name',
                'vname' => 'LBL_SELECTED_REVISION_NAME',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'contract_status' =>
            array(
                'name' => 'contract_status',
                'vname' => 'LBL_CONTRACT_STATUS',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'contract_name' =>
            array(
                'name' => 'contract_name',
                'vname' => 'LBL_CONTRACT_NAME',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'linked_id' =>
            array(
                'name' => 'linked_id',
                'vname' => 'LBL_LINKED_ID',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'selected_revision_id' =>
            array(
                'name' => 'selected_revision_id',
                'vname' => 'LBL_SELECTED_REVISION_ID',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'latest_revision_id' =>
            array(
                'name' => 'latest_revision_id',
                'vname' => 'LBL_LATEST_REVISION_ID',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'selected_revision_filename' =>
            array(
                'name' => 'selected_revision_filename',
                'vname' => 'LBL_SELECTED_REVISION_FILENAME',
                'type' => 'varchar',
                'reportable' => false,
                'source' => 'non-db'
            ),
        'filecontents' =>
            array(
                'name' => 'filecontents',
                'vname' => 'LBL_FILE_CONTENTS',
                'type' => 'varchar',
                'source' => 'non-db',
            ),
//END fields used for contract documents subpanel.
        'aos_contracts' => array(
            'name' => 'aos_contracts',
            'vname' => 'LBL_AOS_CONTRACTS',
            'type' => 'link',
            'relationship' => 'aos_contracts_documents',
            'source' => 'non-db',
            'module' => 'AOS_Contracts',
        ),
        'show_preview' => array(
            'name' => 'show_preview',
            'type' => 'bool',
            'source' => 'non-db',
            'reportable' => false
        ),

    ),
    'indices' => array(
        array('name' => 'idx_doc_cat', 'type' => 'index', 'fields' => array('category_id', 'subcategory_id')),
    ),
    'relationships' => array(
        'document_revisions' => array('lhs_module' => 'Documents', 'lhs_table' => 'documents', 'lhs_key' => 'id',
            'rhs_module' => 'DocumentRevisions', 'rhs_table' => 'document_revisions', 'rhs_key' => 'document_id',
            'relationship_type' => 'one-to-many')

    , 'documents_modified_user' =>
            array('lhs_module' => 'Users', 'lhs_table' => 'users', 'lhs_key' => 'id',
                'rhs_module' => 'Documents', 'rhs_table' => 'documents', 'rhs_key' => 'modified_user_id',
                'relationship_type' => 'one-to-many')

    , 'documents_created_by' =>
            array('lhs_module' => 'Users', 'lhs_table' => 'users', 'lhs_key' => 'id',
                'rhs_module' => 'Documents', 'rhs_table' => 'documents', 'rhs_key' => 'created_by',
                'relationship_type' => 'one-to-many')
    ),

);
VardefManager::createVardef('Documents', 'Document', array('default', 'assignable', 'security_groups',
));
