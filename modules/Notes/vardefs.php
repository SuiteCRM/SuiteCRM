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

$dictionary['Note'] = array(

    'table' => 'notes',
    'unified_search' => true, 'full_text_search' => true, 'unified_search_default_enabled' => true,

    'comment' => 'Notes and Attachments',
    'fields' => [
          'id' =>
              [
                'name' => 'id',
                'vname' => 'LBL_ID',
                'type' => 'id',
                'required'=>true,
                'reportable'=>true,
                'comment' => 'Unique identifier'
              ],

          'date_entered' =>
              [
                'name' => 'date_entered',
                'vname' => 'LBL_DATE_ENTERED',
                'type' => 'datetime',
                'comment' => 'Date record created',
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
              ],

          'date_modified' =>
              [
                'name' => 'date_modified',
                'vname' => 'LBL_DATE_MODIFIED',
                'type' => 'datetime',
                'comment' => 'Date record last modified',
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
              ],

          'modified_user_id' =>
              [
                'name' => 'modified_user_id',
                'rname' => 'user_name',
                'id_name' => 'modified_user_id',
                'vname' => 'LBL_MODIFIED',
                'type' => 'assigned_user_name',
                'table' => 'users',
                'isnull' => 'false',
                 'group'=>'modified_by_name',
                'dbType' => 'id',
                'reportable'=>true,
                'comment' => 'User who last modified record',
              ],

           'modified_by_name' =>
              [
                'name' => 'modified_by_name',
                'vname' => 'LBL_MODIFIED_BY',
                'type' => 'relate',
                'reportable'=>false,
                'source'=>'non-db',
                'rname'=>'user_name',
                'table' => 'users',
                'id_name' => 'modified_user_id',
                'module'=>'Users',
                'link'=>'modified_user_link',
                'duplicate_merge'=>'disabled'
              ],

           'created_by' =>
              [
                'name' => 'created_by',
                'rname' => 'user_name',
                'id_name' => 'modified_user_id',
                'vname' => 'LBL_CREATED_BY',
                'type' => 'assigned_user_name',
                'table' => 'users',
                'isnull' => 'false',
                'dbType' => 'id',
                'comment' => 'User who created record'
              ],

           'created_by_name' =>
              [
                'name' => 'created_by_name',
                'vname' => 'LBL_CREATED_BY',
                'type' => 'relate',
                'reportable'=>false,
                'link' => 'created_by_link',
                'rname' => 'user_name',
                'source'=>'non-db',
                'table' => 'users',
                'id_name' => 'created_by',
                'module'=>'Users',
                'duplicate_merge'=>'disabled',
                'importable' => 'false',
              ],

          'name' =>
              [
                'name' => 'name',
                'vname' => 'LBL_NOTE_SUBJECT',
                'dbType' => 'varchar',
                'type' => 'name',
                'len' => '255',
                'unified_search' => true,
                'full_text_search' => ['boost' => 3],
                'comment' => 'Name of the note',
                'importable' => 'required',
                'required' => true,
              ],

          'file_mime_type' =>
              [
                'name' => 'file_mime_type',
                'vname' => 'LBL_FILE_MIME_TYPE',
                'type' => 'varchar',
                'len' => '100',
                'comment' => 'Attachment MIME type',
                'importable' => false,
              ],

          'file_url'=>
              [
                'name'=>'file_url',
                'vname' => 'LBL_FILE_URL',
                'type'=>'function',
                'function_class'=>'UploadFile',
                'function_name'=>'get_upload_url',
                'function_params'=> ['$this'],
                'source'=>'function',
                'reportable'=>false,
                'comment' => 'Path to file (can be URL)',
                'importable' => false,
              ],

          'filename' =>
            [
                'name' => 'filename',
                'vname' => 'LBL_FILENAME',
                'type' => 'file',
                'dbType' => 'varchar',
                'len' => '255',
                'reportable'=>true,
                'comment' => 'File name associated with the note (attachment)',
                'importable' => false,
            ],

          'parent_type'=>
            [
                'name'=>'parent_type',
                'vname'=>'LBL_PARENT_TYPE',
                'type' =>'parent_type',
                'dbType' => 'varchar',
                'group'=>'parent_name',
                'options'=> 'parent_type_display',
                'len'=> '255',
                'comment' => 'Sugar module the Note is associated with'
            ],

          'parent_id'=>
            [
                'name'=>'parent_id',
                'vname'=>'LBL_PARENT_ID',
                'type'=>'id',
                'required'=>false,
                'reportable'=>true,
                'comment' => 'The ID of the Sugar item specified in parent_type'
            ],

        'contact_id'=>
            [
                'name'=>'contact_id',
                'vname'=>'LBL_CONTACT_ID',
                'type'=>'id',
                'required'=>false,
                'reportable'=>false,
                'comment' => 'Contact ID note is associated with'
            ],

        'portal_flag' =>
            [
                'name' => 'portal_flag',
                'vname' => 'LBL_PORTAL_FLAG',
                'type' => 'bool',
                'required' => true,
                'comment' => 'Portal flag indicator determines if note created via portal'
            ],

        'embed_flag' =>
            [
                'name' => 'embed_flag',
                'vname' => 'LBL_EMBED_FLAG',
                'type' => 'bool',
                'default' => 0,
                'comment' => 'Embed flag indicator determines if note embedded in email'
            ],

        'description' =>
            [
                'name' => 'description',
                'vname' => 'LBL_DESCRIPTION',
                'type' => 'text',
                'comment' => 'Full text of the note',
                'rows' => 30,
                'cols' => 90,
            ],

        'deleted' =>
           [
                'name' => 'deleted',
                'vname' => 'LBL_DELETED',
                'type' => 'bool',
                'required' => false,
                'default' => '0',
                'reportable'=>false,
                'comment' => 'Record deletion indicator'
            ],

        'filecontents' =>
            [
                'name' => 'filecontents',
                'vname' => 'LBL_FILE_CONTENTS',
                'type' => 'varchar',
                'source' => 'non-db',
            ],

        'parent_name'=>
            [
                'name'=> 'parent_name',
                'parent_type'=>'record_type_display' ,
                'type_name'=>'parent_type',
                'id_name'=>'parent_id',
                'vname'=>'LBL_RELATED_TO',
                'type'=>'parent',
                'source'=>'non-db',
                'options'=> 'record_type_display_notes',
            ],
        'show_preview' =>
            [
                'name' => 'show_preview',
                'type' => 'bool',
                'source' => 'non-db',
                'reportable' => false,
                'mass_update' => false
            ],

         'contact_name'=>
            [
                'name'=>'contact_name',
                'rname'=>'name',
                'id_name'=>'contact_id',
                'vname'=>'LBL_CONTACT_NAME',
                'table'=>'contacts',
                'type'=>'relate',
                'link'=>'contact',
                'join_name'=>'contacts',
                'db_concat_fields'=> [0=>'first_name', 1=>'last_name'],
                'isnull'=>'true',
                'module'=>'Contacts',
                'source'=>'non-db',
            ],

         'contact_phone'=>
            [
                'name'=>'contact_phone',
                'vname' => 'LBL_PHONE',
                'type'=>'phone',
                'source'=>'non-db'
            ],

         'contact_email'=>
            [
                'name'=>'contact_email',
                'type'=>'varchar',
                'vname' => 'LBL_EMAIL_ADDRESS',
                'source' => 'non-db',
                'studio' => false
            ],

         'account_id' =>
            [
                'name' => 'account_id',
                'vname' => 'LBL_ACCOUNT_ID',
                'type' => 'id',
                'reportable'=>false,
                'source'=>'non-db',
            ],

         'opportunity_id' =>
            [
                'name' => 'opportunity_id',
                'vname' => 'LBL_OPPORTUNITY_ID',
                'type' => 'id',
                'reportable'=>false,
                'source'=>'non-db',
            ],

        'campaign_id' =>
            [
                'name' => 'campaign_id',
                'vname' => 'LBL_CAMPAIGN_ID',
                'type' => 'id',
                'reportable' => false,
                'source' => 'non-db',
            ],

          'acase_id' =>
              [
                'name' => 'acase_id',
                'vname' => 'LBL_CASE_ID',
                'type' => 'id',
                'reportable'=>false,
                'source'=>'non-db',
              ],

          'lead_id' =>
              [
                'name' => 'lead_id',
                'vname' => 'LBL_LEAD_ID',
                'type' => 'id',
                'reportable'=>false,
                'source'=>'non-db',
              ],

          'created_by_link' =>
              [
                    'name' => 'created_by_link',
                'type' => 'link',
                'relationship' => 'notes_created_by',
                'vname' => 'LBL_CREATED_BY_USER',
                'link_type' => 'one',
                'module'=>'Users',
                'bean_name'=>'User',
                'source'=>'non-db',
              ],

          'modified_user_link' =>
              [
                'name' => 'modified_user_link',
                'type' => 'link',
                'relationship' => 'notes_modified_user',
                'vname' => 'LBL_MODIFIED_BY_USER',
                'link_type' => 'one',
                'module'=>'Users',
                'bean_name'=>'User',
                'source'=>'non-db',
              ],

          'contact' =>
              [
                'name' => 'contact',
                'type' => 'link',
                'relationship' => 'contact_notes',
                'vname' => 'LBL_LIST_CONTACT_NAME',
                'source'=>'non-db',
              ],

          'cases' =>
              [
                'name' => 'cases',
                'type' => 'link',
                'relationship' => 'case_notes',
                'vname' => 'LBL_CASES',
                'source'=>'non-db',
              ],

          'accounts' =>
              [
                'name' => 'accounts',
                'type' => 'link',
                'relationship' => 'account_notes',
                'source'=>'non-db',
                'vname'=>'LBL_ACCOUNTS',
              ],

          'opportunities' =>
              [
                'name' => 'opportunities',
                'type' => 'link',
                'relationship' => 'opportunity_notes',
                'source'=>'non-db',
                'vname'=>'LBL_OPPORTUNITIES',
              ],

          'leads' =>
              [
                'name' => 'leads',
                'type' => 'link',
                'relationship' => 'lead_notes',
                'source'=>'non-db',
                'vname'=>'LBL_LEADS',
              ],

          'bugs' =>
              [
                'name' => 'bugs',
                'type' => 'link',
                'relationship' => 'bug_notes',
                'source'=>'non-db',
                'vname'=>'LBL_BUGS',
              ],

        'campaigns' =>
            [
                'name' => 'campaigns',
                'type' => 'link',
                'relationship' => 'campaign_notes',
                'source' => 'non-db',
                'vname' => 'LBL_CAMPAIGNS',
            ],

          'aos_contracts' =>
              [
                'name' => 'aos_contracts',
                'type' => 'link',
                'relationship' => 'aos_contracts_notes',
                'source'=>'non-db',
                'vname'=>'LBL_CONTRACT',
              ],

          'emails' =>
              [
                'name'=> 'emails',
                'vname'=> 'LBL_EMAILS',
                'type'=> 'link',
                'relationship'=> 'emails_notes_rel',
                'source'=> 'non-db',
              ],

          'projects' =>
              [
                'name' => 'projects',
                'type' => 'link',
                'relationship' => 'projects_notes',
                'source'=>'non-db',
                'vname'=>'LBL_PROJECTS',
              ],

          'project_tasks' =>
              [
                'name' => 'project_tasks',
                'type' => 'link',
                'relationship' => 'project_tasks_notes',
                'source'=>'non-db',
                'vname'=>'LBL_PROJECT_TASKS',
              ],

          'meetings' =>
              [
                'name' => 'meetings',
                'type' => 'link',
                'relationship' => 'meetings_notes',
                'source'=>'non-db',
                'vname'=>'LBL_MEETINGS',
              ],

          'calls' =>
              [
                'name' => 'calls',
                'type' => 'link',
                'relationship' => 'calls_notes',
                'source'=>'non-db',
                'vname'=>'LBL_CALLS',
              ],

          'tasks' =>
              [
                'name' => 'tasks',
                'type' => 'link',
                'relationship' => 'tasks_notes',
                'source'=>'non-db',
                'vname'=>'LBL_TASKS',
              ],
    ],

'relationships'=>array(
'notes_modified_user' =>
   array('lhs_module'=> 'Users', 'lhs_table'=> 'users', 'lhs_key' => 'id',
   'rhs_module'=> 'Notes', 'rhs_table'=> 'notes', 'rhs_key' => 'modified_user_id',
   'relationship_type'=>'one-to-many')

   ,'notes_created_by' =>
   array('lhs_module'=> 'Users', 'lhs_table'=> 'users', 'lhs_key' => 'id',
   'rhs_module'=> 'Notes', 'rhs_table'=> 'notes', 'rhs_key' => 'created_by',
   'relationship_type'=>'one-to-many')


)
                                                      , 'indices' => array(
       array('name' =>'notespk', 'type' =>'primary', 'fields'=>array('id')),
       array('name' =>'idx_note_name', 'type'=>'index', 'fields'=>array('name')),
       array('name' =>'idx_notes_parent', 'type'=>'index', 'fields'=>array('parent_id', 'parent_type')),
       array('name' =>'idx_note_contact', 'type'=>'index', 'fields'=>array('contact_id')),
       array('name' =>'idx_notes_assigned_del', 'type' =>'index', 'fields'=>array( 'deleted', 'assigned_user_id')),
                                                      )


                                                      //This enables optimistic locking for Saves From EditView
    ,'optimistic_locking'=>true,
                            );

VardefManager::createVardef('Notes', 'Note', array('assignable', 'security_groups',
));
