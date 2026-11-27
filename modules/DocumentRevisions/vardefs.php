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


$dictionary['DocumentRevision'] = array('table' => 'document_revisions'
                               ,'fields' => array(
  'id' =>
  array(
    'name' => 'id',
    'vname' => 'LBL_REVISION_NAME',
    'type' => 'varchar',
    'len' => '36',
    'required'=>true,
    'reportable'=>false,
  ),

 'change_log' =>
  array(
    'name' => 'change_log',
    'vname' => 'LBL_CHANGE_LOG',
    'type' => 'varchar',
    'len' => '255',
  ),

  'document_id' =>
  array(
    'name' => 'document_id',
    'vname' => 'LBL_DOCUMENT',
    'type' => 'varchar',
    'len' => '36',
    'required'=>false,
    'reportable'=>false,
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
    'studio' => 'false',
  ),
   'date_entered' =>
  array(
    'name' => 'date_entered',
    'vname' => 'LBL_DATE_ENTERED',
    'type' => 'datetime',
  ),
  'created_by' =>
  array(
    'name' => 'created_by',
    'rname' => 'user_name',
    'id_name' => 'modified_user_id',
    'vname' => 'LBL_CREATED',
    'type' => 'assigned_user_name',
    'table' => 'users',
    'isnull' => 'false',
    'dbType' => 'id',
    'source'=>'db',
  ),
  'filename' =>
  array(
    'name' => 'filename',
    'vname' => 'LBL_FILENAME',
    'type' => 'file',
    'dbType' => 'varchar',
    'required'=>true,
    'len' => '255',
    'allowEapm' => true,
    'linkModuleOverride' => 'Documents',
  ),
  'file_ext' =>
  array(
    'name' => 'file_ext',
    'vname' => 'LBL_FILE_EXTENSION',
    'type' => 'varchar',
    'len' => 100,
  ),
  'file_mime_type' =>
  array(
    'name' => 'file_mime_type',
    'vname' => 'LBL_MIME',
    'type' => 'varchar',
    'len' => '100',
  ),

  'revision'=>
  array(
    'name' => 'revision',
    'vname' => 'LBL_REVISION',
    'type' => 'varchar',
    'len' => 100,
    'importable' => 'required',
  ),

  'deleted' =>
  array(
    'name' => 'deleted',
    'vname' => 'LBL_DELETED',
    'type' => 'bool',
    'default' => 0,
    'reportable'=>false,
  ),
  'date_modified' =>
  array(
    'name' => 'date_modified',
    'vname' => 'LBL_DATE_MODIFIED',
    'type' => 'datetime',
  ),
  'documents' =>
  array(
    'name' => 'documents',
    'type' => 'link',
    'relationship' => 'document_revisions',
    'source'=>'non-db',
        'vname'=>'LBL_REVISIONS',
  ),

'created_by_link' =>
  array(
    'name' => 'created_by_link',
    'type' => 'link',
    'relationship' => 'revisions_created_by',
    'vname' => 'LBL_CREATED_BY_USER',
    'link_type' => 'one',
    'module'=>'Users',
    'bean_name'=>'User',
    'source'=>'non-db',
  ),

'created_by_name' =>
  array(
    'name' => 'created_by_name',
    'rname' => 'user_name',
    'db_concat_fields'=> array(0=>'first_name', 1=>'last_name'),
    'id_name' => 'created_by',
    'vname' => 'LBL_CREATED_BY_NAME',
    'type' => 'relate',
    'table' => 'users',
    'isnull' => 'true',
    'module' => 'Users',
    'dbType' => 'varchar',
    'link'=>'created_by_link',
    'len' => '255',
     'source'=>'non-db',
  ),
  'latest_revision_id'=>
  array(
    'name' => 'latest_revision_id',
    'vname' => 'LBL_REVISION',
    'type' => 'varchar',
    'len' => '36',
    'source'=>'non-db',
  ),
  'document_name' =>
  array(
      'name' => 'document_name',
      'vname' => 'LBL_DOC_NAME',
      'type' => 'varchar',
      'len' => '255',
      'source' => 'non-db',
  ),
  'filecontents' =>
  array(
       'name' => 'filecontents',
       'vname' => 'LBL_FILE_CONTENTS',
       'type' => 'varchar',
       'source' => 'non-db',
  ),
  'latest_revision' =>
  array(
      'name' => 'latest_revision',
      'vname' => 'LBL_CURRENT_DOC_VERSION',
      'type' => 'varchar',
      'len' => '255',
      'source' => 'non-db',
  ),
  
),
'relationships'=>array(
   'revisions_created_by' => array('lhs_module'=> 'Users', 'lhs_table'=> 'users', 'lhs_key' => 'id',
            'rhs_module'=> 'DocumentRevisions', 'rhs_table'=> 'document_revisions', 'rhs_key' => 'created_by',
            'relationship_type'=>'one-to-many'),
),

'indices' => array(
       array('name' =>'documentrevisionspk', 'type' =>'primary', 'fields'=>array('id')),
       array('name' =>'documentrevision_mimetype', 'type' =>'index', 'fields'=>array('file_mime_type')),
       )
);
