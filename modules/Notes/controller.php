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

/*
 * Created on Mar 23, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
 require_once('include/MVC/Controller/SugarController.php');


 #[\AllowDynamicProperties]
 class NotesController extends SugarController
 {
     public function action_save()
     {
         require_once('include/upload_file.php');

         // CCL - Bugs 41103 and 43751.  41103 address the issue where the parent_id is set, but
         // the relate_id field overrides the relationship.  43751 fixes the problem where the relate_id and
         // parent_id are the same value (in which case it should just use relate_id) by adding the != check
         if ((!empty($_REQUEST['relate_id']) && !empty($_REQUEST['parent_id'])) && ($_REQUEST['relate_id'] != $_REQUEST['parent_id'])) {
             $_REQUEST['relate_id'] = false;
         }

         // If Note is related to a Contact, we automatically fill the Contact field
         if ($_REQUEST['parent_type'] === 'Contacts'
            && !empty($_REQUEST['parent_id'])
            && !empty($_REQUEST['parent_name'])) {
             $this->bean->contact_id = $_REQUEST['parent_id'];
             $this->bean->contact_name = $_REQUEST['parent_name'];
         }

         $GLOBALS['log']->debug('PERFORMING NOTES SAVE');
         $upload_file = new UploadFile('uploadfile');
         $do_final_move = 0;
         if (isset($_FILES['uploadfile']) && $upload_file->confirm_upload()) {
             if (!empty($this->bean->id) && !empty($_REQUEST['old_filename'])) {
                 $upload_file->unlink_file($this->bean->id, $_REQUEST['old_filename']);
             }

             $this->bean->filename = $upload_file->get_stored_file_name();
             $this->bean->file_mime_type = $upload_file->mime_type;

             $do_final_move = 1;
         } else {
             if (isset($_REQUEST['old_filename'])) {
                 $this->bean->filename = $_REQUEST['old_filename'];
             }
         }

         $check_notify = false;
         if (!empty($_POST['assigned_user_id']) &&
            (empty($this->bean->fetched_row) || $this->bean->fetched_row['assigned_user_id'] != $_POST['assigned_user_id']) &&
            ($_POST['assigned_user_id'] != $GLOBALS['current_user']->id)) {
             $check_notify = true;
         }
         $this->bean->save($check_notify);

         $isValidator = new \SuiteCRM\Utility\SuiteValidator();
         if ($do_final_move) {
             $upload_file->final_move($this->bean->id);
         } else {
             if (! empty($_REQUEST['old_id'] ?? '') && $isValidator->isValidId($_REQUEST['old_id'] ?? '')) {
                 UploadFile::duplicate_file($_REQUEST['old_id'], $this->bean->id, $this->bean->filename);
             }
         }
     }

     public function action_editview()
     {
         $this->view = 'edit';
         $GLOBALS['view'] = $this->view;
         if (!empty($_REQUEST['deleteAttachment'])) {
             ob_clean();
             echo $this->bean->deleteAttachment($_REQUEST['isDuplicate']) ? 'true' : 'false';
             sugar_cleanup(true);
         }
     }
 }
