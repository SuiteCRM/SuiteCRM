<?php

use SuiteCRM\Utility\SuiteValidator;

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

require_once('include/upload_file.php');

require_once('include/upload_file.php');

#[\AllowDynamicProperties]
class NoteSoap
{
    public $upload_file;

    public function __construct()
    {
        $this->upload_file = new UploadFile('uploadfile');
    }




    /**
     * @param array $note
     * @param bool $portal
     * @return string
     */
    public function saveFile($note, $portal = false)
    {
        global $sugar_config;

        $focus = BeanFactory::newBean('Notes');



        if (!empty($note['id'])) {

            $isValidator = new SuiteValidator();
            if (!$isValidator->isValidId($note['id'])) {
                return '-1';
            }

            $focus->retrieve($note['id']);
            if (empty($focus->id)) {
                return '-1';
            }
        } else {
            return '-1';
        }

        if (!empty($note['file'])) {
            $decodedFile = base64_decode($note['file']);
            $this->upload_file->set_for_soap($note['filename'], $decodedFile);

            $ext_pos = strrpos((string) $this->upload_file->stored_file_name, ".");
            $this->upload_file->file_ext = substr((string) $this->upload_file->stored_file_name, $ext_pos + 1);
            if (in_array($this->upload_file->file_ext, $sugar_config['upload_badext'])) {
                $this->upload_file->stored_file_name .= ".txt";
                $this->upload_file->file_ext = "txt";
            }

            $focus->filename = $this->upload_file->get_stored_file_name();
            $focus->file_mime_type = $this->upload_file->getMimeSoap($focus->filename);
            $focus->id = $note['id'];
            $return_id = $focus->save();
            $this->upload_file->final_move($focus->id);
        } else {
            return '-1';
        }

        return $return_id;
    }

    public function newSaveFile($note, $portal = false)
    {
        global $sugar_config;

        $focus = BeanFactory::newBean('Notes');


        if (!empty($note['id'])) {
            $focus->retrieve($note['id']);
            if (empty($focus->id)) {
                return '-1';
            }
        } else {
            return '-1';
        }

        if (!empty($note['file'])) {
            $decodedFile = base64_decode($note['file']);
            $this->upload_file->set_for_soap($note['filename'], $decodedFile);

            $ext_pos = strrpos((string) $this->upload_file->stored_file_name, ".");
            $this->upload_file->file_ext = substr((string) $this->upload_file->stored_file_name, $ext_pos + 1);
            if (in_array($this->upload_file->file_ext, $sugar_config['upload_badext'])) {
                $this->upload_file->stored_file_name .= ".txt";
                $this->upload_file->file_ext = "txt";
            }

            $focus->filename = $this->upload_file->get_stored_file_name();
            $focus->file_mime_type = $this->upload_file->getMimeSoap($focus->filename);
            $focus->save();
        }

        $return_id = $focus->id;

        if (!empty($note['file'])) {
            $this->upload_file->final_move($focus->id);
        }

        if (!empty($note['related_module_id']) && !empty($note['related_module_name'])) {
            $focus->process_save_dates=false;
            $module_name = $note['related_module_name'];
            $module_id = $note['related_module_id'];
            if ($module_name != 'Contacts') {
                $focus->parent_type=$module_name;
                $focus->parent_id = $module_id;
            } else {
                $focus->contact_id=$module_id;
            }
            $focus->save();
        } // if
        return $return_id;
    }

    public function retrieveFile($id, $filename)
    {
        if (empty($filename)) {
            return '';
        }

        $this->upload_file->stored_file_name = $filename;
        $filepath = $this->upload_file->get_upload_path($id);
        if (file_exists($filepath)) {
            $file = file_get_contents($filepath);
            return base64_encode($file);
        }
        return -1;
    }
}
