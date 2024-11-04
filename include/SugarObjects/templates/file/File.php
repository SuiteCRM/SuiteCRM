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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/SugarObjects/templates/basic/Basic.php';
require_once 'include/upload_file.php';
require_once 'include/formbase.php';

class File extends Basic
{
    public $file_url;
    public $file_url_noimage;
    public $file_ext;
    public $document_name;
    public $filename;
    public $uploadfile;
    public $status;
    public $file_mime_type;
    public $show_preview = true;


    /**
     * File constructor.
     */
    public function __construct()
    {
        parent::__construct();
    }



    /**
     * @see SugarBean::save()
     *
     * @param bool $check_notify
     *
     * @return string
     */
    public function save($check_notify = false)
    {
        if (!empty($this->uploadfile)) {
            $this->filename = $this->uploadfile;
        }

        return parent::save($check_notify);
    }

    /**
     * @see SugarBean::fill_in_additional_detail_fields()
     */
    public function fill_in_additional_detail_fields()
    {
        global $app_list_strings;
        global $img_name;
        global $img_name_bare;

        $this->uploadfile = $this->filename;

        // Bug 41453 - Make sure we call the parent method as well
        parent::fill_in_additional_detail_fields();

        if (!empty($this->file_ext)) {
            $img_name = SugarThemeRegistry::current()->getImageURL(strtolower($this->file_ext) . '_image_inline.gif');
            $img_name_bare = strtolower($this->file_ext) . '_image_inline';
        }

        if (empty($this->filename ?? '') || stripos($this->filename, 'svg') || stripos($this->file_mime_type ?? '', 'svg')){
            $this->show_preview = false;
        }

        //set default file name.
        if (!empty($img_name) && file_exists($img_name)) {
            $img_name = $img_name_bare;
        } else {
            $img_name = 'def_image_inline'; //todo change the default image.
        }
        $this->file_url_noimage = $this->id;

        if (!empty($this->status_id)) {
            $this->status = $app_list_strings['document_status_dom'][$this->status_id];
        }
    }

    /**
     * @see SugarBean::retrieve()
     *
     * @param int $id
     * @param bool $encode
     * @param bool $deleted
     *
     * @return SugarBean
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        $ret_val = parent::retrieve($id, $encode, $deleted);

        //If statement added to prevent the 'name' from being overwritten with a null value
        if ($this->document_name !== null) {
            $this->name = $this->document_name;
        }

        return $ret_val;
    }

    /**
     * Method to delete an attachment
     *
     * @param string $isDuplicate
     *
     * @return bool
     */
    public function deleteAttachment($isDuplicate = 'false')
    {
        if ($this->ACLAccess('edit')) {
            if ($isDuplicate === 'true') {
                return true;
            }
            $removeFile = "upload://{$this->id}";
        }
        if (file_exists($removeFile)) {
            if (!unlink($removeFile)) {
                $GLOBALS['log']->error("*** Could not unlink() file: [ {$removeFile} ]");
            } else {
                $this->uploadfile = '';
                $this->filename = '';
                $this->file_mime_type = '';
                $this->file_ext = '';
                $this->save();

                return true;
            }
        } else {
            $this->uploadfile = '';
            $this->filename = '';
            $this->file_mime_type = '';
            $this->file_ext = '';
            $this->save();

            return true;
        }

        return false;
    }
}
