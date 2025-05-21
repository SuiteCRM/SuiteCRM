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

require_once('include/SugarFields/Fields/File/SugarFieldFile.php');

class SugarFieldImage extends SugarFieldFile
{
    public function getListViewSmarty($parentFieldArray, $vardef, $displayParams, $col)
    {
        if (isset($displayParams['module']) && !empty($displayParams['module'])) {
            $this->ss->assign("module", $displayParams['module']);
        } else {
            $this->ss->assign("module", $_REQUEST['module']);
        }
        return parent::getListViewSmarty($parentFieldArray, $vardef, $displayParams, $col);
    }
    public function save(&$bean, $params, $field, $vardef, $prefix = '')
    {
        $fakeDisplayParams = array();
        $this->fillInOptions($vardef, $fakeDisplayParams);

        require_once('include/upload_file.php');
        $upload_file = new UploadFile($prefix . $field . '_file');
        //remove file
        if (isset($_REQUEST['remove_file_' . $field]) && $params['remove_file_' . $field] == 1) {
            $upload_file->unlink_file($bean->$field);
            $bean->$field = "";
        }

        $move = false;
        if (isset($_FILES[$prefix . $field . '_file']) && $upload_file->confirm_upload()) {
            if ($this->verify_image($upload_file)) {
                $bean->$field = $upload_file->get_stored_file_name();
                $move = true;
            } else {
                //not valid image.
                $GLOBALS['log']->fatal("Image Field : Not a Valid Image.");
                $temp = $vardef['vname'];
                $temp = translate($temp, $bean->module_name);
                SugarApplication::appendErrorMessage($temp . " Field :  Not a valid image format.");
            }
        }

        if (empty($bean->id)) {
            $bean->id = create_guid();
            $bean->new_with_id = true;
        }

        if ($move) {
            $upload_file->final_move($bean->id . '_' . $field); //BEAN ID IS THE FILE NAME IN THE INSTANCE.

            $docType = isset($vardef['docType']) && isset($params[$prefix . $vardef['docType']]) ?
                $params[$prefix . $vardef['docType']] : '';
            $upload_file->upload_doc($bean, $bean->id, $docType, $bean->$field, $upload_file->mime_type);
        } else {
            if (!empty($old_id)) {
                // It's a duplicate, I think

                if (empty($params[$prefix . $vardef['docUrl']])) {
                    $upload_file->duplicate_file($old_id, $bean->id, $bean->$field);
                } else {
                    $docType = $vardef['docType'];
                    $bean->$docType = $params[$prefix . $field . '_old_doctype'];
                }
            } else {
                if (!empty($params[$prefix . $field . '_remoteName'])) {
                    // We aren't moving, we might need to do some remote linking
                    $displayParams = array();
                    $this->fillInOptions($vardef, $displayParams);

                    if (isset($params[$prefix . $vardef['docId']])
                && !empty($params[$prefix . $vardef['docId']])
                && isset($params[$prefix . $vardef['docType']])
                && !empty($params[$prefix . $vardef['docType']])
            ) {
                        $bean->$field = $params[$prefix . $field . '_remoteName'];

                        require_once('include/utils/file_utils.php');
                        $extension = get_file_extension($bean->$field);
                        if (!empty($extension)) {
                            $bean->file_ext = $extension;
                            $bean->file_mime_type = get_mime_content_type_from_filename($bean->$field);
                        }
                    }
                }
            }
        }
    }

    public function verify_image($upload_file)
    {
        if (verify_uploaded_image($upload_file->temp_file_location)) {
            return true;
        }

        return false;
    }
    private function fillInOptions(&$vardef, &$displayParams)
    {
        if (isset($vardef['allowEapm']) && $vardef['allowEapm'] == true) {
            if (empty($vardef['docType'])) {
                $vardef['docType'] = 'doc_type';
            }
            if (empty($vardef['docId'])) {
                $vardef['docId'] = 'doc_id';
            }
            if (empty($vardef['docUrl'])) {
                $vardef['docUrl'] = 'doc_url';
            }
        } else {
            $vardef['allowEapm'] = false;
        }

        // Override the default module
        if (isset($vardef['linkModuleOverride'])) {
            $vardef['linkModule'] = $vardef['linkModuleOverride'];
        } else {
            $vardef['linkModule'] = '{$module}';
        }

        // This is needed because these aren't always filled out in the edit/detailview defs
        if (!isset($vardef['fileId'])) {
            if (isset($displayParams['id'])) {
                $vardef['fileId'] = $displayParams['id'];
            } else {
                $vardef['fileId'] = 'id';
            }
        }
    }
}
