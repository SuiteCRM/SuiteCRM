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


require_once('include/JSON.php');
require_once('include/entryPoint.php');
require_once 'include/upload_file.php';

global $sugar_config;
$supportedExtensions = array('jpg', 'png', 'jpeg');
$json = getJSONobj();
$rmdir=true;
$returnArray = array();
if ($json->decode(html_entity_decode((string) $_REQUEST['forQuotes']))) {
    $returnArray['forQuotes']="quotes";
} else {
    $returnArray['forQuotes']="company";
}
$upload_ok = false;
$upload_path = 'tmp_logo_' . $returnArray['forQuotes'] . '_upload';
if (isset($_FILES['file_1'])) {
    $upload = new UploadFile('file_1');
    if ($upload->confirm_upload()) {
        $upload_dir  = 'upload://' . $upload_path;
        UploadStream::ensureDir($upload_dir);
        if (!verify_uploaded_image($upload->temp_file_location, $returnArray['forQuotes'] == 'quotes')) {
            $returnArray['data']='other';
            $returnArray['path'] = '';
            echo $json->encode($returnArray);
            sugar_cleanup();
            exit();
        }
        $file_name = $upload_dir . '/' . str_replace(' ', '_', (string) $upload->get_stored_file_name());
        if ($upload->final_move($file_name)) {
            $upload_ok = true;
        }
    }
}
if (!$upload_ok) {
    $returnArray['data']='not_recognize';
    echo $json->encode($returnArray);
    sugar_cleanup();
    exit();
}
if (file_exists($file_name) && is_file($file_name)) {
    $encoded_file_name = rawurlencode(str_replace(' ', '_', (string) $upload->get_stored_file_name()));
    $returnArray['path'] = $upload_path . '/' . $encoded_file_name;
    $returnArray['url']= 'cache/images/'.$encoded_file_name;
    if (!verify_uploaded_image($file_name, $returnArray['forQuotes'] == 'quotes')) {
        $returnArray['data']='other';
        $returnArray['path'] = '';
        unlink($file_name);
    } else {
        $img_size = getimagesize($file_name);
        $filetype = $img_size['mime'];
        $test=$img_size[0]/$img_size[1];
        if (($test>10 || $test<1) && $returnArray['forQuotes'] == 'company') {
            $rmdir=false;
            $returnArray['data']='size';
        }
        if (($test>20 || $test<3)&& $returnArray['forQuotes'] == 'quotes') {
            $returnArray['data']='size';
        }

        if (!is_dir(sugar_cached('images'))) {
            try {
                sugar_mkdir(sugar_cached('images'));
            } catch (Exception $exception) {
                $GLOBALS['log']->fatal('Unable to create cache directory \'images\'');
            }
        }

        copy($file_name, sugar_cached('images/' . str_replace(' ', '_', (string) $upload->get_stored_file_name())));
    }
    if (!empty($returnArray['data'])) {
        echo $json->encode($returnArray);
    } else {
        $rmdir=false;
        $returnArray['data']='ok';
        echo $json->encode($returnArray);
    }
} else {
    $returnArray['data']='file_error';
    echo $json->encode($returnArray);
}
sugar_cleanup();
exit();
