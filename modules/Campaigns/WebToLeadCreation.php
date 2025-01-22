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

/*********************************************************************************
 * Description:  WebToLeadCreation
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\Utility\SuiteValidator;

require_once 'include/EditView/EditView2.php';

require_once 'modules/Campaigns/utils.php';

global $mod_strings, $app_list_strings, $app_strings, $current_user, $import_bean_map,$import_file_name, $theme;

$xtpl=new XTemplate('modules/Campaigns/WebToLeadCreation.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);
if (isset($_REQUEST['module'])) {
    $xtpl->assign("MODULE", $_REQUEST['module']);
}
if (isset($_REQUEST['return_module'])) {
    $xtpl->assign("RETURN_MODULE", $_REQUEST['return_module']);
}
if (isset($_REQUEST['return_id'])) {
    $xtpl->assign("RETURN_ID", $_REQUEST['return_id']);
}
if (isset($_REQUEST['return_id'])) {
    $xtpl->assign("RETURN_ACTION", $_REQUEST['return_action']);
}
if (isset($_REQUEST['record'])) {
    $xtpl->assign("RECORD", $_REQUEST['record']);
}

global $theme;
global $currentModule;

$ev = new EditView;

$subclasses = getListOfExtendingClasses("Person");

$beanList = filterFieldsFromBeans($subclasses);

$xtpl->assign("BEAN_LIST", json_encode($beanList));

$personTypeList = "<select id='personTypeSelect'>";
if ((is_countable($beanList) ? count($beanList) : 0) > 0) {
    $count=0;
    foreach ($beanList as $b) {
        $personTypeList.="<option value='".$count."'>".$b->name."</option>";
        $count++;
    }
} else {
    $personTypeList.="<option value='noPerson'>No matching types</option>";
}

$site_url = $sugar_config['site_url'];
$web_post_url = $site_url.'/index.php?entryPoint=WebToPersonCapture';
$json = getJSONobj();
// Users Popup
$popup_request_data = array(
    'call_back_function' => 'set_return',
    'form_name' => 'WebToLeadCreation',
    'field_to_name_array' => array(
        'id' => 'assigned_user_id',
        'user_name' => 'assigned_user_name',
        ),
    );
$xtpl->assign('encoded_users_popup_request_data', $json->encode($popup_request_data));

//Campaigns popup
$popup_request_data = array(
        'call_back_function' => 'set_return',
        'form_name' => 'WebToLeadCreation',
        'field_to_name_array' => array(
            'id' => 'campaign_id',
            'name' => 'campaign_name',
        ),
);
$encoded_users_popup_request_data = $json->encode($popup_request_data);
$xtpl->assign('encoded_campaigns_popup_request_data', $json->encode($popup_request_data));

$field_defs_js = "var field_defs = {'Contacts':[";

$xtpl->assign("WEB_POST_URL", $web_post_url);

if (!empty($focus)) {
    if (empty($focus->assigned_user_id) && empty($focus->id)) {
        $focus->assigned_user_id = $current_user->id;
    }
    if (empty($focus->assigned_name) && empty($focus->id)) {
        $focus->assigned_user_name = $current_user->user_name;
    }
    $xtpl->assign("ASSIGNED_USER_NAME", $focus->assigned_user_name);
    $xtpl->assign("ASSIGNED_USER_ID", $focus->assigned_user_id);
} else {
    $xtpl->assign("ASSIGNED_USER_NAME", $current_user->user_name);
    $xtpl->assign("ASSIGNED_USER_ID", $current_user->id);
}


$xtpl->assign("REDIRECT_URL_DEFAULT", 'http://');

$isValidator = new SuiteValidator();


if (isset($_REQUEST['campaign_id']) && $isValidator->isValidId($_REQUEST['campaign_id'])) {
    $campaign = BeanFactory::newBean('Campaigns');
    if ($campaign) {
        $campaign->retrieve($_REQUEST['campaign_id']);
        $xtpl->assign('CAMPAIGN_ID', $campaign->id);
        $xtpl->assign('CAMPAIGN_NAME', $campaign->name);
    }
}

$xtpl->parse("main");
$xtpl->out("main");


//This is a generic method to allow for returning all of the sub-classes of a particular class
//This is used to allow for us to pass in person and get [Lead,Contact,Prospect,...]
function getListOfExtendingClasses($superclass)
{
    $subclasses = array();
    foreach ($GLOBALS['moduleList'] as $mod) {
        $item = BeanFactory::getBean($mod);
        if ($item && is_subclass_of($item, $superclass)) {
            $subclasses[] = $item;
        }
    }
    return $subclasses;
}
