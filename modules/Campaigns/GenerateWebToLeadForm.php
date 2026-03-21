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


require_once('include/formbase.php');




require_once('include/utils/db_utils.php');




global $app_list_strings, $app_strings,$mod_strings;

$site_url = $sugar_config['site_url'];
$web_form_header = $mod_strings['LBL_LEAD_DEFAULT_HEADER'];
$web_form_description = $mod_strings['LBL_DESCRIPTION_TEXT_LEAD_FORM'];
$web_form_submit_label = $mod_strings['LBL_DEFAULT_LEAD_SUBMIT'];
$web_form_required_fields_msg = $mod_strings['LBL_PROVIDE_WEB_TO_LEAD_FORM_FIELDS'];
$web_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
$web_not_valid_email_address = $mod_strings['LBL_NOT_VALID_EMAIL_ADDRESS'];
$web_post_url = $site_url.'/index.php?entryPoint=WebToPersonCapture';
$web_redirect_url = '';
$web_notify_campaign = '';
$web_assigned_user = '';
$web_team_user = '';
$web_form_footer = '';
$regex = "/^\w+(['\.\-\+]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,})+\$/";

$moduleDir = '';
if (!empty($_REQUEST['moduleDir'])) {
    $moduleDir= $_REQUEST['moduleDir'];
}

if (!empty($_REQUEST['web_header'])) {
    $web_form_header= $_REQUEST['web_header'];
}
if (!empty($_REQUEST['web_description'])) {
    $web_form_description= $_REQUEST['web_description'];
}
if (!empty($_REQUEST['web_submit'])) {
    $web_form_submit_label=to_html($_REQUEST['web_submit']);
}
if (!empty($_REQUEST['post_url'])) {
    $web_post_url= $_REQUEST['post_url'];
}
if (!empty($_REQUEST['redirect_url']) && $_REQUEST['redirect_url'] !="http://") {
    $web_redirect_url= $_REQUEST['redirect_url'];
}
if (!empty($_REQUEST['notify_campaign'])) {
    $web_notify_campaign = $_REQUEST['notify_campaign'];
}
if (!empty($_REQUEST['web_footer'])) {
    $web_form_footer= $_REQUEST['web_footer'];
}
if (!empty($_REQUEST['campaign_id'])) {
    $web_form_campaign= $_REQUEST['campaign_id'];
}
if (!empty($_REQUEST['assigned_user_id'])) {
    $web_assigned_user = $_REQUEST['assigned_user_id'];
}

$typeOfPerson = !empty($_REQUEST['typeOfPerson']) ? $_REQUEST['typeOfPerson'] : 'Lead';
 $person = new $typeOfPerson();
 $fieldsMetaData = BeanFactory::newBean('EditCustomFields');
 $xtpl=new XTemplate('modules/Campaigns/WebToLeadForm.html');
 $xtpl->assign("MOD", $mod_strings);
 $xtpl->assign("APP", $app_strings);

include_once 'WebToLeadFormBuilder.php';
$Web_To_Lead_Form_html = WebToLeadFormBuilder::generate(
    $_REQUEST,
    $person,
    $moduleDir,
    $site_url,
    $web_post_url,
    $web_form_header,
    $web_form_description,
    $app_list_strings,
    $web_required_symbol,
    $web_form_footer,
    $web_form_submit_label,
    $web_form_campaign,
    $web_redirect_url,
    $web_assigned_user,
    $web_form_required_fields_msg
);
$xtpl->assign("BODY", $Web_To_Lead_Form_html ? $Web_To_Lead_Form_html : '');
$xtpl->assign("BODY_HTML", $Web_To_Lead_Form_html ? $Web_To_Lead_Form_html : '');


require_once('include/SugarTinyMCE.php');
$tiny = new SugarTinyMCE();
$tiny->defaultConfig['height'] = 700;
$tinyConfig = $tiny->getConfig();
$js = '<script type="text/javascript">';
$js .= $tinyConfig;
$js .= "tinyConfig.selector = '#body_html';";
$js .= "tinyConfig.extended_valid_elements += ',input[name|id|type|value|onclick|required|placeholder|title],select[name|id|multiple|tabindex|onchange|required|title],option[value|selected|title]';";
$js .= 'tinymce.init(tinyConfig);';
$js .= '</script>';
$xtpl->assign("tiny", $js);

$xtpl->parse("main.textarea");

$xtpl->assign("INSERT_VARIABLE_ONCLICK", "insert_variable_html(document.EditView.variable_text.value)");
$xtpl->parse("main.variable_button");




$xtpl->parse("main");
$xtpl->out("main");

function ifRadioButton($customFieldName)
{
    $custRow = null;
    $query="select id,type from fields_meta_data where deleted = 0 and name = '$customFieldName'";
    $result=DBManagerFactory::getInstance()->query($query);
    $row = DBManagerFactory::getInstance()->fetchByAssoc($result);
    if ($row != null && $row['type'] == 'radioenum') {
        return $custRow = $row;
    }
    return $custRow;
}
