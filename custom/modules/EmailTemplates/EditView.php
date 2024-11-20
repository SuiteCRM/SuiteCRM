<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $current_user;
require_once('modules/Campaigns/utils.php');

//if campaign_id is passed then we assume this is being invoked from the campaign module and in a popup.
$has_campaign = true;
$inboundEmail = true;
if (!isset($_REQUEST['campaign_id']) || empty($_REQUEST['campaign_id'])) {
    $has_campaign = false;
}
if (!isset($_REQUEST['inboundEmail']) || empty($_REQUEST['inboundEmail'])) {
    $inboundEmail = false;
}
$focus = BeanFactory::newBean('EmailTemplates');

if (isset($_REQUEST['record'])) {
    $focus->retrieve($_REQUEST['record']);
}

$old_id = '';
if (isset($_REQUEST['isDuplicate']) && $_REQUEST['isDuplicate'] == 'true') {
    $old_id = $focus->id; // for attachments down below
    $focus->id = "";
}


//setting default flag value so due date and time not required
if (!isset($focus->id)) {
    $focus->date_due_flag = 1;
}

//needed when creating a new case with default values passed in
if (isset($_REQUEST['contact_name']) && is_null($focus->contact_name)) {
    $focus->contact_name = htmlspecialchars($_REQUEST['contact_name'], ENT_QUOTES);
}
if (isset($_REQUEST['contact_id']) && is_null($focus->contact_id)) {
    $focus->contact_id = htmlspecialchars($_REQUEST['contact_id'], ENT_QUOTES);
}
if (isset($_REQUEST['parent_name']) && is_null($focus->parent_name)) {
    $focus->parent_name = htmlspecialchars($_REQUEST['parent_name'], ENT_QUOTES);
}
if (isset($_REQUEST['parent_id']) && is_null($focus->parent_id)) {
    $focus->parent_id = htmlspecialchars($_REQUEST['parent_id'], ENT_QUOTES);
}
if (isset($_REQUEST['parent_type'])) {
    $focus->parent_type = htmlspecialchars($_REQUEST['parent_type'], ENT_QUOTES);
} elseif (!isset($focus->parent_type)) {
    $focus->parent_type = $app_list_strings['record_type_default_key'];
}
if (isset($_REQUEST['filename']) && $_REQUEST['isDuplicate'] != 'true') {
    $focus->filename = htmlspecialchars($_REQUEST['filename'], ENT_QUOTES);
}

if ($has_campaign || $inboundEmail) {
    insert_popup_header($theme);
}


$params = array();

if (empty($focus->id)) {
    $params[] = $GLOBALS['app_strings']['LBL_CREATE_BUTTON_LABEL'];
} else {
    $params[] = "<a href='index.php?module={$focus->module_dir}&action=DetailView&record={$focus->id}'>{$focus->name}</a>";
    $params[] = $GLOBALS['app_strings']['LBL_EDIT_BUTTON_LABEL'];
}

echo getClassicModuleTitle($focus->module_dir, $params, true);

if (!$focus->ACLAccess('EditView') || (!is_admin($current_user) && isset($focus->type) && $focus->type === 'system')) {
    ACLController::displayNoAccess(true);
    sugar_cleanup(true);
}

$GLOBALS['log']->info("EmailTemplate detail view");

if ($has_campaign || $inboundEmail) {
    // STIC-Custom 20210816 MHP - Insert var in the subject body
	// STIC#368
    // STIC-Custom 20241002 AAM - Extend custom funcionality
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/442
    // $xtpl = new XTemplate('modules/EmailTemplates/EditView.html');
    $xtpl = new XTemplate(get_custom_file_if_exists('modules/EmailTemplates/EditView.html'));
    // END STIC
} else {
    // STIC-Custom 20241002 AAM - Extend custom funcionality
    // $xtpl = new XTemplate('modules/EmailTemplates/EditViewMain.html');
    $xtpl = new XTemplate(get_custom_file_if_exists('modules/EmailTemplates/EditViewMain.html'));
    // END STIC
} // else
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);

$xtpl->assign("LBL_ACCOUNT", $app_list_strings['moduleList']['Accounts']);
$xtpl->parse("main.variable_option");

$returnAction = 'index';
if (isset($_REQUEST['return_module'])) {
    $xtpl->assign("RETURN_MODULE", $_REQUEST['return_module']);
}
if (isset($_REQUEST['return_action'])) {
    $xtpl->assign("RETURN_ACTION", $_REQUEST['return_action']);
    $returnAction = $_REQUEST['return_action'];
}
if (isset($_REQUEST['return_id'])) {
    $xtpl->assign("RETURN_ID", $_REQUEST['return_id']);
}
// handle Create $module then Cancel
if (empty($_REQUEST['return_id'])) {
    $xtpl->assign("RETURN_ACTION", 'index');
}

if ($has_campaign || $inboundEmail) {
    $cancel_script = "window.close();";
} else {
    $cancel_script = "this.form.action.value='{$returnAction}'; this.form.module.value='{$_REQUEST['return_module']}';
    this.form.record.value=";
    if (empty($_REQUEST['return_id'])) {
        $cancel_script = "this.form.action.value='index'; this.form.module.value='{$_REQUEST['return_module']}';this.form.name.value='';this.form.description.value=''";
    } else {
        $cancel_script .= "'{$_REQUEST['return_id']}'";
    }
}

//Setup assigned user name
$popup_request_data = array(
    'call_back_function' => 'set_return',
    'form_name' => 'EditView',
    'field_to_name_array' => array(
        'id' => 'assigned_user_id',
        'user_name' => 'assigned_user_name',
    ),
);
$json = getJSONobj();
$xtpl->assign('encoded_assigned_users_popup_request_data', $json->encode($popup_request_data));
if (!empty($focus->assigned_user_name)) {
    $xtpl->assign("ASSIGNED_USER_NAME", $focus->assigned_user_name);
}

$xtpl->assign("assign_user_select", '<span class="suitepicon suitepicon-action-select"></span>');
$xtpl->assign("assign_user_clear", '<span class="suitepicon suitepicon-action-clear"></span>');
//Assign qsd script
require_once('include/QuickSearchDefaults.php');
$qsd = QuickSearchDefaults::getQuickSearchDefaults();
$sqs_objects = array('EditView_assigned_user_name' => $qsd->getQSUser());
$quicksearch_js = '<script type="text/javascript" language="javascript">sqs_objects = ' . $json->encode($sqs_objects) . '; enableQS();</script>';

$xtpl->assign("CANCEL_SCRIPT", $cancel_script);
$xtpl->assign("PRINT_URL", "index.php?" . $GLOBALS['request_string']);
$xtpl->assign("JAVASCRIPT", get_set_focus_js() . $quicksearch_js);

if (!is_file(sugar_cached('jsLanguage/') . $GLOBALS['current_language'] . '.js')) {
    require_once('include/language/jsLanguage.php');
    jsLanguage::createAppStringsCache($GLOBALS['current_language']);
}
$jsLang = getVersionedScript("cache/jsLanguage/{$GLOBALS['current_language']}.js", $GLOBALS['sugar_config']['js_lang_version']);
$xtpl->assign("JSLANG", $jsLang);

$xtpl->assign("ID", $focus->id);
if (isset($focus->name)) {
    $xtpl->assign("NAME", $focus->name);
} else {
    $xtpl->assign("NAME", "");
}

//Bug45632
/* BEGIN - SECURITY GROUPS */
/**
 * if(isset($focus->assigned_user_id)) $xtpl->assign("ASSIGNED_USER_ID", $focus->assigned_user_id); else $xtpl->assign("ASSIGNED_USER_ID", "");
 */
if (isset($focus->assigned_user_id)) {
    $xtpl->assign("ASSIGNED_USER_ID", $focus->assigned_user_id);
} else {
    if (empty($focus->id) && empty($focus->assigned_user_id)) {
        $xtpl->assign("ASSIGNED_USER_ID", $current_user->id);
        $xtpl->assign("ASSIGNED_USER_NAME", get_assigned_user_name($current_user->id));
    } else {
        $xtpl->assign("ASSIGNED_USER_ID", "");
    }
}
/* END - SECURITY GROUPS */
//Bug45632

if (isset($focus->description)) {
    $xtpl->assign("DESCRIPTION", $focus->description);
} else {
    $xtpl->assign("DESCRIPTION", "");
}
if (isset($focus->subject)) {
    $xtpl->assign("SUBJECT", $focus->subject);
} else {
    $xtpl->assign("SUBJECT", "");
}
if ($focus->published == 'on') {
    $xtpl->assign("PUBLISHED", "CHECKED");
}
//if text only is set to true, then make sure input is checked and value set to 1
if (isset($focus->text_only) && $focus->text_only) {
    $xtpl->assign("TEXTONLY_CHECKED", "CHECKED");
    $xtpl->assign("TEXTONLY_VALUE", "1");
} else {//set value to 0
    $xtpl->assign("TEXTONLY_VALUE", "0");
}

require_once("modules/EmailTemplates/templateFields.php");

$xtpl->assign("FIELD_DEFS_JS", generateFieldDefsJS2());
$xtpl->assign("LBL_CONTACT", $app_list_strings['moduleList']['Contacts']);

if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
    $record = '';
    if (!empty($_REQUEST['record'])) {
        $record = $_REQUEST['record'];
    }

    $xtpl->assign("ADMIN_EDIT", "<a href='index.php?action=index&module=DynamicLayout&from_action=" . $_REQUEST['action'] . "&from_module=" . $_REQUEST['module'] . "&record=" . $record . "'>" . SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDIT_LAYOUT']) . "</a>");
}
if (isset($focus->parent_type) && $focus->parent_type != "") {
    $change_parent_button = "<input title='" . $app_strings['LBL_SELECT_BUTTON_TITLE'] . "'
tabindex='3' type='button' class='button' value='" . $app_strings['LBL_SELECT_BUTTON_LABEL'] . "' name='button' LANGUAGE=javascript onclick='return
window.open(\"index.php?module=\"+ document.EditView.parent_type.value +
\"&action=Popup&html=Popup_picker&form=TasksEditView\",\"test\",\"width=600,height=400,resizable=1,scrollbars=1\");'>";
    $xtpl->assign("CHANGE_PARENT_BUTTON", $change_parent_button);
}
if ($focus->parent_type == "Account") {
    $xtpl->assign("DEFAULT_SEARCH", "&query=true&account_id=$focus->parent_id&account_name=" . urlencode($focus->parent_name));
}

$xtpl->assign("DESCRIPTION", $focus->description);
$xtpl->assign("TYPE_OPTIONS", get_select_options_with_id($app_list_strings['record_type_display'], $focus->parent_type));
//$xtpl->assign("DEFAULT_MODULE","Accounts");

if (isset($focus->body)) {
    $xtpl->assign("BODY", $focus->body);
} else {
    $xtpl->assign("BODY", "");
}
if (isset($focus->body_html)) {
    $xtpl->assign("BODY_HTML", $focus->body_html);
} else {
    $xtpl->assign("BODY_HTML", "");
}


// ---------------------------------
// ------------ EDITOR -------------
// ---------------------------------


// STIC-Custom 20210204 JCH - Customize tinymce editor & unfilter on save email templates
// The PR had conflict issues, and the changes were added later in the Maintenance merge
// STIC#7d3b251d359de4fe33ec8d22240c738436c58d43
// STIC#193
// STIC#189
require_once 'custom/modules/EmailTemplates/SticSuiteEditorConnector.php';
// require_once 'modules/EmailTemplates/SuiteEditorConnector.php';
// END STIC
$templateWidth = 600;
$xtpl->assign('template_width', $templateWidth);
// STIC-Custom 20210204 JCH - Customize tinymce editor & unfilter on save email templates
// The PR had conflict issues, and the changes were added later in the Maintenance merge
// STIC#7d3b251d359de4fe33ec8d22240c738436c58d43
// STIC#193
// STIC#189
// $xtpl->assign('BODY_EDITOR', SuiteEditorConnector::getHtml(SuiteEditorConnector::getSuiteSettings(isset($focus->body_html) ? html_entity_decode($focus->body_html) : '', $templateWidth)));
$xtpl->assign('BODY_EDITOR', SticSuiteEditorConnector::getSticHtml(SticSuiteEditorConnector::getSticSuiteSettings(isset($focus->body_html) ? html_entity_decode($focus->body_html) : '', $templateWidth)));
// END STIC
$xtpl->assign('width_style', 'style="display:'.($current_user->getEditorType() != 'mozaik' ? 'none' : 'table-row').';"');

// ---------------------------------
// ---------------------------------
// ---------------------------------


if (true) {
    if (!isTouchScreen()) {
        require_once("include/SugarTinyMCE.php");
        $tiny = new SugarTinyMCE();
        $tiny->defaultConfig['cleanup_on_startup'] = true;
        $tiny->defaultConfig['height'] = 600;
        $tiny->defaultConfig['plugins'] .= ",fullpage";
        $tinyHtml = $tiny->getInstance();
        $xtpl->assign("tiny", $tinyHtml);
    }
    ///////////////////////////////////////
    ////	MACRO VARS
    $xtpl->assign("INSERT_VARIABLE_ONCLICK", "insert_variable(document.EditView.variable_text.value, \"email_template_editor\")");

    // bug 37255, included without condition
    $xtpl->parse("main.NoInbound.variable_button");

    ///////////////////////////////////////
    ////	CAMPAIGNS
    if ($has_campaign || $inboundEmail) {
        $xtpl->assign("INPOPUPWINDOW", 'true');
        // STIC-Custom 20220404 MHP - Change the function that inserts the tracker in the email template when accessed from the marketing email
        // STIC#687
        // $xtpl->assign("INSERT_URL_ONCLICK", "insert_variable_html_link(document.EditView.tracker_url.value)");
        $xtpl->assign("INSERT_URL_ONCLICK", 'EmailTrackerController("insert", "{$CAMPAIGN_ID}")');
        // END STIC-Custom
        if ($has_campaign) {
            $campaign_urls = get_campaign_urls($_REQUEST['campaign_id']);
        }
        if (!empty($campaign_urls)) {
            $xtpl->assign("DEFAULT_URL_TEXT", key($campaign_urls));
        }
        if ($has_campaign) {
            $xtpl->assign("TRACKER_KEY_OPTIONS", get_select_options_with_id($campaign_urls, null));
            $xtpl->parse("main.NoInbound.tracker_url");
        }
    }

    // create option of "Contact/Lead/Task" from corresponding module
    // translations
    $lblContactAndOthers = implode('/', array(
        isset($app_list_strings['moduleListSingular']['Contacts']) ? $app_list_strings['moduleListSingular']['Contacts'] : 'Contact',
        isset($app_list_strings['moduleListSingular']['Leads']) ? $app_list_strings['moduleListSingular']['Leads'] : 'Lead',
        isset($app_list_strings['moduleListSingular']['Prospects']) ? $app_list_strings['moduleListSingular']['Prospects'] : 'Target',
    ));

    // The insert variable drodown should be conditionally displayed.
    // If it's campaign then hide the Account.
    if ($has_campaign) {
        $dropdown = "<option value='Contacts'>
						" . $lblContactAndOthers . "
			       </option>";
        $xtpl->assign("DROPDOWN", $dropdown);
        $xtpl->assign("DEFAULT_MODULE", 'Contacts');
    //$xtpl->assign("CAMPAIGN_POPUP_JS", '<script type="text/javascript" src="include/javascript/sugar_3.js"></script>');
    } else {
        // STIC-Custom - JBL - 20240830 - Notifications: Filter modules for Notifications
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/44
        // $xtpl->assign("DROPDOWN", genDropDownJS2());
        // $xtpl->assign("DEFAULT_MODULE", 'Accounts');
        if (isset($_REQUEST['type']) && $_REQUEST['type'] == "notification") {
            $dropdown = '';
            $defaultModule = isset($_REQUEST['parent_type']) ? $_REQUEST['parent_type'] : "";
            if(!array_key_exists($defaultModule, $app_list_strings['parent_type_display_notifications'])) {
                $defaultModule = "";
            }

            if ($defaultModule != "") {
                // Add $defaultModule as first module in dropdown
                if (isset($app_list_strings['moduleListSingular'][$defaultModule])) {
                    $dropdown .= "<option value='" . $defaultModule . "'>
                        " . $app_list_strings['moduleListSingular'][$defaultModule] . "
                        </option>";
                } else {
                    $dropdown .= "<option value='" . $defaultModule . "'>
                        " . $app_list_strings['moduleList'][$defaultModule] . "
                        </option>";
                }
            }
            array_multisort($app_list_strings['parent_type_display_notifications'], SORT_ASC, $app_list_strings['parent_type_display_notifications']);
            foreach ($app_list_strings['parent_type_display_notifications'] as $key => $name) {
                if ($key != $defaultModule) {
                    if (isset($app_list_strings['moduleListSingular'][$key])) {
                        $dropdown .= "<option value='" . $key . "'>
                            " . $app_list_strings['moduleListSingular'][$key] . "
                            </option>";
                    } else {
                        $dropdown .= "<option value='" . $key . "'>
                            " . $app_list_strings['moduleList'][$key] . "
                            </option>";
                    }
                    if ($defaultModule == "") {
                        $defaultModule = $key;
                    }
                }
            }
            $xtpl->assign("DROPDOWN", $dropdown);
            $xtpl->assign("DEFAULT_MODULE", $defaultModule);
        } else {
            $xtpl->assign("DROPDOWN", genDropDownJS2());
            $xtpl->assign("DEFAULT_MODULE", 'Accounts');
        }
        // END STIC-Custom
    }
    ////	END CAMPAIGNS
    ///////////////////////////////////////

    ///////////////////////////////////////
    ////    ATTACHMENTS
    $attachments = '';
    if (!empty($focus->id)) {
        $etid = $focus->id;
    } elseif (!empty($old_id)) {
        $xtpl->assign('OLD_ID', $old_id);
        $etid = $old_id;
    }
    if (!empty($etid)) {
        $note = BeanFactory::newBean('Notes');
        $where = "notes.parent_id='{$etid}' AND notes.filename IS NOT NULL";
        $notes_list = $note->get_full_list("", $where, true);

        if (!isset($notes_list)) {
            $notes_list = array();
        }
        for ($i = 0; $i < count($notes_list); $i++) {
            $the_note = $notes_list[$i];
            if (empty($the_note->filename)) {
                continue;
            }
            $secureLink = 'index.php?entryPoint=download&id=' . $the_note->id . '&type=Notes';
            $attachments .= '<input type="checkbox" name="remove_attachment[]" value="' . $the_note->id . '"> ' . $app_strings['LNK_REMOVE'] . '&nbsp;&nbsp;';
            $attachments .= '<a href="' . $secureLink . '" target="_blank">' . $the_note->filename . '</a><br>';
        }
    }
    $attJs = '<script type="text/javascript">';
    $attJs .= 'var lnk_remove = "' . $app_strings['LNK_REMOVE'] . '";';
    $attJs .= '</script>';
    $xtpl->assign('ATTACHMENTS', $attachments);
    $xtpl->assign('ATTACHMENTS_JAVASCRIPT', $attJs);

    ////    END ATTACHMENTS
    ///////////////////////////////////////
    $templateType = !empty($focus->type) ? $focus->type : '';
    if ($has_campaign) {
        if (empty($_REQUEST['record'])) {
            // new record, default to campaign
            $xtpl->assign("TYPEDROPDOWN", get_select_options_with_id($app_list_strings['emailTemplates_type_list_campaigns'], 'campaign'));
        } else {
            $xtpl->assign("TYPEDROPDOWN", get_select_options_with_id($app_list_strings['emailTemplates_type_list_campaigns'], $templateType));
        }
    } else {
        // if the type is workflow, we will show it
        // otherwise we don't allow user to select workflow type because workflow type email template
        // should be created from within workflow module because it requires more fields (such as base module, etc)
        if ($templateType == 'workflow') {
            $xtpl->assign("TYPEDROPDOWN", get_select_options_with_id($app_list_strings['emailTemplates_type_list'], $templateType));
        } else {
            // STIC-Custom - JBL - 20240830 - Notifications: Edit EmailTemplates with fixed type
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/44
            // $xtpl->assign("TYPEDROPDOWN", get_select_options_with_id($app_list_strings['emailTemplates_type_list_no_workflow'], $templateType));
            if (isset($_REQUEST['type']) && !empty($_REQUEST['type'])) {
                $type_list = [$_REQUEST['type'] => $app_list_strings['emailTemplates_type_list_no_workflow'][$_REQUEST['type']]];
                $selected_item = $_REQUEST['type'];
                if ($templateType != "" && $templateType != $_REQUEST['type']) {
                    $type_list[$templateType] = $app_list_strings['emailTemplates_type_list_no_workflow'][$templateType];
                    $selected_item = $templateType;
                }
                $xtpl->assign("TYPEDROPDOWN", get_select_options_with_id($type_list, $selected_item));
            } else {
                $xtpl->assign("TYPEDROPDOWN", get_select_options_with_id($app_list_strings['emailTemplates_type_list_no_workflow'], $templateType));
            }
            // END STIC-Custom
        }
    }
    // done and parse
    $xtpl->parse("main.textarea");
}

//Add Custom Fields
require_once('modules/DynamicFields/templates/Files/EditView.php');
$xtpl->parse("main.NoInbound");
if (!$inboundEmail) {
    $xtpl->parse("main.NoInbound1");
    $xtpl->parse("main.NoInbound2");
    $xtpl->parse("main.NoInbound3");
}
$xtpl->parse("main.NoInbound4");
$xtpl->parse("main.NoInbound5");
$xtpl->parse("main");

$xtpl->out("main");

$javascript = new javascript();
$javascript->setFormName('EditView');
$javascript->setSugarBean($focus);
$javascript->addAllFields('');
echo $javascript->getScript();
