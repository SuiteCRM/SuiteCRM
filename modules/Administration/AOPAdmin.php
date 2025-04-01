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

require_once 'modules/AOP_Case_Updates/util.php';

global $current_user, $sugar_config;
global $mod_strings;
global $app_list_strings;
global $app_strings;
global $theme;

if (!is_admin($current_user)) {
    sugar_die("Unauthorized access to administration.");
}

require_once('modules/Configurator/Configurator.php');

echo getClassicModuleTitle(
    "Administration",
    array(
        "<a href='index.php?module=Administration&action=index'>" .
        translate('LBL_MODULE_NAME', 'Administration') .
        "</a>",
        $mod_strings['LBL_AOP_ADMIN_MANAGE_AOP'],
    ),
    false
);

$cfg = new Configurator();
$sugar_smarty = new Sugar_Smarty();
$errors = array();

if (!array_key_exists('aop', $cfg->config)) {
    $cfg->config['aop'] = array(
        'enable_aop' => 1,
        'enable_portal' => '',
        'joomla_url' => '',
        'joomla_access_key' => '',
        'distribution_method' => '',
        'distribution_options' => '',
        'distribution_user_id' => '',
        'user_email_template_id' => '',
        'contact_email_template_id' => '',
        'case_creation_email_template_id' => '',
        'case_closure_email_template_id' => '',
        'joomla_account_creation_email_template_id' => '',
        'support_from_address' => '',
        'support_from_name' => '',
        'case_status_changes' => json_encode(array()),
    );
}
if (!array_key_exists('enable_aop', $cfg->config['aop'])) {
    $cfg->config['aop']['enable_aop'] = 1;
}
if (isset($_REQUEST['do']) && $_REQUEST['do'] == 'save') {
    $joomlaUrl = strtolower(trim($_REQUEST['joomla_url']));
    if (!empty($joomlaUrl)) {
        $cfg->config['aop']['joomla_url'] =
            preg_match("@^https?://@", $joomlaUrl) ? $joomlaUrl : 'http://' . $joomlaUrl;
    } else {
        $cfg->config['aop']['joomla_url'] = '';
    }
    $cfg->config['aop']['enable_aop'] = !empty($_REQUEST['enable_aop']);
    $cfg->config['aop']['enable_portal'] = !empty($_REQUEST['enable_portal']);
    $cfg->config['aop']['joomla_access_key'] = $_REQUEST['joomla_access_key'];
    $cfg->config['aop']['distribution_method'] = $_REQUEST['distribution_method'];
    $cfg->config['aop']['distribution_user_id'] = $_REQUEST['distribution_user_id'];
    $cfg->config['aop']['distribution_options'] = $_REQUEST['distribution_options'];
    $cfg->config['aop']['user_email_template_id'] = $_REQUEST['user_email_template_id'];
    $cfg->config['aop']['contact_email_template_id'] = $_REQUEST['contact_email_template_id'];
    $cfg->config['aop']['case_creation_email_template_id'] = $_REQUEST['case_creation_email_template_id'];
    $cfg->config['aop']['case_closure_email_template_id'] = $_REQUEST['case_closure_email_template_id'];
    $cfg->config['aop']['joomla_account_creation_email_template_id'] =
        $_REQUEST['joomla_account_creation_email_template_id'];
    $cfg->config['aop']['support_from_address'] = $_REQUEST['support_from_address'];
    $cfg->config['aop']['support_from_name'] = $_REQUEST['support_from_name'];

    if(isset($_REQUEST['inbound_email_case_macro'])){
        $cfg->config['inbound_email_case_subject_macro'] = $_REQUEST['inbound_email_case_macro'];
    }

    /*
     * We save the case_status_changes array as json since the way config changes are persisted to config.php
     * means that removing entries is tricky. json simplifies this.
     */
    $cfg->config['aop']['case_status_changes'] = json_encode(array_combine($_POST['if_status'] ?? [], $_POST['then_status'] ?? []));
    $cfg->saveConfig();
    header('Location: index.php?module=Administration&action=index');
    exit();
}
$distribStrings = $app_list_strings['dom_email_distribution_for_auto_create'];
unset($distribStrings['AOPDefault']);
$distributionMethod = get_select_options_with_id($distribStrings, $cfg->config['aop']['distribution_method']);
$distributionOptionsValue = $cfg->config['aop']['distribution_options'] ?? '';
$distributionOptions = getAOPAssignField('distribution_options', $distributionOptionsValue);

if (!empty($cfg->config['aop']['distribution_user_id'])) {
    $distributionUserName = BeanFactory::getBean("Users", $cfg->config['aop']['distribution_user_id'])->name;
} else {
    $distributionUserName = '';
}

$sugar_smarty->assign('distribution_user_name', $distributionUserName);

$emailTemplateList = get_bean_select_array(true, 'EmailTemplate', 'name', '', 'name');

$userEmailTemplateDropdown =
    get_select_options_with_id($emailTemplateList, $cfg->config['aop']['user_email_template_id']);
$contactEmailTemplateDropdown =
    get_select_options_with_id($emailTemplateList, $cfg->config['aop']['contact_email_template_id']);
$creationEmailTemplateDropdown =
    get_select_options_with_id($emailTemplateList, $cfg->config['aop']['case_creation_email_template_id']);
$closureEmailTemplateDropdown =
    get_select_options_with_id($emailTemplateList, $cfg->config['aop']['case_closure_email_template_id']);
$joomlaEmailTemplateDropdown =
    get_select_options_with_id($emailTemplateList, $cfg->config['aop']['joomla_account_creation_email_template_id']);

$case = BeanFactory::newBean('Cases');
$inboundEmailCaseMacro = $cfg->config['inbound_email_case_subject_macro'] ?? $case->getEmailSubjectMacro();

$sugar_smarty->assign('inbound_email_case_macro', $inboundEmailCaseMacro);
$sugar_smarty->assign('USER_EMAIL_TEMPLATES', $userEmailTemplateDropdown);
$sugar_smarty->assign('CONTACT_EMAIL_TEMPLATES', $contactEmailTemplateDropdown);
$sugar_smarty->assign('CREATION_EMAIL_TEMPLATES', $creationEmailTemplateDropdown);
$sugar_smarty->assign('CLOSURE_EMAIL_TEMPLATES', $closureEmailTemplateDropdown);
$sugar_smarty->assign('JOOMLA_EMAIL_TEMPLATES', $joomlaEmailTemplateDropdown);

$sugar_smarty->assign('DISTRIBUTION_METHOD', $distributionMethod);
$sugar_smarty->assign('DISTRIBUTION_OPTIONS', $distributionOptions);
$sugar_smarty->assign('MOD', $mod_strings);
$sugar_smarty->assign('APP', $app_strings);
$sugar_smarty->assign('APP_LIST', $app_list_strings);
$sugar_smarty->assign('LANGUAGES', get_languages());
$sugar_smarty->assign("JAVASCRIPT", get_set_focus_js());
$sugar_smarty->assign('config', $cfg->config['aop']);
$sugar_smarty->assign('error', $errors);

$cBean = BeanFactory::getBean('Cases');
$statusDropdown = get_select_options($app_list_strings[$cBean->field_name_map['status']['options']], '');
$currentStatuses = '';

if ($cfg->config['aop']['case_status_changes']) {
    foreach (json_decode((string) $cfg->config['aop']['case_status_changes']) as $if => $then) {
        $ifDropdown = get_select_options($app_list_strings[$cBean->field_name_map['status']['options']], $if);
        $thenDropdown = get_select_options($app_list_strings[$cBean->field_name_map['status']['options']], $then);
        $currentStatuses .= getStatusRowTemplate($mod_strings, $ifDropdown, $thenDropdown) . "\n";
    }
}


$sugar_smarty->assign('currentStatuses', $currentStatuses);

$buttons = <<<EOQ
    <input title="{$app_strings['LBL_SAVE_BUTTON_TITLE']}"
                       accessKey="{$app_strings['LBL_SAVE_BUTTON_KEY']}"
                       class="button primary"
                       type="submit"
                       name="save"
                       onclick="return check_form('ConfigureSettings');"
                       value="  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  " >
                &nbsp;<input title="{$mod_strings['LBL_CANCEL_BUTTON_TITLE']}"  onclick="document.location.href='index.php?module=Administration&action=index'" class="button"  type="button" name="cancel" value="  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  " >
EOQ;

$sugar_smarty->assign("BUTTONS", $buttons);

$sugar_smarty->display('modules/Administration/AOPAdmin.tpl');

$javascript = new javascript();
$javascript->setFormName('ConfigureSettings');
echo $javascript->getScript();
?>
    <script type="text/template" id="statusRowTemplate">
        <?= getStatusRowTemplate($mod_strings, $statusDropdown, $statusDropdown) ?>
    </script>
    <script language="Javascript" type="text/javascript">

      var selectElement = document.getElementById('distribution_method_select');
      selectElement.onchange = function (event) {
        var distribRow = document.getElementById('distribution_user_row');
        var distribOptionsRow = document.getElementById('distribution_options_row');
        if (selectElement.value == 'singleUser') {
          showElem('distribution_user_row');
          hideElem('distribution_options_row');
          addToValidate('ConfigureSettings', 'distribution_user_id', 'relate', true, "Please choose a user to assign cases to.");
        } else {
          SUGAR.clearRelateField(this.form, 'distribution_user_name', 'distribution_user_id');
          hideElem('distribution_user_row');
          showElem('distribution_options_row');
          removeFromValidate('ConfigureSettings', 'distribution_user_id');
        }
      };
      selectElement.onchange();
      function hideElem(id) {
        if (document.getElementById(id)) {
          document.getElementById(id).style.display = "none";
          document.getElementById(id).value = "";
        }
      }

      function showElem(id) {
        if (document.getElementById(id)) {
          document.getElementById(id).style.display = "";
        }
      }

      function assign_field_change(field) {
        hideElem(field + '[1]');
        hideElem(field + '[2]');

        if (document.getElementById(field + '[0]').value == 'role') {
          showElem(field + '[2]');
        }
        else if (document.getElementById(field + '[0]').value == 'security_group') {
          showElem(field + '[1]');
          showElem(field + '[2]');
        }
      }
      var currentEmailSelect;

      function open_email_template_form(id) {
        currentEmailSelect = id + "_select";
        URL = "index.php?module=EmailTemplates&action=EditView&inboundEmail=1&return_module=AOW_WorkFlow&base_module=AOW_WorkFlow";
        URL += "&show_js=1";

        windowName = 'email_template';
        windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

        win = window.open(URL, windowName, windowFeatures);
        if (window.focus) {
          // put the focus on the popup if the browser supports the focus() method
          win.focus();
        }
      }

      function edit_email_template_form(id) {
        currentEmailSelect = id + "_select";
        var field = document.getElementById(id + "_select");
        URL = "index.php?module=EmailTemplates&action=EditView&inboundEmail=1&return_module=AOW_WorkFlow&base_module=AOW_WorkFlow";
        if (field.options[field.selectedIndex].value != 'undefined') {
          URL += "&record=" + field.options[field.selectedIndex].value;
        }
        URL += "&show_js=1";

        windowName = 'email_template';
        windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

        win = window.open(URL, windowName, windowFeatures);
        if (window.focus) {
          // put the focus on the popup if the browser supports the focus() method
          win.focus();
        }
      }

      function refresh_email_template_list(template_id, template_name) {
        refresh_template_list(template_id, template_name, currentEmailSelect);
      }

      var templateFields = ['user_email_template_id_select', 'contact_email_template_id_select', 'case_creation_email_template_id_select', 'case_closure_email_template_id_select', 'joomla_account_creation_email_template_id_select'];

      function refreshEditVisibility() {
        for (var x = 0; x < templateFields.length; x++) {
          var fieldId = templateFields[x];
          var field = document.getElementById(fieldId);
          field.onchange();
        }
      }

      function refresh_template_list(template_id, template_name, select) {
        var bfound = 0;
        for (var x = 0; x < templateFields.length; x++) {
          var fieldId = templateFields[x];
          var field = document.getElementById(fieldId);
          for (var i = 0; i < field.options.length; i++) {
            if (field.options[i].value == template_id) {
              if (field.options[i].selected == false && fieldId == select) {
                field.options[i].selected = true;
              }
              field.options[i].text = template_name;
              bfound = 1;
            }
          }
          //add item to selection list.
          if (bfound == 0) {
            var newElement = document.createElement('option');
            newElement.text = template_name;
            newElement.value = template_id;
            field.options.add(newElement);
            if (fieldId == select) {
              newElement.selected = true;
            }
          }
        }
        refreshEditVisibility();
      }
      function show_edit_template_link(field) {
        var field1 = document.getElementById(field.name + "_edit_template_link");
        if (field.selectedIndex == 0) {
          field1.style.visibility = "hidden";
        } else {
          field1.style.visibility = "visible";
        }
      }

      refreshEditVisibility();
      $(document).ready(function () {
        $('#addStatusButton').click(function (e) {
          var template = $('#statusRowTemplate').html();
          $(e.target).closest('tr').before(template);
        });
        $(document).on('click', '.removeStatusButton', function (e) {
          $(e.target).closest('tr').remove();
        });
      });
    </script>
<?php
/**
 * @param $mod_strings
 * @param $ifDropdown
 * @param $thenDropdown
 * @return string
 */
function getStatusRowTemplate($mod_strings, $ifDropdown, $thenDropdown)
{
    $html = <<<EOF
    <tr>
        <td  scope="row" width="100">{$mod_strings['LBL_AOP_IF_STATUS']}: </td>
        <td width="100">
            <select id='if_status_select[]' name='if_status[]'>{$ifDropdown}</select>
        </td>
        <td  scope="row" width="100">{$mod_strings['LBL_AOP_THEN_STATUS']}: </td>
        <td width="100">
            <select id='then_status_select[]' name='then_status[]'>{$thenDropdown}</select>
        </td>
        <td><button class="removeStatusButton button" type="button">{$mod_strings['LBL_AOP_REMOVE_STATUS']}</button></td>
    </tr>
EOF;

    return $html;
}
