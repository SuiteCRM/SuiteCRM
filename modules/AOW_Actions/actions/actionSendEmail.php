<?php
/**
 * Advanced OpenWorkflow, Automating SugarCRM.
 * @package Advanced OpenWorkflow for SugarCRM
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility <info@salesagility.com>
 */


require_once __DIR__ . '/../../AOW_Actions/actions/actionBase.php';
require_once __DIR__ . '/../../AOW_WorkFlow/aow_utils.php';
class actionSendEmail extends actionBase
{
    private $emailableModules = array();

    /**
     *
     * @var int
     */
    protected $lastEmailsFailed;

    /**
     *
     * @var int
     */
    protected $lastEmailsSuccess;

    public function __construct($id = '')
    {
        parent::__construct($id);
        $this->clearLastEmailsStatus();
    }

    public function loadJS()
    {
        return array('modules/AOW_Actions/actions/actionSendEmail.js');
    }

    // STIC Custom 20250220 JBL - Avoid Deprecated Warning: Using explicit nullable type
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
    // public function edit_display($line, SugarBean $bean = null, $params = array())
    public function edit_display($line, ?SugarBean $bean = null, $params = array())
    // END STIC Custom
    {
        global $app_list_strings;
        $email_templates_arr = get_bean_select_array(true, 'EmailTemplate', 'name', '', 'name');

        if (!in_array($bean->module_dir, getEmailableModules())) {
            unset($app_list_strings['aow_email_type_list']['Record Email']);
        }
        $targetOptions = getRelatedEmailableFields($bean->module_dir);
        if (empty($targetOptions)) {
            unset($app_list_strings['aow_email_type_list']['Related Field']);
        }

        $html = '<input type="hidden" name="aow_email_type_list" id="aow_email_type_list" value="'.get_select_options_with_id($app_list_strings['aow_email_type_list'], '').'">
				  <input type="hidden" name="aow_email_to_list" id="aow_email_to_list" value="'.get_select_options_with_id($app_list_strings['aow_email_to_list'], '').'">';

        $checked = '';
        if (isset($params['individual_email']) && $params['individual_email']) {
            $checked = 'CHECKED';
        }

        $html .= "<table border='0' cellpadding='0' cellspacing='0' width='100%' data-workflow-action='send-email'>";
        $html .= "<tr>";
        $html .= '<td id="relate_label" scope="row" valign="top"><label>' . translate(
            "LBL_INDIVIDUAL_EMAILS",
            "AOW_Actions"
        ) . ':</label>';
        $html .= '</td>';
        $html .= "<td valign='top'>";
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][individual_email]' value='0' >";
        $html .= "<input type='checkbox' id='aow_actions_param[".$line."][individual_email]' name='aow_actions_param[".$line."][individual_email]' value='1' $checked></td>";
        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        // $html .= '</td>';
        // END STIC-Custom
        if (!isset($params['email_template'])) {
            $params['email_template'] = '';
        }
        $hidden = "style='visibility: hidden;'";
        if ($params['email_template'] != '') {
            $hidden = "";
        }

        $html .= '<td id="name_label" scope="row" valign="top"><label>' . translate(
            "LBL_EMAIL_TEMPLATE",
            "AOW_Actions"
        ) . ':<span class="required">*</span></label></td>';
        $html .= "<td valign='top'>";
        $html .= "<select name='aow_actions_param[".$line."][email_template]' id='aow_actions_param_email_template".$line."' onchange='show_edit_template_link(this,".$line.");' >".get_select_options_with_id($email_templates_arr, $params['email_template'])."</select>";

        $html .= "&nbsp;<a href='javascript:open_email_template_form(".$line.")' >".translate('LBL_CREATE_EMAIL_TEMPLATE', 'AOW_Actions')."</a>";
        $html .= "&nbsp;<span name='edit_template' id='aow_actions_edit_template_link".$line."' $hidden><a href='javascript:edit_email_template_form(".$line.")' >".translate('LBL_EDIT_EMAIL_TEMPLATE', 'AOW_Actions')."</a></span>";
        $html .= "</td>";
        $html .= "</tr>";

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117

        // Advanced options section
        $html .= "<tr style='margin-top:20px;' >";
        $html .= '<td id="relate_label_2" scope="row" valign="top" style="width:20%;" > <a href="javascript:void($(\'.advancedOptions\').toggle());">' . translate(
            "LBL_SHOW_ADVANCED",
            "AOW_Actions"
            ) . '<span class="inline-help glyphicon glyphicon-triangle-bottom"></span> <span id="info-availability" class="inline-help glyphicon glyphicon-info-sign" data-hasqtip="1" aria-describedby="qtip-1"></span></a> ';
        $html .= '</td>'; 
        $html .= "</tr>";
            
        // Show output accounts
        $emailsList = $this->get_output_smtps();
        list($fromName, $fromAddress) = $this->getSelectedSMTPData($emailsList, $params['output_smtp'] ?? '');
        
        $html .= "<tr style='margin-top:20px; margin-bottom:20px; display:none;' class='advancedOptions'>";
        $html .= '<td id="relate_label_5" scope="row" valign="top" style="width:20%;"><label>' . translate(
            "LBL_OUTPUT_SMTP",
            "AOW_Actions"
        ) . ':<span class="required">*</span></label>';
        $html .= '</td>';

        $html .= "<td valign='top' style='width:20%; margin-bottom:20px;'>";
        $html .= "<select name='aow_actions_param[".$line."][output_smtp]' id='aow_actions_param[".$line."][output_smtp]' >" . $this->get_output_smtps_options($emailsList, $params['output_smtp'] ?? '') . "</select>";
        $html .= '</td>';
        $html .= '</tr>';

        // From name
        if (isset($params['from_email_name']) && $params['from_email_name']) {
            $from_name = $params['from_email_name'];
        } else {
            $from_name= $fromName;
        }      

        $html .= "<tr style='margin-top:20px; display:none;' class='advancedOptions'>";
        $html .= '<td id="relate_label_3" scope="row" valign="top" style="width:20%;"><label>' . translate(
            "LBL_FROM_NAME",
            "AOW_Actions"
        ) . ':</label>';
        $html .= '</td>';
        $html .= "<td valign='top' style='width:20% !important;'>";
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][from_email_name]' value='0' >";
        $html .= "<input type='text' id='aow_actions_param[".$line."][from_email_name]' name='aow_actions_param[".$line."][from_email_name]' value='{$from_name}' ></td>";
 
        // From address
        if (isset($params['from_email_address']) && $params['from_email_address']) {
            $from = $params['from_email_address'];
        } else {
            $from= $fromAddress;
        }

        $html .= '<td id="relate_label_2" scope="row" valign="top" style="width:20%;"> <label>' . translate(
            "LBL_FROM_EMAIL",
            "AOW_Actions"
        ) . ':</label>';
        $html .= '</td>';
        $html .= "<td valign='top' style='width:20% !important;'>";
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][from_email_address]' value='' >";
        $html .= "<input type='text' id='aow_actions_param[".$line."][from_email_address]' name='aow_actions_param[".$line."][from_email_address]' value='{$from}' ></td>";
        $html .= "</tr>";

        // Reply to name
        if (isset($params['reply_to_name']) && $params['reply_to_name']) {
            $reply_to_name = $params['reply_to_name'];

        } else {
            $reply_to_name = '';
        }  
        $html .= "<tr style='margin-top:20px; margin-bottom:20px; display:none' class='advancedOptions'>";
        $html .= '<td id="relate_label_4" scope="row" valign="top" style="width:20%;"><label>' . translate(
            "LBL_REPLY_TO_NAME",
            "AOW_Actions"
        ) . ':</label>';
        $html .= '</td>';
        $html .= "<td valign='top' style='width:20% !important;'>";
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][reply_to_name]' value='0' >";
        $html .= "<input type='text' id='aow_actions_param[".$line."][reply_to_name'] name='aow_actions_param[".$line."][reply_to_name]' value='{$reply_to_name}' ></td>";
        $html .= '</td>';

        // Reply to address
        if (isset($params['reply_to']) && $params['reply_to']) {
            $reply_to = $params['reply_to'];

        } else {
            $reply_to='';
        }  
        $html .= '<td id="relate_label_4" scope="row" valign="top" style="width:20%;"><label>' . translate(
            "LBL_REPLY_TO_EMAIL",
            "AOW_Actions"
        ) . ':</label>';
        $html .= '</td>';
        $html .= "<td valign='top' style='width:20% !important;'>";
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][reply_to]' value='0' >";
        $html .= "<input type='text' id='aow_actions_param[".$line."][reply_to'] name='aow_actions_param[".$line."][reply_to]' value='{$reply_to}' ></td>";
        $html .= '</td>';
        $html .= "</tr>";

        // Section end
        $html .= "<tr style='margin-top:20px;' >";
        $html .= "<td valign='top' style='width:20% !important;'>";
        $html .= '</td>';
        $html .= '</tr>';

        // END STIC-Custom

        $html .= "<tr>";
        $html .= '<td id="name_label" scope="row" valign="top"><label>' . translate(
            "LBL_EMAIL",
            "AOW_Actions"
        ) . ':<span class="required">*</span></label></td>';
        $html .= '<td valign="top" scope="row">';

        $html .='<button type="button" onclick="add_emailLine('.$line.')"><img src="'.SugarThemeRegistry::current()->getImageURL('id-ff-add.png').'"></button>';
        $html .= '<table id="emailLine'.$line.'_table" width="100%" class="email-line"></table>';
        $html .= '</td>';
        $html .= "</tr>";
        $html .= "</table>";

        $html .= "<script id ='aow_script".$line."'>";

        //backward compatible
        if (isset($params['email_target_type']) && !is_array($params['email_target_type'])) {
            $email = '';
            switch ($params['email_target_type']) {
                case 'Email Address':
                    $email = $params['email'];
                    break;
                case 'Specify User':
                    $email = $params['email_user_id'];
                    break;
                case 'Related Field':
                    $email = $params['email_target'];
                    break;
            }
            $html .= "load_emailline('".$line."','to','".$params['email_target_type']."','".$email."');";
        }
        //end backward compatible

        if (isset($params['email_target_type'])) {
            foreach ($params['email_target_type'] as $key => $field) {
                if (is_array($params['email'][$key])) {
                    $params['email'][$key] = json_encode($params['email'][$key]);
                }
                $html .= "load_emailline('".$line."','".$params['email_to_type'][$key]."','".$params['email_target_type'][$key]."','".$params['email'][$key]."');";
            }
        }
        $html .= "</script>";

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        
        $html .= "<script id ='info'> 
        // script for tooltip
        $('#info-availability').qtip({
            content: {
              text: '". translate(
                "LBL_ADVANCED_TOOLTIP_BODY",
                "AOW_Actions"
            )."',
              title: {
                text: '". translate(
                    "LBL_ADVANCED_TOOLTIP_HEADER",
                    "AOW_Actions"
                ) ."',
              },
              style: {
                classes: 'qtip-inline-help'
              }
            },
        });

        // script for outbound mail change
        var selection = document.getElementById('aow_actions_param[".$line."][output_smtp]');
        selection.onchange = function(event){
          let from = event.target.options[event.target.selectedIndex].dataset.from;
          let address = event.target.options[event.target.selectedIndex].dataset.address;
          document.getElementById('aow_actions_param[" . $line . "][from_email_address]').setAttribute('value', address);
          document.getElementById('aow_actions_param[" . $line . "][from_email_name]').setAttribute('value', from);
        };";

        // Hide advanced section when default configuration is used
        if (isset($params['output_smtp']) && ($params['output_smtp'] != 'system' || $fromName != $from_name || $fromAddress != $from
                || !empty($params['reply_to']) || !empty($params['reply_to_name']))) {
            $html .= "$('.advancedOptions').toggle();";
        }

        $html .= "</script>";
        //END STIC-Custom

        return $html;
    }

    // STIC-Custom 20240307 EPS - Improve send mail action
    // https://github.com/SinergiaTIC/SinergiaCRM/issues/117

    private function getSelectedSMTPData($emailsList, $selectedSmtp) {
        $selectedData = array();
        foreach($emailsList as $id => $props) {
            if ($selectedSmtp == $props['name']) {
                $selectedData = array(
                    $props['smtp_from_name'],
                    $props['smtp_from_addr'],
                );
                return $selectedData;
            }
        }
        $selectedData = array(null, null);
        return $selectedData;
    }
    private function get_output_smtps_options($emailsList, $selectedSmtp) {
        $selectedSmtp = $selectedSmtp == '' ? 'system' : $selectedSmtp;
        $optionString = "";
        foreach($emailsList as $id => $props) {
            $selected = $props['name'] == $selectedSmtp ? 'selected' : '';
            $optionString .= "<option value='{$props['name']}' {$selected} data-from='{$props['smtp_from_name']}' data-address='{$props['smtp_from_addr']}'>{$props['name']}</option> ";
        }

        return $optionString;
    }

    private function get_output_smtps() {
        $emailsList = array();
        $oeaList = BeanFactory::getBean('OutboundEmailAccounts')->get_full_list('', "(type = 'system' OR type = 'group')"); // Personal and overrides are ignored, as this will be a system based email
        foreach ($oeaList as $oea) {
            $emailsList[$oea->id] = array(
                'name' => $oea->name,
                'smtp_from_name' => $oea->smtp_from_name,
                'smtp_from_addr' => $oea->smtp_from_addr
            );
        }
        return $emailsList;
    }
    // END STIC-Custom

    protected function getEmailsFromParams(SugarBean $bean, $params)
    {
        $emails = array();
        //backward compatible
        if (isset($params['email_target_type']) && !is_array($params['email_target_type'])) {
            $email = '';
            switch ($params['email_target_type']) {
                case 'Email Address':
                    $params['email'] = array($params['email']);
                    break;
                case 'Specify User':
                    $params['email'] = array($params['email_user_id']);
                    break;
                case 'Related Field':
                    $params['email'] = array($params['email_target']);
                    break;
            }
            $params['email_target_type'] = array($params['email_target_type']);
            $params['email_to_type'] = array('to');
        }
        //end backward compatible
        if (isset($params['email_target_type'])) {
            foreach ($params['email_target_type'] as $key => $field) {
                switch ($field) {
                    case 'Email Address':
                        if (trim($params['email'][$key]) != '') {
                            $emails[$params['email_to_type'][$key]][] = $params['email'][$key];
                        }
                        break;
                    case 'Specify User':
                        $user = BeanFactory::newBean('Users');
                        $user->retrieve($params['email'][$key]);
                        $user_email = $user->emailAddress->getPrimaryAddress($user);
                        if (trim($user_email) != '') {
                            $emails[$params['email_to_type'][$key]][] = $user_email;
                            $emails['template_override'][$user_email] = array('Users' => $user->id);
                        }

                        break;
                    case 'Users':
                        $users = array();
                        switch ($params['email'][$key][0]) {
                            case 'security_group':
                                if (file_exists('modules/SecurityGroups/SecurityGroup.php')) {
                                    require_once('modules/SecurityGroups/SecurityGroup.php');
                                    $security_group = BeanFactory::newBean('SecurityGroups');
                                    $security_group->retrieve($params['email'][$key][1]);
                                    $users = $security_group->get_linked_beans('users', 'User');
                                    $r_users = array();
                                    if ($params['email'][$key][2] != '') {
                                        require_once('modules/ACLRoles/ACLRole.php');
                                        $role = BeanFactory::newBean('ACLRoles');
                                        $role->retrieve($params['email'][$key][2]);
                                        $role_users = $role->get_linked_beans('users', 'User');
                                        foreach ($role_users as $role_user) {
                                            $r_users[$role_user->id] = $role_user->name;
                                        }
                                    }
                                    foreach ($users as $user_id => $user) {
                                        if ($params['email'][$key][2] != '' && !isset($r_users[$user->id])) {
                                            unset($users[$user_id]);
                                        }
                                    }
                                    break;
                                }
                            //No Security Group module found - fall through.
                            // no break
                            case 'role':
                                require_once('modules/ACLRoles/ACLRole.php');
                                $role = BeanFactory::newBean('ACLRoles');
                                $role->retrieve($params['email'][$key][2]);
                                $users = $role->get_linked_beans('users', 'User');
                                break;
                            case 'all':
                            default:
                                $db = DBManagerFactory::getInstance();
                                $sql = "SELECT id from users WHERE status='Active' AND portal_only=0 ";
                                $result = $db->query($sql);
                                while ($row = $db->fetchByAssoc($result)) {
                                    $user = BeanFactory::newBean('Users');
                                    $user->retrieve($row['id']);
                                    $users[$user->id] = $user;
                                }
                                break;
                        }
                        foreach ($users as $user) {
                            $user_email = $user->emailAddress->getPrimaryAddress($user);
                            if (trim($user_email) != '') {
                                $emails[$params['email_to_type'][$key]][] = $user_email;
                                $emails['template_override'][$user_email] = array('Users' => $user->id);
                            }
                        }
                        break;
                    case 'Related Field':
                        $emailTarget = $params['email'][$key];
                        $relatedFields = array_merge($bean->get_related_fields(), $bean->get_linked_fields());
                        $field = $relatedFields[$emailTarget];
                        if ($field['type'] == 'relate') {
                            $linkedBeans = array();
                            $idName = $field['id_name'];
                            $id = $bean->$idName;
                            $linkedBeans[] = BeanFactory::getBean($field['module'], $id);
                        } else {
                            if ($field['type'] == 'link') {
                                $relField = $field['name'];
                                if (isset($field['module']) && $field['module'] != '') {
                                    $rel_module = $field['module'];
                                } else {
                                    if ($bean->load_relationship($relField)) {
                                        $rel_module = $bean->$relField->getRelatedModuleName();
                                    }
                                }
                                $linkedBeans = $bean->get_linked_beans($relField, $rel_module);
                            } else {
                                $linkedBeans = $bean->get_linked_beans($field['link'], $field['module']);
                            }
                        }
                        if ($linkedBeans) {
                            foreach ($linkedBeans as $linkedBean) {
                                if (!empty($linkedBean)) {
                                    $rel_email = $linkedBean->emailAddress->getPrimaryAddress($linkedBean);
                                    if (trim($rel_email) != '') {
                                        $emails[$params['email_to_type'][$key]][] = $rel_email;
                                        $emails['template_override'][$rel_email] = array($linkedBean->module_dir => $linkedBean->id);
                                    }
                                }
                            }
                        }
                        break;
                    case 'Record Email':
                        $recordEmail = $bean->emailAddress->getPrimaryAddress($bean);
                        if ($recordEmail == '' && isset($bean->email1)) {
                            $recordEmail = $bean->email1;
                        }
                        if (trim($recordEmail) != '') {
                            $emails[$params['email_to_type'][$key]][] = $recordEmail;
                        }
                        break;
                }
            }
        }
        return $emails;
    }

    /**
     * Return true on success otherwise false.
     * Use actionSendEmail::getLastEmailsSuccess() and actionSendEmail::getLastEmailsFailed()
     * methods to get last email sending status
     *
     * @param SugarBean $bean
     * @param array $params
     * @param bool $in_save
     * @return boolean
     */
    public function run_action(SugarBean $bean, $params = array(), $in_save = false)
    {
        include_once __DIR__ . '/../../EmailTemplates/EmailTemplate.php';

        $this->clearLastEmailsStatus();

        $emailTemp = BeanFactory::newBean('EmailTemplates');
        $emailTemp->retrieve($params['email_template']);

        if ($emailTemp->id == '') {
            return false;
        }

        $emails = $this->getEmailsFromParams($bean, $params);

        if (!isset($emails['to']) || empty($emails['to'])) {
            return false;
        }

        $attachments = $this->getAttachments($emailTemp);

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        $fromEmail = $params['from_email_address'] ?? '';
        $fromName = $params['from_email_name'] ?? '';
        $replyto = $params['reply_to'] ?? '';
        $replytoName = $params['reply_to_name'] ?? '';
        $outputSmtp = $params['output_smtp'] ?? '';
        // END STIC-Custom

        $ret = true;

        $emailCC = $emails['cc'] ?? '';
        $emailBCC = $emails['bcc'] ?? '';

        if (isset($params['individual_email']) && $params['individual_email']) {
            foreach ($emails['to'] as $email_to) {
                $emailTemp = BeanFactory::newBean('EmailTemplates');
                $emailTemp->retrieve($params['email_template']);
                $template_override = $emails['template_override'][$email_to] ?? array();
                $this->parse_template($bean, $emailTemp, $template_override);
                // STIC-Custom 20240307 EPS - Improve send mail action
                // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
                // if (!$this->sendEmail(array($email_to), $emailTemp->subject, $emailTemp->body_html, $emailTemp->body, $bean, $emailCC, $emailBCC, $attachments)) {
                if (!$this->sendEmail($emailTemp, array($email_to), $outputSmtp, $fromEmail, $fromName, $replyto, $replytoName, $bean, $emailCC, $emailBCC, $attachments)) {
                // END STIC-Custom
                    $ret = false;
                    $this->lastEmailsFailed++;
                } else {
                    $this->lastEmailsSuccess++;
                }
            }
        } else {
            $this->parse_template($bean, $emailTemp);
            if ($emailTemp->text_only == '1') {
                $email_body = $emailTemp->body;
                $email_body_alt = null;
            } else {
                $email_body = $emailTemp->body_html;
                $email_body_alt = $emailTemp->body;
            }

            // STIC-Custom 20240307 EPS - Improve send mail action
            // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
            // if (!$this->sendEmail($emails['to'], $emailTemp->subject, $email_body, $email_body_alt, $bean, $emailCC, $emailBCC, $attachments)) {
            if (!$this->sendEmail($emailTemp, $emails['to'], $outputSmtp, $fromEmail, $fromName, $replyto, $replytoName, $bean, $emailCC, $emailBCC, $attachments)) {
            // END STIC-Custom
                $ret = false;
                $this->lastEmailsFailed++;
            } else {
                $this->lastEmailsSuccess++;
            }
        }
        return $ret;
    }

    /**
     *  clear last email sending status
     */
    protected function clearLastEmailsStatus()
    {
        $this->lastEmailsFailed = 0;
        $this->lastEmailsSuccess = 0;
    }

    /**
     * failed emails count at last run_action
     * @return int
     */
    public function getLastEmailsFailed()
    {
        return $this->lastEmailsFailed;
    }

    /**
     * successfully sent emails count at last run_action
     * @return type
     */
    public function getLastEmailsSuccess()
    {
        return $this->lastEmailsSuccess;
    }

    public function parse_template(SugarBean $bean, &$template, $object_override = array())
    {

        global $sugar_config;

        require_once __DIR__ . '/templateParser.php';

        $object_arr = [];
        $object_arr[$bean->module_dir] = $bean->id;

        foreach ($bean->field_defs as $bean_arr) {
            if ($bean_arr['type'] == 'relate') {
                if (isset($bean_arr['module']) &&  $bean_arr['module'] != '' && isset($bean_arr['id_name']) &&  $bean_arr['id_name'] != '' && $bean_arr['module'] != 'EmailAddress') {
                    $idName = $bean_arr['id_name'];
                    if (isset($bean->field_defs[$idName]) && ($bean->field_defs[$idName]['source'] ?? '') != 'non-db') {
                        if (!isset($object_arr[$bean_arr['module']])) {
                            // STIC Custom 20250312 JBL - Avoid Warning: Undefined property
                            // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
                            // $object_arr[$bean_arr['module']] = $bean->$idName;
                            $object_arr[$bean_arr['module']] = $bean->$idName ?? '';
                            // END STIC Custom
                        }
                    }
                }
            } else {
                if ($bean_arr['type'] == 'link') {
                    if (!isset($bean_arr['module']) || $bean_arr['module'] == '') {
                        $bean_arr['module'] = getRelatedModule($bean->module_dir, $bean_arr['name']);
                    }
                    if (isset($bean_arr['module']) &&  $bean_arr['module'] != ''&& !isset($object_arr[$bean_arr['module']])&& $bean_arr['module'] != 'EmailAddress') {
                        $linkedBeans = $bean->get_linked_beans($bean_arr['name'], $bean_arr['module'], array(), 0, 1);
                        if ($linkedBeans) {
                            $linkedBean = $linkedBeans[0];
                            if (!isset($object_arr[$linkedBean->module_dir])) {
                                $object_arr[$linkedBean->module_dir] = $linkedBean->id;
                            }
                        }
                    }
                }
            }
        }

        $object_arr['Users'] = is_a($bean, 'User') ? $bean->id : $bean->assigned_user_id;

        $object_arr = array_merge($object_arr, $object_override);

        $parsedSiteUrl = parse_url((string) $sugar_config['site_url']);
        $host = $parsedSiteUrl['host'];
        if (!isset($parsedSiteUrl['port'])) {
            $parsedSiteUrl['port'] = 80;
        }

        $port		= ($parsedSiteUrl['port'] != 80) ? ":".$parsedSiteUrl['port'] : '';
        $path		= !empty($parsedSiteUrl['path']) ? $parsedSiteUrl['path'] : "";
        $cleanUrl	= "{$parsedSiteUrl['scheme']}://{$host}{$port}{$path}";

        $url =  $cleanUrl."/index.php?module={$bean->module_dir}&action=DetailView&record={$bean->id}";

        $template->subject = str_replace("\$contact_user", "\$user", (string) $template->subject);
        $template->body_html = str_replace("\$contact_user", "\$user", (string) $template->body_html);
        $template->body = str_replace("\$contact_user", "\$user", (string) $template->body);
        $template->subject = aowTemplateParser::parse_template($template->subject, $object_arr);
        $template->body_html = aowTemplateParser::parse_template($template->body_html, $object_arr);
        $template->body_html = str_replace("\$url", $url, (string) $template->body_html);
        $template->body_html = str_replace('$sugarurl', $sugar_config['site_url'], $template->body_html);
        $template->body = aowTemplateParser::parse_template($template->body, $object_arr);
        $template->body = str_replace("\$url", $url, (string) $template->body);
        $template->body = str_replace('$sugarurl', $sugar_config['site_url'], $template->body);
    }

    public function getAttachments(EmailTemplate $template)
    {
        $attachments = array();
        if ($template->id != '') {
            $note_bean = BeanFactory::newBean('Notes');
            $notes = $note_bean->get_full_list('', "parent_type = 'Emails' AND parent_id = '".$template->id."'");

            if ($notes != null) {
                foreach ($notes as $note) {
                    $attachments[] = $note;
                }
            }
        }
        return $attachments;
    }
    // STIC-Custom 20240307 EPS - Improve send mail action
    // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
    // public function sendEmail($emailTo, $emailSubject, $emailBody, $altemailBody, SugarBean $relatedBean = null, $emailCc = array(), $emailBcc = array(), $attachments = array())
    // STIC Custom 20250220 JBL - Avoid Deprecated Warning: Using explicit nullable type
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
    // public function sendEmail($templateData, $emailTo, $mailerName = 'system', $fromEmail = '', $fromName = '', $replyto = '', $replytoName = '', SugarBean $relatedBean = null, $emailCc = array(), $emailBcc = array(), $attachments = array())
    public function sendEmail($templateData, $emailTo, $mailerName = 'system', $fromEmail = '', $fromName = '', $replyto = '', $replytoName = '', ?SugarBean $relatedBean = null, $emailCc = array(), $emailBcc = array(), $attachments = array())
    // END STIC Custom JBL
    // END STIC-Custom
    {
        require_once('modules/Emails/Email.php');
        require_once('include/SugarPHPMailer.php');

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        if ((empty($emailTo)) || (!is_array($emailTo))) {
            return false;
        }
        // END STIC-Custom

        $emailObj = BeanFactory::newBean('Emails');
        $mail = new SugarPHPMailer();

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        // $defaults = $emailObj->getSystemDefaultEmail();
        // $mail->setMailerForSystem();
        // $mail->From = $defaults['email'];
        // isValidEmailAddress($mail->From);
        // $mail->FromName = $defaults['name'];
        
        
        $outboundEmail = new OutboundEmail();
        if (empty($mailerName) || $mailerName === 'system') {
            $outboundEmail = $outboundEmail->getSystemMailerSettings();
        }
        else {
            $user = new User();
            $user->getSystemUser();
            $outboundEmail = $outboundEmail->getMailerByName($user, $mailerName);
        }
        $mail->From = $fromEmail? $fromEmail : $outboundEmail->smtp_from_addr;
        $mail->FromName = $fromName ? $fromName : $outboundEmail->smtp_from_name;
        $mail->ClearCustomHeaders();

        $mail->Mailer = 'smtp';
        $mail->Host = $outboundEmail->mail_smtpserver;
        $mail->Port = $outboundEmail->mail_smtpport;

        if ($outboundEmail->mail_smtpssl == 1) {
            $mail->SMTPSecure = 'ssl';
        } // if
        if ($outboundEmail->mail_smtpssl == 2) {
            $mail->SMTPSecure = 'tls';
        } // if
        if ($outboundEmail->mail_smtpauth_req) {
            $mail->SMTPAuth = true;
            $mail->Username = $outboundEmail->mail_smtpuser;
            $mail->Password = $outboundEmail->mail_smtppass;
        }
        // END STIC-Custom 

        $mail->ClearAllRecipients();
        $mail->ClearReplyTos();

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        // $mail->Subject=from_html($emailSubject);
        // $mail->Body=$emailBody;
        // if($altemailBody){
        //     $mail->AltBody = $altemailBody;
        // }
        $mail->Subject = $templateData->subject;
        if ($templateData->text_only == '1') {
            $mail->Body = $templateData->body;
        } else {
            $mail->Body = $templateData->body_html;
            $mail->isHTML(true);
        }
        $mail->AltBody = $templateData->body;
        // END STIC-Custom


        $mail->handleAttachments($attachments);
        $mail->prepForOutbound();

        foreach ($emailTo as $to) {
            $mail->AddAddress($to);
        }

        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        if (!empty($replyto)){
            $mail->addReplyTo($replyto, $replytoName);
        }
        // END STIC-Custom
        if (!empty($emailCc)) {
            foreach ($emailCc as $email) {
                $mail->AddCC($email);
            }
        }
        if (!empty($emailBcc)) {
            foreach ($emailBcc as $email) {
                $mail->AddBCC($email);
            }
        }
        if (!is_array($emailCc)) {
            $emailCc = [];
        }

        if (!is_array($emailBcc)) {
            $emailBcc = [];
        }

        //now create email
        if ($mail->Send()) {
            $emailObj->to_addrs= implode(',', $emailTo);
            // STIC Custom 20230511 - JBL - Reducing use of deprecated code and warnings
            // STIC#1066
            //  - PHP Warning:  implode(): Invalid arguments passed
            // $emailObj->cc_addrs= implode(',', $emailCc);
            // $emailObj->bcc_addrs= implode(',', $emailBcc);
            $emailObj->cc_addrs= is_array($emailCc)? implode(',', $emailCc) : null;
            $emailObj->bcc_addrs= is_array($emailBcc)? implode(',', $emailBcc): null;
            // End STIC Custom
            $emailObj->type= 'out';
            $emailObj->deleted = '0';
            $emailObj->name = $mail->Subject;
            $emailObj->description = $mail->AltBody;
            $emailObj->description_html = $mail->Body;
            $emailObj->from_addr = $mail->From;
            isValidEmailAddress($emailObj->from_addr);
            if ($relatedBean instanceof SugarBean && !empty($relatedBean->id)) {
                $emailObj->parent_type = $relatedBean->module_dir;
                $emailObj->parent_id = $relatedBean->id;
            }
            $emailObj->date_sent_received = TimeDate::getInstance()->nowDb();
            $emailObj->modified_user_id = '1';
            $emailObj->created_by = '1';
            $emailObj->status = 'sent';
            $emailObj->save();

            // Fix for issue 1561 - Email Attachments Sent By Workflow Do Not Show In Related Activity.
            foreach ($attachments as $attachment) {
                $note = BeanFactory::newBean('Notes');
                $note->id = create_guid();
                $note->date_entered = $attachment->date_entered;
                $note->date_modified = $attachment->date_modified;
                $note->modified_user_id = $attachment->modified_user_id;
                $note->assigned_user_id = $attachment->assigned_user_id;
                $note->new_with_id = true;
                $note->parent_id = $emailObj->id;
                $note->parent_type = $attachment->parent_type;
                $note->name = $attachment->name;
                ;
                $note->filename = $attachment->filename;
                $note->file_mime_type = $attachment->file_mime_type;
                $fileLocation = "upload://{$attachment->id}";
                $dest = "upload://{$note->id}";
                if (!copy($fileLocation, $dest)) {
                    $GLOBALS['log']->debug("EMAIL 2.0: could not copy attachment file to $fileLocation => $dest");
                }
                $note->save();
            }
        // STIC-Custom 20240307 EPS - Improve send mail action
        // https://github.com/SinergiaTIC/SinergiaCRM/issues/117
        //     return true;
        // }
        // return false;
        }
        else {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error send the notification email.");
            return false;
        }
        return true;
    }
    // END STIC-Custom
}