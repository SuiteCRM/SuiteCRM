<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */



require_once 'modules/AOW_Actions/actions/actionBase.php';
require_once 'modules/AOW_WorkFlow/aow_utils.php';
require_once 'modules/stic_Messages/Utils.php';

class actionSendMessage extends actionBase
{
    public function __construct($id = '')
    {
        parent::__construct($id);
    }

    public function loadJS()
    {
        return array('modules/AOW_Actions/actions/actionSendMessage.js');
    }

    public function edit_display($line, ?SugarBean $bean = null, $params = array())
    {

        // Get default values for combos
        $messagesBean = BeanFactory::newBean('stic_Messages');

        $defaultType = $messagesBean->field_defs['type']['default'] ?? '';
        $defaultDirection = $messagesBean->field_defs['direction']['default'] ?? '';
        $defaultStatus = $messagesBean->field_defs['status']['default'] ?? '';

        // Get all email templates for SMS and WhatsApp types
        global $app_list_strings;
        
        // Query templates directly to get type info (avoiding cache issues)
        require_once 'modules/EmailTemplates/EmailTemplate.php';
        $emailTemplateBean = new EmailTemplate();
        
        $db = DBManagerFactory::getInstance();
        $sql = "SELECT {$emailTemplateBean->table_name}.id, {$emailTemplateBean->table_name}.name, {$emailTemplateBean->table_name}.type ";
        $sql .= "FROM {$emailTemplateBean->table_name} ";
        $sql .= "WHERE deleted = 0 AND (type = 'sms' OR type = 'whatsapp') ";
        
        $accessWhere = $emailTemplateBean->buildAccessWhere('list');
        if (!empty($accessWhere)) {
            $sql .= ' AND ' . $accessWhere;
        }
        
        $sql .= " ORDER BY name";
        
        $GLOBALS['log']->debug('actionSendMessage: email templates query: ' . $sql);
        $result = $db->query($sql, true);
        
        $smsTemplates = array();
        $whatsappTemplates = array();
        
        while ($row = $db->fetchByAssoc($result)) {
            if ($row['type'] === 'sms') {
                $smsTemplates[$row['id']] = $row['name'];
            } elseif ($row['type'] === 'whatsapp') {
                $whatsappTemplates[$row['id']] = $row['name'];
            }
        }
        
        // Merge all templates for the select dropdown
        $email_templates_arr = array_merge($smsTemplates, $whatsappTemplates);
        unset($email_templates_arr['']);
        asort($email_templates_arr);
        
        // Pass templates by type to JavaScript for dynamic filtering
        $smsTemplatesJson = json_encode($smsTemplates);
        $whatsappTemplatesJson = json_encode($whatsappTemplates);

        // If the bean has no phone record, the option is removed from list
        if (!in_array($bean->module_dir, stic_MessagesUtils::getMessageableModules())) {
            unset($app_list_strings['aow_message_type_list']['Record Phone']);
        }
        // If the bean has no valid related records, the option is removed from list
        $targetOptions = stic_MessagesUtils::getRelatedMessageableFields($bean->module_dir);
        if (empty($targetOptions)) {
            unset($app_list_strings['aow_message_type_list']['Related Field']);
        }

        // Ensure AOW action does not allow WhatsAppWeb selection: remove from stic_messages_type_list
        if (isset($app_list_strings['stic_messages_type_list']['whatsapp_web'])) {
            unset($app_list_strings['stic_messages_type_list']['whatsapp_web']);
        }

        // Error status not allowed on message creation
        unset($app_list_strings['stic_messages_status_list']['error']);

        $html = '<input type="hidden" name="aow_message_type_list" id="aow_message_type_list" value="'.get_select_options_with_id($app_list_strings['aow_message_type_list'], '').'">';
        
        // Hidden fields with templates by type for JavaScript filtering
        $html .= '<input type="hidden" id="aow_sms_templates" value="'.htmlspecialchars($smsTemplatesJson, ENT_QUOTES, 'UTF-8').'">';
        $html .= '<input type="hidden" id="aow_whatsapp_templates" value="'.htmlspecialchars($whatsappTemplatesJson, ENT_QUOTES, 'UTF-8').'">';
        $html .= '<input type="hidden" id="aow_none_label" value="'.htmlspecialchars(translate('LBL_NONE', 'app_strings'), ENT_QUOTES, 'UTF-8').'">';

        $html .= "<table border='0' cellpadding='0' cellspacing='0' width='100%' data-workflow-action='send-message'>";
        $html .= "<tr>";
  
        if (!isset($params['email_template'])) {
            $params['email_template'] = '';
        }
        $hidden = "style='visibility: hidden;'";
        if ($params['email_template'] != '') {
            $hidden = "";
        }

        $html .= '<td id="name_label" scope="row" valign="top"><label>' . translate(
            "LBL_TEMPLATE",
            "stic_Messages"
        ) . ':<span class="required">*</span></label></td>';
        $html .= "<td valign='top'>";
        $html .= "<select name='aow_actions_param[".$line."][email_template]' id='aow_actions_param_email_template".$line."' onchange='show_edit_template_link(this,".$line.");' >".get_select_options_with_id($email_templates_arr, $params['email_template'])."</select>";

        $html .= "&nbsp;<a href='javascript:open_email_template_form(".$line.")' >".translate('LBL_CREATE_EMAIL_TEMPLATE', 'AOW_Actions')."</a>";
        $html .= "&nbsp;<span name='edit_template' id='aow_actions_edit_template_link".$line."' $hidden><a href='javascript:edit_email_template_form(".$line.")' >".translate('LBL_EDIT_EMAIL_TEMPLATE', 'AOW_Actions')."</a></span>";
        $html .= "</td>";
        
        // Sender name
        if (isset($params['sender_name']) && $params['sender_name']) {
            $from_name = $params['sender_name'];
        } else {
            require_once('modules/stic_Settings/Utils.php');
            $from_name = stic_SettingsUtils::getSetting('MESSAGES_SENDER');
        }      

        $html .= '<td id="relate_label_3" scope="row" valign="top" style="width:20%;"><label>' . translate(
            "LBL_SENDER",
            "stic_Messages"
        ) . ':</label>';
        $html .= '</td>';
        $html .= "<td valign='top' style='width:20% !important;'>";
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][sender_name]' value='0' >";
        $html .= "<input type='text' id='aow_actions_param[".$line."][sender_name]' name='aow_actions_param[".$line."][sender_name]' value='{$from_name}' ></td>";
        $html .= "</tr>";


        // Type & direction
        $html .= "<tr style='margin-top:20px; margin-bottom:20px;'>";
        $html .= '<td id="relate_label_5" scope="row" valign="top" style="width:10%;"><label>' . translate(
            "LBL_TYPE",
            "stic_Messages"
        ) . ':<span class="required">*</span></label>';
        $html .= '</td>';

        $html .= "<td valign='top' style='width:20%; margin-bottom:20px;'>";
        $selectedType = $params['type'] ?? $defaultType;
        $html .= "<select name='aow_actions_param[".$line."][type]' id='aow_actions_param_type_".$line."' >" . get_select_options_with_id($app_list_strings['stic_messages_type_list'], $selectedType) . "</select>";
        $html .= '</td>';

        // Direction field hidden until other type of messages included
        // $html .= '<td id="relate_label_5" scope="row" valign="top" style="width:20%;"><label>' . translate(
        //     "LBL_DIRECTION",
        //     "stic_Messages"
        // ) . ':<span class="required">*</span></label>';
        // $html .= '</td>';

        // $html .= "<td valign='top' style='width:20%; margin-bottom:20px;'>";
        // $html .= "<select name='aow_actions_param[".$line."][direction]' id='aow_actions_param[".$line."][direction]' >" . get_select_options_with_id($app_list_strings['stic_messages_direction_list'], $defaultDirection) . "</select>";
        // $html .= '</td>';
        $html .= "<td style='width:20%; margin-bottom:20px;'>";
        $html .= '</td>';
        $html .= "<td style='width:20%; margin-bottom:20px;'>";
        $html .= '</td>';

        $html .= '</tr>';

        // Status
        $html .= "<tr style='margin-top:20px; margin-bottom:20px;'>";
        $html .= '<td id="relate_label_5" scope="row" valign="top" style="width:10%;"><label>' . translate(
            "LBL_STATUS",
            "stic_Messages"
        ) . ':<span class="required">*</span></label>';
        $html .= '</td>';

        $html .= "<td valign='top' style='width:20%; margin-bottom:20px;'>";
        $html .= "<select name='aow_actions_param[".$line."][status]' id='aow_actions_param[".$line."][status]' >" . get_select_options_with_id($app_list_strings['stic_messages_status_list'], $defaultStatus) . "</select>";
        $html .= '</td>';
        $html .= '</tr>';

        // Phone: Different parts of the phone field, will be loaded on recipient type selection
        $html .= "<tr>";
        $html .= '<td id="name_label" scope="row" valign="top"><label>' . translate(
            "LBL_PHONE",
            "stic_Messages"
        ) . ':<span class="required">*</span></label></td>';
        $html .= '<td valign="top" scope="row">';

        $html .='<button type="button" onclick="add_phoneLine('.$line.')"><img src="'.SugarThemeRegistry::current()->getImageURL('id-ff-add.png').'"></button>';
        $html .= '<table id="phoneLine'.$line.'_table" width="100%" class="phone-line"></table>';
        $html .= '</td>';
        $html .= "</tr>";
        $html .= "</table>";

        $html .= "<script id ='aow_script".$line."'>";

        if (isset($params['phone_target_type'])) {
            foreach ($params['phone_target_type'] as $key => $field) {
                if (is_array($params['phone'][$key])) {
                    $params['phone'][$key] = json_encode($params['phone'][$key]);
                }
                $html .= "load_phoneline('".$line."','".$params['phone_target_type'][$key]."','".$params['phone'][$key]."');";
            }
        }
        // Initialize template filter for this line
        $html .= "initTemplateFilter(".$line.");";
        $html .= "</script>";

        return $html;
    }

    protected function getPhonesFromParams(SugarBean $bean, $params)
    {
        $recipients = array();
        
        if (isset($params['phone_target_type'])) {
            foreach ($params['phone_target_type'] as $key => $field) {
                switch ($field) {
                    case 'Phone':
                        if (trim($params['phone'][$key]) != '') {
                            $recipients[]['phone'] = $params['phone'][$key];
                        }
                        break;
                    case 'Specify User':
                        $user = BeanFactory::newBean('Users');
                        $user->retrieve($params['phone'][$key]);
                        $user_phone = $user->phone_mobile;
                        if (trim($user_phone) != '') {
                            $recipients[] = array(
                                'phone' => $user_phone,
                                'parent_type' => $user->module_name,
                                'parent_id' => $user->id,
                            );
                        }

                        break;
                    case 'Users':
                        $users = array();
                        switch ($params['phone'][$key][0]) {
                            case 'security_group':
                                if (file_exists('modules/SecurityGroups/SecurityGroup.php')) {
                                    require_once('modules/SecurityGroups/SecurityGroup.php');
                                    $security_group = BeanFactory::newBean('SecurityGroups');
                                    $security_group->retrieve($params['phone'][$key][1]);
                                    $users = $security_group->get_linked_beans('users', 'User');
                                    $r_users = array();
                                    if ($params['phone'][$key][2] != '') {
                                        require_once('modules/ACLRoles/ACLRole.php');
                                        $role = BeanFactory::newBean('ACLRoles');
                                        $role->retrieve($params['phone'][$key][2]);
                                        $role_users = $role->get_linked_beans('users', 'User');
                                        foreach ($role_users as $role_user) {
                                            $r_users[$role_user->id] = $role_user->name;
                                        }
                                    }
                                    foreach ($users as $user_id => $user) {
                                        if ($params['phone'][$key][2] != '' && !isset($r_users[$user->id])) {
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
                                $role->retrieve($params['phone'][$key][2]);
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
                            $user_phone = $user->phone_mobile;
                            if (trim($user_phone) != '') {
                                $recipients[] = array(
                                    'phone' => $user_phone,
                                    'parent_type' => $user->module_name,
                                    'parent_id' => $user->id,
                                );
                            }
                        }
                        break;
                    case 'Related Field':
                        $phoneTarget = $params['phone'][$key];
                        $relatedFields = array_merge($bean->get_related_fields(), $bean->get_linked_fields());
                        $field = $relatedFields[$phoneTarget];
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
                                    $rel_phone = stic_MessagesUtils::getPhoneForMessage($linkedBean);
                                    if (trim($rel_phone) != '') {
                                        $recipients[] = array(
                                            'phone' => $rel_phone,
                                            'parent_type' => $linkedBean->module_name,
                                            'parent_id' => $linkedBean->id,
                                        );
                                    }
                                }
                            }
                        }
                        break;
                    case 'Record Phone':
                        $recordPhone = stic_MessagesUtils::getPhoneForMessage($bean);
                        if (trim($recordPhone) != '') {
                            $recipients[] = array(
                                'phone' =>$recordPhone,
                                'parent_type' => $bean->module_name,
                                'parent_id' => $bean->id,
                            );
                        }
                        break;
                }
            }
        }
        return $recipients;
    }

    /**
     * Return true on success otherwise false.
     *
     * @param SugarBean $bean
     * @param array $params
     * @param bool $in_save
     * @return boolean
     */
    public function run_action(SugarBean $bean, $params = array(), $in_save = false)
    {
        include_once 'modules/EmailTemplates/EmailTemplate.php';

        // Recuperamos el template (si lo hay)
        $emailTemplate = BeanFactory::getBean('EmailTemplates', $params['email_template']);
        $txt = $emailTemplate->body;

        // Se sustituyen las variables con la información del bean y se genera un TXT sin variables
        // No se puede dejar la sustitución al propio bean en el save, ya que el mensaje se relaciona con un bean distinto del que origina el mensaje.
        $messageBean = BeanFactory::newBean('stic_Messages');
        $txt = $messageBean->replaceTemplateVariables($txt, $bean);

        // Se recuperan los objetos a los que enviar el mensaje (tipo, id y teléfono)
        $recipients = $this->getPhonesFromParams($bean, $params);

        $messageBean->sender = $params['sender_name'];
        $messageBean->template_id = $params['email_template'];
        $messageBean->status = $params['status'];
        $messageBean->type = $params['type'];

        // Direction field not used until new types added
        // $messageBean->direction = $params['direction'];

        $messageBean->message = $txt;
        $name = $messageBean->fillName($bean->module_name, $bean->id);

        foreach($recipients as $recipient) {
            // Por cada destinatario se crea un mensaje
            $messageBean = BeanFactory::newBean('stic_Messages');

            $messageBean->sender = $params['sender_name'];
            $messageBean->template_id = $params['email_template'];
            $messageBean->status = $params['status'];
            $messageBean->type = $params['type'];
            // Direction field not used until new types added
            // $messageBean->direction = $params['direction'];
            $messageBean->phone = $recipient['phone'];
            $messageBean->parent_type = $recipient['parent_type'];
            $messageBean->parent_id = $recipient['parent_id'];
            $messageBean->message = $txt;
            $messageBean->name = $name;
            $messageBean->save();
        }

        return true;
    }
}