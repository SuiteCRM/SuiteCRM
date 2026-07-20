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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\Utility\SuiteValidator;

include_once 'include/Exceptions/SugarControllerException.php';
require_once 'modules/stic_Messages/Utils.php';
require_once("modules/AOW_WorkFlow/aow_utils.php");

class stic_MessagesController extends SugarController
{
    // We remap EditView action when no id is present (new record) to use the ComposeView
    protected function remapAction()
    {
        if (!empty($this->action_remap[$this->do_action])) {
            $this->action = $this->action_remap[$this->do_action];
            $this->do_action = $this->action;
        }

        if ($this->do_action == 'EditView' && empty($this->bean->id)) {
            $this->action = 'ComposeView';
            $this->do_action = 'ComposeView';
        }
    }

    public function action_Save() {
        if (isset($_REQUEST['mass_ids']) && $_REQUEST['mass_ids'] !== '') {
            $idsArray = explode(';', $_REQUEST['mass_ids']);
            $phonesArray = explode(',', $_REQUEST['phone']);

            array_map(function($id, $phone) {
                $newBean = BeanFactory::newBean('stic_Messages');
                $this->bean = $newBean;
                $this->pre_save();
                $this->bean->parent_id = $id;
                $this->bean->parent_type = $_REQUEST['return_module'];
                $this->bean->phone = $phone;
                $this->prepareConversationDataForMessage();
                $this->bean->save(!empty($this->bean->notify_on_save));
            }, $idsArray, $phonesArray);
        }
        else {
            $this->pre_save();
            $this->applyConversationSubpanelDefaults();
            $this->prepareConversationDataForMessage();
            if (!$this->validateConversationRequiredFields($this->bean)) {
                echo json_encode(array('success' => false, 'number_found' => false));
                exit;
            }
            $this->bean->save(!empty($this->bean->notify_on_save));

            $saveSuccess = ($this->bean->status === 'sent' || $this->bean->status === 'redirected');
            echo json_encode(array(
                'success' => $saveSuccess,
                'number_found' => true,
                'message_status' => $this->bean->status ?? '',
                'response' => $this->bean->response ?? '',
            ));
            exit;
        }
    }

    public function pre_savePopUp(){
        parent::pre_save();
    }

    public function action_SavePopUp() {
        global $app_strings, $current_language;
        $mod_strings = return_module_language($current_language, $this->module);

        if (isset($_REQUEST['mass_ids']) && $_REQUEST['mass_ids'] !== '') {
            $idsArray = explode(';', $_REQUEST['mass_ids']);
            $phonesArray = explode(',', $_REQUEST['phone']);

            // Load helper once for mass save response
            $type = $_REQUEST['type'] ?? '';
            $messageHelper = stic_MessagesUtils::instantiateHelperByType($type);

            array_map(function($id, $phone) use ($mod_strings) {
                $newBean = BeanFactory::newBean('stic_Messages');
                $this->bean = $newBean;
                $this->pre_save();
                $this->bean->parent_id = $id;
                $this->bean->parent_type = $_REQUEST['return_module'];
                $this->bean->phone = $phone;
                $conversationValidation = $this->prepareConversationDataForMessage();
                if (!$conversationValidation['success']) {
                    $this->returnConversationValidationError($mod_strings);
                }
                $this->bean->save(!empty($this->bean->notify_on_save));
            }, $idsArray, $phonesArray);

            // Check if helper has custom mass save response (e.g., WhatsAppWeb redirect)
            if ($messageHelper !== null) {
                $phonesRaw = isset($_REQUEST['phone']) ? $_REQUEST['phone'] : '';
                $phonesList = $phonesRaw !== '' ? explode(',', $phonesRaw) : array();
                $text = isset($_REQUEST['message']) ? $_REQUEST['message'] : '';
                $customResponse = $messageHelper->getMassSaveResponseData($phonesList, $text, $idsArray);

                if (!empty($customResponse)) {
                    while (ob_get_level()) { ob_end_clean(); }
                    header('Content-Type: application/json');
                    echo json_encode(array_merge(
                        ['success' => true, 'title' => $app_strings['LBL_EMAIL_SUCCESS'], 'detail' => $mod_strings['LBL_WHATSAPP_WEB_SENT']],
                        $customResponse
                    ));
                    exit;
                }
            }

            // Clear any accidental output (warnings, HTML, etc.) so the response is pure JSON
            while (ob_get_level()) { ob_end_clean(); }
            header('Content-Type: application/json');
            echo json_encode(array('success' => true, 'type' => 'sms', 'title' => $app_strings['LBL_EMAIL_SUCCESS'], 'detail' => $mod_strings['LBL_CHECK_STATUS']));
            exit;
        }
        else {
            $this->pre_save();
            $oldStatus = $this->bean->fetched_row['status']??'';

            // Subpanel conversations to validate
            $this->applyConversationSubpanelDefaults();
            if (!$this->validateConversationRequiredFields($this->bean)) {
                $this->returnConversationRequiredFieldsError($mod_strings);
            }

            $conversationValidation = $this->prepareConversationDataForMessage();
            if (!$conversationValidation['success']) {
                $this->returnConversationValidationError($mod_strings);
            }

            $id = $this->bean->save(!empty($this->bean->notify_on_save));

            // Check if helper has custom save response (e.g., WhatsAppWeb redirect)
            $messageHelper = stic_MessagesUtils::instantiateHelperByType($this->bean->type ?? '');
            if ($messageHelper !== null) {
                $customResponse = $messageHelper->getSaveResponseData($this->bean);
                if (!empty($customResponse)) {
                    while (ob_get_level()) { ob_end_clean(); }
                    header('Content-Type: application/json');
                    echo json_encode(array_merge(['success' => true], $customResponse));
                    exit;
                }
            }

            // Ensure response is clean JSON
            while (ob_get_level()) { ob_end_clean(); }
            header('Content-Type: application/json');
            switch ($this->bean->status) {
                case 'sent':
                    if ($this->bean->status !== $oldStatus) {
                        $title = $app_strings['LBL_EMAIL_SUCCESS'];
                        $detail = $mod_strings['LBL_MESSAGE_SENT'];
                    }
                    else {
                        $title = $app_strings['LBL_EMAIL_SUCCESS'];
                        $detail = $mod_strings['LBL_MESSAGE_SAVED'];
                    }
                    break;
                case 'error':
                    $title = $mod_strings['LBL_ERROR'];
                    $detail = $mod_strings['LBL_MESSAGE_NOT_SENT'];
                    if (!empty($this->bean->response)) {
                        $responseData = json_decode($this->bean->response, true);
                        if ($responseData && !empty($responseData['message'])) {
                            $detail .= ': ' . $responseData['message'];
                        } else {
                            $detail .= ': ' . $this->bean->response;
                        }
                    }
                    break;
                case 'draft':
                    $title = $app_strings['LBL_EMAIL_SUCCESS'];
                    $detail = $mod_strings['LBL_MESSAGE_SAVED'];
                    break;
                default:
                    $title = $mod_strings['LBL_EMAIL_SUCCESS'];
                    $detail = $mod_strings['LBL_MESSAGE_SAVED'];
            }
            echo json_encode(array('success' => $this->bean->status === 'error' ? false : true, 'type' => 'sms', 'title' => $title, 'detail' => $detail, 'id' => $id));
            exit;
        }
    }


    public function action_RetryOne() {
        global $app_strings, $mod_strings;

        $id = $_REQUEST['recordId'];
        $bean = BeanFactory::getBean('stic_Messages', $id);
        
        // Check ACL edit permission
        if ($bean->bean_implements('ACL')) {
            if (!ACLController::checkAccess($bean->module_dir, 'edit', true)) {
                ACLController::displayNoAccess();
                sugar_die('');
            }
        }
        
        // Check if this message type can be retried
        $messageHelper = stic_MessagesUtils::instantiateHelperByType($bean->type ?? '');
        if ($messageHelper !== null && !$messageHelper->isRetryable()) {
            echo json_encode(array(
                'success' => false, 
                'title' => $mod_strings['LBL_ERROR'], 
                'detail' => $mod_strings['LBL_WHATSAPP_WEB_RETRY'],
                'id' => $id
            ));
            exit;
        }
        
        $bean->status = 'sent';
        $bean->save();

        $title = $bean->status !== 'error' ? $app_strings['LBL_EMAIL_SUCCESS'] : $mod_strings['LBL_ERROR'];
        $detail = $bean->status !== 'error' ? $mod_strings['LBL_MESSAGE_SENT'] : $mod_strings['LBL_MESSAGE_NOT_SENT'];
        echo json_encode(array('success' => $bean->status === 'error' ? false : true, 'title' => $title, 'detail' => $detail, 'id' => $id));
        exit;
    }
    public function action_Retry(){

        $db = DBManagerFactory::getInstance();
        $where = '';

        $focus = BeanFactory::newBean('stic_Messages');
        if ($focus->bean_implements('ACL')) {
            if (!ACLController::checkAccess($focus->module_dir, 'edit', true)) {
                ACLController::displayNoAccess();
                sugar_die('');
            }
        }

        // only messages not sent and with direction outbound can be retried
        // Non-retryable types are excluded dynamically
        $nonRetryableTypes = stic_MessagesUtils::getNonRetryableTypes();
        $typeExclusion = '';
        if (!empty($nonRetryableTypes)) {
            $escapedTypes = array_map(function($t) use ($db) {
                return "'" . $db->quote($t) . "'";
            }, $nonRetryableTypes);
            $typeExclusion = " AND stic_messages.type NOT IN (" . implode(',', $escapedTypes) . ")";
        }
        $baseWhere = "stic_messages.deleted = 0 AND stic_messages.status <> 'sent' AND stic_messages.direction = 'outbound'" . $typeExclusion;

        if (isset($_REQUEST['select_entire_list']) && $_REQUEST['select_entire_list'] == '1' && isset($_REQUEST['current_query_by_page'])) {
            require_once 'include/export_utils.php';
            $retArray = generateSearchWhere('stic_Messages', $_REQUEST['current_query_by_page']);
            if (!empty($retArray['where'])) {
                $where = $baseWhere . " AND " . $retArray['where'];
            } else {
                $where = $baseWhere;
            }
        } else {
            $ids = explode(',', $_REQUEST['uid']);
            $idList = implode("','", $ids);
            $where = $baseWhere . " AND stic_messages.id in ('{$idList}')";
        }

        $orderBy = 'stic_messages.date_entered DESC';
        $beans = $focus->get_full_list($orderBy, $where);

        foreach ($beans as $bean) {
            $bean->status = 'sent';
            $bean->save();
        }

        SugarApplication::redirect("index.php?module=stic_Messages&action=index");
    }

    public function action_ComposeView() {
        $this->view = 'edit';
        // For viewing the Compose as modal from other modules we need to load the stic_Messages language strings
        if (isset($_REQUEST['in_popup']) && $_REQUEST['in_popup']) {
            if (!is_file('cache/jsLanguage/stic_Messages/' . $GLOBALS['current_language'] . '.js')) {
                require_once('include/language/jsLanguage.php');
                jsLanguage::createModuleStringsCache('stic_Messages', $GLOBALS['current_language']);
            }
            echo '<script src="cache/jsLanguage/stic_Messages/'. $GLOBALS['current_language'] . '.js"></script>';
        }


        // Building and running the query that retrieves all the record that were selected in ListView
        if(!empty($_REQUEST['targetModule'])){
            $bean = BeanFactory::getBean($_REQUEST['targetModule']);
            // Check if the target module has the necessary configuration to show the phone field in the compose view
            $messageableModules = stic_MessagesUtils::getMessageableModules();
            if (empty($bean->module_name) || !in_array($bean->module_name, $messageableModules, true)) {
                return;
            }

            $phoneFieldName = stic_MessagesUtils::getPhoneFieldNameForMessage($bean->module_name);
            $nameFieldName = stic_MessagesUtils::getNameFieldNameForMessage($bean->module_name);
            $moduleTable = $bean->table_name;
            $moduleName = $bean->module_name;
            $sql = "SELECT id, $phoneFieldName as phone, $nameFieldName as name FROM {$moduleTable} WHERE {$moduleTable}.deleted=0";
            $where = '';
            if (isset($_REQUEST['select_entire_list']) && $_REQUEST['select_entire_list'] == '1' && isset($_REQUEST['current_query_by_page'])) {
                require_once 'include/export_utils.php';
                $retArray = generateSearchWhere($moduleName, $_REQUEST['current_query_by_page']);
                $where = '';
                if (!empty($retArray['where'])) {
                    $where = " AND " . $retArray['where'];
                }
            } else {
                $ids = explode(',', rtrim($_REQUEST['ids'], ','));
                $idList = implode("','", $ids);
                $where = " AND id in ('{$idList}')";
            }
            $sql .= $where;
            $db = DBManagerFactory::getInstance();
            $resultado = $db->query($sql);
            unset($ids);
            $ids = array();
    
            while ($row = $db->fetchByAssoc($resultado)) {
                // Building the Summary count table
                $idLine = '<input type="hidden" class="phone-compose-view-to-list" ';
                $idLine .= 'data-record-module="' . $_REQUEST['targetModule'] . '" ';
                $idLine .= 'data-record-id="' . $row['id'] . '" ';
                $idLine .= 'data-record-name="' . $row['name'] . '" ';
                $idLine .= 'data-record-phone="' . $row['phone'] . '">';
                echo $idLine;
            }
        }
    }

    public function action_getParentPhone() {
        $parentId = $_POST["parentId"];
        $parentType = $_POST["parentType"];
        
        require_once('modules/stic_Messages/Utils.php');
        $phoneFieldName = stic_MessagesUtils::getPhoneFieldNameForMessage($parentType);
        $tableName = stic_MessagesUtils::gettableNameForMessage($parentType);

        $response = array();
        $response['code'] = 'No data';
        $db = DBManagerFactory::getInstance();

        $sql = "SELECT {$phoneFieldName} as phone FROM {$tableName} WHERE id = '{$parentId}'";

        $result = $db->query($sql);
        if($row = $result->fetch_assoc()) {
            $response['code'] = 'OK';
            $response['data']['phone'] = $row['phone'];
        }
        else {
            $response['data']['phone'] = '';
        }

        echo json_encode($response);
        exit;
    }

    protected function action_getPhoneField() {
        $module = $_REQUEST['aow_module'];
        $aow_field = $_REQUEST['aow_newfieldname'];

        if (isset($_REQUEST['view'])) {
            $view = $_REQUEST['view'];
        } else {
            $view= 'EditView';
        }

        if (isset($_REQUEST['aow_value'])) {
            $value = $_REQUEST['aow_value'];
        } else {
            $value = '';
        }

        switch ($_REQUEST['aow_type']) {
            case 'Record Phone':
                echo '';
                break;
            case 'Related Field':
                $rel_field_list = stic_MessagesUtils::getRelatedMessageableFields($module);
                if ($view == 'EditView') {
                    echo "<select type='text'  name='$aow_field' id='$aow_field' title='' tabindex='116'>". get_select_options_with_id($rel_field_list, $value) ."</select>";
                } else {
                    echo $rel_field_list[$value];
                }
                break;
            case 'Specify User':
                echo getModuleField('Accounts', 'assigned_user_name', $aow_field, $view, $value);
                break;
            case 'Users':
                echo getAssignField($aow_field, $view, $value);
                break;
            case 'Phone':
            default:
                if ($view == 'EditView') {
                    echo "<input type='text' name='$aow_field' id='$aow_field' size='25' title='' tabindex='116' value='$value'>";
                } else {
                    echo $value;
                }
                break;
        }
        die;
    }

    public function action_FillDynamicListMessageTemplate() {
        $typeParam = $_REQUEST['type'] ?? '';

        // Try to get template type from helper if type is a helper class name
        $helperType = null;
        if (!empty($typeParam)) {
            $file = $typeParam;
            if (file_exists('custom/modules/stic_Messages/Helpers/' . $file . '.php')) {
                require_once('custom/modules/stic_Messages/Helpers/' . $file . '.php');
                $helper = new $file;
                $helperType = $helper->getTemplateType();
            } elseif (file_exists('modules/stic_Messages/Helpers/' . $file . '.php')) {
                require_once('modules/stic_Messages/Helpers/' . $file . '.php');
                $helper = new $file;
                $helperType = $helper->getTemplateType();
            }
        }

        // Fallback: infer from type string if helper not found
        if ($helperType === null) {
            $typeLower = strtolower($typeParam);
            if (strpos($typeLower, 'whatsapp') !== false) {
                $helperType = 'whatsapp';
            } elseif (strpos($typeLower, 'sms') !== false || strpos($typeLower, 'seven') !== false) {
                $helperType = 'sms';
            } else {
                $helperType = 'sms';
            }
        }

        require_once 'modules/stic_Messages/Utils.php';
        stic_MessagesUtils::fillDynamicListMessageTemplate($helperType);

        $list = $GLOBALS['app_list_strings']['dynamic_message_template_list'] ?? array();

        // Convert associative array to list of {id,name}
        $out = array();
        foreach ($list as $id => $name) {
            // skip empty key used for 'None'
            if ($id === '') continue;
            $out[] = array('id' => $id, 'name' => $name);
        }

        header('Content-Type: application/json');
        echo json_encode(array('success' => true, 'data' => $out));
        exit;
    }

    public function action_checkWhatsAppWindow() {
        $parentId = $_REQUEST['parent_id']   ?? '';
        $parentType = $_REQUEST['parent_type'] ?? '';

        require_once 'modules/stic_Messages/Utils.php';
        $windowState = stic_MessagesUtils::getWhatsAppWindowState($parentId, $parentType);

        echo json_encode(array(
            'success' => true,
            'windowOpen' => $windowState['windowOpen'],
            'hoursLeft' => $windowState['hoursLeft'],
            'minutesLeft' => $windowState['minutesLeft']
        ));
        exit;
    }

    /**
     * Create and return a conversation id for the current message
     */
    protected function createConversationForMessage($messageBean, $conversationSubject = '')
    {
        global $current_user;

        $conversationBean = BeanFactory::newBean('stic_Conversations');
        $cleanSubject = trim(strip_tags((string)$conversationSubject));
        $conversationBean->subject = mb_substr($cleanSubject, 0, 60);
        $conversationBean->assigned_user_id = !empty($messageBean->assigned_user_id) ? $messageBean->assigned_user_id : $current_user->id;

        if (!empty($messageBean->parent_type) && $messageBean->parent_type === 'Contacts' && !empty($messageBean->parent_id)) {
            $conversationBean->contact_name = $messageBean->parent_name;
            $conversationBean->contacts_ida = $messageBean->parent_id;
        }

        $conversationBean->save();
        return $conversationBean->id;
    }

    /**
     * Normalize conversation data before saving a message
     */
    protected function prepareConversationDataForMessage()
    {
        $messageBean = $this->bean;
        $type = $messageBean->type ?? '';

        if ($type !== 'private_area') {
            $messageBean->new_conversation = 0;
            $messageBean->stic_conversation_subject = '';
            $messageBean->stic_conversations_stic_messages_name = '';
            $messageBean->stic_conversations_ida = '';
            return array('success' => true);
        }

        // Conversations can only be related to Contacts.
        $messageBean->parent_type = 'Contacts';

        if (!empty($messageBean->parent_id)) {
            $contactBean = BeanFactory::getBean('Contacts', $messageBean->parent_id);
            if (empty($contactBean) || empty($contactBean->id)) {
                $messageBean->parent_id = '';
                $messageBean->parent_name = '';
            }
        }

        $newConversation = !empty($messageBean->new_conversation);
        $messageBean->new_conversation = $newConversation ? 1 : 0;

        if ($newConversation) {
            $conversationSubject = trim((string)($messageBean->stic_conversation_subject ?? ''));
            if ($conversationSubject === '') {
                return array('success' => false);
            }

            if (empty($messageBean->stic_conversations_ida)) {
                $messageBean->stic_conversations_ida = $this->createConversationForMessage($messageBean, $conversationSubject);
            }
            return array('success' => true);
        }

        if (empty($messageBean->stic_conversations_ida)) {
            return array('success' => false);
        }

        return array('success' => true);
    }

    /**
     * Check if current save comes from Conversations subpanel quickcreate
     */
    protected function isConversationSubpanelSaveRequest()
    {
        return (
            !empty($_REQUEST['return_module'])
            && $_REQUEST['return_module'] === 'stic_Conversations'
            && !empty($_REQUEST['return_id'])
        );
    }

    /**
     * Force subpanel conversation defaults
     */
    protected function applyConversationSubpanelDefaults()
    {
        $messageBean = $this->bean;
        if (!$this->isConversationSubpanelSaveRequest()) {
            return;
        }

        $messageBean->type = 'private_area';
        $messageBean->parent_type = 'Contacts';
        $messageBean->new_conversation = 0;

        if (empty($messageBean->stic_conversations_ida)) {
            $messageBean->stic_conversations_ida = $_REQUEST['return_id'];
        }

        if (empty($_REQUEST['stic_conversations_ida'])) {
            $_REQUEST['stic_conversations_ida'] = $messageBean->stic_conversations_ida;
        }

        if (!empty($messageBean->stic_conversations_ida) && (empty($messageBean->parent_id) || empty($messageBean->parent_name))) {
            $conversationBean = BeanFactory::getBean('stic_Conversations', $messageBean->stic_conversations_ida);
            if (!empty($conversationBean) && !empty($conversationBean->id) && $conversationBean->load_relationship('contacts_stic_conversations')) {
                $contactIds = $conversationBean->contacts_stic_conversations->get();
                if (!empty($contactIds) && !empty($contactIds[0])) {
                    $contactBean = BeanFactory::getBean('Contacts', $contactIds[0]);
                    if (!empty($contactBean) && !empty($contactBean->id)) {
                        $messageBean->parent_id = $contactBean->id;
                        $contactName = trim(($contactBean->first_name ?? '') . ' ' . ($contactBean->last_name ?? ''));
                        $messageBean->parent_name = !empty($contactName) ? $contactName : ($contactBean->name ?? '');
                    }
                }
            }
        }
    }

    /**
     * Validate required fields for conversation messages
     */
    protected function validateConversationRequiredFields($messageBean)
    {
        $type = $messageBean->type ?? ($_REQUEST['type'] ?? '');
        if ($type !== 'private_area') {
            return true;
        }

        $message = trim((string)($messageBean->message ?? ($_REQUEST['message'] ?? '')));

        return ($message !== '');
    }

    /**
     * Return JSON validation error for conversation-related saves
     */
    protected function returnConversationValidationError($mod_strings)
    {
        while (ob_get_level()) {
            ob_end_clean();
        }
        header('Content-Type: application/json');
        echo json_encode(array(
            'success' => false,
            'type' => 'sms',
            'title' => $mod_strings['LBL_ERROR'],
            'detail' => $mod_strings['LBL_NEW_CONVERSATION_HELP'],
        ));
        exit;
    }

    public function action_conversation() {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'stic_Messages');

        $parentId = $_REQUEST['parent_id']   ?? '';
        $parentType = $_REQUEST['parent_type'] ?? 'Contacts';

        if (empty($parentId)) die('Missing parent_id');

        $db = DBManagerFactory::getInstance();
        $parentIdSafe = $db->quote($parentId);

        require_once('modules/stic_Messages/Utils.php');
        $contactPhone = '';
        $parentName = '';
        $contactBean = BeanFactory::getBean($parentType, $parentId);
        if ($contactBean) {
            $contactPhone = stic_MessagesUtils::getPhoneForMessage($contactBean);
            $parentName = $contactBean->name ?? $contactBean->full_name ?? '';
        }
        $sql = "SELECT id, message, type, status, date_entered, sender, phone, direction,
                    template_id
                FROM stic_messages
                WHERE parent_id = '{$parentIdSafe}'
                AND deleted = 0
                AND type IN ('whatsapp', 'whatsapp_web')
                ORDER BY date_entered ASC";

        $result = $db->query($sql);
        $messages = [];
        while ($row = $db->fetchByAssoc($result)) {
            $messages[] = $row;
        }

        // Calculate 24h window using shared utility function
        $windowState = stic_MessagesUtils::getWhatsAppWindowState($parentId, $parentType);
        $windowOpen = $windowState['windowOpen'];

        if ($windowOpen) {
            $windowMessage = sprintf(
                $mod_strings['LBL_CONVERSATION_WINDOW_OPEN'],
                $windowState['hoursLeft'],
                $windowState['minutesLeft']
            );
        } elseif (!empty($messages)) {
            // Find the last event date for closed message
            $lastEvent = null;
            foreach (array_reverse($messages) as $msg) {
                if ($msg['type'] === 'whatsapp' || $msg['type'] === 'whatsapp_web') {
                    if (!empty($msg['template_id'])) {
                        $templateBean = BeanFactory::getBean('EmailTemplates', $msg['template_id']);
                        if ($templateBean && !empty($templateBean->stic_whatsapp_twilio_id_c)) {
                            $lastEvent = $msg['date_entered'];
                            break;
                        }
                    } else {
                        // Free-text message sent within the 24h window also counts as event
                        $lastEvent = $msg['date_entered'];
                        break;
                    }
                }
            }
            if ($lastEvent) {
                $lastEventFormatted = $GLOBALS['timedate']->to_display_date_time($lastEvent);
                $windowMessage = sprintf(
                    $mod_strings['LBL_CONVERSATION_WINDOW_CLOSED'],
                    $lastEventFormatted
                );
            } else {
                $windowMessage = $mod_strings['LBL_CONVERSATION_NO_HISTORY'];
            }
        } else {
            $windowMessage = $mod_strings['LBL_CONVERSATION_NO_HISTORY'];
        }

        // Build URL to create a new stic_Messages record pre-linked to the parent
        $newMessageUrl = 'index.php?module=stic_Messages&action=EditView'
            . '&return_module=' . urlencode($parentType)
            . '&return_id='     . urlencode($parentId)
            . '&parent_type='   . urlencode($parentType)
            . '&parent_id='     . urlencode($parentId)
            . '&parent_name='   . urlencode($parentName)
            . '&phone='         . urlencode($contactPhone)
            . '&type='          . urlencode('whatsapp');

        require_once('modules/stic_Messages/views/view.conversation.php');
        $view = new stic_MessagesViewConversation();
        $view->messages = $messages;
        $view->parentName = $parentName;
        $view->parentId = $parentId;
        $view->parentType = $parentType;
        $view->contactPhone = $contactPhone;
        $view->windowOpen = $windowOpen;
        $view->windowMessage = $windowMessage;
        $view->newMessageUrl = $newMessageUrl;
        $view->modStrings = $mod_strings;
        $view->display();
        sugar_cleanup();
        exit();
    }

    public function action_uploadConversationMedia() {
        header('Content-Type: application/json');

        $allowedMimes = [
            'image/jpeg', 'image/png', 'image/gif', 'image/webp',
            'video/mp4', 'video/3gpp',
            'audio/ogg', 'audio/mpeg', 'audio/mp4', 'audio/amr',
            'application/pdf',
            'application/msword',
            'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
            'application/vnd.ms-excel',
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
            'application/vnd.ms-powerpoint',
            'application/vnd.openxmlformats-officedocument.presentationml.presentation',
            'text/csv',
            'application/vnd.openxmlformats-officedocument.presentationml.presentation',
        ];

        if (empty($_FILES['media']) || $_FILES['media']['error'] !== UPLOAD_ERR_OK) {
            echo json_encode(['success' => false, 'error' => $this->sticMessagesGetString('LBL_ERROR_NO_FILE_RECEIVED')]);
            exit();
        }

        $file = $_FILES['media'];
        $mimeType = mime_content_type($file['tmp_name']);

        if (!in_array($mimeType, $allowedMimes)) {
            echo json_encode(['success' => false, 'error' => $this->sticMessagesGetString('LBL_ERROR_UNSUPPORTED_FILE_TYPE') . ': ' . $mimeType]);
            exit();
        }

        $sizeLimit = (strpos($mimeType, 'image/') === 0) ? 5 * 1024 * 1024 : 16 * 1024 * 1024;
        if ($file['size'] > $sizeLimit) {
            $limitMb = $sizeLimit / 1024 / 1024;
            echo json_encode(['success' => false, 'error' => $this->sticMessagesGetString('LBL_ERROR_FILE_SIZE_EXCEEDED') . " {$limitMb}MB"]);
            exit();
        }

        $note = BeanFactory::newBean('Notes');
        $note->parent_type = 'stic_Messages';
        $note->parent_id = '';
        $note->name = $file['name'];
        $note->filename = $file['name'];
        $note->file_mime_type = $mimeType;
        $note->deleted = 0;
        $noteId = $note->save();

        if (empty($noteId)) {
            echo json_encode(['success' => false, 'error' => $this->sticMessagesGetString('LBL_ERROR_CREATING_NOTE')]);
            exit();
        }

        $destPath = rtrim(getcwd(), '/') . '/upload/' . $noteId;
        if (!move_uploaded_file($file['tmp_name'], $destPath)) {
            $note->deleted = 1;
            $note->save();
            echo json_encode(['success' => false, 'error' => $this->sticMessagesGetString('LBL_ERROR_SAVING_FILE')]);
            exit();
        }

        $GLOBALS['log']->info('stic_Messages: attachment uploaded. note_id=' . $noteId . ' file=' . $file['name']);

        echo json_encode([
            'success' => true,
            'media_note_id' => $noteId,
            'name' => $file['name'],
            'mime' => $mimeType,
        ]);
        exit();
    }
    /**
     * Return JSON validation error for required conversation fields
     */
    protected function returnConversationRequiredFieldsError($mod_strings)
    {
        global $app_strings;

        while (ob_get_level()) {
            ob_end_clean();
        }

        header('Content-Type: application/json');
        echo json_encode(array(
            'success' => false,
            'type' => 'sms',
            'title' => $mod_strings['LBL_ERROR'],
            'detail' => $app_strings['ERR_MISSING_REQUIRED_FIELDS'],
        ));
        exit;
    }

    /**
     * Get latest non-deleted message from conversation
     */
    protected function getLatestConversationMessage($conversationBean)
    {
        if (!$conversationBean->load_relationship('stic_conversations_stic_messages')) {
            return null;
        }

        $latestMessage = null;
        $latestMessageTs = -1;
        $relatedMessages = $conversationBean->stic_conversations_stic_messages->getBeans();

        foreach ($relatedMessages as $relatedMessage) {
            if (empty($relatedMessage) || !empty($relatedMessage->deleted)) {
                continue;
            }

            $currentTs = !empty($relatedMessage->date_entered) ? strtotime($relatedMessage->date_entered) : false;
            $currentTs = $currentTs !== false ? $currentTs : -1;

            if ($currentTs > $latestMessageTs) {
                $latestMessage = $relatedMessage;
                $latestMessageTs = $currentTs;
            }
        }

        return $latestMessage;
    }

    /**
     * Fill parent data from linked contact when missing
     */
    protected function fillConversationParentFromContact($conversationBean, &$data)
    {
        if (!empty($data['parent_id']) && !empty($data['parent_name'])) {
            return;
        }

        if (!$conversationBean->load_relationship('contacts_stic_conversations')) {
            return;
        }

        $contactIds = $conversationBean->contacts_stic_conversations->get();
        if (empty($contactIds) || empty($contactIds[0])) {
            return;
        }

        $contactBean = BeanFactory::getBean('Contacts', $contactIds[0]);
        if (!empty($contactBean) && !empty($contactBean->id)) {
            $data['parent_id'] = $contactBean->id;
            $data['parent_type'] = 'Contacts';
            $contactName = trim(($contactBean->first_name ?? '') . ' ' . ($contactBean->last_name ?? ''));
            $data['parent_name'] = !empty($contactName) ? $contactName : ($contactBean->name ?? '');
        }
    }

    /**
     * Check if response contains enough conversation data
     */
    protected function hasConversationData($data)
    {
        // If conversation id or name is present, we consider the conversation data valid
        if (!empty($data['conversation_id']) || !empty($data['conversation_name'])) {
            return true;
        }

        return !empty($data['sender'])
            || !empty($data['parent_id'])
            || !empty($data['parent_type'])
            || !empty($data['parent_name']);
    }

    /**
     * AJAX endpoint to retrieve sender/parent from conversation
     */
    public function action_getConversationData() {
        $conversationId = $_POST['conversationId'] ?? '';

        $response = array();
        $response['code'] = 'No data';
        $response['data'] = array(
            'sender' => '',
            'assigned_user_id' => '',
            'assigned_user_name' => '',
            'conversation_id' => '',
            'conversation_name' => '',
            'parent_id' => '',
            'parent_type' => '',
            'parent_name' => '',
        );

        if (!empty($conversationId)) {
            $conversationBean = BeanFactory::getBean('stic_Conversations', $conversationId);

            if (!empty($conversationBean) && !empty($conversationBean->id)) {
                $response['data']['conversation_id'] = $conversationBean->id;
                $response['data']['conversation_name'] = $conversationBean->name ?? '';
                $response['data']['assigned_user_id'] = $conversationBean->assigned_user_id ?? '';
                $response['data']['assigned_user_name'] = $conversationBean->assigned_user_name ?? '';

                $latestMessage = $this->getLatestConversationMessage($conversationBean);
                if ($latestMessage) {
                    $response['data']['sender'] = $latestMessage->sender ?? '';
                    $response['data']['parent_id'] = $latestMessage->parent_id ?? '';
                    $response['data']['parent_type'] = $latestMessage->parent_type ?? '';
                    $response['data']['parent_name'] = $latestMessage->parent_name ?? '';

                    if (empty($response['data']['assigned_user_id']) && !empty($latestMessage->assigned_user_id)) {
                        $response['data']['assigned_user_id'] = $latestMessage->assigned_user_id;
                    }

                    if (empty($response['data']['assigned_user_name']) && !empty($latestMessage->assigned_user_name)) {
                        $response['data']['assigned_user_name'] = $latestMessage->assigned_user_name;
                    }
                }

                $this->fillConversationParentFromContact($conversationBean, $response['data']);

                if ($this->hasConversationData($response['data'])) {
                    $response['code'] = 'OK';
                }
            }
        }

        echo json_encode($response);
        exit;
    }

    private function sticMessagesGetString($key)
    {
        global $mod_strings;
        return $mod_strings[$key] ?? $key;
    }
}