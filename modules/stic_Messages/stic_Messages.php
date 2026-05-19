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

class stic_Messages extends Basic
{
    const OK = 1;
    const ERROR_NO_HELPER_CLASS = 11;
    const ERROR_NOT_SENT = 12;

    public $new_schema = true;
    public $module_dir = 'stic_Messages';
    public $object_name = 'stic_Messages';
    public $table_name = 'stic_messages';
    public $importable = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $SecurityGroups;

    public $type;
    public $direction;
    public $phone;
    public $sender;
    public $message;
    public $template_id;
    public $parent_type;
    public $parent_id;
    public $status;
    public $response;


    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     * Override the bean's save function to assign an auto-incrementing value to the code field when a new record is created
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = false)
    {
        global $current_user;

        if (empty($this->name)){
            $this->fillName();
        }

        // WhatsAppWeb messages cannot be edited once created (they are always sent immediately)
        if ($this->fetched_row && $this->fetched_row['type'] === 'WhatsAppWeb' && !empty($this->id)) {
            // Restore all original values to prevent any modification
            $this->template_id = $this->fetched_row['template_id'];
            $this->message = $this->fetched_row['message'];
            $this->type = $this->fetched_row['type'];
            $this->phone = $this->fetched_row['phone'];
            $this->status = $this->fetched_row['status'];
            $this->sender = $this->fetched_row['sender'];
            // Only allow updating parent relationship
            // parent_type and parent_id can still be updated
        }

        // If message is not in draft, only direction and related to can be changed
        if ($this->fetched_row && $this->fetched_row['status'] !== 'draft' && !empty($this->id)) {
            $this->template_id = $this->fetched_row['template_id'];
            $this->message = $this->fetched_row['message'];
            $this->type = $this->fetched_row['type'];
            $this->phone = $this->fetched_row['phone'];
        }

        // If there is nothing in the message field, assume the template body
        if (empty($this->message) && !empty($this->template_id)) {
            $template = BeanFactory::getBean('EmailTemplates', $this->template_id);
            $this->message = $template->body;
        }

        // Only if we are in mass update, assume the body of the template as the message, otherwise the user may have modified the text
        if ($this->status === 'draft' && ($_REQUEST['massupdate']??false) && $this->template_id !== $this->fetched_row['template']) {
            $template = BeanFactory::getBean('EmailTemplates', $this->template_id);
            $this->message = $template->body;
        }

        if ($this->status === 'sent') {
            $bean = BeanFactory::getBean($this->parent_type, $this->parent_id);
    
            $processedText = $this->replaceTemplateVariables($this->message, $bean);
            $this->message = $processedText;
        }

        $assignedUserId = $current_user->id ?? '';

        // For conversation messages, resolve the assignee from related records
        if ($this->type === 'private_area') {
            $contactId = '';
            $conversationBean = null;

            // If the message already points to a Contact, use it first
            if ($this->parent_type === 'Contacts' && !empty($this->parent_id)) {
                $contactId = $this->parent_id;
            }

            // Otherwise, try to get the Contact from the related conversation
            if (empty($contactId) && !empty($this->stic_conversations_ida)) {
                $conversationBean = BeanFactory::getBean('stic_Conversations', $this->stic_conversations_ida);
                if (!empty($conversationBean) && !empty($conversationBean->id) && $conversationBean->load_relationship('contacts_stic_conversations')) {
                    $contactIds = $conversationBean->contacts_stic_conversations->get();
                    if (!empty($contactIds) && !empty($contactIds[0])) {
                        $contactId = $contactIds[0];
                    }
                }
            }

            // If we have a Contact, assign the message to that Contact's assigned user
            if (!empty($contactId)) {
                $contactBean = BeanFactory::getBean('Contacts', $contactId);
                if (!empty($contactBean) && !empty($contactBean->id) && !empty($contactBean->assigned_user_id)) {
                    $assignedUserId = $contactBean->assigned_user_id;
                }
            }

            // Ensure parent points to the resolved Contact for notifications
            if (empty($this->parent_id) && !empty($contactId)) {
                $this->parent_id = $contactId;
            }

            // Use the conversation assigned user
            if (empty($assignedUserId) && !empty($this->stic_conversations_ida)) {
                if (empty($conversationBean) || empty($conversationBean->id)) {
                    $conversationBean = BeanFactory::getBean('stic_Conversations', $this->stic_conversations_ida);
                }

                if (!empty($conversationBean) && !empty($conversationBean->id) && !empty($conversationBean->assigned_user_id)) {
                    $assignedUserId = $conversationBean->assigned_user_id;
                }
            }

        }

        // Apply the final assigned user to the message
        if (!empty($assignedUserId)) {
            $this->assigned_user_id = $assignedUserId;
            // Ensure assigned_user_name is populated for notifications
            if (empty($this->assigned_user_name)) {
                $userBean = BeanFactory::getBean('Users', $assignedUserId);
                if (!empty($userBean) && !empty($userBean->name)) {
                    $this->assigned_user_name = $userBean->name;
                }
            }
        }

        // Conversation messages: set direction and sender
        if ($this->type === 'private_area') {
            if (empty($this->id) && empty($this->fetched_row['id'])) {
                if ($this->direction === 'inbound') {
                    $this->sender = 'sticpa';
                } else {
                    if (!empty($current_user->id)) {
                        $this->sender = $current_user->name;
                    }
                    $this->direction = 'outbound';
                }
            }
            $this->status = 'sent';
        }

        // For WhatsAppWeb messages, set sender to assigned user name
        if ($this->type === 'WhatsAppWeb') {
            $this->sender = $current_user->name;
        }

        // WhatsAppWeb messages are always sent immediately, never saved as draft
        if ($this->type === 'WhatsAppWeb' && $this->status === 'draft') {
            $this->status = 'sent';
        }

        // If Message is being created or status changed to "sent"
        if (($this->id === null && $this->status === 'sent') || ($this->status === 'sent' && $this->fetched_row['status'] !== 'sent')) {
            // If type is WhatsAppWeb we don't have a server-side sender: mark as sent and skip helper
            if ($this->type === 'WhatsAppWeb') {
                // mark as sent because user/client will open WhatsApp Web
                $response = array('code' => self::OK, 'message' => 'Sent via WhatsApp Web (client)');
                $this->status = 'sent';
                $this->response = $response['message'];
                $this->sent_date = $GLOBALS['timedate']->nowDb();
            } elseif ($this->type === 'private_area') {
                // Conversation type is handled internally, without external provider
                $this->status = 'sent';
                $this->response = 'Conversation message saved';
                $this->sent_date = $GLOBALS['timedate']->nowDb();
            } else {
            if (!empty($this->phone)){
                $response = $this->sendMessage();
                if ($response['code'] === self::OK) {
                    $this->status = 'sent';
                    $this->response = $response['message'] ?? '';
                    $this->sent_date = $GLOBALS['timedate']->nowDb();
                }
                else {
                    $this->status = 'error';
                    $this->response = $response['message'] ?? '';
                }
            }
            else {
                $this->status = 'error';
                $this->response = 'No phone number';
            }
            }
        }

        if ($this->type === 'private_area') {
            $this->parent_type = 'Contacts';

            // Store conversation subject on the bean for workflow notifications before save
            if (empty($this->stic_conversations_subject) && !empty($this->stic_conversations_ida)) {
                $convBean = BeanFactory::getBean('stic_Conversations', $this->stic_conversations_ida);
                if (!empty($convBean) && !empty($convBean->id) && !empty($convBean->subject)) {
                    $this->stic_conversations_subject = $convBean->subject;
                }
            }
        }
        // Save the bean
        parent::save($check_notify);

        // For conversation messages, ensure the M:M relationship is created in the join table
        if ($this->type === 'private_area' && !empty($this->stic_conversations_ida)) {
            $this->load_relationship('stic_conversations_stic_messages');
            if (!empty($this->stic_conversations_stic_messages)) {
                $this->stic_conversations_stic_messages->add($this->stic_conversations_ida);
            }

            // If conversation has no subject, use the message text as subject
            $conversationId = is_array($this->stic_conversations_ida) ? reset($this->stic_conversations_ida) : $this->stic_conversations_ida;
            if (!empty($conversationId)) {
                $convBean = BeanFactory::getBean('stic_Conversations', $conversationId);
                if (!empty($convBean) && !empty($convBean->id) && empty($convBean->subject)) {
                    $cleanSubject = trim(strip_tags((string)$this->message));
                    if ($cleanSubject !== '') {
                        $convBean->subject = mb_substr($cleanSubject, 0, 60);
                        $convBean->save();
                    }
                }
            }
        }

        return $this->id;
    }

    public function fillName($parentType = null, $parentId = null)
    {
        global $current_user, $timedate;

        // Allow send messages from no authenticated contexts as Signature Portal
        if (empty($current_user->id)) {
            // Get first admin active user
            $adminUser = BeanFactory::getBean('Users');
            $adminUser->retrieve_by_string_fields(array('is_admin' => 1, 'status' => 'Active'));
            $current_user = $adminUser;
        }

        $parentType = $parentType?? $this->parent_type;
        $parentId = $parentId ?? $this->parent_id;

        $relatedObjectName = '';
        if (!empty($parentType)){
            $relatedObject = BeanFactory::getBean($parentType, $parentId);
            $relatedObjectName = $relatedObject->name;
        }
        $templateName = '';
        if (!empty($this->template_id)){
            $template = BeanFactory::getBean('EmailTemplates', $this->template_id);
            $templateName = ' - ' . $template->name;
        }

        if (empty($this->date_entered)) {
            $this->date_entered = $GLOBALS['timedate']->nowDb();
        }

        $messageDateTime = $this->date_entered;
        if ($userDate = $timedate->fromUser($messageDateTime, $current_user)) {
            $messageDateTime = $userDate->asDb();
        }

        $date = SugarDateTime::createFromFormat(TimeDate::DB_DATETIME_FORMAT, $messageDateTime, new DateTimeZone("UTC"));

        // get user timezone
        $userPreferences = new UserPreference($current_user);
        $userPreferences->retrieve_by_string_fields(array('assigned_user_id' => $current_user->id));

        // Get the timezone from the user's preferences
        $timezone = $userPreferences->getPreference('timezone');
        if ($timezone === null) {
            require_once('include/TimeDate.php');
            $timezone =  TimeDate::guessTimezone();;
        }

        $date = $date->setTimezone(new DateTimeZone($timezone));
        $formatedDate = $date->format($timedate->get_date_time_format($current_user));


        $this->name = $relatedObjectName . ' - ' . $formatedDate . $templateName;
        return $this->name;
    }

    public function sendMessage() {

        // In the list stic_messages_type_list, the keypart is the name of the file containing the helper class.
        $messageHelper = null;
        $file = $this->type;
        if (file_exists('custom/modules/stic_Messages/Helpers/' . $file . '.php')) {
            require_once('custom/modules/stic_Messages/Helpers/' . $file . '.php');
            $messageHelper = new $file; 
        }
        else if (file_exists('modules/stic_Messages/Helpers/' . $file . '.php')) {
            require_once('modules/stic_Messages/Helpers/' . $file . '.php');
            $messageHelper = new $file; 
        }

        if ($messageHelper !== null) {
            $returnCode = $messageHelper->sendMessage($this->sender, $this->message, $this->phone);
        }
        else {
            $returnCode = self::ERROR_NO_HELPER_CLASS;
        }
        return $returnCode;

    }

    public static function replaceTemplateVariables($screenText, $bean)
    {
            $macro_nv = array();
    
            $focusName = $bean->module_name;
            $focus = $bean;
    
            /**
             * @var EmailTemplate $emailTemplate
             */
            $emailTemplate = BeanFactory::newBean('EmailTemplates');
            $templateData = $emailTemplate->parse_email_template(
                array(
                    'body' => $screenText,
                ),
                $focusName,
                $focus,
                $macro_nv
            );

            $emailTemplate = BeanFactory::newBean('EmailTemplates');
            if ($focusName === 'Leads') {
                $templateData = $emailTemplate->parse_email_template(
                    array(
                        'body' => $templateData['body'],
                    ),
                    'Contacts',
                    $focus,
                    $macro_nv
                );
    
            }
        return html_entity_decode($templateData['body'], ENT_QUOTES | ENT_HTML5, 'UTF-8');
    }
    

}