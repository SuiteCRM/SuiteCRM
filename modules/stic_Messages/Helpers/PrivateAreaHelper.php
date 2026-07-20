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

require_once('modules/stic_Messages/Helpers/stic_MessagesHelper.php');

/**
 * Helper for conversation messages (private_area type).
 *
 * This type handles internal conversations without external providers.
 * Messages are always related to Contacts and use M:M relationships.
 */
class PrivateAreaHelper extends stic_MessagesHelper {

    /**
     * {@inheritdoc}
     */
    protected function getProviderName(): string {
        return 'private_area';
    }

    /**
     * {@inheritdoc}
     */
    protected function getRequiredSettings(): array {
        return [];
    }

    /**
     * {@inheritdoc}
     */
    protected function performApiCall(array $params): array {
        return $this->buildError('private_area does not use API calls');
    }

    /**
     * {@inheritdoc}
     */
    protected function getSpecificUIConfig(): array {
        return [
            'lockSender' => true,
            'fixedStatus' => 'sent',
            'canRetry' => false,
            'hideAttachment' => true,
            'allowedStatus' => ['sent'],
        ];
    }

    /**
     * {@inheritdoc}
     */
    public function shouldSkipApiCall(): bool {
        return true;
    }

    /**
     * {@inheritdoc}
     */
    public function prepareBeanBeforeSave(stic_Messages $bean): void {
        global $current_user;

        // Resolve assignee from related records
        $assignedUserId = $current_user->id ?? '';
        $contactId = '';
        $conversationBean = null;

        // If the message already points to a Contact, use it first
        if ($bean->parent_type === 'Contacts' && !empty($bean->parent_id)) {
            $contactId = $bean->parent_id;
        }

        // Otherwise, try to get the Contact from the related conversation
        if (empty($contactId) && !empty($bean->stic_conversations_ida)) {
            $conversationBean = BeanFactory::getBean('stic_Conversations', $bean->stic_conversations_ida);
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
        if (empty($bean->parent_id) && !empty($contactId)) {
            $bean->parent_id = $contactId;
        }

        // Use the conversation assigned user as fallback
        if (empty($assignedUserId) && !empty($bean->stic_conversations_ida)) {
            if (empty($conversationBean) || empty($conversationBean->id)) {
                $conversationBean = BeanFactory::getBean('stic_Conversations', $bean->stic_conversations_ida);
            }

            if (!empty($conversationBean) && !empty($conversationBean->id) && !empty($conversationBean->assigned_user_id)) {
                $assignedUserId = $conversationBean->assigned_user_id;
            }
        }

        // Apply the final assigned user to the message
        if (!empty($assignedUserId)) {
            $bean->assigned_user_id = $assignedUserId;
            // Ensure assigned_user_name is populated for notifications
            if (empty($bean->assigned_user_name)) {
                $userBean = BeanFactory::getBean('Users', $assignedUserId);
                if (!empty($userBean) && !empty($userBean->name)) {
                    $bean->assigned_user_name = $userBean->name;
                }
            }
        }

        // Set direction and sender for new messages
        if (empty($bean->id) && empty($bean->fetched_row['id'])) {
            if ($bean->direction === 'inbound') {
                $bean->sender = 'sticpa';
            } else {
                if (!empty($current_user->id)) {
                    $bean->sender = $current_user->name;
                }
                $bean->direction = 'outbound';
            }
        }

        $bean->status = 'sent';
    }

    /**
     * {@inheritdoc}
     */
    public function processSuccessfulSend(stic_Messages $bean): void {
        global $timedate;

        $bean->status = 'sent';
        $bean->response = 'Conversation message saved';
        $bean->sent_date = $timedate->nowDb();

        // Force parent_type to Contacts
        $bean->parent_type = 'Contacts';

        // Store conversation subject on the bean for workflow notifications before save
        if (empty($bean->stic_conversations_subject) && !empty($bean->stic_conversations_ida)) {
            $convBean = BeanFactory::getBean('stic_Conversations', $bean->stic_conversations_ida);
            if (!empty($convBean) && !empty($convBean->id) && !empty($convBean->subject)) {
                $bean->stic_conversations_subject = $convBean->subject;
            }
        }
    }

    /**
     * {@inheritdoc}
     */
    public function processAfterSave(stic_Messages $bean): void {
        // Ensure the M:M relationship is created in the join table
        if (!empty($bean->stic_conversations_ida)) {
            $bean->load_relationship('stic_conversations_stic_messages');
            if (!empty($bean->stic_conversations_stic_messages)) {
                $bean->stic_conversations_stic_messages->add($bean->stic_conversations_ida);
            }

            // If conversation has no subject, use the message text as subject
            $conversationId = is_array($bean->stic_conversations_ida) ? reset($bean->stic_conversations_ida) : $bean->stic_conversations_ida;
            if (!empty($conversationId)) {
                $convBean = BeanFactory::getBean('stic_Conversations', $conversationId);
                if (!empty($convBean) && !empty($convBean->id) && empty($convBean->subject)) {
                    $cleanSubject = trim(strip_tags((string)$bean->message));
                    $convBean->subject = mb_substr($cleanSubject, 0, 200);
                    $convBean->save();
                }
            }
        }
    }
}
