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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Defines the context from which a Deferred Action will resume.
 * This determines what type of child actions (sub-flows) will be allowed to be nested.
 */
enum DeferredResumptionContext: string {
    /**
     * The flow resumes in the background via an S2S (Webhook) call. No human user is present. 
     * It does NOT support child terminal actions.
     */
    case SERVER_WEBHOOK = 'server_webhook'; 

    /**
     * The workflow is restarted by a different person than the one who filled out the form (e.g., CRM Administrator). 
     * It does NOT support endpoint actions intended for the original user.
     */
    case THIRD_PARTY_HUMAN = 'third_party_human';

    /**
     * The workflow resumes synchronously with the user who filled out the form.
     * It supports all types of actions (Terminal and other Deferred).
     */
    case ORIGINAL_USER = 'original_user';
}

/**
* Data Transfer Object to manage the execution context of a Deferred Ticket.
* Centralizes serialization/deserialization avoiding arrays with hardcoded keys.
*/
class DeferredContextData 
{
    public string $formId;
    public string $actionClass;
    public string $actionText;
    public string $ticketId;
    public ?string $flowSuccessId;
    public ?string $flowErrorId;
    public ?string $beanId;
    public ?string $module;
    public array $blockReferences = [];
    public ?string $alreadyProcessedTitle = null;
    public ?string $alreadyProcessedMessage = null;
    public ?string $expiredTitle = null;
    public ?string $expiredMessage = null;
    
    /** @var array Additional data specific to the action (ex: email, stripe_session...) */
    public array $customData = [];

    public function __construct(
        string $formId, 
        string $actionClass, 
        string $actionText, 
        string $ticketId, 
        ?string $flowSuccessId = null, 
        ?string $flowErrorId = null, 
        ?string $beanId = null, 
        ?string $module = null
    ) {
        $this->formId = $formId;
        $this->actionClass = $actionClass;
        $this->actionText = $actionText;
        $this->ticketId = $ticketId;
        $this->flowSuccessId = $flowSuccessId;
        $this->flowErrorId = $flowErrorId;
        $this->beanId = $beanId;
        $this->module = $module;
    }

    public function captureBlockReferences(ExecutionContext $context): void {
        $this->blockReferences = [];
        foreach ($context->formConfig->data_blocks as $bId => $b) {
            if ($b->getBeanReference() !== null) {
                $this->blockReferences[$bId] = $b->getBeanReference()->beanId;
            }
        }
    }

    /**
    * Adds custom data to the context.
    */
    public function setCustom(string $key, mixed $value): void {
        $this->customData[$key] = $value;
    }

    /**
     * Retrieve a custom data.
     */
    public function getCustom(string $key, mixed $default = null): mixed {
        return $this->customData[$key] ?? $default;
    }

    /**
     * Serialize the object to save it to the database (JSON).
     */
    public function toJson(): string {
        $data = [
            'form_id' => $this->formId,
            'action_class' => $this->actionClass,
            'action_text' => $this->actionText,
            'ticket_id' => $this->ticketId,
            'flow_success_id' => $this->flowSuccessId,
            'flow_error_id' => $this->flowErrorId,
            'bean_id' => $this->beanId,
            'module' => $this->module,
            'block_references' => $this->blockReferences,
            'already_processed_title' => $this->alreadyProcessedTitle,
            'already_processed_message' => $this->alreadyProcessedMessage,
            'expired_title' => $this->expiredTitle,
            'expired_message' => $this->expiredMessage,
        ];
        // Merge the fixed data with the custom data (the fixed data takes precedence in case of collision)
        return json_encode(array_merge($this->customData, $data), JSON_UNESCAPED_UNICODE);
    }

    /**
     * Constructs the object from the JSON stored in the database.
     */
    public static function fromJson(string $json): self {
        $cleanJson = html_entity_decode($json, ENT_QUOTES, 'UTF-8');
        $data = json_decode($cleanJson, true) ?: [];
        
        $instance = new self(
            $data['form_id'] ?? '',
            $data['action_class'] ?? '',
            $data['action_text'] ?? '',
            $data['ticket_id'] ?? '',
            $data['flow_success_id'] ?? null,
            $data['flow_error_id'] ?? null,
            $data['bean_id'] ?? null,
            $data['module'] ?? null,
        );
        $instance->blockReferences = $data['block_references'] ?? [];

        $instance->alreadyProcessedTitle = $data['already_processed_title'] ?? null;
        $instance->alreadyProcessedMessage = $data['already_processed_message'] ?? null;
        $instance->expiredTitle = $data['expired_title'] ?? null;
        $instance->expiredMessage = $data['expired_message'] ?? null;

        // Remove the fixed keys to save only the custom ones
        unset($data['form_id'], 
              $data['action_class'], 
              $data['action_text'], 
              $data['ticket_id'], 
              $data['flow_success_id'], 
              $data['flow_error_id'], 
              $data['bean_id'], 
              $data['module'], 
              $data['block_references'],
              $data['already_processed_title'],
              $data['already_processed_message'],
              $data['expired_title'],
              $data['expired_message']);
        $instance->customData = $data;

        return $instance;
    }

    /**
     * Export to array for compatibility with ExecutionContext.
     */
    public function toArray(): array {
        $data = [
            'form_id' => $this->formId,
            'action_class' => $this->actionClass,
            'action_text' => $this->actionText,
            'ticket_id' => $this->ticketId,
            'flow_success_id' => $this->flowSuccessId,
            'flow_error_id' => $this->flowErrorId,
            'bean_id' => $this->beanId,
            'module' => $this->module,
            'block_references' => $this->blockReferences,
            'already_processed_title' => $this->alreadyProcessedTitle,
            'already_processed_message' => $this->alreadyProcessedMessage,
            'expired_title' => $this->expiredTitle,
            'expired_message' => $this->expiredMessage,
        ];
        return array_merge($this->customData, $data);
    }

    public static function createSnapshot(string $actionClass, stic_AWF_Deferred_Tickets $ticket, FormAction $actionConfig, ?SugarBean $bean, ExecutionContext $context, array $customData): self {
        $bean = $bean ?? $context->responseBean;
        $instance = new self(
            $context->formId,
            $actionClass,
            $actionConfig->text ?? $actionConfig->name,
            $ticket->id,
            $actionConfig->flow_success_id ?: null,
            $actionConfig->flow_error_id ?: null,
            $bean?->id,
            $bean?->module_dir ?? null
        );
        $instance->customData = $customData;
        $instance->captureBlockReferences($context);
        
        $instance->alreadyProcessedTitle = $actionConfig->getResolvedParameter('already_processed_title');
        $instance->alreadyProcessedMessage = $actionConfig->getResolvedParameter('already_processed_message');

        $instance->expiredTitle = $actionConfig->getResolvedParameter('expired_title');
        $instance->expiredMessage = $actionConfig->getResolvedParameter('expired_message');

        return $instance;
    }
}