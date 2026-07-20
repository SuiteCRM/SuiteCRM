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
* Trait to abstract away the generation of return URLs
* for any deferred action in the AWF framework.
*/
trait DeferredActionHelperTrait {

    /**
     * Generates the URL for the ResumeHandler (User's browser return line).
     * For redirection platforms that do not use asynchronous webhooks.
     * @param stic_AWF_Deferred_Tickets $ticket the ticket of the response
     * @param string $status optional status
     * @return string The url for sequential return
     */
    public function getSequentialReturnUrl(stic_AWF_Deferred_Tickets $ticket, string $status = 'ok'): string {
        global $sugar_config;
        $siteUrl = rtrim($sugar_config['site_url'] ?? '', '/');
        
        return $siteUrl . '/index.php?entryPoint=stic_AWF_resumeHandler' .
               '&token=' . urlencode($ticket->token_hash) .
               '&status=' . urlencode($status);
    }

    /**
     * Generates the URL for the WebhookHandler (S2S asynchronous notifications or email clicks).
     * Requires the source identifier (source) and, optionally, the ticket.
     * @param string $source The source identifier
     * @param stic_AWF_Deferred_Tickets $ticket the ticket of the response
     * @param array $extraParams
     * @return string The url for asynchronous notifications
     */
    public function getAsyncCallbackUrl(string $source, ?stic_AWF_Deferred_Tickets $ticket = null, array $extraParams = []): string {
        global $sugar_config;
        $siteUrl = rtrim($sugar_config['site_url'] ?? '', '/');
        
        $url = $siteUrl . '/index.php?entryPoint=stic_AWF_webhookHandler' .
               '&source=' . urlencode($source);

        if ($ticket !== null) {
            $url .= '&token=' . urlencode($ticket->token_hash);
        }

        if (!empty($extraParams)) {
            $url .= '&' . http_build_query($extraParams);
        }

        return $url;
    }

    public function createDeferredTicket(ExecutionContext $context, FormAction $actionConfig, ?SugarBean $bean, array $customData = [], 
                                         string $ticketNamePrefix = 'AWF Deferred', int $defaultDays = 7): stic_AWF_Deferred_Tickets {
        
        /** @var stic_AWF_Deferred_Tickets $ticket */
        $ticket = BeanFactory::newBean('stic_AWF_Deferred_Tickets');
        $ticket->id = create_guid(); // Set Id for the ContextData
        $ticket->new_with_id = true;

        $ticket->name = $ticketNamePrefix . ': ' . date('Y-m-d H:i:s');
        $ticket->stic_awf_responses_id_c = $context->responseId;
        $ticket->token_hash = bin2hex(random_bytes(32));
        $ticket->status = 'pending';
        $ticket->handler_action_id = $actionConfig->id;

        // Set the expiration date
        $days = (int)$actionConfig->getResolvedParameter('expiration_days', $defaultDays);
        $ticket->expiration_date = date('Y-m-d H:i:s', strtotime("+{$days} days"));

        // Set the context data for the deferred flow
        $associatedBean = $bean ?? $context->responseBean;
        $contextData = DeferredContextData::createSnapshot(
            static::class, // The child class name
            $ticket, 
            $actionConfig, 
            $associatedBean, 
            $context, 
            $customData
        ); 
        $ticket->context_data = $contextData->toJson();

        return $ticket;
    }

}