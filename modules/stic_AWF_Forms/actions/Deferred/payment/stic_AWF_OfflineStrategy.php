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

include_once __DIR__."/stic_AWF_PaymentStrategy.php";
require_once "modules/stic_Payments/stic_Payments.php";

/**
 * Offline payment strategy.
 * Used for bank transfers, cash and other payment methods that do not require
 * redirection to an external gateway. Returns OK immediately so the flow
 * continues without waiting for an external webhook.
 */
class stic_AWF_OfflineStrategy extends stic_AWF_PaymentStrategy
{
    /**
     * Returns the webhook source identifier for this strategy.
     * Used by WebhookHandler to route incoming webhooks to the correct strategy.
     */
    public static function getSourceName(): string { 
        return 'offline';
    }

    /**
     * Extracts the external transaction ID from the raw webhook request data.
     * Each gateway sends the ID in a different location/format.
     *
     * @param array $rawData POST data array
     * @param string $rawBody Raw request body (for JSON-based gateways)
     * @return string|null The external transaction ID or null if not found
     */
    public static function extractExternalId(array $rawData, string $rawBody , array $headers): ?string {
        return null;
    }

    /**
     * Initiates an offline payment: sets the payment status to 'pending' and
     * returns OK so PaymentRouterAction triggers the Deferred OK flow immediately.
     */
    protected function initiateStrategy(ExecutionContext $context, FormAction $actionConfig, stic_Payments $beanPayment): ActionResult {
        // Offline payments (bank transfer, cash, direct debit) keep their default status
        // (not_remitted as set by the Payment Commitment hook on creation).
        // No gateway interaction needed; return OK immediately so the flow continues.
        return new ActionResult(ResultStatus::OK, $actionConfig, 'Offline payment registered');
    }

    /**
     * No-op: Offline payments never reach the terminal step.
     */
    public function performTerminal(ExecutionContext $context, ActionResult $result): void {
        // Offline payments return OK immediately, so this is never called.
    }

    /**
     * No-op: Offline payments do not receive webhooks.
     */
    public function processNotification(ExecutionContext $context, ActionResult $result): ActionResult {
        // Offline payments return OK immediately, so this is never called.
        return new ActionResult(ResultStatus::OK, null, 'Offline: no webhook expected');
    }
}
