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

class stic_AWF_PaypalStrategy extends stic_AWF_PaymentStrategy
{
    protected string $configType = 'PAYPAL'; 
    protected string $configKeyPrefix = 'PAYPAL';

    /**
     * Returns the webhook source identifier for this strategy.
     * Used by WebhookHandler to route incoming webhooks to the correct strategy.
     */
    public static function getSourceName(): string {
        return "Paypal";
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
        // TODO

        return null;
    }

    /**
     * Prepare payment for the current Strategy (Offline, RedSys, CECA...)
     */
    protected function initiateStrategy(ExecutionContext $context, FormAction $actionConfig, stic_Payments $beanPayment): ActionResult {
        // TODO

        $config = $this->getConfigValues(array('ID', 'ID_TEST', 'TEST'));
        $config['URL'] = 'https://www.paypal.com/cgi-bin/webscr';
        $config['URL_TEST'] = 'https://www.sandbox.paypal.com/cgi-bin/webscr';

        return new ActionResult(ResultStatus::WAIT, $actionConfig, "");
    }

    /**
    * Terminal: Execute the output (HTML form, Redirect header...).
    * Only called if initiate() has returned WAIT.
    */
    public function performTerminal(ExecutionContext $context, ActionResult $result): void {
        // TODO
    }

    /**
    * WEBHOOK: Process action when notification arrives from external event.
    * Can be called with or without a Deferred Ticket:
    * - With ticket: $context->deferredContext contains strategy_class, payment_id, etc.
    * - Without ticket: context is minimal; strategy handles recurring events directly.
    */ 
    public function processNotification(ExecutionContext $context, ActionResult $result): ActionResult {
        // TODO

        return new ActionResult(ResultStatus::SKIPPED, null, "Paypal: TODO webhook execution");
    }
}