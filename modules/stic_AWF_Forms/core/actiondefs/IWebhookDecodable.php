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
 * IWebhookDecodable interface for actions that handle webhooks whose token must be decoded
 * The token decoding type is indicated by the Source url parameter. For example: source="stripe_payment"
 */
interface IWebhookDecodable {

    /**
     * Indicates whether the action knows how to handle the specified Source.
     * @param string $source The source url parameter
     * @return bool indicating if the action can handle the specified source
     */
    public function handlesSource(string $source): bool;

    /**
     * Asks the action to extract the Token from the raw payload.
     * Returns the hash of the Deferred_Ticket.
     * @param string $source The source url parameter
     * @param array $requestData the request data received (POST or GET)
     * @param string $rawPayload the body raw payload received
     * @param array $headers the headers received
     * @return string|null the hash of the Deferred_Ticket
     */
    public function extractTokenFromEvent(string $source, array $requestData, string $rawPayload, array $headers): ?string;

    /**
     * Processes an asynchronous incoming event that does not have an active session ticket (Orphan).
     * @param ExecutionContext $context Emergency context isolated
     * @param string $source The source identifier
     * @param array $rawData The request payload
     * @return ActionResult
     */
    public function processOrphanWebhook(ExecutionContext $context, string $source, array $rawData): ActionResult;
}