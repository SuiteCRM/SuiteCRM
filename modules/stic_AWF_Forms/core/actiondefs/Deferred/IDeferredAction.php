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
 * Interface for all actions that can wait for an external event.
 * Examples: Payments, SMS Validation, Digital Signature, Manual Approval...
 */
interface IDeferredAction extends IServerAction {

    /**
     * Declares who will resume this deferred process and how.
     */
    public function getResumptionContext(): DeferredResumptionContext;

    /**
     * Processes an incoming request (webhook) from an external service.
     * 
     * This method is only relevant for actions that expect a server callback.
     * @param ExecutionContext $context The global context.
     * @param array $requestData The data of the incoming request.
     * @return ActionResult Result of the execution of the action.
     */
    public function processWebhook(ExecutionContext $context, array $requestData): ActionResult;

    /**
     * Returns the Subflow success label
     */
    public function getFlowSuccessLabel(): string;

    /**
     * Returns the Subflow error label
     */
    public function getFlowErrorLabel(): string;
}