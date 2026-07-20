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

require_once "modules/stic_AWF_Forms/core/includes.php";
require_once "modules/stic_AWF_Deferred_Tickets/stic_AWF_Deferred_Tickets.php";
require_once "modules/stic_Web_Forms/Catcher/FormConfig.php";

/**
 * EntryPoint: stic_AWF_returnHandler
 * Handles the user return from an external platform (e.g. a payment gateway).
 *
 * When a deferred action (like PaymentRouterAction) redirects the user to an
 * external gateway, the gateway processes the payment and redirects the user
 * back to this handler via a token-based URL.
 *
 * This handler:
 *   1. Reads the token from the request
 *   2. Looks up the Deferred Ticket by token_hash
 *   3. If pending/processing: shows a waiting page
 *   4. If resolved/failed/cancelled: rebuilds the ExecutionContext
 *      from the ticket and executes the corresponding deferred flow
 *      (flow_success_id or flow_error_id) via stic_AWFUtils::rebuildContextAndResumeDeferredFlow(),
 *      letting the flow's actions decide what to render or where to redirect.
 */
class ReturnHandler
{
    public function run(): void
    {
        // Get Token
        $token = $_REQUEST['token'] ?? '';
        if (empty($token)) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid request: missing token");
            stic_AWFUtils::renderGenericResponseError(null);
            return;
        }

        // Get Ticket from token
        /** @var stic_AWF_Deferred_Tickets $ticket */
        $ticket = BeanFactory::getBean('stic_AWF_Deferred_Tickets');
        $ticket->retrieve_by_string_fields(['token_hash' => $token, 'deleted' => 0]);
        if (empty($ticket->id)) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid request: no ticket found for token");
            stic_AWFUtils::renderGenericResponseError(null);
            return;
        }
        $ticketStatus = $ticket->status ?? 'pending';

        // For pending/processing, show a waiting page (no flow to execute yet)
        if ($ticketStatus === 'pending' || $ticketStatus === 'processing') {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": ReturnHandler: Ticket is pending or processing");
            $title = translate('LBL_PROCESSING_TITLE', 'stic_AWF_Deferred_Tickets');
            $msg = translate('LBL_PROCESSING_MSG', 'stic_AWF_Deferred_Tickets');
            stic_AWFUtils::renderGenericResponse(null, $title, $msg);
            return;
        }

        // For resolved states, rebuild context and execute the deferred flow.
        // The flow (configured by the form designer via flow_success_id / flow_error_id)
        // decides what to do: redirect to a thank-you page, show a summary, send emails, etc.
        stic_AWFUtils::rebuildContextAndResumeDeferredFlow($ticket);
    }
}

$handler = new ReturnHandler();
$handler->run();
