<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

    if (!(ACLController::checkAccess('Opportunities', 'edit', true))) {
        ACLController::displayNoAccess();
        die;
    }

    global $app_list_strings;

    require_once('modules/AOS_Quotes/AOS_Quotes.php');
    require_once('modules/Opportunities/Opportunity.php');
    
    //Setting values in Quotes
    $quote = BeanFactory::newBean('AOS_Quotes');
    $quote->retrieve($_REQUEST['record']);

    //Setting Opportunity Values
    $opportunity = BeanFactory::newBean('Opportunities');
    $opportunity->name = $quote->name;
    $opportunity->assigned_user_id = $quote->assigned_user_id;
    $opportunity->amount = format_number($quote->total_amount);
    $opportunity->account_id = $quote->billing_account_id;
    $opportunity->currency_id = $quote->currency_id;
    $opportunity->sales_stage = 'Proposal/Price Quote';
    $opportunity->probability = $app_list_strings['sales_probability_dom']['Proposal/Price Quote'];
    $opportunity->lead_source = 'Self Generated';
    $opportunity->date_closed = $quote->expiration;

    $opportunity->save();

    //Setting opportunity quote relationship
    $quote->load_relationship('opportunities');
    $quote->opportunities->add($opportunity->id);
    ob_clean();
    header('Location: index.php?module=Opportunities&action=EditView&record='.$opportunity->id);
