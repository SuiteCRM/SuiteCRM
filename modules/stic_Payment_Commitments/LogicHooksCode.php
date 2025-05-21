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

class stic_Payment_CommitmentsLogicHooks
{

    public function before_save(&$bean, $event, $arguments)
    {
        global $app_list_strings;
        include_once 'SticInclude/Utils.php';
        include_once 'modules/stic_Payment_Commitments/Utils.php';

        // Create name for contact if empty
        if (!empty($bean->stic_payment_commitments_contactscontacts_ida)) {
            if (empty($bean->name)) {
                $relatedBean = SticUtils::getRelatedBeanObject($bean, 'stic_payment_commitments_contacts');
                $bean->name = $relatedBean->first_name . ' ' . $relatedBean->last_name . ' - ' . $app_list_strings['stic_payments_methods_list'][$bean->payment_method] . ' - ' . $bean->amount;
            }
        }

        // Create name for account if empty
        elseif (!empty($bean->stic_payment_commitments_accountsaccounts_ida)) {
            if (empty($bean->name)) {
                global $app_list_strings;
                $relatedBean = SticUtils::getRelatedBeanObject($bean, 'stic_payment_commitments_accounts');
                $bean->name = $relatedBean->name . ' - ' . $app_list_strings['stic_payments_methods_list'][$bean->payment_method] . ' - ' . $bean->amount;
            }
        }

        // Set pending_annualized_fee if any of the fields involved in the calculation changes, 
        // or if the payment commitment is active, and either expected_payments_detail 
        // or pending_annualized_fee fields are empty.
        if ($bean->fetched_row['first_payment_date'] ?? null != $bean->first_payment_date
            || $bean->fetched_row['end_date'] ?? null != $bean->end_date
            || $bean->fetched_row['amount'] ?? null != $bean->amount
            || $bean->fetched_row['periodicity'] ?? null != $bean->periodicity
            || ($bean->active == 1 && (empty($bean->expected_payments_detail) || empty($bean->pending_annualized_fee)))
        ) {
            $res = stic_Payment_CommitmentsUtils::getPendingPayments($bean);
            $bean->pending_annualized_fee = $res['pending_annualized_fee'];
            $bean->expected_payments_detail = $res['expected_payments_detail'];
        }

        // Set annualized_fee
        $bean->annualized_fee = stic_Payment_CommitmentsUtils::getAnnualizedFee($bean);

        // Generation of the mandate and the date of signature when appropriate in direct payment methods
        if ($bean->payment_method == 'direct_debit') {

            // Generate mandate if empty
            if (empty($bean->mandate)) {
                $bean->mandate = substr(mt_rand(100000000, 999999999), 0, 8);
            }

            // In case the account number or mandate has changed and there is an account number
            if (!empty($bean->bank_account) && ($bean->bank_account != ($bean->fetched_row['bank_account'] ?? null) || ($bean->mandate != $bean->fetched_row['mandate'] && !empty($bean->mandate)))) {
                // If mandate is empty or has not been modified by the user, we generate it
                if (empty($bean->mandate) || ($bean->mandate == ($bean->fetched_row['mandate'] ?? null))) {
                    $bean->mandate = substr(mt_rand(100000000, 999999999), 0, 8);
                }
                // The signature date is updated in all cases where the mandate or account number has changed
                $bean->signature_date = date("Y-m-d");
            }
        }

        // Set active/inactive status
        if (
            (empty($bean->first_payment_date) || $bean->first_payment_date <= date("Y-m-d"))
            && (empty($bean->end_date) || $bean->end_date > date("Y-m-d"))
        ) {
            $bean->active = true;
        } else {
            $bean->active = false;
        }

    }

    public function after_save(&$bean, $event, $arguments)
    {

        // Create initial payments, only if it is a new record (and not modified)
        if (empty($bean->fetched_row)) {
            stic_Payment_CommitmentsUtils::createInitialPayments($bean);
        }

    }

}
