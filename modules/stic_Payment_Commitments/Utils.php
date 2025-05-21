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

class stic_Payment_CommitmentsUtils
{
    /**
     * Calculation of the annualized fee based on the amount and the periodicity of the payment commitment.
     *
     * @param Object Bean Payment Commitment bean
     * @return Int
     */
    public static function getAnnualizedFee($PCBean)
    {

        $multiplier = 1;

        switch ($PCBean->periodicity) {
            case "monthly":
                $multiplier = 12;
                break;

            case "bimonthly":
                $multiplier = 6;
                break;

            case "quarterly":
                $multiplier = 4;
                break;

            case "four_monthly":
                $multiplier = 3;
                break;

            case "half_yearly":
                $multiplier = 2;
                break;

            case "annual":
                $multiplier = 1;
                break;

            case "punctual":
                $multiplier = 1;
                break;
        }

        return $multiplier * (float)$PCBean->amount;
    }

    /**
     *  Creation of initial payments after a new payment commitment is created:
       *   1) A first payment matching first_payment_date (always)
       *   2) A payment for the following month (if applicable according to GENERAL_PAYMENT_GENERATION_MONTH)
       *   3) Previous payments, according to the periodicity (if first_payment_date is earlier than the current month)
     *
     * @param Object $PCBean Payment commitment bean
     * @return void
     */
    public static function createInitialPayments($PCBean)
    {

        global $db;
        include_once 'modules/stic_Payment_Commitments/Utils.php';
        include_once 'modules/stic_Settings/Utils.php';

        // Get the value of 'GENERAL_PAYMENT_GENERATION_MONTH' setting:
        // 0 means current month - 1 means next month
        $monthGenerationSetting = stic_SettingsUtils::getSetting('GENERAL_PAYMENT_GENERATION_MONTH') == 1 ? 1 : 0;

        // Set the number of months between payments according to the payment commitment periodicity
        $periodicity = $PCBean->periodicity;
        switch ($periodicity) {
            case 'monthly':
                $monthsEvery = '1';
                break;
            case 'bimonthly':
                $monthsEvery = '2';
                break;
            case 'quarterly':
                $monthsEvery = '3';
                break;
            case 'four_monthly':
                $monthsEvery = '4';
                break;
            case 'half_yearly':
                $monthsEvery = '6';
                break;
            case 'annual':
                $monthsEvery = '12';
                break;
            case 'punctual':
                $monthsEvery = '0';
                break;
            default:
                $GLOBALS['log']->error(__METHOD__ . ' line ' . __LINE__ . ' It is not possible to create the initial payments because the payment commitment has no periodicity.', $PCBean->name . " " . $PCBean->id);
                return;
                break;
        }

        // Set dates & date formats to use in payments
        $firstPaymentYearMonth = date('Y-m', strtotime($PCBean->first_payment_date));
        $firstPaymentDate = date('Y-m-d', strtotime($PCBean->first_payment_date));
        $currentYearMonth = date('Y-m');

        // Check if end date exists. If so, use it as deadline
        if ($PCBean->end_date == '') {
            $deadlineYearMonth = 'NULL';
        } else {
            $deadlineYearMonth = date('Y-m', strtotime($PCBean->end_date));
        }

        // Create initial payments according to specific conditions

        if ($monthGenerationSetting == '1' && $monthsEvery == '1' && $currentYearMonth == $firstPaymentYearMonth) {

            // In next month mode, for a payment commitment with monthly periodicity and first payment date in the current month
            // two payments must be created: one for the current month using first payment date and one for the next month.
            self::createPayment($PCBean, $firstPaymentDate, 0);
            self::createPayment($PCBean, 0, 1);
        } else if ($currentYearMonth > $firstPaymentYearMonth && $monthsEvery != '0') {

            // This is a periodical payment commitment with first payment date in the past.

            // Create the first payment
            self::createPayment($PCBean, $firstPaymentDate, 0);

            // Check month mode
            if ($monthGenerationSetting == '1') {
                // Create payments until next month
                $untilYearMonth = self::addMonths(1, $currentYearMonth);
            } else {
                // Create payments until this month
                $untilYearMonth = $currentYearMonth;
            }

            // Check if payment commitment end_date will affect payment creation
            if ($deadlineYearMonth != 'NULL' && $deadlineYearMonth < $untilYearMonth) {
                $untilYearMonth = $deadlineYearMonth;
            }

            // Create as many more payments as needed
            $nextYearMonth = self::addMonths($monthsEvery, $firstPaymentYearMonth);
            while ($nextYearMonth <= $untilYearMonth) {
                self::createPayment($PCBean, $nextYearMonth, 2);
                $nextYearMonth = self::addMonths($monthsEvery, $nextYearMonth);
            }
        } else {

            // In any other case only a single payment will be created
            self::createPayment($PCBean, $firstPaymentDate, 0);
        }
    }

    /**
     * Create a payment from a payment commitment
     *
     * @param ObjectBean $PCBean Payment Commitment Bean
     * @param String $paymentDate (will be 'Y-m-d', 'Y-m' or null depending on $creationMode)
     * @param Int $creationMode (0 - standard, 1 - next moth, 2 - recurring, so 'Y-m' date is provided)
     * @return void
     */
    public static function createPayment($PCBean, $paymentDate, $creationMode)
    {

        include_once 'SticInclude/Utils.php';

        // Build the payment date values that will be needed later
        if ($creationMode == '1') {
            // A payment for the next month will be created
            // The payment date (first day of next month) will be fully built from scratch
            $month = date('m') + 1;
            $year = date('Y');
            if ($month > '12') {
                $month = $month - 12;
                $year = $year + 1;
            }
            $month = sprintf('%02d', $month);
            $paymentDate = $year . '-' . $month . '-01';
            $formattedPaymentDate = $year . '-' . $month;
        } else if ($creationMode == '2') {
            // This is a past recurring payment, only YearMonth is provided
            // so Day part must be added
            $paymentDate = $paymentDate . '-01';
            $formattedPaymentDate = $paymentDate;
        } else {
            // Standard payment. Full date is provided as a param
            $paymentDate = $paymentDate;
            $formattedPaymentDate = $paymentDate;
        }

        // Create payment bean
        $paymentBean = BeanFactory::getBean('stic_Payments');

        // Override automatic user setting. This will allow to set specific users later. Useful for webforms record creation.
        $paymentBean->set_created_by = false;
        $paymentBean->update_modified_by = false;

        // Transfer data from the payment commitment to the payment
        $paymentBean->name = $PCBean->name . " - " . $formattedPaymentDate;
        $paymentBean->amount = format_number($PCBean->amount, 2, 2);
        $paymentBean->payment_method = $PCBean->payment_method;
        $paymentBean->payment_type = $PCBean->payment_type;
        $paymentBean->payment_date = $paymentDate;
        $paymentBean->segmentation = $PCBean->segmentation;
        $paymentBean->bank_account = $PCBean->bank_account;
        $paymentBean->transaction_type = $PCBean->transaction_type;
        $paymentBean->mandate = $PCBean->mandate;
        $paymentBean->banking_concept = $PCBean->banking_concept;
        $paymentBean->assigned_user_id = $PCBean->assigned_user_id;
        $paymentBean->created_by = $PCBean->created_by;
        $paymentBean->modified_user_id = $PCBean->modified_user_id;

        // The status will depend on the payment method
        // Direct debit -> Not remitted, Other -> Pending
        if ($PCBean->payment_method == 'direct_debit') {
            $paymentBean->status = 'not_remitted';
        } else {
            $paymentBean->status = 'pending';
        }

        // Set the relationship between payment & payment commitment
        $paymentBean->stic_paymebfe2itments_ida = $PCBean->id;

        // Save the payment
        $paymentBean->save(false);
    }

    /**
     * Add $number months to $date in format 'Y-m'
     *
     * @param Int $number
     * @param String $date ('Y-m')
     * @return String ('Y-m')
     */
    public static function addMonths($number, $date)
    {
        $month = date('m', strtotime($date));
        $year = date('Y', strtotime($date));
        $month = $month + $number;
        if ($month > '12') {
            $month = $month - 12;
            $year = $year + 1;
        }
        $month = sprintf('%02d', $month);
        $date_format = $year . '-' . $month;
        return ($date_format);
    }

    /**
     * Recalculates all future payments for active payment commitments using SQL.
     *
     * This function updates the expected payments detail and pending annualized fee for all active payment commitments
     * based on their payment periodicity, first payment date, and end date. It calculates the projected amounts for each
     * payment commitment and updates the relevant fields in the "stic_payment_commitments" table.
     *
     * The function constructs a SQL query that calculates the projection for each payment commitment based on the current
     * date and the commitment's payment periodicity. It then updates the corresponding records in the database with the new
     * projection values.
     *
     * @return void
     */
    public static function recalculateAllFuturePaymentsViaSQL()
    {
        global $db;
        $sql = "UPDATE stic_payment_commitments AS t1
        JOIN (
            SELECT
                id,
                GROUP_CONCAT(
                    CASE
                        WHEN month_counter < DATE_FORMAT(first_payment_date, '%Y%m') THEN 0
                        WHEN month_counter > DATE_FORMAT(end_date, '%Y%m') AND STR_TO_DATE(end_date, '%Y-%m-%d') IS NOT NULL THEN 0
                        WHEN (DATE_FORMAT(first_payment_date, '%Y%m') = month_counter) OR
                             (periodicity = 'monthly') OR
                             (periodicity = 'bimonthly' AND SUBSTRING(month_counter, 5) % 2 = MONTH(first_payment_date) % 2) OR
                             (periodicity = 'quarterly' AND SUBSTRING(month_counter, 5) % 3 = MONTH(first_payment_date) % 3) OR
                             (periodicity = 'four_monthly' AND SUBSTRING(month_counter, 5) % 4 = MONTH(first_payment_date) % 4) OR
                             (periodicity = 'half_yearly' AND SUBSTRING(month_counter, 5) % 6 = MONTH(first_payment_date) % 6) OR
                             (periodicity = 'annual' AND SUBSTRING(month_counter, 5) % 12 = MONTH(first_payment_date) % 12)
                        THEN amount
                        ELSE 0
                    END ORDER BY month_counter SEPARATOR '|'
                ) AS projection,
                SUM(
                    CASE
                        WHEN SUBSTRING(month_counter, 1, 4) = YEAR(NOW()) AND month_counter >= DATE_FORMAT(NOW(), '%Y%m') THEN
                            CASE
                                WHEN month_counter > DATE_FORMAT(end_date, '%Y%m') AND STR_TO_DATE(end_date, '%Y-%m-%d') IS NOT NULL THEN 0
                                WHEN (DATE_FORMAT(first_payment_date, '%Y%m') = month_counter) OR
                                     (periodicity = 'monthly') OR
                                     (periodicity = 'bimonthly' AND SUBSTRING(month_counter, 5) % 2 = MONTH(first_payment_date) % 2) OR
                                     (periodicity = 'quarterly' AND SUBSTRING(month_counter, 5) % 3 = MONTH(first_payment_date) % 3) OR
                                     (periodicity = 'four_monthly' AND SUBSTRING(month_counter, 5) % 4 = MONTH(first_payment_date) % 4) OR
                                     (periodicity = 'half_yearly' AND SUBSTRING(month_counter, 5) % 6 = MONTH(first_payment_date) % 6) OR
                                     (periodicity = 'annual' AND SUBSTRING(month_counter, 5) % 12 = MONTH(first_payment_date) % 12)
                                THEN amount
                                ELSE 0
                            END
                        ELSE 0
                    END
                ) AS this_year_projection
            FROM
                stic_payment_commitments
            CROSS JOIN
                (SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 1 MONTH), '%Y%m') AS month_counter UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 2 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 3 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 4 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 5 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 6 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 7 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 8 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 9 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 10 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 11 MONTH), '%Y%m') UNION ALL
                 SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 12 MONTH), '%Y%m')) as months
            WHERE
                active = 1
                AND deleted = 0
                AND periodicity != 'punctual'
            GROUP BY
                id
        ) AS t2 ON t1.id = t2.id
        SET t1.expected_payments_detail = t2.projection,
            t1.pending_annualized_fee = t2.this_year_projection;";
        $GLOBALS['log']->debug(__METHOD__ . ": Updating payment projection ...");
        $res = $db->query($sql);
        if ($res === false) {
            $GLOBALS['log']->error(__METHOD__ . ": An error occurred updating the payment projection [{$db->last_error}]");
        } else {
            $GLOBALS['log']->info(__METHOD__ . ": [" . $db->getAffectedRowCount($res) . "] payments linked to the remittance [{$remittanceBean->id} - {$remittanceBean->name}] have been updated to paid status.");
        }

    }

    /**
     * Recalculates the total paid amount for the current year in the stic_payment_commitments table
     * based on the related stic_payments data using SQL.
     *
     * This function updates the 'paid_annualized_fee' field of payment commitments for the current year
     * by joining stic_payment_commitments with aggregated payment data from stic_payments. It calculates
     * the total paid amount for each commitment and updates the 'paid_annualized_fee' accordingly.
     *
     * @return void
     */
    public static function recalculateCurrentYearTotalPaidViaSQL()
    {
        global $db;
        $sql = "UPDATE
            stic_payment_commitments pc
            JOIN (
                SELECT
                    rel.stic_paymebfe2itments_ida,
                    SUM(sp.amount) AS total
                FROM
                    stic_payments sp
                    JOIN stic_payments_stic_payment_commitments_c rel ON rel.stic_payments_stic_payment_commitmentsstic_payments_idb = sp.id
                WHERE
                    sp.status = 'paid'
                    AND sp.deleted = 0
                    AND rel.deleted = 0
                    AND YEAR(sp.payment_date) = YEAR(CURDATE())
                    AND rel.stic_paymebfe2itments_ida IN (
                        SELECT
                            id
                        FROM
                            stic_payment_commitments
                        where
                            deleted = 0
                            AND (
                                end_date IS NULL
                                OR YEAR(end_date) = YEAR(CURDATE())
                            )
                    )
                GROUP BY
                    rel.stic_paymebfe2itments_ida
            ) pay_sum ON pc.id = pay_sum.stic_paymebfe2itments_ida
        SET
            pc.date_modified = DATE_FORMAT(UTC_TIMESTAMP(), '%Y-%m-%d %H:%i:%s'),
            pc.paid_annualized_fee = pay_sum.total;";

        $GLOBALS['log']->debug(__METHOD__ . ": Updating current year total paid ...");
        $res = $db->query($sql);
        if ($res === false) {
            $GLOBALS['log']->error(__METHOD__ . ": An error occurred updating the current year total paid [{$db->last_error}]");
        } else {
            $GLOBALS['log']->info(__METHOD__ . ": [" . $db->getAffectedRowCount($res) . "] records updated.");
        }

    }

    /**
     * Update the paid annual fee of the payment commitment record using SQL.
     *
     * @param object $PCBean The payment commitment object to update.
     *
     * @return void
     */
    public static function setPaidAnnualizedFee($PCBean)
    {
        global $db;
        require_once 'SticInclude/Utils.php';

        $sql = "UPDATE stic_payment_commitments
                SET
                date_modified=UTC_TIMESTAMP(),
                paid_annualized_fee = (
                SELECT SUM(pay.amount) as total
                    FROM stic_payments pay
                    JOIN stic_payments_stic_payment_commitments_c paypc
                        ON paypc.stic_payments_stic_payment_commitmentsstic_payments_idb = pay.id
                    WHERE pay.status = 'paid'
                        AND pay.deleted = 0
                        AND paypc.deleted = 0
                        AND year(pay.payment_date) = year(CURDATE())
                        AND paypc.stic_paymebfe2itments_ida = '{$PCBean->id}'
                    GROUP BY paypc.stic_paymebfe2itments_ida
                )
                WHERE stic_payment_commitments.id = '{$PCBean->id}';";
        $res = $db->query($sql);
        if (!$res) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . " Error calculating the paid_annualized_fee field in the payment commitment: {$PCBean->id} [Error: {$db->last_error}]");
        }

    }

    /**
     * This function calculates the pending payments for a payment commitment based on its details.
     * @param SugarBean $PCBean PaymentCommitments Bean
     * @return array|string Returns an array with two values:
     *                     - expected_payments_detail: a string with a list of payments separated by "|"
     *                     - pending_annualized_fee: the total amount of payments in the current year
     */
    public static function getPendingPayments($PCBean)
    {
        global $db;

        // Extract relevant details from the PaymentCommitments Bean
        $amount = $PCBean->amount;
        $firstPaymentDate = $PCBean->first_payment_date;
        $endDate = $PCBean->end_date;
        $periodicity = $PCBean->periodicity;

        // This SQL does not use any specific table, but it has been found to be very effective in generating the string with the payment forecast
        // and thus we use the calculation mode that is used in the script that runs monthly for the set of records.
        $detailedSQL = "
        SELECT
            GROUP_CONCAT(
                    CASE
                            WHEN month_counter <  DATE_FORMAT('$firstPaymentDate', '%Y%m') THEN 0
                            WHEN month_counter >  DATE_FORMAT('$endDate', '%Y%m') AND STR_TO_DATE('$endDate', '%Y-%m-%d') IS NOT NULL THEN 0
                            WHEN (DATE_FORMAT('$firstPaymentDate', '%Y%m')  = month_counter) OR
                                ('$periodicity' = 'monthly') OR
                                ('$periodicity' = 'bimonthly' AND substr(month_counter,5) % 2 = MONTH('$firstPaymentDate') % 2) OR
                                ('$periodicity' = 'quarterly' AND substr(month_counter,5) % 3 = MONTH('$firstPaymentDate') % 3) OR
                                ('$periodicity' = 'four_monthly' AND substr(month_counter,5) % 4 = MONTH('$firstPaymentDate') % 4) OR
                                ('$periodicity' = 'half_yearly' AND substr(month_counter,5) % 6 = MONTH('$firstPaymentDate') % 6) OR
                                ('$periodicity' = 'annual' AND substr(month_counter,5) % 12 = MONTH('$firstPaymentDate') % 12)
                            THEN $amount
                            ELSE 0
                            END ORDER BY month_counter SEPARATOR '|')
                            AS projection
        FROM (
            SELECT DATE_FORMAT(DATE_ADD(NOW(), INTERVAL 1 + n.n MONTH), '%Y%m') AS month_counter
            FROM (
                SELECT 0 AS n UNION ALL
                SELECT 1 UNION ALL
                SELECT 2 UNION ALL
                SELECT 3 UNION ALL
                SELECT 4 UNION ALL
                SELECT 5 UNION ALL
                SELECT 6 UNION ALL
                SELECT 7 UNION ALL
                SELECT 8 UNION ALL
                SELECT 9 UNION ALL
                SELECT 10 UNION ALL
                SELECT 11
            ) AS n
        ) as months";

        // Get the detailed string containing projected payments
        $detailString = $db->getOne($detailedSQL);

        // Store the projected payments details in the result array
        $res['expected_payments_detail'] = $detailString;

        // Calculate the pending annualized fee for the current year
        $detailArray = explode('|', $detailString);
        $nextMonth = date('n');
        $thisYearArray = array_slice($detailArray, 0, count($detailArray) - $nextMonth);
        $thisYearTotal = 0;

        // Sum up the values corresponding to the current year
        foreach ($thisYearArray as $value) {
            $thisYearTotal += floatval($value);
        }

        // Store the pending annualized fee in the result array
        $res['pending_annualized_fee'] = $thisYearTotal;

        // Return the result array with payment details
        return $res;

    }

}
