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

class stic_RemittancesUtils
{
    /**
     * 1) Mark all payments related to the remittance as paid, by changing its status to "sent"
     * 2) Calculate the value of the paid_annualized_fee field of the payment commitments involved in the remittance.Changes them via SQL
     * @param Object $remittanceBean Then bean with remittance
     * @return void
     */
    public static function managePaymentsIfRemittanceIsSent($remittanceBean)
    {

        // In card payments remittances the payment status should not be globally updated as it will be managed on individual payment execution
        if ($remittanceBean->type == 'cards') {
            return;
        }

        // We will only change the payment status if the remittance changes status, not if any other field is modified
        if ($remittanceBean->status == 'sent' && $remittanceBean->fetched_row['status'] != $remittanceBean->status) {

            // We do not update payments that are already paid because in this way their date of last modification is at the first moment they were marked with that status.
            $GLOBALS['log']->debug(__METHOD__ . ": Payment status will be updated [{$remittanceBean->fetched_row['status']}] -> [{$remittanceBean->status}] ...");

            // We use the tables specified in the relationship so that the code is transparent to a change of tables if necessary
            global $current_user;

            $sql = "UPDATE
                        stic_payments sp
                    JOIN stic_payments_stic_remittances_c spsrc ON
                        sp.id = spsrc.stic_payments_stic_remittancesstic_payments_idb
                    SET
                        sp.status = 'paid',
                        sp.date_modified = DATE_FORMAT(UTC_TIMESTAMP(), '%Y-%m-%d %H:%i:%s'),
                        sp.modified_user_id = '{$current_user->id}'
                    WHERE
                        spsrc.deleted = 0
                        AND spsrc.stic_payments_stic_remittancesstic_remittances_ida = '{$remittanceBean->id}'
                        AND sp.status != 'paid'
                        AND sp.deleted = 0;";

            $GLOBALS['log']->debug(__METHOD__ . ": Payment status update SQL [{$sql}]");

            // Retrieve the database object
            $db = DBManagerFactory::getInstance();
            $GLOBALS['log']->debug(__METHOD__ . ": Updating payment status ...");
            $res = $db->query($sql);
            if ($res === false) {
                $GLOBALS['log']->error(__METHOD__ . ": An error occurred updating the status of payments linked to the remittance [{$remittanceBean->id} - {$remittanceBean->name}]");
            } else {
                $GLOBALS['log']->info(__METHOD__ . ": [" . $db->getAffectedRowCount($res) . "] payments linked to the remittance [{$remittanceBean->id} - {$remittanceBean->name}] have been updated to paid status.");
            }

            // Recalculate the 'paid_annualized_fee' for all Payment Commitments involved in a remittance status change.
            require_once 'modules/stic_Payment_Commitments/Utils.php';

            $GLOBALS['log']->debug(__METHOD__ . ": Started recalculation of 'paid_annualized_fee' for all Payment Commitments involved in remittance status change.");

            // create subquery statement
            $selectIdSubquery = "SELECT
                    spspcc.stic_paymebfe2itments_ida AS id
                FROM
                    stic_payments_stic_remittances_c spsrc
                JOIN stic_payments_stic_payment_commitments_c spspcc ON
                    spsrc.stic_payments_stic_remittancesstic_payments_idb = spspcc.stic_payments_stic_payment_commitmentsstic_payments_idb
                WHERE
                    spsrc.stic_payments_stic_remittancesstic_remittances_ida = '{$remittanceBean->id}'
                    AND spspcc.deleted = 0
                    AND spsrc.deleted = 0";
            // create update query
            $paidAnnualizedFeeSQL = "UPDATE stic_payment_commitments pc
            JOIN (
            SELECT rel.stic_paymebfe2itments_ida, SUM(sp.amount) AS total
                FROM stic_payments sp
                JOIN stic_payments_stic_payment_commitments_c rel
                    ON rel.stic_payments_stic_payment_commitmentsstic_payments_idb = sp.id
                WHERE sp.status = 'paid'
                    AND sp.deleted = 0
                    AND rel.deleted = 0
                    AND YEAR(sp.payment_date) = YEAR(CURDATE())
                    AND rel.stic_paymebfe2itments_ida IN ($selectIdSubquery)
                GROUP BY rel.stic_paymebfe2itments_ida
            ) pay_sum
            ON pc.id = pay_sum.stic_paymebfe2itments_ida
            SET pc.date_modified = DATE_FORMAT(UTC_TIMESTAMP(), '%Y-%m-%d %H:%i:%s'),
                pc.paid_annualized_fee = pay_sum.total
            WHERE pc.id IN ($selectIdSubquery) ;";

            $res2 = $db->query($paidAnnualizedFeeSQL);

            if ($res2 === false) {
                $GLOBALS['log']->error(__METHOD__ . ": An error occurred while updating the 'paid_annualized_fee' of Payment Commitments related to the remittance [{$remittanceBean->id} - {$remittanceBean->name}].");
            } else {
                $updatedRows2 = $db->getAffectedRowCount($res2);
                $GLOBALS['log']->info(__METHOD__ . ": Successfully updated 'paid_annualized_fee' for [{$updatedRows2}] payment commitment(s) related to the remittance [{$remittanceBean->id} - {$remittanceBean->name}].");
            }

        }

    }

/**
 * Returns the link to formatted edition to use
 *
 * @param String $module
 * @param String $record
 * @param String $text
 * @return String  The link to formatted edition to use
 */
    public static function goToEdit($module, $record, $text)
    {

        return '<i>' . $text . '</i> - <a target="_blank" href="index.php?module=' . $module . '&action=EditView&record=' . $record . '" ><b>' . translate('LBL_SEPA_FIX_REMITTANCE_ERROR') . '</b></a>';

    }

    /**
     * Populates the dynamic list used in the "issuing_organization" enum field
     * Organizations are defined in GENERAL Settings: 
     *    - GENERAL_ORGANIZATION_NAME: Default organization
     *    - GENERAL_ORGANIZATION_NAME_XXXX: Organization XXXX
     */
    public static function fillDynamicListForIssuingOrganizations()
    {
        include_once 'modules/stic_Settings/Utils.php';
        // Load GENERAL Settings
        $generalSettings = stic_SettingsUtils::getSettingsByType('GENERAL');

        // Default Organization name
        $dynamic_issuing_organization_list = array("" => $generalSettings['GENERAL_ORGANIZATION_NAME']);

        // Other Organization names
        foreach ($generalSettings as $key => $value) {
            if (strpos($key, 'GENERAL_ORGANIZATION_NAME_') === 0) {
                $organizationKey = str_replace('GENERAL_ORGANIZATION_NAME_', '', $key);
                $dynamic_issuing_organization_list[$organizationKey] = $value;
            }
        }

        // Set Organization names to dynamic list
        $GLOBALS['app_list_strings']['dynamic_issuing_organization_list'] = $dynamic_issuing_organization_list;
    }
}
