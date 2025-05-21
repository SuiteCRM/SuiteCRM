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

class SepaReturns {
    
    public static $xmlReturnDate = '';
    
    
    /**
     * Get and process the remittances contained in a SEPA XML format returns file
     *
     ** @return void
     */
    public static function loadSEPAReturns() { 
        
        global $mod_strings;

        $error = false;

        if ($_FILES['file']['error'] > 0) { // Check if there is any problem with the file upload

            SugarApplication::appendErrorMessage($mod_strings['LBL_SEPA_RETURN_ERR_UPLOADING_FILE'] . $_FILES['file']['error']);
            $error = true;

        } else { // File upload is ok

            // Open the file
            $handle = @fopen($_FILES['file']['tmp_name'], 'r');

            if ($handle) {

                $xml = simplexml_load_file($_FILES['file']['tmp_name']);
                $xmlArray = json_decode(json_encode($xml), true);

                // Get file header date for later use as rejection_date for each returned payment
                self::$xmlReturnDate = substr($xmlArray['CstmrPmtStsRpt']['GrpHdr']['CreDtTm'], 0 , 10 );

                // Look for <OrgnlPmtInfAndSts> nodes (under <CstmrPmtStsRpt> nodes), which contain remittances data
                // The file might contain any number of <OrgnlPmtInfAndSts> nodes
                $remittancesNodes = 0;
                foreach ($xmlArray['CstmrPmtStsRpt'] as $key => $remittanceNode) {
                    if ($key == 'OrgnlPmtInfAndSts') {
                        // One single node can contain data from one or more remittances.
                        // If there is only one, the remittance data will be straight under <OrgnlPmtInfAndSts> node.
                        // If there is more than one, transforming XML structure to PHP array will make an extra level
                        // with numeric keys (0, 1, 2...) be inserted under <OrgnlPmtInfAndSts> node, since a PHP array, 
                        // unlike a XML structure, cannot have two elements with the same label on the same level.
                        if (array_key_exists('0', $remittanceNode)) {
                            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'A node with ' . count($remittanceNode) . ' remittances has been found in the file.');
                            foreach ($remittanceNode as $remittance) {
                                self::processReturnedRemittance($remittance);
                            }
                        } else {
                            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'A node with 1 remittance has been found in the file.');
                            self::processReturnedRemittance($remittanceNode);
                        }
                        $remittancesNodes++;
                    }
                }

                if ($remittancesNodes == 0) {
                    SugarApplication::appendErrorMessage($mod_strings['LBL_SEPA_RETURN_ERR_NO_RECEIPT']);
                    $error = true;
                }

            } else {
                SugarApplication::appendErrorMessage($mod_strings['LBL_SEPA_RETURN_ERR_OPENING_FILE']);
                $error = true;
            }

            // Close file
            fclose($handle);

            if (!$error) {
                SugarApplication::appendErrorMessage($mod_strings['LBL_SEPA_RETURN_FILE_OK']);
            }
        }
    }

    /**
     * Loops through the array of the returned remittances and determines whether it contains a single receipt
     * or an array of receipts for later processing.
     *
     * @param Array $remittance
     * @return void
     */
    public static function processReturnedRemittance($remittance) {

        // Look for <TxInfAndSts> nodes, which contain receipts data
        // The $remittance param might contain any number of <TxInfAndSts> nodes

        foreach ($remittance as $key => $receiptNode) {
            if ($key == 'TxInfAndSts') {
                // One single node can contain data from one or more receipts.
                // If there is only one, the receipt data will be straight under <TxInfAndSts> node.
                // If there is more than one, transforming XML structure to PHP array will make an extra level
                // with numeric keys (0, 1, 2...) be inserted under <TxInfAndSts> node, since a PHP array, 
                // unlike a XML structure, cannot have two elements with the same label on the same level.
                if (array_key_exists('0', $receiptNode)) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'The remittance contains ' . count($receiptNode) . ' receipts.');
                    foreach ($receiptNode as $receipt) {
                        self::processReturnedReceipt($receipt);
                    }
                } else {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . ' The remittance contains 1 receipt.');
                    self::processReturnedReceipt($receiptNode);
                }
            }
        }

    }

    /**
     * Process each returned individual receipt
     *
     * @param Array $receipt Individual array with return information
     * @return void
     */
    public static function processReturnedReceipt($receipt) {

        global $mod_strings;

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Entering the receipt...');

        $returnDate = self::$xmlReturnDate;

        // Recover the data of the returned receipt
        $receiptId = $receipt['OrgnlEndToEndId'];
        $GLOBALS['log']->debug('OrgnlEndToEndId: ' . $receiptId);
        $returnCode = $receipt['StsRsnInf']['Rsn']['Cd'] ?? null;
        $GLOBALS['log']->debug('Cd: ' . $returnCode);
        $amount = $receipt['OrgnlTxRef']['Amt']['InstdAmt'] ?? null;
        $GLOBALS['log']->debug('Amount: ' . $amount);
        $debtor = $receipt['OrgnlTxRef']['Dbtr']['Nm'] ?? null;
        $GLOBALS['log']->debug('Debtor: ' . $debtor);

        // Set receiptId not case-sensitive
        $receiptId = strtolower($receiptId);

        // Rebuild the payment id according to the CRM format
        $paymentId = substr($receiptId, 0, 8);
        $paymentId .= "-";
        $paymentId .= substr($receiptId, 8, 4);
        $paymentId .= "-";
        $paymentId .= substr($receiptId, 12, 4);
        $paymentId .= "-";
        $paymentId .= substr($receiptId, 16, 4);
        $paymentId .= "-";
        $paymentId .= substr($receiptId, 20, 12);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Transaction code: ' . $paymentId);

        // Load the returned payment from the id in the XML file
        $paymentBean = new stic_Payments();
        $paymentBean->retrieve($paymentId);
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Payment Name: ' . $paymentBean->name);

        // If the payment exists in the CRM...
        if ($paymentBean->id == $paymentId) {

            // Mark it as unpaid
            $paymentBean->status = "unpaid";

            // Write down the code and the return date
            $paymentBean->sepa_rejected_reason = $returnCode;
            $paymentBean->rejection_date = $returnDate;

            $paymentBean->save();

            // Generate a link to the payment to display it as the result of the process
            SugarApplication::appendErrorMessage($mod_strings['LBL_SEPA_RETURN_UNPAID_PAYMENT'] . '<a href="index.php?module=stic_Payments&action=DetailView&record=' . $paymentBean->id . '">' . $paymentBean->name . '</a>');

        } else {

            SugarApplication::appendErrorMessage($mod_strings['LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_1'] . $paymentId . $mod_strings['LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_2'] . $debtor . $mod_strings['LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_3'] . $amount . $mod_strings['LBL_SEPA_RETURN_PAYMENT_NOT_FOUND_4'] . $returnDate . ')');
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Leaving the receipt...');

    }

}
