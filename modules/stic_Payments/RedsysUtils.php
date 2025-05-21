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
class RedsysUtils
{

    /**
     * Process a single card recurring payment
     *
     * @param String $paymentBean Id
     * @param Boolean $debugMode If true shows additional info about Redsys TPV Request & Response (only when is called individually). If user is not admin, is automatically disabled.
     * @return Array 'res' true if payment is successful, false if not. 'resCode' result information message
     *
     */
    public static function runRecurringCardPayment($paymentId, $debugMode = false)
    {
        require_once 'modules/stic_Web_Forms/Catcher/Include/Payment/lib/ApiRedsys.php';
        require 'modules/stic_Web_Forms/Catcher/Include/Payment/lib/RedsysResponseCodes.php';
        require_once 'SticInclude/Utils.php';
        require_once 'modules/stic_Settings/Utils.php';

        // If user is not admin, disable $debugMode
        global $current_user, $mod_strings;
        $debugMode = $current_user->isAdmin() ? $debugMode : false;

        // Retrieve payment bean
        $paymentBean = BeanFactory::getBean('stic_Payments', $paymentId);

        // Retrieve TPV settings
        // Although won't be usual, it could happen that in the same remittance there were payments related to different POS/TPV,
        // so this is why their settings must be retrieved individually.
        $tpvSettings = stic_SettingsUtils::getTPVSettings($paymentBean->payment_method);

        // Choose appropriate password according to TPV mode
        $currentTpvPassword = $tpvSettings['TPV_TEST'] == 0 ? $tpvSettings['TPV_PASSWORD'] : $tpvSettings['TPV_PASSWORD_TEST'];

        if ($debugMode) {
            $debugMsg = '<div class="row">';
        }

        // If payment is already paid don't proccess it again and exit
        if ($paymentBean->status == 'paid') {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Omitting the payment {$paymentBean->name} because it is already paid.");
            return array(
                'res' => false,
                'resCode' => translate('LBL_CARD_PAYMENT_ALREADY_PAID', 'stic_Payments'),
            );
        }

        // Get payment commitment bean
        $PCBean = SticUtils::getRelatedBeanObject($paymentBean, 'stic_payments_stic_payment_commitments');

        // Create Redsys object
        $tpvSys = new RedsysAPI();
        $tpvSys->setParameter('DS_MERCHANT_IDENTIFIER', $PCBean->redsys_ds_merchant_identifier);
        $tpvSys->setParameter('DS_MERCHANT_COF_INI', 'N');
        $tpvSys->setParameter('DS_MERCHANT_COF_TXNID', $PCBean->redsys_ds_merchant_cof_txnid);
        $tpvSys->setParameter('DS_MERCHANT_MERCHANTCODE', $tpvSettings['TPV_MERCHANT_CODE']);
        $tpvSys->setParameter('DS_MERCHANT_TRANSACTIONTYPE', '0');
        $tpvSys->setParameter('DS_MERCHANT_EXCEP_SCA', 'MIT');
        $tpvSys->setParameter('DS_MERCHANT_DIRECTPAYMENT', 'true');
        $tpvSys->setParameter('DS_MERCHANT_ORDER', str_pad($paymentBean->transaction_code, 12, '0', STR_PAD_LEFT));
        $tpvSys->setParameter('DS_MERCHANT_TERMINAL', $tpvSettings['TPV_TERMINAL']);
        $tpvSys->setParameter('DS_MERCHANT_CURRENCY', $tpvSettings['TPV_CURRENCY']);
        $tpvSys->setParameter('DS_MERCHANT_AMOUNT', round($paymentBean->amount * 100, 0));

        // Choose appropriate TPV URL according to TPV mode
        $tpvUrl = $tpvSettings['TPV_TEST'] == 0 ? 'https://sis.redsys.es/sis/rest/trataPeticionREST' : 'https://sis-t.redsys.es:25443/sis/rest/trataPeticionREST';

        if ($debugMode) {
            $debugMsg .= '<div class="col-md-6"><b>PETITION</b><pre>' . print_r($tpvSys, true) . '</pre></div>';
        }

        // Prepare JSON object
        $dataToSend = array(
            'Ds_MerchantParameters' => $tpvSys->createMerchantParameters(),
            'Ds_SignatureVersion' => 'HMAC_SHA256_V1',
            'Ds_Signature' => str_replace('+', '%2B', $tpvSys->createMerchantSignature($currentTpvPassword)), // Replace "+" with "%2B" to avoid Redsys error/bug
        );

        if ($debugMode) {
            $debugMsg .= '<div class="col-md-6"><b>PETITION</b><pre>' . print_r($dataToSend, true) . '</pre></div>';
        }

        // Prepare CURL data
        $fullURL = "{$tpvUrl}?Ds_MerchantParameters={$dataToSend['Ds_MerchantParameters']}&Ds_SignatureVersion={$dataToSend['Ds_SignatureVersion']}&Ds_Signature={$dataToSend['Ds_Signature']}";

        $curl = curl_init();

        curl_setopt_array($curl, array(
            CURLOPT_URL => $fullURL,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_ENCODING => '',
            CURLOPT_MAXREDIRS => 10,
            CURLOPT_TIMEOUT => 0,
            CURLOPT_FOLLOWLOCATION => true,
            CURLOPT_HTTP_VERSION => CURL_HTTP_VERSION_1_1,
            CURLOPT_CUSTOMREQUEST => 'POST',
        ));

        // Send payment to TPV
        $redsysResponse = curl_exec($curl);

        curl_close($curl);

        // Decode response
        $response = json_decode($redsysResponse, true);

        // Proccess decoded response
        if (isset($response['errorCode'])) {
            $gatewayErrorText = "[{$response['errorCode']}] " . $redsysResponseCode['9' . substr($response['errorCode'], -3)];
            $paymentBean->status = 'rejected_gateway';
            $paymentBean->gateway_rejection_reason = $gatewayErrorText;
            $paymentBean->gateway_log .= '####### '.print_r($response, true);
            $paymentBean->save();

            if ($debugMode) {
                $debugMsg .= '<div class="col-md-6"><b>RESPONSE</b><pre>' . print_r($response, true) . $redsysResponseCode['9' . substr($response['errorCode'], -3)] . '</pre></div></div>';
                SugarApplication::appendErrorMessage($debugMsg);
            }
            
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": The Redsys payment request for {$paymentBean->name} has not been processed. Response code: [{$gatewayErrorText}");

            // If errorCode is that the card is expired (SIS0071), send a notification message to the assigned user of the payment commitment
            if ($response['errorCode'] == 'SIS0071') {
                // Retrieve payment commitment and set end_date and description info (only first try)
                if (empty($PCBean->end_date)) {
                    $PCBean = SticUtils::getRelatedBeanObject($paymentBean, 'stic_payments_stic_payment_commitments');
                    $PCBean->end_date = date('Y-m-d');
                    $PCBean->description .= "
                    ########## " . date('Y-m-d H:i') . " {$mod_strings['LBL_CARD_PAYMENT_COMMITMENT_EXPIRED']} ({$PCBean->card_expiry_date}).";
                    $PCBean->save();
                    // Send notification
                    self::sendExpiredCardNotifyEmail($paymentBean, $PCBean);
                }
            }

            return array(
                'res' => false,
                'resCode' => $gatewayErrorText,
                'id' => $paymentBean->id,
                'name' => $paymentBean->name
            );

        } else {
            // Decode Ds_MerchantParameters using Redsys API
            $response['Ds_MerchantParameters'] = json_decode($tpvSys->decodeBase64($response['Ds_MerchantParameters']), true);

            // If the payment has been accepted (Ds_Response = '0000')
            if ($response['Ds_MerchantParameters']['Ds_Response'] == '0000') {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": The Redsys payment request for {$paymentBean->name} has been successfully processed. Response code: [{$gatewayErrorText}");
                $paymentBean->status = 'paid';
                $paymentBean->gateway_log .= '####### '.print_r($response, true);
                $paymentBean->save();
 
                if ($debugMode) {
                    $debugMsg .= '<div class="col-md-6"><b>RESPONSE</b><pre>' . print_r($response, true) . $redsysResponseCode['9' . substr($response['errorCode'], -3)] . '</pre></div></div>';
                    SugarApplication::appendErrorMessage($debugMsg);
                }

                return array(
                    'res' => true,
                    'resCode' => $mod_strings['LBL_CARD_PAYMENT_SUCCESSFUL'],
                );

            } else {
                // If Ds_Response exists but is not '0000'
                $gatewayResponseText = "[{$response['Ds_MerchantParameters']['Ds_Response']}] " . $redsysResponseCode['9' . substr($response['Ds_MerchantParameters']['Ds_Response'], -3)];

                $paymentBean->status = 'rejected_gateway';
                $paymentBean->save();

                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": The Redsys payment request for {$paymentBean->name} has been processed unsuccessfully. Error code: [{$gatewayErrorText}");

                if ($debugMode) {
                    $debugMsg .= '<div class="col-md-6"><b>RESPONSE</b><pre>' . print_r($response, true) . $redsysResponseCode['9' . substr($response['errorCode'], -3)] . '</pre></div></div>';
                    SugarApplication::appendErrorMessage($debugMsg);
                }

                return array(
                    'res' => false,
                    'resCode' => $gatewayResponseText,
                );

            }
        }
    }

    /**
     * Send email expiry card notification to the payment commitment assigned user
     *
     * @param [type] $paymentBean
     * @return void
     */
    private static function sendExpiredCardNotifyEmail($paymentBean, $PCBean)
    {

        global $mod_strings, $sugar_config, $app_list_strings;

        $usersBean = BeanFactory::getBean('Users', $PCBean->assigned_user_id);
        $PCUrl = rtrim($sugar_config['site_url'], '/') . "/index.php?module=stic_Payment_Commitments&action=DetailView&record={$PCBean->id}";
        $paymentUrl = rtrim($sugar_config['site_url'], '/') . "/index.php?module=stic_Payments&action=DetailView&record={$paymentBean->id}";

        require_once 'include/SugarPHPMailer.php';
        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();
        // require_once 'modules/Emails/Email.php';
        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();
        $mail->From = $defaults['email'];
        $mail->FromName = $defaults['name'];
        $mail->AddBCC($usersBean->email1);
        $mail->Subject = $mod_strings['LBL_CARD_PAYMENT_COMMITMENT_EXPIRED_TITLE'];

        $bodyContent = "<h1>{$mod_strings['LBL_CARD_PAYMENT_COMMITMENT_EXPIRED_TITLE']}</h1>";
        $bodyContent .= "<h2>{$mod_strings['LBL_CARD_PAYMENT_COMMITMENT_EXPIRED_SUBTITLE']}</h2>";
        $bodyContent .= "<ul>";
        $bodyContent .= "<li>{$app_list_strings['moduleListSingular']['stic_Payment_Commitments']}: <a href='{$PCUrl}'>{$PCBean->name}</a>";
        $bodyContent .= "<li>{$app_list_strings['moduleListSingular']['stic_Payments']}: <a href='{$paymentUrl}'>{$paymentBean->name}</a>";
        $completeHTML = "
        <html lang=\"{$GLOBALS['current_language']}\">
            <head>
                <title>{$mail->Subject}</title>
            </head>
            <body style=\"font-family: Arial\">
            {$bodyContent}
            </body>
        </html>";

        $mail->Body = $completeHTML;

        $mail->IsHTML(true);
        $mail->prepForOutbound();

        // Send the message
        if (!$mail->Send()) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error sending notification email.");
        }

    }
}
