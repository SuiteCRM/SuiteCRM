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

require_once __DIR__ . '/PaymentBO.php';
require_once __DIR__ . '/../../WebFormDataController.php';

/**
 * Class that controls the flow for donation forms
 */
class PaymentController extends WebFormDataController {
    public const RESPONSE_TYPE_NEW_PAYMENT = '';
    public const RESPONSE_TYPE_TPV_RESPONSE = 'TPV_RESPONSE';
    public const RESPONSE_TYPE_TPVCECA_RESPONSE = 'TPV_CECA_RESPONSE';
    public const RESPONSE_TYPE_PAYPAL_RESPONSE = 'PAYPAL_RESPONSE';
    public const RESPONSE_TYPE_STRIPE_RESPONSE = 'STRIPE_RESPONSE';

    protected $version = 1; // Use the same logic for forms generated with different versions of the wizard

    /**
     * Controller Builder
     */
    public function __construct($version = 1) {
        parent::__construct();
        $this->bo = new PaymentBO();
        $this->version = $version;
    }

    /**
     * Overloading the parent method
     * Execute the operations required to handle the operation.
     * Your call will be preceded by a call to:
     * - getActionDefParams,
     * - checkActionDefParams,
     * - getDefParams,
     * - checkDefParams,
     * - getParams
     * - checkParams
     * @return Array an array of response type
     */
    protected function doAction() {
        $response = null;
        $actionType = $this->bo->getParam('type');
        switch ($actionType) {
            case self::RESPONSE_TYPE_NEW_PAYMENT: // Start a new payment
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Managing of a new payment...");
                $response = $this->actionNewPayment();
                break;
            case self::RESPONSE_TYPE_TPV_RESPONSE:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Managing the response of a POS payment...");
                $response = $this->actionTPVResponse();
                break;
            case self::RESPONSE_TYPE_TPVCECA_RESPONSE:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Managing the response of a POS CECA payment...");
                $response = $this->actionCECAResponse();
                break;
            case self::RESPONSE_TYPE_PAYPAL_RESPONSE:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Managing the response of a Paypal payment...");
                $response = $this->actionPaypalResponse();
                break;
            case self::RESPONSE_TYPE_STRIPE_RESPONSE:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Managing the response of a Stripe payment...");
                $response = $this->actionStripeResponse();
                break;
            default:
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Type of request [{$actionType}] not valid.");
                $this->returnCode('UNEXPECTED_ERROR');
                $response = $this->feedBackError($this);
                break;
        }
        return $response;
    }

    /**
     * Parent method overload
     * Retrieve form definition parameter and populate with it the defParams array
     */
    protected function getDefParams() {
        if ($this->version == 1) {
            $defParams = $this->filterFields($this->bo->getDefFields());
        } else {
            // If it is not version 1 of the form, replace the form keys by extracting the module prefix
            $defParams = $this->filterFields($this->bo->getDefFields(), null, "stic_Payment_Commitments___", true);
        }

        // If it has parameters encoded in JSON (field defParams) it decodes it
        if (!empty($defParams['defParams'])) {
            $jsonDefParams = json_decode(urldecode($defParams['defParams']), true);
            $defParams['decodedDefParams'] = $jsonDefParams;
        }

        $this->bo->setDefParams($defParams);
    }

    /**
     * Parent method overload
     * Retrieve form definition parameter and populate with it the formParams array
     */
    protected function getParams() {
        if ($this->version == 1) {
            $this->bo->setFormParams($this->filterFields($this->bo->getFormFields()));
        } else {
            // If it is not version 1 of the form, replace the form keys by extracting the module prefix
            $this->bo->setFormParams($this->filterFields($this->bo->getFormFields(), null, "stic_Payment_Commitments___", true));
        }
    }

    /**
     * Manage the action of a new payment.
     * If it is a payment with a card indicates the next step
     */
    private function actionNewPayment() {
        /**
         * 1. Create the payment commitment
         * 2. If it is a payment by card or with Paypal, it generates an order number, and indicates the first step of this type of payment
         */
        $response = null;

        // Create the new payment
        $payment = $this->bo->newPayment();
        if (!$payment) {
            // If it could not be created it generates an error response
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": There was an error generating the payment.");
            $response = $this->createResponse(self::RESPONSE_STATUS_ERROR, self::RESPONSE_TYPE_TXT, $this->bo->getLastError());
        } else if ($payment->payment_method == 'direct_debit') {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": A direct debit payment has been created.");
            $response = $this->createResponse(self::RESPONSE_STATUS_OK, self::RESPONSE_TYPE_TXT, $this->getMsgString('LBL_THANKS_FOR_DONATION'));
        } else if ($payment->payment_method == 'card' || $payment->payment_method == 'bizum' || substr($payment->payment_method, 0, 5) == 'card_' || substr($payment->payment_method, 0, 6) == 'bizum_') {
            // TVP REDSYS
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": A {$payment->payment_method} payment has begun.");
            $response = $this->redsysPrepareFirstStep($payment);
        } else if ($payment->payment_method == 'ceca_card' || substr($payment->payment_method, 0, 10) == 'ceca_card_') {
            // TVP CECA
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": A {$payment->payment_method} payment has begun.");
            $response = $this->cecaPrepareFirstStep($payment);

        } else if ($payment->payment_method == 'paypal') {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": A paypal payment has been initiated.");
            $response = $this->paypalPrepareFirstStep($payment);
        } else if ($payment->payment_method == 'stripe' || substr($payment->payment_method, 0, 7) == 'stripe_') {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": A stripe payment has been initiated.");
            $response = $this->stripePrepareFirstStep($payment);
        }
        // Store response data
        $this->setResponseData($payment);

        return $response;
    }

    /**
     * Process the POS response
     */
    private function actionTPVResponse() {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Processing request ...");

        // The bookstore is included
        require_once __DIR__ . '/lib/ApiRedsys.php';

        // Object is created
        $tpvSys = new RedsysAPI();

        $version = $this->bo->getParam("Ds_SignatureVersion");
        $data = $this->bo->getParam("Ds_MerchantParameters");
        $receivedSignature = $this->bo->getParam("Ds_Signature");

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Decoding received parameters...");
        $decodec = $tpvSys->decodeMerchantParameters($data);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving POS settings...");

        $tpvSys->stringToArray($decodec);

        // At the time of processing the response from the POS, the payment payment_method created before retrieving the settings corresponding to the alternative POS is necessary.
        global $db;
        $paymentMethod = $db->getOne("select payment_method from stic_payments where transaction_code = CONVERT('{$tpvSys->vars_pay['Ds_Order']}', UNSIGNED INTEGER)");

        $settings = PaymentBO::getTPVSettings($paymentMethod);
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Settings: ", print_r($settings, true));
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": TPVResponseSettings: ", print_r($settings, true));

        $kc = $settings["TPV_PASSWORD"];
        if (empty($kc)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Could not retrieve the PASSWORD POS constant.");
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Checking signature...");
        $signature = $tpvSys->createMerchantSignatureNotif($kc, $data);
        if ($signature != $receivedSignature) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": The signatures do not match [{$signature}] [{$receivedSignature}].");
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Signature validated. Processing request...");
        $retCode = $this->bo->proccessTPVResponse($tpvSys->vars_pay);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Loading mail manager ...");
        require_once __DIR__ . "/PaymentMailer.php";
        $mailer = WebFormMailer::readDataToDeferredMail(intval($tpvSys->vars_pay['Ds_Order']));
        if (!$mailer) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": No data for sending deferred emails.");
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending notification emails ...");
            $mailer->sendDeferredMails($retCode, self::RESPONSE_TYPE_TPV_RESPONSE);
        }

        if ($retCode) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error processing the POS response [{$decodec}].");
            $this->returnCode($this->bo->getLastError());
            return $this->feedBackError($this);
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": POS response processed successfully.");
            return $this->createResponse(self::RESPONSE_STATUS_OK, self::RESPONSE_TYPE_TXT, $this->getMsgString('LBL_THANKS_FOR_DONATION'));
        }
    }

    /**
     * Manage the response from CECA's TPV system following a payment attempt.
     * It validates the payment authorization based on CECA's response parameters and handles
     * the business logic for both successful and unsuccessful payments.
     */
    private function actionCECAResponse()
    {
        global $db;

        // Retrieve payment details using the operation number provided by CECA's response.
        // This is crucial for linking the response to the corresponding payment record.
        $payment = $db->fetchOne("select id, payment_method from stic_payments where transaction_code = CONVERT('{$_REQUEST['Num_operacion']}', UNSIGNED INTEGER)");

        // Verification of the existence of payment details to ensure the operation can be linked to an existing payment.
        if (empty($payment['id']) || empty($payment['payment_method'])) {
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        // Save the payment ID for later processing steps. This bridges the current response handling with subsequent actions.
        $_REQUEST['paymentId'] = $payment['id'];

        // Retrieve payment method settings, particularly looking for the encryption key to validate the response signature.
        // This step is critical for ensuring the integrity and authenticity of the response from CECA.
        $settings = PaymentBO::getTPVCECASettings($payment['payment_method']);

        // Fill in 0 on the left
        $settings['TPVCECA_MERCHANT_CODE']=str_pad($settings['TPVCECA_MERCHANT_CODE'],9,'0', STR_PAD_LEFT);
        $settings['TPVCECA_ACQUIRER_BIN']=str_pad($settings['TPVCECA_ACQUIRER_BIN'], 10, '0', STR_PAD_LEFT);
        $settings['TPVCECA_TERMINAL']=str_pad($settings['TPVCECA_TERMINAL'], 8, '0', STR_PAD_LEFT);

        if (empty($settings['TPVCECA_PASSWORD'])) {
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        // Construct the signature string from response and settings, a step required to authenticate the response.
        // The choice between 'Referencia' and 'Codigo_error' for signature computation is based on whether the payment was successful or not.
        $receivedSignature = $this->bo->getParam("Firma");
        $newSignSourceString = 
            $settings['TPVCECA_PASSWORD'] 
            . $_REQUEST['MerchantID'] 
            . $_REQUEST['AcquirerBIN'] 
            . $_REQUEST['TerminalID'] 
            . $_REQUEST['Num_operacion'] 
            . $_REQUEST['Importe'] 
            . $settings['TPVCECA_CURRENCY'] 
            . $_REQUEST['Exponente'] 
            . ($_REQUEST['Referencia'] ?? $_REQUEST['Codigo_error']);

        if (strlen(trim($newSignSourceString)) > 0) {
            $newSign = strtolower(hash('sha256', $newSignSourceString));
        } else {
            $this->returnCode('INVALID_CECA_SIGNATURE');
            return $this->feedBackError($this);
        }

        // Signature verification to confirm the response's integrity. Mismatched signatures indicate potential tampering or issues in the communication.
        if ($newSign != $receivedSignature) {
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        // Process the verified response according to the business logic, which might involve updating payment status, sending notifications, etc.
        $retCode = $this->bo->proccessTPVCECAResponse($_REQUEST);

        // Handling email notifications based on the processing outcome. This is part of the post-processing steps to inform relevant parties of the payment status.
        require_once __DIR__ . "/PaymentMailer.php";
        $mailer = WebFormMailer::readDataToDeferredMail(intval($_REQUEST['Num_operacion']));
        if ($mailer) {
            $mailer->sendDeferredMails($retCode, self::RESPONSE_TYPE_TPV_RESPONSE);
        }

        // Final handling based on the overall processing outcome, including error management and success acknowledgments.
        if ($retCode) {
            $this->returnCode($this->bo->getLastError());
            return $this->feedBackError($this);
        } else {
            return $this->createResponse(self::RESPONSE_STATUS_OK, self::RESPONSE_TYPE_TXT, $this->getMsgString('LBL_THANKS_FOR_DONATION'));
        }
    }

    /**
     * Generate the answer for the first step in redsys payment methods (card or bizum)
     * Returns the Response generated to initiate redsys payment methods
     */
    private function redsysPrepareFirstStep($payment) {

        // The library is included
        require_once __DIR__ . '/lib/ApiRedsys.php';
        // Object is created
        $tpvSys = new RedsysAPI();

        // Retrieve application settings
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving POS settings...");
        $settings = $this->bo->getTPVSettings($payment->payment_method);
        if ($settings == null) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Cannot continue because the POS settings cannot be retrieved.");
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        // Check that the settings are complete and if so, add it to the parameters
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Assigning POS settings to request parameters...");

        $requiredConsts = array(
            "TPV_MERCHANT_CODE" => "DS_MERCHANT_MERCHANTCODE",
            "TPV_TERMINAL" => "DS_MERCHANT_TERMINAL",
            "TPV_CURRENCY" => "DS_MERCHANT_CURRENCY",
            "TPV_TRANSACTION_TYPE" => "DS_MERCHANT_TRANSACTIONTYPE",
            "TPV_MERCHANT_URL" => "DS_MERCHANT_MERCHANTURL",
            "TPV_PASSWORD" => "TPV_PASSWORD",
            "TPV_VERSION" => "Ds_SignatureVersion",
            "TPV_SERVER_URL" => "TPV_SERVER_URL",

        );

        // Specific config for bizum payments
        if ($payment->payment_method == 'bizum'  || substr($payment->payment_method, 0, 6) == 'bizum_') {
            $tpvSys->setParameter('DS_MERCHANT_PAYMETHODS', 'z');
        }

        foreach ($requiredConsts as $key => $value) {
            if (!array_key_exists($key, $settings)) {
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": The constant {$key} missing or empty.");
                $this->returnCode('UNEXPECTED_ERROR');
                return $this->feedBackError($this);
            } else {
                // If the parameter exists it adds it to the parameters
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$key} = {$settings[$key]}.");
                $tpvSys->setParameter($value, $settings[$key]);
            }
        }

        // The amount must go without decimals and expressed in cents
        $amount = $payment->amount * 100;

        $PCBean = $this->bo->getLastPC();

        // The order number must have between 4 and 12 characters, fill with 0 on the left in case there are missing positions.
        $id = str_pad($payment->transaction_code, 12, '0', STR_PAD_LEFT);

        // Specific config for recurring card payments
        if (($payment->payment_method == 'card' || substr($payment->payment_method, 0, 5) == 'card_') && $PCBean->periodicity != 'punctual') {
            $tpvSys->setParameter('DS_MERCHANT_IDENTIFIER', 'REQUIRED');
            $tpvSys->setParameter("DS_MERCHANT_COF_INI", "S");
            $tpvSys->setParameter("DS_MERCHANT_COF_TYPE", "R");

            
            // Check if first_payment_date is set and if it is in d/m/Y format (if payment is from event), in this case we convert it to Y-m-d
            if (!empty($PCBean->first_payment_date) && preg_match('/^\d{1,2}\/\d{1,2}\/\d{4}$/', $PCBean->first_payment_date)) {               
                $dateObj = DateTime::createFromFormat('d/m/Y', $PCBean->first_payment_date);
                if ($dateObj !== false) {
                    $PCBean->first_payment_date = $dateObj->format('Y-m-d');
                }
            }
            
            // If the first payment date is today, the operation is processed as a payment with the amount indicated in the form,
            // if it is a future date, the amount is 0
            if ($PCBean->first_payment_date > date('Y-m-d')) {
                $amount = 0;

                // If it is an initial authorization for a later payment, we add the suffix "-AUT" after "transaction_code"
                // to use in  DS_MERCHANT_ORDER, and prevent this value from repeating when executing the first recurring payment and
                // avoid error 9051 ("Error número de pedido repetido")
                $id = str_pad($payment->transaction_code . '-AUT', 12, '0', STR_PAD_LEFT);
            }
        }

        $koURL = $this->bo->getKOURL();
        $okURL = $this->bo->getOKURL();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Adding non-constant parameters [{$amount}] [{$id}] [{$payment->transaction_code}] [{$okURL}] [{$koURL}] ...");

        $tpvSys->setParameter("DS_MERCHANT_AMOUNT", $amount);

        // Add the non-dependent fields of the settings
        $tpvSys->setParameter("DS_MERCHANT_ORDER", $id);
        $tpvSys->setParameter("DS_MERCHANT_URLKO", $koURL);
        $tpvSys->setParameter("DS_MERCHANT_URLOK", $okURL);
        $tpvSys->setParameter("DS_MERCHANT_CONSUMERLANGUAGE", PaymentBO::getTPVLanguage($this->getLanguage()));

        // Set the Titular name to the TPV (DS_MERCHANT_TITULAR)
        include_once 'SticInclude/Utils.php';
        $merchant_titular = "";
        $relatedContactBean = SticUtils::getRelatedBeanObject($PCBean, 'stic_payment_commitments_contacts');
        if($relatedContactBean) {
            $merchant_titular = $relatedContactBean->full_name;
        } else {
            $relatedAccountBean = SticUtils::getRelatedBeanObject($PCBean, 'stic_payment_commitments_accounts');
            if($relatedAccountBean) {
                $merchant_titular = $relatedAccountBean->name;
            }
        }
        if (!empty($merchant_titular)) {
            // Limit to length 16
            $merchant_titular = substr($merchant_titular, 0, 60);
            $tpvSys->setParameter("DS_MERCHANT_TITULAR", $merchant_titular);
        }

        // Configuration data
        $version = $settings["TPV_VERSION"];
        $kc = $settings["TPV_PASSWORD"];
        $url = $settings["TPV_SERVER_URL"];

        // The request parameters are generated
        $params = $tpvSys->createMerchantParameters();
        $signature = $tpvSys->createMerchantSignature($kc);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Parameters [{$params}] signature [{$signature}] URL {$url}...");
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving template...");
        $xtpl = self::getNewTemplate(__DIR__ . '/tpls/TPVFirstStep.html');
        $xtpl->assign('SIG_VERSION', $version);
        $xtpl->assign('SIGNATURE', $signature);
        $xtpl->assign('PARAMS', $params);
        $xtpl->assign('SERVER_URL', $url);
        $xtpl->assign('LANG', $this->getLanguage());
        $xtpl->assign('LOADING_MESSAGE', $this->getMsgString('LBL_TPV_LOADING_MESSAGE'));
        $xtpl->parse('main');
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Returning answer...");

        return $this->createResponse(self::RESPONSE_STATUS_PENDING, self::RESPONSE_TYPE_TEMPLATE, $xtpl);
    }
    /**
     * Generate the answer for the first step in CECA payment methods
     * Returns the Response generated to initiate CECA payment methods
     */
    private function cecaPrepareFirstStep($payment)
    {
        // Retrieve application settings
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving CECA settings...");

        // Obtaining CECA settings
        $settings = $this->bo->getTPVCECASettings($payment->payment_method);
        if ($settings == null) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Cannot continue because the POS settings cannot be retrieved.");
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }
        
        // Fill in 0 on the left
        $settings['TPVCECA_MERCHANT_CODE']=str_pad($settings['TPVCECA_MERCHANT_CODE'],9,'0', STR_PAD_LEFT);
        $settings['TPVCECA_ACQUIRER_BIN']=str_pad($settings['TPVCECA_ACQUIRER_BIN'], 10, '0', STR_PAD_LEFT);
        $settings['TPVCECA_TERMINAL']=str_pad($settings['TPVCECA_TERMINAL'], 8, '0', STR_PAD_LEFT);
           
        // Check that the settings are complete and if so, add it to the parameters
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Assigning CECA settings to request parameters...");
        $requiredConsts = [
            'TPVCECA_MERCHANT_CODE',
            'TPVCECA_ACQUIRER_BIN',
            'TPVCECA_TERMINAL',
            'TPVCECA_CURRENCY',
            'TPVCECA_MERCHANT_URL',
            'TPVCECA_PASSWORD',
            'TPVCECA_VERSION',
            'TPVCECA_TEST',
            'TPVCECA_SERVER_URL',
        ];

        foreach ($requiredConsts as $key) {
            if (empty($settings[$key]) && $settings[$key] != 0 ) { 
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": The setting {$key} is missing or empty.");
                $this->returnCode('UNEXPECTED_ERROR');
                return $this->feedBackError($this);
            } else {
                // If the parameter exists it adds it to the parameters
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$key} = {$settings[$key]}.");
            }
        }

        // Convert the amount to cents
        $amount = ($payment->amount * 100);
        $koURL = $this->bo->getKOURL();
        $okURL = $this->bo->getOKURL();

        // The order number must have between 4 and 12 characters, fill with 0 on the left in case there are missing positions.
        // str_pad($payment->transaction_code, 12, '0', STR_PAD_LEFT);
        $id = $payment->transaction_code;

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Adding non-constant parameters [{$amount}] [{$id}] [{$payment->transaction_code}] [{$okURL}] [{$koURL}] ...");

        // Get the last PC
        $PCBean = $this->bo->getLastPC();

        // Configuration data
        if ($PCBean->periodicity == 'punctual') {
            $xtpl = self::getNewTemplate(__DIR__ . '/tpls/CecaFirstStep.html');
        } else {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "CECA Recurring payments are not enabled.");
        }

        // Define language
        switch ($this->getLanguage()) {
            case 'es_ES':
                $idioma = 1;
                break;
            case 'ca_ES':
                $idioma = 2;
                break;
            case 'eu_ES':
                $idioma = 3;
                break;
            case 'gl_ES':
                $idioma = 4;
                break;
            case 'en_us':
                $idioma = 6;
                break;
            case 'fr_FR':
                $idioma = 7;
                break;
            case 'de_DE':
                $idioma = 8;
                break;
            case 'pt_PT':
                $idioma = 9;
                break;
            case 'it_IT':
                $idioma = 10;
                break;
            case 'ru_RU':
                $idioma = 14;
                break;
            case 'no_NO':
                $idioma = 15;
                break;
            default:
                $idioma = 1;
                break;
        }

        // Calculate the signature value required to include in the form
        $firma = $settings['TPVCECA_PASSWORD']
        . $settings['TPVCECA_MERCHANT_CODE']
        . $settings['TPVCECA_ACQUIRER_BIN']
        . $settings['TPVCECA_TERMINAL']
        . $id
        . $amount
        . $settings['TPVCECA_CURRENCY']
        . '2'
        . 'SHA2'
        . $okURL
        . $koURL;


        if (strlen(trim($firma)) > 0) {
            // SHA256 calculation
            $firma = strtolower(hash('sha256', $firma));
        } else {
            $this->returnCode('INVALID_CECA_SIGNATURE');
            return $this->feedBackError($this);
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Invalid CECA signature ");
        }
        // Retrieve template
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving template...");
        $xtpl->assign('server_url', $settings["TPVCECA_SERVER_URL"]);
        $xtpl->assign('merchant_id', $settings['TPVCECA_MERCHANT_CODE']);
        $xtpl->assign('acquirer_bin', $settings['TPVCECA_ACQUIRER_BIN']);
        $xtpl->assign('terminal_id', $settings['TPVCECA_TERMINAL']);
        $xtpl->assign('koURL', $koURL);
        $xtpl->assign('okURL', $okURL);
        $xtpl->assign('num_operation', $id);
        $xtpl->assign('importe', $amount);
        $xtpl->assign('tipomoneda', $settings['TPVCECA_CURRENCY']);
        $xtpl->assign('description', $PCBean->banking_concept);
        $xtpl->assign('idioma', $idioma);
        $xtpl->assign('firma', $firma);
        $xtpl->assign('LOADING_MESSAGE', $this->getMsgString('LBL_TPV_LOADING_MESSAGE'));
        $xtpl->parse('main');

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Returning answer...");

        // Create response
        return $this->createResponse(self::RESPONSE_STATUS_PENDING, self::RESPONSE_TYPE_TEMPLATE, $xtpl);
    }

    /**
     * Generate the answer for the first step in the payment with Paypal
     * Returns the Response generated to initiate card payment
     */
    private function PaypalPrepareFirstStep($payment) {
        // Retrieve application settings
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving PayPal settings...");
        $settings = $this->bo->getPaypalSettings();
        if ($settings == null) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Unable to continue because PayPal settings can't be retrieved.");
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }

        // Check that the settings are complete and if so, add it to the parameters
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Assigning PayPal settings to request parameters...");
        $requiredConsts = array("PAYPAL_URL", "PAYPAL_ID");

        foreach ($requiredConsts as $key) {
            if (empty($settings[$key])) {
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": The setting {$key} is missing or empty.");
                $this->returnCode('UNEXPECTED_ERROR');
                return $this->feedBackError($this);
            } else {
                // If the parameter exists it adds it to the parameters
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$key} = {$settings[$key]}.");
            }
        }

        $amount = $payment->amount;
        $koURL = $this->bo->getKOURL();
        $okURL = $this->bo->getOKURL();
        // The order number must have between 4 and 12 characters, fill with 0 on the left in case there are missing positions.
        $id = str_pad($payment->transaction_code, 12, '0', STR_PAD_LEFT);
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Adding non-constant parameters [{$amount}] [{$id}] [{$payment->transaction_code}] [{$okURL}] [{$koURL}] ...");

        $PCBean = $this->bo->getLastPC();

        // Configuration data
        if ($PCBean->periodicity != 'punctual') {

            $xtpl = self::getNewTemplate(__DIR__ . '/tpls/PaypalRecurringFirstStep.html');

            switch ($PCBean->periodicity) {
                case 'monthly':
                    $monthsCount = 1;
                    break;
                case 'bimonthly':
                    $monthsCount = 2;
                    break;
                case 'quarterly':
                    $monthsCount = 3;
                    break;
                case 'four_monthly':
                    $monthsCount = 4;
                    break;
                case 'half_yearly':
                    $monthsCount = 6;
                    break;
                case 'annual':
                    $monthsCount = 12;
                    break;

                default:
                    break;
            }

            $xtpl->assign('intervalCount', $monthsCount);

        } else {
            $xtpl = self::getNewTemplate(__DIR__ . '/tpls/PaypalFirstStep.html');
        }

        $paypal_url = $settings["PAYPAL_URL"];
        $paypal_id = $settings["PAYPAL_ID"];
        $ipn = self::getMerchantPaypalURL();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving template...");
        $xtpl->assign('paypal_url', $paypal_url);
        $xtpl->assign('paypal_id', $paypal_id);
        $xtpl->assign('ipn', $ipn);
        $xtpl->assign('amount', $amount);
        $xtpl->assign('itemName', $PCBean->banking_concept);
        $xtpl->assign('koURL', $koURL);
        $xtpl->assign('okURL', $okURL);
        $xtpl->assign('custom', $id);
        $xtpl->assign('LANG', $this->getLanguage());
        $xtpl->assign('LOADING_MESSAGE', $this->getMsgString('LBL_PAYPAL_LOADING_MESSAGE'));
        $xtpl->parse('main');
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Returning answer...");

        return $this->createResponse(self::RESPONSE_STATUS_PENDING, self::RESPONSE_TYPE_TEMPLATE, $xtpl);
    }

    private static function getMerchantPaypalURL() {
        require_once 'modules/stic_Web_Forms/controller.php';
        $server = stic_Web_FormsController::getServerURL();
        $url = "{$server}/index.php?entryPoint=stic_Web_Forms_paypal_response";
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$url}");
        return $url;
    }

    /**
     *   Process the PayPal response
     */
    private function actionPaypalResponse() {

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Processing request...");

        // Call the PayPal verification
        $res = $this->verifyPayPal();

        if (strcmp($res, "VERIFIED") == 0) {
            // PayPal says the message is valid. Prepare data for later use.
            $id = $_POST['custom'];
            $paypalIPNMessage = $_POST;
        } else {
            // PayPal says the message is not valid or says nothing
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid or empty paypal IPN message: {$res}");
            $this->returnCode('INVALID_IPN_SOURCE');
            return $this->feedBackError($this);
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": IPNmessage verified by PayPal. Processing request...");

        $retCode = $this->bo->proccessPaypalResponse($paypalIPNMessage);

        require_once __DIR__ . "/PaymentMailer.php";
        $mailer = WebFormMailer::readDataToDeferredMail(intval($id), true);
        if (!$mailer) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": No data for sending deferred emails.");
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending notification emails ...");
            $mailer->sendDeferredMails($retCode, self::RESPONSE_TYPE_PAYPAL_RESPONSE);
        }

        if ($retCode) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error processing the PayPal response [{$decodec}].");
            $this->returnCode($this->bo->getLastError());
            return $this->feedBackError($this);
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": PayPal response processed successfully.");
            return $this->createResponse(self::RESPONSE_STATUS_OK, self::RESPONSE_TYPE_TXT, $this->getMsgString('LBL_THANKS_FOR_DONATION'));
        }
    }

    /**
     * Verify that the payment notification actually comes from PayPal, not from any other origin.
     * PayPal will return VERIFIED or INVALID when asked.
     * More information: https://developer.paypal.com/api/nvp-soap/ipn/ht-ipn/
     * https://developer.paypal.com/api/nvp-soap/ipn/IPNImplementation/#specs
     */
    protected function verifyPayPal() {
        // Check the cURL version, below version 1.0 the origin cannot be verified
        $curlInfo = curl_version();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": SSL_VERSION [{$curlInfo['ssl_version']}]");
        if ($curlInfo['ssl_version'] < "OpenSSL/1.0.1") {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": SSL_VERSION [{$curlInfo['ssl_version']}] less than OpenSSL / 1.0.1t. Verification of the origin of the confirmation is omitted.");
            // Although message can't be verified against PayPal, let's assume it's a valid message 
            // in order to let it be further processed in the main function
            return "VERIFIED";
        }

        $raw_post_data = file_get_contents('php://input');
        $raw_post_array = explode('&', $raw_post_data);
        $myPost = array();

        foreach ($raw_post_array as $keyval) {
            $keyval = explode('=', $keyval);
            if (count($keyval) == 2) {
                $myPost[$keyval[0]] = urldecode($keyval[1]);
            }
        }

        $req = 'cmd=_notify-validate'; // Prefix that must be added to the message to verify

        foreach ($myPost as $key => $value) {
            if (function_exists('get_magic_quotes_gpc') && get_magic_quotes_gpc() == 1) {
                $value = urlencode(stripslashes($value));
            } else {
                $value = urlencode($value);
            }
            $req .= "&$key=$value";
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving PayPal settings...");
        $settings = $this->bo->getPaypalSettings();
        if ($settings == null) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Unable to continue because PayPal settings can't be retrieved.");
            $this->returnCode('UNEXPECTED_ERROR');
            return $this->feedBackError($this);
        }
        $paypal_url = $settings["PAYPAL_URL"];

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Starting cURL...");
        $ch = curl_init($paypal_url);
        if ($ch == false) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Unable to continue because can't start CURL [" . curl_error($ch) . "].");
            return false;
        }

        curl_setopt($ch, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_1_1);
        curl_setopt($ch, CURLOPT_POST, 1);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
        curl_setopt($ch, CURLOPT_POSTFIELDS, $req);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, 1);
        curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, 2);
        curl_setopt($ch, CURLOPT_FORBID_REUSE, 1);

        define('DEBUG', false);
        
        if (DEBUG == true) {
            curl_setopt($ch, CURLOPT_HEADER, 1);
            curl_setopt($ch, CURLINFO_HEADER_OUT, 1);
        }

        // If proxy is used, uncomment the following 2 lines
        // curl_setopt($ch, CURLOPT_PROXY, $proxy);
        // curl_setopt($ch, CURLOPT_HTTPPROXYTUNNEL, 1);

        // Set the max time in order to get a connection to the server
        curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 30);

        curl_setopt($ch, CURLOPT_HTTPHEADER, array('Connection: Close'));

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Connecting to the server...");
        $res = curl_exec($ch);

        if (curl_errno($ch) != 0) // cURL error
        {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Unable to connect to PayPal to validate IPN [" . curl_error($ch) . "]");
            curl_close($ch);
            return null;
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": HTTP response: " . curl_getinfo($ch, CURLINFO_HEADER_OUT) . " for IPN {$req}");
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": HTTP response to validation: {$res}");
            curl_close($ch);
        }

        $tokens = explode("\r\n\r\n", trim($res));
        $res = trim(end($tokens));
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Verification result [{$res}]");
        return $res;
    }

    /**
     * Logs a fatal error and returns feedBackError
     *
     * @param integer $line The number line of the error (__LINE__)
     * @param string $method The method name of the error (__METHOD__)
     * @param string $message The message to log with description of the error
     * @param string $returnCode The return code. Default: 'UNEXPECTED_ERROR'
     * @return array The feedBackError
     */
    private function logFatalAndFeedBackError($line, $method, $message, $returnCode = 'UNEXPECTED_ERROR') {
        $GLOBALS['log']->fatal('Line ' . $line . ': ' . $method . ": ". $message);
        $this->returnCode($returnCode);
        return $this->feedBackError($this);
    }

    /**
     * Gets the periodicity interval in months of the Payment Commitment
     *
     * @param object $pcBean The Payment Commitment to get this periodicity (stic_Payment_Commitments)
     * @return integer The periodicity value in months (-1 if not has periodiciy)
     */
    protected function getMonthIntervalPeriodicity($pcBean) {
        if ($pcBean == null || $pcBean->periodicity == null) {
            return -1;
        }
        switch ($pcBean->periodicity) {
            case 'monthly': 
                return 1;
            case 'bimonthly': 
                return 2;
            case 'quarterly': 
                return 3;
            case 'four_monthly': 
                return 4;
            case 'half_yearly': 
                return 6;
            case 'annual': 
                return 12;
            default: 
                return -1; // No periodicity
        }
    }

    /**
     * Gets the Stripe Payment Method types
     *
     * @return array
     */
    protected function getStripePaymentMethodTypes() {
        $payment_method_types = array();

        // Get Payment Method Types from request
        if (isset($_REQUEST['stripe_payment_method_types'])) {
            $payment_method_types = explode(',',$_REQUEST['stripe_payment_method_types']);
        }

        // Trim all payment_method_types
        foreach ($payment_method_types as &$method_type) {
            $method_type = trim($method_type);
        }

        return $payment_method_types;
    }

    /**
     * Prepares the necessary data to create a payment session on Stripe and redirects the user to the checkout page.
     *
     * @param object $payment Object that contains payment information.
     * @return array
     */
    private function stripePrepareFirstStep($payment)
    {
        // Include Stripe library
        require_once 'SticInclude/vendor/stripe/stripe-php/init.php';
        require_once "modules/stic_Settings/Utils.php";

        // Retrieve application settings
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving Stripe settings...");
        $settings = $this->bo->getStripeSettings();

        // In case of secondary Stripe account (payment method begins with "stripe_"), get the proper settings
        $settingsKey = "";
        if (str_starts_with($payment->payment_method, 'stripe_')) {
            $settingsKey = strtoupper(str_replace('stripe_', '', $payment->payment_method));
        }

        if ($settings == null || !isset($settings[$settingsKey])) {
            return $this->logFatalAndFeedBackError(__LINE__, __METHOD__, "Unable to continue because Stripe settings for {$payment->payment_method} can't be retrieved.");
        }

        // Check that the settings are complete
        $requiredConsts = array("STRIPE_SECRET_KEY");
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Checking Stripe settings to request parameters for {$payment->payment_method}...");
        foreach ($requiredConsts as $key) {
            if (!isset($settings[$settingsKey][$key]) || empty($settings[$settingsKey][$key])) {
                return $this->logFatalAndFeedBackError(__LINE__, __METHOD__, "The setting {$key} is missing or empty for {$payment->payment_method}.");
            } 
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Payment_method: {$payment->payment_method}; {$key} = {$settings[$settingsKey][$key]}.");
        }
        
        // The amount must go without decimals and expressed in cents
        $amount = $payment->amount * 100;
        $koURL = $this->bo->cleanUpUrl($this->bo->getKOURL(), true);
        $okURL = $this->bo->cleanUpUrl($this->bo->getOKURL(), true);

        // Get the Stripe Api Key
        $apiKey = $settings[$settingsKey]["STRIPE_SECRET_KEY"];

        // Get the last PCBean object
        $PCBean = $this->bo->getLastPC();

        // Get the item name if specified, else use the organization name.
        $itemName = !empty($PCBean->banking_concept) ? $PCBean->banking_concept : stic_SettingsUtils::getSetting('GENERAL_ORGANIZATION_NAME');
        if (empty($itemName)) {
            // Assign a value: Stripe needs an ItemName
            $itemName = "[ Empty ]";
            //return $this->logFatalAndFeedBackError(__LINE__, __METHOD__, "Unable to continue because itemName is empty (check banking_concept in form or setting 'GENERAL_ORGANIZATION_NAME'", 'LBL_STRIPE_ITEM_EMPTY');
        }

        // Get the transaction code
        $transaction_code = $payment->transaction_code;

        // Get peridodicity by months
        $payment_mode = $PCBean->periodicity == 'punctual' ? 'payment' : 'subscription';
        $monthsCount = $this->getMonthIntervalPeriodicity($PCBean);

        // Get available payment method types
        $payment_method_types = $this->getStripePaymentMethodTypes();

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Adding non-constant parameters [{$amount}] [{$transaction_code}] [{$okURL}] [{$koURL}] [{$payment_mode}] [{$monthsCount}]...");

        \Stripe\Stripe::setApiKey($apiKey);

        // Define payment session values
        $stripeSessionValues = [
            // https://stripe.com/docs/api/payment_methods/object#payment_method_object-type
            'payment_method_types' => $payment_method_types,
            'line_items' => [[
                'price_data' => [
                    'currency' => 'eur', 
                    'unit_amount' => $amount, 
                    'product_data' => ['name' => $itemName]
                ],
                'quantity' => 1
            ]],
            'metadata' => ['transaction_code' => $transaction_code],
            'mode' => $payment_mode,
            'success_url' => $okURL,
            'cancel_url' => $koURL,
        ];

        // Add user's email if available
        if (isset($_REQUEST['Contacts___email1']) && !empty($_REQUEST['Contacts___email1'])) {
            $stripeSessionValues['customer_email'] = $_REQUEST['Contacts___email1'];
        }

        // Add recurring interval
        if ($monthsCount > 0) {
            $stripeSessionValues['line_items'][0]['price_data']['recurring'] = [
                'interval' => 'month',
                'interval_count' => $monthsCount,
            ];
        }

        // Create checkout session on Stripe
        $checkout_session = \Stripe\Checkout\Session::create($stripeSessionValues);

        // Redirect user to checkout page
        // header("HTTP/1.1 303 See Other");
        // header("Location: " . $checkout_session->url);
        return $this->createResponse(self::RESPONSE_STATUS_PENDING, self::RESPONSE_TYPE_REDIRECTION, $checkout_session->url);
    }

    /**
     *   Process the Stripe response
     */
    private function actionStripeResponse() {
        // Include Stripe PHP library
        require_once 'SticInclude/vendor/stripe/stripe-php/init.php';
        require_once __DIR__ . "/PaymentMailer.php";

        // Retrieve Stripe variables from stic_Settings
        $stripeSettings = $this->bo->getStripeSettings();
        if ($stripeSettings == null) {
            return $this->logFatalAndFeedBackError(__LINE__, __METHOD__, "Unable to continue because Stripe settings can't be retrieved.");
        }

        // Read the incoming webhook request body
        $payload = @file_get_contents('php://input');
        $sig_header = $_SERVER['HTTP_STRIPE_SIGNATURE'];
        $event = null;

        // foreach Stripe Key pairs: try to construct Stripe event
        foreach ($stripeSettings as $settingKey => $settings) {
            // Get the Stripe Api Key
            $apiKey = $settings["STRIPE_SECRET_KEY"];
            // Get the Stripe Webhook Secret
            $webhookSecret = $settings["STRIPE_WEBHOOK_SECRET"];
            
            // Set up Stripe API key
            \Stripe\Stripe::setApiKey($apiKey);
            // Try to construct a Stripe event object with pair API key - Webhook Secret
            try {
                $event = \Stripe\Webhook::constructEvent($payload, $sig_header, $webhookSecret);
                $retCode = '';
                if (!$this->actionStripeResponseEvent($event, $retCode)) {
                    return $this->logFatalAndFeedBackError(__LINE__, __METHOD__, "Unable process Stripe Event.", $retCode);
                } else {
                    // Send an HTTP 200 response to indicate that the webhook has been successfully processed
                    http_response_code(200);

                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Stripe response processed successfully.");
                    return $this->createResponse(self::RESPONSE_STATUS_OK, self::RESPONSE_TYPE_TXT, $this->getMsgString('LBL_THANKS_FOR_DONATION'));
                }
            } catch (\UnexpectedValueException $e) {
                // Invalid payload: Construction fails. Exit (Keys are valid, but message is corrupt)
                return $this->logFatalAndFeedBackError(__LINE__, __METHOD__, "Unable to continue because Stripe rise an 'UnexpectedValueException': Invalid payload.");
            } catch(\Stripe\Exception\SignatureVerificationException $e) {
                // Invalid signature: Construction fails. Try with next Key pairs
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": Can not recover Stripe event, raised an 'UnexpectedValueException': Invalid payload with ApiKey:{$apiKey} and WebHookSecret:{$webhookSecret}");
                // We will try with next Key pairs
                // continue; 
            }
        }
    }

    /**
     * Process the Stripe event
     *
     * @param Stripe\Event $event The Stripe event
     * @param string $retCode The returning code (by ref)
     * @return boolean Says if the event is correctly processed
     */
    private function actionStripeResponseEvent($event, &$retCode) {
        $transactionCode = "";
        $retCode = "";
        
        // Process the Stripe event and fills the $transactionCode and retCode
        $isProcessed = $this->bo->processStripeEvent($event, $transactionCode, $retCode);

        if (empty($transactionCode)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": No transactionCode returned in Stripe response.");
        }
        else {
            // Send mail with payment information
            $mailer = WebFormMailer::readDataToDeferredMail(intval($transactionCode), true);
            if (!$mailer) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": No data for sending deferred emails.");
            } else {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending notification emails ...");
                $mailer->sendDeferredMails($retCode, self::RESPONSE_TYPE_STRIPE_RESPONSE);
            }
        }

        return $isProcessed;
    }

}
