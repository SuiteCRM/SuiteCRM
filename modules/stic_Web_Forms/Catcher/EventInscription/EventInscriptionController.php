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

require_once __DIR__ . '/EventInscriptionBO.php';
require_once __DIR__ . '/EventInscriptionPaymentController.php';

/**
 * Class that controls the flow for event registration forms
 */
class EventInscriptionController extends WebFormDataController
{
    /**
     * Controller builder
     */
    public function __construct()
    {
        parent::__construct();
        $this->bo = new EventInscriptionBO();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The builder is called");
    }

    public function getObjectsCreated() {
        return $this->bo->getObjectsCreated();
    }



    /**
     * Parent method overload
     * Execute the necessary operations to manage the operation.
     * Your call will be preceded by a call to getParams and checkParams
     * In case of error, return false and leave a message in $this->feedBackMsg
     * In case of success returns true
     */
    protected function doAction()
    {
        // Generate the inscription
        $inscription = $this->bo->doInscription();

        // By default, the request ends OK.
        $response['status'] = self::RESPONSE_STATUS_OK;

        // Retrieve information about whether or not the payment method is included
        $defParams = $this->bo->getDefParams();

        if ($defParams['include_payment_commitment']) // If the payment method is included, the payment controller is called
        {
            // Create a payment controller
            $this->fp = new EventInscriptionPaymentController();

            $params = array(
                'payer' => $this->bo->getContactObject(),
                'inscription' => $this->bo->getInscriptionObject(),
                'redirect_url' => $this->bo->getOKURL(),
                'redirect_ko_url' => $this->bo->getKOURL(),
            );

            // Tell the controller who the donor will be.
            $this->fp->setNoRequestParams($params);

            // We call the controller of the payment methods, delegating the request so that the answer is treated in the method that has called us
            $response = $this->fp->manage(true);
        }
        
        // Set response status to 0 if it is not set, to compare it later
        $response['status'] = $response['status'] ?? 0;

        // If the creation of the payment method has been successful (or was not required or is initiated in the case of card or bizum payment, send the corresponding mail)
        if ($response['status'] == self::RESPONSE_STATUS_OK ||
            $response['status'] == self::RESPONSE_STATUS_PENDING) {
            $this->sendMails($response['status']);
        }

        // If the request has finished, correctly or with error, check if there is a redirection address.
        // If there is, it turns the response into a redirection, otherwise it will return a text response
        if (isset($response['status']) && ($response['status'] == self::RESPONSE_STATUS_OK ||
            $response['status'] == self::RESPONSE_STATUS_ERROR)) {
            $url = $response['status'] == self::RESPONSE_STATUS_OK ? $this->bo->getOKURL() : $this->bo->getKOURL();
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The status [{$response['status']}] redirection url [{$url}].");

            // Create a redirect response
            if (self::isNotEmptyUrl($url)) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  It is a valid URL, we create a redirect.");
                $response = $this->createResponse($response['status'], self::RESPONSE_TYPE_REDIRECTION, $url);
            } else {
                // Create a text response
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  It is not a valid url or is empty, it generates a text response.");
                if ($response['status'] == self::RESPONSE_STATUS_OK) {
                    $msg = $this->getMsgString('LBL_THANKS_FOR_REGISTRATION');
                    $response = $this->createResponse($response['status'], self::RESPONSE_TYPE_TXT, $msg);
                } else {
                    $this->undoChanges();
                    $this->returnCode('UNEXPECTED_ERROR');
                    $response = $this->feedBackError($this);
                }
            }
        }
        return $response;
    }

    /**
     * Go back changes made in case of error
     */
    protected function undoChanges()
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Undoing changes made ...");
        $bo = $this->bo;
        $defParams = $bo->getDefParams();
        $inscription = $bo->getInscriptionObject();
        if (!empty($inscription)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Deleting registration ...");
            $inscription->mark_deleted($inscription->id);
        }
        $contactResult = $bo->getContactResult();

        // If a contact has been generated, you try to delete it
        if ($contactResult == EventInscriptionBO::CONTACT_NEW) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  A person has been generated, removing it ...");
            $contact = $bo->getContactObject();
            if (!empty($contact)) {
                $contact->mark_deleted($contact->id);
            }
        }

        // If an organization has been generated, try to delete it
        $accountResult = $bo->getAccountResult();
        if ($accountResult == EventInscriptionBO::ACCOUNT_NEW) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  An organization has been generated, eliminating it...");
            $account = $bo->getAccountObject();
            if (!empty($account)) {
                $account->mark_deleted($account->id);
            }
        }

        // If payment method has been generated, you try to delete
        if ($defParams['include_payment_commitment']) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Checking payment details ...");
            if (!empty($this->fp)) {
                $fpBO = $this->fp->getBO();
                $payment = $fpBO->getLastPayment();
                $fp = $fpBO->getLastPC();
                if (!empty($payment)) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Deleting payment data ...");
                    $payment->mark_deleted($payment->id);
                }

                if (!empty($fp)) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Deleting data from the payment method ...");
                    $fp->mark_deleted($fp->id);
                }
            }
        }
    }

    /**
     * Send the necessary warning emails
     */
    protected function sendMails($status)
    {
        $defParams = $this->bo->getDefParams();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Generating warning mail...");

        $payment = null;
        if ($defParams['include_payment_commitment']) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Payment data on registration.");
            $payment = $this->fp->getResponseData();
        }

        require_once __DIR__ . '/EventInscriptionMailer.php';
        $mailer = new EventInscriptionMailer($this->bo, $payment);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending mail to the application administrator ...");
        if (!$mailer->sendAdminMail()) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Failed to send warning email to administrator.");
        }

        // If it is a deferred response operation, prepare the emails for when the response arrives
        if ($status == self::RESPONSE_STATUS_PENDING) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving data for deferred mail delivery [$this->lang]...");
            if (!$mailer->prepareDeferredMails($defParams['email_template_id'], $this->lang)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Failed to save information for sending deferred emails.");
            }
        } else { // Otherwise, send the emails immediately
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending mail to the registered user...");
            if (!$mailer->sendUserMail($defParams['email_template_id'])) {
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Unable to send the email to the user.");
            }
        }
    }
}
