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
require_once __DIR__ . '/DonationBO.php';
require_once __DIR__ . '/DonationPaymentController.php';

/**
 * Class that controls the flow for donation forms
 */
#[AllowDynamicProperties]
class DonationController extends WebFormDataController
{
    protected $version = 1; // Allows you to use the same logic for forms generated with different versions of the assistant

    /**
     * Controller builder
     */
    public function __construct()
    {
        parent::__construct();
        $this->bo = new DonationBO();
    }

    public function getObjectsCreated() {
        return array_merge($this->bo->getObjectsCreated(), $this->fp->getObjectsCreated());
    }

    /**
     * Overload of the parent method.
     * Retrieve the form parameters and populate the formParams array with it
     */
    protected function getParams()
    {
        $defParams = $this->bo->getDefParams();

        // Check the form version
        if (!empty($defParams['decodedDefParams']['version'])) {
            $this->version = $defParams['decodedDefParams']['version'];
        }

        if ($this->version == 1) {
            $this->bo->setFormParams($this->filterFields($this->bo->getFormFields()));
        } else {
            // If it is not version 1 of the form, replace the form keys by extracting the module prefix
            $this->bo->setFormParams($this->filterFields($this->bo->getFormFields(), null, "{$defParams['web_module']}___", true));

            // If there are required fields, we also delete the prefix
            if (!empty($defParams['req_id'])) {
                $prefix = $prefix ?? null;
                $newReqId = preg_replace("/{$prefix}/u", '', $defParams['req_id']);
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  New Req Id {$newReqId}");
                $defParams['req_id'] = $newReqId;
                $this->bo->setDefParams($defParams);
            }
        }
    }

    /**
     * Parent method overload.
     * Execute the necessary operations to manage the operation.
     * Will be preceded by a call to getParams and checkParams
     * @return Array In case of error return false and leave a message in $ this-> feedBackMsg, in case of success return true
     */
    protected function doAction()
    {
        // Decide the donor (and create it if necessary)
        $donator = null;
        $candidates = null;
        if (isset($_REQUEST['custom_contacts_matching']) && !empty($_REQUEST['custom_contacts_matching'])) {
            $donatorResult = $this->bo->getCustomContactMatching($donator, $candidates);
        } else {
            $donatorResult = $this->bo->getDonator($donator, $candidates);
        }

        $objWeb = array_pop($candidates);

        // We create a payment controller
        $this->fp = new DonationPaymentController($this->version);
        $params = array(
            'payer' => $donator,
            'campaign' => $this->bo->getCampaign(),
            'redirect_url' => $this->bo->getOKURL(),
            'redirect_ko_url' => $this->bo->getKOURL(),
        );

        // Tell the controller who the donor will be.
        $this->fp->setNoRequestParams($params);

        // We call the controller of the payment methods, delegating the request so that the answer is treated in the method that has called us
        $response = $this->fp->manage(true);

        // If the creation of the payment method has been successful (or is initiated in the case of card or bizum payment, send the corresponding mail)
        $response['status'] = $response['status'] ?? null;
        if ($response['status'] == self::RESPONSE_STATUS_OK ||
            $response['status'] == self::RESPONSE_STATUS_PENDING) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Generating warning mail ...");

            require_once __DIR__ . '/DonationMailer.php';
            $mailer = new DonationMailer();
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding recipient ...");
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  UserID: " . $this->bo->getAssignedUserId());
            $mailer->addUserIdDest($this->bo->getAssignedUserId());

            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Payment data in Donation");
            $payment = $this->fp->getResponseData();

            // If the shipment has not gone ok it generates a trace
            if (!$mailer->sendAdminMail($objWeb, $payment, $this->bo->getFormParams(), $donator, $candidates, $donatorResult)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Unable to send the email to the administrator.");
            }

            // If we are in an advanced version of the form send the notice to the user
            if ($this->version > 1) {
                $defParams = $this->bo->getDefParams();
                // If it is a deferred operation, save the data but the notification is not sent
                if ($response['status'] == self::RESPONSE_STATUS_PENDING) {
                    if (!$mailer->prepareDeferredMails(
                        $defParams['decodedDefParams']['email_template_id'],
                        $objWeb,
                        $payment,
                        $this->bo->getAssignedUserId(),
                        $this->lang
                    )) {
                        $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Failed to save information for sending deferred emails.");
                    }
                } // If it is an immediate operation the notification is automatically sent
                else if (!$mailer->sendUserMail($defParams['decodedDefParams']['email_template_id'], $objWeb, $payment)) {
                    $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Unable to send confirmation email to user.");
                }
            }
        }

        // If the request has finished, correctly or with error, check if there is a redirection address
        // If there is, it turns the response into a redirection, otherwise it will return a text response
        if ($response['status'] == self::RESPONSE_STATUS_OK ||
            $response['status'] == self::RESPONSE_STATUS_ERROR) {
            $url = $response['status'] == self::RESPONSE_STATUS_OK ? $this->bo->getOKURL() : $this->bo->getKOURL();
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The status [{$response['status']}] redirect url [{$url}].");
            // Create a redirect response
            if (self::isNotEmptyUrl($url)) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  It is a valid URL, we create a redirect.");
                $response = $this->createResponse($response['status'], self::RESPONSE_TYPE_REDIRECTION, $url);
            } else {
                // Create a text response
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  It is not a valid url or is empty, it generates a text response.");
                if ($response['status'] == self::RESPONSE_STATUS_OK) {
                    $msg = $this->getMsgString('LBL_THANKS_FOR_DONATION');
                    $response = $this->createResponse($response['status'], self::RESPONSE_TYPE_TXT, $msg);
                } else {
                    $this->returnCode('UNEXPECTED_ERROR');
                    $response = $this->feedBackError($this);
                }
            }
        }
        return $response;
    }
}
