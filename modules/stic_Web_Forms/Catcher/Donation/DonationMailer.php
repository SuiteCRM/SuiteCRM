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
require_once __DIR__ . "/../Include/Mailer/WebFormMailer.php";
require_once __DIR__ . "/../Include/Payment/PaymentMailer.php";
require_once __DIR__ . "/DonationBO.php";

/**
 * Class that allows the sending of donations data by mail
 */
class DonationMailer extends WebFormMailer
{

    public function __construct()
    {
        parent::__construct();
        $this->subject = $this->translate('LBL_DONATION_SUBJECT');
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Mail subject: {$this->subject}");
    }

    /**
     * Send the notification email to the administrator
     */
    public function sendAdminMail($objWeb, $payment, $formParams, $donator, $candidates, $donatorResult)
    {

        // If found parameter on REQUEST to avoid sending administrator emails, return without sending mail
        if (isset($_REQUEST['stic_skip_admin_emails']) && !empty($_REQUEST['stic_skip_admin_emails']) 
            && $_REQUEST['stic_skip_admin_emails'] == 1) {
            return true;
        }


        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending notice email to the administrator...");
        // Send an email with the result of the operation
        switch ($donatorResult) {
            case DonationBO::DONATOR_MULTIPLE:
                $html = $this->duplicateObjectMail($objWeb, $payment, $formParams, $donator, $candidates);
                break;
            case DonationBO::DONATOR_NEW:
            case DonationBO::DONATOR_UNIQUE:
                $html = $this->newDonationMail($objWeb, $payment, $formParams, $donator, $donatorResult == DonationBO::DONATOR_NEW);
                break;
        }

        $paymentMailer = new PaymentMailer();
        $html .= $paymentMailer->paymentToHTML($payment);

        // Link the attached form files to the mail
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Linking the attached documents of the form to the mail to be sent to the administrator ...");
        $documents = $donator->documents->tempBeans;

        if ($documents) {
            $html .= '<span style="font-weight: bold;">';
            $html .= $this->translate('LBL_ATTACHMENTS_WEBFORM');
            $html .= '</span>';
            $html .= '<table>';

            foreach ($documents as $key => $value) {
                $your_url = $GLOBALS['sugar_config']['site_url'] . "/index.php?module=Documents&action=DetailView&record=" . $value->id;
                $html .= '<tr><td><a href="' . $your_url . '">' . $value->document_name . '</a></td></tr>';
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  File name: " . $value->document_name . " - File URL: " . $your_url);
            }
            $html .= '</table><br><br>';
        }
        $this->body = $html;
        $this->subject = "{$payment->transaction_code} - {$this->subject}";

        return $this->send();
    }

    /**
     * Prepare the necessary information for deferred mail delivery
     */
    public function prepareDeferredMails($templateId, $objWeb, $payment, $adminId, $lang)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving data for sending deferred emails ...");
        $data = array("templateId" => $templateId, "objWeb" => $objWeb, "payment" => $payment, "adminId" => $adminId, "language" => $lang);

        return $this->saveDataToDeferredMail($payment->transaction_code, 'Donation', 'DonationMailer', $data);
    }

    /**
     * Send deferred emails
     * @param $response Response Data
     */
    public function sendDeferredMails($response = null, $mailType = null)
    {
        // Observation: Is Ok? (JBL 02/06/2023)
        //     In Donations, the mail are the same for every mailType (payment method)
        //     In Event Inscriptions there is a customization for every payment method
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending deferred notification emails ...");
        if (!$response) {
            // If there is no error send the notification to the user
            $this->sendUserMail($this->deferredData['templateId'], $this->deferredData['objWeb'], $this->deferredData['payment'], $this->deferredData['language']);
        }
        $this->sendAdminConfirmation($this->deferredData['payment'], $this->deferredData['adminId'], $response);
    }

    /**
     * Send the confirmation email to the administrator
     */
    public function sendAdminConfirmation($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator ...");
        $paymentMailer = new PaymentMailer();

        return $paymentMailer->sendTpvAdminResponse($payment, $adminId, $errorCode);
    }

    /**
     * Sending deferred notification emails to the user
     */
    public function sendDeferredPaypalMails($response = null)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending deferred notification emails ...");
        if (!$response) {
            // If there is no error send the notification to the user
            $this->sendUserMail($this->deferredData['templateId'], $this->deferredData['objWeb'], $this->deferredData['payment']);
        }
        $this->sendAdminPaypalConfirmation($this->deferredData['payment'], $this->deferredData['adminId'], $response);
    }

    /**
     * Send the confirmation email to the administrator
     */
    public function sendAdminPaypalConfirmation($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator ...");
        $paymentMailer = new PaymentMailer();
        return $paymentMailer->sendPaypalAdminResponse($payment, $adminId, $errorCode);
    }

    /**
     * Prepare the body of the notification email when more than one entity to which the donation can be linked is detected
     * @param $objWeb Object data arrived via form
     * @param $objAssigned Data of the object to which the donation has been assigned
     * @param $objCollection Array with the rest of the objects detected
     * @return bool
     */
    public function duplicateObjectMail($objWeb, $payment, $formParams, $objAssigned, $objCollection)
    {
        $html = $this->translate('LBL_DUPLICATED_BODY') . ':<br><br><br>';
        $html .= $this->webObjectToHtml($objWeb, $formParams);
        $html .= '<span style="font-weight: bold;">' . $this->translate('LBL_OBJECTS_COLLECTION') . '</span>
                <table>';

        // list all objects likely to be duplicated
        foreach ($objCollection as $obj) {
            $html .= '<tr>
                        <th>' . $this->translate($obj->field_defs['name']['vname'], $obj->module_name) . '</th>
                        <td><a href="' . self::createLinkToDetailView($obj->module_name, $obj->id) . '">' . $obj->name . '</a></td>
                    </tr>';
        }
        $html .= '</table>
                <br><br>
                <span style="font-weight: bold;">' . $this->translate('LBL_OBJECT_SELECTED') . '</span>';
        $html .= '<table>
                    <tr>
                        <th>' . $this->translate($objAssigned->field_defs['name']['vname'], $objAssigned->module_name) . '</th>
                        <td><a href="' . self::createLinkToDetailView($objAssigned->module_name, $objAssigned->id) . '">' . $objAssigned->name . '</a></td>
                    </tr>';
        $html .= '</table>
                <br><br>';
        return $html;
    }

    /**
     * Prepare the body of the notification email
     */
    public function newDonationMail($objWeb, $payment, $formParams, $objDonator, $isNewObject)
    {
        $html = $this->webObjectToHtml($objWeb, $formParams);

        $html .= '<span style="font-weight: bold;">';
        $html .= ($isNewObject ? $this->translate('LBL_NEW_OBJECT_BODY') : $this->translate('LBL_OBJECT_SELECTED')) . ':';
        $html .= '</span>';
        $html .= '<table>
                     <tr>
                        <th>' . $this->translate($objDonator->field_defs['name']['vname'], $objDonator->module_name) . '</th>
                        <td><a href="' . self::createLinkToDetailView($objDonator->module_name, $objDonator->id) . '">' . $objDonator->name . '</a></td>
                     </tr>';
        $html .= '</table>
                 <br><br>';
        return $html;
    }

    /**
     * Convert the data received from the form into HTML
     */
    private function webObjectToHtml($objWeb, $formParams)
    {
        $excludeFields = array('assigned_user_id'); // Fields that will not be included in the mail

        $html = '<span style="font-weight: bold;">' . $this->translate('LBL_OBJECT_FROM_WEB') . ':</span>';
        $html .= '<table>';
        foreach ($formParams as $key => $value) {
            $label = '';
            if (!in_array($key, $excludeFields)) // If the field is not in the list of fields to ignore...
            {
                if ($key == 'campaign_id') // If it is the campaign identifier, retrieve the information and include it in the mail
                {
                    $campaign = self::getCampaingData($value);
                    $label = $campaign['label'];
                    $value = $campaign['value'];
                } else {
                    $label = $this->translate($objWeb->field_defs[$key]['vname'], $objWeb->module_name);
                    if (!empty($objWeb->field_defs[$key]['options'])) {
                        // If it is a drop-down field, load the labels
                        // TODO: check multi-selection fields
                        $value = $GLOBALS['app_list_strings'][$objWeb->field_defs[$key]['options']][$value];
                    }
                }
                $html .= '    <tr><th>' . rtrim($label, ':') . ':</th><td>' . $value . '</td></tr>';
            }
        }
        $html .= '</table>
                <br><br>';
        return $html;
    }

    /**
     * Send the notification email to the registered user
     */
    public function sendUserMail($templateId, $objWeb, $payment, $lang = null)
    {
        // Reset the recipient list
        $this->resetDest();

        if (empty($objWeb->email1)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Shipping email address not found.");
            return false;
        }

        if (empty($templateId)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No template ID received.");
            return false;
        }

        // Add the recipient
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding recipient [{$objWeb->email1}] ...");
        $this->addMailsDest($objWeb->email1);

        // Build the array of objects to parse
        $replacementObjects = array();
        $replacementObjects[0] = $objWeb;
        $replacementObjects[1] = $payment;
        if ($payment->load_relationship('stic_payments_stic_payment_commitments')) {
            $relatedBeans = $payment->stic_payments_stic_payment_commitments->getBeans();
            foreach ($relatedBeans as $fpBean) {
                $replacementObjects[] = $fpBean;
            }
        }

        // Parse the template
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Parsing template...");

        if (false === parent::parseEmailTemplateById($templateId, $replacementObjects, $lang)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Error parsing the template.");
            return false;
        }

        // Send the mail
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending mail...");
        return $this->send();
    }
}
