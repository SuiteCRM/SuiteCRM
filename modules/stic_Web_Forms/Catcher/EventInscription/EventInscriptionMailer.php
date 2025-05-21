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
require_once __DIR__ . "/EventInscriptionBO.php";

/**
 * Class that allows the sending of donation data by mail
 */
class EventInscriptionMailer extends WebFormMailer
{
    protected $eventInscriptionBO = null;
    protected $payment = null;

    public function __construct($eventInscriptionBO = null, $payment = null)
    {
        parent::__construct();
        $this->setEventInscriptionBO($eventInscriptionBO);
        $this->setPayment($payment);
    }

    public function setEventInscriptionBO($eventInscriptionBO)
    {
        $this->eventInscriptionBO = $eventInscriptionBO;
    }

    public function setPayment($payment)
    {
        $this->payment = $payment;
    }

    /**
     * Send the informative email to the event administrator user
     * @return boolean
     */
    public function sendAdminMail()
    {

        // If found parameter on REQUEST to avoid sending administrator emails, return without sending mail
        if (isset($_REQUEST['stic_skip_admin_emails']) && !empty($_REQUEST['stic_skip_admin_emails']) 
            && $_REQUEST['stic_skip_admin_emails'] == 1) {
            return true;
        }

        // Reset the recipient list
        $this->resetDest();

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Building issue ...");
        $this->subject = $this->translate('LBL_INSCRIPTION_MAIL_ADMIN_SUBJECT') . " " . $this->eventInscriptionBO->getEvent()->name;
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Mail subject: {$this->subject}");

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding recipient ...");
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  UserID: " . $this->eventInscriptionBO->getAssignedUserId());

        $this->addUserIdDest($this->eventInscriptionBO->getAssignedUserId());

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Building message body ...");

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Registration data ...");
        $inscription = $this->eventInscriptionBO->getInscriptionObject();

        $defParams = $this->eventInscriptionBO->getDefParams();
        $formParams = $this->eventInscriptionBO->getFormParams();
        $html = $this->newObjectBodyHTML(($defParams['include_registration'] ? $inscription : null), $formParams, $inscription, true);

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Contact details ...");

        $contactResult = $this->eventInscriptionBO->getContactResult();
        $candidates = $this->eventInscriptionBO->getContactCandidates();
        $contactObject = $this->eventInscriptionBO->getContactObject();
        $objWeb = array_pop($candidates);

        switch ($contactResult) {
            case EventInscriptionBO::CONTACT_MULTIPLE:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Generating information for CONTACT_MULTIPLE");
                $html .= $this->duplicateObjectHTML($objWeb, $formParams, $contactObject, $candidates);
                break;
            case EventInscriptionBO::CONTACT_NEW:
            case EventInscriptionBO::CONTACT_UNIQUE:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Generating information for CONTACT_NEW or CONTACT_UNIQUE");
                $html .= $this->newObjectBodyHTML($objWeb, $formParams, $contactObject, $contactResult == EventInscriptionBO::CONTACT_NEW);
                break;
        }

        // Link the attached form files to the mail
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Linking the attached documents of the form to the mail to be sent to the administrator ...");
        $documents = $contactObject->documents->tempBeans;

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

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Organization Data ...");
        $accountResult = $this->eventInscriptionBO->getAccountResult();
        $candidates = $this->eventInscriptionBO->getAccountCandidates();
        $accountObject = $this->eventInscriptionBO->getAccountObject();

        if ($accountResult != EventInscriptionBO::ACCOUNT_NO_DATA) {
            $objWeb = array_pop($candidates);
        }
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  AccountResult [{$accountResult}]");

        // Send an email with the result of the operation
        switch ($accountResult) {
            case EventInscriptionBO::ACCOUNT_MULTIPLE:
                $html .= $this->duplicateObjectHTML($objWeb, $formParams, $accountObject, $candidates);
                break;
            case EventInscriptionBO::ACCOUNT_NEW:
            case EventInscriptionBO::ACCOUNT_UNIQUE:
                $html .= $this->newObjectBodyHTML($objWeb, $formParams, $accountObject, $accountResult == EventInscriptionBO::ACCOUNT_NEW);
                break;
            case EventInscriptionBO::ACCOUNT_NO_DATA:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The form does not include organizational data.");
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Payment data ...");
        if ($this->payment == null) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Payment data have not been included.");
        } else {
            $paymentMailer = new PaymentMailer();
            $html .= $paymentMailer->paymentToHTML($this->payment);
            $this->subject = "{$this->payment->transaction_code} - {$this->subject}"; // If there is linked payment, include the payment number in the subject of the mail
        }
        $this->body = $html;

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending mail ...");
        return $this->send();
    }

    /**
     * Send the notification email to the user registered to the event when the eventInscriptionBO property is informed
     */
    public function sendUserMail($templateId, $lang = null)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Preparing data for sending the notification to the user.");

        // Retrieve the data of the person received via the web (may differ from the pre-existing data in the CRM)
        $candidates = $this->eventInscriptionBO->getContactCandidates();
        $objContactWeb = array_pop($candidates);

        $candidates = $this->eventInscriptionBO->getAccountCandidates();
        $account = (!empty($candidates) ? array_pop($candidates) : null);

        return $this->__sendUserMail($templateId,
            $objContactWeb,
            $this->eventInscriptionBO->getEvent(),
            $this->eventInscriptionBO->getInscriptionObject(),
            $account,
            $this->payment,
            $lang);
    }

    /**
     * Send the notification email to the registered user
     */
    protected function __sendUserMail($templateId, $objContactWeb, $event, $inscription, $account, $payment, $lang)
    {
        // Reset the recipient list
        $this->resetDest();

        if (empty($objContactWeb->email1)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The data received does not include user email, notification cannot be sent.");
            return false;
        }
        if (empty($templateId)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No templateId present.");
            return false;
        }

        // Añade el destinatario
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding recipient [{$objContactWeb->email1}] ...");
        $this->addMailsDest($objContactWeb->email1);

        // Build the array of objects to parse
        $replacementObjects = array();
        $replacementObjects[0] = $objContactWeb;
        $replacementObjects[1] = $event;
        $replacementObjects[2] = $inscription;

        if (!empty($account)) {
            $replacementObjects[] = $account;
        }

        if (!empty($payment)) {
            $replacementObjects[] = $payment;
            if ($payment->load_relationship('stic_payments_stic_payment_commitments')) {
                $relatedBeans = $payment->stic_payments_stic_payment_commitments->getBeans();
                foreach ($relatedBeans as $fpBean) {
                    $replacementObjects[] = $fpBean;
                }
            }
        }

        // Parse the template
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Parsing template ...");
        if (false === parent::parseEmailTemplateById($templateId, $replacementObjects, $lang)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Error parsing the template.");
            return false;
        }

        // Send the mail
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending mail ...");
        return $this->send();
    }

    /**
     * Prepare the necessary information for deferred mail delivery
     */
    public function prepareDeferredMails($templateId, $lang = null)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving data for sending deferred emails ...");
        $adminId = $this->eventInscriptionBO->getAssignedUserId();

        // It recovers the data of the person received via the web (it may differ from the pre-existing data in the CRM)
        $candidates = $this->eventInscriptionBO->getContactCandidates();
        $objContactWeb = array_pop($candidates);

        $candidates = $this->eventInscriptionBO->getAccountCandidates();
        $account = (!empty($candidates) ? array_pop($candidates) : null);

        $data = array(
            "templateId" => $templateId,
            "objContactWeb" => $objContactWeb,
            "payment" => $this->payment,
            "adminId" => $adminId,
            "event" => $this->eventInscriptionBO->getEvent(),
            "inscription" => $this->eventInscriptionBO->getInscriptionObject(),
            "account" => $account,
            "language" => $lang,
        );

        return $this->saveDataToDeferredMail($this->payment->transaction_code, 'EventInscription', 'EventInscriptionMailer', $data);
    }

    /**
     * Send deferred emails
     * @param $response Response Data
     */
    public function sendDeferredMails($response = null, $type = null)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending deferred notification emails ...");

        if (!$response) // If there is no error send the notification to the user
        {
            $this->__sendUserMail(
                $this->deferredData['templateId'],
                $this->deferredData['objContactWeb'],
                $this->deferredData['event'],
                $this->deferredData['inscription'],
                $this->deferredData['account'],
                $this->deferredData['payment'],
                $this->deferredData['language']);
        }

        switch ($type) {
            case PaymentController::RESPONSE_TYPE_TPV_RESPONSE:
                $this->sendAdminConfirmation($this->deferredData['payment'], $this->deferredData['adminId'], $response);
                break;
            case PaymentController::RESPONSE_TYPE_TPVCECA_RESPONSE:
                $this->sendAdminConfirmation($this->deferredData['payment'], $this->deferredData['adminId'], $response);
                break;
            case PaymentController::RESPONSE_TYPE_PAYPAL_RESPONSE:
                $this->sendAdminPaypalConfirmation($this->deferredData['payment'], $this->deferredData['adminId'], $response);
                break;
            case PaymentController::RESPONSE_TYPE_STRIPE_RESPONSE:
                $this->sendAdminStripeConfirmation($this->deferredData['payment'], $this->deferredData['adminId'], $response);
                break;
        }

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
     * Send the confirmation email to the administrator
     */
    public function sendAdminPaypalConfirmation($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator ...");
        $paymentMailer = new PaymentMailer();
        return $paymentMailer->sendPaypalAdminResponse($payment, $adminId, $errorCode);
    }
    
    /**
     * Send the confirmation email to the administrator
     */
    public function sendAdminStripeConfirmation($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator ...");
        $paymentMailer = new PaymentMailer();
        return $paymentMailer->sendStripeAdminResponse($payment, $adminId, $errorCode);
    }

    /**
     * @param unknown $objWeb            Bean with the data received via web
     * @param unknown $formParams        Array of data received from the form
     * @param unknown $objAssigned        Object selected to perform the action
     * @param unknown $objCollection    Collection of candidate objects from which the previous object has been selected
     * @return string                    HTML with the message body
     */
    public function duplicateObjectHTML($objWeb, $formParams, $objAssigned, $objCollection)
    {
        $html = $this->translate('LBL_DUPLICATED_BODY') . ':<br><br><br>';
        $html .= $this->webObjectToHtml($objWeb, $formParams);
        $html .= '<span style="font-weight: bold;">' . $this->translate('LBL_OBJECTS_COLLECTION') . '</span>
				<table>';

        // Draw all objects likely to be duplicated
        foreach ($objCollection as $obj) {
            $html .= '<tr>
            	    <th>' . $this->translate($obj->field_defs['name']['vname'], $obj->module_name) . '</th>
            	    <td><a href="' . self::createLinkToDetailView($obj->module_name, $obj->id) . '">' . $obj->name . '</a></td>
        		    </tr>';
        }

        $html .= '</table>
                <br><br>
                <span style="font-weight: bold;">' . $this->translate('LBL_INSCRIPTION_OBJECT_SELECTED') . '</span>';
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
     * Generate the message body for unique or new objects created during the process
     * @param unknown $objWeb            Bean with the data received via web
     * @param unknown $formParams        Array of data received from the form
     * @param unknown $objSelected        Object selected to perform the action
     * @param unknown $isNewObject        Indicates whether the object is new or has been recovered from the database
     * @return string                    HTML with the message body*
     */
    public function newObjectBodyHTML($objWeb, $formParams, $objSelected, $isNewObject)
    {
        global $app_list_strings;
        $moduleLabel = $app_list_strings['moduleList'][$objSelected->module_name];
        $html = "";
        if ($objWeb != null) {
            $html = $this->webObjectToHtml($objWeb, $formParams);
        }
        $html .= '<span style="font-weight: bold;">' . $moduleLabel . ' - ';
        $html .= ($isNewObject ? $this->translate('LBL_INSCRIPTION_NEW_OBJECT_BODY') : $this->translate('LBL_INSCRIPTION_OBJECT_SELECTED')) . ':';
        $html .= '</span>';
        $html .= '<table>
                     <tr>
                        <th>' . $this->translate($objSelected->field_defs['name']['vname'], $objSelected->module_name) . '</th>
                        <td><a href="' . self::createLinkToDetailView($objSelected->module_name, $objSelected->id) . '">' . $objSelected->name . '</a></td>
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
        global $app_list_strings;
        $excludeFields = array('assigned_user_id'); // Fields that will not be included in the mail
        $prefix = "{$objWeb->module_name}___";

        $moduleLabel = $app_list_strings['moduleList'][$objWeb->module_name];
        $html = '<span style="font-weight: bold;">' . $moduleLabel . ' - ' . $this->translate('LBL_OBJECT_FROM_WEB') . ':</span>';
        $html .= '<table>';

        foreach ($formParams as $key => $value) {
            $label = '';
            if (strpos($key, $prefix) !== false) {
                $key = preg_replace("/{$prefix}/Au", '', $key);
                if (!in_array($key, $excludeFields)) // If the field is not in the list of fields to ignore ...
                {
                    $label = $this->translate($objWeb->field_defs[$key]['vname'], $objWeb->module_name);
                    $objList = $objWeb->field_defs[$key]['options'] ?? null;
                    if (!empty($objList) && strpos($objList, 'range_search_dom') === false) // If it is a drop-down field, load the labels
                    {
                        // TODO: review multi-selection fields
                        $value = $GLOBALS['app_list_strings'][$objWeb->field_defs[$key]['options']][$value] ?? null;
                    }
                    $html .= '    <tr><th>' . rtrim($label, ':') . ':</th><td>' . $value . '</td></tr>';
                }
            }
        }
        $html .= '</table>
                <br>';
        return $html;
    }
}
