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
require_once __DIR__ . "/../Mailer/WebFormMailer.php";

/**
 * Class that allows the sending of payment data by mail
 * TODO: complete the class so that payment data can be sent independently of other functionalities.
 * At the moment it is only implemented payment formatting function.
 */
class PaymentMailer extends WebFormMailer
{
    public function paymentToHTML($payment, $fieldsToShow = null)
    {
        $html = '<span style="font-weight: bold;">' . $this->translate('LBL_PAYMENT_DATA') . ':</span>';
        if (empty($fieldsToShow)) {
            $fieldsToShow = array('name', 'payment_type', 'payment_method', 'amount', 'periodicity');
        }
        $html .= '<table>';
        foreach ($fieldsToShow as $key) {
            $label = isset($payment->field_defs[$key]['vname']) ? $this->translate($payment->field_defs[$key]['vname'], $payment->module_name) : '';
            if ($key == 'name') {
                // If the field is the name, create a link to directly access the record
                $value = '<a href="' . self::createLinkToDetailView($payment->module_name, $payment->id) . '">' . $payment->name . '</a>';
            } else if ($key == 'periodicity') {
                $label = ucfirst($key);
                if ($payment->load_relationship('stic_payments_stic_payment_commitments')) {
                    $relatedBeans = $payment->stic_payments_stic_payment_commitments->getBeans();
                    $fpBean = false;
                    foreach ($relatedBeans as $fpBean2) {
                        $fpBean = $fpBean2;
                    }

                    if ($fpBean) {
                        if (!empty($fpBean->field_defs[$key]['options'])) // If it is a drop-down field, load the labels
                        {
                            // TODO: Check multi-selection fields
                            $value = $GLOBALS['app_list_strings'][$fpBean->field_defs[$key]['options']][$fpBean->$key];
                        }
                    }
                }
            } else {
                $objList = $payment->field_defs[$key]['options'];
                if (!empty($objList) && strpos($objList, 'range_search_dom') === false) // If it is a drop-down field, load the labels
                {
                    // TODO: Review multi-selection fields
                    $value = $GLOBALS['app_list_strings'][$payment->field_defs[$key]['options']][$payment->$key];
                } else if ($payment->field_defs[$key]['type'] == 'currency') {
                    $value = currency_format_number($payment->$key);
                } else {
                    $value = $payment->$key;
                }
            }
            $html .= '    <tr><th>' . rtrim($label, ':') . ':</th><td>' . $value . '</td></tr>';
        }
        $html .= '</table>
                <br><br>';
        return $html;
    }

    /**
     * It generates the body and the subject of the confirmation of the payment by tpv
     * @param unknown $payment
     * @param unknown $retCode
     */
    public function sendTpvAdminResponse($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator. Transaction code: [{$payment->transaction_code}] Admin Id: [{$adminId}] ErrorCode: [{$errorCode}]");
        if ($errorCode) {
            $this->subject = $this->translate('LBL_TPV_ADMIN_RESPONSE_MAIL_SUBJECT_KO');
            $this->body = '<span>' . $this->translate('LBL_TPV_ADMIN_RESPONSE_MAIL_BODY_KO') . '</span><br><br>';
        } else {
            $this->subject = $this->translate('LBL_TPV_ADMIN_RESPONSE_MAIL_SUBJECT_OK');
            $this->body = '<span>' . $this->translate('LBL_TPV_ADMIN_RESPONSE_MAIL_BODY_OK') . '</span><br><br>';
        }
        $this->subject = "{$payment->transaction_code} - {$this->subject}";
        $this->body .= $this->paymentToHtml($payment);
        $this->addUserIdDest($adminId);

        return $this->send();
    }

    public function sendPaypalAdminResponse($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator. Transaction code: [{$payment->transaction_code}] Admin Id: [{$adminId}] ErrorCode: [{$errorCode}]");
        if ($errorCode) {
            $this->subject = $this->translate('LBL_PAYPAL_ADMIN_RESPONSE_MAIL_SUBJECT_KO');
            $this->body = '<span>' . $this->translate('LBL_PAYPAL_ADMIN_RESPONSE_MAIL_BODY_KO') . '</span><br><br>';
        } else {
            $this->subject = $this->translate('LBL_PAYPAL_ADMIN_RESPONSE_MAIL_SUBJECT_OK');
            $this->body = '<span>' . $this->translate('LBL_PAYPAL_ADMIN_RESPONSE_MAIL_BODY_OK') . '</span><br><br>';
        }
        $this->subject = "{$payment->transaction_code} - {$this->subject}";
        $this->body .= $this->paymentToHtml($payment);
        $this->addUserIdDest($adminId);

        return $this->send();
    }

    public function sendStripeAdminResponse($payment, $adminId, $errorCode)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending confirmation email to the administrator. Transaction code: [{$payment->transaction_code}] Admin Id: [{$adminId}] ErrorCode: [{$errorCode}]");
        if ($errorCode) {
            $this->subject = $this->translate('LBL_STRIPE_ADMIN_RESPONSE_MAIL_SUBJECT_KO');
            $this->body = '<span>' . $this->translate('LBL_STRIPE_ADMIN_RESPONSE_MAIL_BODY_KO') . '</span><br><br>';
        } else {
            $this->subject = $this->translate('LBL_STRIPE_ADMIN_RESPONSE_MAIL_SUBJECT_OK');
            $this->body = '<span>' . $this->translate('LBL_STRIPE_ADMIN_RESPONSE_MAIL_BODY_OK') . '</span><br><br>';
        }
        $this->subject = "{$payment->transaction_code} - {$this->subject}";
        $this->body .= $this->paymentToHtml($payment);
        $this->addUserIdDest($adminId);

        return $this->send();
    }

     /**
     * Sends an email with Stripe recurring payments information
     *
     * @param string $userId If not empty send email only to $userId
     * @param string $subject email subject
     * @param string $body email body
     * @return bool
     */
    public function sendStripeInfo($userId, $subject, $body)
    {
        $this->subject = $subject;
        $this->body = $body;

        // If no $userId is provided, send the email to all admin users
        if (empty($userId)) {
            $users = BeanFactory::getBean('Users');
            $adminList = $users->get_full_list('', "users.is_admin = 1 AND users.status = 'Active'");
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending Stripe info to all admin users. " . print_r($adminList, true));
            
            foreach ($adminList as $user) {
                $this->addUserIdDest($user->id);
            }
        } else {
            $this->addUserIdDest($userId);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending Stripe info to user {$userId}.");
        }

        $emailSent = $this->send();

        if (!$emailSent) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$subject} email sending failed.");
            return false;
        } else {
            return true;
        }
    }

    /**
     * Sends an email with PayPal recurring payments information
     *
     * @param String $userId If not empty send email only to $userId
     * @param String $subject email subject
     * @param String $body email body
     * @return void
     */
    public function sendPaypalInfo($userId, $subject, $body)
    {

        $this->subject = $subject;
        $this->body = $body;

        // If no $userId is provided, send the email to all admin users
        if (empty($userId)) {
            $users = BeanFactory::getBean('Users');
            $adminList = $users->get_full_list('', "users.is_admin = 1 AND users.status = 'Active'");
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending PayPal info to all admin users. " . print_r($adminList, true));
            
            foreach ($adminList as $user) {
                $this->addUserIdDest($user->id);
            }
        } else {
            $this->addUserIdDest($userId);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Sending PayPal info to user {$userId}.");
        }

        $emailSent = $this->send();

        if (!$emailSent) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$subject} email sending failed.");
            return false;
        } else {
            return true;
        }
    }
}
