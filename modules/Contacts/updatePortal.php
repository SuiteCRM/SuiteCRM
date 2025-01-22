<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'modules/AOP_Case_Updates/util.php';

#[\AllowDynamicProperties]
class updatePortal
{
    /**
     * @param Contact $bean
     */
    public function updateUser($bean)
    {
        $object_arr = [];
        if (!isAOPEnabled()) {
            return;
        }
        if (isset($bean->joomla_account_access) && $bean->joomla_account_access !== '') {
            global $sugar_config;
            $aop_config = $sugar_config['aop'];

            $template = BeanFactory::getBean('EmailTemplates', $aop_config['joomla_account_creation_email_template_id']);

            $search = array("\$joomla_pass", "\$portal_address");
            $replace = array($bean->joomla_account_access, $aop_config['joomla_url']);

            $object_arr['Contacts'] = $bean->id;
            $body_html = aop_parse_template($template->body_html, $object_arr);
            $body_html = str_replace($search, $replace, (string) $body_html);

            $body_plain = aop_parse_template($template->body, $object_arr);
            $body_plain = str_replace($search, $replace, (string) $body_plain);

            $this->sendEmail($bean->email1, $template->subject, $body_html, $body_plain, $bean);
        }
    }

    /**
     * @param $emailTo
     * @param $emailSubject
     * @param $emailBody
     * @param $altEmailBody
     * @param SugarBean|null $relatedBean
     */
    public function sendEmail($emailTo, $emailSubject, $emailBody, $altEmailBody, SugarBean $relatedBean = null)
    {
        require_once 'modules/Emails/Email.php';
        require_once 'include/SugarPHPMailer.php';

        $emailObj = BeanFactory::newBean('Emails');
        $emailSettings = getPortalEmailSettings();

        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();
        $mail->From = $emailSettings['from_address'];
        isValidEmailAddress($mail->From);
        $mail->FromName = $emailSettings['from_name'];
        $mail->clearAllRecipients();
        $mail->clearReplyTos();
        $mail->Subject = from_html($emailSubject);
        $mail->Body = $emailBody;
        $mail->AltBody = $altEmailBody;
        $mail->prepForOutbound();
        $mail->addAddress($emailTo);

        //now create email
        if (@$mail->send()) {
            $emailObj->to_addrs_names = $emailTo;
            $emailObj->type = 'out';
            $emailObj->deleted = '0';
            $emailObj->name = $mail->Subject;
            $emailObj->description = $mail->AltBody;
            $emailObj->description_html = $mail->Body;
            $emailObj->from_addr_name = $mail->From;
            if ($relatedBean instanceof SugarBean && !empty($relatedBean->id)) {
                $emailObj->parent_type = $relatedBean->module_dir;
                $emailObj->parent_id = $relatedBean->id;
            }
            $emailObj->date_sent_received = TimeDate::getInstance()->nowDb();
            $emailObj->modified_user_id = '1';
            $emailObj->created_by = '1';
            $emailObj->status = 'sent';
            $emailObj->save();
        }
    }
}
