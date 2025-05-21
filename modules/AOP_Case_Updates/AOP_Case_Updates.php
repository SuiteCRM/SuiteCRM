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

require_once 'util.php';
require_once 'include/clean.php';

/**
 * Class AOP_Case_Updates.
 */
#[\AllowDynamicProperties]
class AOP_Case_Updates extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOP_Case_Updates';
    public $object_name = 'AOP_Case_Updates';
    public $table_name = 'aop_case_updates';
    public $tracker_visibility = false;
    public $importable = false;
    public $disable_row_level_security = true;
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $case;
    public $case_name;
    public $case_id;
    public $contact;
    public $contact_name;
    public $contact_id;
    public $internal;
    public $notes;

    public function __construct()
    {
        parent::__construct();
    }



    /**
     * @param $interface
     *
     * @return bool
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
            default:
                return false;
        }
    }

    /**
     * @param bool $check_notify
     * @return string
     */
    public function save($check_notify = false)
    {
        $this->name = SugarCleaner::cleanHtml($this->name);
        $this->parseDescription();
        parent::save($check_notify);
        if (file_exists('custom/modules/AOP_Case_Updates/CaseUpdatesHook.php')) {
            require_once 'custom/modules/AOP_Case_Updates/CaseUpdatesHook.php';
        } else {
            require_once 'modules/AOP_Case_Updates/CaseUpdatesHook.php';
        }
        if (class_exists('CustomCaseUpdatesHook')) {
            $hook = new CustomCaseUpdatesHook();
        } else {
            $hook = new CaseUpdatesHook();
        }
        $hook->sendCaseUpdate($this);

        return $this->id;
    }

    /**
     * Fixes unclosed HTML tags
     */
    private function parseDescription()
    {
        $description = SugarCleaner::cleanHtml($this->description);

        if (preg_match('/<[^<]+>/', (string) $description, $matches) !== 0) {
            // remove external warning, if HTML is not valid
            libxml_use_internal_errors(true);
            $dom = new DOMDocument();
            $dom->loadHTML(mb_convert_encoding($description, 'HTML-ENTITIES', 'UTF-8'));
            foreach ($dom->getElementsByTagName('head') as $headElement) {
                $headElement->parentNode->removeChild($headElement);
            }
            $dom->removeChild($dom->doctype);
            $dom->appendChild($dom->firstChild);
            $description = $dom->saveHTML();

            foreach (libxml_get_errors() as $xmlError) {
                $GLOBALS['log']->warn(sprintf('%s in %s', trim($xmlError->message), get_class($this)));
            }

            libxml_clear_errors();
        }

        $this->description = trim(preg_replace('/\s\s+/', ' ', (string) $description));
    }

    /**
     * @return aCase
     */
    public function getCase()
    {
        return BeanFactory::getBean('Cases', $this->case_id);
    }

    /**
     * @return null|Contact[]
     */
    public function getContacts()
    {
        $case = $this->getCase();
        if ($case) {
            return $case->get_linked_beans('contacts', 'Contacts');
        }

        return null;
    }

    /**
     * @return null|Contact
     */
    public function getUpdateContact()
    {
        if ($this->contact_id) {
            return BeanFactory::getBean('Contacts', $this->contact_id);
        }

        return null;
    }

    /**
     * @return User
     */
    public function getUser()
    {
        return BeanFactory::getBean('Users', $this->getCase()->assigned_user_id);
    }

    /**
     * @return User
     */
    public function getUpdateUser()
    {
        return BeanFactory::getBean('Users', $this->assigned_user_id);
    }

    /**
     * @return array
     */
    public function getEmailForUser()
    {
        $user = $this->getUser();
        if ($user) {
            return array($user->emailAddress->getPrimaryAddress($user));
        }

        return array();
    }

    /**
     * @param EmailTemplate $template
     * @param bool          $addDelimiter
     * @param null          $contactId
     *
     * @return array
     */
    private function populateTemplate(EmailTemplate $template, $addDelimiter = true, $contactId = null)
    {
        global $app_strings, $sugar_config;

        $user = $this->getUpdateUser();
        if (!$user) {
            $this->getUser();
        }
        $beans = array('Contacts' => $contactId, 'Cases' => $this->getCase()->id, 'Users' => $user->id, 'AOP_Case_Updates' => $this->id);
        $ret = array();
        $ret['subject'] = from_html(aop_parse_template($template->subject, $beans));
        $body = aop_parse_template(str_replace('$sugarurl', $sugar_config['site_url'], (string) $template->body_html), $beans);
        $bodyAlt = aop_parse_template(str_replace('$sugarurl', $sugar_config['site_url'], (string) $template->body), $beans);
        if ($addDelimiter) {
            $body = $app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER'].$body;
            $bodyAlt = $app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER'].$bodyAlt;
        }
        $ret['body'] = from_html($body);
        $ret['body_alt'] = strip_tags(from_html($bodyAlt));

        return $ret;
    }

    /**
     * @param array $emails
     * @param EmailTemplate $template
     * @param array $signature
     * @param null $caseId
     * @param bool $addDelimiter
     * @param null $contactId
     *
     * @return bool
     */
    public function sendEmail(
        $emails,
        $template,
        $signature = array(),
        $caseId = null,
        $addDelimiter = true,
        $contactId = null
    ) {
        $GLOBALS['log']->info('AOPCaseUpdates: sendEmail called');
        require_once 'include/SugarPHPMailer.php';
        $mailer = new SugarPHPMailer();
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        $mailer->prepForOutbound();
        $mailer->setMailerForSystem();

        $signatureHTML = '';
        if ($signature && array_key_exists('signature_html', $signature)) {
            $signatureHTML = from_html($signature['signature_html']);
        }
        $signaturePlain = '';
        if ($signature && array_key_exists('signature', $signature)) {
            $signaturePlain = $signature['signature'];
        }
        $emailSettings = getPortalEmailSettings();
        $text = $this->populateTemplate($template, $addDelimiter, $contactId);
        $mailer->Subject = $text['subject'];
        $mailer->Body = $text['body'] . $signatureHTML;
        $mailer->isHTML(true);
        $mailer->AltBody = $text['body_alt'] . $signaturePlain;
        $mailer->From = $emailSettings['from_address'];
        isValidEmailAddress($mailer->From);
        $mailer->FromName = $emailSettings['from_name'];
        foreach ($emails as $email) {
            $mailer->addAddress($email);
        }
        try {
            if ($mailer->send()) {
                require_once 'modules/Emails/Email.php';
                $emailObj = BeanFactory::newBean('Emails');
                $emailObj->to_addrs_names = implode(',', $emails);
                $emailObj->type = 'out';
                $emailObj->deleted = '0';
                $emailObj->name = $mailer->Subject;
                $emailObj->description = $mailer->AltBody;
                $emailObj->description_html = $mailer->Body;
                $emailObj->from_addr_name = $mailer->From;
                if ($caseId) {
                    $emailObj->parent_type = 'Cases';
                    $emailObj->parent_id = $caseId;
                }
                $emailObj->date_sent_received = TimeDate::getInstance()->nowDb();
                $emailObj->modified_user_id = '1';
                $emailObj->created_by = '1';
                $emailObj->status = 'sent';
                $emailObj->save();

                return true;
            }
        } catch (phpmailerException $exception) {
            $GLOBALS['log']->fatal('AOPCaseUpdates: sending email Failed:  ' . $exception->getMessage());
        }
        $GLOBALS['log']->info('AOPCaseUpdates: Could not send email:  ' . $mailer->ErrorInfo);

        return false;
    }
}
