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

/**
 * Class CaseUpdatesHook
 */
#[\AllowDynamicProperties]
class CaseUpdatesHook
{
    private $slug_size = 50;

    /**
     * @return string
     */
    private function getAssignToUser()
    {
        require_once 'modules/AOP_Case_Updates/AOPAssignManager.php';
        $assignManager = new AOPAssignManager();

        return $assignManager->getNextAssignedUser();
    }

    /**
     * @return int
     */
    private function arrangeFilesArray()
    {
        $count = 0;
        if(!empty($_FILES['case_update_file'] )) {
            foreach ($_FILES['case_update_file'] as $key => $vals) {
                foreach ($vals as $index => $val) {
                    if (!array_key_exists('case_update_file' . $index, $_FILES)) {
                        $_FILES['case_update_file' . $index] = [];
                        ++$count;
                    }
                    $_FILES['case_update_file' . $index][$key] = $val;
                }
            }
        }

        return $count;
    }

    /**
     * @param aCase $case
     */
    public function saveUpdate($case)
    {
        if (!isAOPEnabled()) {
            return;
        }
        global $current_user, $app_list_strings;
        if (empty($case->fetched_row) || !$case->id) {
            if (!$case->state) {
                $case->state = $app_list_strings['case_state_default_key'];
            }
            if ($case->status === 'New') {
                $case->status = $app_list_strings['case_status_default_key'];
            }

            //New case - assign
            if (!$case->assigned_user_id) {
                $userId = $this->getAssignToUser();
                $case->assigned_user_id = $userId;
                $case->notify_inworkflow = true;
            }

            return;
        }
        if (isset($_REQUEST['module']) && $_REQUEST['module'] === 'Import') {
            return;
        }
        //Grab the update field and create a new update with it.
        $text = $case->update_text;
        if (!$text && empty($_FILES['case_update_file'])) {
            //No text or files, so nothing really to save.
            return;
        }
        $case->update_text = '';
        $case_update = BeanFactory::newBean('AOP_Case_Updates');
        $case_update->name = $text;
        $case_update->internal = $case->internal;
        $case->internal = false;
        $case_update->assigned_user_id = $current_user->id;
        if (strlen((string) $text) > $this->slug_size) {
            $case_update->name = substr((string) $text, 0, $this->slug_size) . '...';
        }
        $case_update->description = nl2br($text);
        $case_update->case_id = $case->id;
        $case_update->save();

        $fileCount = $this->arrangeFilesArray();

        for ($x = 0; $x < $fileCount; ++$x) {
            if ($_FILES['case_update_file']['error'][$x] === UPLOAD_ERR_NO_FILE) {
                continue;
            }
            $uploadFile = new UploadFile('case_update_file' . $x);
            if (!$uploadFile->confirm_upload()) {
                continue;
            }
            $note = $this->newNote($case_update->id);
            $note->name = $uploadFile->get_stored_file_name();
            $note->file_mime_type = $uploadFile->mime_type;
            $note->filename = $uploadFile->get_stored_file_name();
            $note->save();
            $uploadFile->final_move($note->id);
        }
        $postPrefix = 'case_update_id_';
        foreach ($_POST as $key => $val) {
            if (empty($val) || strpos($key, $postPrefix) !== 0) {
                continue;
            }
            //Val is selected doc id
            $doc = BeanFactory::getBean('Documents', $val);
            if (!$doc) {
                continue;
            }
            $note = $this->newNote($case_update->id);
            $note->name = $doc->document_name;
            $note->file_mime_type = $doc->last_rev_mime_type;
            $note->filename = $doc->filename;
            $note->save();
            $srcFile = "upload://{$doc->document_revision_id}";
            $destFile = "upload://{$note->id}";
            copy($srcFile, $destFile);
        }
    }

    /**
     * @param $caseUpdateId
     *
     * @return Note
     */
    private function newNote($caseUpdateId)
    {
        $note = BeanFactory::newBean('Notes');
        $note->parent_type = 'AOP_Case_Updates';
        $note->parent_id = $caseUpdateId;
        $note->not_use_rel_in_req = true;

        return $note;
    }

    /**
     * @param $case_id
     * @param $account_id
     */
    private function linkAccountAndCase($case_id, $account_id)
    {
        if (!$account_id || !$case_id) {
            return;
        }
        $case = BeanFactory::getBean('Cases', $case_id);
        if (!$case->account_id) {
            $case->account_id = $account_id;
            $case->save();
        }
    }

    /**
     * @param aCase $case
     * @param $event
     * @param $arguments
     */
    public function assignAccount($case, $event, $arguments)
    {
        if ($arguments['module'] !== 'Cases' || $arguments['related_module'] !== 'Contacts') {
            return;
        }
        if (!isAOPEnabled()) {
            return;
        }
        $contact = BeanFactory::getBean('Contacts', $arguments['related_id']);
        $contact->load_relationship('accounts');
        if (!$contact || !$contact->account_id) {
            return;
        }
        $this->linkAccountAndCase($case->id, $contact->account_id);
    }

    /**
     * Called when saving a new email and adds the case update to the case.
     *
     * @param Email $email
     */
    public function saveEmailUpdate($email)
    {
        if ($email->intent !== 'createcase' || $email->parent_type !== 'Cases') {
            $GLOBALS['log']->warn('CaseUpdatesHook: saveEmailUpdate: Not a create case or wrong parent type');

            return;
        }
        if (!isAOPEnabled()) {
            return;
        }
        if (!$email->parent_id) {
            $GLOBALS['log']->warn('CaseUpdatesHook: saveEmailUpdate No parent id');

            return;
        }

        if ($email->cases) {
            $GLOBALS['log']->warn('CaseUpdatesHook: saveEmailUpdate cases already set');

            return;
        }

        if ($email->fetched_row['parent_id']) {
            //Will have been processed already
            return;
        }

        $ea = new SugarEmailAddress();
        $beans = $ea->getBeansByEmailAddress($email->from_addr);
        $contact_id = null;
        foreach ($beans as $emailBean) {
            if ($emailBean->module_name === 'Contacts' && !empty($emailBean->id)) {
                $contact_id = $emailBean->id;
                $this->linkAccountAndCase($email->parent_id, $emailBean->account_id);
            }
        }
        $caseUpdate = BeanFactory::newBean('AOP_Case_Updates');
        $caseUpdate->name = $email->name;
        $caseUpdate->contact_id = $contact_id;
        $updateText = $this->unquoteEmail($email->description_html ? $email->description_html : $email->description);
        $caseUpdate->description = $updateText;
        $caseUpdate->internal = false;
        $caseUpdate->case_id = $email->parent_id;
        $caseUpdate->save();
        $notes = $email->get_linked_beans('notes', 'Notes');
        foreach ($notes as $note) {
            //Link notes to case update also
            $newNote = BeanFactory::newBean('Notes');
            $newNote->name = $note->name;
            $newNote->file_mime_type = $note->file_mime_type;
            $newNote->filename = $note->filename;
            $newNote->parent_type = 'AOP_Case_Updates';
            $newNote->parent_id = $caseUpdate->id;
            $newNote->save();
            $srcFile = "upload://{$note->id}";
            $destFile = "upload://{$newNote->id}";
            copy($srcFile, $destFile);
        }

        $this->updateCaseStatus($caseUpdate->case_id);
    }

    /**
     * Changes the status of the supplied case based on the case_status_changes config values.
     *
     * @param $caseId
     */
    private function updateCaseStatus($caseId)
    {
        global $sugar_config;
        if (empty($caseId) || empty($sugar_config['aop']['case_status_changes'])) {
            return;
        }
        $statusMap = json_decode((string) $sugar_config['aop']['case_status_changes'], 1);
        if (empty($statusMap)) {
            return;
        }
        $case = BeanFactory::getBean('Cases', $caseId);
        if (empty($case->id)) {
            return;
        }
        if (array_key_exists($case->status, $statusMap)) {
            $case->status = $statusMap[$case->status];
            $statusBits = explode('_', $case->status);
            $case->state = array_shift($statusBits);
            $case->save();
        }
    }

    /**
     * @param $text
     *
     * @return mixed|string
     */
    private function unquoteEmail($text)
    {
        global $app_strings;
        $text = html_entity_decode((string) $text);
        $text = preg_replace('/(\r\n|\r|\n)/s', "\n", $text);
        $pos = strpos($text, (string) $app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER']);
        if ($pos !== false) {
            $text = substr($text, 0, $pos);
        }

        return $text;
    }

    /**
     * @param aCase $case
     */
    public function closureNotifyPrep($case)
    {
        if (isset($_REQUEST['module']) && $_REQUEST['module'] === 'Import') {
            return;
        }
        $case->send_closure_email = true;
        if (($case->state ?? '') !== 'Closed' || ($case->fetched_row['state'] ?? '') === 'Closed') {
            $case->send_closure_email = false;
        }
    }

    /**
     * @param aCase $case
     */
    public function closureNotify($case)
    {
        if (isset($_REQUEST['module']) && $_REQUEST['module'] === 'Import') {
            return;
        }
        if ($case->state !== 'Closed' || !$case->send_closure_email) {
            return;
        }
        $this->sendClosureEmail($case);
    }

    /**
     * @param aCase $case
     *
     * @return bool
     */
    private function sendClosureEmail(aCase $case)
    {
        if (!isAOPEnabled()) {
            return true;
        }
        $GLOBALS['log']->warn('CaseUpdatesHook: sendClosureEmail called');
        require_once 'include/SugarPHPMailer.php';
        $mailer = new SugarPHPMailer();
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        $mailer->prepForOutbound();
        $mailer->setMailerForSystem();

        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $aop_config = $this->getAOPConfig();
        $emailTemplate->retrieve($aop_config['case_closure_email_template_id']);

        if (!$emailTemplate->id) {
            $GLOBALS['log']->warn('CaseUpdatesHook: sendClosureEmail template is empty');

            return false;
        }

        $contact = $case->get_linked_beans('contacts', 'Contact');
        if ($contact) {
            $contact = $contact[0];
        } else {
            return false;
        }

        $emailSettings = getPortalEmailSettings();

        $text = $this->populateTemplate($emailTemplate, $case, $contact);
        $mailer->Subject = $text['subject'];
        $mailer->Body = $text['body'];
        $mailer->isHTML(true);
        $mailer->AltBody = $text['body_alt'];
        $mailer->From = $emailSettings['from_address'];
        isValidEmailAddress($mailer->From);
        $mailer->FromName = $emailSettings['from_name'];

        $email = $contact->emailAddress->getPrimaryAddress($contact);

        $mailer->addAddress($email);

        try {
            if ($mailer->send()) {
                $this->logEmail($email, $mailer, $case->id);

                return true;
            }
        } catch (phpmailerException $exception) {
            $GLOBALS['log']->fatal('CaseUpdatesHook: sending email Failed:  ' . $exception->getMessage());
        }

        $GLOBALS['log']->info('CaseUpdatesHook: Could not send email:  ' . $mailer->ErrorInfo);

        return false;
    }

    /**
     * Called by the after_relationship_save logic hook in cases. Checks to ensure this is a
     * contact being added and sends an email to that contact.
     *
     * @param $bean
     * @param $event
     * @param $arguments
     */
    public function creationNotify($bean, $event, $arguments)
    {
        if (isset($_REQUEST['module']) && $_REQUEST['module'] === 'Import') {
            return;
        }
        if ($arguments['module'] !== 'Cases' || $arguments['related_module'] !== 'Contacts') {
            return;
        }
        if (!$bean->fetched_row) {
            return;
        }
        if (!empty($arguments['related_bean'])) {
            $contact = $arguments['related_bean'];
        } else {
            $contact = BeanFactory::getBean('Contacts', $arguments['related_id']);
        }
        $this->sendCreationEmail($bean, $contact);
    }

    /**
     * @param EmailTemplate $template
     * @param aCase $bean
     * @param $contact
     *
     * @return array
     */
    private function populateTemplate(EmailTemplate $template, aCase $bean, $contact)
    {
        global $app_strings, $sugar_config;
        //Order of beans seems to matter here so we place contact first.
        $beans = [
            'Contacts' => $contact->id,
            'Cases' => $bean->id,
            'Users' => $bean->assigned_user_id,
        ];
        $ret = [];
        $ret['subject'] = from_html(aop_parse_template($template->subject, $beans));
        $ret['body'] = from_html(
            $app_strings['LBL_AOP_EMAIL_REPLY_DELIMITER'] . aop_parse_template(
                str_replace(
                    '$sugarurl',
                    $sugar_config['site_url'],
                    (string) $template->body_html
                ),
                $beans
            )
        );
        $ret['body_alt'] = strip_tags(
            from_html(
                aop_parse_template(
                    str_replace(
                        '$sugarurl',
                        $sugar_config['site_url'],
                        (string) $template->body
                    ),
                    $beans
                )
            )
        );

        return $ret;
    }

    /**
     * @return array
     */
    private function getAOPConfig()
    {
        global $sugar_config;
        if (!array_key_exists('aop', $sugar_config)) {
            return [];
        }

        return $sugar_config['aop'];
    }

    /**
     * @param aCase $bean
     * @param $contact
     *
     * @return bool
     */
    private function sendCreationEmail(aCase $bean, $contact)
    {
        if (!isAOPEnabled()) {
            return true;
        }
        require_once 'include/SugarPHPMailer.php';
        $mailer = new SugarPHPMailer();
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        $mailer->prepForOutbound();
        $mailer->setMailerForSystem();

        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        $aop_config = $this->getAOPConfig();
        $emailTemplate->retrieve($aop_config['case_creation_email_template_id']);
        if (!$emailTemplate->id) {
            $GLOBALS['log']->warn('CaseUpdatesHook: sendCreationEmail template is empty');

            return false;
        }

        $emailSettings = getPortalEmailSettings();
        $text = $this->populateTemplate($emailTemplate, $bean, $contact);
        $mailer->Subject = $text['subject'];
        $mailer->Body = $text['body'];
        $mailer->isHTML(true);
        $mailer->AltBody = $text['body_alt'];
        $mailer->From = $emailSettings['from_address'];
        isValidEmailAddress($mailer->From);
        $mailer->FromName = $emailSettings['from_name'];
        $email = $contact->emailAddress->getPrimaryAddress($contact);
        if (empty($email) && !empty($contact->email1)) {
            $email = $contact->email1;
        }
        $mailer->addAddress($email);

        try {
            if ($mailer->send()) {
                $this->logEmail($email, $mailer, $bean->id);

                return true;
            }
        } catch (phpmailerException $exception) {
            $GLOBALS['log']->fatal('CaseUpdatesHook: sending email Failed:  ' . $exception->getMessage());
        }

        $GLOBALS['log']->info('CaseUpdatesHook: Could not send email:  ' . $mailer->ErrorInfo);

        return false;
    }

    /**
     * @param string $email
     * @param SugarPHPMailer $mailer
     * @param string $caseId
     */
    private function logEmail($email, SugarPHPMailer $mailer, $caseId = null)
    {
        require_once 'modules/Emails/Email.php';
        $emailObj = BeanFactory::newBean('Emails');
        $emailObj->to_addrs_names = $email;
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
    }

    /**
     * @param SugarBean $bean
     */
    public function filterHTML($bean)
    {
        $bean->description = SugarCleaner::cleanHtml($bean->description, true);
    }

    /**
     * @param AOP_Case_Updates $caseUpdate
     */
    public function sendCaseUpdate(AOP_Case_Updates $caseUpdate)
    {
        global $current_user, $sugar_config;
        $email_template = BeanFactory::newBean('EmailTemplates');

        $module = null;
        if (isset($_REQUEST['module'])) {
            $module = $_REQUEST['module'];
        } else {
            LoggerManager::getLogger()->warn('Requested module is not set for case update');
        }

        if ($module === 'Import') {
            //Don't send email on import
            LoggerManager::getLogger()->warn("Don't send email on import");

            return;
        }
        if (!isAOPEnabled()) {
            LoggerManager::getLogger()->warn("Don't send email if AOP enabled");

            return;
        }
        if ($caseUpdate->internal) {
            LoggerManager::getLogger()->warn("Don't send email if case update is internal");

            return;
        }
        $signature = [];
        $addDelimiter = true;
        $aop_config = $sugar_config['aop'];
        if ($caseUpdate->assigned_user_id) {
            if ($aop_config['contact_email_template_id']) {
                $email_template = $email_template->retrieve($aop_config['contact_email_template_id']);
                $signature = $current_user->getDefaultSignature();
            }
            if ($email_template->id) {
                foreach ($caseUpdate->getContacts() as $contact) {
                    $GLOBALS['log']->info('AOPCaseUpdates: Calling send email');
                    $emails = [];
                    $emails[] = $contact->emailAddress->getPrimaryAddress($contact);
                    $caseUpdate->sendEmail(
                        $emails,
                        $email_template,
                        $signature,
                        $caseUpdate->case_id,
                        $addDelimiter,
                        $contact->id
                    );
                }
            }
        } else {
            $emails = $caseUpdate->getEmailForUser();
            if ($aop_config['user_email_template_id']) {
                $email_template = $email_template->retrieve($aop_config['user_email_template_id']);
            }
            $addDelimiter = false;
            if ($emails && $email_template->id) {
                LoggerManager::getLogger()->info('AOPCaseUpdates: Calling send email');
                $caseUpdate->sendEmail(
                    $emails,
                    $email_template,
                    $signature,
                    $caseUpdate->case_id,
                    $addDelimiter,
                    $caseUpdate->contact_id
                );
            }
        }
    }
}
