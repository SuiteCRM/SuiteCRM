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

require_once('modules/Emails/Email.php');
require_once('modules/Contacts/Contact.php');

/**
 * Class sendEmail
 * TODO: Move to emails module. This class violates single responsibility principle. In that the emails
 * module should handle the email
 */
#[\AllowDynamicProperties]
class sendEmail
{
    /**
     * @param SugarBean $module
     * @param string $module_type
     * @param string $printable
     * @param string $file_name
     * @param bool $attach
     * @see generatePDF (Entrypoint)
     * @deprecated use EmailController::composeViewFrom
     */
    public function send_email($module, $module_type, $printable, $file_name, $attach)
    {
        global $current_user, $mod_strings, $sugar_config;
        // First Create e-mail draft
        $email = BeanFactory::newBean('Emails');
        // set the id for relationships
        $email->id = create_guid();
        $email->new_with_id = true;

        // subject
        $email->name = $mod_strings['LBL_EMAIL_NAME'] . ' ' . $module->name;
        // body
        $email->description_html = $printable;
        // type is draft
        $email->type = "draft";
        $email->status = "draft";

        $contact_id = '';

        if (!empty($module->billing_contact_id)) {
            $contact_id = $module->billing_contact_id;
        } else {
            if (!empty($module->contact_id)) {
                $contact_id = $module->contact_id;
            }
        }

        // TODO: FIX UID / Inbound Email Account
        $inboundEmailID = $current_user->getPreference('defaultIEAccount', 'Emails');
        $email->mailbox_id = $inboundEmailID;

        $contact = new Contact;
        if ($contact->retrieve($contact_id)) {
            $email->parent_type = 'Contacts';
            $email->parent_id = $contact->id;

            if (!empty($contact->email1)) {
                $email->to_addrs_emails = $contact->email1 . ";";
                $email->to_addrs = $contact->name . " <" . $contact->email1 . ">";
                $email->to_addrs_names = $contact->name . " <" . $contact->email1 . ">";
                $email->parent_name = $contact->name;
            }
        }


        // team id
        $email->team_id = $current_user->default_team ?? '';
        // assigned_user_id
        $email->assigned_user_id = $current_user->id;
        // Save the email object
        global $timedate;
        $email->date_start = $timedate->to_display_date_time(gmdate($GLOBALS['timedate']->get_db_date_time_format()));
        $email->save(false);
        $email_id = $email->id;

        if ($attach) {
            $note = BeanFactory::newBean('Notes');
            $note->modified_user_id = $current_user->id;
            $note->created_by = $current_user->id;
            $note->name = $file_name;
            $note->parent_type = 'Emails';
            $note->parent_id = $email_id;
            $note->file_mime_type = 'application/pdf';
            $note->filename = $file_name;
            $noteId = $note->save();

            if (!empty($noteId)) {
                rename($sugar_config['upload_dir'] . 'attachfile.pdf', $sugar_config['upload_dir'] . $note->id);
                $email->attachNote($note);
            } else {
                $GLOBALS['log']->error('AOS_PDF_Templates: Unable to save note');
            }
        }

        // redirect
        if (empty($email_id)) {
            echo "Unable to initiate Email Client";
            exit;
        } else {
            header('Location: index.php?action=ComposeViewWithPdfTemplate&module=Emails&return_module=' . $module_type . '&return_action=DetailView&return_id=' . $module->id . '&record=' . $email_id);
        }
    }
}
