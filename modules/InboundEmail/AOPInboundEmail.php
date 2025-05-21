<?php
 /**
 *
 *
 * @package
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility Ltd <support@salesagility.com>
 */
require_once 'modules/InboundEmail/InboundEmail.php';
require_once 'include/clean.php';
#[\AllowDynamicProperties]
class AOPInboundEmail extends InboundEmail
{
    public $job_name = 'function::pollMonitoredInboxesAOP';

    /**
     * Replaces embedded image links with links to the appropriate note in the CRM.
     * @param $string
     * @param $noteIds A whitelist of note ids to replace
     * @return mixed
     */
    public function processImageLinks($string, $noteIds)
    {
        global $sugar_config;
        if (!$noteIds) {
            return $string;
        }
        $matches = array();
        preg_match('/cid:([[:alnum:]-]*)/', (string) $string, $matches);
        if (!$matches) {
            return $string;
        }
        array_shift($matches);
        $matches = array_unique($matches);
        foreach ($matches as $match) {
            if (in_array($match, $noteIds)) {
                $string = str_replace('cid:'.$match, $sugar_config['site_url']."/index.php?entryPoint=download&id={$match}&type=Notes&", (string) $string);
            }
        }
        return $string;
    }


    public function handleCreateCase(Email $email, $userId)
    {
        global $current_user, $mod_strings, $current_language;
        $mod_strings = return_module_language($current_language, "Emails");
        $GLOBALS['log']->debug('In handleCreateCase in AOPInboundEmail');
        $c = BeanFactory::newBean('Cases');
        $this->getCaseIdFromCaseNumber($email->name, $c);

        $to = [];

        if (!$this->handleCaseAssignment($email) && $this->isMailBoxTypeCreateCase()) {
            // create a case
            $GLOBALS['log']->debug('retrieveing email');
            $email->retrieve($email->id);
            $c = BeanFactory::newBean('Cases');

            $notes = $email->get_linked_beans('notes', 'Notes');
            $noteIds = array();
            foreach ($notes as $note) {
                $noteIds[] = $note->id;
            }
            if ($email->description_html) {
                $c->description = $this->processImageLinks(SugarCleaner::cleanHtml($email->description_html), $noteIds);
            } else {
                $c->description = $email->description;
            }
            $c->assigned_user_id = $userId;
            $c->name = $email->name;
            $c->status = 'Open_New';

            if (!empty($email->reply_to_email)) {
                $contactAddr = $email->reply_to_email;
            } else {
                $contactAddr = $email->from_addr;
            }
            isValidEmailAddress($contactAddr);

            $GLOBALS['log']->debug('finding related accounts with address ' . $contactAddr);
            if ($accountIds = $this->getRelatedId($contactAddr, 'accounts')) {
                if ((is_countable($accountIds) ? count($accountIds) : 0) == 1) {
                    $c->account_id = $accountIds[0];

                    $acct = BeanFactory::newBean('Accounts');
                    $acct->retrieve($c->account_id);
                    $c->account_name = $acct->name;
                } // if
            } // if
            $contactIds = $this->getRelatedId($contactAddr, 'contacts');
            if (!empty($contactIds)) {
                $c->contact_created_by_id = $contactIds[0];
            }

            $c->save(true);
            $c->retrieve($c->id);
            if ($c->load_relationship('emails')) {
                $c->emails->add($email->id);
            } // if
            if (!empty($contactIds) && $c->load_relationship('contacts')) {
                if (!$accountIds && (is_countable($contactIds) ? count($contactIds) : 0) == 1) {
                    $contact = BeanFactory::getBean('Contacts', $contactIds[0]);
                    if ($contact->load_relationship('accounts')) {
                        $acct = $contact->accounts->get();
                        if ($c->load_relationship('accounts') && !empty($acct[0])) {
                            $c->accounts->add($acct[0]);
                        }
                    }
                }
                $c->contacts->add($contactIds);
            } // if
            foreach ($notes as $note) {
                //Link notes to case also
                $newNote = BeanFactory::newBean('Notes');
                $newNote->name = $note->name;
                $newNote->file_mime_type = $note->file_mime_type;
                $newNote->filename = $note->filename;
                $newNote->parent_type = 'Cases';
                $newNote->parent_id = $c->id;
                $newNote->save();
                $srcFile = "upload://{$note->id}";
                $destFile = "upload://{$newNote->id}";
                copy($srcFile, $destFile);
            }

            $c->email_id = $email->id;
            $email->parent_type = "Cases";
            $email->parent_id = $c->id;
            // assign the email to the case owner
            $email->assigned_user_id = $c->assigned_user_id;
            $email->name = str_replace('%1', $c->case_number, (string) $c->getEmailSubjectMacro()) . " ". $email->name;
            $email->save();
            $GLOBALS['log']->debug('InboundEmail created one case with number: '.$c->case_number);
            $createCaseTemplateId = $this->get_stored_options('create_case_email_template', "");
            if (!empty($this->stored_options)) {
                $storedOptions = unserialize(base64_decode($this->stored_options));
            }
            if (!empty($createCaseTemplateId)) {
                $fromName = "";
                $fromAddress = "";
                if (!empty($this->stored_options)) {
                    $fromAddress = $storedOptions['from_addr'];
                    isValidEmailAddress($fromAddress);
                    $fromName = from_html($storedOptions['from_name']);
                    $replyToName = (!empty($storedOptions['reply_to_name']))? from_html($storedOptions['reply_to_name']) :$fromName ;
                    $replyToAddr = (!empty($storedOptions['reply_to_addr'])) ? $storedOptions['reply_to_addr'] : $fromAddress;
                } // if
                $defaults = $current_user->getPreferredEmail();
                $fromAddress = (!empty($fromAddress)) ? $fromAddress : $defaults['email'];
                isValidEmailAddress($fromAddress);
                $fromName = (!empty($fromName)) ? $fromName : $defaults['name'];
                $to[0]['email'] = $contactAddr;

                // handle to name: address, prefer reply-to
                if (!empty($email->reply_to_name)) {
                    $to[0]['display'] = $email->reply_to_name;
                } elseif (!empty($email->from_name)) {
                    $to[0]['display'] = $email->from_name;
                }

                $et = BeanFactory::newBean('EmailTemplates');
                $et->retrieve($createCaseTemplateId);
                if (empty($et->subject)) {
                    $et->subject = '';
                }
                if (empty($et->body)) {
                    $et->body = '';
                }
                if (empty($et->body_html)) {
                    $et->body_html = '';
                }

                $et->subject = "Re:" . " " . str_replace('%1', $c->case_number, $c->getEmailSubjectMacro() . " ". $c->name);

                $html = trim($email->description_html);
                $plain = trim($email->description);

                $email->email2init();
                $email->from_addr = $email->from_addr_name;
                isValidEmailAddress($email->from_addr);
                $email->to_addrs = $email->to_addrs_names;
                $email->cc_addrs = $email->cc_addrs_names;
                $email->bcc_addrs = $email->bcc_addrs_names;
                $email->from_name = $email->from_addr;

                $email = $email->et->handleReplyType($email, "reply");
                $ret = $email->et->displayComposeEmail($email);
                $ret['description'] = empty($email->description_html) ?  str_replace("\n", "\n<BR/>", (string) $email->description) : $email->description_html;

                $reply = BeanFactory::newBean('Emails');
                $reply->type				= 'out';
                $reply->to_addrs			= $to[0]['email'];
                $reply->to_addrs_arr		= $to;
                $reply->cc_addrs_arr		= array();
                $reply->bcc_addrs_arr		= array();
                $reply->from_name			= $fromName;
                $reply->from_addr			= $fromAddress;
                $reply->reply_to_name		= $replyToName;
                $reply->reply_to_addr		= $replyToAddr;
                $reply->name				= $et->subject;
                $reply->description			= $et->body . "<div><hr /></div>" .  $email->description;
                if (!$et->text_only) {
                    $reply->description_html	= $et->body_html .  "<div><hr /></div>" . $email->description;
                }
                $GLOBALS['log']->debug('saving and sending auto-reply email');
                //$reply->save(); // don't save the actual email.
                $reply->send();
            } // if
        } else {
            echo "First if not matching\n";
            if (!empty($email->reply_to_email) && isValidEmailAddress($email->reply_to_email)) {
                $contactAddr = $email->reply_to_email;
            } elseif (!empty($email->from_addr) && isValidEmailAddress($email->from_addr)) {
                $contactAddr = $email->from_addr;
            } else {
                $contactAddr = null;
                LoggerManager::getLogger()->error('Contact address is incorrect to Email: ' . $email->id);
            }
            $this->handleAutoresponse($email, $contactAddr);
        }
        echo "End of handle create case\n";
    } // fn
}
