<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

//Shorten name.
$data = $_REQUEST;

if (!empty($data['listViewExternalClient'])) {
    $email = BeanFactory::newBean('Emails');
    echo $email->getNamePlusEmailAddressesForCompose($_REQUEST['action_module'], (explode(",", $_REQUEST['uid'])));
}
//For the full compose/email screen, the compose package is generated and script execution
//continues to the Emails/index.php page.
elseif (!isset($data['forQuickCreate'])) {
    $ret = generateComposeDataPackage($data);
}

/**
 * Initialize the full compose window by creating the compose package
 * and then including Emails index.php file.
 *
 * @param Array $ret
 */
function initFullCompose($ret)
{
    global $current_user;
    $json = getJSONobj();
    $composeOut = $json->encode($ret);

    //For listview 'Email' call initiated by subpanels, just returned the composePackage data, do not
    //include the entire Emails page
    if (isset($_REQUEST['ajaxCall']) && $_REQUEST['ajaxCall']) {
        echo $composeOut;
    } else {
        //For normal full compose screen
        echo "<script type='text/javascript' language='javascript'>\ncomposePackage = {$composeOut};\n</script>";
    }
}

/**
 * Generate the compose data package consumed by the full and quick compose screens.
 *
 * @param Array $data
 * @param Bool $forFullCompose If full compose is set to TRUE, then continue execution and include the full Emails UI.  Otherwise
 *             the data generated is returned.
 */
function generateComposeDataPackage($data, $forFullCompose = true)
{
    // we will need the following:
    if (isset($data['parent_type']) && !empty($data['parent_type']) &&
        isset($data['parent_id']) && !empty($data['parent_id']) &&
        !isset($data['ListView']) && !isset($data['replyForward'])
    ) {
        global $beanList;
        global $beanFiles;
        global $mod_strings;

        $parentName = '';
        $class = $beanList[$data['parent_type']];
        require_once($beanFiles[$class]);

        $bean = new $class();
        $bean->retrieve($data['parent_id']);
        if (isset($bean->full_name)) {
            $parentName = $bean->full_name;
        } elseif (isset($bean->name)) {
            $parentName = $bean->name;
        } else {
            $parentName = '';
        }
        $parentName = from_html($parentName);
        $namePlusEmail = '';
        if (isset($data['to_email_addrs'])) {
            $namePlusEmail = $data['to_email_addrs'];
            $namePlusEmail = from_html(str_replace("&nbsp;", " ", (string) $namePlusEmail));
        } else {
            if (isset($bean->full_name)) {
                $namePlusEmail = from_html($bean->full_name) . " <" . from_html($bean->emailAddress->getPrimaryAddress($bean)) . ">";
            } elseif (isset($bean->emailAddress)) {
                $namePlusEmail = "<" . from_html($bean->emailAddress->getPrimaryAddress($bean)) . ">";
            }
        }

        $subject = "";
        $body = "";
        $email_id = "";
        $attachments = array();
        if ($bean->module_dir == 'Cases') {
            $subject = str_replace('%1', $bean->case_number, $bean->getEmailSubjectMacro() . " " . from_html($bean->name));//bug 41928
            $bean->load_relationship("contacts");
            $contact_ids = $bean->contacts->get();
            $contact = BeanFactory::newBean('Contacts');
            foreach ($contact_ids as $cid) {
                $contact->retrieve($cid);
                $namePlusEmail .= empty($namePlusEmail) ? "" : ", ";
                $namePlusEmail .= from_html($contact->full_name) . " <" . from_html($contact->emailAddress->getPrimaryAddress($contact)) . ">";
            }
        }
        if ($bean->module_dir == 'KBDocuments') {
            require_once("modules/Emails/EmailUI.php");
            $subject = $bean->kbdocument_name;
            $article_body = str_replace('/' . $GLOBALS['sugar_config']['cache_dir'] . 'images/', $GLOBALS['sugar_config']['site_url'] . '/' . $GLOBALS['sugar_config']['cache_dir'] . 'images/', (string) KBDocument::get_kbdoc_body_without_incrementing_count($bean->id));
            $body = from_html($article_body);
            $attachments = KBDocument::get_kbdoc_attachments_for_newemail($bean->id);
            $attachments = $attachments['attachments'];
        } // if
        if ($bean->module_dir == 'Quotes' && isset($data['recordId'])) {
            $quotesData = getQuotesRelatedData($data);
            global $current_language;
            $namePlusEmail = $quotesData['toAddress'];
            $subject = $quotesData['subject'];
            $body = $quotesData['body'];
            $attachments = $quotesData['attachments'];
            $email_id = $quotesData['email_id'];
        } // if
        $ret = array(
            'to_email_addrs' => $namePlusEmail,
            'parent_type' => $data['parent_type'],
            'parent_id' => $data['parent_id'],
            'parent_name' => $parentName,
            'subject' => $subject,
            'body' => $body,
            'attachments' => $attachments,
            'email_id' => $email_id,

        );
    } elseif (isset($data['recordId'])) {
        $quotesData = getQuotesRelatedData($data);
        $namePlusEmail = $quotesData['toAddress'];
        $subject = $quotesData['subject'];
        $body = $quotesData['body'];
        $attachments = $quotesData['attachments'];
        $email_id = $quotesData['email_id'];

        $ret = array(
            'to_email_addrs' => $namePlusEmail,
            'parent_type' => $quotesData['parent_type'],
            'parent_id' => $quotesData['parent_id'],
            'parent_name' => $quotesData['parent_name'],
            'subject' => $subject,
            'body' => $body,
            'attachments' => $attachments,
            'email_id' => $email_id,
        );
    } elseif (isset($_REQUEST['ListView'])) {
        $email = BeanFactory::newBean('Emails');
        $namePlusEmail = $email->getNamePlusEmailAddressesForCompose($_REQUEST['action_module'], (explode(",", $_REQUEST['uid'])));
        $ret = array(
            'to_email_addrs' => $namePlusEmail,
        );
    } elseif (isset($data['replyForward'])) {
        require_once("modules/Emails/EmailUI.php");

        $ret = array();
        $ie = BeanFactory::newBean('InboundEmail');
        $ie->email = BeanFactory::newBean('Emails');
        $ie->email->email2init();
        $replyType = $data['reply'];
        $email_id = $data['record'];
        $ie->email->retrieve($email_id);
        $emailType = "";
        if ($ie->email->type == 'draft') {
            $emailType = $ie->email->type;
        }
        $ie->email->from_addr = $ie->email->from_addr_name;
        isValidEmailAddress($ie->email->from_addr);
        $ie->email->to_addrs = to_html($ie->email->to_addrs_names);
        $ie->email->cc_addrs = to_html($ie->email->cc_addrs_names);
        $ie->email->bcc_addrs = $ie->email->bcc_addrs_names;
        $ie->email->from_name = $ie->email->from_addr;
        $preBodyHTML = "&nbsp;<div><hr></div>";
        if ($ie->email->type != 'draft') {
            $email = $ie->email->et->handleReplyType($ie->email, $replyType);
        } else {
            $email = $ie->email;
            $preBodyHTML = "";
        } // else
        if ($ie->email->type != 'draft') {
            $emailHeader = $email->description;
        }
        $ret = $ie->email->et->displayComposeEmail($email);
        if ($ie->email->type != 'draft') {
            $ret['description'] = $emailHeader;
        }
        if ($replyType == 'forward' || $emailType == 'draft') {
            $ret = $ie->email->et->getDraftAttachments($ret);
        }
        $return = $ie->email->et->getFromAllAccountsArray($ie, $ret);

        if ($replyType == "forward") {
            $return['to'] = '';
        } else {
            if ($email->type != 'draft') {
                $return['to'] = from_html($ie->email->from_addr);
                isValidEmailAddress($return['to']);
            }
        } // else
        $ret = array(
            'to_email_addrs' => $return['to'],
            'parent_type' => $return['parent_type'],
            'parent_id' => $return['parent_id'],
            'parent_name' => $return['parent_name'],
            'subject' => html_entity_decode_utf8($return['name']),
            'body' => $preBodyHTML . $return['description'],
            'attachments' => (isset($return['attachments']) ? $return['attachments'] : array()),
            'email_id' => $email_id,
            'fromAccounts' => $return['fromAccounts'],
        );

        // If it's a 'Reply All' action, append the CC addresses
        if ($data['reply'] == 'replyAll') {
            global $current_user;

            $ccEmails = $ie->email->to_addrs;

            if (!empty($ie->email->cc_addrs)) {
                $ccEmails .= ", " . $ie->email->cc_addrs;
            }

            $myEmailAddresses = array();
            foreach ($current_user->emailAddress->addresses as $p) {
                array_push($myEmailAddresses, $p['email_address']);
            }

            //remove current user's email address (if contained in To/CC)
            $ccEmailsArr = explode(", ", $ccEmails);

            foreach ($ccEmailsArr as $p => $q) {
                preg_match('/<(.*?)>/', $q, $email);
                if (isset($email[1])) {
                    $checkemail = $email[1];
                } else {
                    $checkemail = $q;
                }
                if (in_array($checkemail, $myEmailAddresses)) {
                    unset($ccEmailsArr[$p]);
                }
            }

            $ccEmails = implode(", ", $ccEmailsArr);

            $ret['cc_addrs'] = from_html($ccEmails);
        }
    } else {
        $ret = array(
            'to_email_addrs' => '',
        );
    }

    if ($forFullCompose) {
        initFullCompose($ret);
    } else {
        return $ret;
    }
}

function getQuotesRelatedData($data)
{
    $return = array();
    $emailId = $data['recordId'];

    require_once("modules/Emails/EmailUI.php");
    $email = BeanFactory::newBean('Emails');
    $email->retrieve($emailId);
    $return['subject'] = $email->name;
    $return['body'] = from_html($email->description_html);
    $return['toAddress'] = $email->to_addrs;
    $ret = array();
    $ret['uid'] = $emailId;
    $ret = (new EmailUI())->getDraftAttachments($ret);
    $return['attachments'] = $ret['attachments'];
    $return['email_id'] = $emailId;
    $return['parent_type'] = $email->parent_type;
    $return['parent_id'] = $email->parent_id;
    $return['parent_name'] = '';

    if (isset($return['parent_type']) && !empty($return['parent_type']) && isset($return['parent_id']) && !empty($return['parent_id'])) {
        global $beanList;
        global $beanFiles;

        $class = $beanList[$return['parent_type']];
        require_once($beanFiles[$class]);

        $bean = new $class();
        $bean->retrieve($return['parent_id']);
        if (isset($bean->full_name)) {
            $parentName = $bean->full_name;
        } elseif (isset($bean->name)) {
            $parentName = $bean->name;
        } else {
            $parentName = '';
        }
        $parentName = from_html($parentName);

        $return['parent_name'] = $parentName;
        $return['toAddress'] = from_html($bean->full_name) . " <" . from_html($bean->emailAddress->getPrimaryAddress($bean)) . ">";
    }


    return $return;
}
// fn
