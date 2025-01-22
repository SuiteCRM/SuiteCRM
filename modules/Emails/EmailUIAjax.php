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

use SuiteCRM\Utility\SuiteValidator;

/**
 * handle requested subscriptions
 *
 * @param array $subs
 * @param Email $email
 * @param JSON $json
 * @param User|null $user
 * @return string JSON
 */
function handleSubs($subs, $email, $json, $user = null)
{

    // flows into next case statement
    $db = DBManagerFactory::getInstance();
    global $current_user;

    if (!$user) {
        $user = $current_user;
    }

    $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: setFolderViewSelection");
    $viewFolders = $subs;
    $user->setPreference('showFolders', base64_encode(serialize($viewFolders)), '', 'Emails');
    $tree = $email->et->getMailboxNodes(false);
    $return = $tree->generateNodesRaw();
    $out = $json->encode($return);


    $sub = array();
    foreach ($viewFolders as $f) {
        $query = 'SELECT * FROM folders WHERE folders.id LIKE "' . $f
            . '" OR folders.parent_folder LIKE "' . $f . '"';
        $result = $db->query($query);
        while (($row = $db->fetchByAssoc($result))) {
            $sub[] = $row['id'];
        }
    }

    $email->et->folder->setSubscriptions($sub, $user);

    return $out;
}


//increate timeout for phpo script execution
ini_set('max_execution_time', 300);
//ajaxInit();


require_once("include/OutboundEmail/OutboundEmail.php");
require_once("include/ytree/Tree.php");
require_once("include/ytree/ExtNode.php");

$email = BeanFactory::newBean('Emails');
$email->email2init();
$ie = BeanFactory::newBean('InboundEmail');
$ie->email = $email;
$json = getJSONobj();

global $current_user;

$showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));

if (isset($_REQUEST['emailUIAction'])) {
    if (isset($_REQUEST['user']) && $_REQUEST['user'] && is_admin($current_user)) {
        $cid = $current_user->id;
        $current_user = BeanFactory::getBean('Users', $_REQUEST['user']);
    } else {
        $GLOBALS['log']->debug(
            'If you are on user edit view and want to apply the email settings for the selected user,' .
            ' add user to request on user edit view: [emailUIAction:' . $_REQUEST['emailUIAction'] . ']'
        );
    }

    switch ($_REQUEST['emailUIAction']) {


        ///////////////////////////////////////////////////////////////////////////
        ////    COMPOSE REPLY FORWARD
        // this is used in forward/reply
        case "composeEmail":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: composeEmail");
            if (isset($_REQUEST['sugarEmail']) && $_REQUEST['sugarEmail'] == 'true' && isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $ie->email->retrieve($_REQUEST['uid']);
                $ie->email->from_addr = $ie->email->from_addr_name;
                isValidEmailAddress($ie->email->from_addr);
                $ie->email->to_addrs = to_html($ie->email->to_addrs_names);
                $ie->email->cc_addrs = to_html($ie->email->cc_addrs_names);
                $ie->email->bcc_addrs = $ie->email->bcc_addrs_names;
                $ie->email->from_name = $ie->email->from_addr;
                $email = $ie->email->et->handleReplyType($ie->email, $_REQUEST['composeType']);
                $ret = $ie->email->et->displayComposeEmail($email);
                $ret['description'] = empty($email->description_html) ? str_replace(
                    "\n",
                    "\n<BR/>",
                    (string) $email->description
                ) : $email->description_html;
                //get the forward header and add to description
                $forward_header = $email->getForwardHeader();

                $ret['description'] = $forward_header . $ret['description'];
                if ($_REQUEST['composeType'] == 'forward') {
                    $ret = $ie->email->et->getDraftAttachments($ret);
                }
                $ret = $ie->email->et->getFromAllAccountsArray($ie, $ret);
                $ret['from'] = from_html($ret['from']);
                $ret['name'] = from_html($ret['name']);
                $out = $json->encode($ret, true);
                echo $out;
            } elseif (isset($_REQUEST['uid']) && !empty($_REQUEST['uid']) && isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $ie->retrieve($_REQUEST['ieId']);
                $ie->mailbox = $_REQUEST['mbox'];
                global $timedate;
                $ie->setEmailForDisplay($_REQUEST['uid']);
                $ie->email->date_start = $timedate->to_display_date($ie->email->date_sent_received);
                $ie->email->time_start = $timedate->to_display_time($ie->email->date_sent_received);
                $ie->email->date_sent_received = $timedate->to_display_date_time($ie->email->date_sent_received);
                $email = $ie->email->et->handleReplyType($ie->email, $_REQUEST['composeType']);
                $ret = $ie->email->et->displayComposeEmail($email);
                if ($_REQUEST['composeType'] == 'forward') {
                    $ret = $ie->email->et->createCopyOfInboundAttachment($ie, $ret, $_REQUEST['uid']);
                }
                $ret = $ie->email->et->getFromAllAccountsArray($ie, $ret);
                $ret['from'] = from_html($ret['from']);
                $ret['name'] = from_html($ret['name']);
                $ret['ieId'] = $_REQUEST['ieId'];
                $ret['mbox'] = $_REQUEST['mbox'];
                $out = $json->encode($ret, true);
                echo $out;
            }
            break;

        /**
         * sendEmail handles both send and save draft duties
         */
        case "sendEmail":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: sendEmail");


            $sea = new SugarEmailAddress();

            $email->type = 'out';
            $email->status = 'sent';

            if (isset($_REQUEST['email_id']) && !empty($_REQUEST['email_id'])) {// && isset($_REQUEST['saveDraft']) && !empty($_REQUEST['saveDraft'])) {
                $email->retrieve($_REQUEST['email_id']); // uid is GUID in draft cases
            }
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $email->uid = $_REQUEST['uid'];
            }

            if ($email->email2Send($_REQUEST)) {
                $ret = array(
                    'composeLayoutId' => $_REQUEST['composeLayoutId'],
                );
                $out = $json->encode($ret, true);
                echo $out; // async call to close the proper compose tab
            }
            break;

        case "uploadAttachment":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: uploadAttachment");
            $metadata = $email->email2saveAttachment();

            if (!empty($metadata)) {
                $out = $json->encode($metadata);
                echo $out;
            }
            break;

        case "removeUploadedAttachment":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: removeUploadedAttachment");
            $fileFromRequest = from_html($_REQUEST['file']);
            $fileGUID = substr((string) $fileFromRequest, 0, 36);
            // Bug52727: sanitize fileGUID to remove path components: /\.
            $fileGUID = cleanDirName($fileGUID);
            $fileName = $email->et->userCacheDir . "/" . $fileGUID;
            $filePath = clean_path($fileName);
            unlink($filePath);
            break;

        case "fillComposeCache": // fills client-side compose email cache with signatures and email templates
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: fillComposeCache");
            $out = array();
            $email_templates_arr = $email->et->getEmailTemplatesArray();
            natcasesort($email_templates_arr);
            $out['emailTemplates'] = $email_templates_arr;
            $sigs = $current_user->getSignaturesArray();
            // clean "none"
            foreach ($sigs as $k => $v) {
                if ($k == "") {
                    $sigs[$k] = $app_strings['LBL_NONE'];
                } else {
                    if (is_array($v) && isset($v['name'])) {
                        $sigs[$k] = $v['name'];
                    } else {
                        $sigs[$k] = $v;
                    }
                }
            }
            $out['signatures'] = $sigs;
            $out['fromAccounts'] = $email->et->getFromAccountsArray($ie);
            $out['errorArray'] = array();

            $oe = new OutboundEmail();
            if ($oe->doesUserOverrideAccountRequireCredentials($current_user->id)) {
                $overideAccount = $oe->getUsersMailerForSystemOverride($current_user->id);
                //If the user override account has not been created yet, create it for the user.
                if ($overideAccount == null) {
                    $overideAccount = $oe->createUserSystemOverrideAccount($current_user->id);
                }

                $out['errorArray'] = array($overideAccount->id => $app_strings['LBL_EMAIL_WARNING_MISSING_USER_CREDS']);
            }

            $ret = $json->encode($out);
            echo $ret;
            break;

        case "getSignature":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getSignature");
            if (isset($_REQUEST['id'])) {
                $signature = $current_user->getSignature($_REQUEST['id']);
                $signature['signature_html'] = from_html($signature['signature_html']);
                $out = $json->encode($signature);
                echo $out;
            } else {
                die();
            }
            break;

    case "deleteSignature":
        $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: deleteSignature");
        if (isset($_REQUEST['id'])) {
            require_once("modules/Users/UserSignature.php");
            $us = new UserSignature();
            $us->mark_deleted($_REQUEST['id']);
            $signatureArray = $current_user->getSignaturesArray();
            // clean "none"
            foreach ($signatureArray as $k => $v) {
                if ($k == "") {
                    $sigs[$k] = $app_strings['LBL_NONE'];
                } else {
                    if (is_array($v) && isset($v['name'])) {
                        $sigs[$k] = $v['name'];
                    } else {
                        $sigs[$k] = $v;
                    }
                }
            }
            $out['signatures'] = $signatureArray;
            $ret = $json->encode($out);
            echo $ret;
        } else {
            die();
        }
        break;
    case 'getTemplateAttachments':
        $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getTemplateAttachments");
        if (isset($_REQUEST['parent_id']) && !empty($_REQUEST['parent_id'])) {
            $db = DBManagerFactory::getInstance();


            $where = "parent_id='{$db->quote($_REQUEST['parent_id'])}'";
            $order = '';
            $seed = BeanFactory::newBean('Notes');
            $fullList = $seed->get_full_list($order, $where, '');
            $all_fields = array_merge($seed->column_fields, $seed->additional_column_fields);

            $js_fields_arr = array();

            $i=1; // js doesn't like 0 index?
            if (!empty($fullList)) {
                foreach ($fullList as $note) {
                    $js_fields_arr[$i] = array();

                    foreach ($all_fields as $field) {
                        if (isset($note->$field)) {
                            $note->$field = from_html($note->$field);
                            $note->$field = preg_replace('/\r\n/', '<BR>', (string) $note->$field);
                            $note->$field = preg_replace('/\n/', '<BR>', $note->$field);
                            $js_fields_arr[$i][$field] = addslashes($note->$field);
                        }
                    }
                    $i++;
                }
            }

            $out = $json->encode($js_fields_arr);
            echo $out;
        }
        break;
        ////    END COMPOSE REPLY FORWARD
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    MESSAGE HANDLING
        case "displayView":
            $ret = array();
            $ie->retrieve($_REQUEST['ieId']);
            $ie->mailbox = $_REQUEST['mailbox'];
            $ie->connectMailserver();

            switch ($_REQUEST['type']) {
                case "headers":
                    $title = (string)($app_strings['LBL_EMAIL_VIEW_HEADERS']);
                    $text = $ie->getFormattedHeaders($_REQUEST['uid']);
                    break;

                case "raw":
                    $title = (string)($app_strings['LBL_EMAIL_VIEW_RAW']);
                    $text = $ie->getFormattedRawSource($_REQUEST['uid']);
                    break;

                case "printable":

                    break;
            }

            $ret['html'] = $text;
            $ret['title'] = $title;

            $out = $json->encode($ret, true);
            echo $out;
            break;

        case "getQuickCreateForm":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getQuickCreateForm");
            if (isset($_REQUEST['qc_module']) && !empty($_REQUEST['qc_module'])) {
                if (!ACLController::checkAccess($_REQUEST['qc_module'], 'edit', true)) {
                    echo trim($json->encode(array('html' => translate('LBL_NO_ACCESS', 'ACL')), true));
                    break;
                }
                $people = array("Users", "Contacts", "Leads", "Prospects");
                $showSaveToAddressBookButton = false;//(in_array($_REQUEST['qc_module'], $people)) ? true : false;

                if (isset($_REQUEST['sugarEmail']) && !empty($_REQUEST['sugarEmail'])) {
                    $ie->email->retrieve($_REQUEST['uid']); // uid is a sugar GUID in this case
                } else {
                    $ie->retrieve($_REQUEST['ieId']);
                    $ie->mailbox = $_REQUEST['mailbox'];
                    $ie->setEmailForDisplay($_REQUEST['uid']);
                }
                $ret = $email->et->getQuickCreateForm($_REQUEST, $ie->email, $showSaveToAddressBookButton);
                $ret['ieId'] = $_REQUEST['ieId'];
                $ret['mbox'] = $_REQUEST['mailbox'];
                $ret['uid'] = $_REQUEST['uid'];
                $ret['module'] = $_REQUEST['qc_module'];
                if (!isset($_REQUEST['iframe'])) {
                    $out = trim($json->encode($ret, false));
                } else {
                    $out = $ret['html'];
                }
                echo $out;
            }
            break;

        case 'saveQuickCreate':
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveQuickCreate");
            require_once('include/MVC/Controller/ControllerFactory.php');
            if (isset($_REQUEST['qcmodule'])) {
                $GLOBALS['log']->debug("********** QCmodule was set: {$_REQUEST['qcmodule']}");
            }
            $controller = ControllerFactory::getController($_REQUEST['qcmodule']);
            $controller->loadBean();
            $controller->pre_save();
            $GLOBALS['log']->debug("********** saving new {$controller->module}");
            $controller->action_save();
            //Relate the email to the new bean
            if (isset($_REQUEST['sugarEmail']) && $_REQUEST['sugarEmail'] == 'true' && isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $ie->email->retrieve($_REQUEST['uid']);
            } elseif (isset($_REQUEST['uid']) && !empty($_REQUEST['uid']) && isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $GLOBALS['log']->debug("********** Quick Create from non-imported message");
                $ie->retrieve($_REQUEST['ieId']);
                $ie->mailbox = $_REQUEST['mbox'];
                $ie->connectMailserver();
                $uid = $_REQUEST['uid'];
                if ($ie->protocol == 'imap') {
                    $_REQUEST['uid'] = $ie->getImap()->getMessageNo($_REQUEST['uid']);
                } else {
                    $_REQUEST['uid'] = $ie->getCorrectMessageNoForPop3($_REQUEST['uid']);
                }

                if (!$ie->returnImportedEmail($_REQUEST['uid'], $uid)) {
                    $ie->getDuplicateEmailId($_REQUEST['uid'], $uid);
                } // id
                $ie->email->retrieve($ie->email->id);
                $GLOBALS['log']->debug("**********Imported Email");
                $ie->email->assigned_user_id = $controller->bean->assigned_user_id;
                $ie->email->assigned_user_name = $controller->bean->assigned_user_name;
            }
            if (isset($ie->email->id)) {
                if (empty($ie->email->parent_id)) {
                    $ie->email->parent_id = $controller->bean->id;
                    $ie->email->parent_type = $controller->module;
                } // if
                $ie->email->status = 'read';
                $ie->email->save();
                $mod = strtolower($controller->module);
                $ie->email->load_relationship($mod);
                $ie->email->$mod->add($controller->bean->id);
                if ($controller->bean->load_relationship('emails')) {
                    $controller->bean->emails->add($ie->email->id);
                }
                if ($controller->bean->module_dir == 'Cases') {
                    if ($controller->bean->load_relationship('contacts')) {
                        $emailAddressWithName = $ie->email->from_addr;
                        if (!empty($ie->email->reply_to_addr)) {
                            $emailAddressWithName = $ie->email->reply_to_addr;
                        } // if

                        $emailAddress = SugarEmailAddress::_cleanAddress($emailAddressWithName);
                        $contactIds = $ie->email->emailAddress->getRelatedId($emailAddress, 'contacts');
                        if (!empty($contactIds)) {
                            $controller->bean->contacts->add($contactIds);
                        } // if
                    } // if
                } // if
                echo $json->encode(array('id' => $ie->email->id));
            }
            break;

        case "getImportForm":
            $ie->retrieve($_REQUEST['ieId']);
            //            $ie->mailbox = $_REQUEST['mailbox'];
            $ie->setEmailForDisplay($_REQUEST['uid']);
            $ret = $email->et->getImportForm($_REQUEST, $ie->email);
            $out = trim($json->encode($ret, false));
            echo $out;
            break;

        case "getRelateForm":
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $uids = $json->decode(from_html($_REQUEST['uid']));
                $email->retrieve($uids[0]);
                $ret = $email->et->getImportForm(array(
                    'showTeam' => false,
                    'showAssignTo' => false,
                    'showDelete' => false
                ), $email, 'RelateEditView');
                $out = trim($json->encode($ret, false));
                echo $out;
            }
            break;

        case "getEmail2DetailView":
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $ret = $email->et->getDetailViewForEmail2($_REQUEST['uid']);
                if (!isset($_REQUEST['print']) || $_REQUEST['print'] === false) {
                    $out = trim($json->encode($ret, false));
                    echo $out;
                } else {
                    echo $ret['html'];
                }
            }
            break;

        case "relateEmails":
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid']) &&
                isset($_REQUEST['parent_id']) && !empty($_REQUEST['parent_id']) &&
                isset($_REQUEST['parent_type']) && !empty($_REQUEST['parent_type'])
            ) {
                $uids = explode($app_strings['LBL_EMAIL_DELIMITER'], $_REQUEST['uid']);
                $mod = strtolower($_REQUEST['parent_type']);
                $modId = $_REQUEST['parent_id'];
                foreach ($uids as $id) {
                    $email = BeanFactory::newBean('Emails');
                    $email->retrieve($id);
                    $email->parent_id = $modId;
                    $email->parent_type = $_REQUEST['parent_type'];
                    $email->status = 'read';

                    // BUG FIX BEGIN
                    // Bug 50979 - relating a message in group inbox removes message
                    if (empty($email->assigned_user_id)) {
                        $email->setFieldNullable('assigned_user_id');
                    }
                    $email->save();
                    // Bug 50979 - reset assigned_user_id field defs
                    if (empty($email->assigned_user_id)) {
                        $email->revertFieldNullable('assigned_user_id');
                    }
                    // BUG FIX END

                    $email->load_relationship($mod);
                    $email->$mod->add($modId);
                }
            }
            break;


        case "getAssignmentDialogContent":
            $out = $email->distributionForm("");
            $out = trim($json->encode($out, false));
            echo $out;
            break;
        case "doAssignmentAssign":
            $out = $email->et->doAssignment(
                $_REQUEST['distribute_method'],
                $_REQUEST['ieId'],
                $_REQUEST['folder'],
                $_REQUEST['uids'],
                $_REQUEST['users']
            );
            echo $out;
            break;
        case "doAssignmentDelete":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: doAssignmentDelete");
            if (isset($_REQUEST['uids']) && !empty($_REQUEST['uids']) &&
                isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId']) &&
                isset($_REQUEST['folder']) && !empty($_REQUEST['folder'])
            ) {
                $email->et->markEmails("deleted", $_REQUEST['ieId'], $_REQUEST['folder'], $_REQUEST['uids']);
            }

            break;
        case "markEmail":
            global $app_strings;
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: markEmail");
            if (isset($_REQUEST['uids']) && !empty($_REQUEST['uids']) &&
                isset($_REQUEST['type']) && !empty($_REQUEST['type']) &&
                isset($_REQUEST['folder']) && !empty($_REQUEST['folder']) &&
                isset($_REQUEST['ieId']) && (!empty($_REQUEST['ieId']) || (empty($_REQUEST['ieId']) && strpos(
                    (string) $_REQUEST['folder'],
                    'sugar::'
                ) !== false))
            ) {
                $uid = $json->decode(from_html($_REQUEST['uids']));
                $uids = array();
                if (is_array($uid)) {
                    $uids = $uid;
                } else {
                    if (strpos((string) $uid, (string) $app_strings['LBL_EMAIL_DELIMITER']) !== false) {
                        $uids = explode($app_strings['LBL_EMAIL_DELIMITER'], $uid);
                    } else {
                        $uids[] = $uid;
                    }
                }   // else
                $uids = implode($app_strings['LBL_EMAIL_DELIMITER'], $uids);
                $GLOBALS['log']->debug("********** EMAIL 2.0 - Marking emails $uids as {$_REQUEST['type']}");

                $ret = array();
                if (strpos(
                    (string) $_REQUEST['folder'],
                    'sugar::'
                ) !== false && ($_REQUEST['type'] == 'deleted') && !ACLController::checkAccess(
                    'Emails',
                    'delete'
                        )
                ) {
                    $ret['status'] = false;
                    $ret['message'] = $app_strings['LBL_EMAIL_DELETE_ERROR_DESC'];
                } else {
                    $email->et->markEmails($_REQUEST['type'], $_REQUEST['ieId'], $_REQUEST['folder'], $uids);
                    $ret['status'] = true;
                }
                $out = trim($json->encode($ret, false));
                echo $out;
            } else {
            }

            break;

        case "checkEmail2":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: checkEmail2");

            $showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));

            $ret = array();
            $ret['numberAccounts'] = is_countable($showFolders) ? count($showFolders) : 0;

            $GLOBALS['log']->info("EMAIL2.0: async checkEmail - found [ " . $ret['numberAccounts'] . " ] accounts to check");

            if (!empty($showFolders) && is_array($showFolders)) {
                foreach ($showFolders as $ieId) {
                    $ieId = trim($ieId);

                    if (!empty($ieId)) {
                        $GLOBALS['log']->info("INBOUNDEMAIL: trying to check email for GUID [ {$ieId} ]");
                        $ie->disconnectMailserver();
                        $ie->retrieve($ieId);

                        $ret[$ieId] = $ie->checkEmail2_meta();
                    }
                }
            } else {
                $GLOBALS['log']->info("EMAIL2.0: at checkEmail() async call - not subscribed accounts to check.");
            }


            $out = $json->encode($ret, true);
            echo $out;
            break;

        case "checkEmail":
            $GLOBALS['log']->info("[EMAIL] - Start checkEmail action for user [{$current_user->user_name}]");
            if (isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $ie->retrieve($_REQUEST['ieId']);
                $ie->mailbox = (isset($_REQUEST['mbox']) && !empty($_REQUEST['mbox'])) ? $_REQUEST['mbox'] : "INBOX";
                $ie->checkEmail(false);
            } elseif (isset($_REQUEST['all']) && !empty($_REQUEST['all'])) {
                $showFolders = sugar_unserialize(base64_decode($current_user->getPreference('showFolders', 'Emails')));

                $GLOBALS['log']->info("[EMAIL] - checkEmail found " . (is_countable($showFolders) ? count($showFolders) : 0) . " accounts to check for user [{$current_user->user_name}]");

                if (!empty($showFolders) && is_array($showFolders)) {
                    foreach ($showFolders as $ieId) {
                        $ieId = trim($ieId);
                        if (!empty($ieId)) {
                            $GLOBALS['log']->info("[EMAIL] - Start checking email for GUID [{$ieId}] for user [{$current_user->user_name}]");
                            $ie->disconnectMailserver();
                            // If I-E not exist - skip check
                            if (is_null($ie->retrieve($ieId))) {
                                $GLOBALS['log']->info("[EMAIL] - Inbound with GUID [{$ieId}] not exist");
                                continue;
                            }
                            $ie->checkEmail(false);
                            $GLOBALS['log']->info("[EMAIL] - Done checking email for GUID [{$ieId}] for user [{$current_user->user_name}]");
                        }
                    }
                } else {
                    $GLOBALS['log']->info("EMAIL2.0: at checkEmail() async call - not subscribed accounts to check.");
                }
            }

            $tree = $email->et->getMailboxNodes(true); // preserve cache files
            $return = $tree->generateNodesRaw();
            $out = $json->encode($return);
            echo $out;
            $GLOBALS['log']->info("[EMAIL] - Done checkEmail action for user [{$current_user->user_name}]");
            break;

        case "checkEmailProgress":
            $GLOBALS['log']->info("[EMAIL] - Start checkEmail action for user [{$current_user->user_name}]");
            if (isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $ie->retrieve($_REQUEST['ieId']);
                $ie->mailbox = (isset($_REQUEST['mbox']) && !empty($_REQUEST['mbox'])) ? $_REQUEST['mbox'] : "INBOX";
                $synch = (isset($_REQUEST['synch']) && ($_REQUEST['synch'] == "true"));
                if (!$ie->is_personal) {
                    $return = array('status' => "done");
                } else {
                    if ($ie->protocol == "pop3") {
                        $return = $ie->pop3_checkPartialEmail($synch);
                    } else {
                        $return = $ie->checkEmailIMAPPartial(false, $synch);
                    } // else
                } // if
                $return['ieid'] = $ie->id;
                $return['synch'] = $synch;
                if (isset($_REQUEST['totalcount']) && !empty($_REQUEST['totalcount']) && $_REQUEST['totalcount'] >= 0) {
                    if ($ie->protocol == "pop3") {
                        $return['totalcount'] = $_REQUEST['totalcount'];
                    } // else
                }
                echo $json->encode($return);
            } // if
            break;

        case "getAllFoldersTree":
            $tree = $email->et->getMailboxNodes(true); // preserve cache files
            $return = $tree->generateNodesRaw();
            $out = $json->encode($return);
            echo $out;
            $GLOBALS['log']->info("[EMAIL] - Done checkEmail action for user [{$current_user->user_name}]");
            break;

        case "synchronizeEmail":
            $GLOBALS['log']->info("[EMAIL] Start action synchronizeEmail for user [{$current_user->user_name}]");
            $ie->syncEmail(true);
            $tree = $email->et->getMailboxNodes(true);
            $return = $tree->generateNodesRaw();
            $out = $json->encode($return);
            echo $out;
            $GLOBALS['log']->info("[EMAIL] Done action synchronizeEmail for user [{$current_user->user_name}]");
            break;

        case "importEmail":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: importEmail");
            $ie->retrieve($_REQUEST['ieId']);
            $ie->mailbox = $_REQUEST['mbox'];
            $ie->connectMailserver();
            $return = array();
            $status = true;
            $count = 1;
            if (strpos((string) $_REQUEST['uid'], (string) $app_strings['LBL_EMAIL_DELIMITER']) !== false) {
                $exUids = explode($app_strings['LBL_EMAIL_DELIMITER'], $_REQUEST['uid']);
                foreach ($exUids as $msgNo) {
                    $uid = $msgNo;
                    if ($ie->protocol == 'imap') {
                        $msgNo = $ie->getImap()->getMessageNo($msgNo);
                        $status = $ie->returnImportedEmail($msgNo, $uid);
                    } else {
                        $status = $ie->returnImportedEmail($ie->getCorrectMessageNoForPop3($msgNo), $uid);
                    } // else
                    $return[] = $app_strings['LBL_EMAIL_MESSAGE_NO'] . " " . $count . ", " . $app_strings['LBL_STATUS'] . " " . ($status ? $app_strings['LBL_EMAIL_IMPORT_SUCCESS'] : $app_strings['LBL_EMAIL_IMPORT_FAIL']);
                    $count++;
                    if (($_REQUEST['delete'] == 'true') && $status && ($current_user->is_admin == 1 || $ie->group_id == $current_user->id)) {
                        $ie->deleteMessageOnMailServer($uid);
                        $ie->deleteMessageFromCache($uid);
                    } // if
                } // for
            } else {
                $msgNo = $_REQUEST['uid'];
                if ($ie->protocol == 'imap') {
                    $msgNo = $ie->getImap()->getMessageNo($_REQUEST['uid']);
                    $status = $ie->returnImportedEmail($msgNo, $_REQUEST['uid']);
                } else {
                    $status = $ie->returnImportedEmail($ie->getCorrectMessageNoForPop3($msgNo), $_REQUEST['uid']);
                } // else
                $return[] = $app_strings['LBL_EMAIL_MESSAGE_NO'] . " " . $count . ", " . $app_strings['LBL_STATUS'] . " " . ($status ? $app_strings['LBL_EMAIL_IMPORT_SUCCESS'] : $app_strings['LBL_EMAIL_IMPORT_FAIL']);

                if (($_REQUEST['delete'] == 'true') && $status && ($current_user->is_admin == 1 || $ie->group_id == $current_user->id)) {
                    $ie->deleteMessageOnMailServer($_REQUEST['uid']);
                    $ie->deleteMessageFromCache($_REQUEST['uid']);
                } // if
            } // else
            echo $json->encode($return);
            break;

        case "setReadFlag":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: setReadFlag");
            $ie->retrieve($_REQUEST['ieId']);
            $ie->setReadFlagOnFolderCache($_REQUEST['mbox'], $_REQUEST['uid']);
            $email->et->getListEmails($_REQUEST['ieId'], $_REQUEST['mbox'], 0, 'true');
            //unlink("{$cacheRoot}/{$_REQUEST['ieId']}/folders/{$_REQUEST['mbox']}.php");
            break;

        case "deleteMessage":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: deleteMessage");
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid']) && isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $ie->retrieve($_REQUEST['ieId']);
                $ie->mailbox = $_REQUEST['mbox'];

                if ($current_user->is_admin == 1 || $ie->group_id == $current_user->id) {
                    $ie->deleteMessageOnMailServer($_REQUEST['uid']);
                    $ie->deleteMessageFromCache($_REQUEST['uid']);
                } else {
                    $GLOBALS['log']->debug("*** ERROR: tried to delete an email for an account for which {$current_user->full_name} is not the owner!");
                    echo "NOOP: error see log";
                }
            } else {
                echo "error: missing credentials";
            }
            break;

        case "getSingleMessage":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getSingleMessage");
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid']) && isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                // this method needs to guarantee UTF-8 charset - encoding detection
                // and conversion is unreliable, and can break valid UTF-8 text
                $out = $email->et->getSingleMessage($ie);

                echo $json->encode($out);
            } else {
                echo "error: no UID";
            }
            break;

        case "getSingleMessageFromSugar":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getSingleMessageFromSugar");
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $email->retrieve($_REQUEST['uid']);
                //$email->description_html = from_html($email->description_html);
                $ie->email = $email;

                if ($email->status == 'draft' || $email->type == 'draft') {
                    // forcing an editview since we are looking at a draft message
                    global $current_user;
                    $ret = $ie->email->et->displayComposeEmail($email);

                    $ret = $ie->email->et->getDraftAttachments($ret, $ie);
                    $ret = $ie->email->et->getFromAllAccountsArray($ie, $ret);


                    $out = $json->encode($ret, true);
                    echo $out;
                } else {
                    $out = $ie->displayOneEmail($_REQUEST['uid'], $_REQUEST['mbox']);
                    $out['meta']['email']['description'] =
                        empty($email->description_html) ? str_replace(
                            "\n",
                            "\n<BR/>",
                            (string) $email->description
                        ) : $email->description_html;
                    $out['meta']['email']['date_start'] = $email->date_start;
                    $out['meta']['email']['time_start'] = $email->time_start;
                    $out['meta']['ieId'] = $_REQUEST['ieId'];
                    $out['meta']['mbox'] = $_REQUEST['mbox'];
                    $out['meta']['email']['toaddrs'] = $email->et->generateExpandableAddrs($out['meta']['email']['toaddrs']);
                    if (!empty($out['meta']['email']['cc_addrs'])) {
                        $ccs = $email->et->generateExpandableAddrs($out['meta']['email']['cc_addrs']);
                        $out['meta']['email']['cc'] = <<<eoq
        				<tr>
        					<td NOWRAP valign="top" class="displayEmailLabel">
        						{$app_strings['LBL_EMAIL_CC']}:
        					</td>
        					<td class="displayEmailValue">
        						{$ccs}
        					</td>
        				</tr>
eoq;
                    }
                    echo $json->encode($out);
                }
            } else {
                echo "error: no UID";
            }
            break;

        case "getMultipleMessages":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getMultipleMessages");
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid']) && isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $exUids = explode(",", $_REQUEST['uid']);

                $out = array();
                foreach ($exUids as $k => $uid) {
                    if ($email->et->validCacheFileExists(
                        $_REQUEST['ieId'],
                        'messages',
                        $_REQUEST['mbox'] . $uid . ".php"
                    )
                    ) {
                        $msg = $email->et->getCacheValue(
                            $_REQUEST['ieId'],
                            'messages',
                            $_REQUEST['mbox'] . $uid . ".php",
                            'out'
                        );
                    } else {
                        $ie->retrieve($_REQUEST['ieId']);
                        $ie->mailbox = $_REQUEST['mbox'];
                        $ie->setEmailForDisplay($uid, false, true);
                        $msg = $ie->displayOneEmail($uid, $_REQUEST['mbox']);
                        $email->et->writeCacheFile(
                            'out',
                            $msg,
                            $_REQUEST['ieId'],
                            'messages',
                            "{$_REQUEST['mbox']}{$uid}.php"
                        );
                    }

                    $out[] = $msg;
                }
                echo $json->encode($out);
            } else {
                echo "error: no UID";
            }
            break;

        case "getMultipleMessagesFromSugar":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getMultipleMessagesFromSugar");
            if (isset($_REQUEST['uid']) && !empty($_REQUEST['uid'])) {
                $exIds = explode(",", $_REQUEST['uid']);
                $out = array();

                foreach ($exIds as $id) {
                    $e = BeanFactory::newBean('Emails');
                    $e->retrieve($id);
                    $e->description_html = from_html($e->description_html);
                    $ie->email = $e;
                    $out[] = $ie->displayOneEmail($id, $_REQUEST['mbox']);
                }

                echo $json->encode($out);
            }

            break;
        ////    END MESSAGE HANDLING
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    LIST VIEW
        case "getMessageCount":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getMessageCount");

            $out = $ie->getCacheCount($_REQUEST['mbox']);
            echo $json->encode($out);
            break;

        case "getMessageList":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getMessageListJSON");
            if (isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                // user view preferences
                $email->et->saveListView($_REQUEST['ieId'], $_REQUEST['mbox']);
                // list output
                $ie->retrieve($_REQUEST['ieId']);
                if (isset($_REQUEST['start']) && isset($_REQUEST['limit'])) {
                    $page = ceil($_REQUEST['start'] / $_REQUEST['limit']) + 1;
                } else {
                    $page = 1;
                }
                $list = $ie->displayFolderContents($_REQUEST['mbox'], $_REQUEST['forceRefresh'], $page);
                $count = $ie->getCacheCount($_REQUEST['mbox']);
                $unread = $ie->getCacheUnread($_REQUEST['mbox']);
                $out = $email->et->jsonOuput($list, 'Email', $count, true, $unread);

                @ob_end_clean();
                ob_start();
                echo $out;
                ob_end_flush();
            //die();
            } else {
                echo $msg = 'error: no ieID';
                $GLOBALS['log']->error($msg);
            }
            break;

        case "getMessageListSugarFolders":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getMessageListSugarFoldersJSON");
            if (isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                // user view preferences
                $email->et->saveListView($_REQUEST['ieId'], "SUGAR.{$_REQUEST['mbox']}");
                if (isset($_REQUEST['start']) && isset($_REQUEST['limit'])) {
                    $page = ceil($_REQUEST['start'] / $_REQUEST['limit']) + 1;
                } else {
                    $page = 1;
                }
                if (!isset($_REQUEST['sort']) || !isset($_REQUEST['dir'])) {
                    $_REQUEST['sort'] = '';
                    $_REQUEST['dir'] = '';
                }
                $emailSettings = $current_user->getPreference('emailSettings', 'Emails');
                // cn: default to a low number until user specifies otherwise
                if (empty($emailSettings['showNumInList'])) {
                    $emailSettings['showNumInList'] = 20;
                }

                // jchi #9424, get sort and direction from user preference
                $sort = 'date';
                $direction = 'desc';
                $sortSerial = $current_user->getPreference('folderSortOrder', 'Emails');
                if (!empty($sortSerial) && !empty($_REQUEST['ieId']) && !empty($_REQUEST['mbox'])) {
                    $sortArray = sugar_unserialize($sortSerial);
                    $GLOBALS['log']->debug("********** EMAIL 2.0********** ary=" . print_r(
                        $sortArray,
                        true
                    ) . ' id=' . $_REQUEST['ieId'] . '; box=' . $_REQUEST['mbox']);
                    $sort = $sortArray[$_REQUEST['ieId']][$_REQUEST['mbox']]['current']['sort'];
                    $direction = $sortArray[$_REQUEST['ieId']][$_REQUEST['mbox']]['current']['direction'];
                }
                //set sort and direction to user predference
                if (!empty($_REQUEST['sort']) && !empty($_REQUEST['dir'])) {
                    $email->et->saveListViewSortOrder(
                        $_REQUEST['ieId'],
                        $_REQUEST['mbox'],
                        $_REQUEST['sort'],
                        $_REQUEST['dir']
                    );
                    $sort = $_REQUEST['sort'];
                    $direction = $_REQUEST['dir'];
                } else {
                    $_REQUEST['sort'] = '';
                    $_REQUEST['dir'] = '';
                }
                //end

                $metalist = $email->et->folder->getListItemsForEmailXML(
                    $_REQUEST['ieId'],
                    $page,
                    $emailSettings['showNumInList'],
                    $sort,
                    $direction
                );
                $count = $email->et->folder->getCountItems($_REQUEST['ieId']);

                if (!empty($_REQUEST['getUnread'])) {
                    $out = $email->et->jsonOuput($metalist, 'Email', $count, false);
                } else {
                    $unread = $email->et->folder->getCountUnread($_REQUEST['ieId']);
                    $out = $email->et->jsonOuput($metalist, 'Email', $count, false, $unread);
                }

                @ob_end_clean();
                ob_start();
                echo $out;
                ob_end_flush();
            } else {
                echo "error: no ieID";
            }
            break;
        ////    END LIST VIEW
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    FOLDER ACTIONS
        case "emptyTrash":
            $email->et->emptyTrash($ie);
            break;

        case "clearInboundAccountCache":
            $email->et->clearInboundAccountCache($_REQUEST['ieId']);
            break;

        case "updateSubscriptions":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: updateSubscriptions");
            if (isset($_REQUEST['subscriptions']) && !empty($_REQUEST['subscriptions'])) {
                $subs = explode("::", $_REQUEST['subscriptions']);
                $childrenSubs = array();
                //Find all children of the group folder subscribed to and add
                //them to the list of folders to show.
                foreach ($subs as $singleSub) {
                    $email->et->folder->findAllChildren($singleSub, $childrenSubs);
                }

                $subs = array_merge($subs, $childrenSubs);
                $email->et->folder->setSubscriptions($subs);

                $out = handleSubs($subs, $email, $json);
            } elseif (empty($_REQUEST['subscriptions'])) {
                $email->et->folder->clearSubscriptions();
            } else {
                $GLOBALS['log']->fatal('Incorrect request for update subscriptions');
            }
            break;

        case "refreshSugarFolders":
            try {
                $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: refreshSugarFolders");
                $rootNode = new ExtNode('', '');
                $folderOpenState = $current_user->getPreference('folderOpenState', 'Emails');
                $folderOpenState = (empty($folderOpenState)) ? "" : $folderOpenState;
                $ret = $email->et->folder->getUserFolders(
                    $rootNode,
                    sugar_unserialize($folderOpenState),
                    $current_user,
                    true
                );
                $out = $json->encode($ret);
                echo $out;
            } catch (SugarFolderEmptyException $e) {
                $GLOBALS['log']->warn($e);
                $out = $json->encode(array(
                    'message' => 'No folder selected warning message here...',
                ));
                echo $out;
            }
            break;


        case "getFoldersForSettings":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getFoldersForSettings");
            $ret = $email->et->folder->getFoldersForSettings($current_user);
            $out = $json->encode($ret);
            echo $out;
            break;

        case "moveEmails":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: moveEmails");
            $ie->moveEmails(
                $_REQUEST['sourceIeId'],
                $_REQUEST['sourceFolder'],
                $_REQUEST['destinationIeId'],
                $_REQUEST['destinationFolder'],
                $_REQUEST['emailUids']
            );
            break;

        case "saveNewFolder":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveNewFolder");
            if (isset($_REQUEST['folderType']) && !empty($_REQUEST['folderType'])) {
                switch ($_REQUEST['folderType']) {
                    case "sugar":
                        $ret = $email->et->saveNewFolder($_REQUEST['nodeLabel'], $_REQUEST['parentId']);
                        $out = $json->encode($ret);
                        echo $out;
                        break;

                    case "imap":
                        $ie->retrieve($_REQUEST['ieId']);
                        $ie->connectMailserver();
                        $ie->saveNewFolder($_REQUEST['newFolderName'], $_REQUEST['mbox']);
                        break;
                }
            } else {
                echo "NOOP: no folderType defined";
            }
            break;

        case "setFolderViewSelection":
            $isValidator = new SuiteValidator();
            $user =
                isset($_REQUEST['user']) && $_REQUEST['user'] && $isValidator->isValidId($_REQUEST['user']) ?
                    BeanFactory::getBean('Users', $_REQUEST['user']) :
                    $current_user;

            $out = handleSubs($_REQUEST['ieIdShow'], $email, $json, $user);

            echo $out;
            break;

        case "deleteFolder":
            $v = $app_strings['LBL_NONE'];
            $return = array();
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: deleteFolder");
            if (isset($_REQUEST['folderType']) && !empty($_REQUEST['folderType'])) {
                switch ($_REQUEST['folderType']) {
                    case "sugar":
                        $status = $email->et->folder->deleteChildrenCascade($_REQUEST['folder_id']);
                        if ($status == true) {
                            $return['status'] = true;
                            $return['folder_id'] = $_REQUEST['folder_id'];
                        } else {
                            $return['status'] = false;
                            $return['errorMessage'] = $app_strings['LBL_EMAIL_ERROR_DELETE_GROUP_FOLDER'];
                        }
                        break;

                    case "imap":
                        $ie->retrieve($_REQUEST['ieId']);
                        $ie->connectMailserver();
                        $returnArray = $ie->deleteFolder($_REQUEST['mbox']);
                        $status = $returnArray['status'];
                        $errorMessage = $returnArray['errorMessage'];
                        if ($status == true) {
                            $return['status'] = true;
                            $return['mbox'] = $_REQUEST['mbox'];
                            $return['ieId'] = $_REQUEST['ieId'];
                        } else {
                            $return['status'] = false;
                            $return['errorMessage'] = $errorMessage;
                        }
                        break;
                }
            } else {
                $return['status'] = false;
                $return['errorMessage'] = "NOOP: no folderType defined";
            }
            $out = $json->encode($return);
            echo $out;
            break;
        case "renameFolder":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: renameFolder");

            if (isset($_REQUEST['ieId']) && isset($_REQUEST['oldFolderName']) && !empty($_REQUEST['oldFolderName'])
                && isset($_REQUEST['newFolderName']) && !empty($_REQUEST['newFolderName'])
            ) {
                $ie->retrieve($_REQUEST['ieId']);
                $ie->renameFolder($_REQUEST['oldFolderName'], $_REQUEST['newFolderName']);
            } elseif (isset($_REQUEST['folderId']) && !empty($_REQUEST['folderId']) && isset($_REQUEST['newFolderName']) && !empty($_REQUEST['newFolderName'])) {
                if (is_guid($_REQUEST['folderId'])) {
                    $email->et->folder->retrieve($_REQUEST['folderId']);
                    $email->et->folder->name = $_REQUEST['newFolderName'];
                    $email->et->folder->save();
                } else {
                    echo "NOOP - not a Sugar Folder";
                }
            }
            // no break
        case "moveFolder":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: moveFolder");
            if (isset($_REQUEST['folderId']) && !empty($_REQUEST['folderId']) && isset($_REQUEST['newParentId']) && !empty($_REQUEST['newParentId']) && $_REQUEST['newParentId'] != $_REQUEST['folderId']) {
                if (is_guid($_REQUEST['folderId']) && is_guid($_REQUEST['newParentId'])) {
                    $email->et->folder->retrieve($_REQUEST['folderId']);
                    $email->et->folder->updateFolder(array(
                        "record" => $_REQUEST['folderId'],
                        "name" => $email->et->folder->name,
                        "parent_folder" => $_REQUEST['newParentId'],
                        "team_id" => $email->et->folder->team_id,
                        "team_set_id" => $email->et->folder->team_set_id,
                    ));
                } else {
                    echo "NOOP - not a Sugar Folder";
                }
            }
            break;
        case "getGroupFolder":
            $email->et->folder->retrieve($_REQUEST['folderId']);
            $_REQUEST['record'] = $_REQUEST['folderId'];
            $ret = array();
            $ret['folderId'] = $email->et->folder->id;
            $ret['folderName'] = $email->et->folder->name;
            $ret['parentFolderId'] = $email->et->folder->parent_folder;
            $out = $json->encode($ret);
            echo $out;
            break;


        case "rebuildFolders":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: rebuildFolders");
            $tree = $email->et->getMailboxNodes(false);
            $return = $tree->generateNodesRaw();
            $out = $json->encode($return);
            echo $out;
            break;

        case "setFolderOpenState":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: setFolderOpenState");
            $email->et->saveFolderOpenState($_REQUEST['focusFolder'], $_REQUEST['focusFolderOpen']);
            break;

        case "saveListViewSortOrder":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveListViewSortOrder");
            $email->et->saveListViewSortOrder(
                $_REQUEST['ieId'],
                $_REQUEST['focusFolder'],
                $_REQUEST['sortBy'],
                $_REQUEST['reverse']
            );
            break;
        ////    END FOLDER ACTIONS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////    INBOUND EMAIL ACCOUNTS

        case "retrieveAllOutbound":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: retrieveAllOutbound");
            global $current_user;
            $oe = new OutboundEmail();
            $outbounds = $oe->getUserMailers($current_user);
            $results = array('outbound_account_list' => $outbounds, 'count' => is_countable($outbounds) ? count($outbounds) : 0);
            $out = $json->encode($results, false);
            echo $out;

            break;

        case "editOutbound":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: editOutbound");
            if (isset($_REQUEST['outbound_email']) && !empty($_REQUEST['outbound_email'])) {
                $oe = new OutboundEmail();
                $oe->retrieve($_REQUEST['outbound_email']);

                $ret = array();

                foreach ($oe->field_defs as $def) {
                    $ret[$def] = $oe->$def;
                }
                $ret['mail_smtppass'] = ''; // don't send back the password
                $ret['has_password'] = isset($oe->mail_smtppass);

                $out = $json->encode($ret, true);
                echo $out;
            } else {
                echo "NOOP";
            }
            break;

        case "deleteOutbound":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: deleteOutbound");
            if (isset($_REQUEST['outbound_email']) && !empty($_REQUEST['outbound_email'])) {
                $oe = new OutboundEmail();
                global $current_user;
                $oe->retrieve($_REQUEST['outbound_email']);
                $affectedInboundAccounts = $oe->getAssociatedInboundAccounts($current_user);

                //Check if the user has confirmed he wants to delete the email account even if associated to an inbound accnt.
                $confirmedDelete = (isset($_REQUEST['confirm']) && $_REQUEST['confirm']) ? true : false;

                if ((is_countable($affectedInboundAccounts) ? count($affectedInboundAccounts) : 0) > 0 && !$confirmedDelete) {
                    $results = array(
                        'is_error' => true,
                        'error_message' => $app_strings['LBL_EMAIL_REMOVE_SMTP_WARNING'],
                        'outbound_email' => $_REQUEST['outbound_email']
                    );
                } else {
                    $oe->delete();
                    $results = array('is_error' => false, 'error_message' => '');
                }

                $out = $json->encode($results);
                @ob_end_clean();
                ob_start();
                echo $out;
                ob_end_flush();
                die();
            } else {
                echo "NOOP";
            }
            break;

        case "saveOutbound":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveOutbound");

            $oe = new OutboundEmail();
            $oe->id = $_REQUEST['mail_id'];
            $oe->retrieve($oe->id);
            $oe->name = $_REQUEST['mail_name'];
            $type = empty($_REQUEST['type']) ? 'user' : $_REQUEST['type'];
            $oe->type = $type;
            $oe->user_id = $current_user->id;
            $oe->mail_sendtype = "SMTP";

            $oe->smtp_from_name = trim($_REQUEST['smtp_from_name']);
            $oe->smtp_from_addr = trim($_REQUEST['smtp_from_addr']);
            $oe->mail_smtpserver = trim($_REQUEST['mail_smtpserver']);
            $oe->mail_smtpport = $_REQUEST['mail_smtpport'];
            $oe->mail_smtpssl = $_REQUEST['mail_smtpssl'];
            $oe->mail_smtpauth_req = isset($_REQUEST['mail_smtpauth_req']) ? 1 : 0;
            $oe->mail_smtpuser = trim($_REQUEST['mail_smtpuser']);
            if (!empty($_REQUEST['mail_smtppass'])) {
                $oe->mail_smtppass = $_REQUEST['mail_smtppass'];
            }
            $oe = $oe->save();
            echo $oe->id;
            break;

        case "saveDefaultOutbound":
            global $current_user;
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveDefaultOutbound");
            $outbound_id = empty($_REQUEST['id']) ? "" : $_REQUEST['id'];
            $ie = BeanFactory::newBean('InboundEmail');
            $ie->setUsersDefaultOutboundServerId($current_user, $outbound_id);
            break;
        case "testOutbound":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: testOutbound");

            $pass = '';
            if (!empty($_REQUEST['mail_smtppass'])) {
                $pass = $_REQUEST['mail_smtppass'];
            } elseif (isset($_REQUEST['mail_name'])) {
                $oe = new OutboundEmail();
                $oe = $oe->getMailerByName($current_user, $_REQUEST['mail_name']);
                if (!empty($oe)) {
                    $pass = $oe->mail_smtppass;
                }
            }
            $out = $email->sendEmailTest(
                $_REQUEST['mail_smtpserver'],
                $_REQUEST['mail_smtpport'],
                $_REQUEST['mail_smtpssl'],
                (isset($_REQUEST['mail_smtpauth_req']) ? 1 : 0),
                $_REQUEST['mail_smtpuser'],
                $pass,
                $_REQUEST['outboundtest_from_address'],
                $_REQUEST['outboundtest_from_address']
            );

            $out = $json->encode($out);
            echo $out;
            break;

        case "rebuildShowAccount":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: rebuildShowAccount");
            $ret = $email->et->getShowAccountsOptions($ie);
            $results = array('account_list' => $ret, 'count' => is_countable($ret) ? count($ret) : 0);
            $out = $json->encode($results);
            echo $out;
            break;

        case "rebuildShowAccountForSearch":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: rebuildShowAccount");
            $ret = $email->et->getShowAccountsOptionsForSearch($ie);
            $out = $json->encode($ret);
            echo $out;
            break;

        case "deleteIeAccount":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: deleteIeAccount");
            if (isset($_REQUEST['group_id']) && $_REQUEST['group_id'] == $current_user->id) {
                $ret = array();
                $updateFolders = array();
                $ret['id'] = $_REQUEST['ie_id'];
                $out = $json->encode($ret);
                $ie->hardDelete($_REQUEST['ie_id']);
                $out = $json->encode(array('id' => $_REQUEST['ie_id']));
                echo $out;

                foreach ($showFolders as $id) {
                    if ($id != $_REQUEST['ie_id']) {
                        $updateFolders[] = $id;
                    }
                }

                $showStore = base64_encode(serialize($updateFolders));
                $current_user->setPreference('showFolders', $showStore, 0, 'Emails');
            }
            break;

        case "saveIeAccount":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveIeAccount");
            if (isset($_REQUEST['server_url']) && !empty($_REQUEST['server_url'])) {
                if (false === $ie->savePersonalEmailAccount($current_user->id, $current_user->user_name, false)) {
                    $ret = array('error' => 'error');
                    $out = $json->encode($ret);
                    echo $out;
                } else {
                    $ie->retrieve($_REQUEST['ie_id']);
                    if (!isset($ie->created_by_link)) {
                        $ie->created_by_link = null;
                    }
                    if (!isset($ie->modified_user_id_link)) {
                        $ie->modified_user_id_link = null;
                    }
                    if (!is_array($showFolders)) {
                        $showFolders = array();
                    }
                    if (!in_array($ie->id, $showFolders)) {
                        $showFolders[] = $ie->id;
                        $showStore = base64_encode(serialize($showFolders));
                        $current_user->setPreference('showFolders', $showStore, 0, 'Emails');
                    }

                    if (isset($_REQUEST['account_signature_id'])) {
                        $email_signatures = $current_user->getPreference('account_signatures', 'Emails');
                        $email_signatures = sugar_unserialize(base64_decode($email_signatures));
                        if (empty($email_signatures)) {
                            $email_signatures = array();
                        }

                        $email_signatures[$ie->id] = $_REQUEST['account_signature_id'];
                        $showStore = base64_encode(serialize($email_signatures));
                        $current_user->setPreference('account_signatures', $showStore, 0, 'Emails');
                    }

                    foreach ($ie->field_defs as $k => $v) {
                        if (isset($v['type']) && ($v['type'] == 'link')) {
                            continue;
                        }
                        if ($k == 'stored_options') {
                            $ie->$k = sugar_unserialize(base64_decode($ie->$k));
                            if (isset($ie->stored_options['from_name'])) {
                                $ie->stored_options['from_name'] = from_html($ie->stored_options['from_name']);
                            }
                        } elseif ($k == 'service') {
                            $service = explode("::", $ie->$k);
                            $retService = array();

                            foreach ($service as $serviceK => $serviceV) {
                                if (!empty($serviceV)) {
                                    $retService[$serviceK] = $serviceV;
                                }
                            }

                            $ie->$k = $retService;
                        }

                        if (isset($ie->$k)) {
                            $ret[$k] = $ie->$k;
                        }
                    }

                    $out = $json->encode($ret);
                    echo $out;
                }

                //If the user is saving the username/password then we need to update the outbound account.
                $outboundMailUser = (isset($_REQUEST['mail_smtpuser'])) ? $_REQUEST['mail_smtpuser'] : "";
                $outboundMailPass = (isset($_REQUEST['mail_smtppass'])) ? $_REQUEST['mail_smtppass'] : "";
                $outboundMailId = (isset($_REQUEST['outbound_email'])) ? $_REQUEST['outbound_email'] : "";

                if (!empty($outboundMailUser) && !empty($outboundMailPass) && !empty($outboundMailId)) {
                    $oe = new OutboundEmail();
                    $oe->retrieve($outboundMailId);
                    $oe->mail_smtpuser = $outboundMailUser;
                    $oe->mail_smtppass = $outboundMailPass;
                    $oe->save();
                }
            } else {
                echo "NOOP";
            }
            break;

        case "getIeAccount":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getIeAccount");
            $ieId = $_REQUEST['ieId'];
            $ie->retrieve($ieId);

            if (($ie->group_id == $current_user->id) || ($current_user->is_admin)) {
                $ret = array();

                foreach ($ie->field_defs as $k => $v) {
                    if ($k == 'stored_options') {
                        $ie->$k = unserialize(base64_decode($ie->$k));
                        if (isset($ie->stored_options['from_name'])) {
                            $ie->stored_options['from_name'] = from_html($ie->stored_options['from_name']);
                        }
                    } elseif ($k == 'service') {
                        $service = explode("::", $ie->$k);
                        $retService = array();
                        foreach ($service as $serviceK => $serviceV) {
                            if (!empty($serviceV)) {
                                $retService[$serviceK] = $serviceV;
                            }
                        }

                        $ie->$k = $retService;
                    }

                    $ret[$k] = $ie->$k;
                }
                unset($ret['email_password']); // no need to send the password out

                $email_signatures = $current_user->getPreference('account_signatures', 'Emails');
                $email_signatures = sugar_unserialize(base64_decode($email_signatures));

                if (!empty($email_signatures) && isset($email_signatures[$ieId])) {
                    $ret['email_signatures'] = $email_signatures[$ieId];
                } else {
                    $ret['email_signatures'] = null;
                }

                global $current_user;
                $email_default_signatures = $current_user->getPreference('signature_default');
                $email_account_signatures = $current_user->getEmailAccountSignatures(false, '');


                $ret['email_account_signatures'] = $email_account_signatures;


                $out = $json->encode($ret);
            } else {
                $out = "NOOP: ID mismatch";
            }
            echo $out;
            break;
        ////    END INBOUND EMAIL ACCOUNTS
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    SEARCH
        case "search":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: search");
            if (isset($_REQUEST['subject']) && !empty($_REQUEST['subject']) && isset($_REQUEST['ieId']) && !empty($_REQUEST['ieId'])) {
                $metalist = $ie->search($_REQUEST['ieId'], $_REQUEST['subject']);
                if (!isset($_REQUEST['page'])) {
                    $_REQUEST['page'] = 1;
                }
                $_REQUEST['pageSize'] = is_countable($metalist['out']) ? count($metalist['out']) : 0;
                $out = $email->et->xmlOutput($metalist, 'Email', false);
                @ob_end_clean();
                ob_start();
                header("Content-type: text/xml");
                echo $out;
                ob_end_flush();
                die();
            } else {
                echo "NOOP: no search criteria found";
            }

            break;

        case "searchAdvanced":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: searchAdvanced");

            $metalist = $email->searchImportedEmails();
            $out = $email->et->jsonOuput($metalist, 'Email', $metalist['totalCount']);

            @ob_end_clean();
            ob_start();
            echo $out;
            ob_end_flush();
            die();

            break;
        ////    END SEARCH
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    SETTINGS
        case "loadPreferences":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: loadPreferences");
            $prefs = $email->et->getUserPreferencesJS();
            $out = $json->encode($prefs);
            echo $out;
            break;

        case "saveSettingsGeneral":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveSettingsGeneral");
            $emailSettings = array();
            $emailSettings['emailCheckInterval'] = $_REQUEST['emailCheckInterval'];
            //$emailSettings['autoImport'] = isset($_REQUEST['autoImport']) ? '1' : '0';
            $emailSettings['alwaysSaveOutbound'] = '1';
            $emailSettings['sendPlainText'] = isset($_REQUEST['sendPlainText']) ? '1' : '0';
            $emailSettings['showNumInList'] = $_REQUEST['showNumInList'];
            $emailSettings['defaultOutboundCharset'] = $_REQUEST['default_charset'];
            $current_user->setPreference('emailSettings', $emailSettings, '', 'Emails');

            // signature
            $current_user->setPreference('signature_default', $_REQUEST['signature_id']);
            $current_user->setPreference('signature_prepend', (isset($_REQUEST['signature_prepend'])) ? true : false);

            $out = $json->encode($email->et->getUserPreferencesJS());
            echo $out;
            break;

        case "setPreference":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: setPreference");
            if (isset($_REQUEST['prefName']) && isset($_REQUEST['prefValue'])) {
                // handle potential JSON encoding
                if (isset($_REQUEST['decode'])) {
                    $_REQUEST['prefValue'] = $json->decode(from_html($_REQUEST['prefValue']));
                }

                $current_user->setPreference($_REQUEST['prefName'], $_REQUEST['prefValue'], '', 'Emails');
            }
            break;
        ////    END SETTINGS
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    ADDRESS BOOK

        case "editContact":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: editContact");
            if (isset($_REQUEST['id']) && !empty($_REQUEST['id'])) {
                $module = "Contacts";
                $ret = $email->et->getEditContact($_REQUEST['id'], $module);
            }
            $out = $json->encode($ret);
            echo $out;
            break;


        /* The four calls below all have the same return signature */
        case "removeContact":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: removeContacts");
            if (strpos((string) $_REQUEST['ids'], "::") !== false) {
                $removeIds = explode("::", $_REQUEST['ids']);
            } else {
                $removeIds[] = $_REQUEST['ids'];
            }
            $email->et->removeContacts($removeIds);

            // no break
        case "saveContactEdit":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: saveContactEdit");
            if (isset($_REQUEST['args']) && !empty($_REQUEST['args'])) {
                $email->et->saveContactEdit($_REQUEST['args']);
            }
        // flow into getUserContacts();
        // no break
        case "addContact":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: addContacts");
            $contacts = array();

            if (isset($_REQUEST['bean_module']) && !empty($_REQUEST['bean_module']) && isset($_REQUEST['bean_id']) && !empty($_REQUEST['bean_id'])) {
                $contacts[$_REQUEST['bean_id']] = array(
                    'id' => $_REQUEST['bean_id'],
                    'module' => $_REQUEST['bean_module']
                );
                $email->et->setContacts($contacts);
            }

            // no break
        case "addContactsMultiple":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: addContacts");
            if (isset($_REQUEST['contactData'])) {
                $contacts = $json->decode(from_HTML($_REQUEST['contactData']));
                if ($contacts) {
                    $email->et->setContacts($contacts);
                }
            }

            // no break
        case "getUserContacts":
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: getUserContacts");
            $contacts = $email->et->getContacts();

            if (is_array($contacts) && !empty($contacts)) {
                $ret = $email->et->getUserContacts($contacts, $current_user);
                $out = $json->encode($ret);
                echo $out;
            } else {
                echo "{}";
            }
            break;


        // address book search
        case "getUnionData":
            $wheres = array();
            $person;
            if (isset($_REQUEST['first_name']) && !empty($_REQUEST['first_name'])) {
                $wheres['first_name'] = $_REQUEST['first_name'];
            }
            if (isset($_REQUEST['last_name']) && !empty($_REQUEST['last_name'])) {
                $wheres['last_name'] = $_REQUEST['last_name'];
            }
            if (isset($_REQUEST['email_address']) && !empty($_REQUEST['email_address'])) {
                $wheres['email_address'] = $_REQUEST['email_address'];
            }
            if (isset($_REQUEST['person']) && !empty($_REQUEST['person'])) {
                $person = $_REQUEST['person'];
            }
            $q = $email->et->_getPeopleUnionQuery($wheres, $person);
            $r = $ie->db->limitQuery($q, 0, 25, true);

            $out = array();
            while ($a = $ie->db->fetchByAssoc($r)) {
                $person = array();
                $person['id'] = $a['id'];
                $person['module'] = $a['module'];
                $person['full_name'] = $locale->getLocaleFormattedName($a['first_name'], $a['last_name'], '');
                $person['email_address'] = $a['email_address'];
                $out[] = $person;
            }

            $ret = $json->encode($out, true);
            echo $ret;
            break;

        case "getAddressSearchResults":
            $wheres = array();
            $person = 'contacts';
            $relatedBeanInfo = '';
            if (isset($_REQUEST['related_bean_id']) && !empty($_REQUEST['related_bean_id'])) {
                $isRelatedSearch = true;
                $relatedBeanInfo['related_bean_id'] = $_REQUEST['related_bean_id'];
                $relatedBeanInfo['related_bean_type'] = ucfirst($_REQUEST['related_bean_type']);
            }

            if (isset($_REQUEST['search_field'])) {
                $wheres['first_name'] = $_REQUEST['search_field'];
                $wheres['last_name'] = $_REQUEST['search_field'];
                $wheres['email_address'] = $_REQUEST['search_field'];
            }

            if (isset($_REQUEST['person']) && !empty($_REQUEST['person'])) {
                $person = $_REQUEST['person'];
            }
            if (!empty($_REQUEST['start'])) {
                $start = (int)$_REQUEST['start'];
            } else {
                $start = 0;
            }

            $qArray = $email->et->getRelatedEmail($person, $wheres, $relatedBeanInfo);
            $out = array();
            $count = 0;
            if (!empty($qArray['query'])) {
                $countq = $qArray['countQuery'];
                $time = microtime(true);
                $r = $ie->db->query($countq);
                $GLOBALS['log']->debug("***QUERY counted in " . (microtime(true) - $time) . " milisec\n");
                if ($row = DBManagerFactory::getInstance()->fetchByAssoc($r)) {
                    $count = $row['c'];
                }
                $time = microtime(true);

                //Handle sort and order requests
                $sort = !empty($_REQUEST['sort']) ? $ie->db->getValidDBName($_REQUEST['sort']) : "id";
                $sort = ($sort == 'bean_id') ? 'id' : $sort;
                $sort = ($sort == 'email') ? 'email_address' : $sort;
                $sort = ($sort == 'name') ? 'last_name' : $sort;
                $direction = !empty($_REQUEST['dir']) && in_array(
                    strtolower($_REQUEST['dir']),
                    array("asc", "desc")
                ) ? $_REQUEST['dir'] : "asc";
                $order = (!empty($sort) && !empty($direction)) ? " ORDER BY {$sort} {$direction}" : "";

                $r = $ie->db->limitQuery($qArray['query'] . " $order ", $start, 25, true);
                $GLOBALS['log']->debug("***QUERY Got results in " . (microtime(true) - $time) . " milisec\n");


                while ($a = $ie->db->fetchByAssoc($r)) {
                    $person = array();
                    $person['bean_id'] = $a['id'];
                    $person['bean_module'] = $a['module'];
                    $person['name'] = $locale->getLocaleFormattedName($a['first_name'], $a['last_name'], '');
                    $person['email'] = $a['email_address'];
                    $out[] = $person;
                }
            }
            $ret = $email->et->jsonOuput(array('out' => $out), 'Person', $count);

            @ob_end_clean();
            ob_start();
            echo $ret;
            ob_end_flush();
            break;

        ////    END ADDRESS BOOK
        ///////////////////////////////////////////////////////////////////////////


        ///////////////////////////////////////////////////////////////////////////
        ////    MISC

        default:
            $GLOBALS['log']->debug("********** EMAIL 2.0 - Asynchronous - at: default");
            echo "NOOP";
            break;
    } // switch

    if (isset($cid) && $cid) {
        $current_user->savePreferencesToDB();
        $current_user = BeanFactory::getBean('Users', $cid);
    }
} // if
