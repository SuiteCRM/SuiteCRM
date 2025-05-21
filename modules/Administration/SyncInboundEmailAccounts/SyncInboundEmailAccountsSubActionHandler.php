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
 * Class SyncInboundEmailAccountsSubActionHandler
 *
 * Separated methods specially for SyncInboundEmailAccounts sub-actions handling
 *
 */
#[\AllowDynamicProperties]
class SyncInboundEmailAccountsSubActionHandler
{

    /**
     * @const string
     */
    public const PROCESS_OUTPUT_FILE = "modules/Administration/SyncInboundEmailAccounts/sync_output.html";

    /**
     * @var SyncInboundEmailAccountsPage
     */
    protected $sync;

    /**
     * @var DBManager
     */
    protected $db;

    /**
     * SyncInboundEmailAccountsSubActionHandler constructor.
     *
     * Handle sub-action for Sync Inbound Email Accounts
     *
     * @param SyncInboundEmailAccountsPage $sync
     * @throws SyncInboundEmailAccountsException
     * @throws SyncInboundEmailAccountsNoMethodException
     */
    public function __construct(SyncInboundEmailAccountsPage $sync)
    {
        global $mod_strings;

        $this->sync = $sync;

        try {
            $this->db = DBManagerFactory::getInstance();

            $subAction = $this->getRequestedSubAction();

            switch ($subAction) {

                case 'index':
                    $this->action_Index();
                    break;

                case 'sync':
                    $this->action_Sync();
                    break;

                default:
                    throw new SyncInboundEmailAccountsNoMethodException(
                        "trying to call an unsupported method: " . $subAction
                    );

            }
        } catch (SyncInboundEmailAccountsException $e) {
            $code = $e->getCode();
            switch ($code) {

                case SyncInboundEmailAccountsException::PROCESS_OUTPUT_CLEANUP_ERROR:
                    $this->sync->showOutput($mod_strings['LBL_PROCESS_OUTPUT_CLEANUP_ERROR']);
                    break;

                case SyncInboundEmailAccountsException::PROCESS_OUTPUT_WRITE_ERROR:
                    $this->sync->showOutput($mod_strings['LBL_PROCESS_OUTPUT_WRITE_ERROR']);
                    break;

                default:
                    throw new SyncInboundEmailAccountsException(
                        "Unknown error in sync process, see previous exception",
                        SyncInboundEmailAccountsException::UNKNOWN_ERROR,
                        $e
                    );

            }
        }
    }

    /**
     * Return the requested sub action, use $_REQUEST['method']
     * or an 'index' string by default if request was empty or incorrect
     *
     * @return string
     * @throws SyncInboundEmailAccountsInvalidMethodTypeException
     */
    protected function getRequestedSubAction()
    {
        $ret = "index";

        // handle requested sub-action in method parameter

        if (isset($_REQUEST['method']) && $_REQUEST['method']) {
            $ret = $_REQUEST['method'];

            // validate for correct method
            if (!is_string($ret)) {
                throw new SyncInboundEmailAccountsInvalidMethodTypeException(
                    "Method name should be a string but received type is: " . gettype($ret)
                );
            }
        }

        return $ret;
    }

    /**
     * Default 'index' action, shows the main form
     *
     */
    protected function action_Index()
    {

        // fetch data to view
        $ieList = $this->getInboundEmailRows();

        // show sync-form
        $this->sync->showForm($ieList);
    }

    /**
     * Return all results for non-deleted active inbound email account
     * in an inbound email account id indexed array
     *
     * @return array
     * @throws SyncInboundEmailAccountsEmptyException
     */
    protected function getInboundEmailRows()
    {
        $ret = $this->select("SELECT * FROM inbound_email WHERE status='Active' AND deleted = 0;");

        return $ret;
    }

    /**
     * @param string $emailId
     * @return bool|SugarBean
     */
    protected function getEmailBean($emailId)
    {
        $email = BeanFactory::getBean('Emails', $emailId);
        return $email;
    }

    /**
     * @param string $ieId
     * @return bool|SugarBean
     */
    protected function getInboundEmailBean($ieId)
    {
        $ie = BeanFactory::getBean('InboundEmail', $ieId);
        return $ie;
    }

    /**
     * @throws SyncInboundEmailAccountsException
     * @throws SyncInboundEmailAccountsInvalidSubActionArgumentsException
     * @throws SyncInboundEmailException
     */
    protected function action_Sync()
    {
        global $mod_strings;

        // make sure there is no time limit
        // so we will having enough time to sync everything
        set_time_limit(0);

        $this->cleanup();

        $this->output($mod_strings['LBL_SYNC_MESSAGE']);

        $ieList = $this->getRequestedInboundEmailAccounts();


        foreach ($ieList as $ieId) {

            // TODO: scrm-539 - BeanFactory::getBean() return value is SugarBean|bool but never can be (bool)true, it may cause confusion in future
            if ($ie = $this->getInboundEmailBean($ieId)) {
                $this->output(sprintf($mod_strings['LBL_SYNC_PROCESSING'], $ie->name));
                try {
                    $IMAPHeaders = $this->getEmailHeadersOfIMAPServer($ie);

                    $emailIds = $this->getEmailIdsOfInboundEmail($ieId);

                    $updated = 0;
                    foreach ($emailIds as $emailId => $emailData) {
                        $e = $this->getEmailBean($emailId);
                        if ($e !== false) {
                            $e->orphaned = $this->isOrphanedEmail($e, $IMAPHeaders);
                            if ($e->uid = $this->getIMAPUID($e->message_id, $IMAPHeaders)) {
                                if ($e->save()) {
                                    $updated++;
                                }
                                // todo: scrm-539 handle if bean save failed
                            }
                            // todo: scrm-539 handle if there is no uid
                        }
                    }

                    $this->output(sprintf($mod_strings['LBL_SYNC_UPDATED'], $updated));
                } catch (SyncInboundEmailAccountsIMapConnectionException $e) {
                    $GLOBALS['log']->warn($e->getMessage());
                    $this->output($mod_strings['LBL_SYNC_ERROR_CONN']);
                } catch (SyncInboundEmailAccountsEmptyException $e) {
                    $this->output($mod_strings['LBL_SYNC_NO_EMAIL']);
                }

                $this->handleIMAPErrors($ie);
            } else {
                $this->output($mod_strings['LBL_IE_NOT_FOUND']);
                $GLOBALS['log']->debug("Inbound Email Account record not found, please check the record still exists and non-deleted: " . $ieId);
            }
        }
        $this->output($mod_strings['LBL_SYNC_DONE']);

        $output = file_get_contents(self::PROCESS_OUTPUT_FILE);

        $this->cleanup();

        $this->sync->showOutput($output);
        die();
    }

    /**
     * @throws SyncInboundEmailAccountsException
     */
    protected function handleIMAPErrors(InboundEmail $ie)
    {
        global $mod_strings;
        $errs = $ie->getImap()->getErrors();
        if ($errs) {
            foreach ($errs as $err) {
                $GLOBALS['log']->error("IMAP error detected: " . $err);
            }
            $this->output($mod_strings['LBL_SYNC_ERROR_FOUND']);
        }

        $warns = $ie->getImap()->getAlerts();
        if ($warns) {
            foreach ($warns as $warn) {
                $GLOBALS['log']->warn("IMAP error detected: " . $warn);
            }
            $this->output($mod_strings['LBL_SYNC_ALERT_FOUND']);
        }
    }

    /**
     * @throws SyncInboundEmailAccountsException
     */
    protected function cleanup()
    {
        if (file_exists(self::PROCESS_OUTPUT_FILE)) {
            if (!unlink(self::PROCESS_OUTPUT_FILE)) {
                throw new SyncInboundEmailAccountsException(
                    "Unable to cleanup output file. Please check permission..",
                    SyncInboundEmailAccountsException::PROCESS_OUTPUT_CLEANUP_ERROR
                );
            }
        }
    }

    /**
     * @param $msg
     * @throws SyncInboundEmailAccountsException
     */
    protected function output($msg)
    {
        $msg = "{$msg}<br>";
        if (false === file_put_contents(self::PROCESS_OUTPUT_FILE, $msg, FILE_APPEND)) {
            throw new SyncInboundEmailAccountsException(
                "Unable to write output file. Please check permission..",
                SyncInboundEmailAccountsException::PROCESS_OUTPUT_WRITE_ERROR
            );
        }
    }

    /**
     * @param $emailMD5
     * @param $IMAPHeaders
     * @return null|int
     */
    protected function getIMAPUID($emailMD5, $IMAPHeaders)
    {
        foreach ($IMAPHeaders as $header) {
            if ($header->message_id_md5 == $emailMD5) {
                return $header->imap_uid;
            }
        }
        return null;
    }

    /**
     * @param string $ieId
     * @return array
     * @throws SyncInboundEmailAccountsEmptyException
     * @throws SyncInboundEmailException
     */
    protected function getEmailIdsOfInboundEmail($ieId)
    {
        $isValidator = new SuiteValidator();
        if (!$isValidator->isValidId($ieId)) {
            throw new SyncInboundEmailException("Invalid Inbound Email ID");
        }
        $query = "SELECT id FROM emails WHERE mailbox_id = '{$ieId}' AND deleted = 0;";
        $emailIds = $this->select($query);

        return $emailIds;
    }

    /**
     * @param $query
     * @return array
     * @throws SyncInboundEmailAccountsEmptyException
     */
    protected function select($query)
    {

        // run sql select, grab results into an array and pass back in return
        $ret = array();
        $r = $this->db->query($query);
        while ($e = $this->db->fetchByAssoc($r)) {
            $ret[$e['id']] = $e;
        }
        if (empty($ret)) {
            throw new SyncInboundEmailAccountsEmptyException("No imported related Email to Inbound Email Account");
        }

        return $ret;
    }

    /**
     * @param InboundEmail $ie
     * @param bool $test
     * @param bool $force
     * @param null $useSsl
     * @return mixed
     * @throws SyncInboundEmailAccountsIMapConnectionException
     */
    protected function getEmailHeadersOfIMAPServer(InboundEmail $ie, $test = false, $force = false, $useSsl = null)
    {

        // ---------- CONNECT TO IMAP ------------

        // override $_REQUEST['ssl'] as an argument for
        // old one method InboundEmails::connectMailserver()
        // to make sure the behavior is same

        if (null != $useSsl) {
            $_REQUEST['ssl'] = $useSsl;
        }

        // connect to mail server view old method

        // TODO: SCRM-539 check first, may we have to restore the folder name to INBOX

        $results = $ie->connectMailserver($test, $force);

        // handle the error..

        if ($results !== "true") {
            throw new SyncInboundEmailAccountsIMapConnectionException("Connection failed to IMap ({$ie->name}): " . $results);
        }

        // ------------- READ IMAP EMAIL-HEADERS AND CALCULATE MD5 BASED MESSAGE_IDs ----------------

        $imap_uids = $ie->getImap()->sort(SORTDATE, 0, SE_UID);
        $headers = array();
        foreach ($imap_uids as $imap_uid) {
            $msgNo = $ie->getImap()->getMessageNo((int)$imap_uid);
            $headers[$imap_uid] = $ie->getImap()->getHeaderInfo($msgNo);
            $headers[$imap_uid]->imap_uid = $imap_uid;
            $headers[$imap_uid]->imap_msgid_int = (int)$msgNo;
        }


        foreach ($headers as &$header) {
            $header->message_id_md5 = $this->getCompoundMessageIdMD5($ie, $header->imap_uid);
        }


        // ------------ IMAP CLOSE -------------

        $ie->getImap()->close();


        return $headers;
    }

    /**
     * @param $header
     * @param InboundEmail $ie
     * @param $uid
     * @param null $msgNo
     * @return string
     */
    protected function getCompoundMessageIdMD5(InboundEmail $ie, $uid, $msgNo = null)
    {
        if (empty($msgNo) && !empty($uid)) {
            $msgNo = $ie->getImap()->getMessageNo((int)$uid);
        }

        $header = $ie->getImap()->getHeaderInfo($msgNo);
        $fullHeader = $ie->getImap()->fetchHeader($msgNo);
        $message_id = $header->message_id;
        $deliveredTo = $ie->id;
        $matches = array();
        preg_match('/(delivered-to:|x-real-to:){1}\s*(\S+)\s*\n{1}/im', (string) $fullHeader, $matches);
        if (count($matches)) {
            $deliveredTo = $matches[2];
        }
        if (empty($message_id) || !isset($message_id)) {
            $GLOBALS['log']->debug('*********** NO MESSAGE_ID.');
            $message_id = $ie->getMessageId($header);
        }

        // generate compound messageId
        $compoundMessageId = trim($message_id) . trim($deliveredTo);
        // if the length > 255 then md5 it so that the data will be of smaller length
        //if (strlen($compoundMessageId) > 255) {
        $compoundMessageId = md5($compoundMessageId);
        //} // if

        if (empty($compoundMessageId)) {
            return null; //throw new Exception('????');
        } // if
        //$counter++;
        $potentials = clean_xss($compoundMessageId, false);

        if (is_array($potentials) && !empty($potentials)) {
            foreach ($potentials as $bad) {
                $compoundMessageId = str_replace($bad, "", $compoundMessageId);
            }
        }

        return $compoundMessageId;
    }

    /**
     * @param Email $e
     * @param $IMAPheaders
     * @return bool
     */
    protected function isOrphanedEmail(Email $e, $IMAPheaders)
    {
        foreach ($IMAPheaders as $header) {
            if ($header->message_id_md5 == $e->message_id) {
                return false;
            }
        }
        return true;
    }


    /**
     * This function only for main form handling,
     * calling by sync action to get selected inbound email accounts
     *
     * @return mixed
     * @throws SyncInboundEmailAccountsInvalidSubActionArgumentsException
     */
    protected function getRequestedInboundEmailAccounts()
    {

        // validate for selected inbound email(s)

        if (!isset($_REQUEST['ie-sel'])) {
            // it's should be in the request
            throw new SyncInboundEmailAccountsInvalidSubActionArgumentsException("Invalid action parameter");
        }

        $ieSel = $_REQUEST['ie-sel'];

        if (!$ieSel) {
            // if there is not any selected, just fill out with all inbound email
            $ieSel = array_keys($this->getInboundEmailRows());
        }

        return $ieSel;
    }
}
