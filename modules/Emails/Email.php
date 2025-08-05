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

require_once __DIR__ . '/EmailFromValidator.php';
include_once __DIR__ . '/EmailException.php';
require_once __DIR__ . '/../../include/SugarPHPMailer.php';
require_once __DIR__ . '/../../include/UploadFile.php';
require_once __DIR__ . '/../../include/UploadMultipleFiles.php';
require_once __DIR__ . '/NonGmailSentFolderHandler.php';


#[\AllowDynamicProperties]
class Email extends Basic
{
    /**
     * @var string $from_addr
     */
    public $from_addr;

    /**
     * @var string $reply_to_addr
     */
    public $reply_to_addr;

    /**
     * @var string $to_addrs
     */
    public $to_addrs;

    /**
     * @var string $cc_addrs
     */
    public $cc_addrs;

    /**
     * @var string $bcc_addrs
     */
    public $bcc_addrs;

    /**
     * @var string $message_id
     */
    public $message_id;

    /**
     * @var string $type
     */
    public $type = 'archived';

    /**
     * @var string $date_sent_received
     */
    public $date_sent_received;

    /**
     * @var string $status dom_email_status
     */
    public $status;

    /**
     * @var string $intent
     */
    public $intent;

    /**
     * @var string $mailbox_id
     */
    public $mailbox_id;

    /**
     * @var string $from_name
     */
    public $from_name;

    /**
     * @var string $reply_to_status
     */
    public $reply_to_status;

    /**
     * @var string $reply_to_name
     */
    public $reply_to_name;

    /**
     * @var string $reply_to_email
     */
    public $reply_to_email;

    /**
     * @var string $description_html
     */
    public $description_html;

    /**
     * @var string $raw_source html
     */
    public $raw_source;

    /**
     * @var string $parent_id
     */
    public $parent_id;

    /**
     * @var string $parent_type
     */
    public $parent_type;

    /**
     * @var string $parent_name
     */
    public $parent_name;

    // legacy
    /**
     * @var string $date_start
     */
    public $date_start;

    /**
     * @var string $time_start
     */
    public $time_start;

    /**
     * @var string $from_addr_name
     */
    public $from_addr_name;

    /**
     * @var array $to_addrs_arr
     */
    public $to_addrs_arr;

    /**
     * @var array $cc_addrs_arr
     */
    public $cc_addrs_arr;

    /**
     * @var array $bcc_addrs_arr
     */
    public $bcc_addrs_arr;

    /**
     * @var string $to_addrs_ids
     */
    public $to_addrs_ids;

    /**
     * @var string $to_addrs_names
     */
    public $to_addrs_names;

    /**
     * @var string $to_addrs_emails
     */
    public $to_addrs_emails;

    /**
     * @var string $cc_addrs_ids
     */
    public $cc_addrs_ids;

    /**
     * @var string $cc_addrs_names
     */
    public $cc_addrs_names;

    /**
     * @var string $cc_addrs_emails
     */
    public $cc_addrs_emails;

    /**
     * @var string $bcc_addrs_ids
     */
    public $bcc_addrs_ids;

    /**
     * @var string $bcc_addrs_names
     */
    public $bcc_addrs_names;

    /**
     * @var string $bcc_addrs_emails
     */
    public $bcc_addrs_emails;

    /**
     * @var string $contact_id
     */
    public $contact_id;

    /**
     * @var string $contact_name
     */
    public $contact_name;

    // Archive Email attributes
    /**
     * @var string $duration_hours
     */
    public $duration_hours;

    /**
     * @var bool $new_schema
     */
    public $new_schema = true;

    /**
     * @var string $table_name
     */
    public $table_name = 'emails';

    /**
     * @var string $module_dir
     */
    public $module_dir = 'Emails';

    /**
     * @var string $module_name
     */
    public $module_name = 'Emails';

    /**
     * @var string $object_name
     */
    public $object_name = 'Email';


    /**
     * @var string $rolloverStyle
     */
    public $rolloverStyle = "";

    /**
     * @var string $cachePath
     */
    public $cachePath;

    /**
     * @var string $cacheFile
     */
    public $cacheFile = 'robin.cache.php';

    /**
     * @var string $replyDelimiter
     */
    public $replyDelimiter = "> ";

    /**
     * @var string $emailRawSource
     */
    public $emailRawSource;

    /**
     * @var string $link_action eg DetailView
     */
    public $link_action;

    /**
     * @var SugarEmailAddress $emailAddress
     */
    public $emailAddress;

    /**
     * @var array $attachments
     */
    public $attachments = array();

    /**
     * @var bool $isDuplicate
     */
    public $isDuplicate;

    /**
     * IMAP UID
     *
     * @var uid
     */
    public $uid;

    /**
     * @var string $to
     */
    public $to;

    /**
     * @var integer $flagged 0 === false, 1 === true
     */
    public $flagged;

    /**
     * @var integer $answered 0 === false, 1 === true
     */
    public $answered;

    /**
     * @var integer $seen 0 === false, 1 === true
     */
    public $seen;

    /**
     * @var integer $draft 0 === false, 1 === true
     */
    public $draft;

    /**
     * @var array $relationshipMap
     */
    public $relationshipMap = array(
        'Contacts' => 'emails_contacts_rel',
        'Accounts' => 'emails_accounts_rel',
        'Leads' => 'emails_leads_rel',
        'Users' => 'emails_users_rel',
        'Prospects' => 'emails_prospects_rel',
    );

    /**
     * @var EmailUI $et
     */
    public $et;        // EmailUI object

    /**
     * @var string $imagePrefix prefix to use when importing inlinge images in emails
     */
    public $imagePrefix;

    /**
     * Used for keeping track of field defs that have been modified
     *
     * @var array
     */
    public $modifiedFieldDefs = array();

    /**
     * @var string $attachment_image
     */
    public $attachment_image;

    /**
     * @var Link2
     */
    public $cases;

    /**
     * @var string $category_id
     */
    public $category_id;

    /**
     * orphaned on IMAP
     *
     * @var bool
     */
    public $orphaned;

    /**
     * @var Link2 $notes
     */
    public $notes;

    /**
     * Should be a From Address (From Name should be stored in $FromName class variable)
     *
     * @var string
     */
    public $From;

    /**
     * Should be a From Name (From Address should be stored in $From class variable)
     *
     * @var string
     */
    public $FromName;

    /**
     * @var string
     */
    public $Subject;

    /**
     * @var string
     */
    public $Body;

    /**
     * @var string
     */
    public $AltBody;

    public $msgNo;

    /**
     * @var string
     */
    public $to_name;

    protected static $validFieldNames = array(
        'email_address',
        'to',
        'from',
        'from_name',
        'cc',
        'bcc'
    );

    /**
     *
     * @var string
     */
    public $assigned_user_name;
    public const NO_ERROR = 0;
    public const ERR_NOT_STORED_AS_SENT = 1;
    public const ERR_NO_IE = 2;
    public const ERR_NO_IE_MAIL_ID = 3;
    public const ERR_CODE_SHOULD_BE_INT = 4;
    public const ERR_IE_RETRIEVE = 5;
    public const UNHANDLED_LAST_ERROR = 6;

    /**
     *
     * @var int
     */
    protected $lastSaveAndStoreInSentError = null;

    /**
     *
     * @var NonGmailSentFolderHandler
     */
    protected $nonGmailSentFolderHandler = null;

    /**
     *
     * @var Email
     */
    protected $tempEmailAtSend = null;

    /**
     *
     * @param int $err
     */
    protected function setLastSaveAndStoreInSentError($err)
    {
        if (!is_int($err)) {
            throw new InvalidArgumentException('Error code should be an integer.', self::ERR_CODE_SHOULD_BE_INT);
        }

        if (null !== $this->lastSaveAndStoreInSentError) {
            throw new EmailException(
                'Last Error for method SaveAndStoreInSentFolder() already set but never checked: ' .
                $this->lastSaveAndStoreInSentError,
                self::UNHANDLED_LAST_ERROR
            );
        }
        $this->lastSaveAndStoreInSentError = $err;
    }

    /**
     *
     * @return int
     */
    public function getLastSaveAndStoreInSentError()
    {
        $ret = $this->lastSaveAndStoreInSentError;
        $this->lastSaveAndStoreInSentError = null;
        return $ret;
    }

    /**
     *
     * @param NonGmailSentFolderHandler $nonGmailSentFolderHandler
     */
    protected function setNonGmailSentFolderHandler(NonGmailSentFolderHandler $nonGmailSentFolderHandler)
    {
        $this->nonGmailSentFolderHandler = $nonGmailSentFolderHandler;
    }

    /**
     *
     * @return NonGmailSentFolderHandler
     */
    public function getNonGmailSentFolderHandler()
    {
        return $this->nonGmailSentFolderHandler;
    }

    /**
     *
     */
    protected function clearTempEmailAtSend()
    {
        $this->tempEmailAtSend = null;
    }

    /**
     *
     * @param \Email|null $email
     */
    protected function createTempEmailAtSend(Email $email = null)
    {
        $this->tempEmailAtSend = $email ? $email : BeanFactory::newBean('Emails');
        if (!$this->tempEmailAtSend->date_sent_received) {
            $this->tempEmailAtSend->date_sent_received = TimeDate::getInstance()->nowDb();
        }
    }

    /**
     *
     * @return Email
     */
    public function getTempEmailAtSend()
    {
        return $this->tempEmailAtSend;
    }

    /**
     * sole constructor
     * @global $current_user
     */
    public function __construct()
    {
        global $current_user, $sugar_config;
        $this->cachePath = sugar_cached('modules/Emails');
        parent::__construct();

        $this->emailAddress = new SugarEmailAddress();

        if (isset($sugar_config['site_url'])) {
            $this->imagePrefix = $sugar_config['site_url'] . '/cache/images/';
        }
    }

    /**
     *
     */
    public function __destruct()
    {
        $err = $this->getLastSaveAndStoreInSentError();
        if (null !== $err) {
            LoggerManager::getLogger()->error('Unhandled email save and store as sent error: ' . $err, self::UNHANDLED_LAST_ERROR);
        }
    }




    /**
     * assigns $this->ev with EmailUI
     */
    public function email2init()
    {
        require_once('modules/Emails/EmailUI.php');
        $this->et = new EmailUI();
    }

    /**
     * @param $interface
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
     * Presaves one attachment for new email 2.0 spec
     * DOES NOT CREATE A NOTE
     * @return array ID of note associated with the attachment
     */
    public function email2saveAttachment()
    {
        $email_uploads = "modules/Emails/{$GLOBALS['current_user']->id}";
        $upload = new UploadFile('email_attachment');
        if (!$upload->confirm_upload()) {
            $err = $upload->get_upload_error();
            if ($err) {
                $GLOBALS['log']->error("Email Attachment could not be attached due to error: $err");
            }

            return array();
        }

        $guid = create_guid();
        $fileName = $upload->create_stored_filename();
        $GLOBALS['log']->debug("Email Attachment [$fileName]");
        if ($upload->final_move($guid)) {
            copy("upload://$guid", sugar_cached("$email_uploads/$guid"));

            return array(
                'guid' => $guid,
                'name' => DBManagerFactory::getInstance()->quote($fileName),
                'nameForDisplay' => $fileName
            );
        }
        $GLOBALS['log']->debug("Email Attachment [$fileName] could not be moved to upload dir");

        return array();
    }

    /**
     * @param string $filename
     * @global $sugar_config
     * @return bool
     */
    public function safeAttachmentName($filename)
    {
        global $sugar_config;
        $badExtension = false;
        //get position of last "." in file name
        $file_ext_beg = strrpos($filename, ".");
        $file_ext = "";

        //get file extension
        if ($file_ext_beg !== false) {
            $file_ext = substr($filename, $file_ext_beg + 1);
        }

        //check to see if this is a file with extension located in "badext"
        foreach ($sugar_config['upload_badext'] as $badExt) {
            if (strtolower($file_ext) === strtolower($badExt)) {
                //if found, then append with .txt and break out of lookup
                $filename = $filename . ".txt";
                $badExtension = true;
                break; // no need to look for more
            } // if
        } // foreach

        return $badExtension;
    } // fn

    /**
     * takes output from email 2.0 to/cc/bcc fields and returns appropriate arrays for usage by PHPMailer
     * @param string $addresses
     * @return array
     */
    public function email2ParseAddresses($addresses)
    {
        $addresses = from_html($addresses);
        $addresses = $this->et->unifyEmailString($addresses);

        $pattern = '/@.*,/U';
        preg_match_all($pattern, $addresses, $matchs);
        if (!empty($matchs[0])) {
            $total = $matchs[0];
            foreach ($total as $match) {
                $convertedPattern = str_replace(',', '::;::', (string) $match);
                $addresses = str_replace($match, $convertedPattern, $addresses);
            } //foreach
        }

        $exAddr = explode("::;::", $addresses);

        $ret = array();
        $clean = array("<", ">");
        $dirty = array("&lt;", "&gt;");

        foreach ($exAddr as $addr) {
            $name = '';

            $addr = str_replace($dirty, $clean, $addr);

            if ((strpos($addr, "<") === false) && (strpos($addr, ">") === false)) {
                $address = $addr;
            } else {
                $address = substr($addr, strpos($addr, "<") + 1, strpos($addr, ">") - 1 - strpos($addr, "<"));
                $name = substr($addr, 0, strpos($addr, "<"));
            }

            $addrTemp = array();
            $addrTemp['email'] = trim($address);
            $addrTemp['display'] = trim($name);
            $ret[] = $addrTemp;
        }

        return $ret;
    }

    /**
     * takes output from email 2.0 to/cc/bcc fields and returns appropriate arrays for usage by PHPMailer
     * @param string $addresses
     * @return array
     */
    public function email2ParseAddressesForAddressesOnly($addresses)
    {
        $addresses = from_html($addresses);
        $pattern = '/@.*,/U';
        preg_match_all($pattern, (string) $addresses, $matchs);
        if (!empty($matchs[0])) {
            $total = $matchs[0];
            foreach ($total as $match) {
                $convertedPattern = str_replace(',', '::;::', (string) $match);
                $addresses = str_replace($match, $convertedPattern, (string) $addresses);
            } //foreach
        }

        $exAddr = explode("::;::", $addresses);

        $ret = array();
        $clean = array("<", ">");
        $dirty = array("&lt;", "&gt;");

        foreach ($exAddr as $addr) {
            $name = '';

            $addr = str_replace($dirty, $clean, $addr);

            if (strpos($addr, "<") && strpos($addr, ">")) {
                $address = substr($addr, strpos($addr, "<") + 1, strpos($addr, ">") - 1 - strpos($addr, "<"));
            } else {
                $address = $addr;
            }

            $ret[] = trim($address);
        }

        return $ret;
    }

    /**
     * Determines MIME-type encoding as possible.
     * @param string $fileLocation relative path to file
     * @return string MIME-type
     */
    public function email2GetMime($fileLocation)
    {
        if (!is_readable($fileLocation)) {
            return 'application/octet-stream';
        }
        if (function_exists('mime_content_type')) {
            $mime = mime_content_type($fileLocation);
        } elseif (function_exists('ext2mime')) {
            $mime = ext2mime($fileLocation);
        } else {
            $mime = 'application/octet-stream';
        }

        return $mime;
    }


    /**
     * @param $mailserver_url
     * @param $port
     * @param $ssltls
     * @param $smtp_auth_req
     * @param $smtp_username
     * @param $smtppassword
     * @param $fromaddress
     * @param $toaddress
     * @param string $mail_sendtype
     * @param string $fromname
     * @global $current_user
     * @global $app_strings
     * @return array
     */
    public function sendEmailTest(
        $mailserver_url,
        $port,
        $ssltls,
        $smtp_auth_req,
        $smtp_username,
        $smtppassword,
        $fromaddress,
        $toaddress,
        $mail_sendtype = 'smtp',
        $fromname = '',
        $authType = 'no_auth',
        $externalOAuthConnectionId = ''
    ) {
        global $current_user, $app_strings;
        $mod_strings = return_module_language($GLOBALS['current_language'], 'Emails'); //Called from EmailMan as well.
        $mail = new SugarPHPMailer(true);
        $mail->Mailer = strtolower($mail_sendtype);
        if ($mail->Mailer == 'smtp') {
            $mail->Host = $mailserver_url;
            $mail->Port = $port;

            $mail->setSecureProtocol($ssltls ?? false);
            $mail->initSMTPAuth(
                $authType ?? '',
                $externalOAuthConnectionId ?? '',
                $smtp_username ?? '',
                $smtppassword ?? ''
            );
        } else {
            $mail->Mailer = 'sendmail';
        }

        $mail->Subject = from_html($mod_strings['LBL_TEST_EMAIL_SUBJECT']);
        $mail->From = $fromaddress;
        isValidEmailAddress($mail->From);

        if ($fromname != '') {
            $mail->FromName = html_entity_decode($fromname, ENT_QUOTES);
        } else {
            $mail->FromName = $current_user->name;
        }

        $mail->Sender = $mail->From;
        isValidEmailAddress($mail->Sender);
        $mail->AddAddress($toaddress);
        $mail->Body = $mod_strings['LBL_TEST_EMAIL_BODY'];

        $return = array();

        if (!$mail->Send()) {
            ob_clean();
            $return['status'] = false;
            $return['errorMessage'] = $app_strings['LBL_EMAIL_ERROR_PREPEND'] . $mail->ErrorInfo;
            $return['fullSmtpLog'] = $mail->fullSmtpLog;
            return $return;
        } // if
        $return['status'] = true;

        return $return;
    } // fn

    /**
     * @param $htmlData
     * @return mixed
     */
    public function decodeDuringSend($htmlData)
    {
        $htmlData = str_replace("sugarLessThan", "&lt;", (string) $htmlData);
        $htmlData = str_replace("sugarGreaterThan", "&gt;", $htmlData);

        return $htmlData;
    }

    /**
     * Returns true or false if this email is a draft.
     *
     * @param array $request
     * @return bool True indicates this email is a draft.
     */
    public function isDraftEmail($request)
    {
        return (isset($request['saveDraft']) || ($this->type == 'draft' && $this->status == 'draft'));
    }

    /**
     *
     * Sends Email for Email 2.0
     *
     * @param $request
     * @global $mod_strings
     * @global $app_strings
     * @global $current_user
     * @global $sugar_config
     * @global $locale
     * @global $timedate
     * @global $beanList
     * @global $beanFiles
     * @return bool
     */
    public function email2Send($request)
    {
        global $mod_strings;
        global $app_strings;
        global $current_user;
        global $sugar_config;
        global $locale;
        global $timedate;
        global $beanList;
        global $beanFiles;

        $OBCharset = $locale->getPrecedentPreference('default_email_charset');
        $ie = null;

        /**********************************************************************
         * Sugar Email PREP
         */
        /* preset GUID */

        $orignialId = "";
        if (!empty($this->id)) {
            $orignialId = $this->id;
        } // if

        if (empty($this->id)) {
            $this->id = create_guid();
            $this->new_with_id = true;
        }

        /* satisfy basic HTML email requirements */
        $this->name = $request['sendSubject'];
        $this->description_html = '&lt;html&gt;&lt;body&gt;' . $request['sendDescription'] . '&lt;/body&gt;&lt;/html&gt;';

        /**********************************************************************
         * PHPMAILER PREP
         */
        $mail = new SugarPHPMailer();
        $mail = $this->setMailer($mail, '', $request['fromAccount']);
        if (empty($mail->Host) && !$this->isDraftEmail($request)) {
            $this->status = 'send_error';

            if ($mail->oe->type == 'system') {
                echo($app_strings['LBL_EMAIL_ERROR_PREPEND'] . $app_strings['LBL_EMAIL_INVALID_SYSTEM_OUTBOUND']);
            } else {
                echo($app_strings['LBL_EMAIL_ERROR_PREPEND'] . $app_strings['LBL_EMAIL_INVALID_PERSONAL_OUTBOUND']);
            }

            return false;
        }

        $subject = $this->name;
        $mail->Subject = from_html($this->name);

        // work-around legacy code in SugarPHPMailer
        if ($request['setEditor'] == 1) {
            $request['description_html'] = $request['sendDescription'];
            $this->description_html = $request['description_html'];
        } else {
            $this->description_html = '';
            $this->description = $request['sendDescription'];
        }
        // end work-around

        if ($this->isDraftEmail($request)) {
            if ($this->type != 'draft' && $this->status != 'draft') {
                $this->id = create_guid();
                $this->new_with_id = true;
                $this->date_entered = "";
            } // if
            $q1 = "update emails_email_addr_rel set deleted = 1 WHERE email_id = '{$this->id}'";
            $r1 = $this->db->query($q1);
        } // if

        if (isset($request['saveDraft'])) {
            $this->type = 'draft';
            $this->status = 'draft';
            $forceSave = true;
        } else {
            /* Apply Email Templates */
            // do not parse email templates if the email is being saved as draft....
            $toAddresses = $this->email2ParseAddresses($request['sendTo']);
            $sea = new SugarEmailAddress();
            $object_arr = array();

            if (isset($request['parent_type']) && !empty($request['parent_type']) &&
                isset($request['parent_id']) && !empty($request['parent_id']) &&
                in_array($request['parent_type'], ['Accounts', 'Cases', 'Contacts', 'Leads', 'Users', 'Prospects'])) {
                if (isset($beanList[$request['parent_type']]) && !empty($beanList[$request['parent_type']])) {
                    $className = $beanList[$request['parent_type']];
                    if (isset($beanFiles[$className]) && !empty($beanFiles[$className])) {
                        if (!class_exists($className)) {
                            require_once($beanFiles[$className]);
                        }
                        $bean = new $className();
                        $bean->retrieve($request['parent_id']);
                        $object_arr[$bean->module_dir] = $bean->id;
                    } // if
                } // if
            }
            foreach ($toAddresses as $addrMeta) {
                $addr = $addrMeta['email'];
                $beans = $sea->getBeansByEmailAddress($addr);
                foreach ($beans as $bean) {
                    if (!isset($object_arr[$bean->module_dir])) {
                        $object_arr[$bean->module_dir] = $bean->id;
                    }
                }
            }

            /* template parsing */
            if (empty($object_arr)) {
                $object_arr= array('Contacts' => '123');
            }
            $object_arr['Users'] = $current_user->id;
            $this->description_html = EmailTemplate::parse_template($this->description_html, $object_arr);
            $this->name = EmailTemplate::parse_template($this->name, $object_arr);
            $this->description = EmailTemplate::parse_template($this->description, $object_arr);
            $this->description = html_entity_decode((string) $this->description, ENT_COMPAT, 'UTF-8');
            if ($this->type != 'draft' && $this->status != 'draft') {
                $this->id = create_guid();
                $this->date_entered = "";
                $this->new_with_id = true;
                $this->type = 'out';
                $this->status = 'sent';
            }
        }

        if (isset($request['parent_type']) && empty($request['parent_type']) &&
            isset($request['parent_id']) && empty($request['parent_id'])) {
            $this->parent_id = "";
            $this->parent_type = "";
        } // if


        $mail->Subject = $this->name;
        $mail = $this->handleBody($mail);
        $mail->Subject = $this->name;
        $this->description_html = from_html($this->description_html);
        $this->description_html = $this->decodeDuringSend($this->description_html);
        $this->description = $this->decodeDuringSend($this->description);

        /* from account */
        $replyToAddress = $current_user->emailAddress->getReplyToAddress($current_user, true);
        $replyToName = "";
        if (empty($request['fromAccount'])) {
            $defaults = $current_user->getPreferredEmail();
            $mail->From = $defaults['email'];
            isValidEmailAddress($mail->From);
            $mail->FromName = $defaults['name'];
            $replyToName = $mail->FromName;
        //$replyToAddress = $current_user->emailAddress->getReplyToAddress($current_user);
        } else {
            // passed -> user -> system default
            $ie = BeanFactory::newBean('InboundEmail');
            $ie->retrieve($request['fromAccount']);
            $storedOptions = sugar_unserialize(base64_decode($ie->stored_options));
            $fromName = "";
            $fromAddress = "";
            $replyToName = "";
            //$replyToAddress = "";
            if (!empty($storedOptions)) {
                $fromAddress = $storedOptions['from_addr'];
                isValidEmailAddress($fromAddress);
                $fromName = from_html($storedOptions['from_name']);
                $replyToAddress = (isset($storedOptions['reply_to_addr']) ? $storedOptions['reply_to_addr'] : "");
                $replyToName = (isset($storedOptions['reply_to_name']) ? from_html($storedOptions['reply_to_name']) : "");
            } // if
            $defaults = $current_user->getPreferredEmail();
            // Personal Account doesn't have reply To Name and Reply To Address. So add those columns on UI
            // After adding remove below code

            // code to remove
            if ($ie->is_personal) {
                if (empty($replyToAddress)) {
                    $replyToAddress = $current_user->emailAddress->getReplyToAddress($current_user, true);
                } // if
                if (empty($replyToName)) {
                    $replyToName = $defaults['name'];
                } // if
                //Personal accounts can have a reply_address, which should
                //overwrite the users set default.
                if (!empty($storedOptions['reply_to_addr'])) {
                    $replyToAddress = $storedOptions['reply_to_addr'];
                }
            }
            // end of code to remove
            $mail->From = (!empty($fromAddress)) ? $fromAddress : $defaults['email'];
            isValidEmailAddress($mail->From);
            $mail->FromName = (!empty($fromName)) ? $fromName : $defaults['name'];
            $replyToName = (!empty($replyToName)) ? $replyToName : $mail->FromName;
        }

        $mail->Sender = $mail->From; /* set Return-Path field in header to reduce spam score in emails sent via Sugar's Email module */
        isValidEmailAddress($mail->Sender);

        if (!empty($replyToAddress)) {
            $mail->AddReplyTo($replyToAddress, $locale->translateCharsetMIME(trim($replyToName), 'UTF-8', $OBCharset));
        } else {
            $mail->AddReplyTo($mail->From, $locale->translateCharsetMIME(trim($mail->FromName), 'UTF-8', $OBCharset));
        } // else
        $emailAddressCollection = array(); // used in linking to beans below
        // handle to/cc/bcc
        foreach ($this->email2ParseAddresses($request['sendTo']) as $addr_arr) {
            if (empty($addr_arr['email'])) {
                continue;
            }

            if (empty($addr_arr['display'])) {
                $mail->AddAddress($addr_arr['email'], "");
            } else {
                $mail->AddAddress(
                    $addr_arr['email'],
                    $locale->translateCharsetMIME(trim($addr_arr['display']), 'UTF-8', $OBCharset)
                );
            }
            $emailAddressCollection[] = $addr_arr['email'];
        }
        foreach ($this->email2ParseAddresses($request['sendCc']) as $addr_arr) {
            if (empty($addr_arr['email'])) {
                continue;
            }

            if (empty($addr_arr['display'])) {
                $mail->AddCC($addr_arr['email'], "");
            } else {
                $mail->AddCC(
                    $addr_arr['email'],
                    $locale->translateCharsetMIME(trim($addr_arr['display']), 'UTF-8', $OBCharset)
                );
            }
            $emailAddressCollection[] = $addr_arr['email'];
        }

        foreach ($this->email2ParseAddresses($request['sendBcc']) as $addr_arr) {
            if (empty($addr_arr['email'])) {
                continue;
            }

            if (empty($addr_arr['display'])) {
                $mail->AddBCC($addr_arr['email'], "");
            } else {
                $mail->AddBCC(
                    $addr_arr['email'],
                    $locale->translateCharsetMIME(trim($addr_arr['display']), 'UTF-8', $OBCharset)
                );
            }
            $emailAddressCollection[] = $addr_arr['email'];
        }


        /* parse remove attachments array */
        $removeAttachments = array();
        if (!empty($request['templateAttachmentsRemove'])) {
            $exRemove = explode("::", $request['templateAttachmentsRemove']);

            foreach ($exRemove as $file) {
                $removeAttachments = substr($file, 0, 36);
            }
        }

        /* handle attachments */
        if (!empty($request['attachments'])) {
            $exAttachments = explode("::", $request['attachments']);

            foreach ($exAttachments as $file) {
                $file = trim(from_html($file));
                $file = str_replace("\\", "", $file);
                if (!empty($file)) {
                    //$fileLocation = $this->et->userCacheDir."/{$file}";
                    $fileGUID = preg_replace('/[^a-z0-9\-]/', "", substr($file, 0, 36));
                    $fileLocation = $this->et->userCacheDir . "/{$fileGUID}";
                    $filename = substr(
                        $file,
                        36,
                        strlen($file)
                    ); // strip GUID	for PHPMailer class to name outbound file

                    $mail->AddAttachment($fileLocation, $filename, 'base64', $this->email2GetMime($fileLocation));
                    //$mail->AddAttachment($fileLocation, $filename, 'base64');

                    // only save attachments if we're archiving or drafting
                    if ((($this->type == 'draft') && !empty($this->id)) || (isset($request['saveToSugar']) && $request['saveToSugar'] == 1)) {
                        $note = BeanFactory::newBean('Notes');
                        $note->id = create_guid();
                        $note->new_with_id = true; // duplicating the note with files
                        $note->parent_id = $this->id;
                        $note->parent_type = $this->module_dir;
                        $note->name = $filename;
                        $note->filename = $filename;
                        $note->file_mime_type = $this->email2GetMime($fileLocation);
                        $dest = "upload://{$note->id}";
                        if (!copy($fileLocation, $dest)) {
                            $GLOBALS['log']->debug("EMAIL 2.0: could not copy attachment file to $fileLocation => $dest");
                        }

                        $note->save();
                    }
                }
            }
        }

        /* handle sugar documents */
        if (!empty($request['documents'])) {
            $exDocs = explode("::", $request['documents']);

            foreach ($exDocs as $docId) {
                $docId = trim($docId);
                if (!empty($docId)) {
                    $doc = BeanFactory::newBean('Documents');
                    $docRev = BeanFactory::newBean('DocumentRevisions');
                    $doc->retrieve($docId);
                    $docRev->retrieve($doc->document_revision_id);

                    $filename = $docRev->filename;
                    $docGUID = preg_replace('/[^a-z0-9\-]/', "", (string) $docRev->id);
                    $fileLocation = "upload/{$docGUID}";
                    $mime_type = $docRev->file_mime_type;
                    $mail->AddAttachment(
                        $fileLocation,
                        $locale->translateCharsetMIME(trim($filename), 'UTF-8', $OBCharset),
                        'base64',
                        $mime_type
                    );

                    // only save attachments if we're archiving or drafting
                    if ((($this->type == 'draft') && !empty($this->id)) || (isset($request['saveToSugar']) && $request['saveToSugar'] == 1)) {
                        $note = BeanFactory::newBean('Notes');
                        $note->id = create_guid();
                        $note->new_with_id = true; // duplicating the note with files
                        $note->parent_id = $this->id;
                        $note->parent_type = $this->module_dir;
                        $note->name = $filename;
                        $note->filename = $filename;
                        $note->file_mime_type = $mime_type;
                        $dest = "upload/{$note->id}";
                        if (!copy($fileLocation, $dest)) {
                            $GLOBALS['log']->debug("EMAIL 2.0: could not copy SugarDocument revision file $fileLocation => $dest");
                        }

                        $note->save();
                    }
                }
            }
        }

        /* handle template attachments */
        if (!empty($request['templateAttachments'])) {
            $exNotes = explode("::", $request['templateAttachments']);
            foreach ($exNotes as $noteId) {
                $noteId = trim($noteId);
                if (!empty($noteId)) {
                    $note = BeanFactory::newBean('Notes');
                    $note->retrieve($noteId);
                    if (!empty($note->id)) {
                        $filename = $note->filename;
                        $noteGUID = preg_replace('/[^a-z0-9\-]/', "", (string) $note->id);
                        $fileLocation = "upload/{$noteGUID}";
                        $mime_type = $note->file_mime_type;
                        if (!$note->embed_flag) {
                            $mail->AddAttachment($fileLocation, $filename, 'base64', $mime_type);
                            // only save attachments if we're archiving or drafting
                            if ((($this->type == 'draft') && !empty($this->id)) || (isset($request['saveToSugar']) && $request['saveToSugar'] == 1)) {
                                if ($note->parent_id != $this->id) {
                                    $this->saveTempNoteAttachments($filename, $fileLocation, $mime_type);
                                }
                            } // if
                        } // if
                    } else {
                        //$fileLocation = $this->et->userCacheDir."/{$file}";
                        $fileGUID = preg_replace('/[^a-z0-9\-]/', "", substr($noteId, 0, 36));
                        $fileLocation = $this->et->userCacheDir . "/{$fileGUID}";
                        //$fileLocation = $this->et->userCacheDir."/{$noteId}";
                        $filename = substr(
                            $noteId,
                            36,
                            strlen($noteId)
                        ); // strip GUID	for PHPMailer class to name outbound file

                        $mail->AddAttachment(
                            $fileLocation,
                            $locale->translateCharsetMIME(trim($filename), 'UTF-8', $OBCharset),
                            'base64',
                            $this->email2GetMime($fileLocation)
                        );

                        //If we are saving an email we were going to forward we need to save the attachments as well.
                        if ((($this->type == 'draft') && !empty($this->id))
                            || (isset($request['saveToSugar']) && $request['saveToSugar'] == 1)
                        ) {
                            $mimeType = $this->email2GetMime($fileLocation);
                            $this->saveTempNoteAttachments($filename, $fileLocation, $mimeType);
                        } // if
                    }
                }
            }
        }


        /**********************************************************************
         * Final Touches
         */
        /* save email to sugar? */
        $forceSave = false;

        if ($this->type == 'draft' && !isset($request['saveDraft'])) {
            // sending a draft email
            $this->type = 'out';
            $this->status = 'sent';
            $forceSave = true;
        } elseif (isset($request['saveDraft'])) {
            $this->type = 'draft';
            $this->status = 'draft';
            $forceSave = true;
        }

        /**********************************************************************
         * SEND EMAIL (finally!)
         */
        $mailSent = false;
        if ($this->type != 'draft') {
            $mail->prepForOutbound();
            $mail->Body = $this->decodeDuringSend($mail->Body);
            $mail->AltBody = $this->decodeDuringSend($mail->AltBody);
            if (!$mail->Send()) {
                $this->status = 'send_error';
                ob_clean();
                echo($app_strings['LBL_EMAIL_ERROR_PREPEND'] . $mail->ErrorInfo);

                return false;
            }
        }

        if ((!(empty($orignialId) || isset($request['saveDraft']) || ($this->type == 'draft' && $this->status == 'draft'))) &&
            (($request['composeType'] == 'reply') || ($request['composeType'] == 'replyAll') || ($request['composeType'] == 'replyCase')) && ($orignialId != $this->id)) {
            $originalEmail = BeanFactory::newBean('Emails');
            $originalEmail->retrieve($orignialId);
            $originalEmail->reply_to_status = 1;
            $originalEmail->save();
            $this->reply_to_status = 0;
        } // if

        if ($request['composeType'] == 'reply' || $request['composeType'] == 'replyCase') {
            if (isset($request['ieId']) && isset($request['mbox'])) {
                $emailFromIe = BeanFactory::newBean('InboundEmail');
                $emailFromIe->retrieve($request['ieId']);
                $emailFromIe->mailbox = $request['mbox'];
                if (isset($emailFromIe->id) && $emailFromIe->is_personal) {
                    if ($emailFromIe->isPop3Protocol()) {
                        $emailFromIe->mark_answered($this->uid, 'pop3');
                    } elseif ($emailFromIe->connectMailserver() == 'true') {
                        $emailFromIe->markEmails($this->uid, 'answered');
                        $emailFromIe->mark_answered($this->uid);
                    }
                }
            }
        }


        if ($forceSave ||
            $this->type == 'draft' ||
            (isset($request['saveToSugar']) && $request['saveToSugar'] == 1)
        ) {

            // saving a draft OR saving a sent email
            $decodedFromName = mb_decode_mimeheader($mail->FromName);
            $this->from_addr = "{$decodedFromName} <{$mail->From}>";
            $this->from_addr_name = $this->from_addr;
            $this->to_addrs = $request['sendTo'];
            $this->to_addrs_names = $request['sendTo'];
            $this->cc_addrs = $request['sendCc'];
            $this->cc_addrs_names = $request['sendCc'];
            $this->bcc_addrs = $request['sendBcc'];
            $this->bcc_addrs_names = $request['sendBcc'];
            $this->assigned_user_id = $current_user->id;

            $this->date_sent_received = $timedate->now();
            ///////////////////////////////////////////////////////////////////
            ////	LINK EMAIL TO SUGARBEANS BASED ON EMAIL ADDY

            if (isset($request['parent_type']) && !empty($request['parent_type']) &&
                isset($request['parent_id']) && !empty($request['parent_id'])) {
                $this->parent_id = $request['parent_id'];
                $this->parent_type = $request['parent_type'];
                $q = "SELECT count(*) c FROM emails_beans WHERE  email_id = '{$this->id}' AND bean_id = '{$request['parent_id']}' AND bean_module = '{$request['parent_type']}'";
                $r = $this->db->query($q);
                $a = $this->db->fetchByAssoc($r);
                if ($a['c'] <= 0) {
                    if (isset($beanList[$request['parent_type']]) && !empty($beanList[$request['parent_type']])) {
                        $className = $beanList[$request['parent_type']];
                        if (isset($beanFiles[$className]) && !empty($beanFiles[$className])) {
                            if (!class_exists($className)) {
                                require_once($beanFiles[$className]);
                            }
                            $bean = new $className();
                            $bean->retrieve($request['parent_id']);
                            if ($bean->load_relationship('emails')) {
                                $bean->emails->add($this->id);
                            } // if
                        } // if
                    } // if
                } // if
            } else {
                if (!class_exists('aCase')) {
                } else {
                    $c = BeanFactory::newBean('Cases');
                    if ($caseId = InboundEmail::getCaseIdFromCaseNumber($mail->Subject, $c)) {
                        $c->retrieve($caseId);
                        $c->load_relationship('emails');
                        $c->emails->add($this->id);
                        $this->parent_type = "Cases";
                        $this->parent_id = $caseId;
                    } // if
                }
            } // else

            ////	LINK EMAIL TO SUGARBEANS BASED ON EMAIL ADDY
            ///////////////////////////////////////////////////////////////////
            $this->save();
        }

        if (!empty($request['fromAccount'])) {
            if (isset($ie->id) && !$ie->isPop3Protocol() && $mail->oe->mail_smtptype != 'gmail') {
                $sentFolder = $ie->get_stored_options("sentFolder");
                if (!empty($sentFolder)) {
                    // Call CreateBody() before CreateHeader() as that is where boundary IDs are generated.
                    $emailbody = $mail->CreateBody();
                    $emailheader = $mail->CreateHeader();
                    $data = $emailheader . "\r\n" . $emailbody . "\r\n";
                    $ie->mailbox = $sentFolder;
                    if ($ie->connectMailserver() == 'true') {
                        $connectString = $ie->getConnectString($ie->getServiceString(), $ie->mailbox);
                        $returnData = $ie->getImap()->append($connectString, $data, "\\Seen");
                        if (!$returnData) {
                            $GLOBALS['log']->debug("could not copy email to {$ie->mailbox} for {$ie->name}");
                        } // if
                    } else {
                        $GLOBALS['log']->debug("could not connect to mail serve for folder {$ie->mailbox} for {$ie->name}");
                    } // else
                } else {
                    $GLOBALS['log']->debug("could not copy email to {$ie->mailbox} sent folder as its empty");
                } // else
            } // if
        } // if
        return true;
    } // end email2send

    /**
     * Generates a config-specified separated name and addresses to be used in compose email screen for
     * contacts or leads from listview
     * By default, use comma, but allow for non-standard delimiters as specified in email_address_separator
     *
     * @param $module string module name
     * @param $idsArray array of record ids to get the email address for
     * @global $local
     * @return string (config-specified) delimited list of email addresses
     */
    public function getNamePlusEmailAddressesForCompose($module, $idsArray)
    {
        global $locale;
        $result = array();

        foreach ($idsArray as $id) {
            // Load bean
            $bean = BeanFactory::getBean($module, $id);

            // Got a bean
            if (!empty($bean)) {
                // For CE, just get primary e-mail address
                $emailAddress = $bean->email1;


                // If we have an e-mail address loaded
                if (!empty($emailAddress)) {
                    // Use bean name by default
                    $fullName = $bean->name;

                    // Depending on module, format the name
                    if (in_array($module, array('Users', 'Employees'))) {
                        $fullName = from_html(
                            $locale->getLocaleFormattedName(
                                $bean->first_name,
                                $bean->last_name,
                                '',
                                $bean->title
                            )
                        );
                    } else {
                        if (SugarModule::get($module)->moduleImplements('Person')) {
                            $fullName = from_html(
                                $locale->getLocaleFormattedName(
                                    $bean->first_name,
                                    $bean->last_name,
                                    $bean->salutation,
                                    $bean->title
                                )
                            );
                        }
                    }

                    // Make e-mail address in format "Name <@email>"
                    $result[$bean->id] = $fullName . " <" . from_html($emailAddress) . ">";
                }
            }
        }

        // Broken out of method to facilitate unit testing
        return $this->_arrayToDelimitedString($result);
    }

    /**
     * @param Array $arr - list of strings
     * @return string the list of strings delimited by email_address_separator
     */
    public function _arrayToDelimitedString($arr)
    {
        // bug 51804: outlook does not respect the correct email address separator (',') , so let
        // clients override the default.
        $separator = (isset($GLOBALS['sugar_config']['email_address_separator']) &&
            !empty($GLOBALS['sugar_config']['email_address_separator'])) ?
            $GLOBALS['sugar_config']['email_address_separator'] :
            ',';

        return implode($separator, array_values($arr));
    }

    /**
     * Overrides
     */
    ///////////////////////////////////////////////////////////////////////////
    ////	SAVERS
    /**
     * @param bool $check_notify
     * @global $current_user
     * @return bool|string
     */
    public function save($check_notify = false)
    {
        global $current_user;

        $id = false;

        if ($this->isDuplicate) {
            $GLOBALS['log']->debug("EMAIL - tried to save a duplicate Email record");
        } else {
            if (empty($this->id)) {
                $this->id = create_guid();
                $this->new_with_id = true;
            }

            if ($this->cleanEmails($this->from_addr_name) === '') {
                $this->from_addr_name = $this->cleanEmails($this->from_name);
            } else {
                $this->from_addr_name = $this->cleanEmails($this->from_addr_name);
            }
            $this->to_addrs_names = $this->cleanEmails($this->to_addrs_names);
            $this->cc_addrs_names = $this->cleanEmails($this->cc_addrs_names);
            $this->bcc_addrs_names = $this->cleanEmails($this->bcc_addrs_names);
            $this->reply_to_addr = $this->cleanEmails($this->reply_to_addr);
            $this->description = SugarCleaner::cleanHtml($this->description ?? '');
            if (empty($this->description_html)) {
                $this->description_html = $this->description;
                $this->description_html = nl2br($this->description_html);
            }
            $this->description_html = SugarCleaner::cleanHtml($this->description_html ?? '', true);
            $this->raw_source = SugarCleaner::cleanHtml($this->raw_source ?? '', true);
            $this->saveEmailText();
            $this->saveEmailAddresses();


            if (empty($this->assigned_user_id)) {
                $this->assigned_user_id = $current_user->id;
            }

            $GLOBALS['log']->debug('-------------------------------> Email called save()');

            if (empty($this->date_sent_received)) {
                global $timedate;

                $date_sent_received_obj = $timedate->fromUser(
                    $timedate->merge_date_time($this->date_start, $this->time_start),
                    $current_user
                );

                if ($date_sent_received_obj !== null && ($date_sent_received_obj instanceof SugarDateTime)) {
                    $this->date_sent_received = $date_sent_received_obj->asDb();
                }
            }


            $validator = new EmailFromValidator();
            if (!defined('SUGARCRM_IS_INSTALLING') && !$validator->isValid($this)) {
                $errors = $validator->getErrorsAsText();
                $details = "Details:\n{$errors['messages']}\ncodes:{$errors['codes']}";
                LoggerManager::getLogger()->error("Saving Email with invalid From name and/or Address. $details");
            }


            if ((!isset($this->date_sent_received) || !$this->date_sent_received) && in_array($this->status, ['sent', 'replied'])) {
                $this->date_sent_received = TimeDate::getInstance()->nowDb();
            }

            $id = parent::save($check_notify);

            if (!empty($this->parent_type) && !empty($this->parent_id)) {
                if (!empty($this->fetched_row) && !empty($this->fetched_row['parent_id']) && !empty($this->fetched_row['parent_type'])) {
                    if ($this->fetched_row['parent_id'] != $this->parent_id || $this->fetched_row['parent_type'] != $this->parent_type) {
                        $mod = strtolower($this->fetched_row['parent_type']);
                        $rel = array_key_exists(
                            $mod,
                            $this->field_defs
                        ) ? $mod : $mod . "_activities_emails"; //Custom modules rel name
                        if ($this->load_relationship($rel)) {
                            $this->$rel->delete($this->id, $this->fetched_row['parent_id']);
                        }
                    }
                }
                $mod = strtolower($this->parent_type);
                $rel = array_key_exists(
                    $mod,
                    $this->field_defs
                ) ? $mod : $mod . "_activities_emails"; //Custom modules rel name
                if ($this->load_relationship($rel)) {
                    $this->$rel->add($this->parent_id);
                }
            }
        }
        $GLOBALS['log']->debug('-------------------------------> Email save() done');

        return $id;
    }

    /**
     * Helper function to save temporary attachments assocaited to an email as note.
     *
     * @param string $filename
     * @param string $fileLocation
     * @param string $mimeType
     * @return boolean success/failed
     */
    public function saveTempNoteAttachments($filename, $fileLocation, $mimeType)
    {
        $tmpNote = BeanFactory::newBean('Notes');
        $tmpNote->id = create_guid();
        $tmpNote->new_with_id = true;
        $tmpNote->parent_id = $this->id;
        $tmpNote->parent_type = $this->module_dir;
        $tmpNote->name = $filename;
        $tmpNote->filename = $filename;
        $tmpNote->file_mime_type = $mimeType;
        $noteFile = "upload://{$tmpNote->id}";

        if (!file_exists($fileLocation)) {
            LoggerManager::getLogger()->warn('Email error: File Location not found for save temp note attachments. File location was: "' . $fileLocation . '"');
        } else {
            if (!copy($fileLocation, $noteFile)) {
                $GLOBALS['log']->fatal("EMAIL 2.0: could not copy SugarDocument revision file $fileLocation => $noteFile");
            } else {
                if (!$tmpNote->save()) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Handles normalization of Email Addressess
     */
    public function saveEmailAddresses()
    {
        // from, single address
        $fromId = $this->emailAddress->getEmailGUID(from_html($this->from_addr));
        if (!empty($fromId)) {
            $this->linkEmailToAddress($fromId, 'from');
        }

        // to, multiple
        $replace = array(",", ";");
        $toaddrs = str_replace($replace, "::", (string) from_html($this->to_addrs));
        $exToAddrs = explode("::", $toaddrs);

        if (!empty($exToAddrs)) {
            foreach ($exToAddrs as $toaddr) {
                $toaddr = trim($toaddr);
                if (!empty($toaddr)) {
                    $toId = $this->emailAddress->getEmailGUID($toaddr);
                    $this->linkEmailToAddress($toId, 'to');
                    $this->sendOptInEmailToEmailAddressById($toId);
                }
            }
        }

        // cc, multiple
        $ccAddrs = str_replace($replace, "::", (string) from_html($this->cc_addrs));
        $exccAddrs = explode("::", $ccAddrs);

        if (!empty($exccAddrs)) {
            foreach ($exccAddrs as $ccAddr) {
                $ccAddr = trim($ccAddr);
                if (!empty($ccAddr)) {
                    $ccId = $this->emailAddress->getEmailGUID($ccAddr);
                    $this->linkEmailToAddress($ccId, 'cc');
                    $this->sendOptInEmailToEmailAddressById($ccId);
                }
            }
        }

        // bcc, multiple
        $bccAddrs = str_replace($replace, "::", (string) from_html($this->bcc_addrs));
        $exbccAddrs = explode("::", $bccAddrs);
        if (!empty($exbccAddrs)) {
            foreach ($exbccAddrs as $bccAddr) {
                $bccAddr = trim($bccAddr);
                if (!empty($bccAddr)) {
                    $bccId = $this->emailAddress->getEmailGUID($bccAddr);
                    $this->linkEmailToAddress($bccId, 'bcc');
                    $this->sendOptInEmailToEmailAddressById($bccId);
                }
            }
        }
    }

    public function linkEmailToAddress($id, $type)
    {
        // TODO: make this update?
        $q1 = "SELECT * FROM emails_email_addr_rel WHERE email_id = '{$this->id}' AND email_address_id = '{$id}' AND address_type = '{$type}' AND deleted = 0";
        $r1 = $this->db->query($q1);
        $a1 = $this->db->fetchByAssoc($r1);

        if (!empty($a1) && !empty($a1['id'])) {
            return $a1['id'];
        }
        $guid = create_guid();
        $q2 = "INSERT INTO emails_email_addr_rel VALUES('{$guid}', '{$this->id}', '{$type}', '{$id}', 0)";
        $r2 = $this->db->query($q2);


        return $guid;
    }

    protected $email_to_text = array(
        "email_id" => "id",
        "description" => "description",
        "description_html" => "description_html",
        "raw_source" => "raw_source",
        "from_addr" => "from_addr_name",
        "reply_to_addr" => "reply_to_addr",
        "to_addrs" => "to_addrs_names",
        "cc_addrs" => "cc_addrs_names",
        "bcc_addrs" => "bcc_addrs_names",
    );

    public function cleanEmails($emails)
    {
        if (empty($emails)) {
            return '';
        }
        $emails = str_replace(array(",", ";"), "::", (string) from_html($emails));
        $addrs = explode("::", $emails);
        $res = array();
        foreach ($addrs as $addr) {
            $parts = $this->emailAddress->splitEmailAddress($addr);
            if (empty($parts["email"])) {
                continue;
            }
            if (!empty($parts["name"])) {
                $res[] = "{$parts['name']} <{$parts['email']}>";
            } else {
                $res[] .= $parts["email"];
            }
        }

        return implode(", ", $res);
    }

    protected function saveEmailText()
    {
        $emailText = SugarModule::get("EmailText")->loadBean();
        foreach ($this->email_to_text as $textfield => $mailfield) {
            $emailText->{$textfield} = $this->{$mailfield};
        }
        $emailText->email_id = $this->id;
        if (!$this->new_with_id) {
            $this->db->update($emailText);
        } else {
            $this->db->insert($emailText);
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    ////	RETRIEVERS
    public function retrieve($id = -1, $encoded = true, $deleted = true)
    {
        // cn: bug 11915, return SugarBean's retrieve() call bean instead of $this
        $email = parent::retrieve($id, $encoded, $deleted);

        if ($email) {
            $email->retrieveEmailText();
            //$ret->raw_source = SugarCleaner::cleanHtml($ret->raw_source);
            $email->description = $email->description;
            if (empty($email->description_html)) {
                $email->description_html = $email->description;
                $email->description_html = nl2br($email->description_html);
            }
            //$ret->description_html = SugarCleaner::cleanHtml($ret->description_html);
            $email->retrieveEmailAddresses();

            $email->date_start = '';
            $email->time_start = '';
            $dateSent = explode(' ', $email->date_sent_received);
            if (!empty($dateSent)) {
                $email->date_start = $dateSent[0];
                if (isset($dateSent[1])) {
                    $email->time_start = $dateSent[1];
                }
            }
        }

        return $email;
    }


    /**
     * Retrieves email addresses from GUIDs
     */
    public function retrieveEmailAddresses()
    {
        $return = array();

        $q = "SELECT email_address, address_type
				FROM emails_email_addr_rel eam
				JOIN email_addresses ea ON ea.id = eam.email_address_id
				WHERE eam.email_id = '{$this->id}' AND eam.deleted=0";
        $r = $this->db->query($q);

        while ($a = $this->db->fetchByAssoc($r)) {
            if (!isset($return[$a['address_type']])) {
                $return[$a['address_type']] = array();
            }
            $return[$a['address_type']][] = $a['email_address'];
        }

        if (count($return) > 0) {
            if (isset($return['from'])) {
                $this->from_addr_name = implode(", ", $return['from']);
            }
            if (isset($return['to'])) {
                $this->to_addrs_names = implode(", ", $return['to']);
            }
            if (isset($return['cc'])) {
                $this->cc_addrs_names = implode(", ", $return['cc']);
            }
            if (isset($return['bcc'])) {
                $this->bcc_addrs_names = implode(", ", $return['bcc']);
            }
        }
    }

    /**
     * Handles longtext fields
     */
    public function retrieveEmailText()
    {
        $q = "SELECT from_addr, reply_to_addr, to_addrs, cc_addrs, bcc_addrs, description, description_html, raw_source FROM emails_text WHERE email_id = '{$this->id}'";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r, false);

        $this->description = $a['description'];
        $this->description_html = $a['description_html'];
        $this->raw_source = $a['raw_source'];
        $this->from_addr_name = $a['from_addr'];
        $this->reply_to_addr = $a['reply_to_addr'];
        $this->to_addrs_names = $a['to_addrs'];
        $this->cc_addrs_names = $a['cc_addrs'];
        $this->bcc_addrs_names = $a['bcc_addrs'];
    }

    /**
     * @param string $id
     */
    public function delete($id = '')
    {
        if (empty($id)) {
            $id = $this->id;
        }

        $id = $this->db->quote($id);

        $q = "UPDATE emails SET deleted = 1 WHERE id = '{$id}'";
        $qt = "UPDATE emails_text SET deleted = 1 WHERE email_id = '{$id}'";
        $qf = "UPDATE folders_rel SET deleted = 1 WHERE polymorphic_id = '{$id}' AND polymorphic_module = 'Emails'";
        $qn = "UPDATE notes SET deleted = 1 WHERE parent_id = '{$id}' AND parent_type = 'Emails'";
        $this->db->query($q);
        $this->db->query($qt);
        $this->db->query($qf);
        $this->db->query($qn);
    }

    /**
     * creates the standard "Forward" info at the top of the forwarded message
     * @return string
     */
    public function getForwardHeader()
    {
        global $mod_strings;
        global $current_user;

        //$from = str_replace(array("&gt;","&lt;"), array(")","("), $this->from_name);
        $from = to_html($this->from_name);
        $subject = to_html($this->name);
        $ret = "<br /><br />";
        $ret .= $this->replyDelimiter . "{$mod_strings['LBL_FROM']} {$from}<br />";
        $ret .= $this->replyDelimiter . "{$mod_strings['LBL_DATE_SENT_RECEIVED']} {$this->date_sent_received}<br />";
        $ret .= $this->replyDelimiter . "{$mod_strings['LBL_TO']} {$this->to_addrs}<br />";
        $ret .= $this->replyDelimiter . "{$mod_strings['LBL_CC']} {$this->cc_addrs}<br />";
        $ret .= $this->replyDelimiter . "{$mod_strings['LBL_SUBJECT']} {$subject}<br />";
        $ret .= $this->replyDelimiter . "<br />";

        return $ret;
        //return from_html($ret);
    }

    /**
     * retrieves Notes that belong to this Email and stuffs them into the "attachments" attribute
     */
    public function getNotes($id, $duplicate = false)
    {
        if (!class_exists('Note')) {
        }

        $exRemoved = array();
        if (isset($_REQUEST['removeAttachment'])) {
            $exRemoved = explode('::', $_REQUEST['removeAttachment']);
        }

        $noteDupe = null;
        $noteArray = array();
        $q = "SELECT id FROM notes WHERE deleted = 0 AND parent_id = " . $this->db->quoted($id);
        $r = $this->db->query($q);

        while ($a = $this->db->fetchByAssoc($r)) {
            if (!in_array($a['id'], $exRemoved)) {
                $note = BeanFactory::newBean('Notes');
                $note->retrieve($a['id']);

                // duplicate actual file when creating forwards
                if ($duplicate) {
                    if (!class_exists('UploadFile')) {
                        require_once('include/upload_file.php');
                    }
                    // save a brand new Note
                    $noteDupe->id = create_guid();
                    $noteDupe->new_with_id = true;
                    $noteDupe->parent_id = $this->id;
                    $noteDupe->parent_type = $this->module_dir;

                    $noteFile = new UploadFile();
                    $noteFile->duplicate_file($a['id'], $note->id, $note->filename);

                    $note->save();
                }
                // add Note to attachments array
                $this->attachments[] = $note;
            }
        }
    }

    /**
     * creates the standard "Reply" info at the top of the forwarded message
     * @global $mod_string
     * @global $current_user
     * @return string
     */
    public function getReplyHeader()
    {
        global $mod_strings;
        global $current_user;

        $from = str_replace(array("&gt;", "&lt;", ">", "<"), array(")", "(", ")", "("), $this->from_name);
        $ret = "<br>{$mod_strings['LBL_REPLY_HEADER_1']} {$this->date_start}, {$this->time_start}, {$from} {$mod_strings['LBL_REPLY_HEADER_2']}";

        return from_html($ret);
    }

    /**
     * Quotes plain-text email text
     * @param string $text
     * @return string
     */
    public function quotePlainTextEmail($text)
    {
        $quoted = "\n";

        // plain-text
        $desc = nl2br(trim($text));
        $exDesc = explode('<br />', $desc);

        foreach ($exDesc as $k => $line) {
            $quoted .= '> ' . trim($line) . "\r";
        }

        return $quoted;
    }

    /**
     * "quotes" (i.e., "> my text yadda" the HTML part of an email
     * @param string $text HTML text to quote
     * @return string
     */
    public function quoteHtmlEmail($text)
    {
        $text = trim(from_html($text));

        if (empty($text)) {
            return '';
        }
        $out = "<div style='border-left:1px solid #00c; padding:5px; margin-left:10px;'>{$text}</div>";

        return $out;
    }

    /**
     * "quotes" (i.e., "> my text yadda" the HTML part of an email
     * @param string $text HTML text to quote
     * @return string
     */
    public function quoteHtmlEmailForNewEmailUI($text)
    {
        $text = trim($text);

        if (empty($text)) {
            return '';
        }
        $text = str_replace("\n", "<BR/>", $text);
        $out = "<div style='border-left:1px solid #00c; padding:5px; margin-left:10px;'>{$text}</div>";

        return $out;
    }

    /**
     * Ensures that the user is able to send outbound emails
     * @global $current_user
     */
    public function check_email_settings()
    {
        global $current_user;

        $mail_fromaddress = $current_user->emailAddress->getPrimaryAddress($current_user);
        $replyToName = $current_user->getPreference('mail_fromname');
        $mail_fromname = (!empty($replyToName)) ? $current_user->getPreference('mail_fromname') : $current_user->full_name;

        if (empty($mail_fromaddress)) {
            return false;
        }
        if (empty($mail_fromname)) {
            return false;
        }

        $send_type = $current_user->getPreference('mail_sendtype');
        if (isSmtp($send_type ?? '')) {
            $mail_smtpserver = $current_user->getPreference('mail_smtpserver');
            $mail_smtpport = $current_user->getPreference('mail_smtpport');
            $mail_smtpauth_req = $current_user->getPreference('mail_smtpauth_req');
            $mail_smtpuser = $current_user->getPreference('mail_smtpuser');
            $mail_smtppass = $current_user->getPreference('mail_smtppass');
            if (empty($mail_smtpserver) ||
                empty($mail_smtpport) ||
                (!empty($mail_smtpauth_req) && (empty($mail_smtpuser) || empty($mail_smtppass)))
            ) {
                return false;
            }
        }

        return true;
    }

    /**
     * outputs JS to set fields in the MassUpdate form in the "My Inbox" view
     * @global $mod_strings
     * @return string
     */
    public function js_set_archived()
    {
        global $mod_strings;
        $script = '
		<script type="text/javascript" language="JavaScript"><!-- Begin
			function setArchived() {
				var form = document.getElementById("MassUpdate");
				var status = document.getElementById("mass_status");
				var ok = false;

				for(var i=0; i < form.elements.length; i++) {
					if(form.elements[i].name == "mass[]") {
						if(form.elements[i].checked == true) {
							ok = true;
						}
					}
				}

				if(ok == true) {
					var user = document.getElementById("mass_assigned_user_name");
					var team = document.getElementById("team");

					user.value = "";
					for(var j=0; j<status.length; j++) {
						if(status.options[j].value == "archived") {
							status.options[j].selected = true;
							status.selectedIndex = j; // for IE
						}
					}

					form.submit();
				} else {
					alert("' . $mod_strings['ERR_ARCHIVE_EMAIL'] . '");
				}

			}
		//  End --></script>';

        return $script;
    }

    /**
     * replaces the javascript in utils.php - more specialized
     * @param string $type
     * @param string $group
     * @param string $assigned_user_id
     * @return string
     */
    public function u_get_clear_form_js($type = '', $group = '', $assigned_user_id = '')
    {
        $uType = '';
        $uGroup = '';
        $uAssigned_user_id = '';

        if (!empty($type)) {
            $uType = '&type=' . $type;
        }
        if (!empty($group)) {
            $uGroup = '&group=' . $group;
        }
        if (!empty($assigned_user_id)) {
            $uAssigned_user_id = '&assigned_user_id=' . $assigned_user_id;
        }

        $the_script = '
		<script type="text/javascript" language="JavaScript"><!-- Begin
			function clear_form(form) {
				var newLoc = "index.php?action=" + form.action.value + "&module=" + form.module.value + "&query=true&clear_query=true' . $uType . $uGroup . $uAssigned_user_id . '";
				if(typeof(form.advanced) != "undefined"){
					newLoc += "&advanced=" + form.advanced.value;
				}
				document.location.href= newLoc;
			}
		//  End --></script>';

        return $the_script;
    }

    /**
     * @global $theme
     * @global $mod_strings
     * @return string
     */
    public function pickOneButton()
    {
        global $theme;
        global $mod_strings;
        $out = '<div><input	title="' . $mod_strings['LBL_BUTTON_GRAB_TITLE'] . '"
						class="button"
						type="button" name="button"
						onClick="window.location=\'index.php?module=Emails&action=Grab\';"
						style="margin-bottom:2px"
						value="  ' . $mod_strings['LBL_BUTTON_GRAB'] . '  "></div>';

        return $out;
    }

    /**
     * Determines what Editor (HTML or Plain-text) the current_user uses;
     * @return string Editor type
     */
    public function getUserEditorPreference()
    {
        global $sugar_config;
        global $current_user;

        $editor = '';

        if (!isset($sugar_config['email_default_editor'])) {
            $sugar_config = $current_user->setDefaultsInConfig();
        }

        $userEditor = $current_user->getPreference('email_editor_option');
        $systemEditor = $sugar_config['email_default_editor'];

        if ($userEditor != '') {
            $editor = $userEditor;
        } else {
            $editor = $systemEditor;
        }

        return $editor;
    }

    /**
     * takes the mess we pass from EditView and tries to create some kind of order
     * @param array $addrs
     * @param array $addrs_ids (from contacts)
     * @param array $addrs_names (from contacts);
     * @param array $addrs_emails (from contacts);
     * @return array Parsed assoc array to feed to PHPMailer
     */
    public function parse_addrs($addrs, $addrs_ids, $addrs_names, $addrs_emails)
    {
        // cn: bug 9406 - enable commas to separate email addresses
        $addrs = str_replace(",", ";", $addrs);

        $ltgt = array('&lt;', '&gt;');
        $gtlt = array('<', '>');

        $return = array();
        $addrs = str_replace($ltgt, '', $addrs);
        $addrs_arr = explode(";", $addrs);
        $addrs_arr = $this->remove_empty_fields($addrs_arr);
        $addrs_ids_arr = explode(";", $addrs_ids);
        $addrs_ids_arr = $this->remove_empty_fields($addrs_ids_arr);
        $addrs_emails_arr = explode(";", $addrs_emails);
        $addrs_emails_arr = $this->remove_empty_fields($addrs_emails_arr);
        $addrs_names_arr = explode(";", $addrs_names);
        $addrs_names_arr = $this->remove_empty_fields($addrs_names_arr);

        ///////////////////////////////////////////////////////////////////////
        ////	HANDLE EMAILS HAND-WRITTEN
        $contactRecipients = array();
        $knownEmails = array();

        foreach ($addrs_arr as $i => $v) {
            if (trim($v) == "") {
                continue;
            } // skip any "blanks" - will always have 1

            $recipient = array();

            //// get the email to see if we're dealing with a dupe
            //// what crappy coding
            preg_match("/[A-Z0-9._%-\']+@[A-Z0-9.-]+\.[A-Z]{2,}/i", (string) $v, $match);


            if (!empty($match[0]) && !in_array(trim($match[0]), $knownEmails)) {
                $knownEmails[] = $match[0];
                $recipient['email'] = $match[0];

                //// handle the Display name
                $display = trim(str_replace($match[0], '', (string) $v));

                //// only trigger a "displayName" <email@address> when necessary
                if (isset($addrs_names_arr[$i])) {
                    $recipient['display'] = $addrs_names_arr[$i];
                } else {
                    if (!empty($display)) {
                        $recipient['display'] = $display;
                    }
                }
                if (isset($addrs_ids_arr[$i]) && $addrs_emails_arr[$i] == $match[0]) {
                    $recipient['contact_id'] = $addrs_ids_arr[$i];
                }
                $return[] = $recipient;
            }
        }

        return $return;
    }

    /**
     * @param $arr
     * @return array
     */
    public function remove_empty_fields(&$arr)
    {
        $newarr = array();

        foreach ($arr as $field) {
            $field = trim($field);
            if (empty($field)) {
                continue;
            }
            array_push($newarr, $field);
        }

        return $newarr;
    }

    /**
     * handles attachments of various kinds when sending email
     * @global $mod_strings
     */
    public function handleAttachments()
    {
        global $mod_strings;

        ///////////////////////////////////////////////////////////////////////////
        ////    ATTACHMENTS FROM DRAFTS
        if (($this->type == 'out' || $this->type == 'draft') && $this->status == 'draft' && isset($_REQUEST['record'])) {
            $this->getNotes($_REQUEST['record']); // cn: get notes from OLD email for use in new email
        }
        ////    END ATTACHMENTS FROM DRAFTS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////    ATTACHMENTS FROM FORWARDS
        // Bug 8034 Jenny - Need the check for type 'draft' here to handle cases where we want to save
        // forwarded messages as drafts.  We still need to save the original message's attachments.
        if (($this->type == 'out' || $this->type == 'draft') &&
            isset($_REQUEST['origType']) && $_REQUEST['origType'] == 'forward' &&
            isset($_REQUEST['return_id']) && !empty($_REQUEST['return_id'])
        ) {
            $this->getNotes($_REQUEST['return_id'], true);
        }

        // cn: bug 8034 - attachments from forward/replies lost when saving in draft
        if (isset($_REQUEST['prior_attachments']) && !empty($_REQUEST['prior_attachments']) && $this->new_with_id == true) {
            $exIds = explode(",", $_REQUEST['prior_attachments']);
            if (!isset($_REQUEST['template_attachment'])) {
                $_REQUEST['template_attachment'] = array();
            }
            $_REQUEST['template_attachment'] = array_merge($_REQUEST['template_attachment'], $exIds);
        }
        ////    END ATTACHMENTS FROM FORWARDS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	ATTACHMENTS FROM TEMPLATES
        // to preserve individual email integrity, we must dupe Notes and associated files
        // for each outbound email - good for integrity, bad for filespace
        if (isset($_REQUEST['template_attachment']) && !empty($_REQUEST['template_attachment'])) {
            $removeArr = array();
            $noteArray = array();

            if (isset($_REQUEST['temp_remove_attachment']) && !empty($_REQUEST['temp_remove_attachment'])) {
                $removeArr = $_REQUEST['temp_remove_attachment'];
            }


            foreach ($_REQUEST['template_attachment'] as $noteId) {
                if (in_array($noteId, $removeArr)) {
                    continue;
                }
                $noteTemplate = BeanFactory::newBean('Notes');
                $noteTemplate->retrieve($noteId);
                $noteTemplate->id = create_guid();
                $noteTemplate->new_with_id = true; // duplicating the note with files
                $noteTemplate->parent_id = $this->id;
                $noteTemplate->parent_type = $this->module_dir;
                $noteTemplate->date_entered = '';
                $noteTemplate->save();

                $noteFile = new UploadFile();
                $noteFile->duplicate_file($noteId, $noteTemplate->id, $noteTemplate->filename);
                $noteArray[] = $noteTemplate;
            }
            $this->attachments = array_merge($this->attachments, $noteArray);
        }
        ////	END ATTACHMENTS FROM TEMPLATES
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	ADDING NEW ATTACHMENTS
        $max_files_upload = 10;
        // Jenny - Bug 8211 Since attachments for drafts have already been processed,
        // we don't need to re-process them.
        if ($this->status != "draft") {
            $notes_list = array();
            if (!empty($this->id) && !$this->new_with_id) {
                $note = BeanFactory::newBean('Notes');
                $where = "notes.parent_id='{$this->id}'";
                $notes_list = $note->get_full_list("", $where, true);
            }
            $this->attachments = array_merge($this->attachments, $notes_list);
        }
        // cn: Bug 5995 - rudimentary error checking
        $filesError = array(
            0 => 'UPLOAD_ERR_OK - There is no error, the file uploaded with success.',
            1 => 'UPLOAD_ERR_INI_SIZE - The uploaded file exceeds the upload_max_filesize directive in php.ini.',
            2 => 'UPLOAD_ERR_FORM_SIZE - The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the HTML form.',
            3 => 'UPLOAD_ERR_PARTIAL - The uploaded file was only partially uploaded.',
            4 => 'UPLOAD_ERR_NO_FILE - No file was uploaded.',
            5 => 'UNKNOWN ERROR',
            6 => 'UPLOAD_ERR_NO_TMP_DIR - Missing a temporary folder. Introduced in PHP 4.3.10 and PHP 5.0.3.',
            7 => 'UPLOAD_ERR_CANT_WRITE - Failed to write file to disk. Introduced in PHP 5.1.0.',
        );

        for ($i = 0; $i < $max_files_upload; $i++) {
            // cn: Bug 5995 - rudimentary error checking
            if (!isset($_FILES["email_attachment{$i}"])) {
                $GLOBALS['log']->debug("Email Attachment {$i} does not exist.");
                continue;
            }
            if ($_FILES['email_attachment' . $i]['error'] != 0 && $_FILES['email_attachment' . $i]['error'] != 4) {
                $GLOBALS['log']->debug('Email Attachment could not be attach due to error: ' . $filesError[$_FILES['email_attachment' . $i]['error']]);
                continue;
            }

            $note = BeanFactory::newBean('Notes');
            $note->parent_id = $this->id;
            $note->parent_type = $this->module_dir;
            $upload_file = new UploadFile('email_attachment' . $i);

            if (empty($upload_file)) {
                continue;
            }

            if (isset($_FILES['email_attachment' . $i]) && $upload_file->confirm_upload()) {
                $note->filename = $upload_file->get_stored_file_name();
                $note->file = $upload_file;
                $note->name = $mod_strings['LBL_EMAIL_ATTACHMENT'] . ': ' . $note->file->original_file_name;

                $this->attachments[] = $note;
            }
        }

        $this->saved_attachments = array();
        foreach ($this->attachments as $note) {
            if (!empty($note->id)) {
                array_push($this->saved_attachments, $note);
                continue;
            }
            $note->parent_id = $this->id;
            $note->parent_type = 'Emails';
            $note->file_mime_type = $note->file->mime_type;
            $note_id = $note->save();

            $this->saved_attachments[] = $note;

            $note->id = $note_id;
            $note->file->final_move($note->id);
        }
        ////	END NEW ATTACHMENTS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	ATTACHMENTS FROM DOCUMENTS
        for ($i = 0; $i < 10; $i++) {
            if (isset($_REQUEST['documentId' . $i]) && !empty($_REQUEST['documentId' . $i])) {
                $doc = BeanFactory::newBean('Documents');
                $docRev = BeanFactory::newBean('DocumentRevisions');
                $docNote = BeanFactory::newBean('Notes');
                $noteFile = new UploadFile();

                $doc->retrieve($_REQUEST['documentId' . $i]);
                $docRev->retrieve($doc->document_revision_id);

                $this->saved_attachments[] = $docRev;

                // cn: bug 9723 - Emails with documents send GUID instead of Doc name
                $docNote->name = $docRev->getDocumentRevisionNameForDisplay();
                $docNote->filename = $docRev->filename;
                $docNote->description = $doc->description;
                $docNote->parent_id = $this->id;
                $docNote->parent_type = 'Emails';
                $docNote->file_mime_type = $docRev->file_mime_type;
                $docId = $docNote = $docNote->save();

                $noteFile->duplicate_file($docRev->id, $docId, $docRev->filename);
            }
        }

        ////	END ATTACHMENTS FROM DOCUMENTS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	REMOVE ATTACHMENTS
        if (isset($_REQUEST['remove_attachment']) && !empty($_REQUEST['remove_attachment'])) {
            foreach ($_REQUEST['remove_attachment'] as $noteId) {
                $q = 'UPDATE notes SET deleted = 1 WHERE id = \'' . $noteId . '\'';
                $this->db->query($q);
            }
        }

        //this will remove attachments that have been selected to be removed from drafts.
        if (isset($_REQUEST['removeAttachment']) && !empty($_REQUEST['removeAttachment'])) {
            $exRemoved = explode('::', $_REQUEST['removeAttachment']);
            foreach ($exRemoved as $noteId) {
                $q = 'UPDATE notes SET deleted = 1 WHERE id = \'' . $noteId . '\'';
                $this->db->query($q);
            }
        }
        ////	END REMOVE ATTACHMENTS
        ///////////////////////////////////////////////////////////////////////////
    }


    /**
     * Handles file attachments with multiple files
     * @global $mod_strings
     */
    public function handleMultipleFileAttachments()
    {
        global $mod_strings;

        ///////////////////////////////////////////////////////////////////////////
        ////    ATTACHMENTS FROM DRAFTS
        if (($this->type == 'out' || $this->type == 'draft')
            && $this->status == 'draft'
            && isset($_REQUEST['record'])) {
            $this->getNotes($_REQUEST['record']); // cn: get notes from OLD email for use in new email
        }
        ////    END ATTACHMENTS FROM DRAFTS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////    ATTACHMENTS FROM FORWARDS
        // Bug 8034 Jenny - Need the check for type 'draft' here to handle cases where we want to save
        // forwarded messages as drafts.  We still need to save the original message's attachments.
        if (($this->type == 'out' || $this->type == 'draft') &&
            isset($_REQUEST['origType']) && $_REQUEST['origType'] == 'forward' &&
            isset($_REQUEST['return_id']) && !empty($_REQUEST['return_id'])
        ) {
            $this->getNotes($_REQUEST['return_id'], true);
        }

        // cn: bug 8034 - attachments from forward/replies lost when saving in draft
        if (isset($_REQUEST['prior_attachments']) && !empty($_REQUEST['prior_attachments']) && $this->new_with_id == true) {
            $exIds = explode(",", $_REQUEST['prior_attachments']);
            if (!isset($_REQUEST['template_attachment'])) {
                $_REQUEST['template_attachment'] = array();
            }
            $_REQUEST['template_attachment'] = array_merge($_REQUEST['template_attachment'], $exIds);
        }
        ////    END ATTACHMENTS FROM FORWARDS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	ATTACHMENTS FROM TEMPLATES
        // to preserve individual email integrity, we must dupe Notes and associated files
        // for each outbound email - good for integrity, bad for filespace
        if (isset($_REQUEST['template_attachment']) && !empty($_REQUEST['template_attachment'])) {
            $removeArr = array();
            $noteArray = array();

            if (isset($_REQUEST['temp_remove_attachment']) && !empty($_REQUEST['temp_remove_attachment'])) {
                $removeArr = $_REQUEST['temp_remove_attachment'];
            }

            foreach ($_REQUEST['template_attachment'] as $noteId) {
                if (in_array($noteId, $removeArr)) {
                    continue;
                }
                $noteTemplate = BeanFactory::newBean('Notes');
                $noteTemplate->retrieve($noteId);
                $noteTemplate->id = create_guid();
                $noteTemplate->new_with_id = true; // duplicating the note with files
                $noteTemplate->parent_id = $this->id;
                $noteTemplate->parent_type = $this->module_dir;
                $noteTemplate->date_entered = '';
                $noteTemplate->save();

                $noteFile = new UploadFile();
                $noteFile->duplicate_file($noteId, $noteTemplate->id, $noteTemplate->filename);
                $noteArray[] = $noteTemplate;
            }
            $this->attachments = array_merge($this->attachments, $noteArray);
        }
        ////	END ATTACHMENTS FROM TEMPLATES
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	ADDING NEW ATTACHMENTS
        $max_files_upload = 10;
        // Jenny - Bug 8211 Since attachments for drafts have already been processed,
        // we don't need to re-process them.
        if ($this->status != "draft") {
            $notes_list = array();
            if (!empty($this->id) && !$this->new_with_id) {
                $note = BeanFactory::newBean('Notes');
                $where = "notes.parent_id='{$this->id}'";
                $notes_list = (array)$note->get_full_list("", $where, true);
            }
            $this->attachments = array_merge($this->attachments, $notes_list);
        }
        // cn: Bug 5995 - rudimentary error checking
        $filesError = array(
            0 => 'UPLOAD_ERR_OK - There is no error, the file uploaded with success.',
            1 => 'UPLOAD_ERR_INI_SIZE - The uploaded file exceeds the upload_max_filesize directive in php.ini.',
            2 => 'UPLOAD_ERR_FORM_SIZE - The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the HTML form.',
            3 => 'UPLOAD_ERR_PARTIAL - The uploaded file was only partially uploaded.',
            4 => 'UPLOAD_ERR_NO_FILE - No file was uploaded.',
            5 => 'UNKNOWN ERROR',
            6 => 'UPLOAD_ERR_NO_TMP_DIR - Missing a temporary folder. Introduced in PHP 4.3.10 and PHP 5.0.3.',
            7 => 'UPLOAD_ERR_CANT_WRITE - Failed to write file to disk. Introduced in PHP 5.1.0.',
        );

        for ($i = 0; $i < $max_files_upload; $i++) {
            // cn: Bug 5995 - rudimentary error checking
            if (!isset($_FILES["email_attachment"]['name'][$i])) {
                $GLOBALS['log']->debug("Email Attachment {$i} does not exist.");
                continue;
            }
            if ($_FILES['email_attachment']['error'][$i] != 0 && $_FILES['email_attachment']['error'][$i] != 4) {
                $GLOBALS['log']->debug('Email Attachment could not be attach due to error: ' . $filesError[$_FILES['email_attachment']['error'][$i]]);
                continue;
            }

            $note = BeanFactory::newBean('Notes');
            $note->parent_id = $this->id;
            $note->parent_type = $this->module_dir;
            $upload_file = new UploadMultipleFiles('email_attachment', $i);

            if (empty($upload_file)) {
                continue;
            }

            if (isset($_FILES['email_attachment']['name'][$i]) && $upload_file->confirm_upload()) {
                $note->filename = $upload_file->get_stored_file_name();
                $note->file = $upload_file;
                $note->name = $mod_strings['LBL_EMAIL_ATTACHMENT'] . ': ' . $note->file->original_file_name;

                $this->attachments[] = $note;
            }
        }

        $this->saved_attachments = array();
        foreach ((array)$this->attachments as $note) {
            if (!empty($note->id)) {
                array_push($this->saved_attachments, $note);
                continue;
            }
            $note->parent_id = $this->id;
            $note->parent_type = 'Emails';
            $note->file_mime_type = $note->file->mime_type;
            $note_id = $note->save();

            $this->saved_attachments[] = $note;

            $note->id = $note_id;
            $note->file->final_move($note->id);
        }
        ////	END NEW ATTACHMENTS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	ATTACHMENTS FROM DOCUMENTS
        for ($i = 0; $i < $max_files_upload; $i++) {
            if (isset($_REQUEST['documentId' . $i]) && !empty($_REQUEST['documentId' . $i])) {
                $doc = BeanFactory::newBean('Documents');
                $docRev = BeanFactory::newBean('DocumentRevisions');
                $docNote = BeanFactory::newBean('Notes');
                $noteFile = new UploadFile();

                $doc->retrieve($_REQUEST['documentId' . $i]);
                $docRev->retrieve($doc->document_revision_id);

                $this->saved_attachments[] = $docRev;

                // cn: bug 9723 - Emails with documents send GUID instead of Doc name
                $docNote->name = $docRev->getDocumentRevisionNameForDisplay();
                $docNote->filename = $docRev->filename;
                $docNote->description = $doc->description;
                $docNote->parent_id = $this->id;
                $docNote->parent_type = 'Emails';
                $docNote->file_mime_type = $docRev->file_mime_type;
                $docId = $docNote = $docNote->save();

                $noteFile->duplicate_file($docRev->id, $docId, $docRev->filename);
            }
        }

        ////	END ATTACHMENTS FROM DOCUMENTS
        ///////////////////////////////////////////////////////////////////////////

        ///////////////////////////////////////////////////////////////////////////
        ////	REMOVE ATTACHMENTS
        if (isset($_REQUEST['remove_attachment']) && !empty($_REQUEST['remove_attachment'])) {
            foreach ($_REQUEST['remove_attachment'] as $noteId) {
                $q = 'UPDATE notes SET deleted = 1 WHERE id = \'' . $noteId . '\'';
                $this->db->query($q);
            }
        }

        //this will remove attachments that have been selected to be removed from drafts.
        if (isset($_REQUEST['removeAttachment']) && !empty($_REQUEST['removeAttachment'])) {
            $exRemoved = explode('::', $_REQUEST['removeAttachment']);
            foreach ($exRemoved as $noteId) {
                $q = 'UPDATE notes SET deleted = 1 WHERE id = \'' . $noteId . '\'';
                $this->db->query($q);
            }
        }
        ////	END REMOVE ATTACHMENTS
        ///////////////////////////////////////////////////////////////////////////
    }

    /**
     * Determines if an email body (HTML or Plain) has a User signature already in the content
     * @param array Array of signatures
     * @return bool
     */
    public function hasSignatureInBody($sig)
    {
        // strpos can't handle line breaks - normalize
        $html = $this->removeAllNewlines($this->description_html);
        $htmlSig = $this->removeAllNewlines($sig['signature_html']);
        $plain = $this->removeAllNewlines($this->description);
        $plainSig = $this->removeAllNewlines($sig['signature']);

        // cn: bug 11621 - empty sig triggers notice error
        if (!empty($htmlSig) && false !== strpos($html, $htmlSig)) {
            return true;
        } elseif (!empty($plainSig) && false !== strpos($plain, $plainSig)) {
            return true;
        }
        return false;
    }

    /**
     * internal helper
     * @param string $str to be normalized
     * @return string
     */
    public function removeAllNewlines($str)
    {
        $bad = array("\r\n", "\n\r", "\n", "\r");
        $good = array('', '', '', '');

        return str_replace($bad, $good, strip_tags(br2nl(from_html($str))));
    }


    /**
     * Set navigation anchors to aid DetailView record navigation (VCR buttons)
     * @param string $uri The URI from the referring page (always ListView)
     * @return array start Array of the URI broken down with a special "current_view" for My Inbox Navs
     */
    public function getStartPage($uri)
    {
        if (strpos(
            $uri,
            '&'
        )) { // "&" to ensure that we can explode the GET vars - else we're gonna trigger a Notice error
            $serial = substr($uri, (strpos($uri, '?') + 1), strlen($uri));
            $exUri = explode('&', $serial);
            $start = array('module' => '', 'action' => '', 'group' => '', 'record' => '', 'type' => '');

            foreach ($exUri as $k => $pair) {
                $exPair = explode('=', $pair);
                $start[$exPair[0]] = $exPair[1];
            }

            // specific views for current_user
            if (isset($start['assigned_user_id'])) {
                $start['current_view'] = "{$start['action']}&module={$start['module']}&assigned_user_id={$start['assigned_user_id']}&type={$start['type']}";
            }

            return $start;
        }
        return array();
    }

    /**
     * preps SMTP info for email transmission
     * @param object $mail SugarPHPMailer object
     * @param string $mailer_id
     * @param string $ieId
     * @global $current_user
     * @return object mail SugarPHPMailer object
     */
    public function setMailer($mail, $mailer_id = '', $ieId = '')
    {
        global $current_user;

        require_once("include/OutboundEmail/OutboundEmail.php");
        $oe = new OutboundEmail();
        $oe = $oe->getInboundMailerSettings($current_user, $mailer_id, $ieId);

        // ssl or tcp - keeping outside isSMTP b/c a default may inadvertantly set ssl://
        $mail->protocol = ($oe->mail_smtpssl) ? "ssl://" : "tcp://";
        if (isSmtp($oe->mail_sendtype ?? '')) {
            //Set mail send type information
            $mail->Mailer = "smtp";
            $mail->Host = $oe->mail_smtpserver;
            $mail->Port = $oe->mail_smtpport;

            $mail->setSecureProtocol($oe->mail_smtpssl);
            $mail->initSMTPAuth(
                $oe->auth_type ?? '',
                $oe->external_oauth_connection_id ?? '',
                $oe->mail_smtpuser ?? '',
                $oe->mail_smtppass ?? '',
            );
        } else {
            $mail->Mailer = "sendmail";
        }

        $mail->oe = $oe;

        return $mail;
    }

    /**
     * preps SMTP info for email transmission
     * @param SugarPHPMailer $mail SugarPHPMailer object
     */
    public function setOutboundBasedMailer(SugarPHPMailer $mail, OutboundEmailAccounts $outboundEmail): void
    {
        $type = $outboundEmail->type;

        if ($type === 'system-override') {
            $mail->setMailerForSystem();
        } else {
            $this->setupMailerFromOutbound($outboundEmail, $mail);

        }

        $mail->oe = $outboundEmail;
    }

    /**
     * @param OutboundEmailAccounts $oe
     * @param SugarPHPMailer $mail
     * @return void
     */
    protected function setupMailerFromOutbound(OutboundEmailAccounts $oe, SugarPHPMailer $mail): void
    {
        // ssl or tcp - keeping outside isSMTP b/c a default may inadvertantly set ssl://
        $mail->protocol = ($oe->mail_smtpssl) ? "ssl://" : "tcp://";
        if (isSmtp($oe->mail_sendtype ?? '')) {
            //Set mail send type information
            $mail->Mailer = "smtp";
            $mail->Host = $oe->mail_smtpserver;
            $mail->Port = $oe->mail_smtpport;

            $mail->setSecureProtocol($oe->mail_smtpssl);
            $mail->initSMTPAuth(
                $oe->auth_type ?? '',
                $oe->external_oauth_connection_id ?? '',
                $oe->mail_smtpuser ?? '',
                $oe->mail_smtppass ?? '',
            );
        } else {
            $mail->Mailer = "sendmail";
        }
    }

    /**
     * Preps SugarPHPMailer object for HTML or Plain text sends
     * @param SugarPHPMailer $mail
     * @global $current_user
     * @return Email
     */
    public function handleBody($mail)
    {
        global $current_user;

        // User preferences should take precedence over everything else
        $emailSettings = $current_user->getPreference('emailSettings', 'Emails');
        // Protect against accessing emailSettings as an array if it's null.
        $sendPlainText = is_null($emailSettings) ? null : $emailSettings['sendPlainText'];
        $alwaysSendEmailsInPlainText = $sendPlainText === '1';

        $sendEmailsInPlainText = false;
        if (isset($_REQUEST['is_only_plain_text']) && $_REQUEST['is_only_plain_text'] === 'true') {
            $sendEmailsInPlainText = true;
        }

        if ($alwaysSendEmailsInPlainText === true) {
            // plain text only
            $this->handleBodyInPlainTextFormat($mail);
        } elseif ($alwaysSendEmailsInPlainText === false && $sendEmailsInPlainText === true) {
            $this->handleBodyInPlainTextFormat($mail);
        } else {
            $this->handleBodyInHTMLformat($mail);
        }

        // wp: if plain text version has lines greater than 998, use base64 encoding
        foreach (explode("\n", ($mail->ContentType == "text/html") ? $mail->AltBody : $mail->Body) as $line) {
            if (strlen($line) > 998) {
                $mail->Encoding = 'base64';
                break;
            }
        }

        return $mail;
    }

    /**
     * Retrieve function from handlebody() to unit test easily
     * @sugar_config
     * @param SugarPHPMailer $mail
     */
    public function handleBodyInHTMLformat($mail)
    {
        global $sugar_config;
        // wp: if body is html, then insert new lines at 996 characters. no effect on client side
        // due to RFC 2822 which limits email lines to 998
        $mail->IsHTML(true);
        $body = from_html(wordwrap($this->description_html, 996));
        $mail->Body = $body;

        // cn: bug 9725
        // new plan is to use the selected type (html or plain) to fill the other
        $plainText = from_html($this->description_html);
        $plainText = strip_tags(br2nl($plainText));
        $mail->AltBody = $plainText;
        $this->description = $plainText;

        $mail->replaceImageByRegex("(?:{$sugar_config['site_url']})?/?cache/images/", sugar_cached("images/"));

        //Replace any embeded images using the secure entryPoint for src url.
        $mail->replaceImageByRegex(
            "(?:{$sugar_config['site_url']})?/?index.php[?]entryPoint=download&(?:amp;)?[^\"]+?id=",
            "upload://",
            true
        );

        $mail->Body = from_html($mail->Body);
    }

    /**
     * Sends Email
     * @global array $mod_strings
     * @global array $app_strings
     * @global User $current_user
     * @global array $sugar_config
     * @global Localization $locale
     * @param SugarPHPMailer|null $mail
     * @param \NonGmailSentFolderHandler|null $nonGmailSentFolder
     * @return boolean True on success
     */
    public function send(
        SugarPHPMailer $mail = null,
        NonGmailSentFolderHandler $nonGmailSentFolder = null,
        InboundEmail $ie = null,
        Email $tempEmail = null,
        $check_notify = false,
        $options = "\\Seen"
    ) {
        global $mod_strings, $app_strings;
        global $current_user;
        global $sugar_config;
        global $locale;

        $this->clearTempEmailAtSend();

        $OBCharset = $locale->getPrecedentPreference('default_email_charset');
        $mail = $mail ? $mail : new SugarPHPMailer();

        if (!$this->to_addrs_arr) {
            LoggerManager::getLogger()->error('"To" address(es) is not set or empty to sending email.');
            return false; // return false as error, to-address is required to sending an email
        }

        $this->setRecipientAddresses($mail, $locale, $OBCharset);

        $ieId = $this->mailbox_id;
        $mail = $this->setMailer($mail, '', $ieId);

        if ($this->isToUseSystemEmail($mail, $sugar_config)) {
            [$sender, $ReplyToAddr, $ReplyToName] = $this->setSystemBasedSenderAddresses($mail);
        } else {

            // FROM ADDRESS
            if (!empty($this->from_addr)) {
                $mail->From = $this->from_addr;
                isValidEmailAddress($mail->From);
            } else {
                $mail->From = $current_user->getPreference('mail_fromaddress');
                isValidEmailAddress($mail->From);
                $this->from_addr = $mail->From;
                isValidEmailAddress($this->from_addr);
            }
            // FROM NAME
            if (!empty($this->from_name)) {
                $mail->FromName = $this->from_name;
            } elseif (!empty($this->from_addr_name)) {
                $mail->FromName = $this->from_addr_name;
            } else {
                $mail->FromName = $current_user->getPreference('mail_fromname');
                $this->from_name = $mail->FromName;
            }

            //Reply to information for case create and autoreply.
            if (!empty($this->reply_to_name)) {
                $ReplyToName = $this->reply_to_name;
            } else {
                $ReplyToName = $mail->FromName;
            }

            $sender = $mail->From;
            isValidEmailAddress($sender);
            if (!empty($this->reply_to_addr)) {
                $ReplyToAddr = $this->reply_to_addr;
            } else {
                $ReplyToAddr = $mail->From;
            }
            isValidEmailAddress($ReplyToAddr);
        }


        $mail->Sender = $sender; /* set Return-Path field in header to reduce spam score in emails sent via Sugar's Email module */
        isValidEmailAddress($mail->Sender);
        $mail->AddReplyTo($ReplyToAddr, $locale->translateCharsetMIME(trim($ReplyToName), 'UTF-8', $OBCharset));

        $mail->Subject = html_entity_decode((string) $this->name, ENT_QUOTES, 'UTF-8');

        $attachmentLabel = $mod_strings['LBL_EMAIL_ATTACHMENT'] ?? '';

        $this->setupAttachments(
            $attachmentLabel,
            $sugar_config['upload_badext'],
            $mail,
            $locale,
            $OBCharset
        );

        $mail = $this->handleBody($mail);

        $GLOBALS['log']->debug('Email sending --------------------- ');

        $mail->prepForOutbound();

        $this->validateBeforeSend($mail);
        if ($mail->send()) {
            ///////////////////////////////////////////////////////////////////
            ////	INBOUND EMAIL HANDLING
            // mark replied

            if (!empty($_REQUEST['inbound_email_id'])) {
                $ieId = $_REQUEST['inbound_email_id'];
                $this->createTempEmailAtSend($tempEmail);
                $this->getTempEmailAtSend()->status = 'replied';
                $ie = $ie ? $ie : BeanFactory::newBean('InboundEmail');
                $nonGmailSentFolder = $nonGmailSentFolder ? $nonGmailSentFolder : new NonGmailSentFolderHandler();
                if (!$ieMailId = $this->getTempEmailAtSend()->saveAndStoreInSentFolderIfNoGmail($ie, $ieId, $mail, $nonGmailSentFolder, $check_notify, $options)) {
                    LoggerManager::getLogger()->debug('IE Mail ID is ' . ($ieMailId === null ? 'null' : $ieMailId) . ' after save and store in non-gmail sent folder.');
                }
                if (!$this->getTempEmailAtSend()->save()) {
                    LoggerManager::getLogger()->warn('Email saving error: after save and store in non-gmail sent folder.');
                }
            }

            ////	END INBOUND EMAIL HANDLING
            ///////////////////////////////////////////////////////////////////
            $GLOBALS['log']->debug(' --------------------- buh bye -- sent successful');
            return true;
        }
        $GLOBALS['log']->debug($app_strings['LBL_EMAIL_ERROR_PREPEND'] . $mail->ErrorInfo);

        return false;
    }

    /**
     * Send using outbound email configuration only
     * @param OutboundEmailAccounts $outboundEmailAccount
     * @param SugarPHPMailer|null $mail
     * @return bool
     * @throws EmailValidatorException
     * @throws \PHPMailer\PHPMailer\Exception
     * @throws \SuiteCRM\ErrorMessageException
     */
    public function sendFromOutbound(
        OutboundEmailAccounts $outboundEmailAccount,
        SugarPHPMailer $mail = null
    ) {
        global $mod_strings, $app_strings, $sugar_config, $locale;

        $this->clearTempEmailAtSend();

        $OBCharset = $locale->getPrecedentPreference('default_email_charset');
        $mail = $mail ? $mail : new SugarPHPMailer();

        if (!$this->to_addrs_arr) {
            LoggerManager::getLogger()->error('"To" address(es) is not set or empty to sending email.');
            return false; // return false as error, to-address is required to sending an email
        }

        $this->setRecipientAddresses($mail, $locale, $OBCharset);

        $ieId = $this->mailbox_id;
        $this->setOutboundBasedMailer($mail, $outboundEmailAccount);

        if ($this->isToUseSystemEmail($mail, $sugar_config)) {
            [$sender, $ReplyToAddr, $ReplyToName] = $this->setSystemBasedSenderAddresses($mail);
        } else {
            [$sender, $ReplyToAddr, $ReplyToName] = $this->setOutboundBasedSenderAddresses($outboundEmailAccount, $mail);
        }

        if (empty($mail->From) || !isValidEmailAddress($mail->From, 'Invalid email address given', false)) {
            LoggerManager::getLogger()->fatal('No from address defined');
            return false; // return false as error, to-address is required to sending an email
        }

        $mail->Sender = $sender; /* set Return-Path field in header to reduce spam score in emails sent via Sugar's Email module */
        $mail->AddReplyTo($ReplyToAddr, $locale->translateCharsetMIME(trim($ReplyToName), 'UTF-8', $OBCharset));

        $mail->Subject = html_entity_decode((string) $this->name, ENT_QUOTES, 'UTF-8');

        $this->setupAttachments(
            $mod_strings['LBL_EMAIL_ATTACHMENT'],
            $sugar_config['upload_badext'],
            $mail,
            $locale,
            $OBCharset
        );

        $mail = $this->handleBody($mail);

        $GLOBALS['log']->debug('Email sending --------------------- ');

        $mail->prepForOutbound();

        $this->validateBeforeSend($mail);

        if ($mail->send()) {
            return true;
        }

        $GLOBALS['log']->debug($app_strings['LBL_EMAIL_ERROR_PREPEND'] . $mail->ErrorInfo);

        return false;
    }

    /**
     *
     * @param InboundEmail $ie
     * @param string $ieId
     * @param SugarPHPMailer $mail
     * @param NonGmailSentFolderHandler $nonGmailSentFolder
     * @return int|null null if error
     */
    public function saveAndStoreInSentFolderIfNoGmail(
        InboundEmail $ie,
        $ieId,
        SugarPHPMailer $mail,
        NonGmailSentFolderHandler $nonGmailSentFolder,
        $check_notify = false,
        $options = "\\Seen"
    ) {
        $ieMailId = null;
        if (!$ie) {
            $ie = BeanFactory::newBean('InboundEmail');
        }
        if (!$ie->id) {
            if (!$ie->retrieve($ieId)) {
                LoggerManager::getLogger()->warn('Error retrieve InboundEmail, requested ID was: ' . $ieId);
                $this->setLastSaveAndStoreInSentError(self::ERR_IE_RETRIEVE);
            }
        }
        if ($ie && $ie->id) {
            $ieMailId = $this->saveAndStoreInSent($mail, $ie, $nonGmailSentFolder, $check_notify, $options);
            if (!$ieMailId) {
                LoggerManager::getLogger()->warn('Email save and store in sent folder error. Inbound email ID was: ' . $ieId);
            }
        }
        return $ieMailId;
    }

    /**
     *
     * @param SugarPHPMailer $mail
     * @param InboundEmail $ie
     * @param \NonGmailSentFolderHandler|null $nonGmailSentFolder
     * @param bool $check_notify
     * @return string
     */
    protected function saveAndStoreInSent(
        SugarPHPMailer $mail,
        InboundEmail $ie,
        NonGmailSentFolderHandler $nonGmailSentFolder = null,
        $check_notify = false,
        $options = "\\Seen"
    ) {
        $ieMailId = $this->save($check_notify);
        if ($ieMailId) {
            // mark SEEN (STORE MAIL IN SENT BOX)
            $this->setNonGmailSentFolderHandler($nonGmailSentFolder ? $nonGmailSentFolder : new NonGmailSentFolderHandler());
            if (!($ie && $ie->id)) {
                LoggerManager::getLogger()->warn('Exists and retrieved InboundEmail needed for storing email as sent.');
                $this->setLastSaveAndStoreInSentError(self::ERR_NO_IE);
            } else {
                $stored = $this->getNonGmailSentFolderHandler()->storeInSentFolder($ie, $mail, $options);
                if (!$stored) {
                    LoggerManager::getLogger()->warn('Email storing in non gmail sent folder was not necessary. Inbound email ID was: ' . $ie->id);
                    $this->setLastSaveAndStoreInSentError(self::ERR_NOT_STORED_AS_SENT);
                } else {
                    LoggerManager::getLogger()->debug('Email storing in non gmail sent folder success. Inbound email ID was: ' . $ie->id);
                    $this->setLastSaveAndStoreInSentError(self::NO_ERROR);
                }
            }
        } else {
            $this->setLastSaveAndStoreInSentError(self::ERR_NO_IE_MAIL_ID);
        }

        return $ieMailId;
    }

    /**
     * @return string[]
     */
    public function listviewACLHelper()
    {
        $array_assign = parent::listviewACLHelper();
        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->parent_name)) {
            if (!empty($this->parent_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->parent_name_owner;
            }
            /* BEGIN - SECURITY GROUPS */
            //parent_name_owner not being set for whatever reason so we need to figure this out
            else {
                if (!empty($this->parent_type) && !empty($this->parent_id)) {
                    global $current_user;
                    $parent_bean = BeanFactory::getBean($this->parent_type, $this->parent_id);
                    if ($parent_bean !== false) {
                        $is_owner = $current_user->id == $parent_bean->assigned_user_id;
                    }
                }
            }
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess($this->parent_type, $this->parent_id, 'view');
            /* END - SECURITY GROUPS */
        }
        /* BEGIN - SECURITY GROUPS */
        /**
         * if(!ACLController::moduleSupportsACL($this->parent_type) || ACLController::checkAccess($this->parent_type, 'view', $is_owner)){
         */
        if (!ACLController::moduleSupportsACL($this->parent_type) || ACLController::checkAccess(
            $this->parent_type,
            'view',
            $is_owner,
            'module',
            $in_group
        )
        ) {
            /* END - SECURITY GROUPS */
            $array_assign['PARENT'] = 'a';
        } else {
            $array_assign['PARENT'] = 'span';
        }
        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->contact_name)) {
            if (!empty($this->contact_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->contact_name_owner;
            }
            /* BEGIN - SECURITY GROUPS */
            //contact_name_owner not being set for whatever reason so we need to figure this out
            else {
                global $current_user;
                $parent_bean = BeanFactory::getBean('Contacts', $this->contact_id);
                if ($parent_bean !== false) {
                    $is_owner = $current_user->id == $parent_bean->assigned_user_id;
                }
            }
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess('Contacts', $this->contact_id, 'view');
            /* END - SECURITY GROUPS */
        }
        /* BEGIN - SECURITY GROUPS */
        /**
         * if(ACLController::checkAccess('Contacts', 'view', $is_owner)) {
         */
        if (ACLController::checkAccess('Contacts', 'view', $is_owner, 'module', $in_group)) {
            /* END - SECURITY GROUPS */
            $array_assign['CONTACT'] = 'a';
        } else {
            $array_assign['CONTACT'] = 'span';
        }

        return $array_assign;
    }

    /**
     * @return array
     */
    public function getSystemDefaultEmail()
    {

        $oe = new OutboundEmail();
        $oe = $oe->getSystemMailerSettings();

        $email['email'] = $oe->smtp_from_addr ?? '';
        $email['name'] = $oe->smtp_from_name ?? '';

        return $email;
    }


    /**
     * @param string $order_by
     * @param string $where
     * @param array $filter
     * @param array $params
     * @param int $show_deleted
     * @param string $join_type
     * @param bool $return_array
     * @param null $parentbean
     * @param bool $singleSelect
     * @param bool $ifListForExport
     * @return string
     */
    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ) {
        $where_auto = '';

        if ($return_array) {
            return parent::create_new_list_query(
                $order_by,
                $where,
                $filter,
                $params,
                $show_deleted,
                $join_type,
                $return_array,
                $parentbean,
                $singleSelect
            );
        }
        $custom_join = $this->getCustomJoin();

        $query = "SELECT " . $this->table_name . ".*, users.user_name as assigned_user_name\n";

        $query .= $custom_join['select'];
        $query .= " FROM emails\n";
        if ($where != "" && (strpos($where, "contacts.first_name") > 0)) {
            $query .= " LEFT JOIN emails_beans ON emails.id = emails_beans.email_id\n";
        }

        $query .= " LEFT JOIN users ON emails.assigned_user_id=users.id \n";
        if ($where != "" && (strpos($where, "contacts.first_name") > 0)) {
            $query .= " JOIN contacts ON contacts.id= emails_beans.bean_id AND emails_beans.bean_module='Contacts' and contacts.deleted=0 \n";
        }

        $query .= $custom_join['join'];

        if ($show_deleted == 0) {
            $where_auto = " emails.deleted=0 \n";
        } else {
            if ($show_deleted == 1) {
                $where_auto = " emails.deleted=1 \n";
            }
        }

        if ($where != "") {
            $query .= "WHERE $where AND " . $where_auto;
        } else {
            $query .= "WHERE " . $where_auto;
        }

        if ($order_by != "") {
            $query .= " ORDER BY $order_by";
        } else {
            $query .= " ORDER BY date_sent_received DESC";
        }

        return $query;
    } // fn

    /**
     * @global $timedate
     * @global $mod_strings
     * fill_in_additional_list_fields
     */
    public function fill_in_additional_list_fields()
    {
        $this->fill_in_additional_detail_fields();

        $this->link_action = 'DetailView';
        $this->attachment_image = '';

        if (!empty($this->id)) {
            //populate attachment_image, used to display attachment icon.
            $query = "select 1 from notes where notes.parent_id = '$this->id' and notes.deleted = 0";
            $result = $this->db->query($query, true, " Error filling in additional list fields: ");

            $row = $this->db->fetchByAssoc($result);

            if ($row) {
                $this->attachment_image = SugarThemeRegistry::current()->getImage(
                    'attachment',
                    '',
                    null,
                    null,
                    '.gif',
                    translate('LBL_ATTACHMENT', 'Emails')
                );
            }
        }

        if (empty($this->contact_id) && !empty($this->parent_id) && !empty($this->parent_type) && $this->parent_type === 'Contacts' && !empty($this->parent_name)) {
            $this->contact_id = $this->parent_id;
            $this->contact_name = $this->parent_name;
        }
    }

    /**
     * @global $app_list_strings
     * @global $mod_strings
     * fill_in_additional_detail_fields
     */
    public function fill_in_additional_detail_fields()
    {
        global $app_list_strings;

        $mod_strings = return_module_language($GLOBALS['current_language'], 'Emails'); //Called from EmailMan as well.

        // Fill in the assigned_user_name
        $this->assigned_user_name = get_assigned_user_name($this->assigned_user_id, '');
        //if ($this->parent_type == 'Contacts') {
        $query = "SELECT contacts.first_name, contacts.last_name, contacts.phone_work, contacts.id, contacts.assigned_user_id contact_name_owner, 'Contacts' contact_name_mod FROM contacts, emails_beans ";
        $query .= "WHERE emails_beans.email_id='$this->id' AND emails_beans.bean_id=contacts.id AND emails_beans.bean_module = 'Contacts' AND emails_beans.deleted=0 AND contacts.deleted=0";
        if (!empty($this->parent_id) && $this->parent_type == 'Contacts') {
            $query .= " AND contacts.id= '" . $this->parent_id . "' ";
        } else {
            if (!empty($_REQUEST['record']) && !empty($_REQUEST['module']) && $_REQUEST['module'] == 'Contacts') {
                $query .= " AND contacts.id= '" . $_REQUEST['record'] . "' ";
            }
        }
        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");

        // Get the id and the name.
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            $contact = BeanFactory::newBean('Contacts');
            $contact->retrieve($row['id']);
            $this->contact_name = $contact->full_name;
            $this->contact_phone = $row['phone_work'];
            $this->contact_id = $row['id'];
            $this->contact_email = $contact->emailAddress->getPrimaryAddress($contact);
            $this->contact_name_owner = $row['contact_name_owner'];
            $this->contact_name_mod = $row['contact_name_mod'];
            $GLOBALS['log']->debug("Call($this->id): contact_name = $this->contact_name");
            $GLOBALS['log']->debug("Call($this->id): contact_phone = $this->contact_phone");
            $GLOBALS['log']->debug("Call($this->id): contact_id = $this->contact_id");
            $GLOBALS['log']->debug("Call($this->id): contact_email1 = $this->contact_email");
        } else {
            $this->contact_name = '';
            $this->contact_phone = '';
            $this->contact_id = '';
            $this->contact_email = '';
            $this->contact_name_owner = '';
            $this->contact_name_mod = '';
            $GLOBALS['log']->debug("Call($this->id): contact_name = $this->contact_name");
            $GLOBALS['log']->debug("Call($this->id): contact_phone = $this->contact_phone");
            $GLOBALS['log']->debug("Call($this->id): contact_id = $this->contact_id");
            $GLOBALS['log']->debug("Call($this->id): contact_email1 = $this->contact_email");
        }
        //}
        $this->created_by_name = get_assigned_user_name($this->created_by);
        $this->modified_by_name = get_assigned_user_name($this->modified_user_id);

        $this->link_action = 'DetailView';

        if (!empty($this->type)) {
            if ($this->type == 'out' && $this->status == 'send_error') {
                $this->type_name = $mod_strings['LBL_NOT_SENT'];
            } else {
                $this->type_name = $app_list_strings['dom_email_types'][$this->type];
            }

            if (($this->type == 'out' && $this->status == 'send_error') || $this->type == 'draft') {
                $this->link_action = 'EditView';
            }
        }

        //todo this  isset( $app_list_strings['dom_email_status'][$this->status]) is hack for 3261.
        if (!empty($this->status) && isset($app_list_strings['dom_email_status'][$this->status])) {
            $this->status_name = $app_list_strings['dom_email_status'][$this->status];
        }

        if (empty($this->name) && empty($_REQUEST['record']) && !empty($mod_strings['LBL_NO_SUBJECT'])) {
            $this->name = $mod_strings['LBL_NO_SUBJECT'];
        }

        $this->fill_in_additional_parent_fields();
    }

    /**
     * @param string $order_by
     * @param string $where
     * @return string
     */
    public function create_export_query($order_by, $where)
    {
        $contact_required = stristr($where, "contacts");
        $custom_join = $this->getCustomJoin(true, true, $where);

        if ($contact_required) {
            $query = "SELECT emails.*, contacts.first_name, contacts.last_name";
            $query .= $custom_join['select'];

            $query .= " FROM contacts, emails, emails_contacts ";
            $where_auto = "emails_contacts.contact_id = contacts.id AND emails_contacts.email_id = emails.id AND emails.deleted=0 AND contacts.deleted=0";
        } else {
            $query = 'SELECT emails.*';
            $query .= $custom_join['select'];

            $query .= ' FROM emails ';
            $where_auto = "emails.deleted=0";
        }

        $query .= $custom_join['join'];

        if ($where != "") {
            $query .= "where $where AND " . $where_auto;
        } else {
            $query .= "where " . $where_auto;
        }

        if ($order_by != "") {
            $query .= " ORDER BY $order_by";
        } else {
            $query .= " ORDER BY emails.name";
        }

        return $query;
    }

    /**
     * @global $app_list_strings
     * @global $theme
     * @global $current_user
     * @global $timedate
     * @global $mod_strings
     * @return array
     */
    public function get_list_view_data()
    {
        global $app_list_strings;
        global $theme;
        global $current_user;
        global $timedate;
        global $mod_strings;

        $email_fields = $this->get_list_view_array();
        $this->retrieveEmailText();
        $email_fields['FROM_ADDR'] = $this->from_addr_name;
        isValidEmailAddress($email_fields['FROM_ADDR']);
        $email_fields['FROM_ADDR_NAME'] = $this->from_addr_name;
        $email_fields['TO_ADDRS'] = $this->to_addrs;
        $email_fields['TO_ADDRS_NAMES'] = $this->to_addrs_names;
        $mod_strings = return_module_language(
            $GLOBALS['current_language'],
            'Emails'
        ); // hard-coding for Home screen ListView

        if ($this->status != 'replied') {
            $email_fields['QUICK_REPLY'] = '<a href="index.php?module=Emails&action=ReplyTo&record='. $this->id .'">'
                . $mod_strings['LNK_QUICK_REPLY'] . '</a>';
            $email_fields['STATUS'] = ($email_fields['REPLY_TO_STATUS'] == 1 ? $mod_strings['LBL_REPLIED'] : $email_fields['STATUS']);
        } else {
            $email_fields['QUICK_REPLY'] = $mod_strings['LBL_REPLIED'];
        }
        if (!empty($this->parent_type)) {
            $email_fields['PARENT_MODULE'] = $this->parent_type;
        } else {
            switch ($this->intent) {
                case 'support':
                    $email_fields['CREATE_RELATED'] = '<a href="index.php?module=Cases&action=EditView&inbound_email_id=' . $this->id . '" >' . SugarThemeRegistry::current()->getImage(
                        'CreateCases',
                        'border="0"',
                        null,
                        null,
                        ".gif",
                        $mod_strings['LBL_CREATE_CASES']
                    ) . $mod_strings['LBL_CREATE_CASE'] . '</a>';
                    break;

                case 'sales':
                    $email_fields['CREATE_RELATED'] = '<a href="index.php?module=Leads&action=EditView&inbound_email_id=' . $this->id . '" >' . SugarThemeRegistry::current()->getImage(
                        'CreateLeads',
                        'border="0"',
                        null,
                        null,
                        ".gif",
                        $mod_strings['LBL_CREATE_LEADS']
                    ) . $mod_strings['LBL_CREATE_LEAD'] . '</a>';
                    break;

                case 'contact':
                    $email_fields['CREATE_RELATED'] = '<a href="index.php?module=Contacts&action=EditView&inbound_email_id=' . $this->id . '" >' . SugarThemeRegistry::current()->getImage(
                        'CreateContacts',
                        'border="0"',
                        null,
                        null,
                        ".gif",
                        $mod_strings['LBL_CREATE_CONTACTS']
                    ) . $mod_strings['LBL_CREATE_CONTACT'] . '</a>';
                    break;

                case 'bug':
                    $email_fields['CREATE_RELATED'] = '<a href="index.php?module=Bugs&action=EditView&inbound_email_id=' . $this->id . '" >' . SugarThemeRegistry::current()->getImage(
                        'CreateBugs',
                        'border="0"',
                        null,
                        null,
                        ".gif",
                        $mod_strings['LBL_CREATE_BUGS']
                    ) . $mod_strings['LBL_CREATE_BUG'] . '</a>';
                    break;

                case 'task':
                    $email_fields['CREATE_RELATED'] = '<a href="index.php?module=Tasks&action=EditView&inbound_email_id=' . $this->id . '" >' . SugarThemeRegistry::current()->getImage(
                        'CreateTasks',
                        'border="0"',
                        null,
                        null,
                        ".gif",
                        $mod_strings['LBL_CREATE_TASKS']
                    ) . $mod_strings['LBL_CREATE_TASK'] . '</a>';
                    break;

                case 'bounce':
                    break;

                case 'pick':
                    // break;

                case 'info':
                    //break;

                default:
                    $email_fields['CREATE_RELATED'] = $this->quickCreateForm();
                    break;
            }
        }

        //BUG 17098 - MFH changed $this->from_addr to $this->to_addrs
        $email_fields['CONTACT_NAME'] = empty($this->contact_name) ? '</a>' . $this->trimLongTo($this->to_addrs) . '<a>' : $this->contact_name;
        $email_fields['CONTACT_ID'] = empty($this->contact_id) ? '' : $this->contact_id;
        $email_fields['ATTACHMENT_IMAGE'] = $this->attachment_image;
        $email_fields['LINK_ACTION'] = $this->link_action;

        if (isset($this->type_name)) {
            $email_fields['TYPE_NAME'] = $this->type_name;
        }

        $email_fields['CATEGORY_ID'] = empty($this->category_id) ? "" : $app_list_strings['email_category_dom'][$this->category_id];

        return $email_fields;
    }

    /**
     * @global $mod_strings
     * @global $app_strings
     * @global $currentModule
     * @global $current_language
     * @return string
     */
    public function quickCreateForm()
    {
        global $mod_strings, $app_strings, $currentModule, $current_language;

        // Coming from the home page via Dashlets
        if ($currentModule != 'Email') {
            $mod_strings = return_module_language($current_language, 'Emails');
        }

        return $mod_strings['LBL_QUICK_CREATE'] . "&nbsp;<a id='$this->id' onclick='return quick_create_overlib(\"{$this->id}\", \"" . SugarThemeRegistry::current() . "\", this);' href=\"#\" >" . SugarThemeRegistry::current()->getImage(
            "advanced_search",
            "border='0' align='absmiddle'",
            null,
            null,
            '.gif',
            $mod_strings['LBL_QUICK_CREATE']
        ) . "</a>";
    }

    /**
     * @global timedate
     * @global $current_user
     * @global $beanList
     * @global $sugar_config
     * @gloabl $app_strings
     *  Searches all imported emails and returns the result set as an array.
     * @param string $sort
     * @param string $direction
     * @return array
     */
    public function searchImportedEmails($sort = '', $direction = '')
    {
        require_once('include/TimeDate.php');
        global $timedate;
        global $current_user;
        global $beanList;
        global $sugar_config;
        global $app_strings;

        $emailSettings = $current_user->getPreference('emailSettings', 'Emails');
        // cn: default to a low number until user specifies otherwise
        if (empty($emailSettings['showNumInList'])) {
            $pageSize = 20;
        } else {
            $pageSize = $emailSettings['showNumInList'];
        }

        if (isset($_REQUEST['start']) && isset($_REQUEST['limit'])) {
            $page = ceil($_REQUEST['start'] / $_REQUEST['limit']) + 1;
        } else {
            $page = 1;
        }

        //Determine sort ordering

        //Sort ordering parameters in the request do not coincide with actual column names
        //so we need to remap them.
        $hrSortLocal = array(
            'flagged' => 'type',
            'status' => 'reply_to_status',
            'from' => 'emails_text.from_addr',
            'subject' => 'name',
            'date' => 'date_sent_received',
            'AssignedTo' => 'assigned_user_id',
            'flagged' => 'flagged'
        );

        $sort = !empty($_REQUEST['sort']) ? $this->db->getValidDBName($_REQUEST['sort']) : "";
        $direction = !empty($_REQUEST['dir']) && in_array(
            strtolower($_REQUEST['dir']),
            array("asc", "desc")
        ) ? $_REQUEST['dir'] : "";

        $order = (!empty($sort) && !empty($direction)) ? " ORDER BY {$hrSortLocal[$sort]} {$direction}" : "";

        //Get our main query.
        $fullQuery = $this->_genereateSearchImportedEmailsQuery();

        //Perform a count query needed for pagination.
        $countQuery = $this->create_list_count_query($fullQuery);

        $count_rs = $this->db->query($countQuery, false, 'Error executing count query for imported emails search');
        $count_row = $this->db->fetchByAssoc($count_rs);
        $total_count = ($count_row != null) ? $count_row['c'] : 0;

        $start = ($page - 1) * $pageSize;

        //Execute the query
        $rs = $this->db->limitQuery($fullQuery . $order, $start, $pageSize);

        $return = array();

        while ($a = $this->db->fetchByAssoc($rs)) {
            $temp = array();
            $temp['flagged'] = (is_null($a['flagged']) || $a['flagged'] == '0') ? '' : 1;
            $temp['status'] = (is_null($a['reply_to_status']) || $a['reply_to_status'] == '0') ? '' : 1;
            $temp['subject'] = $a['name'];
            $temp['date'] = $timedate->to_display_date_time($a['date_sent_received']);
            $temp['uid'] = $a['id'];
            $temp['ieId'] = $a['mailbox_id'];
            $temp['site_url'] = $sugar_config['site_url'];
            $temp['seen'] = ($a['status'] == 'unread') ? 0 : 1;
            $temp['type'] = $a['type'];
            $temp['mbox'] = 'sugar::Emails';
            $temp['hasAttach'] = $this->doesImportedEmailHaveAttachment($a['id']);
            //To and from addresses may be stored in emails_text, if nothing is found, revert to
            //regular email addresses.
            $temp['to_addrs'] = preg_replace('/[\x00-\x08\x0B-\x1F]/', '', (string) $a['to_addrs']);
            $temp['from'] = preg_replace('/[\x00-\x08\x0B-\x1F]/', '', (string) $a['from_addr']);
            isValidEmailAddress($temp['from']);
            if (empty($temp['from']) || empty($temp['to_addrs'])) {
                //Retrieve email addresses seperatly.
                $tmpEmail = BeanFactory::newBean('Emails');
                $tmpEmail->id = $a['id'];
                $tmpEmail->retrieveEmailAddresses();
                $temp['from'] = $tmpEmail->from_addr;
                isValidEmailAddress($temp['from']);
                $temp['to_addrs'] = $tmpEmail->to_addrs;
            }

            $return[] = $temp;
        }

        $metadata = array();
        $metadata['totalCount'] = $total_count;
        $metadata['out'] = $return;

        return $metadata;
    }

    /**
     * Determine if an imported email has an attachment by examining the relationship to notes.
     *
     * @param string $id
     * @return boolean
     */
    public function doesImportedEmailHaveAttachment($id)
    {
        $hasAttachment = false;
        $query = "SELECT id FROM notes where parent_id='$id' AND parent_type='Emails' AND file_mime_type is not null AND deleted=0";
        $rs = $this->db->limitQuery($query, 0, 1);
        $row = $this->db->fetchByAssoc($rs);
        if (!empty($row['id'])) {
            $hasAttachment = true;
        }

        return (int)$hasAttachment;
    }

    /**
     * @global $timedate
     * Generate the query used for searching imported emails.
     *
     * @return String Query to be executed.
     */
    public function _genereateSearchImportedEmailsQuery()
    {
        global $timedate;

        $additionalWhereClause = $this->_generateSearchImportWhereClause();

        $query = array();
        $fullQuery = "";
        $query['select'] = "emails.id , emails.mailbox_id, emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, emails.reply_to_status,
		                      emails_text.from_addr, emails_text.to_addrs  FROM emails ";

        $query['joins'] = " JOIN emails_text on emails.id = emails_text.email_id ";

        //Handle from and to addr joins
        if (!empty($_REQUEST['from_addr'])) {
            $from_addr = $this->db->quote(strtolower($_REQUEST['from_addr']));
            isValidEmailAddress($from_addr);
            $query['joins'] .= "INNER JOIN emails_email_addr_rel er_from ON er_from.email_id = emails.id AND er_from.deleted = 0 INNER JOIN email_addresses ea_from ON ea_from.id = er_from.email_address_id
                                AND er_from.address_type='from' AND emails_text.from_addr LIKE '%" . $from_addr . "%'";
        }

        if (!empty($_REQUEST['to_addrs'])) {
            $to_addrs = $this->db->quote(strtolower($_REQUEST['to_addrs']));
            $query['joins'] .= "INNER JOIN emails_email_addr_rel er_to ON er_to.email_id = emails.id AND er_to.deleted = 0 INNER JOIN email_addresses ea_to ON ea_to.id = er_to.email_address_id
                                    AND er_to.address_type='to' AND ea_to.email_address LIKE '%" . $to_addrs . "%'";
        }

        $query['where'] = " WHERE (emails.type= 'inbound' OR emails.type='archived' OR emails.type='out') AND emails.deleted = 0 ";
        if (!empty($additionalWhereClause)) {
            $query['where'] .= "AND $additionalWhereClause";
        }

        //If we are explicitly looking for attachments.  Do not use a distinct query as the to_addr is defined
        //as a text which equals clob in oracle and the distinct query can not be executed correctly.
        $addDistinctKeyword = "";
        if (!empty($_REQUEST['attachmentsSearch']) && $_REQUEST['attachmentsSearch'] == 1) { //1 indicates yes
            $query['where'] .= " AND EXISTS ( SELECT id FROM notes n WHERE n.parent_id = emails.id AND n.deleted = 0 AND n.filename is not null )";
        } else {
            if (!empty($_REQUEST['attachmentsSearch']) && $_REQUEST['attachmentsSearch'] == 2) {
                $query['where'] .= " AND NOT EXISTS ( SELECT id FROM notes n WHERE n.parent_id = emails.id AND n.deleted = 0 AND n.filename is not null )";
            }
        }

        $fullQuery = "SELECT " . $query['select'] . " " . $query['joins'] . " " . $query['where'];

        return $fullQuery;
    }

    /**
     * @return array|string
     * @global $timedate
     * Generate the where clause for searching imported emails.
     */
    public function _generateSearchImportWhereClause()
    {
        global $timedate;

        $db = DBManagerFactory::getInstance();

        //The clear button was removed so if a user removes the asisgned user name, do not process the id.
        if (empty($_REQUEST['assigned_user_name']) && !empty($_REQUEST['assigned_user_id'])) {
            unset($_REQUEST['assigned_user_id']);
        }

        $availableSearchParam = [
            'name' => ['table_name' => 'emails'],
            'data_parent_id_search' => ['table_name' => 'emails', 'db_key' => 'parent_id', 'opp' => '='],
            'assigned_user_id' => ['table_name' => 'emails', 'opp' => '=']
        ];

        $additionalWhereClause = [];
        foreach ($availableSearchParam as $key => $properties) {
            if (!empty($_REQUEST[$key])) {
                $db_key = isset($properties['db_key']) ? $properties['db_key'] : $key;
                $searchValue = $this->db->quote($_REQUEST[$key]);

                $opp = isset($properties['opp']) ? $properties['opp'] : 'like';
                if ($opp === 'like') {
                    $searchValue = '%' . $searchValue . '%';
                }

                $additionalWhereClause[] = "{$properties['table_name']}.$db_key $opp '$searchValue' ";
            }
        }


        $isDateFromSearchSet = !empty($_REQUEST['searchDateFrom']);
        $isdateToSearchSet = !empty($_REQUEST['searchDateTo']);
        $bothDateRangesSet = $isDateFromSearchSet && $isdateToSearchSet;

        // Handle date from and to separately
        if ($bothDateRangesSet) {
            $dbFormatDateFrom = $timedate->to_db_date($_REQUEST['searchDateFrom'], false);
            $dbFormatDateFrom = $db->convert("'" . $dbFormatDateFrom . "'", 'datetime');

            $dbFormatDateTo = $timedate->to_db_date($_REQUEST['searchDateTo'], false);
            $dbFormatDateTo = $db->convert("'" . $dbFormatDateTo . "'", 'datetime');

            $additionalWhereClause[] = "( emails.date_sent_received >= $dbFormatDateFrom AND
                                          emails.date_sent_received <= $dbFormatDateTo )";
        } elseif ($isdateToSearchSet) {
            $dbFormatDateTo = $timedate->to_db_date($_REQUEST['searchDateTo'], false);
            $dbFormatDateTo = $db->convert("'" . $dbFormatDateTo . "'", 'datetime');
            $additionalWhereClause[] = "emails.date_sent_received <= $dbFormatDateTo ";
        } elseif ($isDateFromSearchSet) {
            $dbFormatDateFrom = $timedate->to_db_date($_REQUEST['searchDateFrom'], false);
            $dbFormatDateFrom = $db->convert("'" . $dbFormatDateFrom . "'", 'datetime');
            $additionalWhereClause[] = "emails.date_sent_received >= $dbFormatDateFrom ";
        }

        $additionalWhereClause = implode(' AND ', $additionalWhereClause);

        return $additionalWhereClause;
    }

    /**
     *
     * takes a long TO: string of emails and returns the first appended by an elipse
     * @param string $str
     * @return string
     */
    public function trimLongTo($str)
    {
        if (strpos($str, ',')) {
            $exStr = explode(',', $str);

            return $exStr[0] . '...';
        } elseif (strpos($str, ';')) {
            $exStr = explode(';', $str);

            return $exStr[0] . '...';
        }
        return $str;
    }

    /**
     * @return string
     */
    public function get_summary_text()
    {
        return $this->name;
    }


    /**
     * @global $app_list_strings
     * @global $app_strings
     * @global $mod_strings
     * @global $theme
     * @global $current_user
     * @param string $where
     * @return string
     */
    public function distributionForm($where)
    {
        global $app_list_strings;
        global $app_strings;
        global $mod_strings;
        global $theme;
        global $current_user;

        $distribution = get_select_options_with_id($app_list_strings['dom_email_distribution'], '');
        $_SESSION['distribute_where'] = $where;


        $out = '<form name="Distribute" id="Distribute">';
        $out .= get_form_header($mod_strings['LBL_DIST_TITLE'], '', false);
        $out .= <<<eoq
		<script>
			enableQS(true);
		</script>
eoq;
        $out .= '
		<table cellpadding="0" cellspacing="0" width="100%" border="0">
			<tr>
				<td>
					<script type="text/javascript">


						function checkDeps(form) {
							return;
						}

						function mySubmit() {
							var assform = document.getElementById("Distribute");
							var select = document.getElementById("userSelect");
							var assign1 = assform.r1.checked;
							var assign2 = assform.r2.checked;
							var dist = assform.dm.value;
							var assign = false;
							var users = false;
							var rules = false;
							var warn1 = "' . $mod_strings['LBL_WARN_NO_USERS'] . '";
							var warn2 = "";

							if(assign1 || assign2) {
								assign = true;

							}

							for(i=0; i<select.options.length; i++) {
								if(select.options[i].selected == true) {
									users = true;
									warn1 = "";
								}
							}

							if(dist != "") {
								rules = true;
							} else {
								warn2 = "' . $mod_strings['LBL_WARN_NO_DIST'] . '";
							}

							if(assign && users && rules) {

								if(document.getElementById("r1").checked) {
									var mu = document.getElementById("MassUpdate");
									var grabbed = "";

									for(i=0; i<mu.elements.length; i++) {
										if(mu.elements[i].type == "checkbox" && mu.elements[i].checked && mu.elements[i].name.value != "massall") {
											if(grabbed != "") { grabbed += "::"; }
											grabbed += mu.elements[i].value;
										}
									}
									var formgrab = document.getElementById("grabbed");
									formgrab.value = grabbed;
								}
								assform.submit();
							} else {
								alert("' . $mod_strings['LBL_ASSIGN_WARN'] . '" + "\n" + warn1 + "\n" + warn2);
							}
						}

						function submitDelete() {
							if(document.getElementById("r1").checked) {
								var mu = document.getElementById("MassUpdate");
								var grabbed = "";

								for(i=0; i<mu.elements.length; i++) {
									if(mu.elements[i].type == "checkbox" && mu.elements[i].checked && mu.elements[i].name != "massall") {
										if(grabbed != "") { grabbed += "::"; }
										grabbed += mu.elements[i].value;
									}
								}
								var formgrab = document.getElementById("grabbed");
								formgrab.value = grabbed;
							}
							if(grabbed == "") {
								alert("' . $mod_strings['LBL_MASS_DELETE_ERROR'] . '");
							} else {
								document.getElementById("Distribute").submit();
							}
						}

					</script>
						<input type="hidden" name="module" value="Emails">
						<input type="hidden" name="action" id="action">
						<input type="hidden" name="grabbed" id="grabbed">

					<table cellpadding="1" cellspacing="0" width="100%" border="0" class="edit view">
						<tr height="20">
							<td scope="col" scope="row" NOWRAP align="center">
								&nbsp;' . $mod_strings['LBL_ASSIGN_SELECTED_RESULTS_TO'] . '&nbsp;';
        $out .= $this->userSelectTable();
        $out .= '</td>
							<td scope="col" scope="row" NOWRAP align="left">
								&nbsp;' . $mod_strings['LBL_USING_RULES'] . '&nbsp;
								<select name="distribute_method" id="dm" onChange="checkDeps(this.form);">' . $distribution . '</select>
							</td>';


        $out .= '</td>
							</tr>';


        $out .= '<tr>
								<td scope="col" width="50%" scope="row" NOWRAP align="right" colspan="2">
								<input title="' . $mod_strings['LBL_BUTTON_DISTRIBUTE_TITLE'] . '"
									id="dist_button"
									class="button" onClick="AjaxObject.detailView.handleAssignmentDialogAssignAction();"
									type="button" name="button"
									value="  ' . $mod_strings['LBL_BUTTON_DISTRIBUTE'] . '  ">';
        $out .= '</tr>
					</table>
				</td>
			</tr>
		</table>
		</form>';

        return $out;
    }

    /**
     * @global $theme
     * @global $mod_strings
     * @return string
     */
    public function userSelectTable()
    {
        global $theme;
        global $mod_strings;

        $colspan = 1;
        $setTeamUserFunction = '';


        // get users
        $r = $this->db->query("SELECT users.id, users.user_name, users.first_name, users.last_name FROM users WHERE deleted=0 AND status = 'Active' AND is_group=0 ORDER BY users.last_name, users.first_name");

        $userTable = '<table cellpadding="0" cellspacing="0" border="0">';
        $userTable .= '<tr><td colspan="2"><b>' . $mod_strings['LBL_USER_SELECT'] . '</b></td></tr>';
        $userTable .= '<tr><td><input type="checkbox" style="border:0px solid #000000" onClick="toggleAll(this); setCheckMark(); checkDeps(this.form);"></td> <td>' . $mod_strings['LBL_TOGGLE_ALL'] . '</td></tr>';
        $userTable .= '<tr><td colspan="2"><select style="visibility:hidden;" name="users[]" id="userSelect" multiple size="12">';

        while ($a = $this->db->fetchByAssoc($r)) {
            $userTable .= '<option value="' . $a['id'] . '" id="' . $a['id'] . '">' . $a['first_name'] . ' ' . $a['last_name'] . '</option>';
        }
        $userTable .= '</select></td></tr>';
        $userTable .= '</table>';

        $out = '<script type="text/javascript">';
        $out .= $setTeamUserFunction;
        $out .= '
					function setCheckMark() {
						var select = document.getElementById("userSelect");

						for(i=0 ; i<select.options.length; i++) {
							if(select.options[i].selected == true) {
								document.getElementById("checkMark").style.display="";
								return;
							}
						}

						document.getElementById("checkMark").style.display="none";
						return;
					}

					function showUserSelect() {
						var targetTable = document.getElementById("user_select");
						targetTable.style.visibility="visible";
						var userSelectTable = document.getElementById("userSelect");
						userSelectTable.style.visibility="visible";
						return;
					}
					function hideUserSelect() {
						var targetTable = document.getElementById("user_select");
						targetTable.style.visibility="hidden";
						var userSelectTable = document.getElementById("userSelect");
						userSelectTable.style.visibility="hidden";
						return;
					}
					function toggleAll(toggle) {
						if(toggle.checked) {
							var stat = true;
						} else {
							var stat = false;
						}
						var form = document.getElementById("userSelect");
						for(i=0; i<form.options.length; i++) {
							form.options[i].selected = stat;
						}
					}


				</script>
			<span id="showUsersDiv" style="position:relative;">
				<a href="#" id="showUsers" onClick="javascript:showUserSelect();">
					' . SugarThemeRegistry::current()->getImage(
            'Users',
            '',
            null,
            null,
            ".gif",
            $mod_strings['LBL_USERS']
        ) . '</a>&nbsp;
				<a href="#" id="showUsers" onClick="javascript:showUserSelect();">
					<span style="display:none;" id="checkMark">' . SugarThemeRegistry::current()->getImage(
            'check_inline',
            'border="0"',
            null,
            null,
            ".gif",
            $mod_strings['LBL_CHECK_INLINE']
                ) . '</span>
				</a>


				<div id="user_select" style="width:200px;position:absolute;left:2;top:2;visibility:hidden;z-index:1000;">
				<table cellpadding="0" cellspacing="0" border="0" class="list view">
					<tr height="20">
						<td  colspan="' . $colspan . '" id="hiddenhead" onClick="hideUserSelect();" onMouseOver="this.style.border = \'outset red 1px\';" onMouseOut="this.style.border = \'inset white 0px\';this.style.borderBottom = \'inset red 1px\';">
							<a href="#" onClick="javascript:hideUserSelect();">' . SugarThemeRegistry::current()->getImage(
                    'close',
                    'border="0"',
                    null,
                    null,
                    ".gif",
                    $mod_strings['LBL_CLOSE']
                ) . '</a>
							' . $mod_strings['LBL_USER_SELECT'] . '
						</td>
					</tr>
					<tr>';
        //<td valign="middle" height="30"  colspan="'.$colspan.'" id="hiddenhead" onClick="hideUserSelect();" onMouseOver="this.style.border = \'outset red 1px\';" onMouseOut="this.style.border = \'inset white 0px\';this.style.borderBottom = \'inset red 1px\';">
        $out .= '		<td style="padding:5px" class="oddListRowS1" bgcolor="#fdfdfd" valign="top" align="left" style="left:0;top:0;">
							' . $userTable . '
						</td>
					</tr>
				</table></div>
			</span>';

        return $out;
    }

    /**
     * @global $theme
     * @global $mod_strings
     * @param string $type
     * @return string
     */
    public function checkInbox($type)
    {
        global $theme;
        global $mod_strings;
        $out = '<div><input	title="' . $mod_strings['LBL_BUTTON_CHECK_TITLE'] . '"
						class="button"
						type="button" name="button"
						onClick="window.location=\'index.php?module=Emails&action=Check&type=' . $type . '\';"
						style="margin-bottom:2px"
						value="  ' . $mod_strings['LBL_BUTTON_CHECK'] . '  "></div>';

        return $out;
    }

    /**
     * Guesses Primary Parent id from From: email address.  Cascades guesses from Accounts to Contacts to Leads to
     * Users.  This will not affect the many-to-many relationships already constructed as this is, at best,
     * informational linking.
     */
    public function fillPrimaryParentFields()
    {
        if (empty($this->from_addr)) {
            return;
        }

        $GLOBALS['log']->debug("*** Email trying to guess Primary Parent from address [ {$this->from_addr} ]");

        $tables = array('accounts');
        $ret = array();
        // loop through types to get hits
        foreach ($tables as $table) {
            $q = "SELECT name, id FROM {$table} WHERE email1 = '{$this->from_addr}' OR email2 = '{$this->from_addr}' AND deleted = 0";
            $r = $this->db->query($q);
            while ($a = $this->db->fetchByAssoc($r)) {
                if (!empty($a['name']) && !empty($a['id'])) {
                    $this->parent_type = ucwords($table);
                    $this->parent_id = $a['id'];
                    $this->parent_name = $a['name'];

                    return;
                }
            }
        }
    }

    /**
     * Convert reference to inline image (stored as Note) to URL link
     * Enter description here ...
     * @param string $noteId of the note
     * @param string $noteType of the note
     */
    public function cid2Link($noteId, $noteType)
    {
        if (empty($this->description_html)) {
            return;
        }
        list($type, $subtype) = explode('/', $noteType);
        if (strtolower($type) != 'image') {
            return;
        }
        $upload = new UploadFile();
        $this->description_html = preg_replace(
            "#class=\"image\" src=\"cid:$noteId\.(.+?)\"#",
            "class=\"image\" src=\"{$this->imagePrefix}{$noteId}.\\1\"",
            $this->description_html
        );
        // ensure the image is in the cache
        $imgfilename = sugar_cached("images/") . "$noteId." . strtolower($subtype);
        $src = "upload://$noteId";
        if (!file_exists($imgfilename) && file_exists($src)) {
            copy($src, $imgfilename);
        }
    }

    /**
     * Convert all cid: links in this email into URLs
     */
    public function cids2Links()
    {
        if (empty($this->description_html)) {
            return;
        }
        $q = "SELECT id, file_mime_type FROM notes WHERE parent_id = '{$this->id}' AND deleted = 0";
        $r = $this->db->query($q);
        while ($a = $this->db->fetchByAssoc($r)) {
            $this->cid2Link($a['id'], $a['file_mime_type']);
        }
    }

    /**
     * Bugs 50972, 50973
     * Sets the field def for a field to allow null values
     *
     * @todo Consider moving to SugarBean to allow other models to set fields to NULL
     * @param string $field The field name to modify
     * @return void
     */
    public function setFieldNullable($field)
    {
        if (isset($this->field_defs[$field]) && is_array($this->field_defs[$field])) {
            if (empty($this->modifiedFieldDefs[$field])) {
                if (
                    isset($this->field_defs[$field]['isnull']) &&
                    (strtolower($this->field_defs[$field]['isnull']) == 'false' || $this->field_defs[$field]['isnull'] === false)
                ) {
                    $this->modifiedFieldDefs[$field]['isnull'] = $this->field_defs[$field]['isnull'];
                    unset($this->field_defs[$field]['isnull']);
                }

                if (isset($this->field_defs[$field]['dbType']) && $this->field_defs[$field]['dbType'] == 'id') {
                    $this->modifiedFieldDefs[$field]['dbType'] = $this->field_defs[$field]['dbType'];
                    unset($this->field_defs[$field]['dbType']);
                }
            }
        }
    }

    /**
     * Bugs 50972, 50973
     * Set the field def back to the way it was prior to modification
     *
     * @param $field
     * @return void
     */
    public function revertFieldNullable($field)
    {
        if (!empty($this->modifiedFieldDefs[$field]) && is_array($this->modifiedFieldDefs[$field])) {
            foreach ($this->modifiedFieldDefs[$field] as $k => $v) {
                $this->field_defs[$field][$k] = $v;
            }

            unset($this->modifiedFieldDefs[$field]);
        }
    }

    /**
     * Uses the $_REQUEST to populate the fields of an Email SugarBean.
     *
     * @param Email $bean
     * @param array $request TODO: implement PSR 7 interface and refactor
     * @return bool|Email|SugarBean
     */
    public function populateBeanFromRequest($bean, $request)
    {
        if (empty($bean)) {
            $bean = BeanFactory::getBean('Emails');
        }

        if (isset($request['id'])) {
            $bean = $bean->retrieve($_REQUEST['id']);
        }


        foreach ($request as $fieldName => $field) {
            if (array_key_exists($fieldName, $bean->field_defs)) {
                $bean->$fieldName = $field;
            }
        }

        if (isset($_REQUEST['inbound_email_id'])) {
            $bean->mailbox_id = $_REQUEST['inbound_email_id'];
        }


        $old = array('&lt;', '&gt;');
        $new = array('<', '>');


        // Validation first: we have to check that there is
        // 'from' email and/or name in the request and
        // if is not, then use the default one

        // Let's pretend that everything is ok..

        $useDefaultFromAddressName = false;
        $useDefaultFromAddressEmail = false;

        // is from address in the request?

        if (!isset($request['from_addr'])) {
            if (!isset($request['from_addr_name']) || !$request['from_addr_name']) {
                $useDefaultFromAddressName = true;
            }

            // is from name in the request?

            if (!isset($request['from_addr_email']) || !$request['from_addr_email']) {
                $useDefaultFromAddressEmail = true;
            }
        }

        // so, do we have to use any default data?

        if ($useDefaultFromAddressName || $useDefaultFromAddressEmail) {

            // get the default data
            // (curently the system default will be used)

            $defaultEmail = $bean->getSystemDefaultEmail();

            // do we have to use the default from address?

            if ($useDefaultFromAddressEmail) {

                // just make sure are there any default 'from' address set? (validation)

                if (!isset($defaultEmail['email']) || !$defaultEmail['email']) {
                    throw new EmailException("No system default 'from' email address", NO_DEFAULT_FROM_ADDR);
                }

                // use the default one

                $request['from_addr_email'] = $defaultEmail['email'];
            }

            // do we have to use the default name?

            if ($useDefaultFromAddressName) {

                // just make sure are there any default 'from' address set? (validation)

                if (!isset($defaultEmail['name']) || !$defaultEmail['name']) {
                    throw new EmailException("No system default 'from' name", NO_DEFAULT_FROM_NAME);
                }

                // use the default one

                $request['from_addr_name'] = $defaultEmail['name'];
            }
        }

        if (isset($request['from_addr']) && $request['from_addr'] != $request['from_addr_name'] . ' &lt;' . $request['from_addr_email'] . '&gt;') {
            if (false === strpos((string) $request['from_addr'], '&lt;')) { // we have an email only?
                $bean->from_addr = $request['from_addr'];
                isValidEmailAddress($bean->from_addr);
                $bean->from_name = '';
                $bean->reply_to_addr = $bean->from_addr;
                $bean->reply_to_name = $bean->from_name;
            } else { // we have a compound string
                $newFromAddr = str_replace($old, $new, (string) $request['from_addr']);
                $bean->from_addr = substr(
                    $newFromAddr,
                    (1 + strpos($newFromAddr, '<')),
                    (strpos($newFromAddr, '>') - strpos($newFromAddr, '<')) - 1
                );
                isValidEmailAddress($bean->from_addr);
                $bean->from_name = substr($newFromAddr, 0, (strpos($newFromAddr, '<') - 1));
                $bean->reply_to_addr = $bean->from_addr;
                $bean->reply_to_name = $bean->from_name;
            }
        } elseif (!empty($request['from_addr_email']) && isset($request['from_addr_email'])) {
            $bean->from_addr = $request['from_addr_email'];
            isValidEmailAddress($bean->from_addr);
            $bean->from_name = $request['from_addr_name'];
        } else {
            $bean->from_addr = $bean->getSystemDefaultEmail();
            isValidEmailAddress($bean->from_addr);
            $bean->reply_to_addr = $bean->from_addr['email'];
            $bean->reply_to_name = $bean->from_addr['name'];
        }



        if (!empty($request['to_addrs_names'])) {
            $bean->to_addrs_names = htmlspecialchars_decode((string) $request['to_addrs_names']);
        }

        if (!empty($bean->to_addrs_names)) {
            $bean->to_addrs = htmlspecialchars_decode((string) $bean->to_addrs_names);
        }


        $toEmailAddresses = preg_split('/[,;]/', (string) $bean->to_addrs, null, PREG_SPLIT_NO_EMPTY);
        $bean->to_addr_arr = array();
        foreach ($toEmailAddresses as $ea => $address) {
            preg_match(
                '/([a-zA-z0-9\!\#\$\%\&\'\*\+\-\/\ =\?\^\`\{\|\}\~\.\[\]\"\(\)\s]+)((<[a-zA-z0-9\!\#\$\%\&\'\*\+\-\/\=\?\^\_\`\{\|\}\~\.\[\]\"\(\)]+)(@)([a-zA-z0-9\-\.]+\>))$/',
                (string) $address,
                $matches
            );

            // Strip out name from email address
            // eg Angel Mcmahon <sales.vegan@example.it>
            if (count($matches) > 3) {
                $email = $matches[2];
                $display = (str_replace($email, '', (string) $address));
                $display = (trim(str_replace('"', '', $display)));
            } else {
                $email = $address;
                $display = '';
            }

            $email = str_ireplace('<', '', (string) $email);
            $email = str_ireplace('>', '', $email);
            $email = str_ireplace('&lt;', '', $email);
            $email = str_ireplace('&rt;', '', $email);


            $bean->to_addrs_arr[] = array(
                'email' => $email,
                'display' => $display
            );
        }


        if (empty($bean->cc_addrs)) {
            if (!empty($request['cc_addrs_names'])) {
                $bean->cc_addrs_names = htmlspecialchars_decode((string) $request['cc_addrs_names']);
            }

            if (!empty($bean->cc_addrs_names)) {
                $bean->cc_addrs = htmlspecialchars_decode((string) $bean->cc_addrs_names);
            }
        }

        $ccEmailAddresses = preg_split('/[,;]/', (string) $bean->cc_addrs, null, PREG_SPLIT_NO_EMPTY);
        $bean->cc_addrs_arr = array();
        foreach ($ccEmailAddresses as $ea => $address) {
            $email = '';
            $display = '';
            preg_match(
                '/([a-zA-z0-9\!\#\$\%\&\'\*\+\-\/\ =\?\^\`\{\|\}\~\.\[\]\"\(\)\s]+)((<[a-zA-z0-9\!\#\$\%\&\'\*\+\-\/\=\?\^\_\`\{\|\}\~\.\[\]\"\(\)]+)(@)([a-zA-z0-9\-\.]+\>))$/',
                (string) $address,
                $matches
            );

            // Strip out name from email address
            // eg Angel Mcmahon <sales.vegan@example.it>
            if (count($matches) > 3) {
                $display = trim($matches[1]);
                $email = $matches[2];
            } else {
                $email = $address;
                $display = '';
            }

            $email = str_ireplace('<', '', (string) $email);
            $email = str_ireplace('>', '', $email);
            $email = str_ireplace('&lt;', '', $email);
            $email = str_ireplace('&rt;', '', $email);


            $bean->cc_addrs_arr[] = array(
                'email' => $email,
                'display' => $display,
            );
        }


        if (empty($bean->bcc_addrs)) {
            if (!empty($request['bcc_addrs_names'])) {
                $bean->bcc_addrs_names = htmlspecialchars_decode((string) $request['bcc_addrs_names']);
            }

            if (!empty($bean->bcc_addrs_names)) {
                $bean->bcc_addrs = htmlspecialchars_decode((string) $bean->bcc_addrs_names);
            }
        }

        $bccEmailAddresses = preg_split('/[,;]/', (string) $bean->bcc_addrs, null, PREG_SPLIT_NO_EMPTY);
        $bean->bcc_addrs_arr = array();
        foreach ($bccEmailAddresses as $ea => $address) {
            $email = '';
            $display = '';
            preg_match(
                '/([a-zA-z0-9\!\#\$\%\&\'\*\+\-\/\ =\?\^\`\{\|\}\~\.\[\]\"\(\)\s]+)((<[a-zA-z0-9\!\#\$\%\&\'\*\+\-\/\=\?\^\_\`\{\|\}\~\.\[\]\"\(\)]+)(@)([a-zA-z0-9\-\.]+\>))$/',
                (string) $address,
                $matches
            );

            // Strip out name from email address
            // eg Angel Mcmahon <sales.vegan@example.it>
            if (count($matches) > 3) {
                $display = trim($matches[1]);
                $email = $matches[2];
            } else {
                $email = $address;
                $display = '';
            }

            $email = str_ireplace('<', '', (string) $email);
            $email = str_ireplace('>', '', $email);
            $email = str_ireplace('&lt;', '', $email);
            $email = str_ireplace('&rt;', '', $email);


            $bean->bcc_addrs_arr[] = array(
                'email' => $email,
                'display' => $display,
            );
        }

        if (empty($bean->name)) {
            if (!empty($request['name'])) {
                $bean->name = $request['name'];
            }
        }

        if (empty($bean->description_html)) {
            if (!empty($request['description_html'])) {
                $bean->description_html = $request['description_html'];
            }
        }

        if (empty($bean->description)) {
            if (!empty($request['description'])) {
                $bean->description = $request['description'];
            }
        }

        // When use is sending email after selecting forward or reply to
        // We need to generate a new id
        if (isset($_REQUEST['refer_action']) && !empty($_REQUEST['refer_action'])) {
            $referActions = array('Forward', 'ReplyTo', 'ReplyToAll');
            if (in_array($_REQUEST['refer_action'], $referActions)) {
                $bean->id = create_guid();
                $bean->new_with_id = true;
                $bean->type = 'out';
                $bean->status = 'draft';
            }
        }

        return $bean;
    }

    /**
     * @param Note $note
     */
    public function attachNote(Note $note)
    {
        $this->load_relationship('notes');
        $this->notes->addBean($note);
    }

    /**
     * @param $mail
     */
    protected function handleBodyInPlainTextFormat($mail)
    {
        $this->description_html = '';
        $mail->IsHTML(false);
        $plainText = from_html($this->description);
        $plainText = str_replace("&nbsp;", " ", (string) $plainText);
        $plainText = str_replace("</p>", "</p><br />", $plainText);
        $plainText = strip_tags(br2nl($plainText));
        $plainText = str_replace("&amp;", "&", $plainText);
        $plainText = str_replace("&#39;", "'", $plainText);
        $mail->Body = wordwrap($plainText, 996);
        $mail->Body = $this->decodeDuringSend($mail->Body);
        $this->description = $mail->Body;
    }


    /**
     * Send OptIn Email to EmailAddress By Id
     * return success state or false if it's disabled in config
     *
     * @global array $sugar_config
     * @global LoggerManager $log
     * @param string $id
     * @return bool
     */
    private function sendOptInEmailToEmailAddressById($id)
    {
        global $sugar_config;
        global $log;

        $ret = false;

        if (!$id) {
            $log->fatal('Empty Email Id');
        } elseif (
            isset($sugar_config['email_enable_auto_send_opt_in'])
            && $sugar_config['email_enable_auto_send_opt_in']
        ) {
            /** @var EmailAddress $emailAddress */
            $emailAddresses = BeanFactory::getBean('EmailAddresses');
            $emailAddress = $emailAddresses->retrieve($id);

            if (
                $emailAddress !== null
                && $emailAddress->confirm_opt_in_sent_date === null
                && $emailAddress->email_address !== null && $emailAddress->getConfirmedOptInState() === EmailAddress::COI_STAT_CONFIRMED_OPT_IN
            ) {
                $ret = $this->sendOptInEmail($emailAddress);
                if (!$ret) {
                    LoggerManager::getLogger()->error('Error sending opt-in email to: ' . $emailAddress->email_address);
                }
            }
        }

        return $ret;
    }

    /**
     * @deprecated DO NOT CALL THIS, remove this function as we have a similar working version in EmailMan.php
     * @see same functionality implemented in EmailMam::sendOptInEmail() method. Use that instead this
     * @global array $app_strings
     * @param EmailAddress $emailAddress
     * @return boolean
     * @throws Exception
     */
    private function sendOptInEmail(EmailAddress $emailAddress)
    {
        global $app_strings;

        LoggerManager::getLogger()->deprecated(__FUNCTION__ . ' is deprecated.');

        $ret = false;

        $db = $this->db;
        $log = LoggerManager::getLogger();
        $timedate = new TimeDate();
        $configurator = new Configurator();
        $sugar_config =  $configurator->config;
        if (!$configurator->isConfirmOptInEnabled()) {
            return $ret;
        }

        require_once __DIR__ . '/../AOW_Actions/actions/actionSendEmail.php';

        $confirmOptInTemplateId = $configurator->getConfirmOptInTemplateId();

        if (!$confirmOptInTemplateId) {
            $log->fatal('Opt In Email Template is not configured. Please set up in email settings');
            SugarApplication::appendErrorMessage($app_strings['ERR_OPT_IN_TPL_NOT_SET']);
            return $ret;
        }

        // Prevent sending an opt in email multiple time
        if (!$this->parent_name || !$this->parent_type) {
            return $ret;
        }

        $emailAddressString = $emailAddress->email_address;
        if (!$this->isValidEmail($emailAddressString)) {
            $emailAddressString = $emailAddress->email_address[0]['email_address'];
            if (!$this->isValidEmail($emailAddressString)) {
                throw new Exception('Invalid email address: ' . $emailAddressString);
            }
        }

        // Send email template

        $params = array(
            'individual_email' => '1',
            'email_template' => $confirmOptInTemplateId,
            'email_to_type' => array(
                0 => 'to',
            ),
            'email_target_type' => array(
                0 => 'Email Address',
            ),
            'email' => array(
                0 => $emailAddressString,
            ),
        );


        // Get Related Contact | Lead | Target etc.
        $query = ' SELECT * FROM email_addresses' .
            ' JOIN email_addr_bean_rel ON email_addresses.id = email_addr_bean_rel.email_address_id' .
            ' WHERE email_address_id LIKE \'' . $db->quote($emailAddress->id) . '\'' .
                ' AND email_addr_bean_rel.primary_address = 1 '
                . 'AND email_addresses.deleted = 0 AND email_addr_bean_rel.deleted = 0';

        $dbResult = $db->query($query);
        while ($row = $db->fetchByAssoc($dbResult)) {
            if ($ret) {
                throw new RuntimeException('More than one bean related to a primary email address: ' . $emailAddressString);
            }

            $bean = BeanFactory::getBean($row['bean_module'], $row['bean_id']);

            $actionSendEmail = new actionSendEmail();
            $now = TimeDate::getInstance()->nowDb();
            if (!$actionSendEmail->run_action($bean, $params)) {
                $emailAddress->confirm_opt_in_fail_date = $now;
            } else {
                $emailAddress->confirm_opt_in_sent_date = $now;
            }
            $ret = $emailAddress->save();
        }

        return $ret;
    }

    /**
     *
     * @param string $emailAddressString
     * @return boolean
     */
    private function isValidEmail($emailAddressString)
    {
        return filter_var($emailAddressString, FILTER_VALIDATE_EMAIL);
    }

    /**
     * @param SugarPHPMailer $mail
     * @param Localization $locale
     * @param string|null $OBCharset
     * @return void
     * @throws \PHPMailer\PHPMailer\Exception
     */
    protected function setRecipientAddresses(SugarPHPMailer $mail, Localization $locale, ?string $OBCharset): void
    {
        foreach ((array)$this->to_addrs_arr as $addr_arr) {
            if (empty($addr_arr['display'])) {
                if (!isset($addr_arr['email']) || !$addr_arr['email']) {
                    LoggerManager::getLogger()->error('"To" email address is missing!');
                } else {
                    $mail->AddAddress($addr_arr['email'], "");
                }
            } else {
                $mail->AddAddress(
                    $addr_arr['email'],
                    $locale->translateCharsetMIME(trim($addr_arr['display']), 'UTF-8', $OBCharset)
                );
            }
        }

        if (!$this->cc_addrs_arr) {
            LoggerManager::getLogger()->warn('"CC" address(es) is not set or empty to sending email.');
        }
        foreach ((array)$this->cc_addrs_arr as $addr_arr) {
            if (empty($addr_arr['display'])) {
                $mail->AddCC($addr_arr['email'], "");
            } else {
                $mail->AddCC(
                    $addr_arr['email'],
                    $locale->translateCharsetMIME(trim($addr_arr['display']), 'UTF-8', $OBCharset)
                );
            }
        }

        if (!$this->bcc_addrs_arr) {
            LoggerManager::getLogger()->warn('"BCC" address(es) is not set or empty to sending email.');
        }
        foreach ((array)$this->bcc_addrs_arr as $addr_arr) {
            if (empty($addr_arr['display'])) {
                $mail->AddBCC($addr_arr['email'], "");
            } else {
                $mail->AddBCC(
                    $addr_arr['email'],
                    $locale->translateCharsetMIME(trim($addr_arr['display']), 'UTF-8', $OBCharset)
                );
            }
        }
    }

    /**
     * @param object $mail
     * @param array $sugar_config
     * @return bool
     */
    public function isToUseSystemEmail(object $mail, array $sugar_config): bool
    {
        return ($mail->oe->type === 'system') && (!isset($sugar_config['email_allow_send_as_user']) || (!$sugar_config['email_allow_send_as_user']));
    }

    /**
     * @param string $emailField eg from_name
     */
    protected function validateSugarEmailAddressField($emailField)
    {
        if (!is_string($emailField)) {
            throw new InvalidArgumentException('Invalid type. $emailField must be a string value, eg. from_name');
        }

        if ($emailField === 'from_name') {
            LoggerManager::getLogger()->error('from_name is invalid email address field.');
        }
    }

    /**
     * @param $LBL_EMAIL_ATTACHMENT
     * @param $upload_badext
     * @param object $mail
     * @param Localization $locale
     * @param string|null $OBCharset
     * @return void
     */
    protected function setupAttachments(
        $LBL_EMAIL_ATTACHMENT,
        $upload_badext,
        object $mail,
        Localization $locale,
        ?string $OBCharset
    ): void {
///////////////////////////////////////////////////////////////////////
        ////	ATTACHMENTS
        if (isset($this->saved_attachments)) {
            foreach ($this->saved_attachments as $note) {
                $mime_type = 'text/plain';
                if ($note->object_name == 'Note') {
                    if (!empty($note->file->temp_file_location) && is_file($note->file->temp_file_location)) { // brandy-new file upload/attachment
                        $file_location = "file/" . $note->file->temp_file_location;
                        $filename = $note->file->original_file_name;
                        $mime_type = $note->file->mime_type;
                    } else { // attachment coming from template/forward
                        $file_location = "upload/{$note->id}";
                        // cn: bug 9723 - documents from EmailTemplates sent with Doc Name, not file name.
                        $filename = !empty($note->filename) ? $note->filename : $note->name;
                        $mime_type = $note->file_mime_type;
                    }
                } elseif ($note->object_name == 'DocumentRevision') { // from Documents
                    $filePathName = $note->id;
                    // cn: bug 9723 - Emails with documents send GUID instead of Doc name
                    $filename = $note->getDocumentRevisionNameForDisplay();
                    $file_location = "upload/$note->id";
                    $mime_type = $note->file_mime_type;
                }

                // strip out the "Email attachment label if exists
                $filename = str_replace($LBL_EMAIL_ATTACHMENT . ': ', '', (string) $filename);
                $file_ext = pathinfo($filename, PATHINFO_EXTENSION);
                //is attachment in our list of bad files extensions?  If so, append .txt to file location
                //check to see if this is a file with extension located in "badext"
                foreach ($upload_badext as $badExt) {
                    if (strtolower($file_ext) === strtolower($badExt)) {
                        //if found, then append with .txt to filename and break out of lookup
                        //this will make sure that the file goes out with right extension, but is stored
                        //as a text in db.
                        $file_location = $file_location . ".txt";
                        break; // no need to look for more
                    }
                }
                $mail->AddAttachment(
                    $file_location,
                    $locale->translateCharsetMIME(trim($filename), 'UTF-8', $OBCharset),
                    'base64',
                    $mime_type
                );

                // embedded Images
                if ($note->embed_flag == true) {
                    $cid = $filename;
                    $mail->AddEmbeddedImage($file_location, $cid, $filename, 'base64', $mime_type);
                }
            }
        } else {
            LoggerManager::getLogger()->fatal('Attachements not found');
        }
        ////	END ATTACHMENTS
        ///////////////////////////////////////////////////////////////////////
    }

    /**
     * @param OutboundEmailAccounts $outboundEmailAccount
     * @param object $mail
     * @return array
     */
    protected function setOutboundBasedSenderAddresses(OutboundEmailAccounts $outboundEmailAccount, object $mail): array
    {
        $mail->From = $outboundEmailAccount->getFromAddress();
        $this->from_addr = $mail->From;
        $mail->FromName = $outboundEmailAccount->getFromName();
        $this->from_name = $mail->FromName;

        $ReplyToAddr = $outboundEmailAccount->getReplyToAddress();
        $ReplyToName = $outboundEmailAccount->getReplyToName();

        return [$mail->From, $ReplyToAddr, $ReplyToName];
    }

    /**
     * @param object $mail
     * @return array
     */
    protected function setSystemBasedSenderAddresses(object $mail): array
    {
        $fromAddr = $mail->oe->smtp_from_addr;
        $fromName = $mail->oe->smtp_from_name;

        $mail->From = $fromAddr;
        $sender = $fromAddr;
        $ReplyToAddr = $fromAddr;
        isValidEmailAddress($mail->From);
        $ReplyToName = $fromName;
        $mail->FromName = $fromName;

        return [$sender, $ReplyToAddr, $ReplyToName];
    }

    /**
     * @param $mail
     * @return void
     * @throws EmailValidatorException
     * @throws \SuiteCRM\ErrorMessageException
     */
    protected function validateBeforeSend($mail): void
    {
        $validator = new EmailFromValidator();
        if (!$validator->isValid($this)) {

            // if an email is invalid before sending,
            // maybe at this point sould "return false;" because the email having
            // invalid from address and/or name but we will trying to send it..
            // and we should log the problem at least:

            // (needs EmailFromValidation and EmailFromFixer.. everywhere where from name and/or from email address get a value)

            $errors = $validator->getErrorsAsText();
            $details = "Details:\n{$errors['messages']}\ncodes:{$errors['codes']}\n{$mail->ErrorInfo}";
            LoggerManager::getLogger()->error("Invalid email from address or name detected before sending. $details");
        }
    }

} // end class def
