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

/**
 * NonGmailSentFolderHandler
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class NonGmailSentFolderHandler
{
    public const NO_ERROR = 0;
    public const ERR_EMPTY_MAILBOX = 1;
    public const ERR_NO_STORED_SENT_FOLDER = 2;
    public const INVALID_STORE_AS_SENT_CALL = 3;
    public const ERR_DONT_HAVE_TO_STORE = 4;
    public const ERR_SHOULD_BE_INT = 5;
    public const ERR_SHOULD_BE_STRING = 6;
    public const ERR_NO_IE_FOUND = 7;
    public const ERR_IS_POP3 = 8;
    public const ERR_IS_GMAIL = 9;
    public const ERR_COULDNT_COPY_TO_SENT = 10;
    public const UNHANDLER_ERROR = 100;
    
    /**
     *
     * @var int|null
     */
    protected $lastError = null;
    
    /**
     *
     */
    public function __destruct()
    {
        $err = $this->getLastError();
        if (null !== $err) {
            LoggerManager::getLogger()->error(
                'Unhandled non gmail sent folder hander error: ' . $err,
                self::UNHANDLER_ERROR
            );
        }
    }
    
    /**
     *
     */
    public function clearLastError()
    {
        if (null !== $this->lastError) {
            LoggerManager::getLogger()->fatal(
                'Clear an unused Last Error of NonGmailSentFolderHandler: ' . $this->lastError
            );
        }
        $this->lastError = null;
    }
    
    /**
     *
     * @return int
     */
    public function getLastError()
    {
        $ret = $this->lastError;
        $this->lastError = null;
        return $ret;
    }
    
    /**
     *
     * @param int $err
     */
    public function setLastError($err)
    {
        if (!is_int($err)) {
            throw new InvalidArgumentException('Error code should be an integer.', self::ERR_SHOULD_BE_INT);
        }
        
        if (null !== $this->lastError) {
            LoggerManager::getLogger()->error('Last Error already set for NonGmailSentFolderHandler but never checked: ' . $this->lastError);
        }
        $this->lastError = $err;
    }
    
    
    /**
     *
     * @param InboundEmail $ie
     * @return bool
     * @param SugarPHPMailer $mail
     */
    public function storeInSentFolder(InboundEmail $ie, SugarPHPMailer $mail, $options = "\\Seen")
    {
        $ret = false;
        $ok = !$this->lastError;
        $ieIdFound = isset($ie->id) && $ie->id;
        if ($ok && !$ieIdFound) {
            $this->setLastError(self::ERR_NO_IE_FOUND);
            $ok = false;
        }
        $isPop3Protocol = $ie->isPop3Protocol();
        if ($ok && $isPop3Protocol) {
            $this->setLastError(self::ERR_IS_POP3);
            $ok = false;
        }
        $isGmail = $mail->oe->mail_smtptype == 'gmail';
        if ($ok && $isGmail) {
            $this->setLastError(self::ERR_IS_GMAIL);
            $ok = false;
        }
        if ($ok) {
            $ret = $this->storeInNonGmailSentMailBox($ie, $mail, $options);
        } else {
            LoggerManager::getLogger()->error($this->getProblemOfStoringInNonGmailSentFolder($ie, $mail));
        }
        return $ret;
    }
    
    /**
     *
     * @param InboundEmail $ie
     * @param SugarPHPMailer $mail
     * @return bool
     * @throws EmailException
     */
    protected function storeInNonGmailSentMailBox(InboundEmail $ie, SugarPHPMailer $mail, $options = "\\Seen")
    {
        $ret = false;
        $this->clearLastError();
        $sentFolder = $ie->get_stored_options("sentFolder");
        if (!empty($sentFolder)) {
            $ret = $this->connectToNonGmailServer($ie, $mail, $sentFolder, $options);
        } else {
            if (!$ie->mailbox) {
                LoggerManager::getLogger()->warn("could not copy email to sent folder, mailbox is not set or empty");
                $this->setLastError(self::ERR_EMPTY_MAILBOX);
            } else {
                LoggerManager::getLogger()->warn("could not copy email to {$ie->mailbox} sent folder as its empty");
                $this->setLastError(self::ERR_NO_STORED_SENT_FOLDER);
            }
        }
        return $ret;
    }
    
    /**
     *
     * @param InboundEmail $ie
     * @param SugarPHPMailer $mail
     * @return string
     */
    protected function getProblemOfStoringInNonGmailSentFolder(InboundEmail $ie, SugarPHPMailer $mail)
    {
        $msg = '';
        if (!isset($ie->id)) {
            $msg .= 'IE ID is not set.';
        } elseif (!$ie->id) {
            $msg .= 'IE ID is set but no value.';
        }
        if ($ie->isPop3Protocol()) {
            $msg .= 'It is a pop3 protocoll.';
        }
        if ($mail->oe->mail_smtptype == 'gmail') {
            $msg .= 'It is a gmail.';
        }
        return $msg;
    }
    
    /**
     *
     * @param InboundEmail $ie
     * @param SugarPHPMailer $mail
     * @param string $sentFolder
     * @return bool
     */
    protected function connectToNonGmailServer(InboundEmail $ie, SugarPHPMailer $mail, $sentFolder, $options = "\\Seen")
    {
        if (!is_string($sentFolder) || !$sentFolder) {
            throw new InvalidArgumentException('Sent folder should be a valid folder name string.', self::ERR_SHOULD_BE_STRING);
        }
        
        $msg = $ie->connectMailserver();
        $ret = $msg == 'true';
        if ($ret) {
            $ret = $this->copyToNonGmailSentFolder($ie, $mail, $sentFolder, $options);
            return $ret;
        }
        LoggerManager::getLogger()->warn(
            "could not connect to mail serve for folder {$ie->mailbox} for {$ie->name} error message: $msg"
        );
        return false;
    }
    
    /**
     *
     * @param InboundEmail $ie
     * @param SugarPHPMailer $mail
     * @param string $sentFolder
     * @return @return bool <b>TRUE</b> on success or <b>FALSE</b> on failure.
     */
    protected function copyToNonGmailSentFolder(InboundEmail $ie, SugarPHPMailer $mail, $sentFolder, $options = "\\Seen")
    {
        // Call CreateBody() before CreateHeader() as that is where boundary IDs are generated.
        $emailbody = $mail->CreateBody();
        $emailheader = $mail->CreateHeader();
        $data = $emailheader . "\r\n" . $emailbody . "\r\n";
        $ie->mailbox = $sentFolder;
        $connectString = $ie->getConnectString($ie->getServiceString(), $ie->mailbox);
        $returnData = $ie->getImap()->append($connectString, $data, $options);
        if (!$returnData) {
            $this->setLastError(self::ERR_COULDNT_COPY_TO_SENT);
            LoggerManager::getLogger()->warn("could not copy email to {$ie->mailbox} for {$ie->name}");
        }
        return $returnData;
    }
}
