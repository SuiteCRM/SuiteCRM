<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

use Javanile\Imap2\Connection;
use Javanile\Imap2\Errors;
use Javanile\Imap2\Functions;
use Javanile\Imap2\rcube_result_index;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once __DIR__ . '/ImapHandlerInterface.php';

/**
 * ImapHandler
 * Wrapper class for functions of imap2 lib
 */
#[\AllowDynamicProperties]
class Imap2Handler implements ImapHandlerInterface
{

    /**
     *
     * @var LoggerManager
     */
    protected $logger;

    /**
     *
     * @var mixed|boolean
     */
    protected $stream;

    /**
     *
     * @var bool
     */
    protected $logErrors;

    /**
     *
     * @var bool
     */
    protected $logCalls;

    /**
     *
     * @var string
     */
    protected $charset;

    /**
     *
     * @param bool $logErrors
     * @param bool $logCalls
     * @param null $charset
     */
    public function __construct($logErrors = true, $logCalls = true, $charset = null)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $this->logErrors = $logErrors;
        $this->logCalls = $logCalls;
        $this->logger = LoggerManager::getLogger();
        $this->charset = $charset;
    }

    /**
     *
     * @param mixed $stream
     * @param bool $validate
     */
    protected function setStream($stream, $validate = true)
    {
        if ($validate && !is_a($stream, Connection::class)) {
            $this->logger->warn('ImapHandler trying to set a non valid resource az stream.');
        }
        $this->stream = $stream;
    }

    /**
     *
     * @param bool $validate
     * @return resource
     */
    protected function getStream($validate = true)
    {
        if ($validate && !is_a($this->stream, Connection::class)) {
            $this->logger->fatal('ImapHandler trying to use a non valid resource stream.');
        }

        return $this->stream;
    }

    /**
     *
     * @param array|string $errors
     */
    protected function log($errors)
    {
        if (empty($this->logger)) {
            return;
        }

        if (is_string($errors)) {
            $this->log([$errors]);
        } elseif (!empty($errors) && $this->logErrors) {
            foreach ($errors as $error) {
                if ($error) {
                    $this->logger->fatal('An Imap error detected: ' . json_encode($error));
                }
            }
        }
    }

    /**
     *
     * @param string $func
     * @param array $args
     */
    protected function logCall($func, $args)
    {
        if ($this->logCalls) {
            $this->logger->debug('IMAP wrapper called: ' . self::class . "::$func(" . json_encode($args) . ')');
        }
    }

    /**
     *
     * @param string $func
     * @param mixed $ret
     */
    protected function logReturn($func, $ret)
    {
        if ($this->logCalls) {
            $this->logger->debug('IMAP wrapper return: ' . self::class . "::$func(...) => " . json_encode($ret));
        }
    }

    /**
     *
     * @return boolean
     */
    public function close()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        try {
            $ret = imap2_close($this->getStream());
        } catch (Throwable $e) {
            $ret = false;
        }

        if (!$ret) {
            $this->log('IMAP close error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @return array
     */
    public function getAlerts()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_alerts();
        $this->log($ret);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @return resource|boolean
     */
    public function getConnection()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = $this->getStream();
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @return array
     */
    public function getErrors()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_errors();
        $this->log($ret);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @return string|boolean
     */
    public function getLastError()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_last_error();
        $this->log($ret);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $ref
     * @param string $pattern
     * @return array
     */
    public function getMailboxes($ref, $pattern)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_getmailboxes($this->getStream(), $ref, $pattern);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @return boolean
     */
    public function isAvailable()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = function_exists('imap2_open') && function_exists('imap2_timeout');
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @param string $username
     * @param string $password
     * @param int $options
     * @param int $n_retries
     * @param array|null $params
     * @return resource|boolean
     */
    public function open($mailbox, $username, $password, $options = 0, $n_retries = 0, $params = null)
    {
        $this->logCall(__FUNCTION__, func_get_args());

        $mbh = Connection::open($mailbox, $username, $password, $options, $n_retries, $params);
        $this->setStream($mbh);

        if (empty($mbh) || !is_a($mbh, Connection::class)) {
            $this->log([
                'IMAP open error: ' . imap2_last_error(),
                'IMAP open error | debug data',
                'ImapHandler:open: ' . $mailbox,
                'ImapHandler:open: ' . $username,
                'ImapHandler:open: password is empty: ' . (empty($password) ? 'yes' : 'no'),
                'ImapHandler:open: ' . $options,
                'IMAP open error | debug data end '
            ]);
        }

        if (!$this->getStream()) {
            $this->log('IMAP open error:' . imap2_last_error());
        }

        $this->logReturn(__FUNCTION__, $this->getStream());

        return $this->getStream();
    }

    /**
     *
     * @return boolean
     */
    public function ping()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_ping($this->getStream());
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @param int $options
     * @param int $n_retries
     * @return boolean
     */
    public function reopen($mailbox, $options = 0, $n_retries = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_reopen($this->getStream(), $mailbox, $options, $n_retries);
        if (!$ret) {
            $this->log('IMAP reopen error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $timeout_type
     * @param int $timeout
     * @return mixed
     */
    public function setTimeout($timeout_type, $timeout = -1)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_timeout($timeout_type, $timeout);
        if (!$ret) {
            $this->log('IMAP set timeout error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * Execute callback and check IMAP errors for retry
     * @param callback $callback
     * @param string|null $charset
     * @return array
     */
    protected function executeImapCmd($callback, $charset = null)
    {

        // Default to class charset if none is specified
        $emailCharset = !empty($charset) ? $charset : $this->charset;

        $ret = false;

        try {
            $ret = $callback($emailCharset);

            // catch if we have BADCHARSET as exception is not thrown
            if (empty($ret) || $ret === false) {
                $err = imap2_last_error();
                if (strpos((string) $err, 'BADCHARSET')) {
                    imap2_errors();
                    throw new Exception($err);
                }
            }
        } catch (Exception $e) {
            if (strpos($e, ' [BADCHARSET (US-ASCII)]')) {
                LoggerManager::getLogger()->debug("Encoding changed dynamically from {$emailCharset} to US-ASCII");
                $emailCharset = 'US-ASCII';
                $this->charset = $emailCharset;
                $ret = $callback($emailCharset);
            }
        }

        return $ret;
    }

    /**
     *
     * @param int $criteria
     * @param int $reverse
     * @param int $options
     * @param string $search_criteria
     * @param string $charset
     * @return array
     */
    public function sort($criteria, $reverse, $options = 0, $search_criteria = null, $charset = null)
    {
        $this->logCall(__FUNCTION__, func_get_args());

        $call = function ($charset) use ($criteria, $reverse, $options, $search_criteria) {

            if (!$this->isValidStream($this->getStream())) {
                return false;
            }
            $client = $this->getStream()->getClient();

            $result = $client->sort($this->getStream()->getMailboxName(), $criteria, $search_criteria,
                $options & SE_UID);

            if (empty($result->count())) {
                $this->log('IMAP sort: empty result');

                return false;
            }

            $messages = $result->get();
            foreach ($messages as &$message) {
                $message = is_numeric($message) ? (int)$message : $message;
            }

            return $messages;
        };

        $ret = $this->executeImapCmd($call, $charset);

        if (!$ret) {
            $this->log('IMAP sort error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $uid
     * @return int
     */
    public function getMessageNo($uid)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_msgno($this->getStream(), $uid);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @param int $fromLength
     * @param int $subjectLength
     * @param string $defaultHost
     * @return bool|object Returns FALSE on error or, if successful, the information in an object
     */
    public function getHeaderInfo($msg_number, $fromLength = 0, $subjectLength = 0, $defaultHost = null)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_headerinfo($this->getStream(), $msg_number, $fromLength, $subjectLength, $defaultHost);
        if (!$ret) {
            $this->log('IMAP get header info error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return string
     */
    public function fetchHeader($msg_number, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_fetchheader($this->getStream(), $msg_number, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @param string $message
     * @param string $options
     * @param string $internal_date
     * @return bool
     */
    public function append($mailbox, $message, $options = null, $internal_date = null)
    {
        $this->logCall(__FUNCTION__, func_get_args());

        // ..to evolve a warning about an invalid internal date format
        // BUG at: https://github.com/php/php-src/blob/master/ext/imap/php_imap.c#L1357
        // -->
        if (null === $internal_date) {
            $ret = imap2_append($this->getStream(), $mailbox, $message, $options);
        } else {
            $ret = imap2_append($this->getStream(), $mailbox, $message, $options, $internal_date);
        }

        if (!$ret) {
            $this->log('IMAP append error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @return int
     */
    public function getUid($msg_number)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_uid($this->getStream(), $msg_number);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @return bool
     */
    public function expunge()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_expunge($this->getStream());
        if (!$ret) {
            $this->log('IMAP expunge error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @return object|bool Returns FALSE on failure.
     */
    public function check()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_check($this->getStream());
        if (!$ret) {
            $this->log('IMAP check error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $sequence
     * @param string $flag
     * @param int $options
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function clearFlagFull($sequence, $flag, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_clearflag_full($this->getStream(), $sequence, $flag, $options);
        if (!$ret) {
            $this->log('IMAP clearFlagFull error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function createMailbox($mailbox)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_createmailbox($this->getStream(), $mailbox);
        if (!$ret) {
            $this->log('IMAP createMailbox error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return bool Returns TRUE.
     */
    public function delete($msg_number, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_delete($this->getStream(), $msg_number, $options);
        if (!$ret) {
            $this->log('IMAP delete error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function deleteMailbox($mailbox)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_deletemailbox($this->getStream(), $mailbox);
        if (!$ret) {
            $this->log('IMAP deleteMailbox error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @param string $section
     * @param int $options
     * @return string
     */
    public function fetchBody($msg_number, $section, $options = 0)
    {

        $this->logCall(__FUNCTION__, func_get_args());
        $ret = $this->internalFetchBody($this->getStream(), $msg_number, $section, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * Override to the imap2_fetch_body function
     * @param $imap
     * @param $messageNum
     * @param $section
     * @param $flags
     * @return false|mixed|null
     */
    protected function internalFetchBody($imap, $messageNum, $section, $flags = 0)
    {
        if (!$this->isValidStream($imap)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();

        $isUid = (bool)($flags & FT_UID);

        $query = ['BODY[' . $section . ']'];

        if ($isUid) {
            $query[] = 'UID';
        }

        $messages = $client->fetch($imap->getMailboxName(), $messageNum, $isUid, $query);

        if (empty($messages)) {
            trigger_error(Errors::badMessageNumber(debug_backtrace(), 1), E_USER_WARNING);

            return false;
        }

        $targetMessage = null;
        if ($isUid) {
            foreach ($messages as $message) {
                if (!empty($message->uid) && ((int)$messageNum === (int)$message->uid)) {
                    $targetMessage = $message;
                    break;
                }
            }
        } else {
            $targetMessage = $messages[$messageNum];
        }

        if (empty($targetMessage)) {
            return null;
        }

        if ($section) {
            return $targetMessage->bodypart[$section];
        }

        return $targetMessage->body;
    }

    /**
     *
     * @param string $sequence
     * @param int $options
     * @return array
     */
    public function fetchOverview($sequence, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());

        $ret = $this->fetchList($sequence, $options);

        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return object
     */
    public function fetchStructure($msg_number, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_fetchstructure($this->getStream(), $msg_number, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return string
     */
    public function getBody($msg_number, $options)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_body($this->getStream(), $msg_number, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @return int|bool Return the number of messages in the current mailbox, as an integer, or FALSE on error.
     */
    public function getNumberOfMessages()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_num_msg($this->getStream());
        if (!$ret) {
            $this->log('IMAP getNumberOfMessages error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @param int $options
     * @return object
     */
    public function getStatus($mailbox, $options)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_status($this->getStream(), $mailbox, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $msgList
     * @param string $mailbox
     * @param int $options
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function mailCopy($msgList, $mailbox, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_mail_copy($this->getStream(), $msgList, $mailbox, $options);
        if (!$ret) {
            $this->log('IMAP mailCopy error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $msgList
     * @param string $mailbox
     * @param int $options
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function mailMove($msgList, $mailbox, $options = 0)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_mail_move($this->getStream(), $msgList, $mailbox, $options);
        if (!$ret) {
            $this->log('IMAP mailMove error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $text
     * @return array
     */
    public function mimeHeaderDecode($text)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_mime_header_decode($text);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $old_mbox
     * @param string $new_mbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function renameMailbox($old_mbox, $new_mbox)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_renamemailbox($this->getStream(), $old_mbox, $new_mbox);
        if (!$ret) {
            $this->log('IMAP renameMailbox error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $headers
     * @param string $defaultHost
     * @return object
     */
    public function rfc822ParseHeaders($headers, $defaultHost = 'UNKNOWN')
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_rfc822_parse_headers($headers, $defaultHost);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $criteria
     * @param int $options
     * @param string $charset
     * @return array|bool Return FALSE if it does not understand the search criteria or no messages have been found.
     */
    public function search($criteria, $options = SE_FREE, $charset = null)
    {
        $this->logCall(__FUNCTION__, func_get_args());

        $call = function ($charset) use ($criteria, $options) {
            return imap2_search($this->getStream(), $criteria, $options, $charset);
        };

        $ret = $this->executeImapCmd($call, $charset);

        if (!$ret) {
            $this->log('IMAP search error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $sequence
     * @param string $flag
     * @param int $options
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function setFlagFull($sequence, $flag, $options = NIL)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_setflag_full($this->getStream(), $sequence, $flag, $options);
        if (!$ret) {
            $this->log('IMAP setFlagFull error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function subscribe($mailbox)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_subscribe($this->getStream(), $mailbox);
        if (!$ret) {
            $this->log('IMAP subscribe error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function unsubscribe($mailbox)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_unsubscribe($this->getStream(), $mailbox);
        if (!$ret) {
            $this->log('IMAP unsubscribe error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $data
     * @return string|bool FALSE if text contains invalid modified UTF-7 sequence or text contains a character that is not part of ISO-8859-1 character set.
     */
    public function utf7Encode($data)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_utf7_decode($data);
        if (!$ret) {
            $this->log('IMAP utf7Encode error');
        }
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     *
     * @param string $mime_encoded_text
     * @return string
     */
    public function utf8($mime_encoded_text)
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap2_utf8($mime_encoded_text);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @param $stream
     * @return bool
     */
    public function isValidStream($stream): bool
    {
        return is_a($stream, Connection::class);
    }

    /**
     * Fetch list of emails. Overrides imap2_fetch_override function
     * @param string $sequence
     * @param int $options
     * @param bool $retrieveStructure
     * @return array|mixed
     */
    protected function fetchList(string $sequence, int $options = 0, bool $retrieveStructure = false)
    {
        if (!$this->isValidStream($this->getStream())) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $this->getStream()->getClient();

        $bodyRequest = 'BODY.PEEK[HEADER.FIELDS (SUBJECT FROM TO CC REPLYTO MESSAGEID DATE SIZE REFERENCES)]';

        $query = [
            $bodyRequest,
            'UID',
            'FLAGS',
            'INTERNALDATE',
            'RFC822.SIZE',
            'ENVELOPE',
            'RFC822.HEADER'
        ];

        if ($retrieveStructure === true) {
            $query[] = 'BODYSTRUCTURE';
        }

        $useUid = $options & FT_UID;

        $messages = $client->fetch($this->getStream()->getMailboxName(), $sequence, $useUid, $query);

        $overview = [];
        foreach ($messages as $message) {
            $messageEntry = (object)[
                'subject' => $message->envelope[1],
                'from' => Functions::writeAddressFromEnvelope($message->envelope[2]),
                'to' => $message->get('to'),
                'date' => $message->date,
                'message_id' => $message->envelope[9],
                'references' => $message->references,
                'in_reply_to' => $message->envelope[8],
                'size' => $message->size,
                'uid' => $message->uid,
                'msgno' => $message->id,
                'recent' => (int)($message->flags['RECENT'] ?? 0),
                'flagged' => (int)($message->flags['FLAGGED'] ?? 0),
                'answered' => (int)($message->flags['ANSWERED'] ?? 0),
                'deleted' => (int)($message->flags['DELETED'] ?? 0),
                'seen' => (int)($message->flags['SEEN'] ?? 0),
                'draft' => (int)($message->flags['DRAFT'] ?? 0),
                'udate' => strtotime($message->internaldate),
                'bodystructure' => $message->bodystructure ?? '',
            ];

            if (empty($messageEntry->subject)) {
                unset($messageEntry->subject);
            }

            if (empty($messageEntry->references)) {
                unset($messageEntry->references);
            }

            if (empty($messageEntry->in_reply_to)) {
                unset($messageEntry->in_reply_to);
            }

            if (empty($messageEntry->to)) {
                unset($messageEntry->to);
            }

            $indexField = (int)$message->id;
            if ($useUid) {
                $indexField = (int)$message->uid;
            }

            $overview[$indexField] = $messageEntry;
        }

        return $overview;
    }

    /**
     * Fetch list of emails. Overrides imap2_fetch_override function
     * @param string $sequence
     * @param int $options
     * @return array|mixed
     */
    protected function fetchListWithStructure(string $sequence, int $options = 0)
    {
        return $this->fetchList($sequence, $options, true);
    }

    /**
     * @param string $field
     * @param int $start
     * @param int $end
     * @param $messageSet
     * @param bool $returnUid
     * @return array|false
     */
    protected function getSortedMessageIds(
        string $field = 'ARRIVAL',
        int $start = 1,
        int $end = 1,
        $messageSet = null,
        bool $returnUid = true
    ) {
        global $sugar_config;

        if (!$this->isValidStream($this->getStream())) {
            return [];
        }
        $client = $this->getStream()->getClient();

        if ($messageSet === null && $client->getCapability('SORT') !== false) {

            $result = $client->sort($this->getStream()->getMailboxName(), 'REVERSE ' . $field, "$start:$end",
                $returnUid, 'UTF-8');

            if (empty($result) || empty($result->count())) {
                return [];
            }
            $ids = $result->get();

        } else {
            $ids = $this->getAllIdsSorted($field, $returnUid, $messageSet);

            if (!empty($ids)) {

                $ids = array_slice($ids, $start - 1, $sugar_config['list_max_entries_per_page'] ?? 10);
            }
        }

        return $ids;
    }

    /**
     * @inheritDoc
     */
    public function getMessageList(
        ?string $filterCriteria,
        $sortCriteria,
        $sortOrder,
        int $offset,
        int $pageSize,
        array &$mailboxInfo,
        array $columns,
        string $auth_type
    ): array {

        $uids = null;

        if (!empty($filterCriteria)) {
            // Filtered case and other sorting cases
            // Returns an array of msgno's which are sorted and filtered
            $emailSortedHeaders = $this->search(
                $filterCriteria,
                SE_UID,
            );

            if ($emailSortedHeaders === false) {
                return [];
            }

            $lastSequenceNumber = $mailboxInfo['Nmsgs'] = is_countable($emailSortedHeaders) ? count($emailSortedHeaders) : 0;

            // paginate
            if ($offset === "end") {
                $offset = $lastSequenceNumber - $pageSize;
            } elseif ($offset <= 0) {
                $offset = 0;
            }

            $pageOffSet = $offset + 1;
            $pageLast = $pageOffSet + $pageSize;

            if (!empty($emailSortedHeaders) && is_array($emailSortedHeaders)) {
                $uids = $this->getSortedMessageIds('ARRIVAL', $pageOffSet, $pageLast, $emailSortedHeaders);
            }



            if (empty($uids)) {
                return [];
            }
        }

        $shouldFetchAttachments = isset($columns['has_attachment']) && !empty($columns['has_attachment']);

        $fetchMethod = 'fetchList';
        if ($shouldFetchAttachments === true) {
            $fetchMethod = 'fetchListWithStructure';
        }

        $emailHeaders = [];
        if ($uids === null) {

            $totalMsgs = $this->getNumberOfMessages();
            $mailboxInfo['Nmsgs'] = $totalMsgs;

            if ($offset > (int)$totalMsgs) {
                $offset = 0;
            }

            $pageOffSet = $offset + 1;
            $pageLast = $pageOffSet + $pageSize;
            $sequence = "$pageOffSet:$pageLast";

            if ($auth_type === 'basic'){
                $sorteUids = $this->getSortedMessageIds('ARRIVAL', $pageOffSet, $pageLast, '');
            } else {
                $sorteUids = $this->getSortedMessageIds('ARRIVAL', $pageOffSet, $pageLast);
            }

            $sequence = implode(',', $sorteUids);

            $mailList = $this->$fetchMethod($sequence, FT_UID);

            $emailHeaders = [];
            foreach ($sorteUids as $id) {
                $emailHeaders[] = $mailList[(int)$id];
            }

        } elseif (is_array($uids)) {

            $mailList = $this->$fetchMethod(implode(',', $uids), FT_UID);

            $emailHeaders = [];
            foreach ($uids as $id) {
                $emailHeaders[] = $mailList[(int)$id];
            }
        }

        $emailHeaders = json_decode(json_encode($emailHeaders), true);


        if ($shouldFetchAttachments === true) {
            // get attachment status
            foreach ($emailHeaders as $i => $emailHeader) {

                $emailHeaders[$i]['has_attachment'] = false;
                if (empty($emailHeader['bodystructure']) || !is_array($emailHeader['bodystructure'])) {
                    continue;
                }

                if (!is_array($emailHeader['bodystructure'][0])) {
                    continue;
                }

                $emailHeaders[$i]['has_attachment'] = $this->bodyStructureHasAttachment($emailHeader['bodystructure']);
            }
        }

        return $emailHeaders;
    }


    /**
     * Check if body structure has attachment
     * @param $structure
     * @return bool
     */
    protected function bodyStructureHasAttachment($structure): bool
    {

        if (empty($structure) || !is_array($structure)) {
            return false;
        }

        foreach ($structure as $item) {
            if (!is_array($item)) {
                continue;
            }

            if ($this->isAttachmentPart($item) === true) {
                return true;
            }
        }

        return false;
    }

    /**
     * check if it is an attachment part
     * @param array $item
     * @return bool
     */
    protected function isAttachmentPart(array $item): bool
    {
        foreach ($item as $subItem) {
            if (!is_array($subItem)) {
                continue;
            }
            $subItemType = $subItem[0] ?? '';

            if ($subItemType === 'attachment') {
                return true;
            }
        }

        return false;
    }

    /**
     * Get sorted ids by specified field
     * @param string $field
     * @param bool $returnUid
     * @param null $messageSet
     * @return array
     */
    protected function getAllIdsSorted(string $field = 'ARRIVAL', bool $returnUid = true, $messageSet = null): array
    {
        $client = $this->getStream()->getClient();

        if (empty($messageSet)) {
            $messageSet = '1:*';
        }

        $result = $client->index($this->getStream()->getMailboxName(), $messageSet, $field, true, $returnUid,
            $returnUid);

        if (empty($result) || empty($result->count())) {
            return [];
        }

        $ids = $result->get();

        return array_reverse($ids);
    }

}
