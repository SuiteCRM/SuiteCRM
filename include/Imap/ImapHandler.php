<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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


require_once __DIR__ . '/ImapHandlerInterface.php';

/**
 * ImapHandler
 * Wrapper class for functions of IMAP PHP built in extension.
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class ImapHandler implements ImapHandlerInterface
{

    /**
     *
     * @var LoggerManager
     */
    protected $logger;

    /**
     *
     * @var resource|boolean
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
     * @param resource $stream
     * @param bool $validate
     */
    protected function setStream($stream, $validate = true)
    {
        if ($validate && !is_resource($stream)) {
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
        if ($validate && !is_resource($this->stream)) {
            $this->logger->warn('ImapHandler trying to use a non valid resource stream.');
        }

        return $this->stream;
    }

    /**
     *
     * @param array|string $errors
     */
    protected function log($errors)
    {
        if (is_string($errors)) {
            $this->log([$errors]);
        } elseif ($errors && $this->logErrors) {
            foreach ($errors as $error) {
                if ($error) {
                    $this->logger->warn('An Imap error detected: ' . json_encode($error));
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
            $this->logger->debug('IMAP wrapper called: ' . self::class . "::$func(" .  json_encode($args) . ')');
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
        if (!$ret = imap_close($this->getStream())) {
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
        $ret = imap_alerts();
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
        $ret = imap_errors();
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
        $ret = imap_last_error();
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
        $ret = imap_getmailboxes($this->getStream(), $ref, $pattern);
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
        $ret = function_exists('imap_open') && function_exists('imap_timeout');
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

        // TODO: it makes a php notice, should be fixed on a way like this:
        // $stream = false;
        // if ($username) {
        //     $stream = @imap_open($mailbox, $username, $password, $options, $n_retries, $params);
        // } else {
        //     LoggerManager::getLogger()->error('Trying to connect to an IMAP server without username.');
        // }
        // if (!$stream) {
        //     LoggerManager::getLogger()->warn('Unable to connecting and get a stream to IMAP server.');
        // }
        // $this->setStream($stream);

        $this->setStream(@imap_open($mailbox, $username, $password, $options, $n_retries, $params));


        if (!$this->getStream()) {
            $this->log('IMAP open error');
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
        $ret = imap_ping($this->getStream());
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
        $ret = imap_reopen($this->getStream(), $mailbox, $options, $n_retries);
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
        $ret = imap_timeout($timeout_type, $timeout);
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
    protected function executeImapCmd($callback, $charset=null)
    {

      // Default to class charset if none is specified
        $emailCharset = !empty($charset) ? $charset : $this->charset;

        $ret = false;

        try {
            $ret = $callback($emailCharset);

            // catch if we have BADCHARSET as exception is not thrown
            if (empty($ret) || $ret === false){
                $err = imap_last_error();
                if (strpos($err, 'BADCHARSET')) {
                    imap_errors();
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

        $call = function($charset) use ($criteria, $reverse, $options, $search_criteria){
          return imap_sort($this->getStream(), $criteria, $reverse, $options, $search_criteria, $charset);
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
        $ret = imap_msgno($this->getStream(), $uid);
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
        $ret = imap_headerinfo($this->getStream(), $msg_number, $fromLength, $subjectLength, $defaultHost);
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
        $ret = imap_fetchheader($this->getStream(), $msg_number, $options);
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
            $ret = imap_append($this->getStream(), $mailbox, $message, $options);
        } else {
            $ret = imap_append($this->getStream(), $mailbox, $message, $options, $internal_date);
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
        $ret = imap_uid($this->getStream(), $msg_number);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @return bool
     */
    public function expunge()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap_expunge($this->getStream());
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
        $ret = imap_check($this->getStream());
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
        $ret = imap_clearflag_full($this->getStream(), $sequence, $flag, $options);
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
        $ret = imap_createmailbox($this->getStream(), $mailbox);
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
        $ret = imap_delete($this->getStream(), $msg_number, $options);
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
        $ret = imap_deletemailbox($this->getStream(), $mailbox);
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
        $ret = imap_fetchbody($this->getStream(), $msg_number, $section, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
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
        $ret = imap_fetch_overview($this->getStream(), $sequence, $options);
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
        $ret = imap_fetchstructure($this->getStream(), $msg_number, $options);
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
        $ret = imap_body($this->getStream(), $msg_number, $options);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @return int|bool Return the number of messages in the current mailbox, as an integer, or FALSE on error.
     */
    public function getNumberOfMessages()
    {
        $this->logCall(__FUNCTION__, func_get_args());
        $ret = imap_num_msg($this->getStream());
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
        $ret = imap_status($this->getStream(), $mailbox, $options);
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
        $ret = imap_mail_copy($this->getStream(), $msgList, $mailbox, $options);
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
        $ret = imap_mail_move($this->getStream(), $msgList, $mailbox, $options);
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
        $ret = imap_mime_header_decode($text);
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
        $ret = imap_renamemailbox($this->getStream(), $old_mbox, $new_mbox);
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
        $ret = imap_rfc822_parse_headers($headers, $defaultHost);
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

        $call = function($charset) use ($criteria, $options){
          return imap_search($this->getStream(), $criteria, $options, $charset);
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
        $ret = imap_setflag_full($this->getStream(), $sequence, $flag, $options);
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
        $ret = imap_subscribe($this->getStream(), $mailbox);
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
        $ret = imap_unsubscribe($this->getStream(), $mailbox);
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
        $ret = imap_utf7_decode($data);
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
        $ret = imap_utf8($mime_encoded_text);
        $this->logReturn(__FUNCTION__, $ret);

        return $ret;
    }

    /**
     * @param $stream
     * @return bool
     */
    public function isValidStream($stream): bool
    {
        return is_resource($stream);
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
        $auth_type
    ): array {

        if (empty($filterCriteria) && $sortCriteria === SORTDATE) {
            // Performance fix when no filters are enabled
            $totalMsgs = $this->getNumberOfMessages();
            $mailboxInfo['Nmsgs'] = $totalMsgs;

            if ($sortOrder === 0) {
                // Ascending order
                if ($offset === "end") {
                    $firstMsg = $totalMsgs - (int)$pageSize;
                    $lastMsg = $totalMsgs;
                } elseif ($offset <= 0) {
                    $firstMsg = 1;
                    $lastMsg = $firstMsg + (int)$pageSize;
                } else {
                    $firstMsg = (int)$offset;
                    $lastMsg = $firstMsg + (int)$pageSize;
                }
            } else {
                // Descending order
                if ($offset === "end") {
                    $firstMsg = 1;
                    $lastMsg = $firstMsg + (int)$pageSize;
                } elseif ($offset <= 0) {
                    $firstMsg = $totalMsgs - (int)$pageSize;
                    $lastMsg = $totalMsgs;
                } else {
                    $offset = ($totalMsgs - (int)$offset) - (int)$pageSize;
                    $firstMsg = $offset;
                    $lastMsg = $firstMsg + (int)$pageSize;
                }
            }
            $firstMsg = $firstMsg < 1 ? 1 : $firstMsg;
            $firstMsg = $firstMsg > $totalMsgs ? $totalMsgs : $firstMsg;
            $lastMsg = $lastMsg < $firstMsg ? $firstMsg : $lastMsg;
            $lastMsg = $lastMsg > $totalMsgs ? $totalMsgs : $lastMsg;

            $sequence = $firstMsg . ':' . $lastMsg;
            $emailSortedHeaders = $this->fetchOverview($sequence);

            $uids = [];
            if (!empty($emailSortedHeaders)) {
                $uids = array_map(
                    function ($x) {
                        return $x->uid;
                    },
                    $emailSortedHeaders // TODO: this should be an array!
                );
            }

        } else {
            // Filtered case and other sorting cases
            // Returns an array of msgno's which are sorted and filtered
            $emailSortedHeaders = $this->sort(
                $sortCriteria,
                $sortOrder,
                SE_UID,
                $filterCriteria
            );

            if ($emailSortedHeaders === false) {
                return [];
            }

            $uids = array_slice($emailSortedHeaders, $offset, $pageSize);

            $lastSequenceNumber = $mailboxInfo['Nmsgs'] = count($emailSortedHeaders);

            // paginate
            if ($offset === "end") {
                $offset = $lastSequenceNumber - $pageSize;
            } elseif ($offset <= 0) {
                $offset = 0;
            }
        }

        if (empty($uids)) {
            return [];
        }


        // TODO: uids could be invalid for implode!
        $uids = implode(',', $uids);

        // Get result
        $emailHeaders = $this->fetchOverview(
            $uids,
            FT_UID
        );
        $emailHeaders = json_decode(json_encode($emailHeaders), true);
        if (isset($columns['has_attachment'])) {
            // get attachment status
            foreach ($emailHeaders as $i => $emailHeader) {
                $structure = $this->fetchStructure($emailHeader['uid'], FT_UID);

                $emailHeaders[$i]['has_attachment'] = $this->messageStructureHasAttachment($structure);
            }
        }

        return $emailHeaders;
    }

    /**
     * @param $structure
     * @return bool
     */
    public function messageStructureHasAttachment($structure): bool
    {
        if (($structure->type !== 0) && ($structure->type !== 1)) {
            return true;
        }


        $attachments = [];

        if (empty($structure->parts)) {
            return false;
        }

        foreach ($structure->parts as $i => $part) {
            if (empty($part) || empty($part->dparameters[0])) {
                continue;
            }

            if (is_string($part->dparameters[0]->value)) {
                $attachments[] = $part->dparameters[0]->value;
            }
        }

        return !empty($attachments);
    }
}
