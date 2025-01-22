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

include_once __DIR__ . '/ImapHandlerInterface.php';
include_once __DIR__ . '/ImapHandlerFakeData.php';

/**
 * ImapHandlerFake
 * For tests only.
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class ImapHandlerFake implements ImapHandlerInterface
{
    protected $fakes;

    /**
     *
     * @param ImapHandlerFakeData $fakeData
     */
    public function __construct(ImapHandlerFakeData $fakeData)
    {
        $this->fakes = $fakeData;
    }

    /**
     *
     * @return boolean
     */
    public function close()
    {
        return $this->fakes->call('close');
    }

    /**
     *
     * @return array
     */
    public function getAlerts()
    {
        return $this->fakes->call('getAlerts');
    }

    /**
     *
     * @return resource|boolean
     */
    public function getConnection()
    {
        return $this->fakes->call('getConnection');
    }

    /**
     *
     * @return array
     */
    public function getErrors()
    {
        return $this->fakes->call('getErrors');
    }

    /**
     *
     * @return string|boolean
     */
    public function getLastError()
    {
        return $this->fakes->call('getLastError');
    }

    /**
     *
     * @param string $ref
     * @param string $pattern
     * @return array
     */
    public function getMailboxes($ref, $pattern)
    {
        return $this->fakes->call('getMailboxes', [$ref, $pattern]);
    }

    /**
     *
     * @return boolean
     */
    public function isAvailable()
    {
        return $this->fakes->call('isAvailable');
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
        return $this->fakes->call('open', [$mailbox, $username, $password, $options, $n_retries, $params]);
    }

    /**
     *
     * @return boolean
     */
    public function ping()
    {
        return $this->fakes->call('ping');
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
        return $this->fakes->call('reopen', [$mailbox, $options, $n_retries]);
    }

    /**
     *
     * @param int $timeout_type
     * @param int $timeout
     * @return mixed
     */
    public function setTimeout($timeout_type, $timeout = -1)
    {
        return $this->fakes->call('setTimeout', [$timeout_type, $timeout]);
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
        return $this->fakes->call('sort', [$criteria, $reverse, $options, $search_criteria, $charset]);
    }

    /**
     *
     * @param int $uid
     * @return int
     */
    public function getMessageNo($uid)
    {
        return $this->fakes->call('getMessageNo', [$uid]);
    }

    /**
     *
     * @param int $msg_number
     * @param int $fromlength
     * @param int $subjectlength
     * @param string $defaulthost
     * @return bool|object Returns FALSE on error or, if successful, the information in an object
     */
    public function getHeaderInfo($msg_number, $fromlength = 0, $subjectlength = 0, $defaulthost = null)
    {
        return $this->fakes->call('getHeaderInfo', [$msg_number, $fromlength, $subjectlength, $defaulthost]);
    }

    /**
     *
     * @param type $msg_number
     * @param type $options
     * @return string
     */
    public function fetchHeader($msg_number, $options = 0)
    {
        return $this->fakes->call('fetchHeader', [$msg_number, $options]);
    }

    /**
     *
     * @param string $mailbox
     * @param string $message
     * @param string $options
     * @param string $internal_date
     */
    public function append($mailbox, $message, $options = null, $internal_date = null)
    {
        return $this->fakes->call('append', [$mailbox, $message, $options, $internal_date]);
    }

    /**
     *
     * @param int $msg_number
     * @return int
     */
    public function getUid($msg_number)
    {
        return $this->fakes->call('getUid', [$msg_number]);
    }

    /**
     * @return bool
     */
    public function expunge()
    {
        return $this->fakes->call('expunge', []);
    }


    /**
     * @return object|bool Returns FALSE on failure.
     */
    public function check()
    {
        return $this->fakes->call('check', []);
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
        return $this->fakes->call('clearFlagFull', [$sequence, $flag, $options]);
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function createMailbox($mailbox)
    {
        return $this->fakes->call('createMailbox', [$mailbox]);
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return bool Returns TRUE.
     */
    public function delete($msg_number, $options = 0)
    {
        return $this->fakes->call('delete', [$msg_number, $options]);
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function deleteMailbox($mailbox)
    {
        return $this->fakes->call('deleteMailbox', [$mailbox]);
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
        return $this->fakes->call('fetchBody', [$msg_number, $section, $options]);
    }

    /**
     *
     * @param string $sequence
     * @param int $options
     * @return array
     */
    public function fetchOverview($sequence, $options = 0)
    {
        return $this->fakes->call('fetchOverview', [$sequence, $options]);
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return object
     */
    public function fetchStructure($msg_number, $options = 0)
    {
        return $this->fakes->call('fetchStructure', [$msg_number, $options]);
    }

    /**
     *
     * @param int $msg_number
     * @param int $options
     * @return string
     */
    public function getBody($msg_number, $options)
    {
        return $this->fakes->call('getBody', [$msg_number, $options]);
    }

    /**
     * @return int|bool Return the number of messages in the current mailbox, as an integer, or FALSE on error.
     */
    public function getNumberOfMessages()
    {
        return $this->fakes->call('getNumberOfMessages', []);
    }

    /**
     *
     * @param string $mailbox
     * @param int $options
     * @return object
     */
    public function getStatus($mailbox, $options)
    {
        return $this->fakes->call('getStatus', [$mailbox, $options]);
    }

    /**
     *
     * @param string $msglist
     * @param string $mailbox
     * @param int $options
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function mailCopy($msglist, $mailbox, $options = 0)
    {
        return $this->fakes->call('mailCopy', [$msglist, $mailbox, $options]);
    }

    /**
     *
     * @param string $msglist
     * @param string $mailbox
     * @param int $options
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function mailMove($msglist, $mailbox, $options = 0)
    {
        return $this->fakes->call('mailMove', [$msglist, $mailbox, $options]);
    }

    /**
     *
     * @param string $text
     * @return array
     */
    public function mimeHeaderDecode($text)
    {
        return $this->fakes->call('mimeHeaderDecode', [$text]);
    }

    /**
     *
     * @param string $old_mbox
     * @param string $new_mbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function renameMailbox($old_mbox, $new_mbox)
    {
        return $this->fakes->call('renameMailbox', [$old_mbox, $new_mbox]);
    }

    /**
     *
     * @param string $headers
     * @param string $defaulthost
     * @return object
     */
    public function rfc822ParseHeaders($headers, $defaulthost = "UNKNOWN")
    {
        return $this->fakes->call('rfc822ParseHeaders', [$headers, $defaulthost]);
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
        return $this->fakes->call('search', [$criteria, $options, $charset]);
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
        return $this->fakes->call('setFlagFull', [$sequence, $flag, $options]);
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function subscribe($mailbox)
    {
        return $this->fakes->call('subscribe', [$mailbox]);
    }

    /**
     *
     * @param string $mailbox
     * @return bool Returns TRUE on success or FALSE on failure.
     */
    public function unsubscribe($mailbox)
    {
        return $this->fakes->call('unsubscribe', [$mailbox]);
    }

    /**
     *
     * @param string $data
     * @return string|bool FALSE if text contains invalid modified UTF-7 sequence or text contains a character that is not part of ISO-8859-1 character set.
     */
    public function utf7Encode($data)
    {
        return $this->fakes->call('utf7Encode', [$data]);
    }

    /**
     *
     * @param string $mime_encoded_text
     * @return string
     */
    public function utf8($mime_encoded_text)
    {
        return $this->fakes->call('utf8', [$mime_encoded_text]);
    }

    /**
     * @param $stream
     * @return bool
     * @throws Exception
     */
    public function isValidStream($stream): bool
    {
        return $this->fakes->call('isValidStream', $stream);
    }

    /**
     * @param string|null $filterCriteria
     * @param $sortCriteria
     * @param $sortOrder
     * @param int $offset
     * @param int $pageSize
     * @param array $mailboxInfo
     * @param array $columns
     * @return array
     * @throws Exception
     */
    public function getMessageList(?string $filterCriteria, $sortCriteria, $sortOrder, int $offset, int $pageSize, array &$mailboxInfo, array $columns): array
    {
        return $this->fakes->call('getMessageList', [$filterCriteria, $sortCriteria, $sortOrder, $offset, $pageSize, $mailboxInfo, $columns]);
    }
}
