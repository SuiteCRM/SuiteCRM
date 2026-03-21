<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

use SuiteCRM\Utility\SuiteValidator;

class EmailImportService
{
    public function run(): bool
    {
        require_once 'modules/InboundEmail/AOPInboundEmail.php';
        $this->log('info', '----->Scheduler fired job of type pollMonitoredInboxesAOP()');

        require_once 'modules/Configurator/Configurator.php';

        $inboundEmailRow = $this->getNextInboundEmailAccountToImport();

        if (empty($inboundEmailRow)) {
            $this->log('info', 'No Inbound Email accounts found to poll');
            return true;
        }

        $inboundEmailAccount = new AOPInboundEmail();
        $loadResult = $inboundEmailAccount->retrieve($inboundEmailRow['id']);

        if (!$loadResult || !$inboundEmailAccount->id || $inboundEmailAccount->status !== 'Active') {
            $this->log('error', "Error retrieving AOP Inbound Email: '" . $inboundEmailRow['id'] . "'");
            return false;
        }

        try {
            $this->importFromInboundEmailAccount($inboundEmailAccount);
        } catch (Exception $e) {
            $this->log('error', "Exception during import from AOP Inbound Email with id '" . $inboundEmailRow['id'] . "' : " . $e->getMessage());
            return false;
        }

        $this->updateInboundEmailAccountImportTime($inboundEmailAccount);

        return true;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @return void
     * @throws ImapHandlerException
     * @throws Exception
     */
    protected function importFromInboundEmailAccount(AOPInboundEmail $inboundEmailAccount): void
    {
        $imapHandler = $this->getImapHandler($inboundEmailAccount);

        $mailboxes = $inboundEmailAccount->mailboxarray;

        if (empty($mailboxes)) {
            $this->log('info', 'No mailboxes found for Inbound Email account [ ' . $inboundEmailAccount->name . ' ]', 'importFromMailbox');
            return;
        }

        foreach ($mailboxes as $mailbox) {
            $this->importFromMailbox($mailbox, $inboundEmailAccount);
        }

        $imapHandler->expunge();
        $imapHandler->close(CL_EXPUNGE);
    }

    /**
     * @param $mailbox
     * @param AOPInboundEmail $inboundEmailAccount
     * @return void
     * @throws Exception
     */
    protected function importFromMailbox($mailbox, AOPInboundEmail $inboundEmailAccount): void
    {
        $inboundEmailAccount->mailbox = $mailbox;

        $connectedToMailServer = $this->connectToMailServer($inboundEmailAccount);
        if (!$connectedToMailServer) {
            return;
        }

        [$msgNoToUIDL, $newMessages] = $this->getNewMessages($inboundEmailAccount, $mailbox);

        if (empty($newMessages)) {
            return;
        }

        $current = 1;
        $total = count($newMessages);

        $sugarFolder = $this->loadFolder($inboundEmailAccount);
        $importedMessages = [];

        foreach ($newMessages as $msgNo => $uid) {

            if ($inboundEmailAccount->isPop3Protocol()) {
                $uid = $msgNoToUIDL[$msgNo];
            }

            $importedMessages = $this->importMessage(
                $msgNo,
                $uid,
                $inboundEmailAccount,
                $sugarFolder,
                $importedMessages,
            );

            $this->log('debug', '***** On message [ ' . $current . ' of ' . $total . ' ] *****', 'importFromMailbox');

            $current++;
        }

        if (!empty($importedMessages)) {
            $this->deleteMessagesFromServer($inboundEmailAccount, $importedMessages);
        }
    }

    /**
     * @param string $msgNo
     * @param string $uid
     * @param AOPInboundEmail $inboundEmailAccount
     * @param SugarFolder $sugarFolder
     * @param array $importedMessages
     * @return array
     * @throws ImapHandlerException
     */
    protected function importMessage(
        string $msgNo,
        string $uid,
        AOPInboundEmail $inboundEmailAccount,
        SugarFolder $sugarFolder,
        array $importedMessages
    ): array {

        if (!empty($sugarFolder->id)) {
            $importedMessages = $this->runFolderImport($inboundEmailAccount, $msgNo, $uid, $sugarFolder, $importedMessages);
        } else {
            $this->runIndividualImport($inboundEmailAccount, $msgNo, $uid);
        }

        return $importedMessages;
    }


    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $mailbox
     * @return array|false
     * @throws ImapHandlerException
     */
    protected function getMessagesToImport(AOPInboundEmail $inboundEmailAccount, string $mailbox): array
    {

        $lastImportDateCursor = $this->getLastImportedDay($inboundEmailAccount, $mailbox);

        if (empty($lastImportDateCursor)) {
            $defaultTimeframeStart = $this->getImportTimeframeStartConfig($inboundEmailAccount);
            $lastImportDateCursor = date('Y-m-d', strtotime($defaultTimeframeStart));
        }

        $today = getdate();

        $messagesToImport = [];
        $messageLimit = $this->getMessageLimitConfig($inboundEmailAccount);

        $importDateCursor = strtotime($lastImportDateCursor);
        $todayDate = mktime(0, 0, 0, $today['mon'], $today['mday'], $today['year']);

        while (count($messagesToImport) < $messageLimit && $importDateCursor <= $todayDate) {

            $lastImportDateCursor = date('Y-m-d', $importDateCursor);

            $unImportedMessages = $this->getUnimportedMessagesForDate($inboundEmailAccount, $lastImportDateCursor, $messagesToImport);
            if (empty($unImportedMessages)) {
                $importDateCursor = strtotime('+1 day', $importDateCursor);
                continue;
            }

            $messagesToImport += $unImportedMessages;

            if ($importDateCursor < $todayDate && count($messagesToImport) < $messageLimit) {
                $importDateCursor = strtotime('+1 day', $importDateCursor);
            }
        }

        if ($importDateCursor > $todayDate) {
            $importDateCursor = $todayDate;
        }

        $lastImportDateCursor = date('Y-m-d', $importDateCursor);

        $this->setLastImportedDay($inboundEmailAccount, $mailbox, $lastImportDateCursor);

        return $messagesToImport;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $lastImportDate
     * @param array $messagesToImport
     * @return array
     * @throws ImapHandlerException
     */
    protected function getUnimportedMessagesForDate(AOPInboundEmail $inboundEmailAccount, string $lastImportDate, array $messagesToImport): array
    {
        $unSeenOnly = $this->getUnreadOnlyConfig($inboundEmailAccount);
        $messagesForDate = $inboundEmailAccount->getMessagesFromDate($lastImportDate, $unSeenOnly);

        $messageUids = [];
        foreach ($messagesForDate as $k => $messageNumber) {
            $messageUids[$messageNumber] = $this->getImapHandler($inboundEmailAccount)->getUid($messageNumber);
        }

        $unimportedMessages = $this->getUnimportedMessages($messageUids, $inboundEmailAccount->id);

        return array_diff($unimportedMessages, $messagesToImport);
    }

    /**
     * @param array $messageUids
     * @param string $mailboxId
     * @return array
     */
    protected function getUnimportedMessages(array $messageUids, string $mailboxId): array
    {
        if (empty($messageUids)) {
            return [];
        }

        $db = $this->getDBManager();

        $quotedMessageUids = [];
        foreach ($messageUids as $messageUid) {
            $quotedMessageUids[] = $db->quote($messageUid);
        }
        $inClause = "'" . implode("','", $quotedMessageUids) . "'";

        $quotedMailboxId = $db->quote($mailboxId);
        $query = "SELECT uid FROM emails WHERE uid IN ({$inClause}) AND mailbox_id = '$quotedMailboxId' AND deleted = 0";
        $result = $db->query($query);

        $importedUids = [];
        $row = $db->fetchByAssoc($result);
        while (!empty($row)) {
            $importedUids[] = $row['uid'];
            $row = $db->fetchByAssoc($result);
        }

        return array_diff($messageUids, $importedUids);
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param array $messagesToDelete
     * @return void
     */
    protected function deleteMessagesFromServer(AOPInboundEmail $inboundEmailAccount, array $messagesToDelete): void
    {
        global $app_strings;

        $leaveMessagesOnMailServer = $inboundEmailAccount->get_stored_options("leaveMessagesOnMailServer", 0);
        if (!$leaveMessagesOnMailServer) {
            if ($inboundEmailAccount->isPop3Protocol()) {
                $inboundEmailAccount->deleteMessageOnMailServerForPop3(implode(",", $messagesToDelete));
            } else {
                $inboundEmailAccount->deleteMessageOnMailServer(implode($app_strings['LBL_EMAIL_DELIMITER'] ?? '', $messagesToDelete));
            }
        }
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @return SugarFolder
     */
    protected function loadFolder(AOPInboundEmail $inboundEmailAccount): SugarFolder
    {
        require_once "include/SugarFolders/SugarFolders.php";
        $sugarFolder = new SugarFolder();
        $groupFolderId = $inboundEmailAccount->groupfolder_id;

        if ($groupFolderId !== null && $groupFolderId !== "") {
            $sugarFolder->retrieve($groupFolderId);
            if (empty($sugarFolder->id)) {
                $sugarFolder->retrieve($inboundEmailAccount->id);
            }
        }

        return $sugarFolder;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $msgNo
     * @param string $uid
     * @param SugarFolder $sugarFolder
     * @param array $importedMessages
     * @return array
     */
    protected function runFolderImport(
        AOPInboundEmail $inboundEmailAccount,
        string $msgNo,
        string $uid,
        SugarFolder $sugarFolder,
        array $importedMessages
    ): array {
        $emailId = $inboundEmailAccount->returnImportedEmail(
            $msgNo,
            $uid,
            false,
            true,
            true
        );

        if (empty($emailId)) {
            return $importedMessages;
        }

        $sugarFolder->addBean($inboundEmailAccount);

        if ($inboundEmailAccount->isPop3Protocol()) {
            $importedMessages[] = $msgNo;
        } else {
            $importedMessages[] = $uid;
        }

        $this->handleCaseCreate($inboundEmailAccount, $emailId);

        return $importedMessages;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $msgNo
     * @param string $uid
     * @throws ImapHandlerException
     */
    protected function runIndividualImport(AOPInboundEmail $inboundEmailAccount, string $msgNo, string $uid): void
    {
        if ($inboundEmailAccount->isAutoImport()) {
            $inboundEmailAccount->returnImportedEmail($msgNo, $uid);
            return;
        }

        /*If the group folder doesn't exist then download only those messages
         which has caseid in message*/

        $inboundEmailAccount->getMessagesInEmailCache($msgNo, $uid);
        $email = BeanFactory::newBean('Emails');
        $header = $this->getImapHandler($inboundEmailAccount)->getHeaderInfo($msgNo);
        $email->name = $inboundEmailAccount->handleMimeHeaderDecode($header->subject);
        $email->from_addr = $inboundEmailAccount->convertImapToSugarEmailAddress($header->from);
        isValidEmailAddress($email->from_addr);

        $email->reply_to_email = $inboundEmailAccount->convertImapToSugarEmailAddress($header->reply_to);

        if (!empty($email->reply_to_email)) {
            $contactAddr = $email->reply_to_email;
            isValidEmailAddress($contactAddr);
        } else {
            $contactAddr = $email->from_addr;
            isValidEmailAddress($contactAddr);
        }

        $inboundEmailAccount->handleAutoresponse($email, $contactAddr);
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param $emailId
     */
    protected function handleCaseCreate(AOPInboundEmail $inboundEmailAccount, $emailId): void
    {
        if (!$inboundEmailAccount->isMailBoxTypeCreateCase()) {
            return;
        }

        require_once 'modules/AOP_Case_Updates/AOPAssignManager.php';
        $assignManager = new AOPAssignManager($inboundEmailAccount);

        $userId = $assignManager->getNextAssignedUser();
        $this->log('debug', 'userId [ ' . $userId . ' ]', 'handleCaseCreate');
        $validator = new SuiteValidator();

        if (
            (
                !isset($inboundEmailAccount->email) ||
                !$inboundEmailAccount->email ||
                !isset($inboundEmailAccount->email->id)
                || !$inboundEmailAccount->email->id
            ) &&
            $validator->isValidId($emailId)
        ) {
            $inboundEmailAccount->email = BeanFactory::newBean('Emails');
            if (!$inboundEmailAccount->email->retrieve($emailId)) {
                throw new RuntimeException('Email retrieving error to handle case create, email id was: ' . $emailId);
            }
        }

        if (empty($inboundEmailAccount->email)) {
            throw new RuntimeException('Invalid type for email id ' . $emailId);
        }

        $inboundEmailAccount->handleCreateCase($inboundEmailAccount->email, $userId);
    }

    /**
     * @param string $level
     * @param string $message
     * @param string $section
     * @return void
     */
    protected function log(string $level, string $message, string $section = ''): void
    {
        $logMessage = '[PollMonitoredEmailInboxes]';
        if (!empty($section)) {
            $logMessage .= '[' . $section . ']';
        }

        $logMessage .= ' ' . $message;

        $GLOBALS['log']->$level($logMessage);
    }

    /**
     * @return array|null
     */
    protected function getNextInboundEmailAccountToImport(): ?array
    {
        $db = $this->getDBManager();

        $sqlQueryResult = $db->query(
            "
             SELECT id, name, last_import_run_datetime
             FROM inbound_email
             WHERE is_personal = 0
               AND deleted=0
               AND status='Active'
               AND mailbox_type != 'bounce'
             ORDER BY last_import_run_datetime ASC
             "
        );

        $result = $db->fetchByAssoc($sqlQueryResult);
        if (empty($result)) {
            return [];
        }

        return $result;
    }

    protected function updateInboundEmailAccountImportTime(AOPInboundEmail $inboundEmailAccount): void
    {
        $inboundEmailAccount->last_import_run_datetime = TimeDate::getInstance()->nowDb();
        $inboundEmailAccount->save();
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @return ImapHandlerInterface
     * @throws ImapHandlerException
     */
    protected function getImapHandler(AOPInboundEmail $inboundEmailAccount): ImapHandlerInterface
    {
        $imapHandler = $inboundEmailAccount->getImap();

        if ($imapHandler === null) {
            throw new RuntimeException("Could not connect to mail server for Inbound Email account with id '" . $inboundEmailAccount->id . "'");
        }

        return $imapHandler;
    }

    protected function connectToMailServer(AOPInboundEmail $inboundEmailAccount): bool
    {
        $this->log('debug', 'Trying to connect to mailserver for [ ' . $inboundEmailAccount->name . ' ]', 'importFromMailbox');

        $connected = isTrue($inboundEmailAccount->connectMailServer());
        if (!$connected) {
            $this->log('fatal', "could not get an IMAP connection resource for ID [ {$inboundEmailAccount->id} ]. Skipping mailbox [ {$inboundEmailAccount->name} ].", 'importFromMailbox');
            return false;
        }

        return true;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $mailbox
     * @return array
     * @throws ImapHandlerException
     */
    protected function getNewMessages(AOPInboundEmail $inboundEmailAccount, string $mailbox): array
    {
        $newMessages = [];
        $msgNoToUIDL = [];

        if ($inboundEmailAccount->isPop3Protocol()) {
            $msgNoToUIDL = $inboundEmailAccount->getPop3NewMessagesToDownloadForCron();
            // get all the keys which are msgnos;
            $newMessages = array_keys($msgNoToUIDL);
        }

        if (!$inboundEmailAccount->isPop3Protocol()) {
            $newMessages = $this->getMessagesToImport($inboundEmailAccount, $mailbox);
        }

        if (empty($newMessages)) {
            $this->log('debug', 'No new messages to import for [ ' . $inboundEmailAccount->name . ' ]', 'importFromMailbox');
        }

        return array($msgNoToUIDL, $newMessages);
    }

    /**
     * @return DBManager|object
     */
    protected function getDBManager()
    {
        $db = DBManagerFactory::getInstance();
        if ($db === null) {
            $this->log('error', 'Database connection error');
            throw new RuntimeException('Database connection error');
        }
        return $db;
    }

    protected function getUnreadOnlyConfig(AOPInboundEmail $inboundEmailAccount): bool
    {
        $unreadOnly = $inboundEmailAccount->email_import_unread_only ?? null;
        if ($unreadOnly !== null && $unreadOnly !== '') {
            return isTrue($unreadOnly);
        }

        $configurator = new Configurator();
        $configurator->loadConfig();

        $globalUnreadOnly = $configurator->config['email_import_fetch_unread_only'] ?? true;

        return isTrue($globalUnreadOnly);
    }

    protected function getMessageLimitConfig(AOPInboundEmail $inboundEmailAccount): int
    {
        $threshold = $inboundEmailAccount->email_import_per_run_threshold ?? null;
        if (is_numeric($threshold) && $threshold > 0) {
            return (int)$threshold;
        }

        $configurator = new Configurator();
        $configurator->loadConfig();
        if (
            isset($configurator->config['email_import_per_run_threshold']) &&
            is_numeric($configurator->config['email_import_per_run_threshold']) &&
            $configurator->config['email_import_per_run_threshold'] > 0
        ) {
            return (int)$configurator->config['email_import_per_run_threshold'];
        }

        return 25;
    }

    protected function getImportTimeframeStartConfig(AOPInboundEmail $inboundEmailAccount): string
    {
        $timeframeStart = $inboundEmailAccount->email_import_timeframe_start ?? null;
        if (is_string($timeframeStart) && preg_match('/^-\s*\d+\s+(days|day|months|month|years|year)$/i', trim($timeframeStart))) {
            return $timeframeStart;
        }

        $configurator = new Configurator();
        $configurator->loadConfig();

        $timeframeStart = $configurator->config['email_import_timeframe_start'] ?? '';
        $timeframeStart = is_string($timeframeStart) ? trim($timeframeStart) : '';
        $default = '-30 days';

        if ($timeframeStart === '') {
            return $default;
        }

        if (preg_match('/^-\s*\d+\s+(days|day|months|month|years|year)$/i', $timeframeStart)) {
            return $timeframeStart;
        }

        return $default;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $mailbox
     * @return string
     */
    protected function getLastImportedDay(AOPInboundEmail $inboundEmailAccount, string $mailbox): string
    {
        if (empty($inboundEmailAccount->mailbox_last_imported_days)) {
            return '';
        }

        try {
            $mailboxesLastImportedDay = json_decode(html_entity_decode($inboundEmailAccount->mailbox_last_imported_days), true, 512, JSON_THROW_ON_ERROR);
        } catch (JsonException $exception) {
            return '';
        }

        if (!is_array($mailboxesLastImportedDay)) {
            return '';
        }

        $lastImportedDay = $mailboxesLastImportedDay[$mailbox] ?? '';
        if (empty($lastImportedDay)) {
            return '';
        }

        return $lastImportedDay;
    }

    /**
     * @param AOPInboundEmail $inboundEmailAccount
     * @param string $mailbox
     * @param string $lastImportedDate
     * @return void
     */
    protected function setLastImportedDay(AOPInboundEmail $inboundEmailAccount, string $mailbox, string $lastImportedDate): void
    {
        $mailboxesLastImportedDay = [];
        if (!empty($inboundEmailAccount->mailbox_last_imported_days)) {
            $mailboxesLastImportedDay = $inboundEmailAccount->mailbox_last_imported_days;
        }

        if (is_string($mailboxesLastImportedDay)) {
            try {
                $mailboxesLastImportedDay = json_decode(html_entity_decode($inboundEmailAccount->mailbox_last_imported_days), true, 512, JSON_THROW_ON_ERROR);
            } catch (JsonException $exception) {
                $mailboxesLastImportedDay = [];
            }
        }

        if (!is_array($mailboxesLastImportedDay)) {
            $mailboxesLastImportedDay = [];
        }

        $mailboxesLastImportedDay[$mailbox] = $lastImportedDate;

        try {
            $inboundEmailAccount->mailbox_last_imported_days = json_encode($mailboxesLastImportedDay, JSON_THROW_ON_ERROR);
        } catch (JsonException $e) {
            $inboundEmailAccount->mailbox_last_imported_days = '';
        }
    }
}