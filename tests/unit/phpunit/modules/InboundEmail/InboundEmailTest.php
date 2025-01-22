<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

include_once __DIR__ . '/../../../../../include/Imap/ImapHandlerFakeData.php';
include_once __DIR__ . '/../../../../../include/Imap/ImapHandlerFake.php';
require_once __DIR__ . '/../../../../../modules/InboundEmail/InboundEmail.php';

/**
 * Like tempfile() but takes a mode
 *
 * @param $mode
 * @return bool|resource
 */
function tempFileWithMode($mode)
{
    $path = tempnam(sys_get_temp_dir(), '');
    $file = fopen($path, $mode);
    register_shutdown_function(function() use($path) {
        if (file_exists($path)) {
            unlink($path);
        }
    });
    return $file;
}

class InboundEmailTest extends SuitePHPUnitFrameworkTestCase
{
    // ---------------------------------------------
    // ----- FOLLOWIN TESTS ARE USING FAKE IMAP ----
    // ------------------------------------------------->

    /**
     * @var resource
     */
    protected $connection;

    public function setUp(): void
    {
        parent::setUp();

        $this->connection = tempFileWithMode('wb+');
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'InboundEmail');
    }

    public function testConnectMailServerFolderInboundForceFirstMailbox(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('getConnection', null, [function () {
            return $this->connection;
        }]);
        $fake->add('close', null, [null]);
        $fake->add('ping', null, [true]);
        $fake->add('reopen', ['{:143/service=imap}first', 32768, 0], [true]);
        $fake->add('isValidStream', $this->connection, [true]);
        $imap = new ImapHandlerFake($fake);
        $ie = new InboundEmail($imap);
        $_REQUEST['folder'] = 'inbound';
        $_REQUEST['folder_name'] = [];
        $ie->mailboxarray = ['first'];
        $ret = $ie->connectMailserver(false, true);
        self::assertEquals('true', $ret);
    }

    public function testConnectMailServerFolderInboundForceTestFolder(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('getConnection', null, [function () {
            return $this->connection;
        }]);
        $fake->add('close', null, [null]);
        $fake->add('ping', null, [true]);
        $fake->add('reopen', ['{:143/service=imap}test', 32768, 0], [true]);
        $fake->add('isValidStream', $this->connection, [true]);
        $imap = new ImapHandlerFake($fake);
        $ie = new InboundEmail($imap);
        $_REQUEST['folder'] = 'inbound';
        $_REQUEST['folder_name'] = 'test';
        $ret = $ie->connectMailserver(false, true);
        self::assertEquals('true', $ret);

    }

    public function testConnectMailServerFolderInboundForce(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('getConnection', null, [function () {
            return $this->connection;
        }]);
        $fake->add('close', null, [null]);
        $fake->add('ping', null, [true]);
        $fake->add('reopen', ['{:143/service=imap}INBOX', 32768, 0], [true]);
        $fake->add('isValidStream', $this->connection, [true]);
        $imap = new ImapHandlerFake($fake);
        $ie = new InboundEmail($imap);
        $_REQUEST['folder'] = 'inbound';
        $ret = $ie->connectMailserver(false, true);
        self::assertEquals('true', $ret);
    }

    public function testConnectMailServerFolderSentForce(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('close', null, [null]);
        $fake->add('ping', null, [true]);
        $fake->add('reopen', ['{:143/service=imap}', 32768, 0], [true]);
        $fake->add('isValidStream', $this->connection, [true]);
        $imap = new ImapHandlerFake($fake);
        $ie = new InboundEmail($imap);
        $_REQUEST['folder'] = 'sent';
        $ret = $ie->connectMailserver(false, true);
        self::assertEquals('true', $ret);
    }

    public function testConnectMailserverNoGood(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('setTimeout', [1, 15], [true]);
        $fake->add('setTimeout', [2, 15], [true]);
        $fake->add('setTimeout', [3, 15], [true]);
        $fake->add('open', ["{:143/service=imap/ssl/tls/validate-cert/secure}", null, null, 0, 0, []], [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getLastError', null, ['Too many login failures']);
        $fake->add('getAlerts', null, [null]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/ssl/tls/validate-cert/secure}', '*'], [[]]);
        $fake->add('close', null, [null]);
        $fake->add('isValidStream', $this->connection, [true]);
        $imap = new ImapHandlerFake($fake);

        $_REQUEST['ssl'] = 1;

        $ret = (new InboundEmail($imap))->connectMailserver(true);
        self::assertEquals('Login or Password Incorrect', $ret);
    }

    public function testConnectMailserverUseSsl(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/notls/novalidate-cert/secure}', '*'], [[]]);
        $fake->add('ping', null, [true]);
        $fake->add('reopen', ['{:143/service=imap}', 32768, 0], [true]);
        $fake->add('isValidStream', $this->connection, [true]);
        $imap = new ImapHandlerFake($fake);

        $_REQUEST['ssl'] = 1;

        $ret = (new InboundEmail($imap))->connectMailserver();
        self::assertEquals('true', $ret);
    }

    public function testConnectMailserverNoImap(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [false]);
        $imap = new ImapHandlerFake($fake);

        $ret = (new InboundEmail($imap))->connectMailserver();
        self::assertEquals('Inbound Email <b>cannot</b> function without the IMAP c-client libraries enabled/compiled with the PHP module. Please contact your administrator to resolve this issue.', $ret);
    }

    public function testFindOptimumSettingsFalsePositive(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);  // <-- when the code calls ImapHandlerInterface::isAvailable([null]), it will return true
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('open', ["{:143/service=imap/notls/novalidate-cert/secure}", null, null, 0, 0, []], [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getLastError', null, ["SECURITY PROBLEM: insecure server advertised AUTH=PLAIN"]);
        $fake->add('getAlerts', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/notls/novalidate-cert/secure}', '*'], [[]]);
        $fake->add('close', null, [null]);
        $fake->add('isValidStream', $this->connection, [true]);

        $exp = [
            'serial' => '::::::imap::novalidate-cert::notls::secure',
            'service' => 'foo/notls/novalidate-cert/secure',
        ];

        $imap = new ImapHandlerFake($fake);
        $inboundEmail = new InboundEmail($imap);
        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals($exp, $ret);

        // other errors also cause same results..

        $fake->add('getLastError', null, ["Mailbox is empty"]);
        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals($exp, $ret);
    }

    public function testFindOptimumSettingsFail(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);  // <-- when the code calls ImapHandlerInterface::isAvailable([null]), it will return true
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('open', ["{:143/service=imap/notls/novalidate-cert/secure}", null, null, 0, 0, []], [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getLastError', null, ['Too many login failures']);
        $fake->add('getAlerts', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/notls/novalidate-cert/secure}', '*'], [[]]);
        $fake->add('close', null, [null]);
        $fake->add('isValidStream', $this->connection, [true]);

        $exp = [
            'good' => [],
            'bad' => ['both-secure' => '{:143/service=imap/notls/novalidate-cert/secure}'],
            'err' => ['both-secure' => 'Login or Password Incorrect'],
        ];

        $imap = new ImapHandlerFake($fake);
        $inboundEmail = new InboundEmail($imap);
        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals($exp, $ret);

        // other errors also cause same results..

        $fake->add('getLastError', null, ['[CLOSED] IMAP connection broken (server response)']);
        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals($exp, $ret);

        $fake->add('getLastError', null, ['[AUTHENTICATIONFAILED]']);
        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals($exp, $ret);

        $fake->add('getLastError', null, ['something.. AUTHENTICATE .. something .. failed .. something']);
        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals($exp, $ret);
    }

    public function testFindOptimumSettingsOk(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);  // <-- when the code calls ImapHandlerInterface::isAvailable([null]), it will return true
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('open', ["{:143/service=imap/notls/novalidate-cert/secure}", null, null, 0, 0, []], [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getLastError', null, [false]);
        $fake->add('getAlerts', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/notls/novalidate-cert/secure}', '*'], [[]]);
        $fake->add('close', null, [null]);
        $fake->add('isValidStream', $this->connection, [true]);

        $imap = new ImapHandlerFake($fake);
        $ret = (new InboundEmail($imap))->findOptimumSettings();
        self::assertEquals([
            'serial' => '::::::imap::novalidate-cert::notls::secure',
            'service' => 'foo/notls/novalidate-cert/secure',
        ], $ret);
    }

    public function testFindOptimumSettingsNoImap(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [false]);
        $imap = new ImapHandlerFake($fake);

        $ret = (new InboundEmail($imap))->findOptimumSettings();
        self::assertEquals([
            'good' => [],
            'bad' => [],
            'err' => ['No IMAP libraries found. Please resolve this before continuing with Inbound Email'],
        ], $ret);
    }

    public function testFindOptimumSettingsUseSsl(): void
    {
        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('open', ["{:143/service=imap/ssl/tls/validate-cert/secure}", null, null, 0, 0, []], [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getLastError', null, [false]);
        $fake->add('getAlerts', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/ssl/tls/validate-cert/secure}', '*'], [[]]);
        $fake->add('close', null, [null]);
        $fake->add('isValidStream', $this->connection, [true]);

        $imap = new ImapHandlerFake($fake);

        $_REQUEST['ssl'] = 1;

        $ret = (new InboundEmail($imap))->findOptimumSettings();
        self::assertEquals([
            'serial' => 'tls::::ssl::imap::::::secure',
            'service' => 'foo/ssl/tls/validate-cert/secure',
        ], $ret);
    }

    // ------------------------------------------------------------
    // ----- FOLLOWIN TESTS ARE USING REAL IMAP SO SHOULD FAIL ----
    // ---------------------------------------------------------------->

    public function testInboundEmail(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        self::assertInstanceOf('InboundEmail', $inboundEmail);
        self::assertInstanceOf('SugarBean', $inboundEmail);

        self::assertEquals('InboundEmail', $inboundEmail->module_dir);
        self::assertEquals('InboundEmail', $inboundEmail->object_name);
        self::assertEquals('inbound_email', $inboundEmail->table_name);
        self::assertEquals(true, $inboundEmail->new_schema);
        self::assertEquals(true, $inboundEmail->process_save_dates);
        self::assertEquals('defaultIEAccount', $inboundEmail->keyForUsersDefaultIEAccount);
        self::assertEquals(10, $inboundEmail->defaultEmailNumAutoreplies24Hours);
        self::assertEquals(10, $inboundEmail->maxEmailNumAutoreplies24Hours);
        self::assertEquals('InboundEmail.cache.php', $inboundEmail->InboundEmailCacheFile);
        self::assertEquals('date', $inboundEmail->defaultSort);
        self::assertEquals('DESC', $inboundEmail->defaultDirection);
        self::assertEquals('F', $inboundEmail->iconFlagged);
        self::assertEquals('D', $inboundEmail->iconDraft);
        self::assertEquals('A', $inboundEmail->iconAnswered);
        self::assertEquals('del', $inboundEmail->iconDeleted);
        self::assertEquals(false, $inboundEmail->isAutoImport);
        self::assertEquals(0, $inboundEmail->attachmentCount);
    }

    public function testsaveAndOthers(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->name = 'test';
        $inboundEmail->group_id = 1;
        $inboundEmail->status = 'Active';
        $inboundEmail->email_user = 'testuser';
        $inboundEmail->email_password = 'testpass';
        $inboundEmail->mailbox = 'mailbox1,mailbox2,mailbox3';

        $inboundEmail->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($inboundEmail->id));
        self::assertEquals(36, strlen((string) $inboundEmail->id));

        //test getCorrectMessageNoForPop3 method
        $this->getCorrectMessageNoForPop3($inboundEmail->id);

        //test retrieve method
        $this->retrieve($inboundEmail->id);

        //test retrieveByGroupId method
        $this->retrieveByGroupId($inboundEmail->group_id);

        //test retrieveAllByGroupId method
        $this->retrieveAllByGroupId($inboundEmail->group_id);

        //test retrieveAllByGroupIdWithGroupAccounts method
        $this->retrieveAllByGroupIdWithGroupAccounts($inboundEmail->group_id);

        //test getSingularRelatedId method
        $this->getSingularRelatedId();

        //test renameFolder method
        $this->renameFolder($inboundEmail->id);

        //test search method
        $this->search($inboundEmail->id);

        //test saveMailBoxFolders method
        $this->saveMailBoxFolders($inboundEmail->id);

        //test saveMailBoxValueOfInboundEmail method
        $this->saveMailBoxValueOfInboundEmail($inboundEmail->id);

        //test mark_deleted method
        $this->mark_deleted($inboundEmail->id);

        //test hardDelete method
        $this->hardDelete($inboundEmail->id);
    }

    public function getSingularRelatedId(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $result = $inboundEmail->getSingularRelatedId('test', 'inbound_email');
        self::assertEquals(false, $result);

        $result = $inboundEmail->getSingularRelatedId('invalid test', 'inbound_email');
        self::assertEquals(null, $result);
    }

    public function getCorrectMessageNoForPop3($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->retrieve($id);

        $result = $inboundEmail->getCorrectMessageNoForPop3('100');
        self::assertEquals(-1, $result);

        $result = $inboundEmail->getCorrectMessageNoForPop3('1');
        self::assertEquals(-1, $result);
    }

    public function retrieve($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->retrieve($id);

        self::assertEquals('test', $inboundEmail->name);
        self::assertEquals('Active', $inboundEmail->status);
        self::assertEquals('testuser', $inboundEmail->email_user);
        self::assertEquals('testpass', $inboundEmail->email_password);
    }

    public function retrieveByGroupId($group_id): void
    {
        $result = BeanFactory::newBean('InboundEmail')->retrieveByGroupId($group_id);

        self::assertIsArray($result);

        foreach ($result as $ie) {
            self::assertInstanceOf('InboundEmail', $ie);
        }
    }

    public function retrieveAllByGroupId($group_id): void
    {
        $result = BeanFactory::newBean('InboundEmail')->retrieveAllByGroupId($group_id);

        self::assertIsArray($result);

        foreach ($result as $ie) {
            self::assertInstanceOf('InboundEmail', $ie);
        }
    }

    public function retrieveAllByGroupIdWithGroupAccounts($group_id): void
    {
        $result = BeanFactory::newBean('InboundEmail')->retrieveAllByGroupIdWithGroupAccounts($group_id);

        self::assertIsArray($result);

        foreach ($result as $ie) {
            self::assertInstanceOf('InboundEmail', $ie);
        }
    }

    public function renameFolder($id): void
    {

        $fake = new ImapHandlerFakeData();
        $fake->add('isAvailable', null, [true]);  // <-- when the code calls ImapHandlerInterface::isAvailable([null]), it will return true
        $fake->add('setTimeout', [1, 60], [true]);
        $fake->add('setTimeout', [2, 60], [true]);
        $fake->add('setTimeout', [3, 60], [true]);
        $fake->add('getErrors', null, [false]);
        $fake->add('open', ["{:143/service=imap/notls/novalidate-cert/secure}", null, null, 0, 0, []], [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getLastError', null, [false]);
        $fake->add('getAlerts', null, [false]);
        $fake->add('getConnection', null, [function () {
            return tempFileWithMode('wb+');
        }]);
        $fake->add('getMailboxes', ['{:143/service=imap/notls/novalidate-cert/secure}', '*'], [[]]);
        $fake->add('close', null, [null]);
        $fake->add('isValidStream', $this->connection, [true]);
        $fake->add('renameMailbox', ['{:143/service=imap}mailbox1', '{:143/service=imap}new_mailbox'], [false]);
        $fake->add('ping', null, [true]);
        $fake->add('reopen', ['{:143/service=imap}mailbox1,mailbox2,mailbox3', 32768, 0], [true]);

        $fake->add('isValidStream', $this->connection, [true]);

        $imap = new ImapHandlerFake($fake);

        $inboundEmail = new InboundEmail($imap);
        $inboundEmail->retrieve($id);

        self::assertFalse((bool)$inboundEmail->conn);

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $success = $inboundEmail->renameFolder('mailbox1', 'new_mailbox');
            self::assertFalse((bool)$success);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function search($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->retrieve($id);

        $result = $inboundEmail->search($id);

        self::assertIsArray($result);
        self::assertEquals('Search Results', $result['mbox']);
        self::assertEquals($id, $result['ieId']);
    }

    public function saveMailBoxFolders($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->retrieve($id);

        //execute he method and verify attributes
        $inboundEmail->saveMailBoxFolders('INBOX,TRASH');
        self::assertEquals(array('INBOX', 'TRASH'), $inboundEmail->mailboxarray);

        //retrieve it back and verify the updates
        $inboundEmail->retrieve($id);
        self::assertEquals('INBOX,TRASH', $inboundEmail->mailbox);
    }

    public function saveMailBoxValueOfInboundEmail($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->email_user = 'TEST';

        $inboundEmail->saveMailBoxValueOfInboundEmail();

        //retrieve it back and verify the updates
        $inboundEmail->retrieve($id);
        self::assertEquals('INBOX,TRASH', $inboundEmail->mailbox);
    }

    public function mark_deleted($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mark_deleted($id);

        $result = $inboundEmail->retrieve($id);
        self::assertEquals(null, $result);
    }

    public function hardDelete($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->hardDelete($id);

        $result = $inboundEmail->retrieve($id);
        self::assertEquals(null, $result);
    }

    public function testcustomGetMessageText(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->customGetMessageText('some message');
        self::assertEquals('some message', $result);
    }

    public function testgetFormattedRawSource(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without ID
        $result = $inboundEmail->getFormattedRawSource('1');
        self::assertEquals('This information is not available', $result);

        //test with ID
        $inboundEmail->id = 1;
        $result = $inboundEmail->getFormattedRawSource('1');
        self::assertEquals('', $result);
    }

    public function testfilterMailBoxFromRaw(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with array having common element
        $result = $inboundEmail->filterMailBoxFromRaw(array('mailbox1', 'mailbox2', 'mailbox3'), array('mailbox1'));
        self::assertSame(array('mailbox1'), $result);

        //test with array having nothing common
        $result = $inboundEmail->filterMailBoxFromRaw(array('mailbox1', 'mailbox2'), array('mailbox4'));
        self::assertSame(array(), $result);
    }

    public function testconvertToUtf8(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->convertToUtf8('some text with non UTF8 chars');
        self::assertSame('some text with non UTF8 chars', $result);
    }

    public function testgetFormattedHeaders(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test for default/imap
        $result = $inboundEmail->getFormattedHeaders(1);
        self::assertNull($result);

        //test for pop3
        $inboundEmail->protocol = 'pop3';
        $result = $inboundEmail->getFormattedHeaders(1);
        self::assertNull($result);
    }

    public function testsetAndgetCacheTimestamp(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        //test setCacheTimestamp method
        $inboundEmail->setCacheTimestamp('INBOX');

        //test getCacheTimestamp method
        $result = $inboundEmail->getCacheTimestamp('INBOX');
        self::assertGreaterThan(0, strlen((string) $result));
    }

    private function setDummyCacheValue()
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        $inserts = array();

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);
        $inboundEmail->setCacheValue('INBOX.Trash', $inserts);
        return $inboundEmail;
    }

    public function testsetCacheValue(): void
    {
        //retrieve back to verify the records created
        $result = $this->setDummyCacheValue()->getCacheValue('INBOX');

        self::assertGreaterThan(0, count((array)$result['retArr'][0]));
        self::assertEquals(1, $result['retArr'][0]->message_id);
    }

    public function testgetCacheValueForUIDs(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test wih default protocol
        $result = $inboundEmail->getCacheValueForUIDs('INBOX', array(1, 2, 3, 4, 5));

        self::assertIsArray($result);
        self::assertIsArray($result['uids']);
        self::assertIsArray($result['retArr']);

        //test wih pop3 protocol
        $inboundEmail->protocol = 'pop3';
        $result = $inboundEmail->getCacheValueForUIDs('INBOX', array(1, 2, 3, 4, 5));

        self::assertIsArray($result);
        self::assertIsArray($result['uids']);
        self::assertIsArray($result['retArr']);
    }

    public function testgetCacheValue(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test wih default protocol
        $result = $inboundEmail->getCacheValue('INBOX');

        self::assertIsArray($result);
        self::assertIsArray($result['uids']);
        self::assertIsArray($result['retArr']);

        //test wih pop3 protocol
        $inboundEmail->protocol = 'pop3';
        $result = $inboundEmail->getCacheValue('INBOX');

        self::assertIsArray($result);
        self::assertIsArray($result['uids']);
        self::assertIsArray($result['retArr']);
    }

    public function testValidCacheExists(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without a valid id
        $result = $inboundEmail->validCacheExists('');
        self::assertEquals(false, $result);

        //test with a valid id set
        $inboundEmail = $this->setDummyCacheValue();
        $result = $inboundEmail->validCacheExists('');
        self::assertEquals(true, $result);

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        $result = $inboundEmail->validCacheExists('INBOX');
        self::assertEquals(true, $result);
    }

    public function testdisplayFetchedSortedListXML(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //get the cache values array first
        $inboundEmail->id = 1;
        $ret = $inboundEmail->getCacheValue('INBOX');

        //use the cache values array as parameter and verify that it returns an array
        $result = $inboundEmail->displayFetchedSortedListXML($ret, 'INBOX');
        self::assertIsArray($result);
    }

    public function testgetCacheUnreadCount(): void
    {
        $inboundEmail = $this->setDummyCacheValue();

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        //test with invalid mailbox
        $result = $inboundEmail->getCacheUnreadCount('OUTBOX');
        self::assertEquals(0, $result);

        //test with valid mailbox
        $result = $inboundEmail->getCacheUnreadCount('INBOX');
        self::assertGreaterThanOrEqual(1, $result);
    }

    public function testgetCacheCount(): void
    {
        $inboundEmail = $this->setDummyCacheValue();

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        //test with invalid mailbox
        $result = $inboundEmail->getCacheCount('OUTBOX');
        self::assertEquals(0, $result);

        //test with valid mailbox
        $result = $inboundEmail->getCacheCount('INBOX');
        self::assertGreaterThanOrEqual(1, $result);


    }

    public function testgetCacheUnread(): void
    {
        // test
        $inboundEmail = $this->setDummyCacheValue();

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        //test with invalid mailbox
        $result = $inboundEmail->getCacheUnread('OUTBOX');
        self::assertEquals(0, $result);

        //test with valid mailbox
        $result = $inboundEmail->getCacheUnread('INBOX');
        self::assertGreaterThanOrEqual(1, $result);
    }

    public function testmark_answered(): void
    {
        $inboundEmail = $this->setDummyCacheValue();

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        //execute the method to populate answered field
        $inboundEmail->mark_answered(1, 'pop3');

        //retrieve back to verify the records updated
        $result = $inboundEmail->getCacheValue('INBOX');

        self::assertEquals(1, $result['retArr'][0]->answered);
    }

    public function testpop3_shiftCache(): void
    {
        $inboundEmail = $this->setDummyCacheValue();

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        $result = $inboundEmail->pop3_shiftCache(array('1' => '1'), array('1'));

        //retrieve back to verify the records updated
        $result = $inboundEmail->getCacheValue('INBOX');

        self::assertEquals(1, $result['retArr'][0]->imap_uid);
        self::assertEquals(1, $result['retArr'][0]->msgno);
    }

    public function testgetUIDLForMessage(): void
    {
        $inboundEmail = $this->setDummyCacheValue();

        $inboundEmail->pop3_shiftCache(array('1' => '1'), array('1'));

        //test with invalid msgNo
        $result = $inboundEmail->getUIDLForMessage('2');
        self::assertEquals('', $result);
    }

    public function testgetMsgnoForMessageID(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        //test with invalid msgNo
        $result = $inboundEmail->getMsgnoForMessageID('2');
        self::assertEquals('', $result);

        //test with valid msgNo but most probably it will never work because of wrong column name in return statement
        $result = $inboundEmail->getMsgnoForMessageID('1');
        self::assertEquals('', $result);
    }

    public function testpop3_getCacheUidls(): void
    {
        $inboundEmail = $this->setDummyCacheValue();
        $inboundEmail->pop3_shiftCache(array('1' => '1'), array('1'));

        $result = $inboundEmail->pop3_getCacheUidls();

        self::assertEquals(array('1' => '1'), $result);
    }

    public function testemptyTrash(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        $inboundEmail->emptyTrash();

        $result = $inboundEmail->getCacheValue('INBOX.Trash');
        self::assertCount(0, $result['retArr']);
    }

    public function testdeleteCache(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        $inserts = [];

        $overview = new Overview();
        $overview->imap_uid = 1;
        $overview->subject = 'subject';
        $overview->from = 'from';
        $overview->fromaddr = 'from@email.com';
        $overview->to = 'to';
        $overview->toaddr = 'to@email.com';
        $overview->size = 0;
        $overview->message_id = 1;

        $inserts[] = $overview;

        //execute the method to populate email cache
        $inboundEmail->setCacheValue('INBOX', $inserts);

        $inboundEmail->deleteCache();

        $result = $inboundEmail->getCacheValue('INBOX');
        self::assertCount(0, $result['retArr']);
    }

    public function testdeletePop3Cache(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->deletePop3Cache();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testpop3_open(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->pop3_open();

        self::assertEquals(false, $result);
    }

    public function testpop3_cleanUp(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->pop3_cleanUp();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testpop3_sendCommand(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->pop3_sendCommand('get');

        self::assertEquals('', $result);
    }

    public function testgetPop3NewMessagesToDownload(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->getPop3NewMessagesToDownload();

        self::assertIsArray($result);
    }

    public function testgetPop3NewMessagesToDownloadForCron(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->getPop3NewMessagesToDownloadForCron();

        self::assertIsArray($result);
    }

    public function testpop3_getUIDL(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';
        $inboundEmail->protocol = 'pop3';

        $result = $inboundEmail->getPop3NewMessagesToDownloadForCron();

        self::assertIsArray($result);
    }

    public function testpop3_checkPartialEmail(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';
        $inboundEmail->protocol = 'pop3';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->pop3_checkPartialEmail();
            self::assertEquals('could not open socket connection to POP3 server', $result);

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testpop3_checkEmail(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';
        $inboundEmail->protocol = 'pop3';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->pop3_checkEmail();
            self::assertEquals(false, $result);

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testgetMessagesInEmailCache(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        //test for IMAP
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->getMessagesInEmailCache(0, 1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }

        //test for pop3
        $inboundEmail->protocol = 'pop3';
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->getMessagesInEmailCache(1, 0);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testcheckEmailOneMailboxPartial(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->checkEmailOneMailboxPartial('INBOX');

        self::assertEquals(array('status' => 'done'), $result);
    }

    public function testgetCachedIMAPSearch(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->getCachedIMAPSearch('test');

        self::assertIsArray($result);
    }

    public function testcheckEmailIMAPPartial(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->checkEmailIMAPPartial();

        self::assertIsArray($result);
    }

    public function testcheckEmail2_meta(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->checkEmail2_meta();

        self::assertIsArray($result);
        self::assertEquals(array('mailboxes' => array('INBOX' => 0), 'processCount' => 0), $result);
    }

    public function testgetMailboxProcessCount(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->getMailboxProcessCount('INBOX');

        self::assertEquals(0, $result);
    }

    public function testcheckEmail(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test for IMAP
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->checkEmail('INBOX');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }

        //test for pop3
        $inboundEmail->protocol = 'pop3';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->checkEmail('INBOX');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testsyncEmail(): void
    {
        global $current_user;
        $current_user = new User('1');

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->syncEmail();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testdeleteCachedMessages(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->deleteCachedMessages('1,2', 'test');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testgetOverviewsFromCacheFile(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->getOverviewsFromCacheFile('1,2', 'INBOX');

        self::assertIsArray($result);
    }

    public function testfetchCheckedEmails(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;
        $inboundEmail->mailbox = 'INBOX';

        //test with size over 1000 and no imap_uid
        $overview1 = new Overview();
        $overview1->subject = 'subject 1';
        $overview1->size = '10001';

        $fetchedOverviews = array($overview1);
        $result = $inboundEmail->fetchCheckedEmails($fetchedOverviews);

        self::assertEquals(false, $result);

        //test with size less than 1000 and imap_uid set
        $overview2 = new Overview();
        $overview2->subject = 'subject 2';
        $overview2->size = '100';
        //$overview2->imap_uid = 1; //dies if imap_uid is set

        $fetchedOverviews = array($overview2);
        $result = $inboundEmail->fetchCheckedEmails($fetchedOverviews);

        self::assertEquals(true, $result);
    }

    public function testmarkEmails(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->markEmails('1', 'unread');
            $inboundEmail->markEmails('1', 'read');
            $inboundEmail->markEmails('1', 'flagged');
            $inboundEmail->markEmails('1', 'unflagged');
            $inboundEmail->markEmails('1', 'answered');

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testdeleteFolder(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX';

        $result = $inboundEmail->deleteFolder('INBOX');

        self::assertSame(['status', 'errorMessage'], array_keys($result));
        self::assertFalse($result['status']);
    }

    public function testsaveNewFolder(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->saveNewFolder('TEST', 'INBOX');

        self::assertEquals(false, $result);
    }

    public function testgetImapMboxFromSugarProprietary(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with invalid format string
        $result = $inboundEmail->getImapMboxFromSugarProprietary('INBOX.TRASH');
        self::assertEquals('', $result);

        //test with valid format but shorter string
        $result = $inboundEmail->getImapMboxFromSugarProprietary('INBOX::TRASH');
        self::assertEquals('', $result);

        //test with valid format longer string
        $result = $inboundEmail->getImapMboxFromSugarProprietary('INBOX::TRASH::TEST');
        self::assertEquals('TEST', $result);
    }

    public function testrepairAccount(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->email_password = 'test_pass';

        $result = $inboundEmail->repairAccount();

        self::assertEquals(false, $result);
    }

    public function testsavePersonalEmailAccountAndOthers(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $_REQUEST['ie_name'] = 'test';
        $_REQUEST['ie_status'] = 'Active';
        $_REQUEST['server_url'] = '';
        $_REQUEST['email_user'] = 'test';
        $_REQUEST['email_password'] = 'test_pass';
        $_REQUEST['mailbox'] = 'INBOX';

        $result = $inboundEmail->savePersonalEmailAccount(1, 'admin', true);

        self::assertTrue(isset($inboundEmail->id));
        self::assertEquals(36, strlen((string) $inboundEmail->id));

        //test handleIsPersonal method
        $this->handleIsPersonal($inboundEmail->id);

        //test getUserPersonalAccountCount method
        $this->getUserPersonalAccountCount();

        //test retrieveByGroupFolderId method
        $this->retrieveByGroupFolderId();

        //test getUserNameFromGroupId method
        $this->getUserNameFromGroupId($inboundEmail->id);

        //test deletePersonalEmailAccount method
        $this->deletePersonalEmailAccount($inboundEmail->id);
    }

    public function handleIsPersonal($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with a invalid group_id
        $inboundEmail->group_id = 2;
        $result = $inboundEmail->handleIsPersonal();
        self::assertEquals(false, $result);

        //test with a valid group_id
        $inboundEmail->retrieve($id);
        $result = $inboundEmail->handleIsPersonal();
        self::assertEquals(true, $result);
    }

    public function getUserPersonalAccountCount(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with invalid user id
        $user = BeanFactory::newBean('Users');
        $result = $inboundEmail->getUserPersonalAccountCount($user);
        self::assertEquals(0, $result);

        //test with valid user id
        $user->id = 1;
        $result = $inboundEmail->getUserPersonalAccountCount($user);
        self::assertGreaterThan(0, $result);
    }

    public function retrieveByGroupFolderId(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with invalid groupfolder id
        $result = $inboundEmail->retrieveByGroupFolderId('1');

        self::assertIsArray($result);
        self::assertCount(0, $result);

        //test with valid groupfolder id
        $result = $inboundEmail->retrieveByGroupFolderId('');

        self::assertIsArray($result);
        foreach ($result as $ie) {
            self::assertInstanceOf('InboundEmail', $ie);
        }
    }

    public function getUserNameFromGroupId($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with a invalid group_id
        $inboundEmail->group_id = 2;
        $result = $inboundEmail->getUserNameFromGroupId();
        self::assertEquals('', $result);

        //test with a valid group_id
        $inboundEmail->retrieve($id);
        $result = $inboundEmail->getUserNameFromGroupId();
        self::assertEquals('admin', $result);
    }

    public function deletePersonalEmailAccount($id): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with invalid username
        $result = $inboundEmail->deletePersonalEmailAccount($id, 'test');
        self::assertEquals(false, $result);

        //test with valid username
        $result = $inboundEmail->deletePersonalEmailAccount($id, 'admin');
        self::assertEquals(true, $result);
    }

    public function testgetFoldersListForMailBox(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->getFoldersListForMailBox();
        self::assertIsArray($result);
    }

    public function testfindOptimumSettings(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with different parameters, it will always return false because we do not have a mail server to connect.

        $ret = $inboundEmail->findOptimumSettings();
        self::assertEquals(false, $ret);

        self::assertEquals(false, $inboundEmail->findOptimumSettings(true));

        self::assertEquals(false, $inboundEmail->findOptimumSettings(false, 'test', 'test', '', '', 'INBOX'));
    }

    public function testgetSessionConnectionString(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without setting session key
        $result = $inboundEmail->getSessionConnectionString('mail.google.com', 'test', 22, 'IMAP');
        self::assertEquals('', $result);

        //test with session key set
        $_SESSION['mail.google.comtest22IMAP'] = 'test connection string';
        $result = $inboundEmail->getSessionConnectionString('mail.google.com', 'test', 22, 'IMAP');
        self::assertEquals('test connection string', $result);
    }

    public function testsetSessionConnectionString(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->setSessionConnectionString('mail.google.com', 'test', 22, 'IMAP', 'test connection');
        self::assertEquals('test connection', $_SESSION['mail.google.comtest22IMAP']);
    }

    public function testgetSessionInboundDelimiterString(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without setting session key
        $result = $inboundEmail->getSessionInboundDelimiterString('mail.google.com', 'test', 22, 'IMAP');
        self::assertEquals('', $result);

        //test with session key set
        $_SESSION['mail.google.comtest22IMAPdelimiter'] = 'delimit string';
        $result = $inboundEmail->getSessionInboundDelimiterString('mail.google.com', 'test', 22, 'IMAP');
        self::assertEquals('delimit string', $result);
    }

    public function testsetSessionInboundDelimiterString(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->setSessionInboundDelimiterString('mail.google.com', 'test', 22, 'IMAP', 'test string');
        self::assertEquals('test string', $_SESSION['mail.google.comtest22IMAPdelimiter']);
    }

    public function testgetSessionInboundFoldersString(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without setting session key
        $result = $inboundEmail->getSessionInboundFoldersString('mail.google.com', 'test', 22, 'IMAP');
        self::assertEquals('', $result);

        //test with session key set
        $_SESSION['mail.google.comtest22IMAPfoldersList'] = 'foldersList string';
        $result = $inboundEmail->getSessionInboundFoldersString('mail.google.com', 'test', 22, 'IMAP');
        self::assertEquals('foldersList string', $result);
    }

    public function testsetSessionInboundFoldersString(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->setSessionInboundFoldersString('mail.google.com', 'test', 22, 'IMAP', 'foldersList string');
        self::assertEquals('foldersList string', $_SESSION['mail.google.comtest22IMAPfoldersList']);
    }

    public function testgroupUserDupeCheck(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without name i-e user_name in query
        $result = $inboundEmail->groupUserDupeCheck();
        self::assertEquals(false, $result);

        //test with name i-e user_name in query
        $inboundEmail->name = 'admin';
        $result = $inboundEmail->groupUserDupeCheck();
        self::assertEquals(false, $result);
    }

    public function testgetGroupsWithSelectOptions(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->group_id = 1;

        $result = $inboundEmail->getGroupsWithSelectOptions();
        self::assertEquals('', $result);

        $expected = "\n<OPTION value='0'>1</OPTION>\n<OPTION selected value='1'>2</OPTION>\n<OPTION value='2'>3</OPTION>";
        $result = $inboundEmail->getGroupsWithSelectOptions(array(1, 2, 3));
        self::assertEquals($expected, $result);
    }

    public function testhandleAutoresponse(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->template_id = 1;
        $email = BeanFactory::newBean('Emails');
        $email->name = 'test';
        $email->from_addr = 'test@email.com';
        $contactAddr = 'test@email.com';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->handleAutoresponse($email, $contactAddr);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testhandleCaseAssignment(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $email = BeanFactory::newBean('Emails');
        $email->name = 'test';

        $result = $inboundEmail->handleCaseAssignment($email);
        self::assertEquals(false, $result);
    }

    public function testhandleMailboxType(): void
    {
        $header = null;
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $email = BeanFactory::newBean('Emails');
        $email->name = 'test';

        $inboundEmail->mailbox_type = 'support';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->handleMailboxType($email, $header);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testisMailBoxTypeCreateCase(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without setting attributes
        $result = $inboundEmail->isMailBoxTypeCreateCase();
        self::assertEquals(false, $result);

        //test with attributes set
        $inboundEmail->mailbox_type = 'createcase';
        $inboundEmail->groupfolder_id = 1;

        $result = $inboundEmail->isMailBoxTypeCreateCase();
        self::assertEquals(true, $result);
    }

    public function testhandleCreateCase(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $email = BeanFactory::newBean('Emails');
        $email->name = 'test';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->handleCreateCase($email, 1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testhandleLinking(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $email = BeanFactory::newBean('Emails');
        $email->from_addr = 'test@from.com';

        $result = $inboundEmail->handleLinking($email);
        self::assertEquals($email->from_addr, $result);
    }

    public function testgetCharsetFromBreadCrumb(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $parts = array(
                (Object) array('ifparameters' => 1, 'attribute' => 'charset', 'value' => 'test', 'parts' => array((Object) array('ifparameters' => 1, 'attribute' => 'charset', 'value' => 'test', 'parts' => array((Object) array('ifparameters' => 1, 'attribute' => 'charset', 'value' => 'test', 'parts' => 'dummy parts 2'))))),
        );

        $result = $inboundEmail->getCharsetFromBreadCrumb('1.2.3', $parts);

        self::assertEquals('default', $result);
    }

    public function testaddBreadCrumbOffset(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with empty offset string
        $result = $inboundEmail->addBreadCrumbOffset('1.1.1', '');
        self::assertEquals('1.1.1', $result);

        //test with empty bread crumb string
        $result = $inboundEmail->addBreadCrumbOffset('', '1.1.1');
        self::assertEquals('1.1.1', $result);

        //test with shorter bread crumb string
        $result = $inboundEmail->addBreadCrumbOffset('1.1.1', '2.2.2.2');
        self::assertEquals('3.3.3.2', $result);
    }

    public function testdecodeHeader(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $expected = array(
                          'From' => 'Media Temple user (mt.kb.user@gmail.com)',
                          'Subject' => 'article: How to Trace a Email',
                          'Date' => 'January 25, 2011 3:30:58 PM PDT',
                          'To' => 'user@example.com',
                          'Return-Path' => '<mt.kb.user@gmail.com>',
                          'Envelope-To' => 'user@example.com',
                          'Delivery-Date' => 'Tue, 25 Jan 2011 15:31:01 -0700',
                        );
        $header = "From: Media Temple user (mt.kb.user@gmail.com)\r\nSubject: article: How to Trace a Email\r\nDate: January 25, 2011 3:30:58 PM PDT\r\nTo: user@example.com\r\nReturn-Path: <mt.kb.user@gmail.com>\r\nEnvelope-To: user@example.com\r\nDelivery-Date: Tue, 25 Jan 2011 15:31:01 -0700";

        $result = $inboundEmail->decodeHeader($header);
        self::assertEquals($expected, $result);
    }

    public function testhandleCharsetTranslation(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with default
        $result = $inboundEmail->handleCharsetTranslation('sample text', 'default');
        self::assertEquals('sample text', $result);

        //test with ISO-8859-8
        $result = $inboundEmail->handleCharsetTranslation('sample text', 'ISO-8859-8');
        self::assertEquals('sample text', $result);
    }

    public function testbuildBreadCrumbs(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->buildBreadCrumbs(array(), 'ALTERNATIVE', '1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testbuildBreadCrumbsHTML(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->buildBreadCrumbsHTML(array());
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testconvertImapToSugarEmailAddress(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX';
        $inboundEmail->host = 'mail.google.com';

        $result = $inboundEmail->convertImapToSugarEmailAddress(array($inboundEmail));
        self::assertEquals('INBOX@mail.google.com', $result);
    }

    public function testhandleEncodedFilename(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $result = BeanFactory::newBean('InboundEmail')->handleEncodedFilename('attachment1.pdf');
        self::assertEquals('attachment1.pdf', $result);
    }

    public function testgetMimeType(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        self::assertEquals('text/plain', $inboundEmail->getMimeType(0, 'plain'));
        self::assertEquals('multipart/binary', $inboundEmail->getMimeType(1, 'binary'));
        self::assertEquals('other/subtype', $inboundEmail->getMimeType('test', 'subtype'));
    }

    public function testsaveAttachments(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->saveAttachments('1', array(), '1', '0', true);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testgetNoteBeanForAttachment(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->getNoteBeanForAttachment('1');

        self::assertInstanceOf('Note', $result);
        self::assertEquals('1', $result->parent_id);
        self::assertEquals('Emails', $result->parent_type);
    }

    public function testretrieveAttachmentNameFromStructure(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with filename attribute
        $part = (Object) array('dparameters' => array((Object) array('attribute' => 'filename', 'value' => 'test1.txt'), (Object) array('attribute' => 'filename', 'value' => 'test2.txt')),
                                'parameters' => array((Object) array('attribute' => 'name', 'value' => 'test1'), (Object) array('attribute' => 'name', 'value' => 'test2')),
                                );

        $result = $inboundEmail->retrieveAttachmentNameFromStructure($part);
        self::assertEquals('test1.txt', $result);

        //test with no filename attribute
        $part = (Object) array('dparameters' => array((Object) array('attribute' => 'name', 'value' => 'test1.txt')),
                                'parameters' => array((Object) array('attribute' => 'name', 'value' => 'test1'), (Object) array('attribute' => 'name', 'value' => 'test2')),

                                );

        $result = $inboundEmail->retrieveAttachmentNameFromStructure($part);
        self::assertEquals('test1', $result);
    }

    public function testsaveAttachmentBinaries(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $part = (Object) array('disposition' => 'multipart', 'subtype' => 10);

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->saveAttachmentBinaries(BeanFactory::newBean('Notes'), '1', '1.1', $part, 1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testhandleTranserEncoding(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        self::assertEquals('test', $inboundEmail->handleTranserEncoding('test'));
        self::assertEquals('test', $inboundEmail->handleTranserEncoding('dGVzdA==', 3));
        self::assertEquals('test', $inboundEmail->handleTranserEncoding('test', 4));
    }

    public function testgetMessageId(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $header = "From: Media Temple user (mt.kb.user@gmail.com)\r\nSubject: article: How to Trace a Email\r\nDate: January 25, 2011 3:30:58 PM PDT\r\nTo: user@example.com\r\nReturn-Path: <mt.kb.user@gmail.com>\r\nEnvelope-To: user@example.com\r\nDelivery-Date: Tue, 25 Jan 2011 15:31:01 -0700";

        $result = $inboundEmail->getMessageId($header);

        self::assertEquals('21c65f7db176f0bd93768214b00ae397', $result);
    }

    public function testimportDupeCheck(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $textHeader = "From: Media Temple user (mt.kb.user@gmail.com)\r\nSubject: article: How to Trace a Email\r\nDate: January 25, 2011 3:30:58 PM PDT\r\nTo: user@example.com\r\nReturn-Path: <mt.kb.user@gmail.com>\r\nEnvelope-To: user@example.com\r\nDelivery-Date: Tue, 25 Jan 2011 15:31:01 -0700";

        $result = $inboundEmail->importDupeCheck('1', $textHeader, $textHeader);
        self::assertEquals(true, $result);
    }

    public function testhandleMimeHeaderDecode(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->handleMimeHeaderDecode('Subject: article: How to Trace a Email');

        self::assertEquals('Subject: article: How to Trace a Email', $result);
    }

    public function testgetUnixHeaderDate(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->handleMimeHeaderDecode('Date: January 25, 2011 3:30:58 PM PDT');

        self::assertEquals('Date: January 25, 2011 3:30:58 PM PDT', $result);
    }

    public function testgetDuplicateEmailId(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->getDuplicateEmailId('1', '1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testimportOneEmail(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX';
        $inboundEmail->id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $inboundEmail->importOneEmail('1', '1');
            self::assertEquals(false, $result);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testisUuencode(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        self::assertEquals(false, $inboundEmail->isUuencode('test'));

        self::assertEquals(false, $inboundEmail->isUuencode("begin 0744 odt_uuencoding_file.dat\r+=&5S=\"!S=')I;F<`\r`\rend"));
    }

    public function testhandleUUEncodedEmailBody(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $raw = 'Message Body: This is a KnowledgeBase article that provides information on how to find email headers and use the data to trace a email.';

        $result = $inboundEmail->handleUUEncodedEmailBody($raw, 1);

        self::assertEquals("\n".$raw, $result);
    }

    public function testcheckFilterDomain(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $email = BeanFactory::newBean('Emails');
        $email->reply_to_email = 'reply@gmail.com';
        $email->from_addr = 'from@gmail.com';

        $result = $inboundEmail->checkFilterDomain($email);
        self::assertEquals(true, $result);
    }

    public function testcheckOutOfOffice(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        self::assertEquals(false, $inboundEmail->checkOutOfOffice('currently Out of Office, will reply later'));
        self::assertEquals(true, $inboundEmail->checkOutOfOffice('test subject'));
    }

    public function testsetAndgetAutoreplyStatus(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;

        //execute the setAutoreplyStatus method to set an auto reply status for email
        $inboundEmail->setAutoreplyStatus('auto_reply_test@email.com');

        //test with and invalid email. it will return true as well because it's stil under max limit.
        $result = $inboundEmail->getAutoreplyStatus('invalid@email.com');
        self::assertEquals(true, $result);
    }

    public function testsaveInboundEmailSystemSettings(): void
    {
        global $sugar_config, $db;

        //unset and reconnect Db to resolve mysqli fetch exeception
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //execute the method to save the setting
        $inboundEmail->saveInboundEmailSystemSettings('test', 'test_macro');

        //verify the key created
        self::assertEquals('test_macro', $sugar_config['inbound_email_test_subject_macro']);
    }

    public function testgetCaseIdFromCaseNumber(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->getCaseIdFromCaseNumber('test', BeanFactory::newBean('Cases'));
        self::assertEquals(false, $result);
    }

    public function testget_stored_options(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $result = $inboundEmail->get_stored_options('test', '');
        self::assertEquals('', $result);

        $result = $inboundEmail->get_stored_options('test', 'default_option');
        self::assertEquals('default_option', $result);
    }

    public function testSetStoredOptions(): void
    {
        $ie = BeanFactory::newBean('InboundEmail');
        $so = $ie->getStoredOptions();
        $so['something'] = 'testinfo';
        $ie->setStoredOptions($so);
        $ret = $ie->getStoredOptions();
        self::assertEquals('testinfo', $ret['something']);
    }

    public function testgetRelatedId(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Test with Users module
        $result = $inboundEmail->getRelatedId('getRelatedId@email.com', 'Users');
        self::assertEquals(false, $result);

        // Test with Contacts module
        $result = $inboundEmail->getRelatedId('getRelatedId@email.com', 'Contacts');
        self::assertEquals(false, $result);
    }

    public function testgetNewMessageIds(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->getNewMessageIds();

        self::assertEquals(null, $result);
    }

    public function testgetConnectString(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        self::assertEquals('{:143/service=imap}', $inboundEmail->getConnectString()); //test with default options
        self::assertEquals('{:143/service=imapmail.google.com}INBOX', $inboundEmail->getConnectString('mail.google.com', 'INBOX'));//test with includeMbox true
        self::assertEquals('{:143/service=imapmail.google.com}', $inboundEmail->getConnectString('mail.google.com', 'INBOX', false));//test with includeMbox false
    }

    public function testdisconnectMailserver(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->disconnectMailserver();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testconnectMailserver(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with default parameters
        $result = $inboundEmail->connectMailserver();
        self::assertEquals('false', $result);

        //test with test and force true
        $result = $inboundEmail->connectMailserver(true, true);
        self::assertEquals("Can't open mailbox {:143/service=imap}: invalid remote specification<p><p><p>Please check your settings and try again.", $result);
    }

    public function testcheckImap(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->checkImap();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testget_summary_text(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without setting name
        self::assertEquals(null, $inboundEmail->get_summary_text());

        //test with name set
        $inboundEmail->name = 'test';
        self::assertEquals('test', $inboundEmail->get_summary_text());
    }

    public function testcreate_export_query(): void
    {
        self::markTestIncomplete('#Error: mysqli_real_escape_string(): Couldnt fetch mysqli');
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test with empty string params
        $expected = " SELECT  inbound_email.*  , jt0.user_name created_by_name , jt0.created_by created_by_name_owner  , 'Users' created_by_name_mod FROM inbound_email   LEFT JOIN  users jt0 ON jt0.id=inbound_email.created_by AND jt0.deleted=0\n AND jt0.deleted=0 where inbound_email.deleted=0";
        $actual = $inboundEmail->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = " SELECT  inbound_email.*  , jt0.user_name created_by_name , jt0.created_by created_by_name_owner  , 'Users' created_by_name_mod FROM inbound_email   LEFT JOIN  users jt0 ON jt0.id=inbound_email.created_by AND jt0.deleted=0\n AND jt0.deleted=0 where (jt0.user_name=\"\") AND inbound_email.deleted=0 ORDER BY inbound_email.id";
        $actual = $inboundEmail->create_export_query('id', 'jt0.user_name=""');
        self::assertSame($expected, $actual);
    }

    public function testget_list_view_data(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox_type = 'INBOX';
        $inboundEmail->status = 'Active';

        $result = $inboundEmail->get_list_view_data();

        $expected = array(
            'DELETED' => '0',
            'STATUS' => 'Active',
            'DELETE_SEEN' => '0',
            'MAILBOX_TYPE' => 'INBOX',
            'IS_PERSONAL' => '0',
            'MAILBOX_TYPE_NAME' => null,
            'GLOBAL_PERSONAL_STRING' => 'group',
            'PORT' => '143',
            'MOVE_MESSAGES_TO_TRASH_AFTER_IMPORT' => '0',
            'AUTH_TYPE' => 'Basic Auth',
            'PROTOCOL' => 'imap',
            'IS_SSL' => '0',
            'IS_DEFAULT' => '0',
            'IS_AUTO_IMPORT' => '0',
            'IS_CREATE_CASE' => '0',
            'ALLOW_OUTBOUND_GROUP_USAGE' => '0',
        );

        self::assertIsArray($result);
        self::assertEquals($expected, $result);

    }

    public function testfill_in_additional_list_fields(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->service = 'tls::ca::ssl::protocol';

        $result = $inboundEmail->fill_in_additional_list_fields();

        self::assertEquals('tls', $inboundEmail->tls);
        self::assertEquals('ca', $inboundEmail->ca);
        self::assertEquals('ssl', $inboundEmail->ssl);
        self::assertEquals('protocol', $inboundEmail->protocol);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->service = 'tls::ca::ssl::protocol';

        $result = $inboundEmail->fill_in_additional_detail_fields();

        self::assertEquals('tls', $inboundEmail->tls);
        self::assertEquals('ca', $inboundEmail->ca);
        self::assertEquals('ssl', $inboundEmail->ssl);
        self::assertEquals('protocol', $inboundEmail->protocol);
    }

    public function testisAutoImport(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $user = BeanFactory::newBean('Users');

        //test with invalid user
        $result = $inboundEmail->isAutoImport($user);
        self::assertEquals(false, $result);

        //test with default user
        $user->retrieve('1');
        $result = $inboundEmail->isAutoImport($user);
        self::assertEquals(false, $result);
    }

    public function testcleanOutCache(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->cleanOutCache();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testdeleteMessageOnMailServerForPop3(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->deleteMessageOnMailServerForPop3('1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testisPop3Protocol(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        //test without setting protocol
        self::assertEquals(false, $inboundEmail->isPop3Protocol());

        //test with pop3 protocol
        $inboundEmail->protocol = 'pop3';
        self::assertEquals(true, $inboundEmail->isPop3Protocol());
    }

    public function testSetAndGetUsersDefaultOutboundServerId(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $user = BeanFactory::newBean('Users');
        $user->retrieve(1);

        //set a Outbound Server Id
        $inboundEmail->setUsersDefaultOutboundServerId($user, '11111111-1111-1111-1111-111111111111');

        //retrieve Outbound Server Id back and verify
        $result = $inboundEmail->getUsersDefaultOutboundServerId($user);
        $isValidator = new SuiteCRM\Utility\SuiteValidator();

        self::assertTrue($isValidator->isValidId($result));
    }

    public function testdisplayOneEmail(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;
        $inboundEmail->mailbox = 'INBOX';
        $inboundEmail->email = BeanFactory::newBean('Emails');

        $inboundEmail->email->name = 'test';
        $inboundEmail->email->from_addr_name = 'from';
        $inboundEmail->email->to_addrs_names = 'to';
        $inboundEmail->email->cc_addrs_names = 'cc';
        $inboundEmail->email->reply_to_addr = 'reply';

        $expected = array(
                          'meta' => array('type' => 'archived', 'uid' => 1, 'ieId' => 1, 'email' => array('name' => 'test', 'from_name' => '', 'from_addr' => 'from', 'date_start' => ' ', 'time_start' => '', 'message_id' => '', 'cc_addrs' => 'cc', 'attachments' => '', 'toaddrs' => 'to', 'description' => '', 'reply_to_addr' => 'reply'), 'mbox' => 'INBOX', 'cc' => '', 'is_sugarEmail' => false),
                        );
        $result = $inboundEmail->displayOneEmail(1, 'INBOX');

        self::assertEquals($expected, $result);
    }

    public function testcollapseLongMailingList(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $emails = 'one@email.com,two@email.com,three@email.com,four@email.com,five@email.com,six@email.com';

        $expected = "<span onclick='javascript:SUGAR.email2.detailView.showFullEmailList(this);' style='cursor:pointer;'>one@email.com, two@email.com [...4 More]</span><span onclick='javascript:SUGAR.email2.detailView.showCroppedEmailList(this)' style='cursor:pointer; display:none;'>one@email.com, two@email.com, three@email.com, four@email.com, five@email.com, six@email.com [ less ]</span>";

        $actual = $inboundEmail->collapseLongMailingList($emails);

        self::assertEquals($expected, $actual);
    }

    public function testsortFetchedOverview(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = 1;
        $inboundEmail->mailbox = 'INBOX';

        $overview1 = new Overview();
        $overview1->subject = 'subject 1';
        $overview1->from = 'from 1';
        $overview1->flagged = '1';
        $overview1->answered = '1';
        $overview1->date = '2016-01-01';

        $overview2 = new Overview();
        $overview2->subject = 'subject 2';
        $overview2->from = 'from 2';
        $overview2->flagged = '2';
        $overview2->answered = '2';
        $overview2->date = '2016-01-02';

        $arr = array();
        $arr[] = $overview1;
        $arr[] = $overview2;

        //execute the method to sort the objects array descending and verify the order
        $result = $inboundEmail->sortFetchedOverview($arr, 3, 'DESC');
        self::assertEquals('subject 2', $result['retArr'][0]->subject);

        //execute the method to sort the objects array ascending and verify the order
        $result = $inboundEmail->sortFetchedOverview($arr, 3, 'ASC');
        self::assertEquals('subject 1', $result['retArr'][0]->subject);
    }

    public function testdisplayFolderContents(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $expected = array('mbox' => 'INBOX', 'ieId' => 1, 'name' => 'test', 'fromCache' => 0, 'out' => array());
        $inboundEmail->id = 1;
        $inboundEmail->name = 'test';

        $result = $inboundEmail->displayFolderContents('INBOX', 'false', 1);

        self::assertEquals($expected, $result);
    }

    public function testcreateAutoImportSugarFolder(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        unset($db->database);
        $db->checkConnection();

        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->name = 'test';

        $result = $inboundEmail->createAutoImportSugarFolder();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($result));
        self::assertEquals(36, strlen((string) $result));
    }

    public function testgetMailboxes(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailboxarray = array('INBOX.TRASH', 'OUTBOX.TRASH');
        $expected = array('INBOX' => array('TRASH' => 'TRASH'), 'OUTBOX' => array('TRASH' => 'TRASH'));

        //test with justRaw default/false
        $result = $inboundEmail->getMailboxes();
        self::assertEquals($expected, $result);

        //test with justRaw true
        $result = $inboundEmail->getMailboxes(true);
        self::assertEquals($inboundEmail->mailboxarray, $result);
    }

    public function testgetMailBoxesForGroupAccount(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 1;
        $inboundEmail->mailbox = 'INBOX.TRASH,OUTBOX.TRASH';
        $expected = array('INBOX' => array('TRASH' => 'TRASH'), 'OUTBOX' => array('TRASH' => 'TRASH'));

        $result = $inboundEmail->getMailBoxesForGroupAccount();

        self::assertEquals($expected, $result);
    }

    public function testretrieveMailBoxFolders(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->mailbox = 'INBOX,OUTBOX,TRASH';

        $inboundEmail->retrieveMailBoxFolders();

        self::assertEquals(array('INBOX', 'OUTBOX', 'TRASH'), $inboundEmail->mailboxarray);
    }

    public function testinsertMailBoxFolders(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->id = '101';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->insertMailBoxFolders(array('INBOX', 'OUTBOX'));
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testretrieveDelimiter(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->retrieveDelimiter();

        self::assertEquals('.', $result);
    }

    public function testgenerateFlatArrayFromMultiDimArray(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $arraymbox = array('INBOX' => array('TRASH' => 'TRASH'), 'OUTBOX' => array('TRASH' => 'TRASH'));

        $expected = array('INBOX', 'INBOX.TRASH', 'OUTBOX', 'OUTBOX.TRASH');

        $result = $inboundEmail->generateFlatArrayFromMultiDimArray($arraymbox, '.');

        self::assertEquals($expected, $result);
    }

    public function testgenerateMultiDimArrayFromFlatArray(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $expected = array('INBOX' => array('TRASH' => 'TRASH'), 'OUTBOX' => array('TRASH' => 'TRASH'));

        $result = $inboundEmail->generateMultiDimArrayFromFlatArray(array('INBOX.TRASH', 'OUTBOX.TRASH'), '.');

        self::assertEquals($expected, $result);
    }

    public function testgenerateArrayData(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $result = array();
        $arraymbox = array('INBOX' => array('TRASH' => 'TRASH'));
        $expected = array('MAIN', 'MAIN.INBOX', 'MAIN.INBOX.TRASH');

        $inboundEmail->generateArrayData('MAIN', $arraymbox, $result, '.');

        self::assertEquals($expected, $result);
    }

    public function testsortMailboxes(): void
    {
        $result = BeanFactory::newBean('InboundEmail')->sortMailboxes('INBOX.TRASH', array());

        $expected = array('INBOX' => array('TRASH' => 'TRASH'));

        self::assertEquals($expected, $result);
    }

    public function testgetServiceString(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        $inboundEmail->service = 'tls::ca::ssl::protocol';

        $result = $inboundEmail->getServiceString();

        self::assertEquals('/tls/ca/ssl/protocol', $result);
    }

    public function testgetNewEmailsForSyncedMailbox(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->getNewEmailsForSyncedMailbox();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testimportMessages(): void
    {
        $inboundEmail = BeanFactory::newBean('InboundEmail');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $inboundEmail->protocol = 'pop3';
            $inboundEmail->importMessages();

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail("\nException: " . get_class($e) . ": " . $e->getMessage() . "\nin " . $e->getFile() . ':' . $e->getLine() . "\nTrace:\n" . $e->getTraceAsString() . "\n");
        }
    }

    public function testOverview(): void
    {
        $overview = new Overview();

        self::assertInstanceOf('Overview', $overview);
        self::assertIsArray($overview->fieldDefs);
        self::assertIsArray($overview->indices);
    }
}
