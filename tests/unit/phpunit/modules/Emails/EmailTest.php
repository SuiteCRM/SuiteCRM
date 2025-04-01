<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/SugarPHPMailerMock.php';
require_once __DIR__ . '/NonGmailSentFolderHandlerMock.php';
require_once __DIR__ . '/EmailMock.php';

class EmailTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'Emails');
    }

    public function testSendSaveAndStoreInSentOk(): void
    {
        // handle non-gmail sent folder (mailbox is set)
        $mailer = new SugarPHPMailerMock();
        $ie = BeanFactory::newBean('InboundEmail');
        $ie->save();
        $ieId = $ie->id;
        self::assertTrue((bool)$ieId);
        $_REQUEST['inbound_email_id'] = $ieId;
        $email = new EmailMock();
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $nonGmailSentFolder = new NonGmailSentFolderHandlerMock();
        $ie->mailbox = 'testmailbox';
        $storedOption = $ie->getStoredOptions();
        $storedOption['sentFolder'] = 'testSentFolder';
        $ie->setStoredOptions($storedOption);
        $mailer->oe->mail_smtptype = 'foomail';
        $ret = $email->send($mailer, $nonGmailSentFolder, $ie);

        self::assertTrue($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertNull($email->getTempEmailAtSend()->getNonGmailSentFolderHandler()->getLastError());
        self::assertEquals(Email::NO_ERROR, $email->getTempEmailAtSend()->getLastSaveAndStoreInSentError());
    }

    public function testSendSaveAndStoreInSentOkButIEDoesntMatch(): void
    {
        // handle non-gmail sent folder (mailbox is set)
        $mailer = new SugarPHPMailerMock();
        $ie = BeanFactory::newBean('InboundEmail');
        $ieId = $ie->save();
        self::assertTrue((bool)$ieId);
        $_REQUEST['inbound_email_id'] = $ieId;
        $email = new EmailMock();
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $nonGmailSentFolder = new NonGmailSentFolderHandlerMock();
        $ie->mailbox = 'testmailbox';
        $storedOption = $ie->getStoredOptions();
        $storedOption['sentFolder'] = 'testSentFolder';
        $ie->setStoredOptions($storedOption);
        $mailer->oe->mail_smtptype = 'foomail';
        $ret = $email->send($mailer, $nonGmailSentFolder, $ie);

        self::assertTrue($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertNull($email->getTempEmailAtSend()->getNonGmailSentFolderHandler()->getLastError());
        self::assertEquals(Email::NO_ERROR, $email->getTempEmailAtSend()->getLastSaveAndStoreInSentError());
    }

    public function testSendSaveAndStoreInSentNoSentFolder(): void
    {
        // handle non-gmail sent folder (mailbox is set but no ie stored option: sentFolder)
        $mailer = new SugarPHPMailerMock();
        $ie = BeanFactory::newBean('InboundEmail');
        $ieId = $ie->save();
        self::assertTrue((bool)$ieId);
        $_REQUEST['inbound_email_id'] = $ieId;
        $email = new EmailMock();
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $nonGmailSentFolder = new NonGmailSentFolderHandlerMock();
        $ie->mailbox = 'testmailbox';
        $mailer->oe->mail_smtptype = 'foomail';
        $ret = $email->send($mailer, $nonGmailSentFolder, $ie);

        self::assertTrue($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertEquals(Email::ERR_NOT_STORED_AS_SENT, $email->getTempEmailAtSend()->getLastSaveAndStoreInSentError());
        self::assertEquals(
            NonGmailSentFolderHandler::ERR_NO_STORED_SENT_FOLDER,
            $email->getTempEmailAtSend()->getNonGmailSentFolderHandler()->getLastError()
        );
    }

    public function testSendSaveAndStoreInSentNoMailbox(): void
    {
        // mailbox is not set
        $mailer = new SugarPHPMailerMock();
        $ieId = BeanFactory::newBean('InboundEmail')->save();
        self::assertTrue((bool)$ieId);
        $_REQUEST['inbound_email_id'] = $ieId;
        $email = new EmailMock();
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $mailer->oe->mail_smtptype = 'foomail';
        $ret = $email->send($mailer);

        self::assertTrue($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertEquals(Email::ERR_NOT_STORED_AS_SENT, $email->getTempEmailAtSend()->getLastSaveAndStoreInSentError());
        self::assertEquals(NonGmailSentFolderHandler::ERR_EMPTY_MAILBOX, $email->getTempEmailAtSend()->getNonGmailSentFolderHandler()->getLastError());
    }

    public function testSendSaveAndStoreInSentNoIE(): void
    {
        // no IE
        $mailer = new SugarPHPMailerMock();
        $_REQUEST['inbound_email_id'] = '123';
        $email = BeanFactory::newBean('Emails');
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $ret = $email->send($mailer);

        self::assertTrue($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertEquals(Email::ERR_IE_RETRIEVE, $email->getTempEmailAtSend()->getLastSaveAndStoreInSentError());
        self::assertNull($email->getTempEmailAtSend()->getNonGmailSentFolderHandler());
    }

    public function testSendSaveAndStoreInSentSendFailedButItsOk(): void
    {
        // should send successfully
        $mailer = new SugarPHPMailerMock();
        $email = BeanFactory::newBean('Emails');
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $ret = $email->send($mailer);

        self::assertTrue($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertNull($email->getTempEmailAtSend());
    }

    public function testSendSaveAndStoreInSentSendFailed(): void
    {
        // sending should failing
        $email = BeanFactory::newBean('Emails');
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $email->saved_attachments = [];
        $ret = $email->send();

        self::assertFalse($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertNull($email->getTempEmailAtSend());
    }

    public function testSendSaveAndStoreInSentSendNoAttachment(): void
    {
        // attachenemt error
        $email = BeanFactory::newBean('Emails');
        $email->to_addrs_arr = ['foo@bazz.bar'];
        $ret = $email->send();

        self::assertFalse($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertNull($email->getTempEmailAtSend());
    }

    public function testSendSaveAndStoreInSentSendNoTo(): void
    {
        // "to" array is required
        $email = BeanFactory::newBean('Emails');
        $ret = $email->send();

        self::assertFalse($ret);
        self::assertNull($email->getLastSaveAndStoreInSentError());
        self::assertNull($email->getNonGmailSentFolderHandler());
        self::assertNull($email->getTempEmailAtSend());
    }

    public function testSetLastSaveAndStoreInSentErrorNo(): void
    {
        $email = new EmailMock();
        try {
            $email->setLastSaveAndStoreInSentErrorPublic(null);
            self::assertTrue(false);
        } catch (InvalidArgumentException $e) {
            self::assertEquals(Email::ERR_CODE_SHOULD_BE_INT, $e->getCode());
        } catch (EmailException $e) {
            self::assertEquals(Email::UNHANDLED_LAST_ERROR, $e->getCode());
        }
    }

    public function testSaveAndStoreInSentFolderIfNoGmailWithNoIE(): void
    {
        $email = BeanFactory::newBean('Emails');
        $ie = BeanFactory::newBean('InboundEmail');
        $ieId = null;
        $mail = new SugarPHPMailer();
        $nonGmailSentFolder = new NonGmailSentFolderHandler();
        $ret = $email->saveAndStoreInSentFolderIfNoGmail($ie, $ieId, $mail, $nonGmailSentFolder);

        self::assertNull($ret);
        self::assertEquals(Email::ERR_IE_RETRIEVE, $email->getLastSaveAndStoreInSentError());
    }

    public function testEmail(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $email = BeanFactory::newBean('Emails');
        self::assertInstanceOf('Email', $email);
        self::assertInstanceOf('SugarBean', $email);

        self::assertEquals('Emails', $email->module_dir);
        self::assertEquals('Email', $email->object_name);
        self::assertEquals('emails', $email->table_name);
        self::assertEquals('Emails', $email->module_name);
        self::assertEquals(true, $email->new_schema);
        self::assertEquals('archived', $email->type);
    }

    public function testemail2init(): void
    {
        $email = BeanFactory::newBean('Emails');
        $email->email2init();

        self::assertInstanceOf('EmailUI', $email->et);
    }

    public function testbean_implements(): void
    {
        // test
        $email = BeanFactory::newBean('Emails');
        self::assertEquals(false, $email->bean_implements('')); //test with blank value
        self::assertEquals(false, $email->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $email->bean_implements('ACL')); //test with valid value
    }

    public function testemail2saveAttachment(): void
    {
        $result = BeanFactory::newBean('Emails')->email2saveAttachment();
        self::assertIsArray($result);
    }

    public function testsafeAttachmentName(): void
    {
        $email = BeanFactory::newBean('Emails');

        self::assertEquals(false, $email->safeAttachmentName('test.ext'));
        self::assertEquals(false, $email->safeAttachmentName('test.exe'));
        self::assertEquals(true, $email->safeAttachmentName('test.cgi'));
    }

    public function testemail2ParseAddresses(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->email2init();
        $addresses = 'abc<abc@xyz.com>,xyz<xyz@abc.com>';
        $expected = array(
            array('email' => 'abc@xyz.com', 'display' => 'abc'),
            array('email' => 'xyz@abc.com', 'display' => 'xyz')
        );

        $result = $email->email2ParseAddresses($addresses);
        self::assertSame($expected, $result);
    }

    public function testemail2ParseAddressesForAddressesOnly(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with simplest format
        $addresses = 'abc@xyz.com,xyz@abc.com';
        $result = $email->email2ParseAddressesForAddressesOnly($addresses);
        self::assertEquals(array('abc@xyz.com', 'xyz@abc.com'), $result);

        //test with more used format
        $addresses = 'abc<abc@xyz.com>,xyz<xyz@abc.com>';
        $result = $email->email2ParseAddressesForAddressesOnly($addresses);
        self::assertEquals(array('abc@xyz.com', 'xyz@abc.com'), $result);
    }

    public function testemail2GetMime(): void
    {
        //test with a filename
        $result = BeanFactory::newBean('Emails')->email2GetMime('config.php');
        self::assertEquals('text/x-php', $result);
    }

    public function testdecodeDuringSend(): void
    {
        $email = BeanFactory::newBean('Emails');

        self::assertEquals('some text', $email->decodeDuringSend('some text'));
        self::assertEquals(
            '&lt; some text &gt;',
            $email->decodeDuringSend('sugarLessThan some text sugarGreaterThan')
        );
    }

    public function testisDraftEmail(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with required parametr set
        self::assertEquals(true, $email->isDraftEmail(array('saveDraft' => '1')));

        //test with one of required attribute set
        $email->type = 'draft';
        self::assertEquals(false, $email->isDraftEmail(array()));

        //test with both of required attribute set
        $email->status = 'draft';
        self::assertEquals(true, $email->isDraftEmail(array()));
    }

    public function testgetNamePlusEmailAddressesForCompose(): void
    {
        $result = BeanFactory::newBean('Emails')->getNamePlusEmailAddressesForCompose('Users', array(1));
        self::assertGreaterThanOrEqual(0, strlen((string) $result));
    }

    public function test_arrayToDelimitedString(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with empty array
        $result = $email->_arrayToDelimitedString(array());
        self::assertEquals('', $result);

        //test with valid array
        $result = $email->_arrayToDelimitedString(array('value1', 'value2'));
        self::assertEquals('value1,value2', $result);
    }

    public function testsaveAndOthers(): void
    {
        // test
        $email = BeanFactory::newBean('Emails');

        $email->from_addr = 'from@email.com';
        $email->to_addrs = 'to@email.com';
        $email->cc_addrs = 'cc@email.com';
        $email->bcc_addrs = 'bcc@email.com';

        $email->from_addr_name = 'from';
        $email->to_addrs_names = 'to';
        $email->cc_addrs_names = 'cc';
        $email->bcc_addrs_names = 'bcc';
        $email->reply_to_addr = 'reply@email.com';
        $email->description = 'test description';
        $email->description_html = 'test html description';
        $email->raw_source = 'test raw source';

        $result = $email->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($email->id));
        self::assertEquals(36, strlen((string) $email->id));

        //test retrieve method
        $this->retrieve($email->id);

        //test retrieveEmailAddresses method
        $this->retrieveEmailAddresses($email->id);

        //test retrieveEmailText method
        $this->retrieveEmailText($email->id);

        //test saveEmailAddresses method
        $this->saveEmailAddresses($email->id);

        //test linkEmailToAddres method
        $this->linkEmailToAddress($email->id);

        //test handleAttachments method
        $this->handleAttachments($email->id);

        //test delete method
        $this->delete($email->id);
    }

    public function retrieve($id): void
    {
        $result = BeanFactory::newBean('Emails')->retrieve($id);

        self::assertTrue(isset($result->id));
        self::assertEquals(36, strlen((string) $result->id));

        self::assertTrue(isset($result->from_addr_name));
        self::assertTrue(isset($result->to_addrs_names));
        self::assertTrue(isset($result->cc_addrs_names));
        self::assertTrue(isset($result->bcc_addrs_names));

        self::assertTrue(isset($result->raw_source));
        self::assertTrue(isset($result->description_html));
    }

    public function saveEmailAddresses($id): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->id = $id;
        $email->from_addr = 'from_test@email.com';
        $email->to_addrs = 'to_test@email.com';
        $email->cc_addrs = 'cc_test@email.com';
        $email->bcc_addrs = 'bcc_test@email.com';

        $email->saveEmailAddresses();

        //retrieve and verify that email addresses were saved properly
        $email->retrieveEmailAddresses();

        self::assertNotFalse(strpos($email->from_addr, 'from_test@email.com'));
        self::assertNotFalse(strpos($email->to_addrs, 'to_test@email.com'));
        self::assertNotFalse(strpos($email->cc_addrs, 'cc_test@email.com'));
        self::assertNotFalse(strpos($email->bcc_addrs, 'bcc_test@email.com'));
    }

    public function retrieveEmailAddresses($id): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->id = $id;
        $email->retrieveEmailAddresses();

        self::assertTrue(isset($email->from_addr_name));
        self::assertTrue(isset($email->to_addrs_names));
        self::assertTrue(isset($email->cc_addrs_names));
        self::assertTrue(isset($email->bcc_addrs_names));
    }

    public function linkEmailToAddress($id): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->id = $id;

        $result = $email->linkEmailToAddress(1, 'from');

        self::assertTrue(isset($result));
        self::assertEquals(36, strlen((string) $result));
    }

    public function retrieveEmailText($id): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->id = $id;

        $email->retrieveEmailText();

        self::assertTrue(isset($email->from_addr_name));
        self::assertTrue(isset($email->reply_to_addr));
        self::assertTrue(isset($email->to_addrs_names));
        self::assertTrue(isset($email->cc_addrs_names));
        self::assertTrue(isset($email->bcc_addrs_names));

        self::assertTrue(isset($email->raw_source));
        self::assertTrue(isset($email->description_html));
    }

    public function handleAttachments($id): void
    {
        $email = BeanFactory::newBean('Emails');

        $email = $email->retrieve($id);

        $email->type = 'out';
        $email->status = 'draft';
        $_REQUEST['record'] = $id;

        $email->handleAttachments();

        self::assertIsArray($email->attachments);
    }

    public function delete($id): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->delete($id);

        $result = $email->retrieve($id);
        self::assertEquals(null, $result);
    }

    public function testSaveTempNoteAttachmentsAndGetNotesAndDoesImportedEmailHaveAttachment(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->id = 1;

        //test saveTempNoteAttachments method to create a note for email
        $email->saveTempNoteAttachments('test_file', 'test', 'text/plain');

        //test doesImportedEmailHaveAttachment method to verify note created.
        $result = $email->doesImportedEmailHaveAttachment($email->id);
        self::assertEquals(0, $result);

        //test getNotes method and verify that it retrieves the created note.
        $email->getNotes($email->id);
        self::assertIsArray($email->attachments);
        foreach ($email->attachments as $note) {
            self::assertTrue(isset($note));
            self::assertInstanceOf('Note', $note);
        }

        //finally cleanup
        $email->delete($email->id);
    }

    public function testgetNotesSqlEscape(): void
    {
        $email = BeanFactory::newBean('Emails');
        $email->getNotes("'=");
        self::assertFalse(DBManagerFactory::getInstance()->lastError());
    }

    public function testcleanEmails(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with simplest format
        $addresses = 'abc@xyz.com,xyz@abc.com';
        $result = $email->cleanEmails($addresses);
        self::assertEquals('abc@xyz.com, xyz@abc.com', $result);

        //test with more used format
        $addresses = 'abc<abc@xyz.com>,xyz<xyz@abc.com>';
        $result = $email->cleanEmails($addresses);
        self::assertEquals('abc <abc@xyz.com>, xyz <xyz@abc.com>', $result);
    }

    public function testgetForwardHeader(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->from_name = 'from test';
        $email->name = 'test';
        $email->date_sent_received = '2016-01-01';
        $email->to_addrs = 'to@email.com';
        $email->cc_addrs = 'cc@email.com';

        $expected = '<br /><br />> From: from test<br />> Date Sent/Received: 2016-01-01<br />> To: to@email.com<br />> Cc: cc@email.com<br />> Subject: test<br />> <br />';

        $actual = $email->getForwardHeader();
        self::assertSame($expected, $actual);
    }

    public function testgetReplyHeader(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->from_name = 'from test';
        $email->time_start = '01:01:00';
        $email->date_start = '2016-01-01';

        $expected = '<br>On 2016-01-01, 01:01:00, from test wrote:';

        $actual = $email->getReplyHeader();
        self::assertSame($expected, $actual);
    }

    public function testquotePlainTextEmail(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with plain string containing no line breaks
        $expected = "\n> some text\r";
        $actual = $email->quotePlainTextEmail('some text');
        self::assertSame($expected, $actual);

        //test with string containing line breaks
        $expected = "\n> some\r> text\r> with\r> new\r> lines\r";
        $actual = $email->quotePlainTextEmail("some\ntext\nwith\nnew\nlines");
        self::assertSame($expected, $actual);
    }

    public function testquoteHtmlEmail(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with empty string
        $expected = '';
        $actual = $email->quoteHtmlEmail('');
        self::assertSame($expected, $actual);

        //test with plain string
        $expected = "<div style='border-left:1px solid #00c; padding:5px; margin-left:10px;'>some test</div>";
        $actual = $email->quoteHtmlEmail('some test');
        self::assertSame($expected, $actual);

        //test with string containing special charecters
        $expected = "<div style='border-left:1px solid #00c; padding:5px; margin-left:10px;'>some test with <&</div>";
        $actual = $email->quoteHtmlEmail('some test with <&');
        self::assertSame($expected, $actual);
    }

    public function testquoteHtmlEmailForNewEmailUI(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with empty string
        $expected = '';
        $actual = $email->quoteHtmlEmailForNewEmailUI('');
        self::assertSame($expected, $actual);

        //test with plain string
        $expected = "<div style='border-left:1px solid #00c; padding:5px; margin-left:10px;'>some test</div>";
        $actual = $email->quoteHtmlEmailForNewEmailUI('some test');
        self::assertSame($expected, $actual);

        //test with string containing special charecters
        $expected = "<div style='border-left:1px solid #00c; padding:5px; margin-left:10px;'>some test with</div>";
        $actual = $email->quoteHtmlEmailForNewEmailUI("some test with \n");
        self::assertSame($expected, $actual);
    }

    public function testcheck_email_settings(): void
    {
        global $current_user;

        $email = BeanFactory::newBean('Emails');

        //test without a valid current user
        $result = $email->check_email_settings();
        self::assertEquals(false, $result);

        //test with a valid current user
        $current_user = new User(1);
        $result = $email->check_email_settings();
        self::assertEquals(false, $result);
    }

    public function testjs_set_archived(): void
    {
        $actual = BeanFactory::newBean('Emails')->js_set_archived();
        self::assertGreaterThan(0, strlen((string) $actual));
    }

    public function testu_get_clear_form_js(): void
    {
        self::markTestIncomplete('environment dependency (CRLF?)');
        $email = BeanFactory::newBean('Emails');

        //with empty params
        $expected = "		<script type=\"text/javascript\" language=\"JavaScript\"><!-- Begin
			function clear_form(form) {
				var newLoc = \"index.php?action=\" + form.action.value + \"&module=\" + form.module.value + \"&query=true&clear_query=true\";
				if(typeof(form.advanced) != \"undefined\"){
					newLoc += \"&advanced=\" + form.advanced.value;
				}
				document.location.href= newLoc;
			}
		//  End --></script>";
        $actual = $email->u_get_clear_form_js('', '', '');
        self::assertSame($expected, $actual, "exp:[" . print_r($expected, true) . "] act:[" . print_r($actual, true) . "]");

        //with valid params
        $expected = "\n		<script type=\"text/javascript\" language=\"JavaScript\"><!-- Begin\n			function clear_form(form) {\n				var newLoc = \"index.php?action=\" + form.action.value + \"&module=\" + form.module.value + \"&query=true&clear_query=true&type=out&assigned_user_id=1\";\n				if(typeof(form.advanced) != \"undefined\"){\n					newLoc += \"&advanced=\" + form.advanced.value;\n				}\n				document.location.href= newLoc;\n			}\n		//  End --></script>";
        $actual = $email->u_get_clear_form_js('out', '', '1');
        self::assertSame($expected, $actual, "exp:[" . print_r($expected, true) . "] act:[" . print_r($actual, true) . "]");
    }

    public function testpickOneButton(): void
    {
        $email = BeanFactory::newBean('Emails');

        $expected = "<div><input	title=\"Take from Group\"
						class=\"button\"
						type=\"button\" name=\"button\"
						onClick=\"window.location='index.php?module=Emails&action=Grab';\"
						style=\"margin-bottom:2px\"
						value=\"  Take from Group  \"></div>";
        $actual = $email->pickOneButton();
        self::assertSame($expected, $actual);
    }

    public function testgetUserEditorPreference(): void
    {
        $result = BeanFactory::newBean('Emails')->getUserEditorPreference();
        self::assertEquals('html', $result);
    }

    public function testparse_addrs(): void
    {
        $email = BeanFactory::newBean('Emails');

        $addrs = 'abc<abc@email.com>;xyz<xyz@email.com>';
        $addrs_ids = '1;2';
        $addrs_names = 'abc;xyz';
        $addrs_emails = 'abc@email.com;xyz@email.com';

        $expected = array(
            array('email' => 'abc@email.com', 'display' => 'abc', 'contact_id' => '1'),
            array('email' => 'xyz@email.com', 'display' => 'xyz', 'contact_id' => '2')
        );

        $actual = $email->parse_addrs($addrs, $addrs_ids, $addrs_names, $addrs_emails);

        self::assertSame($expected, $actual);
    }

    public function testremove_empty_fields(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test for array with empty values
        $expected = array('val1', 'val2');
        $fields = array('val1', ' ', 'val2');
        $actual = $email->remove_empty_fields($fields);
        self::assertSame($expected, $actual);

        //test for array without empty values
        $expected = array('val1', 'val2');
        $fields = array('val1', 'val2');
        $actual = $email->remove_empty_fields($fields);
        self::assertSame($expected, $actual);
    }

    public function testhasSignatureInBody(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->description_html = 'some html text with <b>sign</b>';
        $email->description = 'some text with sign';

        //test for strings with signature present
        $sig = array('signature_html' => 'sign', 'signature' => 'sign');
        $result = $email->hasSignatureInBody($sig);
        self::assertEquals(true, $result);

        //test for strings with signature absent
        $sig = array('signature_html' => 'signature', 'signature' => 'signature');
        $result = $email->hasSignatureInBody($sig);
        self::assertEquals(false, $result);
    }

    public function testremoveAllNewlines(): void
    {
        $email = BeanFactory::newBean('Emails');

        self::assertEquals('', $email->removeAllNewlines(''));
        self::assertEquals('some text', $email->removeAllNewlines('some text'));
        self::assertEquals('some text', $email->removeAllNewlines('some text<br>'));
        self::assertEquals('some text', $email->removeAllNewlines("some\n text\n"));
        self::assertEquals('some text', $email->removeAllNewlines("some\r\n text\r\n"));
    }

    public function testgetStartPage(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test without assigned_user_id in url
        $url = 'index.php?module=Users&offset=6&stamp=1453274421025259800&return_module=Users&action=DetailView&record=seed_max_id';
        $expected = array(
            'module' => 'Users',
            'action' => 'DetailView',
            'group' => '',
            'record' => 'seed_max_id',
            'type' => '',
            'offset' => '6',
            'stamp' => '1453274421025259800',
            'return_module' => 'Users',
        );
        $actual = $email->getStartPage($url);
        self::assertSame($expected, $actual);

        //test with assigned_user_id in url
        $url = 'index.php?module=Users&offset=6&stamp=1453274421025259800&return_module=Users&action=DetailView&record=seed_max_id&assigned_user_id=1';
        $expected = array(
            'module' => 'Users',
            'action' => 'DetailView',
            'group' => '',
            'record' => 'seed_max_id',
            'type' => '',
            'offset' => '6',
            'stamp' => '1453274421025259800',
            'return_module' => 'Users',
            'assigned_user_id' => '1',
            'current_view' => 'DetailView&module=Users&assigned_user_id=1&type=',
        );
        $actual = $email->getStartPage($url);
        self::assertSame($expected, $actual);
    }

    public function testsetMailer(): void
    {
        $result = BeanFactory::newBean('Emails')->setMailer(new SugarPHPMailer(), '', '');

        self::assertInstanceOf('SugarPHPMailer', $result);
        self::assertInstanceOf('OutboundEmail', $result->oe);
    }

    public function testhandleBody(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test without setting REQUEST parameters
        $email->description_html = "some email description containing email text &amp; &#39; <br>&nbsp;";
        $result = $email->handleBody(new SugarPHPMailer());
        $expected = "some email description containing email text & ' \n ";
        $actual = $email->description;
        self::assertSame($expected, $actual);
        $expected = 'SugarPHPMailer';
        $actual = $result;
        self::assertInstanceOf($expected, $result);

        // TODO: TASK: UNDEFINED - Refactor html body
        //test with REQUEST parameters set
//        $_REQUEST['setEditor'] = 1;
//        $_REQUEST['description_html'] = '1';
//        $email->description_html = 'some email description containing email text &amp; &#39; <br>&nbsp;';
//
//        $result = $email->handleBody(new SugarPHPMailer());
//
//        $expected = "some email description containing email text & ' \n ";
//        $actual = $email->description;
//        $this->assertEquals($expected, $actual);
//        $this->assertInstanceOf('SugarPHPMailer', $result);
    }

    public function testhandleBodyInHTMLformat(): void
    {
        // TODO: TASK: UNDEFINED - Refactor html body
//        $email = BeanFactory::newBean('Emails');
//
//        $mailer = new SugarPHPMailer();
//        $email->description_html = 'some email description containing email text &amp; &#39; <br>&nbsp;';
//
//        $result = $email->handleBodyInHTMLformat($mailer);
//
//        $this->assertEquals("some email description containing email text & ' \n ", $email->description);
//        $this->assertEquals("some email description containing email text & ' <br> ", $mailer->Body);
    }

    public function testlistviewACLHelper(): void
    {
        self::markTestIncomplete('environment dependency (span os a?)');


        // test
        $email = BeanFactory::newBean('Emails');

        $expected = array('MAIN' => 'span', 'PARENT' => 'a', 'CONTACT' => 'span');
        $actual = $email->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testgetSystemDefaultEmail(): void
    {
        $email = BeanFactory::newBean('Emails');

        $expected = array('email', 'name');
        $actual = array_keys($email->getSystemDefaultEmail());

        self::assertSame($expected, $actual);
    }

    public function testcreate_new_list_query(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with empty string params
        $expected = "SELECT emails.*, users.user_name as assigned_user_name\n FROM emails\n LEFT JOIN users ON emails.assigned_user_id=users.id \nWHERE  emails.deleted=0 \n ORDER BY date_sent_received DESC";
        $actual = $email->create_new_list_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT emails.*, users.user_name as assigned_user_name\n FROM emails\n LEFT JOIN users ON emails.assigned_user_id=users.id \nWHERE users.user_name = \"\" AND  emails.deleted=0 \n ORDER BY emails.id";
        $actual = $email->create_new_list_query('emails.id', 'users.user_name = ""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->parent_id = '1';
        $email->parent_name = 'test';
        $email->parent_type = 'Contacts';

        $email->fill_in_additional_list_fields();

        self::assertEquals('DetailView', $email->link_action);
        self::assertEquals('', $email->attachment_image);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->created_by = '1';
        $email->modified_user_id = '1';
        $email->type = 'out';
        $email->status = 'send_error';

        $email->fill_in_additional_list_fields();

        self::assertEquals('Administrator', $email->created_by_name);
        self::assertEquals('Administrator', $email->modified_by_name);
        self::assertEquals('Send Error', $email->type_name);
        self::assertEquals('(no subject)', $email->name);
        self::assertEquals('DetailView', $email->link_action);
    }

    public function testcreate_export_query(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with empty string params
        $expected = 'SELECT emails.* FROM emails where emails.deleted=0 ORDER BY emails.name';
        $actual = $email->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = 'SELECT emails.* FROM emails where users.user_name = "" AND emails.deleted=0 ORDER BY emails.id';
        $actual = $email->create_export_query('emails.id', 'users.user_name = ""');
        self::assertSame($expected, $actual);
    }

    public function testget_list_view_data(): void
    {
        // TODO: TASK: UNDEFINED - Update to handle new list view
//        $email = BeanFactory::newBean('Emails');
//        $current_theme = SugarThemeRegistry::current();
//
//        $email->from_addr_name = 'Admin';
//        $email->id = 1;
//        $email->intent = 'support';
//        $email->to_addrs = 'abc@email.com';
//        $email->link_action = 'DetailView';
//        $email->type_name = 'out';
//
//        $expected = array(
//                'ID' => 1,
//                'FROM_ADDR_NAME' => 'Admin',
//                'TYPE' => 'Archived',
//                'INTENT' => 'support',
//                'FROM_ADDR' => null,
//                'QUICK_REPLY' => '<a  href="index.php?module=Emails&action=Compose&replyForward=true&reply=reply&record=1&inbound_email_id=1">Reply</a>',
//                'STATUS' => null,
//                'CREATE_RELATED' => '~index.php\?module=Cases&action=EditView&inbound_email_id=1~',
//                'CONTACT_NAME' => '</a>abc@email.com<a>',
//                'CONTACT_ID' => '',
//                'ATTACHMENT_IMAGE' => null,
//                'LINK_ACTION' => 'DetailView',
//                'TYPE_NAME' => 'out',
//        );
//
//        $actual = $email->get_list_view_data();
//        foreach ($expected as $expectedKey => $expectedVal) {
//            if ($expectedKey == 'CREATE_RELATED') {
//                $this->assertRegExp($expected[$expectedKey], $actual[$expectedKey]);
//            } else {
//                $this->assertSame($expected[$expectedKey], $actual[$expectedKey]);
//            }
//        }
        self::markTestIncomplete('Need to be updated');
    }

    public function testquickCreateForm(): void
    {
        $email = BeanFactory::newBean('Emails');

        $expected = '~/images/advanced_search~';

        $actual = $email->quickCreateForm();
        self::assertMatchesRegularExpression($expected, $actual);
    }

    public function testsearchImportedEmails(): void
    {
        $actual = BeanFactory::newBean('Emails')->searchImportedEmails();
        self::assertIsArray($actual);
    }

    public function test_genereateSearchImportedEmailsQuery(): void
    {
        $email = BeanFactory::newBean('Emails');

        $expected = "SELECT emails.id , emails.mailbox_id, emails.name, emails.date_sent_received, emails.status, emails.type, emails.flagged, emails.reply_to_status,
		                      emails_text.from_addr, emails_text.to_addrs  FROM emails   JOIN emails_text on emails.id = emails_text.email_id   WHERE (emails.type= 'inbound' OR emails.type='archived' OR emails.type='out') AND emails.deleted = 0 ";
        $actual = $email->_genereateSearchImportedEmailsQuery();
        self::assertSame($expected, $actual);
    }

    public function test_generateSearchImportWhereClause(): void
    {
        // test
        $email = BeanFactory::newBean('Emails');

        //test without request params
        $expected = '';
        $actual = $email->_generateSearchImportWhereClause();
        self::assertSame($expected, $actual);

        //test with searchDateFrom request param only
        $_REQUEST['searchDateFrom'] = '2015-01-01 00:00:00';
        $expected = "emails.date_sent_received >= '' ";
        $actual = $email->_generateSearchImportWhereClause();
        self::assertSame($expected, $actual);

        //test with searchDateTo request param only
        $_REQUEST['searchDateFrom'] = '';
        $_REQUEST['searchDateTo'] = '2015-01-01 00:00:00';
        $expected = "emails.date_sent_received <= '' ";
        $actual = $email->_generateSearchImportWhereClause();
        self::assertSame($expected, $actual);

        //test with both request params
        $_REQUEST['searchDateFrom'] = '2015-01-01 00:00:00';
        $_REQUEST['searchDateTo'] = '2015-01-01 00:00:00';
        $expected = "( emails.date_sent_received >= '' AND
                                          emails.date_sent_received <= '' )";
        $actual = $email->_generateSearchImportWhereClause();
        self::assertSame($expected, $actual);
    }

    public function testtrimLongTo(): void
    {
        $email = BeanFactory::newBean('Emails');

        self::assertEquals('test string', $email->trimLongTo('test string')); //test without any separator
        self::assertEquals(
            'test string 1...',
            $email->trimLongTo('test string 1, test string2')
        ); //test with , separator
        self::assertEquals(
            'test string 1...',
            $email->trimLongTo('test string 1; test string2')
        );//test with ; separator
    }

    public function testget_summary_text(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test without setting name
        self::assertEquals(null, $email->get_summary_text());

        //test with name set
        $email->name = 'test';
        self::assertEquals('test', $email->get_summary_text());
    }

    public function testdistributionForm(): void
    {
        // test
        require_once 'include/utils/layout_utils.php';
        $email = BeanFactory::newBean('Emails');

        //test with empty string
        $result = $email->distributionForm('');
        self::assertGreaterThan(0, strlen((string) $result));

        //test with valid string
        $result = $email->distributionForm('test');
        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function testuserSelectTable(): void
    {
        $result = BeanFactory::newBean('Emails')->userSelectTable();
        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function testcheckInbox(): void
    {
        $email = BeanFactory::newBean('Emails');

        //test with empty string
        $expected = "<div><input	title=\"Check For New Email\"
						class=\"button\"
						type=\"button\" name=\"button\"
						onClick=\"window.location='index.php?module=Emails&action=Check&type=';\"
						style=\"margin-bottom:2px\"
						value=\"  Check Mail  \"></div>";
        $actual = $email->checkInbox('');
        self::assertSame($expected, $actual);

        //test with valid string
        $expected = "<div><input	title=\"Check For New Email\"
						class=\"button\"
						type=\"button\" name=\"button\"
						onClick=\"window.location='index.php?module=Emails&action=Check&type=test';\"
						style=\"margin-bottom:2px\"
						value=\"  Check Mail  \"></div>";
        $actual = $email->checkInbox('test');
        self::assertSame($expected, $actual);
    }

    public function testfillPrimaryParentFields(): void
    {
        $email = BeanFactory::newBean('Emails');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $email->fillPrimaryParentFields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcid2Link(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->description_html = '<img class="image" src="cid:1">';
        $email->imagePrefix = 'prfx';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $email->cid2Link('1', 'image/png');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcids2Links(): void
    {
        $email = BeanFactory::newBean('Emails');

        $email->description_html = '<img class="image" src="cid:1">';
        $email->imagePrefix = 'prfx';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $email->cids2Links();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testsetFieldNullable(): void
    {
        $email = BeanFactory::newBean('Emails');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $email->setFieldNullable('description');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testrevertFieldNullable(): void
    {
        $email = BeanFactory::newBean('Emails');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $email->revertFieldNullable('description');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }
}
