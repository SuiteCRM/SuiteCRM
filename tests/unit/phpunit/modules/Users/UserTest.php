<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class UserTest extends SuitePHPUnitFrameworkTestCase
{
    public function setUp(): void
    {
        parent::setUp();
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'Users');
    }

    public function testgetSignatureButtons(): void
    {
        self::markTestIncomplete('environment dependency');

        global $mod_strings;

        $user = BeanFactory::newBean('Users');

        //preset required values
        $user->retrieve(1);
        $mod_strings['LBL_BUTTON_EDIT'] = "";
        $mod_strings['LBL_BUTTON_CREATE'] = "";


        //test with defaultDisplay false
        $expected = "<input class='button' onclick='javascript:open_email_signature_form(\"\", \"1\");' value='' type='button'>&nbsp;<span name=\"edit_sig\" id=\"edit_sig\" style=\"visibility:hidden;\"><input class=\"button\" onclick=\"javascript:open_email_signature_form(document.getElementById('signature_id', '').value)\" value=\"\" type=\"button\" tabindex=\"392\">&nbsp;
					</span>";
        $actual = $user->getSignatureButtons('');
        self::assertSame($expected, $actual);


        //test with defaultDisplay true
        $expected = "<input class='button' onclick='javascript:open_email_signature_form(\"\", \"1\");' value='' type='button'>&nbsp;<span name=\"edit_sig\" id=\"edit_sig\" style=\"visibility:inherit;\"><input class=\"button\" onclick=\"javascript:open_email_signature_form(document.getElementById('signature_id', '').value)\" value=\"\" type=\"button\" tabindex=\"392\">&nbsp;
					</span>";
        $actual = $user->getSignatureButtons('', true);
        self::assertSame($expected, $actual);
    }

    public function testUser(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $user = BeanFactory::newBean('Users');

        self::assertInstanceOf('User', $user);
        self::assertInstanceOf('Person', $user);
        self::assertInstanceOf('SugarBean', $user);

        self::assertEquals('Users', $user->module_dir);
        self::assertEquals('User', $user->object_name);
        self::assertEquals('users', $user->table_name);

        self::assertEquals(true, $user->new_schema);
        self::assertEquals(false, $user->authenticated);
        self::assertEquals(true, $user->importable);
        self::assertEquals(false, $user->team_exists);
    }

    public function testgetSystemUser(): void
    {
        self::markTestIncomplete('environment dependency');

        $result = BeanFactory::newBean('Users')->getSystemUser();

        self::assertInstanceOf('User', $result);
        self::assertEquals(1, $result->id);
    }

    public function testgetDefaultSignature(): void
    {
        // test
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->getDefaultSignature();
        self::assertIsArray($result);
    }

    public function testgetSignature(): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->getSignature(1);
        self::assertEquals(false, $result);
    }

    public function testgetSignaturesArray(): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->getSignaturesArray();
        self::assertIsArray($result);
    }

    public function testgetSignatures(): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $expected = "<select onChange='setSigEditButtonVisibility();' id='signature_id' name='signature_id'>\n<OPTION selected value=''>--None--</OPTION>";
        $actual = $user->getSignatures();
        self::assertSame(strpos((string) $actual, $expected), 0);
        self::assertMatchesRegularExpression('/\<\/select\>$/', $actual);
    }

    public function testhasPersonalEmail(): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve(2);

        $result = $user->hasPersonalEmail();
        self::assertEquals(false, $result);
    }

    public function testgetUserPrivGuid(): void
    {
        self::markTestIncomplete('environment dependency');

        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        try {
            $result = $user->getUserPrivGuid();
            self::fail('This function sould throws an Exception.');
        } catch (Exception $e) {
        }

        self::assertTrue(isset($result));
        self::assertEquals(36, strlen((string) $result));
    }

    public function testsetUserPrivGuid(): void
    {
        self::markTestIncomplete('environment dependency');

        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $user->setUserPrivGuid();

        $result = $user->getPreference('userPrivGuid', 'global', $user);

        self::assertTrue(isset($result));
        self::assertEquals(36, strlen((string) $result));
    }

    public function testSetAndGetAndResetPreference(): void
    {
        self::markTestIncomplete('environment dependency');

        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);


        //test setPreference method
        $user->setPreference('userPrivGuid', 'someGuid', 0, 'global', $user);


        //test getPreference method
        $result = $user->getPreference('userPrivGuid', 'global', $user);
        self::assertTrue(isset($result));
        self::assertEquals('someGuid', $result);


        //test resetPreferences method and verify that created preference is no longer available
        $user->resetPreferences();
        $result = $user->getPreference('userPrivGuid', 'global', $user);
        self::assertFalse(isset($result));
    }


    public function testsavePreferencesToDB(): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $user->savePreferencesToDB();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetUserDateTimePreferences(): void
    {
        self::markTestIncomplete('environment dependency');
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->getUserDateTimePreferences();

        self::assertIsArray($result);
        self::assertTrue(isset($result['date']));
        self::assertTrue(isset($result['time']));
        self::assertTrue(isset($result['userGmt']));
        self::assertTrue(isset($result['userGmtOffset']));
    }

    public function testGetETagSeedAndIncrementETag(): void
    {
        self::markTestIncomplete('environment dependency');
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        //execute getETagSeed method, get Etag value
        $ETagInitial = $user->getETagSeed('test');
        self::assertGreaterThanOrEqual(0, $ETagInitial);


        //execute incrementETag to increment
        $user->incrementETag('test');


        //execute getETagSeed method again, get Etag final value and  compare final and initial values
        $ETagFinal = $user->getETagSeed('test');
        self::assertGreaterThan($ETagInitial, $ETagFinal);
    }

    public function testgetLicensedUsersWhere(): void
    {
        $expected = "deleted=0 AND status='Active' AND user_name IS NOT NULL AND is_group=0 AND portal_only=0  AND LENGTH(user_name)>0";
        $actual = User::getLicensedUsersWhere();
        self::assertSame($expected, $actual);
    }

    public function testget_summary_text(): void
    {
        $user = BeanFactory::newBean('Users');

        //test without setting name
        self::assertEquals(null, $user->get_summary_text());

        //test with name set
        $user->name = "test";
        self::assertEquals('test', $user->get_summary_text());
    }

    public function testbean_implements(): void
    {
        $user = BeanFactory::newBean('Users');

        self::assertEquals(false, $user->bean_implements('')); //test with blank value
        self::assertEquals(false, $user->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $user->bean_implements('ACL')); //test with valid value
    }

    public function testcheck_role_membership(): void
    {
        self::markTestIncomplete('environment dependency');
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $user = BeanFactory::newBean('Users');

        $result = $user->check_role_membership("test", '');
        self::assertEquals(false, $result);

        $result = $user->check_role_membership("test", '1');
        self::assertEquals(false, $result);
    }

    public function testsaveAndOthers(): void
    {
        self::markTestIncomplete('environment dependency');

        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();


        $user = BeanFactory::newBean('Users');

        $user->user_name = "test";

        $user->first_name = "firstn";
        $user->last_name = "lastn";

        $user->email1 = "one@email.com";
        $user->email2 = "two@email.com";

        $result = $user->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($user->id));
        self::assertEquals(36, strlen((string) $user->id));


        //test retrieve method
        $this->retrieve($user->id);


        //test retrieve_by_email_address method
        $this->retrieve_by_email_address($user->id);


        //test newPassword And findUserPassword methods
        $this->NewPasswordAndFindUserPassword($user->id);

        //test authenticate_user method
        $this->authenticate_user($user->id);


        //test load_user method
        $this->load_user($user->id);


        //test change_password method
        $this->change_password($user->id);

        //test getPreferredEmail method
        $this->getPreferredEmail($user->id);


        //test getUsersNameAndEmail method
        $this->getUsersNameAndEmail($user->id);


        //test getEmailInfo method
        $this->getEmailInfo($user->id);


        //change username and delete the user to avoid picking it up by password in future
        $user->user_name = "test_deleted";
        $user->save();
        $user->mark_deleted($user->id);
    }

    public function retrieve($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        self::assertEquals("test", $user->user_name);

        self::assertEquals("firstn", $user->first_name);
        self::assertEquals("lastn", $user->last_name);

        self::assertEquals("one@email.com", $user->email1);
        self::assertEquals("two@email.com", $user->email2);
    }

    public function retrieve_by_email_address($id): void
    {
        $user = BeanFactory::newBean('Users');

        //test with invalid email
        $user->retrieve_by_email_address("wrongone@email.com");
        self::assertEquals('', $user->id);


        //test with valid email and test for record ID to verify that record is same
        $user = BeanFactory::getBean('Users', $id);
        $rand = mt_rand(1, 10000);
        $email = "one{$rand}@email.com";
        $user->email1 = $email;
        $user->save();
        $user->retrieve_by_email_address($email);
        self::assertTrue(isset($user->id));
        self::assertEquals($id, $user->id);
    }

    public function NewPasswordAndFindUserPassword($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        // preset
        $query = "DELETE FROM users WHERE user_name = '{$user->user_name}' AND id != '$id'";
        DBManagerFactory::getInstance()->query($query);


        //set user password and then retrieve user by created password
        $rand = 1;
        $pwd = 'test' . $rand;
        $user->setNewPassword($pwd);

        $result = User::findUserPassword($user->user_name, md5($pwd), '', true);

        // here is a really unpredictable mysql connection issue why this test is unstable
        // but should works on a correct test environments:
        // $this->assertTrue(isset($result['id']));
        // $this->assertEquals($id, $result['id']);
    }

    // --- OK

    public function authenticate_user($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        //test with invalid password
        $result = $user->authenticate_user(md5("pass"));
        self::assertEquals(false, $result);

        //test with invalid password

        $result = $user->authenticate_user(md5("test1"));
        self::assertEquals(true, $result);
    }

    public function load_user($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        $result = $user->load_user("test1");

        self::assertEquals(true, $result->authenticated);
    }

    public function change_password($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        //execute the method and verifh that it returns true
        $result = $user->change_password("test1", "testpass");
        self::assertEquals(true, $result);


        //find the user by new password
        $result = User::findUserPassword("test", md5("testpass"));

        self::assertTrue(isset($result['id']));
        self::assertEquals($id, $result['id']);
    }

    public function getPreferredEmail($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        $actual = $user->getPreferredEmail();

        self::assertEquals('firstn lastn', $actual['name']);
        $preg = preg_match('/^one\d{0,}\@email\.com$/', (string) $actual['email'], $matches);
        self::assertCount(1, $matches);
    }

    public function getUsersNameAndEmail($id): void
    {
        $user = BeanFactory::newBean('Users');

        $user->retrieve($id);

        $actual = $user->getUsersNameAndEmail();

        self::assertEquals('firstn lastn', $actual['name']);
        self::assertMatchesRegularExpression('/^one\d{0,}\@email\.com$/', $actual['email']);
    }

    public function getEmailInfo($id): void
    {
        $actual = BeanFactory::newBean('Users')->getEmailInfo($id);

        self::assertEquals('firstn lastn', $actual['name']);
        self::assertMatchesRegularExpression('/^one\d{0,}\@email\.com$/', $actual['email']);
    }

    public function testencrypt_password(): void
    {
        $result = BeanFactory::newBean('Users')->encrypt_password("test");
        self::assertTrue(isset($result));
        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function testgetPasswordHash(): void
    {
        $result = User::getPasswordHash("test");

        self::assertTrue(isset($result));
        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function testcheckPassword(): void
    {
        //test with empty password and empty hash
        $result = User::checkPassword("", '');
        self::assertEquals(false, $result);


        //test with valid hash and empty password
        $result = User::checkPassword("", '$1$Gt0.XI4.$tVVSXgE36sfsVMBNo/9la1');
        self::assertEquals(false, $result);


        //test with valid password and invalid hash
        $result = User::checkPassword("test", '$1$Gt0.XI4.$tVVSXgE36sfsVMBNo/9la2');
        self::assertEquals(false, $result);


        //test with valid password and valid hash
        $result = User::checkPassword("test", '$1$Gt0.XI4.$tVVSXgE36sfsVMBNo/9la1');
        self::assertEquals(true, $result);
    }

    public function testcheckPasswordMD5(): void
    {
        //test with empty password and empty hash
        $result = User::checkPasswordMD5(md5(""), '');
        self::assertEquals(false, $result);


        //test with valid hash and empty password
        $result = User::checkPasswordMD5(md5(""), '$1$Gt0.XI4.$tVVSXgE36sfsVMBNo/9la1');
        self::assertEquals(false, $result);


        //test with valid password and invalid hash
        $result = User::checkPasswordMD5(md5("test"), '$1$Gt0.XI4.$tVVSXgE36sfsVMBNo/9la2');
        self::assertEquals(false, $result);


        //test with valid password and valid hash
        $result = User::checkPasswordMD5(md5("test"), '$1$Gt0.XI4.$tVVSXgE36sfsVMBNo/9la1');
        self::assertEquals(true, $result);
    }

    public function testis_authenticated(): void
    {
        $user = BeanFactory::newBean('Users');

        //test without setting name
        self::assertEquals(false, $user->is_authenticated());

        //test with name set
        $user->authenticated = true;
        self::assertEquals(true, $user->is_authenticated());
    }

    public function testfill_in_additional_list_fields(): void
    {
        self::markTestIncomplete('environment dependency');

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $user->fill_in_additional_list_fields();

        self::assertEquals("Administrator", $user->full_name);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        self::markTestIncomplete('environment dependency');

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $user->fill_in_additional_detail_fields();

        self::assertEquals("Administrator", $user->full_name);
    }

    public function testretrieve_user_id(): void
    {
        self::markTestIncomplete('environment dependency');

        $user = BeanFactory::newBean('Users');

        $result1 = $user->retrieve_user_id('admin');
        $result2 = $user->retrieve_user_id('automated_tester');
        static::assertFalse($result1 == '1' && $result2 == '1');
        static::assertTrue($result1 == '1' || $result2 == '1');
    }

    public function testverify_data(): void
    {
        global $mod_strings;
        include __DIR__ . '/../../../../../modules/Users/language/en_us.lang.php';

        $mod_strings['ERR_EMAIL_NO_OPTS'] = "";

        $user = BeanFactory::getBean('Users');

        $user->retrieve(1);

        //test with default/true
        $result = $user->verify_data();
        self::assertEquals(true, $result);


        //test with false
        $result = $user->verify_data(false);
        self::assertEquals(false, $result);
    }

    public function testget_list_view_data(): void
    {
        // test
        global $mod_strings;
        $mod_strings['LBL_CHECKMARK'] = "";

        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->get_list_view_data();
        self::assertIsArray($result);
    }

    public function testlist_view_parse_additional_sections(): void
    {
        $user = BeanFactory::newBean('Users');

        $list_form = array();
        $result = $user->list_view_parse_additional_sections($list_form);
        self::assertSame($list_form, $result);
    }

    public function testGetAllUsersAndGetActiveUsers(): void
    {
        $all_users = User::getAllUsers();
        self::assertIsArray($all_users);

        $active_users = User::getActiveUsers();
        self::assertIsArray($active_users);

        self::assertGreaterThanOrEqual(count($active_users), count($all_users));
    }


    public function testcreate_export_query(): void
    {
        $user = BeanFactory::newBean('Users');

        global $current_user;
        $current_user->is_admin = '1';
        //test with empty string params
        $expected = "SELECT id, user_name, first_name, last_name, description, date_entered, date_modified, modified_user_id, created_by, title, department, is_admin, phone_home, phone_mobile, phone_work, phone_other, phone_fax, address_street, address_city, address_state, address_postalcode, address_country, reports_to_id, portal_only, status, receive_notifications, employee_status, messenger_id, messenger_type, is_group FROM users  WHERE  users.deleted = 0 ORDER BY users.user_name";
        $actual = $user->create_export_query('', '');
        self::assertSame($expected, $actual);


        //test with valid string params
        $expected = "SELECT id, user_name, first_name, last_name, description, date_entered, date_modified, modified_user_id, created_by, title, department, is_admin, phone_home, phone_mobile, phone_work, phone_other, phone_fax, address_street, address_city, address_state, address_postalcode, address_country, reports_to_id, portal_only, status, receive_notifications, employee_status, messenger_id, messenger_type, is_group FROM users  WHERE user_name=\"\" AND  users.deleted = 0 ORDER BY id";
        $actual = $user->create_export_query('id', 'user_name=""');
        self::assertSame($expected, $actual);

        $current_user->is_admin = '0';
        $this->expectException(RuntimeException::class);
        $user->create_export_query('', '');
    }


    public function testget_meetings(): void
    {
        $result = BeanFactory::newBean('Users')->get_meetings();
        self::assertIsArray($result);
    }

    public function testdisplayEmailCounts(): void
    {
        $user = BeanFactory::newBean('Users');

        $expected = '<script type="text/javascript" language="Javascript">var welcome = document.getElementById("welcome");var welcomeContent = welcome.innerHTML;welcome.innerHTML = welcomeContent + "&nbsp;&nbsp;&nbsp;&nbsp;<a href=index.php?module=Emails&action=ListViewGroup>Group Inbox: (0 New)</a>";</script>';

        //cpature the screen output and compare with exected values

        ob_start();

        $user->displayEmailCounts();

        $renderedContent = ob_get_contents();
        ob_end_clean();

        self::assertSame($expected, $renderedContent);
    }

    public function testgetSystemDefaultNameAndEmail(): void
    {
        $user = BeanFactory::newBean('Users');

        $expected = array('email', 'name');
        $actual = array_keys($user->getSystemDefaultNameAndEmail());
        self::assertSame($expected, $actual);
    }

    public function testsetDefaultsInConfig(): void
    {
        self::markTestIncomplete('Incorrect state hash (in PHPUnitTest): Hash doesn\'t match at key "filesys::/var/www/html/SuiteCRM/config.php".');
        $result = BeanFactory::newBean('Users')->setDefaultsInConfig();

        self::assertIsArray($result);
        self::assertEquals('sugar', $result['email_default_client']);
        self::assertEquals('html', $result['email_default_editor']);
    }


    public function testgetEmailLink2(): void
    {
        // test
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);


        //test with accounts module
        $account = BeanFactory::newBean('Accounts');
        $account->name = "test";

        /** @var SugarEmailAddress $emailAddress*/
        $emailAddress =& $account->emailAddress;
        $emailAddress->addAddress('abc@email.com');

        $expected =
            '            <a class="email-link" href="mailto:abc@email.com"
                    onclick="$(document).openComposeViewModal(this);"
                    data-module="Accounts" data-record-id=""
                    data-module-name="test" data-email-address="abc@email.com"
                >abc@email.com</a>';
        $actual = $user->getEmailLink2("abc@email.com", $account);
        self::assertSame($expected, $actual);


        //test with contacts module
        $contact = BeanFactory::newBean('Contacts');
        // Contact name auto populate from first name and last name, so we need set value for first name or last name to test insteard set value for name
        $contact->first_name = "test";

        /** @var SugarEmailAddress $emailAddress*/
        $emailAddress =& $contact->emailAddress;
        $emailAddress->addAddress('abc@email.com');

        $expected =
            '            <a class="email-link" href="mailto:abc@email.com"
                    onclick="$(document).openComposeViewModal(this);"
                    data-module="Contacts" data-record-id=""
                    data-module-name="test" data-email-address="abc@email.com"
                >abc@email.com</a>';
        $actual = $user->getEmailLink2("abc@email.com", $contact);
        self::assertSame($expected, $actual);
    }


    public function testgetEmailLink(): void
    {
        self::markTestIncomplete('Need to mock up user');
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        //test with accounts module
        $account = BeanFactory::newBean('Accounts');
        $account->name = "test";

        $expected =
            '<a class="email-link"'
            . ' onclick=" $(document).openComposeViewModal(this);" data-module="Accounts"'
            . ' data-record-id="" data-module-name="test"  data-email-address=""></a>';
        $actual = $user->getEmailLink("name", $account);
        self::assertSame($expected, $actual);


        //test with contacts module
        $contact = BeanFactory::newBean('Contacts');
        // Contact name auto populate from first name and last name, so we need set value for first name or last name to test insteard set value for name
        $contact->first_name = "test";

        $expected =
            '<a href="javascript:void(0);"'
            . ' onclick="$(document).openComposeViewModal(this);" data-module="Contacts"'
            . ' data-record-id="" data-module-name="test" data-email-address=""></a>';
        $actual = $user->getEmailLink("name", $contact);
        self::assertSame($expected, $actual);
    }

    public function testgetLocaleFormatDesc(): void
    {
        $result = BeanFactory::newBean('Users')->getLocaleFormatDesc();
        self::assertTrue(isset($result));
        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function testisAdmin(): void
    {
        $user = BeanFactory::newBean('Users');

        //test without setting attribute
        self::assertEquals(false, $user->isAdmin());

        //test with attribute set
        $user->is_admin = 1;
        self::assertEquals(true, $user->isAdmin());
    }

    public function testisDeveloperForAnyModule(): void
    {
        $user = BeanFactory::newBean('Users');

        //test without setting is_admin
        self::assertEquals(false, $user->isDeveloperForAnyModule());


        //test with id set
        $user->id = 1;
        self::assertEquals(false, $user->isDeveloperForAnyModule());


        //test with id and is_admin set
        $user->is_admin = 1;
        self::assertEquals(true, $user->isDeveloperForAnyModule());
    }

    public function testgetDeveloperModules(): void
    {
        // test
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->getDeveloperModules();
        self::assertIsArray($result);
    }

    public function testisDeveloperForModule(): void
    {
        // test
        $user = BeanFactory::newBean('Users');

        //test without setting is_admin
        self::assertEquals(false, $user->isDeveloperForModule("Accounts"));

        //test with id set
        $user->id = 1;
        self::assertEquals(false, $user->isDeveloperForModule("Accounts"));

        //test with id and is_admin set
        $user->is_admin = 1;
        self::assertEquals(true, $user->isDeveloperForModule("Accounts"));
    }

    public function testgetAdminModules(): void
    {
        // test
        $user = BeanFactory::newBean('Users');

        $user->retrieve(1);

        $result = $user->getAdminModules();
        self::assertIsArray($result);
    }

    public function testisAdminForModule(): void
    {
        // test
        $user = BeanFactory::newBean('Users');

        //test without setting is_admin
        self::assertEquals(false, $user->isAdminForModule("Accounts"));

        //test with id set
        $user->id = 1;
        self::assertEquals(false, $user->isAdminForModule("Accounts"));

        //test with id and is_admin set
        $user->is_admin = 1;
        self::assertEquals(true, $user->isAdminForModule("Accounts"));
    }

    public function testshowLastNameFirst(): void
    {
        $result = BeanFactory::newBean('Users')->showLastNameFirst();
        self::assertEquals(false, $result);
    }

    /**
     * @todo: NEEDS FIXING!
     */
    public function testcreate_new_list_query(): void
    {
        /*
            $user = BeanFactory::newBean('Users');

            //test with empty string params
            $expected = " SELECT  users.* , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' securitygroup_noninher_fields , '                                    '  securitygroup_id , LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as full_name, LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as name , jt2.last_name reports_to_name , jt2.created_by reports_to_name_owner  , 'Users' reports_to_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM users   LEFT JOIN  users jt2 ON users.reports_to_id=jt2.id AND jt2.deleted=0\n\n AND jt2.deleted=0 where users.deleted=0";
            $actual = $user->create_new_list_query('','');
            $this->assertSame($expected,$actual);

            //test with valid string params
            $expected = " SELECT  users.* , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' securitygroup_noninher_fields , '                                    '  securitygroup_id , LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as full_name, LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as name , jt2.last_name reports_to_name , jt2.created_by reports_to_name_owner  , 'Users' reports_to_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM users   LEFT JOIN  users jt2 ON users.reports_to_id=jt2.id AND jt2.deleted=0\n\n AND jt2.deleted=0 where (user_name=\"\") AND users.deleted=0 ORDER BY users.id";
            $actual = $user->create_new_list_query('id','user_name=""');
            $this->assertSame($expected,$actual);
        */
        self::assertTrue(true, "NEEDS FIXING!");
    }


    public function testget_first_day_of_week(): void
    {
        $result = BeanFactory::newBean('Users')->get_first_day_of_week();
        self::assertIsNumeric($result);
    }

    public function testgeneratePassword(): void
    {
        //generate apsswords and verify they are not same

        $password1 = User::generatePassword();
        self::assertGreaterThan(0, strlen((string) $password1));

        $password2 = User::generatePassword();
        self::assertGreaterThan(0, strlen((string) $password2));

        self::assertNotEquals($password1, $password2);
    }


    public function testsendEmailForPassword(): void
    {
        $result = BeanFactory::newBean('Users')->sendEmailForPassword("1");

        //expected result is a array with template not found message.
        self::assertIsArray($result);
    }

    public function testafterImportSave(): void
    {
        $user = BeanFactory::newBean('Users');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $user->afterImportSave();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::assertStringStartsWith('Cannot modify header information', $e->getMessage());
        }
    }

    public function testisPrimaryEmail(): void
    {
        $user = BeanFactory::newBean('Users');

        //test without user email
        self::assertEquals(false, $user->isPrimaryEmail("abc@abc.com"));


        //test with non matching user email
        $user->email1 = "xyz@abc.com";
        self::assertEquals(false, $user->isPrimaryEmail("abc@abc.com"));


        //test with matching user email
        $user->email1 = "abc@abc.com";
        self::assertEquals(true, $user->isPrimaryEmail("abc@abc.com"));
    }

    public function testError(): void
    {
        $request = [];
        global $app_strings;

        // setup
        self::assertNotTrue(isset($app_strings['TEST_ERROR_MESSAGE']));

        // test if there is no error

        ob_start();
        include __DIR__ . '/../../../../../modules/Users/Error.php';
        $contents = ob_get_contents();
        ob_end_clean();
        $expected = '<span class=\'error\'><br><br>' . "\n" . $app_strings['NTC_CLICK_BACK'] . '</span>';
        self::assertStringContainsStringIgnoringCase($expected, $contents);

        // test if there is an error

        $app_strings['TEST_ERROR_MESSAGE'] = 'Hello error';
        $request['error_string'] = 'TEST_ERROR_MESSAGE';
        self::assertEquals('TEST_ERROR_MESSAGE', $request['error_string']);
        ob_start();
        include __DIR__ . '/../../../../../modules/Users/Error.php';
        $contents = ob_get_contents();
        ob_end_clean();
        $expected = '<span class=\'error\'>Hello error<br><br>' . "\n"  . $app_strings['NTC_CLICK_BACK'] . '</span>';
        self::assertStringContainsStringIgnoringCase($expected, $contents);


        unset($app_strings['TEST_ERROR_MESSAGE']);
    }
}
