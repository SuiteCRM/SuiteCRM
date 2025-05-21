<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SuiteCRM\Test\TestLogger;

class SugarEmailAddressTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var SugarEmailAddress
     */
    protected $ea;

    /**
     * Sets up the fixture, for example, open a network connection.
     * This method is called before a test is executed.
     */
    protected function setUp(): void
    {
        parent::setUp();
        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');

        $this->ea = new SugarEmailAddress();
    }

    /**
     * Tears down the fixture, for example, close a network connection.
     * This method is called after a test is executed.
     */
    protected function tearDown(): void
    {
        $db = DBManagerFactory::getInstance();
        $query = /** @lang sql */
            "DELETE FROM email_addresses WHERE email_address = 'test9@email.com'";
        $db->query($query);
        $query = /** @lang sql */
            "DELETE FROM aod_indexevent WHERE record_id = 'test_contact_1'";
        $db->query($query);
        $query = /** @lang sql */
            "DELETE FROM contacts_cstm WHERE id_c = 'test_contact_1'";
        $db->query($query);
        $query = /** @lang sql */
            "DELETE FROM sugarfeed WHERE related_id = 'test_contact_1'";
        $db->query($query);

        parent::tearDown();
    }

    /**
     * Test for save() method.
     */
    public function testSave(): void
    {
        $query = "SELECT * FROM email_addr_bean_rel";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddrBeanRel = $rows;


        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        // test
        /** @noinspection PhpDeprecationInspection */
        $this->ea->save(null, null, null, null, null, null, null, null);
        self::assertCount(1, $GLOBALS['log']->calls['deprecated']);

        $GLOBALS['log'] = $logger;

        DBManagerFactory::getInstance()->query("DELETE FROM email_addr_bean_rel");
        foreach ($tableEmailAddrBeanRel as $row) {
            $query = "INSERT email_addr_bean_rel INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    /**
     * Tests for constructor.
     */
    public function testConstruct(): void
    {
        // test
        $ea = new SugarEmailAddress();
        $indexBefore = $ea->index;
        $countBefore = $ea::$count;
        $ea = new SugarEmailAddress();
        $indexAfter = $ea->index;
        $countAfter = $ea::$count;
        self::assertEquals($indexBefore, $indexAfter - 1);
        self::assertEquals($countBefore, $countAfter - 1);
    }

    /**
     * Tests old constructor.
     */
    public function testSugarEmailAddress(): void
    {
        // test
        $ea = new SugarEmailAddress();
        $indexBefore = $ea->index;
        $countBefore = $ea::$count;
        $ea = new SugarEmailAddress();
        $indexAfter = $ea->index;
        $countAfter = $ea::$count;
        self::assertEquals($indexBefore, $indexAfter - 1);
        self::assertEquals($countBefore, $countAfter - 1);
    }

    /**
     * Test for handleLegacySave() method.
     */
    public function testHandleLegacySave(): void
    {
        if (isset($_REQUEST)) {
            $_request = $_REQUEST;
        }

        $c = BeanFactory::newBean('Contacts');

        // test
        if (!empty($_REQUEST)) {
            $req = $_REQUEST;
        }

        $_REQUEST['useEmailWidget'] = true;
        $_REQUEST['massupdate'] = true;
        $this->ea->handleLegacySave($c);
        self::assertCount(0, $this->ea->addresses);

        if (!empty($req)) {
            $_REQUEST = $req;
        } else {
            unset($_REQUEST);
        }

        // test
        $c->email1 = 'test@email.com';
        $c->email2 = 'test2@email.com';
        $this->ea->handleLegacySave($c);
        self::assertCount(2, $this->ea->addresses);
        self::assertSame('test@email.com', $this->ea->addresses[0]['email_address']);
        self::assertSame('1', $this->ea->addresses[0]['primary_address']);
        self::assertSame('0', $this->ea->addresses[0]['reply_to_address']);
        self::assertSame('0', $this->ea->addresses[0]['invalid_email']);
        self::assertSame('0', $this->ea->addresses[0]['opt_out']);
        self::assertNull($this->ea->addresses[0]['email_address_id']);
        self::assertSame('test2@email.com', $this->ea->addresses[1]['email_address']);
        self::assertSame('0', $this->ea->addresses[1]['primary_address']);
        self::assertSame('0', $this->ea->addresses[1]['reply_to_address']);
        self::assertSame('0', $this->ea->addresses[1]['invalid_email']);
        self::assertSame('0', $this->ea->addresses[1]['opt_out']);
        self::assertNull($this->ea->addresses[1]['email_address_id']);

        if (isset($_request)) {
            $_REQUEST = $_request;
        } else {
            unset($_REQUEST);
        }
    }

    /**
     * Test for handleLegacyRetrieve() method.
     */
    public function testHandleLegacyRetrieve(): void
    {
        $c = BeanFactory::newBean('Contacts');

        // test
        $this->ea->handleLegacyRetrieve($c);
        self::assertFalse($c->fetched_row);

        // test
        $c->email1 = 'test5@email.com';
        $this->ea->handleLegacyRetrieve($c);
        self::assertSame(array(
            'email1' => 'test5@email.com',
        ), $c->fetched_row);
    }

    /**
     * Test for populateLegacyFields() method.
     */
    public function testPopulateLegacyFields(): void
    {
        $c = BeanFactory::newBean('Contacts');

        // test
        $this->ea->populateLegacyFields($c);
        self::assertEquals(null, $c->email1);
        self::assertEquals(null, $c->email_opt_out);
        self::assertEquals(null, $c->invalid_email);
        self::assertEquals(null, $c->email2);

        // test
        $this->ea->addAddress('test6@email.com');
        $this->ea->addAddress('test7@email.com');
        $this->ea->populateLegacyFields($c);
        self::assertEquals('test6@email.com', $c->email1);
        self::assertEquals(0, $c->email_opt_out);
        self::assertEquals(0, $c->invalid_email);
        self::assertEquals('test7@email.com', $c->email2);

        // test
        $this->ea->addresses[1]['primary_address'] = 1;
        $this->ea->populateLegacyFields($c);
        self::assertEquals('test7@email.com', $c->email1);
        self::assertEquals(0, $c->email_opt_out);
        self::assertEquals(0, $c->invalid_email);
        self::assertEquals('test6@email.com', $c->email2);
    }

    /**
     * Test for saveEmail() method.
     */
    public function testSaveEmail(): void
    {
        if (isset($_REQUEST)) {
            $_request = $_REQUEST;
        }

        $query = "SELECT * FROM email_addr_bean_rel";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddrBeanRel = $rows;

        $query = "SELECT * FROM contacts";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableContacts = $rows;

        $query = "SELECT * FROM email_addresses";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddresses = $rows;



        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        if (!empty($_REQUEST)) {
            $req = $_REQUEST;
        }

        $db = DBManagerFactory::getInstance();

        // test
        $db->query(/** @lang sql */
            "delete  from email_addr_bean_rel  WHERE email_addr_bean_rel.bean_id = '' AND email_addr_bean_rel.bean_module = '' and email_addr_bean_rel.deleted=0"
        );

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(0, $row['cnt']);

        $this->ea->saveEmail(false, null);
        self::assertCount(1, $GLOBALS['log']->calls['fatal']);

        // test

        $this->ea->addAddress('test8@email.com');
        $this->ea->addAddress('test9@email.com');

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(0, $row['cnt']);

        $this->ea->saveEmail(false, null);
        self::assertCount(2, $GLOBALS['log']->calls['fatal']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(2, $row['cnt']);

        // test

        $this->ea->addAddress('test10@email.com');
        $this->ea->addAddress('test11@email.com');

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(2, $row['cnt']);

        $this->ea->saveEmail(false, null);
        self::assertCount(3, $GLOBALS['log']->calls['fatal']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(4, $row['cnt']);

        // test

        $this->ea->addAddress('test12@email.com');
        $this->ea->addAddress('test13@email.com');
        $this->ea->addresses[1]['primary_address'] = 1;

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(4, $row['cnt']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 AND primary_address = 1";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(0, $row['cnt']);

        $this->ea->saveEmail(false, null);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 AND primary_address = 1 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(1, $row['cnt']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(6, $row['cnt']);

        // test

        $_REQUEST['action'] = 'ConvertLead';

        $this->ea->addAddress('test14@email.com');
        $this->ea->addAddress('test15@email.com');
        $this->ea->addresses[1]['primary_address'] = 1;

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(6, $row['cnt']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 AND primary_address = 1";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(1, $row['cnt']);

        $this->ea->saveEmail(false, null);
        self::assertCount(5, $GLOBALS['log']->calls['fatal']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 AND primary_address = 1 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(1, $row['cnt']);

        $q = /** @lang sql */
            "select count(*) as cnt from email_addr_bean_rel eabr WHERE eabr.bean_id = '' AND eabr.bean_module = '' and eabr.deleted=0 ";
        $r = $db->query($q);
        $row = $db->fetchByAssoc($r);
        self::assertEquals(8, $row['cnt']);

        if (!empty($req)) {
            $_REQUEST = $req;
        } else {
            unset($_REQUEST);
        }

        $GLOBALS['log'] = $logger;



        if (isset($_request)) {
            $_REQUEST = $_request;
        } else {
            unset($_REQUEST);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM email_addresses");
        foreach ($tableEmailAddresses as $row) {
            $query = "INSERT email_addresses INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM contacts");
        foreach ($tableContacts as $row) {
            $query = "INSERT contact INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM email_addr_bean_rel");
        foreach ($tableEmailAddrBeanRel as $row) {
            $query = "INSERT email_addr_bean_rel INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    /**
     * Test for getCountEmailAddressByBean() method.
     */
    public function testGetCountEmailAddressByBean(): void
    {
        $query = "SELECT * FROM email_addresses";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddresses = $rows;

        $c = BeanFactory::getBean('Contacts');
        $result0 = $this->ea->getCountEmailAddressByBean('test12@email.com', $c, 0);
        $result1 = $this->ea->getCountEmailAddressByBean('test13@email.com', $c, 1);
        self::assertEquals(0, $result0);
        self::assertEquals(0, $result1);

        // test
        $i = 1;
        $db = DBManagerFactory::getInstance();

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);

        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);
        $c->id = 'test_contact_1';

        $result0 = $this->ea->getCountEmailAddressByBean('test@email.com', $c, 0);
        $result1 = $this->ea->getCountEmailAddressByBean('test@email.com', $c, 1);
        self::assertEquals(1, $result0);
        self::assertEquals(0, $result1);


        // test
        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);



        DBManagerFactory::getInstance()->query("DELETE FROM email_addresses");
        foreach ($tableEmailAddresses as $row) {
            $query = "INSERT email_addresses INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    /**
     * Test for getRelatedId() method.
     */
    public function testGetRelatedId(): void
    {
        // test
        $results = $this->ea->getRelatedId('test@email.com', 'Contacts');
        self::assertEquals(false, $results);

        // test
        $i = 1;
        $db = DBManagerFactory::getInstance();

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);

        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);

        $results = $this->ea->getRelatedId('test@email.com', 'Contacts');
        self::assertEquals(array('test_contact_1'), $results);

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);
    }

    /**
     * Test for getBeansByEmailAddress() method.
     */
    public function testGetBeansByEmailAddress(): void
    {
        global $locale, $current_user;

        if (empty($locale)) {
            $locale = new Localization();
        }

        if (empty($current_user)) {
            $current_user = BeanFactory::getBean('Users', 1);
        }


        $results = $this->ea->getBeansByEmailAddress('test@email.com');
        self::assertEquals(array(), $results);

        // test
        $i = 1;
        $db = DBManagerFactory::getInstance();

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);

        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);

        $results = $this->ea->getBeansByEmailAddress('test@email.com');
        $expected = BeanFactory::getBean('Contacts', 'test_contact_1');
        self::assertCount(1, $results);
        self::assertSame($expected->id, $results[0]->id);

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);

        // test

        $results = $this->ea->getBeansByEmailAddress('');
        self::assertEquals(array(), $results);

        // test
        $i = 2;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_invalid_{$i}', 'InvalidClass', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TESTINVALID@EMAIL.COM')";
        $db->query($q);

        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        $results = $this->ea->getBeansByEmailAddress('testinvalid@email.com');
        self::assertEquals(array(), $results);
        self::assertCount(1, $GLOBALS['log']->calls['fatal']);

        $GLOBALS['log'] = $logger;

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);

        // test if email address is deleted then no results
        $i = 3;
        $this->getBeansByEmailAddressDeletedLinksTest($db, $i, 1, 0);
        // test if bean is deleted then no results
        $i = 4;
        $this->getBeansByEmailAddressDeletedLinksTest($db, $i, 0, 1);

    }

    private function getBeansByEmailAddressDeletedLinksTest($db, $i, $ea_deleted, $bean_deleted): void
    {
        // must have differing values
        self::assertNotEquals($ea_deleted, $bean_deleted);
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps, deleted) VALUES ('test_email_{$i}', 'TEST{$i}@EMAIL.COM', $ea_deleted)";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id, first_name, deleted) VALUES ('test_contact_{$i}', 'Test_$i}', $bean_deleted)";
        $db->query($q);

        $results = $this->ea->getBeansByEmailAddress("test{$i}@email.com");
        self::assertEquals(array(), $results);

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);
    }

    /**
     * Test for populateAddresses() method.
     */
    public function testPopulateAddresses(): void
    {
        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        $db = DBManagerFactory::getInstance();


        // test
        $results = $this->ea->populateAddresses('', '');
        if (null !== $results) {
            throw new Exception('Assert error 1');
        }
        self::assertEquals(null, $results);

        // test
        $_REQUEST['emailAddressWidget'] = true;
        $results = $this->ea->populateAddresses('', '');
        if (null !== $results) {
            throw new Exception('Assert error 2');
        }
        self::assertEquals(null, $results);

        // test
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $results = $this->ea->populateAddresses('', $module);
        if (null !== $results) {
            throw new Exception('Assert error 3');
        }
        self::assertEquals(null, $results);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $results = $this->ea->populateAddresses('', $module);
        if (array() !== $this->ea->addresses) {
            throw new Exception('Assert error 4');
        }
        self::assertEquals(array(), $this->ea->addresses);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = array();
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddressReplyToFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalidemailAddressReplyToFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddressPrimaryFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalidemailAddressPrimaryFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddressOptOutFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalidemailAddressOptOutFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddressInvalidFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalidemailAddressInvalidFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddressDeleteFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalidemailAddressDeleteFlag'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddressId0'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $_REQUEST['non-exists-or-invalidemailAddress'] = true;
        $_REQUEST['emailAddressWidget'] = true;
        $results = $this->ea->populateAddresses('', $module);

        // test
        $i = 1;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);

        $q = /** @lang sql */
            "UPDATE email_addresses SET opt_out = 1, invalid_email = 0 WHERE email_address_caps = 'TEST@EMAIL.COM'";
        $db->query($q);
        unset($_REQUEST);
        $module = 'non-exists-or-invalid';
        $_REQUEST['non-exists-or-invalid_email_widget_id'] = true;
        $_REQUEST['non-exists-or-invalid1emailAddress0'] = true;
        $results = $this->ea->populateAddresses('', $module, array('emailAddress0' => 'test@email.com'));

        $q = /** @lang sql */
            "UPDATE email_addresses SET opt_out = 0, invalid_email = 1 WHERE email_address_caps = 'TEST@EMAIL.COM'";
        $db->query($q);
        $results = $this->ea->populateAddresses('', $module, array('emailAddress0' => 'test@email.com'));

        self::assertEquals(false, $results);
        self::assertSame(array(
            0 => array(
                'email_address' => 'test@email.com',
                'primary_address' => '0',
                'reply_to_address' => '0',
                'invalid_email' => '0',
                'opt_out' => '0',
                'email_address_id' => null,
                'confirm_opt_in_flag' => '0',
            ),
        ), $this->ea->addresses);

        $q = /** @lang sql */
            "UPDATE email_addresses SET opt_out = 1, invalid_email = 1 WHERE email_address_caps = 'TEST@EMAIL.COM'";
        $db->query($q);
        $results = $this->ea->populateAddresses('', $module, array('emailAddress0' => 'test@email.com'));
        self::assertSame(array(
            0 => array(
                'email_address' => 'test@email.com',
                'primary_address' => '0',
                'reply_to_address' => '0',
                'invalid_email' => '0',
                'opt_out' => '0',
                'email_address_id' => null,
                'confirm_opt_in_flag' => '0',
            ),
        ), $this->ea->addresses);

        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);


        $GLOBALS['log'] = $logger;

    }

    /**
     * Test for addAddress() method.
     */
    public function testAddAddress(): void
    {
        $query = "SELECT * FROM email_addr_bean_rel";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddrBeanRel = $rows;

        $query = "SELECT * FROM contacts";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableContacts = $rows;

        $query = "SELECT * FROM email_addresses";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddresses = $rows;


        $this->ea->addAddress('test20@email.com', true);
        $this->ea->addAddress('test21@email.com', true);
        self::assertSame(array(
            0 => array(
                'email_address' => 'test20@email.com',
                'primary_address' => '0',
                'reply_to_address' => '0',
                'invalid_email' => '0',
                'opt_out' => '0',
                'email_address_id' => null,
                'confirm_opt_in_flag' => null,
            ),
            1 => array(
                'email_address' => 'test21@email.com',
                'primary_address' => '1',
                'reply_to_address' => '0',
                'invalid_email' => '0',
                'opt_out' => '0',
                'email_address_id' => null,
                'confirm_opt_in_flag' => null
            ),
        ), $this->ea->addresses);



        DBManagerFactory::getInstance()->query("DELETE FROM email_addresses");
        foreach ($tableEmailAddresses as $row) {
            $query = "INSERT email_addresses INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM contacts");
        foreach ($tableContacts as $row) {
            $query = "INSERT contact INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM email_addr_bean_rel");
        foreach ($tableEmailAddrBeanRel as $row) {
            $query = "INSERT email_addr_bean_rel INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    /**
     * Test for updateFlags() method.
     */
    public function testUpdateFlags(): void
    {
        self::markTestIncomplete('testUpdateFlags');
//        // test
//        $i = 1;
//        $q = /** @lang sql */
//            "
//          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
//          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
//        ";
//        $db->query($q);
//        $q = /** @lang sql */
//            "INSERT INTO email_addresses (id, email_address, email_address_caps) VALUES ('test_email_{$i}', 'test@email.com', 'TEST@EMAIL.COM')";
//        $db->query($q);
//        $q = /** @lang sql */
//            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
//        $db->query($q);
//
//        $this->ea->addAddress('test@email.com', true);
//
//        $q = /** @lang sql */
//            "UPDATE email_addresses SET opt_out = 0, invalid_email = 1 WHERE email_address_caps = 'TEST@EMAIL.COM'";
//        $db->query($q);
//
//        $this->ea->updateFlags();
//
//        $q = /** @lang sql */
//            "SELECT * FROM email_addresses WHERE email_address_caps = 'TEST@EMAIL.COM'";
//        $r = $db->query($q);
//        $a = $db->fetchByAssoc($r);
//
//        self::assertSame(array(
//            'id' => $a['id'],
//            'email_address' => 'test@email.com',
//            'email_address_caps' => 'TEST@EMAIL.COM',
//            'invalid_email' => '0',
//            'opt_out' => '0',
//            'confirm_opt_in' => null,
//            'confirm_opt_in_date' => null,
//            'confirm_opt_in_sent_date' => null,
//            'date_created' => $a['date_created'],
//            'date_modified' => $a['date_modified'],
//            'deleted' => '0',
//        ), $a);
//
//
//        $q = /** @lang sql */
//            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
//        $db->query($q);
//        $q = /** @lang sql */
//            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
//        $db->query($q);
//        $q = /** @lang sql */
//            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
//        $db->query($q);
    }

    /**
     * Test for splitEmailAddress() method.
     */
    public function testSplitEmailAddress(): void
    {
        $result = $this->ea->splitEmailAddress(null);
        self::assertSame(array(
            'name' => '',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('invalid');
        self::assertSame(array(
            'name' => 'invalid',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('test@email.com');
        self::assertSame(array(
            'name' => '',
            'email' => 'test@email.com',
        ), $result);

        $result = $this->ea->splitEmailAddress('<Mr. Tester> test@email.com');
        self::assertSame(array(
            'name' => 'Mr. Tester test@email.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('<Mr. Tester test@email.com');
        self::assertSame(array(
            'name' => 'Mr. Tester test@email.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('Mr. Tester> test@email.com');
        self::assertSame(array(
            'name' => 'Mr. Tester test@email.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('te st@email.com');
        self::assertSame(array(
            'name' => 'te st@email.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('te st@ema il.com');
        self::assertSame(array(
            'name' => 'te st@ema il.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('te st@em>a il.com');
        self::assertSame(array(
            'name' => 'te st@ema il.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('te <st@ema il.com');
        self::assertSame(array(
            'name' => 'te st@ema il.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('te st@em>a il.com');
        self::assertSame(array(
            'name' => 'te st@ema il.com',
            'email' => '',
        ), $result);

        $result = $this->ea->splitEmailAddress('te <st@em>a il.com');
        self::assertSame(array(
            'name' => 'te st@ema il.com',
            'email' => '',
        ), $result);
    }

    /**
     * Test for _cleanAddress() method.
     */
    public function testCleanAddress(): void
    {
        $result = $this->ea->_cleanAddress(null);
        self::assertEquals('', $result);

        $result = $this->ea->_cleanAddress('<test>');
        self::assertEquals('test', $result);

        $result = $this->ea->_cleanAddress('<test@email.com>');
        self::assertEquals('test@email.com', $result);
    }

    /**
     * Test for getEmailGUID() method.
     */
    public function testGetEmailGUID(): void
    {
        $isValidator = new \SuiteCRM\Utility\SuiteValidator();


        $query = "SELECT * FROM email_addr_bean_rel";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddrBeanRel = $rows;

        $query = "SELECT * FROM contacts";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableContacts = $rows;

        $query = "SELECT * FROM email_addresses";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddresses = $rows;


        $db = DBManagerFactory::getInstance();

        // test
        $q = /** @lang sql */
            "
          DELETE FROM email_addresses
            WHERE
              email_address_caps = ''";
        $db->query($q);

        $result = $this->ea->getEmailGUID(null);
        self::assertEquals('', $result);

        // test
        $result = $this->ea->getEmailGUID('non-valid');
        self::assertTrue($isValidator->isValidId($result));

        // test
        $result = $this->ea->getEmailGUID('nonexists@nihil.com');
        self::assertTrue($isValidator->isValidId($result));

        // test
        $q = /** @lang sql */
            "
          DELETE FROM email_addresses
            WHERE
              email_address_caps = 'TEST@EMAIL.COM'";
        $db->query($q);

        $result = $this->ea->getEmailGUID('test@email.com');
        self::assertTrue($isValidator->isValidId($result));




        DBManagerFactory::getInstance()->query("DELETE FROM email_addresses");
        foreach ($tableEmailAddresses as $row) {
            $query = "INSERT email_addresses INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM contacts");
        foreach ($tableContacts as $row) {
            $query = "INSERT contact INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM email_addr_bean_rel");
        foreach ($tableEmailAddrBeanRel as $row) {
            $query = "INSERT email_addr_bean_rel INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

    }

    /**
     * Test for AddUpdateEmailAddress() method.
     */
    public function testAddUpdateEmailAddress(): void
    {
        // test
        $db = DBManagerFactory::getInstance();

        // test
        $result = $this->ea->AddUpdateEmailAddress(null);
        self::assertEquals('', $result);

        // test
        $result = $this->ea->AddUpdateEmailAddress(null, 0, 0, 1);
        self::assertEquals('', $result);


        // test
        $i = 1;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address, email_address_caps) VALUES ('test_email_{$i}', 'test@email.com', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);

        $result = $this->ea->AddUpdateEmailAddress(null, 0, 0, "test_email_{$i}");
        self::assertEquals('', $result);

        // test
        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);
    }

    /**
     * Test for getPrimaryAddress() method.
     */
    public function testGetPrimaryAddress(): void
    {
        $db = DBManagerFactory::getInstance();

        $c = BeanFactory::newBean('Contacts');

        // test
        $result = $this->ea->getPrimaryAddress($c);
        self::assertEquals('', $result);


        // test
        $i = 1;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address, email_address_caps) VALUES ('test_email_{$i}', 'test@email.com', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);

        $c->id = "test_contact_{$i}";
        $result = $this->ea->getPrimaryAddress($c);
        self::assertEquals('test@email.com', $result);

        // test
        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);
    }

    /**
     * Test for getReplyToAddress() method.
     */
    public function testGetReplyToAddress(): void
    {
        $db = DBManagerFactory::getInstance();

        $c = BeanFactory::getBean('Contacts');


        // test
        $i = 1;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address, email_address_caps) VALUES ('test_email_{$i}', 'test@email.com', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);

        $c->id = "test_contact_{$i}";

        $query = /** @lang sql */
            "UPDATE email_addr_bean_rel SET deleted = 0 WHERE email_address_id = 'test_email_{$i}'";
        $db->query($query);

        $result = $this->ea->getReplyToAddress($c);
        self::assertEquals('test@email.com', $result);

        $query = /** @lang sql */
            "UPDATE email_addr_bean_rel SET reply_to_address = 1 WHERE email_address_id = 'test_email_{$i}'";
        $db->query($query);

        $result = $this->ea->getReplyToAddress($c, true);
        self::assertEquals('test@email.com', $result);

        // test
        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);


        // test
        $result = $this->ea->getReplyToAddress($c);
        self::assertEquals('', $result);

        // test
        $result = $this->ea->getReplyToAddress($c, true);
        self::assertEquals('', $result);
    }

    /**
     * Test for getAddressesByGUID() method.
     */
    public function testGetAddressesByGUID(): void
    {
        $db = DBManagerFactory::getInstance();

        // test
        $id = null;
        $module = null;

        $query = /** @lang sql */
            "
            SELECT count(*) as cnt
                FROM email_addresses ea
                LEFT JOIN email_addr_bean_rel ear ON ea.id = ear.email_address_id
                WHERE ear.bean_module = '' AND ear.bean_id = '' AND ear.deleted = 0";
        $r = $db->query($query);
        $row = $db->fetchByAssoc($r);
        $cnt = $row['cnt'];

        $result = $this->ea->getAddressesByGUID($id, $module);
        $count = is_countable($result) ? count($result) : 0;
        self::assertEquals($cnt, $count);

        // test
        $id = 'non-exists-xyz';
        $module = null;

        $result = $this->ea->getAddressesByGUID($id, $module);
        $count = is_countable($result) ? count($result) : 0;
        self::assertEquals(0, $count);

        // test
        /**
         * @var Contact $c
         */
        $c = BeanFactory::getBean('Contacts');
        $c->email1 = 'test30@email.com';

        $id = $c->id;
        $module = $c->module_name;

        $result = $this->ea->getAddressesByGUID($id, $module);
        self::assertCount(0, $result);
    }

    /**
     * Test for getEmailAddressWidgetEditView() method.
     */
    public function testGetEmailAddressWidgetEditView(): void
    {
        $query = "SELECT * FROM accounts_cstm";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableAccountsCstm = $rows;


        $db = DBManagerFactory::getInstance();

        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        $env = array();
        if (isset($_POST)) {
            $env['$_POST'] = $_POST;
        }
        if (isset($_GET)) {
            $env['$_GET'] = $_GET;
        }
        if (isset($_REQUEST)) {
            $env['$_REQUEST'] = $_REQUEST;
        }

        $i = 1;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);


        // test
        $result = $this->ea->getEmailAddressWidgetEditView(null, null);
        self::assertEquals(false, $result);
        self::assertCount(1, $GLOBALS['log']->calls['fatal']);

        $result = $this->ea->getEmailAddressWidgetEditView('non-exists', null);
        self::assertEquals(false, $result);
        self::assertCount(2, $GLOBALS['log']->calls['fatal']);

        $result = $this->ea->getEmailAddressWidgetEditView(null, 'non-exists');
        self::assertEquals(false, $result);
        self::assertCount(3, $GLOBALS['log']->calls['fatal']);

        // test
        $_REQUEST['full_form'] = true;
        $_REQUEST['emailAddressWidget'] = true;
        $_REQUEST['non-exists-module0emailAddress0'] = true;

        $result = $this->ea->getEmailAddressWidgetEditView('', 'non-exists-module', true);

        self::assertFalse($result);


        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        // test
        $_REQUEST['full_form'] = true;
        $_REQUEST['emailAddressWidget'] = true;
        $_REQUEST['non-exists-module0emailAddress0'] = true;

        $result = $this->ea->getEmailAddressWidgetEditView('', 'non-exists-module');

        self::assertEquals(false, $result);

        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        // test
        $_REQUEST['full_form'] = true;
        $_REQUEST['emailAddressWidget'] = true;

        $result = $this->ea->getEmailAddressWidgetEditView('', 'non-exists-module');

        self::assertEquals(false, $result);

        $expected = !empty($result) && is_string($result);
        self::assertNotTrue($expected);

        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        // test
        $result = $this->ea->getEmailAddressWidgetEditView('non-exists-id', 'non-exists-module');

        self::assertEquals(false, $result);

        // test
        $_POST['is_converted'] = true;
        $result = $this->ea->getEmailAddressWidgetEditView('non-exists-id', 'non-exists-module');

        self::assertEquals(false, $result);

        $expected = !empty($result) && is_string($result);
        self::assertEquals(false, $expected);
        self::assertCount(10, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['is_converted'] = true;
        $_POST['return_id'] = 'any-non-exists-id';
        $result = $this->ea->getEmailAddressWidgetEditView('non-exists-id', 'non-exists-module');
        self::assertEquals(false, $result);

        self::assertCount(12, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['is_converted'] = true;
        $_POST['return_module'] = 'any-non-exists-module';
        $result = $this->ea->getEmailAddressWidgetEditView('non-exists-id', 'non-exists-module');

        self::assertEquals(false, $result);

        self::assertCount(13, $GLOBALS['log']->calls['fatal']);

        // test
        $result = $this->ea->getEmailAddressWidgetEditView('non-exists-id', 'Users');

        self::assertIsString($result);

        // test
        $_POST['return_id'] = 'test_contact_1';
        $_POST['return_module'] = 'Contacts';
        $result = $this->ea->getEmailAddressWidgetEditView('test_contact_1', 'Contacts');

        self::assertEquals(false, $result);

        self::assertFalse(strpos(
            (string) $result,
            '[{"email_address":null,"email_address_caps":"TEST@EMAIL.COM","invalid_email":"0","opt_out":"0","date_created":null,"date_modified":null,"id":"test_email_bean_rel_1","email_address_id":"test_email_1","bean_id":"test_contact_1","bean_module":"Contacts","primary_address":"0","reply_to_address":"0","deleted":"0"},{"email_address":null,"email_address_caps":"TEST@EMAIL.COM","invalid_email":"0","opt_out":"0","date_created":null,"date_modified":null,"id":"","email_address_id":"test_email_1","bean_id":"test_contact_1","bean_module":"Contacts","primary_address":"0","reply_to_address":"1","deleted":"0"}]'
        ));

        self::assertEquals(false, $result);

        // test
        $_POST['return_id'] = 'non-exists-id';
        $_POST['return_module'] = 'Contacts';
        $result = $this->ea->getEmailAddressWidgetEditView('test_contact_1', 'Contacts');

        self::assertEquals(false, $result);

        // test
        $_REQUEST['full_form'] = true;
        $_REQUEST['emailAddressWidget'] = true;
        $_REQUEST['non-exists-module0emailAddress0'] = true;
        $this->ea->view = 'QuickCreate';
        $_REQUEST['action'] = true;

        $result = $this->ea->getEmailAddressWidgetEditView('', 'non-exists-module');

        self::assertEquals(false, $result);

        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        // test
        $_REQUEST['full_form'] = true;
        $_REQUEST['emailAddressWidget'] = true;
        $_REQUEST['non-exists-module0emailAddress0'] = true;
        $this->ea->view = 'QuickCreate';
        $_REQUEST['action'] = true;

        $result = $this->ea->getEmailAddressWidgetEditView('', 'Contacts');

        self::assertEquals(false, $result);

        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        // test
        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);

        if (isset($env['$_POST'])) {
            $_POST = $env['$_POST'];
        }
        if (isset($env['$_GET'])) {
            $_GET = $env['$_GET'];
        }
        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        $GLOBALS['log'] = $logger;



        DBManagerFactory::getInstance()->query("DELETE FROM accounts_cstm");
        foreach ($tableAccountsCstm as $row) {
            $query = "INSERT accounts_cstm INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    /**
     * Test for getEmailAddressWidgetDetailView() method.
     */
    public function testGetEmailAddressWidgetDetailView(): void
    {
        $db = DBManagerFactory::getInstance();

        $i = 1;
        $q = /** @lang sql */
            "
          INSERT INTO email_addr_bean_rel (id, email_address_id, bean_id, bean_module, primary_address, deleted)
          VALUES ('test_email_bean_rel_{$i}', 'test_email_{$i}', 'test_contact_{$i}', 'Contacts', '0', '0')
        ";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO email_addresses (id, email_address_caps) VALUES ('test_email_{$i}', 'TEST@EMAIL.COM')";
        $db->query($q);
        $q = /** @lang sql */
            "INSERT INTO contacts (id) VALUES ('test_contact_{$i}')";
        $db->query($q);


        // test
        $result = $this->ea->getEmailAddressWidgetDetailView(null);
        self::assertEquals('', $result);

        // test
        $c = BeanFactory::getBean('Contacts');
        $result = $this->ea->getEmailAddressWidgetDetailView($c);
        self::assertEquals('', $result);

        // test
        $c = BeanFactory::getBean('Contacts');
        $c->id = 'an-non-exists-id';
        $result = $this->ea->getEmailAddressWidgetDetailView($c);
        self::assertNotFalse(strpos((string) $result, '--None--'));

        $expected = !empty($result) && is_string($result);
        self::assertTrue($expected);

        // test
        $c->id = "test_contact_{$i}";
        $result = $this->ea->getEmailAddressWidgetDetailView($c);
        self::assertFalse(strpos((string) $result, '--None--'));

        $expected = !empty($result) && is_string($result);
        self::assertTrue($expected);


        // test
        $q = /** @lang sql */
            "DELETE FROM email_addr_bean_rel WHERE id = 'test_email_bean_rel_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM email_addresses WHERE id = 'test_email_{$i}'";
        $db->query($q);
        $q = /** @lang sql */
            "DELETE FROM contacts WHERE id = 'test_contact_{$i}'";
        $db->query($q);
    }

    /**
     * Test for getEmailAddressWidgetDuplicatesView() method.
     */
    public function testGetEmailAddressWidgetDuplicatesView(): void
    {
        self::markTestIncomplete('environment dependecy in test');

        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        $env = array();
        if (isset($_POST)) {
            $env['$_POST'] = $_POST;
        }
        if (isset($_GET)) {
            $env['$_GET'] = $_GET;
        }
        if (isset($_REQUEST)) {
            $env['$_REQUEST'] = $_REQUEST;
        }


        // test
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals('', $result);
        self::assertCount(2, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals('', $result);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $_POST['emailAddress0'] = true;
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals(/** @lang html */
            '<input type="hidden" name="_email_widget_id" value="">
<input type="hidden" name="emailAddressWidget" value="">

<input type="hidden" name="emailAddress0" value="1">

<input type="hidden" name="emailAddressPrimaryFlag" value="">
<input type="hidden" name="useEmailWidget" value="true">',
            $result
        );
        self::assertCount(6, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $_POST['emailAddress0'] = true;
        $_POST['emailAddressOptOutFlag'] = true;
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals(/** @lang html */
            '<input type="hidden" name="_email_widget_id" value="">
<input type="hidden" name="emailAddressWidget" value="">

<input type="hidden" name="emailAddress0" value="1">

<input type="hidden" name="emailAddressPrimaryFlag" value="">
<input type="hidden" name="emailAddressOptOutFlag[]" value="1">
<input type="hidden" name="useEmailWidget" value="true">',
            $result
        );
        self::assertCount(9, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $_POST['emailAddressInvalidFlag'] = '';
        $_POST['emailAddress0'] = true;
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals(/** @lang html */
            '<input type="hidden" name="_email_widget_id" value="">
<input type="hidden" name="emailAddressWidget" value="">

<input type="hidden" name="emailAddress0" value="1">

<input type="hidden" name="emailAddressVerifiedValue0" value="">
<input type="hidden" name="emailAddressPrimaryFlag" value="">
<input type="hidden" name="emailAddressOptOutFlag[]" value="1">
<input type="hidden" name="emailAddressInvalidFlag[]" value="">
<input type="hidden" name="emailAddressReplyToFlag[]" value="">
<input type="hidden" name="emailAddressDeleteFlag[]" value="">
<input type="hidden" name="useEmailWidget" value="true">',
            $result
        );
        self::assertCount(13, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $_POST['emailAddressReplyToFlag'] = '';
        $_POST['emailAddress0'] = true;
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals(/** @lang html */
            '<input type="hidden" name="_email_widget_id" value="">
<input type="hidden" name="emailAddressWidget" value="">

<input type="hidden" name="emailAddress0" value="1">

<input type="hidden" name="emailAddressVerifiedValue0" value="">
<input type="hidden" name="emailAddressPrimaryFlag" value="">
<input type="hidden" name="emailAddressOptOutFlag[]" value="1">
<input type="hidden" name="emailAddressInvalidFlag[]" value="">
<input type="hidden" name="emailAddressReplyToFlag[]" value="">
<input type="hidden" name="emailAddressDeleteFlag[]" value="">
<input type="hidden" name="useEmailWidget" value="true">',
            $result
        );
        self::assertCount(18, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $_POST['emailAddressDeleteFlag'] = '';
        $_POST['emailAddress0'] = true;
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals(/** @lang html */
            '<input type="hidden" name="_email_widget_id" value="">
<input type="hidden" name="emailAddressWidget" value="">

<input type="hidden" name="emailAddress0" value="1">

<input type="hidden" name="emailAddressVerifiedValue0" value="">
<input type="hidden" name="emailAddressPrimaryFlag" value="">
<input type="hidden" name="emailAddressOptOutFlag[]" value="1">
<input type="hidden" name="emailAddressInvalidFlag[]" value="">
<input type="hidden" name="emailAddressReplyToFlag[]" value="">
<input type="hidden" name="emailAddressDeleteFlag[]" value="">
<input type="hidden" name="useEmailWidget" value="true">',
            $result
        );
        self::assertCount(24, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressPrimaryFlag'] = '';
        $_POST['emailAddressVerifiedValue1'] = '';
        $_POST['emailAddress0'] = true;
        $result = $this->ea->getEmailAddressWidgetDuplicatesView(null);
        self::assertEquals(/** @lang html */
            '<input type="hidden" name="_email_widget_id" value="">
<input type="hidden" name="emailAddressWidget" value="">

<input type="hidden" name="emailAddress0" value="1">

<input type="hidden" name="emailAddressVerifiedValue0" value="">
<input type="hidden" name="emailAddressPrimaryFlag" value="">
<input type="hidden" name="emailAddressOptOutFlag[]" value="1">
<input type="hidden" name="emailAddressInvalidFlag[]" value="">
<input type="hidden" name="emailAddressReplyToFlag[]" value="">
<input type="hidden" name="emailAddressDeleteFlag[]" value="">
<input type="hidden" name="useEmailWidget" value="true">',
            $result
        );
        self::assertCount(31, $GLOBALS['log']->calls['fatal']);


        // test
        if (isset($env['$_POST'])) {
            $_POST = $env['$_POST'];
        }
        if (isset($env['$_GET'])) {
            $_GET = $env['$_GET'];
        }
        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        $GLOBALS['log'] = $logger;
    }

    /**
     * Test for getFormBaseURL() method.
     */
    public function testGetFormBaseURL(): void
    {
        // test
        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        $env = array();
        if (isset($_POST)) {
            $env['$_POST'] = $_POST;
        }
        if (isset($_GET)) {
            $env['$_GET'] = $_GET;
        }
        if (isset($_REQUEST)) {
            $env['$_REQUEST'] = $_REQUEST;
        }

        $focus = BeanFactory::newBean('Contacts');


        // test
        $result = $this->ea->getFormBaseURL(null);
        self::assertEquals(false, $result);
        self::assertCount(1, $GLOBALS['log']->calls['fatal']);

        // test
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals('&Contacts_email_widget_id=&emailAddressWidget=', $result);
        self::assertCount(3, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['Contacts_email_widget_id'] = 'testid';
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals('&Contacts_email_widget_id=testid&emailAddressWidget=', $result);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        // test
        $_POST['emailAddressWidget'] = 'testaddress';
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals('&Contacts_email_widget_id=testid&emailAddressWidget=testaddress', $result);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        // test
        $_REQUEST['ContactstestidemailAddress0'] = 'testadrr0';
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals(
            '&Contacts_email_widget_id=testid&emailAddressWidget=testaddress&ContactstestidemailAddress0=testadrr0',
            $result
        );
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        // test
        $_REQUEST['ContactstestidemailAddressVerifiedValue1'] = 'testverfdv1';
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals('&Contacts_email_widget_id=testid&emailAddressWidget=testaddress' .
            '&ContactstestidemailAddress0=testadrr0&ContactstestidemailAddressVerifiedValue1=testverfdv1', $result);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        // test
        $_REQUEST['ContactstestidemailAddressPrimaryFlag'] = 'testadentprimflg';
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals('&Contacts_email_widget_id=testid&emailAddressWidget=testaddress' .
            '&ContactstestidemailAddress0=testadrr0&ContactstestidemailAddressVerifiedValue1=testverfdv1' .
            '&ContactstestidemailAddressPrimaryFlag=testadentprimflg', $result);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);

        // test
        $_REQUEST['ContactstestidemailAddressPrimaryFlag'] = array('testadentprimflg1', 'testadentprimflg2');
        $result = $this->ea->getFormBaseURL($focus);
        self::assertEquals('&Contacts_email_widget_id=testid&emailAddressWidget=testaddress' .
            '&ContactstestidemailAddress0=testadrr0&ContactstestidemailAddressVerifiedValue1=testverfdv1' .
            '&ContactstestidemailAddressPrimaryFlag[0]=testadentprimflg1' .
            '&ContactstestidemailAddressPrimaryFlag[1]=testadentprimflg2', $result);
        self::assertCount(4, $GLOBALS['log']->calls['fatal']);


        // tear down
        if (isset($env['$_POST'])) {
            $_POST = $env['$_POST'];
        }
        if (isset($env['$_GET'])) {
            $_GET = $env['$_GET'];
        }
        if (isset($env['$_REQUEST'])) {
            $_REQUEST = $env['$_REQUEST'];
        }

        $GLOBALS['log'] = $logger;
    }

    /**
     * Test for setView() method.
     */
    public function testSetView(): void
    {
        $before = $this->ea->view;
        $this->ea->setView('afterView');
        self::assertEquals('afterView', $this->ea->view);
        $this->ea->setView($before);
        self::assertEquals($before, $this->ea->view);
    }

    /**
     * Test for getCorrectedModule() method.
     */
    public function testGetCorrectedModule(): void
    {
        // test
        $module = null;
        $result = $this->ea->getCorrectedModule($module);
        self::assertSame($module, $result);

        // test
        $module = '';
        $result = $this->ea->getCorrectedModule($module);
        self::assertSame('', $result);

        // test
        $module = 'Nonexists';
        $result = $this->ea->getCorrectedModule($module);
        self::assertSame('Nonexists', $result);

        // test
        $module = 'Contacts';
        $result = $this->ea->getCorrectedModule($module);
        self::assertSame('Contacts', $result);

        // test
        $module = 'Employees';
        $result = $this->ea->getCorrectedModule($module);
        self::assertSame('Users', $result);
    }

    /**
     * Test for stash() method.
     */
    public function testStash(): void
    {
        $db = DBManagerFactory::getInstance();

        $c = BeanFactory::newBean('Contacts');

        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        // test
        $this->ea->stash(null, null);
        self::assertNotTrue(isset($GLOBALS['log']->calls['fatal']));
    }

    /**
     * Test for getEmailAddressWidget() function.
     */
    public function testGetEmailAddressWidget(): void
    {
        self::markTestIncomplete('environment dependecy in test');

        $query = "SELECT * FROM accounts_cstm";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableAccountsCstm = $rows;


        // test
        $c = BeanFactory::getBean('Contacts');
        $c->id = 'test_contact_1';

        $a = BeanFactory::getBean('Accounts');
        $a->id = 'test_account_1';
        $a->save();

        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        // test
        $result = getEmailAddressWidget($c, null, null, 'ConvertLead');
        self::assertFalse(strpos((string) $result, '--None--'));
        self::assertCount(1, $GLOBALS['log']->calls['fatal']);


        // test
        $result = getEmailAddressWidget($a, null, null, 'ConvertLead');
        self::assertFalse(strpos((string) $result, '--None--'));

        // test
        $result = getEmailAddressWidget($c, null, null, 'EditView');
        self::assertFalse(strpos((string) $result, '--None--'));

        // test
        $result = getEmailAddressWidget($c, null, null, 'DetailView');
        self::assertEquals(/** @lang html */
            '
			<table cellpadding="0" cellspacing="0" border="0" width="100%">
								<tr>
					<td>
						<i>--None--</i>
					</td>
				</tr>
							</table>',
            $result
        );

        // test
        $result = getEmailAddressWidget(null, null, null, null);
        self::assertEquals('', $result);


        // test
        $GLOBALS['log'] = $logger;




        DBManagerFactory::getInstance()->query("DELETE FROM accounts_cstm");
        foreach ($tableAccountsCstm as $row) {
            $query = "INSERT accounts_cstm INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }


    public function testGetOptInStatus(): void
    {
        self::markTestIncomplete('COI_STAT_CONFIRMED_OPT_IN');


        global $sugar_config;

        //
        // Test Scenario: when email_enable_confirm_opt_in is disabled
        $sugar_config['email_enable_confirm_opt_in'] = SugarEmailAddress::COI_STAT_DISABLED;
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';

        self::assertEquals(
            EmailAddress::COI_FLAG_OPT_IN_DISABLED,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, opt in
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;

        self::assertEquals(
            EmailAddress::COI_FLAG_OPT_IN_DISABLED,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;
        $emailAddress->confirm_opt_in_fail_date = '2017-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            EmailAddress::COI_FLAG_OPT_IN_DISABLED,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;
        $emailAddress->confirm_opt_in_fail_date = '2015-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN_DISABLED,
            $emailAddress->getOptInStatus()
        );


        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_CONFIRMED_OPT_IN;
        $emailAddress->confirm_opt_in_date = '2018-01-01 10:10:00';
        $emailAddress->confirm_opt_in_fail_date = '2015-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN_DISABLED,
            $emailAddress->getOptInStatus()
        );

        //
        // Test Scenario: when email_enable_confirm_opt_in is opt in
        $sugar_config['email_enable_confirm_opt_in'] = SugarEmailAddress::COI_STAT_OPT_IN;

        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_DISABLED;
        $emailAddress->confirm_opt_in_date = '';
        $emailAddress->confirm_opt_in_fail_date = '';
        $emailAddress->confirm_opt_in_sent_date = '';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_NO_OPT_IN_STATUS,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, opt in
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;
        $emailAddress->confirm_opt_in_fail_date = '2017-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            EmailAddress::COI_FLAG_OPT_IN,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;
        $emailAddress->confirm_opt_in_fail_date = '2015-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            EmailAddress::COI_FLAG_OPT_IN,
            $emailAddress->getOptInStatus()
        );


        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_CONFIRMED_OPT_IN;
        $emailAddress->confirm_opt_in_date = '2018-01-01 10:10:00';
        $emailAddress->confirm_opt_in_fail_date = '2015-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN,
            $emailAddress->getOptInStatus()
        );


        //
        // Test Scenario: when email_enable_confirm_opt_in is confirmed opt in
        $sugar_config['email_enable_confirm_opt_in'] = SugarEmailAddress::COI_STAT_CONFIRMED_OPT_IN;

        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_DISABLED;
        $emailAddress->confirm_opt_in_date = '';
        $emailAddress->confirm_opt_in_fail_date = '';
        $emailAddress->confirm_opt_in_sent_date = '';

        self::assertEquals(
            SugarEmailAddress::COI_STAT_DISABLED,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN_PENDING_EMAIL_NOT_SENT,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;
        $emailAddress->confirm_opt_in_fail_date = '2017-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN_PENDING_EMAIL_FAILED,
            $emailAddress->getOptInStatus()
        );

        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_OPT_IN;
        $emailAddress->confirm_opt_in_fail_date = '2015-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN_PENDING_EMAIL_SENT,
            $emailAddress->getOptInStatus()
        );


        // Test opt in status, email failed
        $emailAddress = new SugarEmailAddress();
        $emailAddress->email_address = 'test@example.com';
        $emailAddress->email_address_caps = 'TEST@EXAMPLE.COM';
        $emailAddress->confirm_opt_in = SugarEmailAddress::COI_STAT_CONFIRMED_OPT_IN;
        $emailAddress->confirm_opt_in_date = '2018-01-01 10:10:00';
        $emailAddress->confirm_opt_in_fail_date = '2015-01-01 10:10:00';
        $emailAddress->confirm_opt_in_sent_date = '2017-01-01 10:10:00';

        self::assertEquals(
            SugarEmailAddress::COI_FLAG_OPT_IN_PENDING_EMAIL_CONFIRMED,
            $emailAddress->getOptInStatus()
        );
    }
}
