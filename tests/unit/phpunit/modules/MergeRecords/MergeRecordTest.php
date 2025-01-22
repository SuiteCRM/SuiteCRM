<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class MergeRecordTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testMergeRecord(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $mergeRecord = BeanFactory::newBean('MergeRecords');

        self::assertInstanceOf('MergeRecord', $mergeRecord);
        self::assertInstanceOf('SugarBean', $mergeRecord);

        self::assertEquals('MergeRecords', $mergeRecord->module_dir);
        self::assertEquals('MergeRecord', $mergeRecord->object_name);

        self::assertEquals(true, $mergeRecord->new_schema);
        self::assertEquals(true, $mergeRecord->acl_display_only);
    }

    public function testsave(): void
    {
        $mergeRecord = BeanFactory::newBean('MergeRecords');
        //$mergeRecord->save();

        self::markTestIncomplete('method has no implementation');
    }

    public function testretrieve(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');

        $mergeRecord = BeanFactory::newBean('MergeRecords');

        //preset variables required
        $mergeRecord->merge_bean = 'Users';
        $_REQUEST['action'] = 'Step2';

        $mergeRecord->retrieve('1');

        self::markTestIncomplete('Merge bean is broken at the moment');
        //$this->assertTrue(isset($mergeRecord->merge_bean->id));

        self::assertEquals(1, $mergeRecord->merge_bean->id);
    }

    public function testload_merge_bean(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        //test without merge_id
        $mergeRecord->load_merge_bean('Contacts');

        self::assertEquals('Contacts', $mergeRecord->merge_module);
        self::assertEquals('Contact', $mergeRecord->merge_bean_class);
        self::assertEquals('modules/Contacts/Contact.php', $mergeRecord->merge_bean_file_path);

        //test with merge_id
        $mergeRecord->load_merge_bean('Users', false, 1);

        self::assertEquals('Users', $mergeRecord->merge_module);
        self::assertEquals('User', $mergeRecord->merge_bean_class);
        self::assertEquals('modules/Users/User.php', $mergeRecord->merge_bean_file_path);

        self::assertInstanceOf('User', $mergeRecord->merge_bean);
    }

    public function testload_merge_bean2(): void
    {
        $mergeRecord = BeanFactory::newBean('MergeRecords');

        //test without merge_id
        $mergeRecord->load_merge_bean2('Contacts');

        self::assertEquals('Contacts', $mergeRecord->merge_module2);
        self::assertEquals('Contact', $mergeRecord->merge_bean_class2);
        self::assertEquals('modules/Contacts/Contact.php', $mergeRecord->merge_bean_file_path2);

        //test with merge_id
        $mergeRecord->load_merge_bean2('Users', false, 1);

        self::assertEquals('Users', $mergeRecord->merge_module2);
        self::assertEquals('User', $mergeRecord->merge_bean_class2);
        self::assertEquals('modules/Users/User.php', $mergeRecord->merge_bean_file_path2);

        self::assertInstanceOf('User', $mergeRecord->merge_bean2);
    }

    public function testfill_in_additional_list_fields(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Users', false, 1);

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $mergeRecord->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');

        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Users', false, 1);

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $mergeRecord->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_summary_text(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');

        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Users');

        //test without setting name
        self::assertEquals(null, $mergeRecord->get_summary_text());

        //test with name set
        $mergeRecord->merge_bean->name = 'test';
        self::assertEquals('test', $mergeRecord->get_summary_text());
    }

    public function testget_list_view_data(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Users');

        $result = $mergeRecord->get_list_view_data();

        self::assertIsArray($result);
    }

    public function testbuild_generic_where_clause(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Contacts');

        //test with string
        $expected = "contacts.last_name like 'test%' or contacts.first_name like 'test%' or accounts.name like 'test%' or contacts.assistant like 'test%' or ea.email_address like 'test%'";
        $actual = $mergeRecord->build_generic_where_clause('test');
        self::assertSame($expected, $actual);

        //test with number
        $expected = "contacts.last_name like '1%' or contacts.first_name like '1%' or accounts.name like '1%' or contacts.assistant like '1%' or ea.email_address like '1%' or contacts.phone_home like '%1%' or contacts.phone_mobile like '%1%' or contacts.phone_work like '%1%' or contacts.phone_other like '%1%' or contacts.phone_fax like '%1%' or contacts.assistant_phone like '%1%'";
        $actual = $mergeRecord->build_generic_where_clause(1);
        self::assertSame($expected, $actual);
    }

    public function testbean_implements(): void
    {
        $mergeRecord = BeanFactory::newBean('MergeRecords');

        self::assertEquals(false, $mergeRecord->bean_implements('')); //test with blank value
        self::assertEquals(false, $mergeRecord->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $mergeRecord->bean_implements('ACL')); //test with valid value
    }

    public function testACLAccess(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        //test without loading merge bean
        self::assertEquals(true, $mergeRecord->ACLAccess('')); //test with valid value

        //test with merge bean loaded
        $mergeRecord->load_merge_bean('Meetings');

        self::assertEquals(true, $mergeRecord->ACLAccess('edit'));
        self::assertEquals(true, $mergeRecord->ACLAccess('save'));
        self::assertEquals(true, $mergeRecord->ACLAccess('editview'));
        self::assertEquals(true, $mergeRecord->ACLAccess('delete'));
    }

    public function testpopulate_search_params(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Meetings');

        $expected = array(
                          'id' => array('name' => 'id', 'vname' => 'LBL_ID', 'type' => 'id', 'required' => true, 'reportable' => true, 'comment' => 'Unique identifier', 'inline_edit' => false, 'value' => '1', 'search_type' => 'Exact'),
                          'name' => array('name' => 'name', 'vname' => 'LBL_SUBJECT', 'required' => true, 'type' => 'name', 'dbType' => 'varchar', 'unified_search' => true, 'full_text_search' => array('boost' => 3), 'len' => '50', 'comment' => 'Meeting name', 'importable' => 'required', 'value' => 'test', 'search_type' => 'Exact'),
                        );

        $mergeRecord->populate_search_params(array('nameSearchField' => 'test', 'idSearchField' => '1'));

        self::assertSame($expected, $mergeRecord->field_search_params);
    }

    public function testget_inputs_for_search_params(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Meetings');

        $expected = "<input type='hidden' name='idSearchField' value='1' />\n<input type='hidden' name='idSearchType' value='' />\n<input type='hidden' name='nameSearchField' value='test' />\n<input type='hidden' name='nameSearchType' value='' />\n";

        $result = $mergeRecord->get_inputs_for_search_params(array('nameSearchField' => 'test', 'idSearchField' => '1'));

        self::assertSame($expected, $result);
    }

    public function testemail_addresses_query(): void
    {
        $table = 'accounts';
        $module = 'Accounts';
        $bean_id = 1;
        $expected = $table.".id IN (SELECT ear.bean_id FROM email_addresses ea
                                LEFT JOIN email_addr_bean_rel ear ON ea.id = ear.email_address_id
                                WHERE ear.bean_module = '{$module}'
                                AND ear.bean_id != '{$bean_id}'
                                AND ear.deleted = 0";

        $result = BeanFactory::newBean('MergeRecords')->email_addresses_query($table, $module, $bean_id);

        self::assertSame($expected, $result);
    }

    public function testrelease_name_query(): void
    {
        $mergeRecord = BeanFactory::newBean('MergeRecords');

        //test with type = like
        $result = $mergeRecord->release_name_query('like', 'test');
        self::assertSame('', $result);

        //test with type = start
        $result = $mergeRecord->release_name_query('start', 'test');
        self::assertSame('', $result);
    }

    public function testcreate_where_statement(): void
    {
        self::markTestIncomplete('Test failing since commit a5acea613 applied php7fix patch');


        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $mergeRecord->load_merge_bean('Contacts');
        $mergeRecord->populate_search_params(array('nameSearchField' => 'test', 'idSearchField' => '1'));

        $expected = array(
            0 => "contacts.id='1'",
            1 => "contacts.name='test'",
            2 => "contacts.id !=''",
        );

        $actual = $mergeRecord->create_where_statement();

        self::assertSame($expected, $actual);
    }

    public function testgenerate_where_statement(): void
    {
        $mergeRecord = BeanFactory::newBean('MergeRecords');

        $clauses = array("contacts.id='1'",  "contacts.name='test'", "contacts.id !=''");
        $expected = "contacts.id='1' AND contacts.name='test' AND contacts.id !=''";

        $actual = $mergeRecord->generate_where_statement($clauses);

        self::assertSame($expected, $actual);

        ////error_reporting(E_ALL);
    }
}
