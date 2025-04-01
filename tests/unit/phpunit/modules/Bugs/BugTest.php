<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class BugTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testBug(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $bug = BeanFactory::newBean('Bugs');
        self::assertInstanceOf('Bug', $bug);
        self::assertInstanceOf('SugarBean', $bug);

        self::assertEquals('Bugs', $bug->module_dir);
        self::assertEquals('Bug', $bug->object_name);
        self::assertEquals('bugs', $bug->table_name);
        self::assertEquals('accounts_bugs', $bug->rel_account_table);
        self::assertEquals('contacts_bugs', $bug->rel_contact_table);
        self::assertEquals('cases_bugs', $bug->rel_case_table);
        self::assertEquals(true, $bug->new_schema);
    }

    public function testget_summary_text(): void
    {
        $bug = BeanFactory::newBean('Bugs');

        //test without setting name
        self::assertEquals(null, $bug->get_summary_text());

        //test with name set//test with name set
        $bug->name = 'test';
        self::assertEquals('test', $bug->get_summary_text());
    }

    public function testcreate_list_query(): void
    {
        self::markTestIncomplete('#Warning: Strings contain different line endings!');
        $bug = BeanFactory::newBean('Bugs');

        //test with empty string params
        $expected = "SELECT \n                               bugs.*\n\n                                ,users.user_name as assigned_user_name, releases.id release_id, releases.name release_name FROM bugs 				LEFT JOIN releases ON bugs.found_in_release=releases.id\n								LEFT JOIN users\n                                ON bugs.assigned_user_id=users.id  where  bugs.deleted=0  ORDER BY bugs.name";
        $actual = $bug->create_list_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT \n                               bugs.*\n\n                                ,users.user_name as assigned_user_name, releases.id release_id, releases.name release_name FROM bugs 				LEFT JOIN releases ON bugs.found_in_release=releases.id\n								LEFT JOIN users\n                                ON bugs.assigned_user_id=users.id  where bugs.name=\"\" AND  bugs.deleted=0  ORDER BY releases.id";
        $actual = $bug->create_list_query('releases.id', 'bugs.name=""');
        self::assertSame($expected, $actual);
    }

    public function testcreate_export_query(): void
    {
        self::markTestIncomplete('#Warning: Strings contain different line endings!');
        $bug = BeanFactory::newBean('Bugs');

        //test with empty string params
        $expected = "SELECT\n                                bugs.*,\n                                r1.name found_in_release_name,\n                                r2.name fixed_in_release_name,\n                                users.user_name assigned_user_name FROM bugs 				LEFT JOIN releases r1 ON bugs.found_in_release = r1.id\n								LEFT JOIN releases r2 ON bugs.fixed_in_release = r2.id\n								LEFT JOIN users\n                                ON bugs.assigned_user_id=users.id where   bugs.deleted=0\n                 ORDER BY bugs.bug_number";
        $actual = $bug->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT\n                                bugs.*,\n                                r1.name found_in_release_name,\n                                r2.name fixed_in_release_name,\n                                users.user_name assigned_user_name FROM bugs 				LEFT JOIN releases r1 ON bugs.found_in_release = r1.id\n								LEFT JOIN releases r2 ON bugs.fixed_in_release = r2.id\n								LEFT JOIN users\n                                ON bugs.assigned_user_id=users.id where bugs.name=\"\" AND   bugs.deleted=0\n                 ORDER BY releases.id";
        $actual = $bug->create_export_query('releases.id', 'bugs.name=""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $bug = BeanFactory::newBean('Bugs');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $bug->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $bug = BeanFactory::newBean('Bugs');
        $bug->assigned_user_id = 1;
        $bug->created_by = 1;
        $bug->modified_user_id = 1;

        //test with attributes preset and verify attributes are set accordingly
        $bug->fill_in_additional_detail_fields();

        self::assertEquals('Administrator', $bug->assigned_user_name);
        self::assertEquals('Administrator', $bug->created_by_name);
        self::assertEquals('Administrator', $bug->modified_by_name);
    }

    public function testset_release(): void
    {
        $bug = BeanFactory::newBean('Bugs');
        $bug->found_in_release = '1';

        $bug->set_release();

        self::assertEquals('', $bug->release_name);
    }

    public function testset_fixed_in_release(): void
    {
        $bug = BeanFactory::newBean('Bugs');
        $bug->found_in_release = '1';

        $bug->set_release();

        self::assertEquals('', $bug->fixed_in_release_name);
    }

    public function testget_list_view_data(): void
    {
        $bug = BeanFactory::newBean('Bugs');

        //execute the method and verify that it retunrs expected results
        $expected = array(
            'DELETED' => 0,
            'NAME' => '<em>blank</em>',
            'PRIORITY' => '',
            'STATUS' => '',
            'TYPE' => '',
            'RELEASE' => null,
            'BUG_NUMBER' => null,
            'ENCODED_NAME' => null,
        );

        $actual = $bug->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testbuild_generic_where_clause(): void
    {
        $bug = BeanFactory::newBean('Bugs');

        //execute with blank parameters
        $expected = "bugs.name like '%'";
        $actual = $bug->build_generic_where_clause('');
        self::assertSame($expected, $actual);

        //execute with numeric parameter
        $expected = "bugs.name like '1%' or bugs.bug_number like '1%'";
        $actual = $bug->build_generic_where_clause(1);
        self::assertSame($expected, $actual);
    }

    public function testset_notification_body(): void
    {
        $bug = BeanFactory::newBean('Bugs');

        $bug->name = 'test';
        $bug->type = 'Defect';
        $bug->priority = 'Urgent';
        $bug->status = 'New';
        $bug->resolution = 'Accepted';
        $bug->bug_number = '1';

        //test with attributes preset and verify template variables are set accordingly
        $result = $bug->set_notification_body(new Sugar_Smarty(), $bug);

        self::assertEquals($bug->name, $result->tpl_vars['BUG_SUBJECT']->value);
        self::assertEquals($bug->type, $result->tpl_vars['BUG_TYPE']->value);
        self::assertEquals($bug->priority, $result->tpl_vars['BUG_PRIORITY']->value);
        self::assertEquals($bug->status, $result->tpl_vars['BUG_STATUS']->value);
        self::assertEquals($bug->resolution, $result->tpl_vars['BUG_RESOLUTION']->value);
        self::assertEquals($bug->bug_number, $result->tpl_vars['BUG_BUG_NUMBER']->value);
    }

    public function testbean_implements(): void
    {
        $bug = BeanFactory::newBean('Bugs');
        self::assertEquals(false, $bug->bean_implements('')); //test with blank value
        self::assertEquals(false, $bug->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $bug->bean_implements('ACL')); //test with valid value
    }

    public function testsave(): void
    {
        $bug = BeanFactory::newBean('Bugs');

        $bug->name = 'test';
        $bug->bug_number = '1';
        $bug->type = 'Defect';
        $bug->priority = 'Urgent';
        $bug->status = 'New';
        $bug->resolution = 'Accepted';

        $bug->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($bug->id));
        self::assertEquals(36, strlen((string) $bug->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $bug->mark_deleted($bug->id);
        $result = $bug->retrieve($bug->id);
        self::assertEquals(null, $result);
    }

    public function testgetReleaseDropDown(): void
    {
        $result = getReleaseDropDown();

        //execute the method and verify it returns an array
        self::assertIsArray($result);
    }
}
