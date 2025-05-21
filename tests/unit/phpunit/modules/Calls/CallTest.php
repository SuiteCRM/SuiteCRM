<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class CallTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testCall(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $call = BeanFactory::newBean('Calls');
        self::assertInstanceOf('Call', $call);
        self::assertInstanceOf('SugarBean', $call);

        self::assertEquals('Calls', $call->module_dir);
        self::assertEquals('Call', $call->object_name);
        self::assertEquals('calls', $call->table_name);
        self::assertEquals('calls_users', $call->rel_users_table);
        self::assertEquals('calls_contacts', $call->rel_contacts_table);
        self::assertEquals('calls_leads', $call->rel_leads_table);
        self::assertEquals(true, $call->new_schema);
        self::assertEquals(true, $call->importable);
        self::assertEquals(false, $call->syncing);
        self::assertEquals(true, $call->update_vcal);
    }

    public function testACLAccess(): void
    {
        $call = BeanFactory::newBean('Calls');

        //test without setting recurring_source attribute
        self::assertTrue($call->ACLAccess(''));
        //$this->assertTrue($call->ACLAccess('edit'));

        //test with recurring_source attribute set
        $call->recurring_source = 'test';
        self::assertFalse($call->ACLAccess('edit'));
    }

    public function testSaveAndMarkDeleted(): void
    {
        $call = BeanFactory::newBean('Calls');

        $call->name = 'test';
        $call->id = $call->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($call->id));
        self::assertEquals(36, strlen((string) $call->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $call->mark_deleted($call->id);
        $result = $call->retrieve($call->id);
        self::assertEquals(null, $result);
    }

    public function testget_contacts(): void
    {
        $call = BeanFactory::newBean('Calls');
        $call->id = 1;

        //execute the method and verify if it returns an array
        $result = $call->get_contacts();
        self::assertIsArray($result);
    }

    public function testget_summary_text(): void
    {
        $call = BeanFactory::newBean('Calls');

        //test without setting name
        self::assertEquals(null, $call->get_summary_text());

        //test with name set
        $call->name = 'test';
        self::assertEquals('test', $call->get_summary_text());
    }

    public function testcreate_list_query(): void
    {
        self::markTestIncomplete('environment dependency');

        $call = BeanFactory::newBean('Calls');

        //test with empty string params
        $expected = "SELECT \n			calls.*,\n			users.user_name as assigned_user_name FROM calls \n			LEFT JOIN users\n			ON calls.assigned_user_id=users.id where  calls.deleted=0   ORDER BY calls.name";
        $actual = $call->create_list_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT \n			calls.*,\n			users.user_name as assigned_user_name FROM calls \n			LEFT JOIN users\n			ON calls.assigned_user_id=users.id where users.user_name=\"\" AND  calls.deleted=0   ORDER BY calls.name";
        $actual = $call->create_list_query('name', 'users.user_name=""');
        self::assertSame($expected, $actual);
    }

    public function testcreate_export_query(): void
    {
        $call = BeanFactory::newBean('Calls');

        //test with empty string params
        $expected = 'SELECT calls.*, users.user_name as assigned_user_name  FROM calls   LEFT JOIN users ON calls.assigned_user_id=users.id where calls.deleted=0 ORDER BY calls.name';
        $actual = $call->create_export_query('', '');
        self::assertSame($expected, $actual);


        //test with empty string params
        $expected = 'SELECT calls.*, users.user_name as assigned_user_name  FROM calls   LEFT JOIN users ON calls.assigned_user_id=users.id where users.user_name="" AND calls.deleted=0 ORDER BY calls.name';
        $actual = $call->create_export_query('name', 'users.user_name=""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $call = BeanFactory::newBean('Calls');

        //execute the method and verify it sets up the intended fields
        $call->fill_in_additional_detail_fields();

        self::assertEquals('0', $call->duration_hours);
        self::assertEquals('15', $call->duration_minutes);
        self::assertEquals(-1, $call->reminder_time);
        self::assertEquals(false, $call->reminder_checked);
        self::assertEquals(-1, $call->email_reminder_time);
        self::assertEquals(false, $call->email_reminder_checked);
        self::assertEquals('Accounts', $call->parent_type);
    }

    public function testget_list_view_data(): void
    {
        self::markTestIncomplete('environment dependency (php5/php7)');

        $call = BeanFactory::newBean('Calls');

        $call->assigned_user_id = 1;
        $call->created_by = 1;
        $call->modified_user_id = 1;

        //execute the method and verify that it retunrs expected results
        $expected = array(
            'MODIFIED_USER_ID' => 1,
            'CREATED_BY' => 1,
            'DELETED' => 0,
            'ASSIGNED_USER_ID' => 1,
            'STATUS' => 'Planned',
            'REMINDER_TIME' => '-1',
            'EMAIL_REMINDER_TIME' => '-1',
            'EMAIL_REMINDER_SENT' => '0',
            'REPEAT_INTERVAL' => '1',
            'SET_COMPLETE' => '',
            'DATE_START' => '<font class=\'overdueTask\'></font>',
            'CONTACT_ID' => null,
            'CONTACT_NAME' => null,
            'PARENT_NAME' => '',
            'REMINDER_CHECKED' => false,
            'EMAIL_REMINDER_CHECKED' => false,
        );

        $actual = $call->get_list_view_data();
        self::assertSame($expected, $actual);

        self::assertEquals('Administrator', $call->assigned_user_name);
        self::assertEquals('Administrator', $call->created_by_name);
        self::assertEquals('Administrator', $call->modified_by_name);
    }

    public function testset_notification_body(): void
    {
        $call = BeanFactory::newBean('Calls');

        //test with attributes preset and verify template variables are set accordingly

        $call->name = 'test';
        $call->duration_hours = '1';
        $call->duration_minutes = '10';
        $call->status = 'Planned';
        $call->description = 'some text';
        $call->date_start = '2015-09-01 00:02:03';
        $call->current_notify_user = new User(1);
        $call->current_notify_user->new_assigned_user_name = 'Admin';

        $result = $call->set_notification_body(new Sugar_Smarty(), $call);

        self::assertEquals($call->name, $result->tpl_vars['CALL_SUBJECT']->value);
        self::assertEquals($call->current_notify_user->new_assigned_user_name, $result->tpl_vars['CALL_TO']->value);
        self::assertEquals($call->duration_hours, $result->tpl_vars['CALL_HOURS']->value);
        self::assertEquals($call->duration_minutes, $result->tpl_vars['CALL_MINUTES']->value);
        self::assertEquals($call->status, $result->tpl_vars['CALL_STATUS']->value);
        self::assertEquals('09/01/2015 00:02 UTC(+00:00)', $result->tpl_vars['CALL_STARTDATE']->value);
        self::assertEquals($call->description, $result->tpl_vars['CALL_DESCRIPTION']->value);
    }

    public function testget_call_users(): void
    {
        $call = BeanFactory::newBean('Calls');
        $call->id = 1;

        //execute the method and verify it returns an array
        $result = $call->get_call_users();
        self::assertIsArray($result);
    }

    public function testget_invite_calls(): void
    {
        $call = BeanFactory::newBean('Calls');
        $user = new User(1);

        //execute the method and verify it returns an array
        $result = $call->get_invite_calls($user);
        self::assertIsArray($result);
    }

    public function testset_accept_status(): void
    {
        $call = BeanFactory::newBean('Calls');
        $call->id = 1;

        //test for calls Users and delete the created linked records afterwards
        $user = BeanFactory::newBean('Users');
        $user->id = '1';

        $call->set_accept_status($user, 'test');

        $call_users = $call->get_linked_beans('users', $call->object_name);
        self::assertCount(1, $call_users);

        $call->delete_linked($call->id);
    }

    public function testget_notification_recipients(): void
    {
        $call = BeanFactory::newBean('Calls');

        //test without setting any user list
        $result = $call->get_notification_recipients();
        self::assertIsArray($result);

        //test with a user in notofication list set
        $call->users_arr = array(1);
        $result = $call->get_notification_recipients();
        self::assertIsArray($result);
        self::assertCount(1, $result);
    }

    public function testbean_implements(): void
    {
        $call = BeanFactory::newBean('Calls');
        self::assertEquals(false, $call->bean_implements('')); //test with blank value
        self::assertEquals(false, $call->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $call->bean_implements('ACL')); //test with valid value
    }

    public function testlistviewACLHelper(): void
    {
        self::markTestIncomplete('environment dependency');
        $call = BeanFactory::newBean('Calls');
        $expected = array('MAIN' => 'a', 'PARENT' => 'a', 'CONTACT' => 'a');
        $actual = $call->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testsave_relationship_changes(): void
    {
        $call = BeanFactory::newBean('Calls');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $call->save_relationship_changes(true);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetDefaultStatus(): void
    {
        $result = BeanFactory::newBean('Calls')->getDefaultStatus();
        self::assertEquals('Planned', $result);
    }
}
