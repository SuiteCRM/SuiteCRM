<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class TaskTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();
        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'Tasks');
    }

    public function testTask(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $task = BeanFactory::newBean('Tasks');

        self::assertInstanceOf('Task', $task);
        self::assertInstanceOf('SugarBean', $task);

        self::assertEquals('tasks', $task->table_name);
        self::assertEquals('Tasks', $task->module_dir);
        self::assertEquals('Task', $task->object_name);

        self::assertEquals(true, $task->new_schema);
        self::assertEquals(true, $task->importable);
    }

    public function testsave(): void
    {
        $task = BeanFactory::newBean('Tasks');

        $task->name = 'test';
        $task->priority = 'Medium';
        $task->parent_type = 'Accounts';
        $task->status = 'In Progress';

        $result = $task->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($task->id));
        self::assertEquals(36, strlen((string) $task->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $task->mark_deleted($task->id);
        $result = $task->retrieve($task->id);
        self::assertEquals(null, $result);
    }

    public function testget_summary_text(): void
    {
        $task = BeanFactory::newBean('Tasks');

        //test without setting name
        self::assertEquals(null, $task->get_summary_text());

        //test with name set
        $task->name = 'test';
        self::assertEquals('test', $task->get_summary_text());
    }

    public function testcreate_export_query(): void
    {
        $task = BeanFactory::newBean('Tasks');

        //test with empty string params
        $expected = 'SELECT tasks.*, users.user_name as assigned_user_name  FROM tasks   LEFT JOIN users ON tasks.assigned_user_id=users.id where tasks.deleted=0 ORDER BY tasks.name';
        $actual = $task->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = 'SELECT tasks.*, users.user_name as assigned_user_name  FROM tasks   LEFT JOIN users ON tasks.assigned_user_id=users.id where users.user_name = "" AND tasks.deleted=0 ORDER BY tasks.name';
        $actual = $task->create_export_query('tasks.id', 'users.user_name = ""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $task = BeanFactory::newBean('Tasks');
        $task->contact_id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $task->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_parent_fields(): void
    {
        $task = BeanFactory::newBean('Tasks');
        $task->parent_type = 'Accounts';
        $task->parent_id = '1';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $task->fill_in_additional_parent_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $task = BeanFactory::newBean('Tasks');
        $current_theme = SugarThemeRegistry::current();

        $task->name = 'test';
        $task->priority = 'Medium';
        $task->parent_type = 'Accounts';
        $task->status = 'In Progress';
        $task->contact_name = 'test';
        $task->contact_phone = '1234567';
        $task->parent_name = 'test';

        $expected = array(
                'NAME' => 'test',
                'DELETED' => 0,
                'STATUS' => 'In Progress',
                'DATE_DUE_FLAG' => '0',
                'DATE_START_FLAG' => '0',
                'PARENT_TYPE' => 'Accounts',
                'PARENT_NAME' => 'test',
                'CONTACT_NAME' => 'test',
                'CONTACT_PHONE' => '1234567',
                'PRIORITY' => 'Medium',
                'PARENT_MODULE' => 'Accounts',
                'SET_COMPLETE' => '<b><a id=\'\' class=\'list-view-data-icon\' title=\'Close\' onclick=\'SUGAR.util.closeActivityPanel.show("Tasks","","Completed","listview","1");\'><span class=\'suitepicon suitepicon-action-clear\'></span></a></b>',
                'TITLE' => "Contact: test\nAccount: test",
        );

        $actual = $task->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testset_notification_body(): void
    {
        $task = BeanFactory::newBean('Tasks');

        //test with attributes preset and verify template variables are set accordingly

        $task->name = 'test';
        $task->priority = 'Medium';
        $task->date_due = '2016-02-11 17:30:00';
        $task->status = 'In Progress';
        $task->description = 'test description';

        $task->current_notify_user = new User(1);

        $result = $task->set_notification_body(new Sugar_Smarty(), $task);

        self::assertEquals($task->name, $result->tpl_vars['TASK_SUBJECT']->value);
        self::assertEquals($task->status, $result->tpl_vars['TASK_STATUS']->value);
        self::assertEquals($task->priority, $result->tpl_vars['TASK_PRIORITY']->value);
        self::assertEquals('02/11/2016 17:30 UTC(+00:00)', $result->tpl_vars['TASK_DUEDATE']->value);
        self::assertEquals($task->description, $result->tpl_vars['TASK_DESCRIPTION']->value);
    }

    public function testbean_implements(): void
    {
        $task = BeanFactory::newBean('Tasks');

        self::assertEquals(false, $task->bean_implements('')); //test with blank value
        self::assertEquals(false, $task->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $task->bean_implements('ACL')); //test with valid value
    }

    public function testlistviewACLHelper(): void
    {
        $task = BeanFactory::newBean('Tasks');

        $expected = array('MAIN' => 'a', 'PARENT' => 'a', 'CONTACT' => 'a');
        $actual = $task->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testgetDefaultStatus(): void
    {
        $result = BeanFactory::newBean('Tasks')->getDefaultStatus();
        self::assertEquals('Not Started', $result);
    }
}
