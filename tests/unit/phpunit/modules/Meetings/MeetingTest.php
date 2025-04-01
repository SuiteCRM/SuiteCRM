<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class MeetingTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testMeeting(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $meeting = BeanFactory::newBean('Meetings');

        self::assertInstanceOf('Meeting', $meeting);
        self::assertInstanceOf('SugarBean', $meeting);

        self::assertEquals('Meetings', $meeting->module_dir);
        self::assertEquals('Meeting', $meeting->object_name);
        self::assertEquals('meetings', $meeting->table_name);

        self::assertEquals(true, $meeting->new_schema);
        self::assertEquals(true, $meeting->importable);
        self::assertEquals(false, $meeting->syncing);
        self::assertEquals(true, $meeting->update_vcal);

        self::assertEquals('meetings_users', $meeting->rel_users_table);
        self::assertEquals('meetings_contacts', $meeting->rel_contacts_table);
        self::assertEquals('meetings_leads', $meeting->rel_leads_table);

        self::assertEquals(null, $meeting->cached_get_users);
        self::assertEquals(false, $meeting->date_changed);
    }

    public function testACLAccess(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        //test without recurring_source
        self::assertEquals(true, $meeting->ACLAccess('edit'));
        self::assertEquals(true, $meeting->ACLAccess('save'));
        self::assertEquals(true, $meeting->ACLAccess('editview'));
        self::assertEquals(true, $meeting->ACLAccess('delete'));

        //test with recurring_source
        $meeting->recurring_source = 'test';
        self::assertEquals(false, $meeting->ACLAccess('edit'));
        self::assertEquals(false, $meeting->ACLAccess('save'));
        self::assertEquals(false, $meeting->ACLAccess('editview'));
        self::assertEquals(false, $meeting->ACLAccess('delete'));
    }

    public function testhasIntegratedMeeting(): void
    {
        $result = BeanFactory::newBean('Meetings')->hasIntegratedMeeting();
        self::assertEquals(false, $result);
    }

    public function testSaveAndMarkdeletedAndSetAcceptStatus(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        $meeting->name = 'test';
        $meeting->status = 'Not Held';
        $meeting->type = 'Sugar';
        $meeting->description = 'test description';
        $meeting->duration_hours = 1;
        $meeting->duration_minutes = 1;
        $meeting->date_start = '2016-02-11 17:30:00';
        $meeting->date_end = '2016-02-11 17:30:00';

        $meeting->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($meeting->id));
        self::assertEquals(36, strlen((string) $meeting->id));

        /* Test set_accept_status method */

        //test set_accept_status with User object
        $user = BeanFactory::newBean('Users');
        $meeting->set_accept_status($user, 'accept');

        //test set_accept_status with contact object
        $contact = BeanFactory::newBean('Contacts');
        $meeting->set_accept_status($contact, 'accept');

        //test set_accept_status with Lead object
        $lead = BeanFactory::newBean('Leads');
        $meeting->set_accept_status($lead, 'accept');

        //mark all created relationships as deleted
        $meeting->mark_relationships_deleted($meeting->id);

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $meeting->mark_deleted($meeting->id);
        $result = $meeting->retrieve($meeting->id);
        self::assertEquals(null, $result);
    }

    public function testget_summary_text(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        //test without setting name
        self::assertEquals(null, $meeting->get_summary_text());

        //test with name set
        $meeting->name = 'test';
        self::assertEquals('test', $meeting->get_summary_text());
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        //preset required attributes
        $meeting->assigned_user_id = 1;
        $meeting->modified_user_id = 1;
        $meeting->created_by = 1;
        $meeting->contact_id = 1;

        $meeting->fill_in_additional_detail_fields();

        //verify effected atributes
        self::assertEquals('Administrator', $meeting->assigned_user_name);
        self::assertEquals('Administrator', $meeting->created_by_name);
        self::assertEquals('Administrator', $meeting->modified_by_name);
        self::assertTrue(isset($meeting->time_start_hour));
        self::assertTrue(isset($meeting->date_start));
        self::assertTrue(isset($meeting->time_start));
        self::assertTrue(isset($meeting->duration_hours));
        self::assertTrue(isset($meeting->duration_minutes));
        self::assertEquals(-1, $meeting->reminder_time);
        self::assertTrue(isset($meeting->reminder_time));
        self::assertEquals(false, $meeting->reminder_checked);
        self::assertEquals(-1, $meeting->email_reminder_time);
        self::assertEquals(false, $meeting->email_reminder_checked);
        self::assertEquals('Accounts', $meeting->parent_type);
    }

    public function testget_list_view_data(): void
    {
        $meeting = BeanFactory::newBean('Meetings');
        $current_theme = SugarThemeRegistry::current();

        //preset required attribute values
        $meeting->status == 'Planned';
        $meeting->parent_type = 'Accounts';
        $meeting->contact_id = 1;
        $meeting->contact_name = 'test';
        $meeting->parent_name = 'Account';

        $expected = array(
                      'DELETED' => 0,
                      'PARENT_TYPE' => 'Accounts',
                      'STATUS' => 'Planned',
                      'TYPE' => 'Sugar',
                      'REMINDER_TIME' => '-1',
                      'EMAIL_REMINDER_TIME' => '-1',
                      'EMAIL_REMINDER_SENT' => '0',
                      'SEQUENCE' => '0',
                      'CONTACT_NAME' => 'test',
                      'PARENT_NAME' => '',
                      'CONTACT_ID' => 1,
                      'REPEAT_INTERVAL' => '1',
                      'PARENT_MODULE' => 'Accounts',
                      'SET_COMPLETE' => '<a id=\'\' onclick=\'SUGAR.util.closeActivityPanel.show("Meetings","","Held","listview","1");\'><img src="themes/'.$current_theme.'/images/close_inline.png?v=fqXdFZ_r6FC1K7P_Fy3mVw"     border=\'0\' alt="Close" /></a>',
                      'DATE_START' => '<font class=\'overdueTask\'></font>',
                      'REMINDER_CHECKED' => false,
                      'EMAIL_REMINDER_CHECKED' => false,
                    );

        $actual = $meeting->get_list_view_data();

        //$this->assertSame($expected, $actual);
        self::assertEquals($expected['PARENT_TYPE'], $actual['PARENT_TYPE']);
        self::assertEquals($expected['STATUS'], $actual['STATUS']);
        self::assertEquals($expected['TYPE'], $actual['TYPE']);
        self::assertEquals($expected['REMINDER_TIME'], $actual['REMINDER_TIME']);
        self::assertEquals($expected['EMAIL_REMINDER_TIME'], $actual['EMAIL_REMINDER_TIME']);
        self::assertEquals($expected['EMAIL_REMINDER_SENT'], $actual['EMAIL_REMINDER_SENT']);
        self::assertEquals($expected['CONTACT_NAME'], $actual['CONTACT_NAME']);
        self::assertEquals($expected['CONTACT_ID'], $actual['CONTACT_ID']);
        self::assertEquals($expected['REPEAT_INTERVAL'], $actual['REPEAT_INTERVAL']);
        self::assertEquals($expected['PARENT_MODULE'], $actual['PARENT_MODULE']);
    }

    public function testset_notification_body(): void
    {
        global $current_user;
        $current_user = new User(1);

        $meeting = BeanFactory::newBean('Meetings');

        //test with attributes preset and verify template variables are set accordingly
        $meeting->name = 'test';
        $meeting->status = 'Not Held';
        $meeting->type = 'Sugar';
        $meeting->description = 'test description';
        $meeting->duration_hours = 1;
        $meeting->duration_minutes = 1;
        $meeting->date_start = '2016-02-11 17:30:00';
        $meeting->date_end = '2016-02-11 17:30:00';

        $result = $meeting->set_notification_body(new Sugar_Smarty(), $meeting);

        self::assertEquals($meeting->name, $result->tpl_vars['MEETING_SUBJECT']->value);
        self::assertEquals($meeting->status, $result->tpl_vars['MEETING_STATUS']->value);
        self::assertEquals('SuiteCRM', $result->tpl_vars['MEETING_TYPE']->value);
        self::assertEquals($meeting->duration_hours, $result->tpl_vars['MEETING_HOURS']->value);
        self::assertEquals($meeting->duration_minutes, $result->tpl_vars['MEETING_MINUTES']->value);
        self::assertEquals($meeting->description, $result->tpl_vars['MEETING_DESCRIPTION']->value);
    }

    public function testcreate_notification_email(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        $meeting->date_start = '2016-02-11 17:30:00';
        $meeting->date_end = '2016-02-11 17:30:00';

        //test without setting user
        $result = $meeting->create_notification_email(BeanFactory::newBean('Users'));
        self::assertInstanceOf('SugarPHPMailer', $result);

        //test with valid user
        $result = $meeting->create_notification_email(new User(1));
        self::assertInstanceOf('SugarPHPMailer', $result);
    }

    public function testsend_assignment_notifications(): void
    {
        $notify_user = new User(1);

        $meeting = BeanFactory::newBean('Meetings');

        $meeting->date_start = '2016-02-11 17:30:00';
        $meeting->date_end = '2016-02-11 17:30:00';
        $meeting->sentAssignmentNotifications = [];
        $meeting->sentAssignmentNotifications[] = $notify_user->id;

        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $meeting->send_assignment_notifications($notify_user, $admin);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_meeting_users(): void
    {
        $result = BeanFactory::newBean('Meetings')->get_meeting_users();
        self::assertIsArray($result);
    }

    public function testget_invite_meetings(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        $user = BeanFactory::newBean('Users');
        $result = $meeting->get_invite_meetings($user);
        self::assertIsArray($result);
    }

    public function testget_notification_recipients(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        //test without special_notification
        $result = $meeting->get_notification_recipients();
        self::assertIsArray($result);

        //test with special_notification
        $meeting->special_notification = 1;
        $result = $meeting->get_notification_recipients();
        self::assertIsArray($result);
    }

    public function testbean_implements(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        self::assertEquals(false, $meeting->bean_implements('')); //test with blank value
        self::assertEquals(false, $meeting->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $meeting->bean_implements('ACL')); //test with valid value
    }

    public function testlistviewACLHelper(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        $expected = array('MAIN' => 'a', 'PARENT' => 'a', 'CONTACT' => 'a');
        $actual = $meeting->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testsave_relationship_changes(): void
    {
        $meeting = BeanFactory::newBean('Meetings');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $meeting->save_relationship_changes(false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    /**
     * This will throw FATAL error on php7
     */
    public function testafterImportSave(): void
    {
        require_once 'data/Link.php';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $meeting = BeanFactory::newBean('Meetings');
            //test without parent_type
            $meeting->afterImportSave();

            //test with parent_type Contacts
            $meeting->parent_type = 'Contacts';
            $meeting->afterImportSave();

            //test with parent_type Leads
            $meeting->parent_type = 'Leads';
            $meeting->afterImportSave();

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetDefaultStatus(): void
    {
        $result = BeanFactory::newBean('Meetings')->getDefaultStatus();
        self::assertEquals('Planned', $result);
    }

    public function testgetMeetingsExternalApiDropDown(): void
    {
        $actual = getMeetingsExternalApiDropDown();
        $expected = array('Sugar' => 'SuiteCRM');
        self::assertSame($expected, $actual);
    }

    public function testgetMeetingTypeOptions(): void
    {
        global $dictionary, $app_list_strings;

        $result = getMeetingTypeOptions($dictionary, $app_list_strings);
        self::assertIsArray($result);
    }
}
