<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class EmployeeTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testEmployee(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $employee = BeanFactory::newBean('Employees');
        self::assertInstanceOf('Employee', $employee);
        self::assertInstanceOf('Person', $employee);
        self::assertInstanceOf('SugarBean', $employee);

        self::assertEquals('Employees', $employee->module_dir);
        self::assertEquals('Employee', $employee->object_name);
        self::assertEquals('users', $employee->table_name);
        self::assertEquals(true, $employee->new_schema);
    }

    public function testget_summary_text(): void
    {
        $employee = BeanFactory::newBean('Employees');

        //test without setting name
        self::assertEquals(' ', $employee->get_summary_text());

        //test with name set
        $employee->retrieve(1);
        self::assertEquals('Administrator', $employee->get_summary_text());
    }

    public function testfill_in_additional_list_fields(): void
    {
        $employee = BeanFactory::newBean('Employees');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $employee->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $employee = BeanFactory::newBean('Employees');

        //test with a empty employee bean
        $employee->fill_in_additional_detail_fields();
        self::assertEquals("", $employee->reports_to_name);


        //test with a valid employee bean
        $employee->retrieve(1);
        $employee->fill_in_additional_detail_fields();
        self::assertEquals("", $employee->reports_to_name);
    }

    public function testretrieve_employee_id(): void
    {
        $employee = BeanFactory::newBean('Employees');
        //$this->assertEquals('1' ,$employee->retrieve_employee_id('admin'));

        self::markTestSkipped('Bug in query: employee_name parameter is wrongly used as user_name');
    }

    public function testverify_data(): void
    {
        $employee = BeanFactory::newBean('Employees');
        self::assertEquals(true, $employee->verify_data());
    }

    public function testget_list_view_data(): void
    {
        $employee = BeanFactory::newBean('Employees');

        $expected = array(
            'SUGAR_LOGIN' => '1',
            'FULL_NAME' => ' ',
            'NAME' => ' ',
            'IS_ADMIN' => '0',
            'EXTERNAL_AUTH_ONLY' => '0',
            'RECEIVE_NOTIFICATIONS' => '1',
            'DELETED' => 0,
            'PORTAL_ONLY' => '0',
            'SHOW_ON_EMPLOYEES' => '1',
            'ENCODED_NAME' => ' ',
            'EMAIL1' => '',
            'EMAIL1_LINK' => '            <a class="email-link" href="mailto:"
                    onclick="$(document).openComposeViewModal(this);"
                    data-module="Employees" data-record-id=""
                    data-module-name=" " data-email-address=""
                ></a>',
            'MESSENGER_TYPE' => '',
            'REPORTS_TO_NAME' => null,
        );

        $actual = $employee->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testlist_view_parse_additional_sections(): void
    {
        $employee = BeanFactory::newBean('Employees');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $ss = new Sugar_Smarty();
            $employee->list_view_parse_additional_sections($ss, null);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcreate_export_query(): void
    {
        $employee = BeanFactory::newBean('Employees');

        global $current_user;
        $current_user->is_admin = '1';
        //test with empty string params
        $expected = "SELECT id, user_name, first_name, last_name, description, date_entered, date_modified, modified_user_id, created_by, title, department, is_admin, phone_home, phone_mobile, phone_work, phone_other, phone_fax, address_street, address_city, address_state, address_postalcode, address_country, reports_to_id, portal_only, status, receive_notifications, employee_status, messenger_id, messenger_type, is_group FROM users  WHERE  users.deleted = 0 ORDER BY users.user_name";
        $actual = $employee->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT id, user_name, first_name, last_name, description, date_entered, date_modified, modified_user_id, created_by, title, department, is_admin, phone_home, phone_mobile, phone_work, phone_other, phone_fax, address_street, address_city, address_state, address_postalcode, address_country, reports_to_id, portal_only, status, receive_notifications, employee_status, messenger_id, messenger_type, is_group FROM users  WHERE users.user_name=\"\" AND  users.deleted = 0 ORDER BY users.id";
        $actual = $employee->create_export_query('users.id', 'users.user_name=""');
        self::assertSame($expected, $actual);

        $current_user->is_admin = '0';
        $this->expectException(RuntimeException::class);
        $employee->create_export_query('', '');

    }

    public function testpreprocess_fields_on_save(): void
    {
        $employee = BeanFactory::newBean('Employees');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $employee->preprocess_fields_on_save();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    /**
     * @todo: NEEDS FIXING!
     */
    public function testcreate_new_list_query(): void
    {
        /*
    	$employee = BeanFactory::newBean('Employees');

    	//test with empty string params
    	$expected = " SELECT  users.* , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' securitygroup_noninher_fields , '                                    '  securitygroup_id , LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as full_name, LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as name , jt2.last_name reports_to_name , jt2.created_by reports_to_name_owner  , 'Users' reports_to_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM users   LEFT JOIN  users jt2 ON users.reports_to_id=jt2.id AND jt2.deleted=0\n\n AND jt2.deleted=0 where ( users.portal_only = 0 ) AND users.deleted=0";
    	$actual = $employee->create_new_list_query('','');
    	$this->assertSame($expected,$actual);


    	//test with valid string params
    	$expected = " SELECT  users.* , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' securitygroup_noninher_fields , '                                    '  securitygroup_id , LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as full_name, LTRIM(RTRIM(CONCAT(IFNULL(users.first_name,''),' ',IFNULL(users.last_name,'')))) as name , jt2.last_name reports_to_name , jt2.created_by reports_to_name_owner  , 'Users' reports_to_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM users   LEFT JOIN  users jt2 ON users.reports_to_id=jt2.id AND jt2.deleted=0\n\n AND jt2.deleted=0 where (users.user_name=\"\" and users.portal_only = 0 ) AND users.deleted=0";
    	$actual = $employee->create_new_list_query('users.id','users.user_name=""');
    	$this->assertSame($expected,$actual);
    	*/
        self::markTestIncomplete();
    }

    public function testhasCustomFields(): void
    {
        $result = BeanFactory::newBean('Employees')->hasCustomFields();
        self::assertEquals(false, $result);
    }

    public function testError(): void
    {
        global $app_strings;

        // setup
        self::assertNotTrue(isset($app_strings['TEST_ERROR_MESSAGE']));

        // test if there is no error

        ob_start();
        include __DIR__ . '/../../../../../modules/Employees/Error.php';
        $contents = ob_get_contents();
        ob_end_clean();
        $expected = '<span class=\'error\'><br><br>' . "\n" . $app_strings['NTC_CLICK_BACK'] . '</span>';
        self::assertStringContainsStringIgnoringCase($expected, $contents);

        // test if there is an error

        $app_strings['TEST_ERROR_MESSAGE'] = 'Hello error';
        $request = [];
        $request['error_string'] = 'TEST_ERROR_MESSAGE';
        self::assertEquals('TEST_ERROR_MESSAGE', $request['error_string']);
        ob_start();
        include __DIR__ . '/../../../../../modules/Employees/Error.php';
        $contents = ob_get_contents();
        ob_end_clean();
        $expected = '<span class=\'error\'>Hello error<br><br>' . "\n" . $app_strings['NTC_CLICK_BACK'] . '</span>';
        self::assertStringContainsStringIgnoringCase($expected, $contents);

        unset($app_strings['TEST_ERROR_MESSAGE']);
    }
}
