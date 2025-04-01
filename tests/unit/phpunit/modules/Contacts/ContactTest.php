<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ContactTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testContact(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $contact = BeanFactory::newBean('Contacts');
        self::assertInstanceOf('Contact', $contact);
        self::assertInstanceOf('Person', $contact);
        self::assertInstanceOf('SugarBean', $contact);

        self::assertEquals('Contacts', $contact->module_dir);
        self::assertEquals('Contact', $contact->object_name);
        self::assertEquals('contacts', $contact->table_name);
        self::assertEquals('accounts_contacts', $contact->rel_account_table);
        self::assertEquals('opportunities_contacts', $contact->rel_opportunity_table);
        self::assertEquals(true, $contact->importable);
        self::assertEquals(true, $contact->new_schema);
    }

    public function testadd_list_count_joins(): void
    {
        self::markTestIncomplete('Breaks on php 7.1');

        $contact = BeanFactory::newBean('Contacts');

        //test with empty strings
        $query = "";
        $contact->add_list_count_joins($query, '');
        self::assertEquals(" LEFT JOIN contacts_cstm ON contacts.id = contacts_cstm.id_c ", $query);


        //test with valid string
        $query = "";
        $expected = "\n	            LEFT JOIN accounts_contacts\n	            ON contacts.id=accounts_contacts.contact_id\n	            LEFT JOIN accounts\n	            ON accounts_contacts.account_id=accounts.id\n                    LEFT JOIN contacts_cstm ON contacts.id = contacts_cstm.id_c ";
        $contact->add_list_count_joins($query, 'accounts.name');
        $query = preg_replace('/\s+/', '', (string) $query);
        $expected =preg_replace('/\s+/', '', (string) $expected);
        self::assertSame($expected, $query);

        //test with valid string
        $query = "";
        $expected = "\n	            LEFT JOIN accounts_contacts\n	            ON contacts.id=accounts_contacts.contact_id\n	            LEFT JOIN accounts\n	            ON accounts_contacts.account_id=accounts.id\n	                 LEFT JOIN contacts_cstm ON contacts.id = contacts_cstm.id_c ";
        $contact->add_list_count_joins($query, 'contacts.name');
        self::assertSame(" LEFT JOIN contacts_cstm ON contacts.id = contacts_cstm.id_c ", $query);
    }

    public function testlistviewACLHelper(): void
    {
        self::markTestIncomplete('environment dependency');


        $contact = BeanFactory::newBean('Contacts');

        $expected = array( "MAIN"=>"span", "ACCOUNT"=>"span");
        $actual = $contact->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    /**
     * @todo: NEEDS FIXING!
     */
    public function testcreate_new_list_query(): void
    {
        /*
        $contact = BeanFactory::newBean('Contacts');

        //test without request action parameter
        $expected =" SELECT  contacts.* , '                                                                                                                                                                                                                                                              ' opportunity_role_fields , '                                    '  opportunity_id , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' e_invite_status_fields , '                                    '  fp_events_contactsfp_events_ida , '                                                                                                                                                                                                                                                              ' e_accept_status_fields , LTRIM(RTRIM(CONCAT(IFNULL(contacts.first_name,''),' ',IFNULL(contacts.last_name,'')))) as name , jt4.user_name modified_by_name , jt4.created_by modified_by_name_owner  , 'Users' modified_by_name_mod , jt5.user_name created_by_name , jt5.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt6.user_name assigned_user_name , jt6.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod, LTRIM(RTRIM(CONCAT(IFNULL(contacts.first_name,''),' ',IFNULL(contacts.last_name,'')))) as full_name, '                                                                                                                                                                                                                                                              ' account_name , '                                    '  account_id  , jt8.last_name report_to_name , jt8.assigned_user_id report_to_name_owner  , 'Contacts' report_to_name_mod , jt9.name campaign_name , jt9.assigned_user_id campaign_name_owner  , 'Campaigns' campaign_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM contacts   LEFT JOIN  users jt4 ON contacts.modified_user_id=jt4.id AND jt4.deleted=0\n\n AND jt4.deleted=0  LEFT JOIN  users jt5 ON contacts.created_by=jt5.id AND jt5.deleted=0\n\n AND jt5.deleted=0  LEFT JOIN  users jt6 ON contacts.assigned_user_id=jt6.id AND jt6.deleted=0\n\n AND jt6.deleted=0  LEFT JOIN  contacts jt8 ON contacts.reports_to_id=jt8.id AND jt8.deleted=0\n\n AND jt8.deleted=0  LEFT JOIN  campaigns jt9 ON contacts.campaign_id=jt9.id AND jt9.deleted=0\n\n AND jt9.deleted=0 where (account.name is null) AND contacts.deleted=0";
        $actual = $contact->create_new_list_query("account.name","account.name is null");
        $this->assertSame($expected,$actual);

        //test with request action parameter = ContactAddressPopup
        $_REQUEST['action'] = 'ContactAddressPopup';
        $expected = "SELECT LTRIM(RTRIM(CONCAT(IFNULL(contacts.first_name,''),'',IFNULL(contacts.last_name,'')))) name, \n				contacts.*,\n                accounts.name as account_name,\n                accounts.id as account_id,\n                accounts.assigned_user_id account_id_owner,\n                users.user_name as assigned_user_name \n                FROM contacts LEFT JOIN users\n	                    ON contacts.assigned_user_id=users.id\n	                    LEFT JOIN accounts_contacts\n	                    ON contacts.id=accounts_contacts.contact_id  and accounts_contacts.deleted = 0\n	                    LEFT JOIN accounts\n	                    ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = contacts.id AND eabl.bean_module = 'Contacts' and eabl.primary_address = 1 and eabl.deleted=0 LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id) where (account.name is null) AND  contacts.deleted=0 ";
        $actual = $contact->create_new_list_query("account.name","account.name is null");
        $this->assertSame($expected,$actual);
        */
        self::assertTrue(true, "NEEDS FIXING!");
    }

    public function testaddress_popup_create_new_list_query(): void
    {
        self::markTestIncomplete('Breaks on php 7.1');
        $contact = BeanFactory::newBean('Contacts');

        //test with empty string params
        $expected = "SELECT LTRIM(RTRIM(CONCAT(IFNULL(contacts.first_name,''),'',IFNULL(contacts.last_name,'')))) name, \n				contacts.*,\n                accounts.name as account_name,\n                accounts.id as account_id,\n                accounts.assigned_user_id account_id_owner,\n                users.user_name as assigned_user_name ,contacts_cstm.*\n                FROM contacts LEFT JOIN users\n	                    ON contacts.assigned_user_id=users.id\n	                    LEFT JOIN accounts_contacts\n	                    ON contacts.id=accounts_contacts.contact_id  and accounts_contacts.deleted = 0\n	                    LEFT JOIN accounts\n	                    ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = contacts.id AND eabl.bean_module = 'Contacts' and eabl.primary_address = 1 and eabl.deleted=0 LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id)  LEFT JOIN contacts_cstm ON contacts.id = contacts_cstm.id_c where  contacts.deleted=0 ";
        $actual = $contact->address_popup_create_new_list_query('', '');
        self::assertSame($expected, $actual);


        //test with valid string params
        $expected = "SELECT LTRIM(RTRIM(CONCAT(IFNULL(contacts.first_name,''),'',IFNULL(contacts.last_name,'')))) name, \n				contacts.*,\n                accounts.name as account_name,\n                accounts.id as account_id,\n                accounts.assigned_user_id account_id_owner,\n                users.user_name as assigned_user_name ,contacts_cstm.*\n                FROM contacts LEFT JOIN users\n	                    ON contacts.assigned_user_id=users.id\n	                    LEFT JOIN accounts_contacts\n	                    ON contacts.id=accounts_contacts.contact_id  and accounts_contacts.deleted = 0\n	                    LEFT JOIN accounts\n	                    ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = contacts.id AND eabl.bean_module = 'Contacts' and eabl.primary_address = 1 and eabl.deleted=0 LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id)  LEFT JOIN contacts_cstm ON contacts.id = contacts_cstm.id_c where (contacts.name=\"\") AND  contacts.deleted=0 ";
        $actual = $contact->address_popup_create_new_list_query('contacts.id', 'contacts.name=""');
        self::assertSame($expected, $actual);
    }

    public function testcreate_export_query(): void
    {
        self::markTestIncomplete('Refactor needed as field ording changes on different test environments');
    }

    public function testfill_in_additional_list_fields(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        //test with attributes preset and verify attributes are set accordingly
        $contact->first_name = "firstn";
        $contact->last_name = "lastn";
        $contact->email1 = "1@test.com";
        $contact->email2 = "2@test.com";


        $contact->fill_in_additional_list_fields();

        self::assertEquals("firstn lastn", $contact->full_name);
        self::assertEquals("firstn lastn &lt;1@test.com&gt;", $contact->email_and_name1);
        self::assertEquals("firstn lastn &lt;2@test.com&gt;", $contact->email_and_name2);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        //test with attributes preset and verify attributes are set accordingly
        $contact->id = "1";

        $contact->fill_in_additional_detail_fields();

        self::assertEquals("", $contact->account_name);
        self::assertEquals("", $contact->account_id);
        self::assertEquals("", $contact->report_to_name);
    }

    public function testload_contacts_users_relationship(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $contact->load_contacts_users_relationship();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        //test with attributes preset and verify attributes are set accordingly
        $contact->first_name = "first";
        $contact->last_name = "last";

        $expected = array(
                      'NAME' => 'first last',
                      'DELETED' => 0,
                      'FIRST_NAME' => 'first',
                      'LAST_NAME' => 'last',
                      'FULL_NAME' => 'first last',
                      'DO_NOT_CALL' => '0',
                      'PORTAL_USER_TYPE' => 'Single user',
                      'ENCODED_NAME' => 'first last',
                      'EMAIL1' => '',
                      'EMAIL1_LINK' => '<a href=\'javascript:void(0);\' onclick=\'SUGAR.quickCompose.init({"fullComposeUrl":"contact_id=\\u0026parent_type=Contacts\\u0026parent_id=\\u0026parent_name=first+last\\u0026to_addrs_ids=\\u0026to_addrs_names=first+last\\u0026to_addrs_emails=\\u0026to_email_addrs=first+last%26nbsp%3B%26lt%3B%26gt%3B\\u0026return_module=Contacts\\u0026return_action=ListView\\u0026return_id=","composePackage":{"contact_id":"","parent_type":"Contacts","parent_id":"","parent_name":"first last","to_addrs_ids":"","to_addrs_names":"first last","to_addrs_emails":"","to_email_addrs":"first last \\u003C\\u003E","return_module":"Contacts","return_action":"ListView","return_id":""}});\' class=\'\'></a>',
                      'EMAIL_AND_NAME1' => 'first last &lt;&gt;',
                    );

        $actual = $contact->get_list_view_data();

        self::assertEquals($expected['NAME'], $actual['NAME']);
        self::assertEquals($expected['FIRST_NAME'], $actual['FIRST_NAME']);
        self::assertEquals($expected['LAST_NAME'], $actual['LAST_NAME']);
        self::assertEquals($expected['FULL_NAME'], $actual['FULL_NAME']);
        self::assertEquals($expected['ENCODED_NAME'], $actual['ENCODED_NAME']);
        self::assertEquals($expected['EMAIL_AND_NAME1'], $actual['EMAIL_AND_NAME1']);
    }

    public function testbuild_generic_where_clause(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        //test with string
        $expected = "contacts.last_name like 'test%' or contacts.first_name like 'test%' or accounts.name like 'test%' or contacts.assistant like 'test%' or ea.email_address like 'test%'";
        $actual = $contact->build_generic_where_clause('test');
        self::assertSame($expected, $actual);


        //test with number
        $expected = "contacts.last_name like '1%' or contacts.first_name like '1%' or accounts.name like '1%' or contacts.assistant like '1%' or ea.email_address like '1%' or contacts.phone_home like '%1%' or contacts.phone_mobile like '%1%' or contacts.phone_work like '%1%' or contacts.phone_other like '%1%' or contacts.phone_fax like '%1%' or contacts.assistant_phone like '%1%'";
        $actual = $contact->build_generic_where_clause(1);
        self::assertSame($expected, $actual);
    }

    public function testset_notification_body(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        //test with attributes preset and verify attributes are set accordingly
        $contact->first_name = "first";
        $contact->last_name = "last";
        $contact->description = "some text";
        $contact->fill_in_additional_list_fields();

        $result = $contact->set_notification_body(new Sugar_Smarty(), $contact);

        self::assertEquals($contact->full_name, $result->tpl_vars['CONTACT_NAME']->value);
        self::assertEquals($contact->description, $result->tpl_vars['CONTACT_DESCRIPTION']->value);
    }

    public function testget_contact_id_by_email(): void
    {
        $result = BeanFactory::newBean('Contacts')->get_contact_id_by_email("");
        self::assertEquals(null, $result);


        self::markTestSkipped('Invalid Columns(email1,email2) in Query ');
    }

    public function testsave_relationship_changes(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $contact->save_relationship_changes(true);
            $contact->save_relationship_changes(false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testbean_implements(): void
    {
        $contact = BeanFactory::newBean('Contacts');
        self::assertEquals(false, $contact->bean_implements('')); //test with blank value
        self::assertEquals(false, $contact->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $contact->bean_implements('ACL')); //test with valid value
    }

    public function testget_unlinked_email_query(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        //execute the method and verify that it retunrs expected results
        $expected = "SELECT emails.id FROM emails  JOIN (select DISTINCT email_id from emails_email_addr_rel eear

	join email_addr_bean_rel eabr on eabr.bean_id ='' and eabr.bean_module = 'Contacts' and
	eabr.email_address_id = eear.email_address_id and eabr.deleted=0
	where eear.deleted=0 and eear.email_id not in
	(select eb.email_id from emails_beans eb where eb.bean_module ='Contacts' and eb.bean_id = '')
	) derivedemails on derivedemails.email_id = emails.id";
        $actual = $contact->get_unlinked_email_query();
        self::assertSame($expected, $actual);
    }

    public function testprocess_sync_to_outlook(): void
    {
        $contact = BeanFactory::newBean('Contacts');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $contact->process_sync_to_outlook("all");
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $contact->process_sync_to_outlook("1");
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }
}
