<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class LeadTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'Leads');
    }

    public function testLead(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $lead = BeanFactory::getBean('Leads');

        self::assertInstanceOf('Lead', $lead);
        self::assertInstanceOf('Person', $lead);
        self::assertInstanceOf('SugarBean', $lead);

        self::assertEquals('Leads', $lead->module_dir);
        self::assertEquals('Lead', $lead->object_name);
        self::assertEquals('Leads', $lead->object_names);
        self::assertEquals('leads', $lead->table_name);

        self::assertEquals(true, $lead->new_schema);
        self::assertEquals(true, $lead->importable);
    }

    public function testget_account(): void
    {
        $lead = BeanFactory::getBean('Leads');

        //test without pre settting attributes
        $result = $lead->get_account();
        self::assertEquals(null, $result);


        //test with required attributes preset
        $lead->account_id = 1;
        $result = $lead->get_account();
        self::assertEquals(null, $result);
    }

    public function testget_opportunity(): void
    {
        $lead = BeanFactory::getBean('Leads');

        //test without pre settting attributes
        $result = $lead->get_opportunity();
        self::assertEquals(null, $result);


        //test with required attributes preset
        $lead->opportunity_id = 1;
        $result = $lead->get_opportunity();
        self::assertEquals(null, $result);
    }

    public function testget_contact(): void
    {
        $lead = BeanFactory::getBean('Leads');

        //test without pre settting attributes
        $result = $lead->get_contact();
        self::assertEquals(null, $result);


        //test with required attributes preset
        $lead->contact_id = 1;
        $result = $lead->get_contact();
        self::assertEquals(null, $result);
    }

    public function testcreate_list_query(): void
    {
        self::markTestIncomplete('Breaks on php 7.1');
        $lead = BeanFactory::getBean('Leads');

        //test with empty string params
        $expected = "SELECT leads.*, users.user_name assigned_user_name,leads_cstm.* FROM leads 			LEFT JOIN users\n                                ON leads.assigned_user_id=users.id LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = leads.id AND eabl.bean_module = 'Leads' and eabl.primary_address = 1 and eabl.deleted=0 LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id)  LEFT JOIN leads_cstm ON leads.id = leads_cstm.id_c where  leads.deleted=0 ";
        $actual = $lead->create_list_query('', '');
        self::assertSame($expected, $actual);


        //test with valid string params
        $expected = "SELECT leads.*, users.user_name assigned_user_name,leads_cstm.* FROM leads 			LEFT JOIN users\n                                ON leads.assigned_user_id=users.id LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = leads.id AND eabl.bean_module = 'Leads' and eabl.primary_address = 1 and eabl.deleted=0 LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id)  LEFT JOIN leads_cstm ON leads.id = leads_cstm.id_c where (users.user_name=\"\") AND  leads.deleted=0  ORDER BY leads.id";
        $actual = $lead->create_list_query('leads.id', 'users.user_name=""');
        self::assertSame($expected, $actual);
    }

    /**
     * @todo: NEEDS FIXING!
     */
    public function testcreate_new_list_query(): void
    {
        /*
        $lead = BeanFactory::getBean('Leads');

        //test with empty string params
        $expected = " SELECT  leads.* , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' e_invite_status_fields , '                                    '  fp_events_leads_1fp_events_ida , '                                                                                                                                                                                                                                                              ' e_accept_status_fields , LTRIM(RTRIM(CONCAT(IFNULL(leads.first_name,''),' ',IFNULL(leads.last_name,'')))) as name , jt3.user_name modified_by_name , jt3.created_by modified_by_name_owner  , 'Users' modified_by_name_mod , jt4.user_name created_by_name , jt4.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt5.user_name assigned_user_name , jt5.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod, LTRIM(RTRIM(CONCAT(IFNULL(leads.first_name,''),' ',IFNULL(leads.last_name,'')))) as full_name , jt6.name campaign_name , jt6.assigned_user_id campaign_name_owner  , 'Campaigns' campaign_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM leads   LEFT JOIN  users jt3 ON leads.modified_user_id=jt3.id AND jt3.deleted=0\n\n AND jt3.deleted=0  LEFT JOIN  users jt4 ON leads.created_by=jt4.id AND jt4.deleted=0\n\n AND jt4.deleted=0  LEFT JOIN  users jt5 ON leads.assigned_user_id=jt5.id AND jt5.deleted=0\n\n AND jt5.deleted=0  LEFT JOIN  campaigns jt6 ON leads.campaign_id=jt6.id AND jt6.deleted=0\n\n AND jt6.deleted=0 where leads.deleted=0";
        $actual = $lead->create_new_list_query('','');
        $this->assertSame($expected,$actual);


        //test with valid string params
        $expected = " SELECT  leads.* , '                                                                                                                                                                                                                                                              ' c_accept_status_fields , '                                    '  call_id , '                                                                                                                                                                                                                                                              ' e_invite_status_fields , '                                    '  fp_events_leads_1fp_events_ida , '                                                                                                                                                                                                                                                              ' e_accept_status_fields , LTRIM(RTRIM(CONCAT(IFNULL(leads.first_name,''),' ',IFNULL(leads.last_name,'')))) as name , jt3.user_name modified_by_name , jt3.created_by modified_by_name_owner  , 'Users' modified_by_name_mod , jt4.user_name created_by_name , jt4.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt5.user_name assigned_user_name , jt5.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod, LTRIM(RTRIM(CONCAT(IFNULL(leads.first_name,''),' ',IFNULL(leads.last_name,'')))) as full_name , jt6.name campaign_name , jt6.assigned_user_id campaign_name_owner  , 'Campaigns' campaign_name_mod, '                                                                                                                                                                                                                                                              ' m_accept_status_fields , '                                    '  meeting_id  FROM leads   LEFT JOIN  users jt3 ON leads.modified_user_id=jt3.id AND jt3.deleted=0\n\n AND jt3.deleted=0  LEFT JOIN  users jt4 ON leads.created_by=jt4.id AND jt4.deleted=0\n\n AND jt4.deleted=0  LEFT JOIN  users jt5 ON leads.assigned_user_id=jt5.id AND jt5.deleted=0\n\n AND jt5.deleted=0  LEFT JOIN  campaigns jt6 ON leads.campaign_id=jt6.id AND jt6.deleted=0\n\n AND jt6.deleted=0 where (users.user_name=\"\") AND leads.deleted=0";
        $actual = $lead->create_new_list_query('leads.id','users.user_name=""');
        $this->assertSame($expected,$actual);
        */
        self::assertTrue(true, "NEEDS FIXING!");
    }

    public function testSaveAndConverted_lead(): void
    {
        self::markTestSkipped("converted_lead: Error in query, id's not properly escaped ");

        $lead = BeanFactory::getBean('Leads');

        $lead->first_name = "firstn";
        $lead->last_name = "lastnn";
        $lead->lead_source = "test";

        $result = $lead->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($lead->id));
        self::assertEquals(36, strlen((string) $lead->id));
        self::assertEquals("New", $lead->status);


        //test converted_lead method after saving
        /*$lead->converted_lead("'" . $lead->id . "'" , "'1'", "'1'", "'1'");

        //retrieve back to test if attributes are updated in db
        $lead = $lead->retrieve($lead->id);
        $this->assertEquals("Converted", $lead->status);
        $this->assertEquals("1", $lead->converted);
        $this->assertEquals("1", $lead->contact_id);
        $this->assertEquals("1", $lead->account_id);
        $this->assertEquals("1", $lead->opportunity_id);
        */

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $lead->mark_deleted($lead->id);
        $result = $lead->retrieve($lead->id);
        self::assertEquals(null, $result);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $lead = BeanFactory::newBean('Leads');

        $lead->first_name = "firstn";
        $lead->last_name = "lastn";

        $lead->fill_in_additional_list_fields();

        self::assertEquals("firstn lastn", $lead->name);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $lead->first_name = "firstn";
        $lead->last_name = "lastn";

        $lead->fill_in_additional_detail_fields();

        self::assertEquals("firstn lastn", $lead->name);
    }

    public function testget_list_view_data(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = array(
            'NAME' => ' ',
            'DELETED' => 0,
            'FULL_NAME' => ' ',
            'DO_NOT_CALL' => '0',
            'CONVERTED' => '0',
            'ENCODED_NAME' => ' ',
            'EMAIL1' => '',
            'EMAIL1_LINK' => '            <a class="email-link" href="mailto:"
                    onclick="$(document).openComposeViewModal(this);"
                    data-module="Leads" data-record-id=""
                    data-module-name=" " data-email-address=""
                ></a>',
            'ACC_NAME_FROM_ACCOUNTS' => null,
        );

        $actual = $lead->get_list_view_data();

        //$this->assertSame($expected, $actual);
        self::assertEquals($expected['NAME'], $actual['NAME']);
        self::assertEquals($expected['DELETED'], $actual['DELETED']);
        self::assertEquals($expected['FULL_NAME'], $actual['FULL_NAME']);
        self::assertEquals($expected['DO_NOT_CALL'], $actual['DO_NOT_CALL']);
        self::assertEquals($expected['EMAIL1_LINK'], $actual['EMAIL1_LINK']);
    }

    public function testget_linked_fields(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = array(
            'created_by_link',
            'modified_user_link',
            'assigned_user_link',
            'email_addresses_primary',
            'email_addresses',
            'reports_to_link',
            'reportees',
            'contacts',
            'accounts',
            'contact',
            'opportunity',
            'campaign_leads',
            'tasks',
            'notes',
            'meetings',
            'calls',
            'emails',
            'campaigns',
            'prospect_lists',
            'fp_events_leads_1',
            'SecurityGroups',
        );
        $actual = $lead->get_linked_fields();
        self::assertIsArray($actual);
        sort($expected);
        $actualKeys = array_keys($actual);
        sort($actualKeys);
        self::assertSame($expected, $actualKeys);
    }

    public function testbuild_generic_where_clause(): void
    {
        self::markTestSkipped('State dependecy');

        $lead = BeanFactory::getBean('Leads');

        //test with empty string params
        $expected = "leads.last_name like '%' or leads.account_name like '%' or leads.first_name like '%' or ea.email_address like '%'";
        $actual = $lead->build_generic_where_clause("");
        self::assertSame($expected, $actual);


        //test with valid string params
        $expected = "leads.last_name like '%' or leads.account_name like '%' or leads.first_name like '%' or ea.email_address like '%'";
        $actual = $lead->build_generic_where_clause("123");
        self::assertSame($expected, $actual);
    }

    public function testset_notification_body(): void
    {
        $lead = BeanFactory::getBean('Leads');

        //test with attributes preset and verify template variables are set accordingly

        $lead->first_name = "firstn";
        $lead->last_name = "lastn";
        $lead->salutation = "Mr";
        $lead->lead_source = "Email";
        $lead->status = "New";
        $lead->description = "tes description";

        $result = $lead->set_notification_body(new Sugar_Smarty(), $lead);

        self::assertEquals("Mr firstn lastn", $result->tpl_vars['LEAD_NAME']->value);
        self::assertEquals($lead->lead_source, $result->tpl_vars['LEAD_SOURCE']->value);
        self::assertEquals($lead->status, $result->tpl_vars['LEAD_STATUS']->value);
        self::assertEquals($lead->description, $result->tpl_vars['LEAD_DESCRIPTION']->value);
    }

    public function testbean_implements(): void
    {
        $lead = BeanFactory::getBean('Leads');

        self::assertEquals(false, $lead->bean_implements('')); //test with blank value
        self::assertEquals(false, $lead->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $lead->bean_implements('ACL')); //test with valid value
    }

    public function testlistviewACLHelper(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = array("MAIN" => "a", "ACCOUNT" => "a", "OPPORTUNITY" => "a", "CONTACT" => "a");
        $actual = $lead->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testconvertCustomFieldsForm(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $form = "";
        $prefix = "";
        $tempBean = BeanFactory::newBean('Contacts');

        $result = $lead->convertCustomFieldsForm($form, $tempBean, $prefix);

        self::assertEquals(true, $result);
        self::assertgreaterThanOrEqual("", $form); //no filed with source = custom_fields
    }

    public function testget_unlinked_email_query(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = "SELECT emails.id FROM emails  JOIN (select DISTINCT email_id from emails_email_addr_rel eear

	join email_addr_bean_rel eabr on eabr.bean_id ='' and eabr.bean_module = 'Leads' and
	eabr.email_address_id = eear.email_address_id and eabr.deleted=0
	where eear.deleted=0 and eear.email_id not in
	(select eb.email_id from emails_beans eb where eb.bean_module ='Leads' and eb.bean_id = '')
	) derivedemails on derivedemails.email_id = emails.id";
        $actual = $lead->get_unlinked_email_query();
        self::assertSame($expected, $actual);
    }

    public function testget_old_related_calls(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = array();
        $expected['select'] = 'SELECT calls.id ';
        $expected['from'] = 'FROM calls ';
        $expected['where'] = " WHERE calls.parent_id = '$lead->id'
            AND calls.parent_type = 'Leads' AND calls.id NOT IN ( SELECT call_id FROM calls_leads ) ";
        $expected['join'] = "";
        $expected['join_tables'][0] = '';

        $actual = $lead->get_old_related_calls();
        self::assertSame($expected, $actual);
    }

    public function testGetActivitiesOptions(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = ["copy" => "Copy", "move" => "Move", "donothing" => "Do Nothing"];
        $actual = $lead::getActivitiesOptions();
        self::assertSame($expected, $actual);
    }

    public function testget_old_related_meetings(): void
    {
        $lead = BeanFactory::getBean('Leads');

        $expected = array();
        $expected['select'] = 'SELECT meetings.id ';
        $expected['from'] = 'FROM meetings ';
        $expected['where'] = " WHERE meetings.parent_id = ''
            AND meetings.parent_type = 'Leads' AND meetings.id NOT IN ( SELECT meeting_id FROM meetings_leads ) ";
        $expected['join'] = "";
        $expected['join_tables'][0] = '';

        $actual = $lead->get_old_related_meetings();
        self::assertSame($expected, $actual);
    }
}
