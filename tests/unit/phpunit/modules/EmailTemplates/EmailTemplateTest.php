<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'modules/EmailTemplates/EmailTemplateParser.php';

class EmailTemplateTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testEmailTemplateParser(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = to_html('<h1>Hello $contact_name</h1>');
        $emailTemplate->body = 'Hello $contact_name';
        $emailTemplate->subject = 'Hello $contact_name';
        $campaign = BeanFactory::newBean('Campaigns');

        $related = [BeanFactory::newBean('Leads'), BeanFactory::newBean('Contacts'), BeanFactory::newBean('Prospects')];
        foreach ($related as $bean) {
            $bean->name = 'foobar';

            $result = (new EmailTemplateParser($emailTemplate, $campaign, $bean, "", ""))->parseVariables();
            self::assertEquals('<h1>Hello foobar</h1>', from_html($result['body_html']));
            self::assertEquals('Hello foobar', $result['body']);
            self::assertEquals('Hello foobar', $result['subject']);
        }
    }

    public function testEmailTemplateParserUser(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body = 'Hello $contact_user_full_name';
        $campaign = BeanFactory::newBean('Campaigns');

        $bean = BeanFactory::newBean('Users');
        $bean->first_name = 'foo';
        $bean->last_name = 'bar';
        $bean->fill_in_additional_detail_fields();

        $result = (new EmailTemplateParser($emailTemplate, $campaign, $bean, "", ""))->parseVariables();
        self::assertEquals('Hello foo bar', $result['body']);
    }

    public function testcreateCopyTemplate(): void
    {
        global $current_user;

        $this->setOutputCallback(function ($msg) {
        });

        $current_user->id = create_guid();
        $_REQUEST['func'] = 'createCopy';
        $_POST['name'] = 'Name';
        $_POST['subject'] = 'Subject';
        $_POST['body_html'] = 'BodyHTML';
        require('modules/EmailTemplates/EmailTemplateData.php');

        $output = json_decode($this->getActualOutput(), true, 512, JSON_THROW_ON_ERROR);
        self::assertNotEmpty($output['data']);
        self::assertNotEmpty($output['data']['id']);
        $template = BeanFactory::newBean('EmailTemplates');
        self::assertNotNull($template->retrieve($output['data']['id']));

        self::assertEquals($current_user->id, $template->assigned_user_id);
    }

    public function testaddDomainToRelativeImagesSrc(): void
    {
        global $sugar_config;

        $template = BeanFactory::newBean('EmailTemplates');
        $html = '<img style="font-family:Arial, Helvetica, sans-serif;font-size:14px;line-height:22.4px;color:#444444;padding:0px;margin:0px;" src="public/c1270a2d-a083-495e-7c61-5c8a9046ec0d.png" alt="c1270a2d-a083-495e-7c61-5c8a9046ec0d.png" width="267" height="200">';
        $template->body_html = to_html($html);
        $sugar_config['site_url'] = 'https://foobar.com';

        $template->addDomainToRelativeImagesSrc();

        $result = from_html($template->body_html);
        self::assertStringContainsString('src="https://foobar.com/public/c1270a2d-a083-495e-7c61-5c8a9046ec0d.png" alt="c1270a2d-a083-495e-7c61-5c8a9046ec0d.png"', $result);
    }

    public function testrepairEntryPointImages(): void
    {
        global $sugar_config;

        $sugar_config['site_url'] = 'https://foobar.com';

        $ids = [create_guid(), create_guid()];
        $html = '<img src="https://foobar.com/index.php?entryPoint=download&type=Notes&id=' . $ids[0] . '&filename=test2.png" alt="" style="font-size:14px;" width="381" height="339">';
        $html .= '<img alt="test.png" src="https://foobar.com/index.php?entryPoint=download&type=Notes&id=' . $ids[1] . '&filename=test.png" width="118" height="105">';

        foreach ($ids as $id) {
            file_put_contents('upload/' . $id, 'IAmAnImage:' . $id);
        }

        $template = BeanFactory::newBean('EmailTemplates');
        $template->body_html = to_html($html);
        $template->new_with_id = true;
        $template->save();
        self::assertNotNull($template->retrieve($template->id));

        foreach ($ids as $id) {
            self::assertTrue(is_file('public/' . $id . '.png'));
            unlink('public/' . $id . '.png');
            unlink('upload/' . $id);
        }

        $expected = '<img src="https://foobar.com/public/' . $ids[0] . '.png" alt="" style="font-size:14px;" width="381" height="339" />';
        $expected .= '<img alt="test.png" src="https://foobar.com/public/' . $ids[1] . '.png" width="118" height="105" />';
        self::assertEquals($expected, from_html($template->body_html));
    }

    public function testEmailTemplate(): void
    {
        // Execute the constructor and check for the Object type and attributes
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        self::assertInstanceOf('EmailTemplate', $emailTemplate);
        self::assertInstanceOf('SugarBean', $emailTemplate);

        self::assertEquals('EmailTemplates', $emailTemplate->module_dir);
        self::assertEquals('EmailTemplate', $emailTemplate->object_name);
        self::assertEquals('email_templates', $emailTemplate->table_name);
        self::assertEquals(true, $emailTemplate->new_schema);
    }

    public function testgenerateFieldDefsJS(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // execute the method and verify that it retunrs expected results
        $expected = 'var field_defs = {"Contacts":[{"name":"contact_name","value":"Name"},{"name":"contact_description","value":"Description"},{"name":"contact_salutation","value":"Salutation"},{"name":"contact_first_name","value":"First Name"},{"name":"contact_last_name","value":"Last Name"},{"name":"contact_full_name","value":"Name"},{"name":"contact_title","value":"Title"},{"name":"contact_photo","value":"Photo"},{"name":"contact_department","value":"Department"},{"name":"contact_phone_home","value":"Home"},{"name":"contact_email","value":"Any Email"},{"name":"contact_phone_mobile","value":"Mobile"},{"name":"contact_phone_work","value":"Office Phone"},{"name":"contact_phone_other","value":"Other Phone"},{"name":"contact_phone_fax","value":"Fax"},{"name":"contact_email1","value":"Email Address"},{"name":"contact_email2","value":"Other Email"},{"name":"contact_primary_address_street","value":"Primary Address Street"},{"name":"contact_primary_address_street_2","value":"Primary Address Street 2"},{"name":"contact_primary_address_street_3","value":"Primary Address Street 3"},{"name":"contact_primary_address_city","value":"Primary Address City"},{"name":"contact_primary_address_state","value":"Primary Address State"},{"name":"contact_primary_address_postalcode","value":"Primary Address Postal Code"},{"name":"contact_primary_address_country","value":"Primary Address Country"},{"name":"contact_alt_address_street","value":"Alternate Address Street"},{"name":"contact_alt_address_street_2","value":"Alternate Address Street 2"},{"name":"contact_alt_address_street_3","value":"Alternate Address Street 3"},{"name":"contact_alt_address_city","value":"Alternate Address City"},{"name":"contact_alt_address_state","value":"Alternate Address State"},{"name":"contact_alt_address_postalcode","value":"Alternate Address Postal Code"},{"name":"contact_alt_address_country","value":"Alternate Address Country"},{"name":"contact_assistant","value":"Assistant"},{"name":"contact_assistant_phone","value":"Assistant Phone"},{"name":"contact_email_addresses_non_primary","value":"Non Primary E-mails"},{"name":"contact_email_and_name1","value":"Name"},{"name":"contact_lead_source","value":"Lead Source"},{"name":"contact_birthdate","value":"Birthdate"},{"name":"contact_joomla_account_id","value":"LBL_JOOMLA_ACCOUNT_ID"},{"name":"contact_joomla_account_access","value":"LBL_JOOMLA_ACCOUNT_ACCESS"},{"name":"contact_portal_user_type","value":"Portal User Type"},{"name":"contact_event_status_name","value":"LBL_LIST_INVITE_STATUS_EVENT"},{"name":"contact_event_invite_id","value":"LBL_LIST_INVITE_STATUS"},{"name":"contact_event_accept_status","value":"LBL_LIST_ACCEPT_STATUS_EVENT"},{"name":"contact_event_status_id","value":"Accept Status"},{"name":"contact_jjwg_maps_address_c","value":""},{"name":"contact_jjwg_maps_geocode_status_c","value":""},{"name":"contact_jjwg_maps_lat_c","value":""},{"name":"contact_jjwg_maps_lng_c","value":""},{"name":"contact_refered_by","value":"Referred By"},{"name":"contact_lead_source_description","value":"Lead Source Description"},{"name":"contact_status","value":"Status"},{"name":"contact_status_description","value":"Status Description"},{"name":"contact_account_name","value":"Account Name"},{"name":"contact_account_id","value":"Account ID"},{"name":"contact_webtolead_email1","value":"Email Address"},{"name":"contact_webtolead_email2","value":"Other Email"},{"name":"contact_portal_name","value":"Portal Name"},{"name":"contact_portal_app","value":"Portal Application"},{"name":"contact_website","value":"Website"},{"name":"contact_tracker_key","value":"Tracker Key"}],"Accounts":[{"name":"account_name","value":"Name"},{"name":"account_description","value":"Description"},{"name":"account_account_type","value":"Type"},{"name":"account_industry","value":"Industry"},{"name":"account_annual_revenue","value":"Annual Revenue"},{"name":"account_phone_fax","value":"Fax"},{"name":"account_billing_address_street","value":"Billing Street"},{"name":"account_billing_address_street_2","value":"Billing Street 2"},{"name":"account_billing_address_street_3","value":"Billing Street 3"},{"name":"account_billing_address_street_4","value":"Billing Street 4"},{"name":"account_billing_address_city","value":"Billing City"},{"name":"account_billing_address_state","value":"Billing State"},{"name":"account_billing_address_postalcode","value":"Billing Postal Code"},{"name":"account_billing_address_country","value":"Billing Country"},{"name":"account_rating","value":"Rating"},{"name":"account_phone_office","value":"Office Phone"},{"name":"account_phone_alternate","value":"Alternate Phone"},{"name":"account_website","value":"Website"},{"name":"account_ownership","value":"Ownership"},{"name":"account_employees","value":"Employees"},{"name":"account_ticker_symbol","value":"Ticker Symbol"},{"name":"account_shipping_address_street","value":"Shipping Street"},{"name":"account_shipping_address_street_2","value":"Shipping Street 2"},{"name":"account_shipping_address_street_3","value":"Shipping Street 3"},{"name":"account_shipping_address_street_4","value":"Shipping Street 4"},{"name":"account_shipping_address_city","value":"Shipping City"},{"name":"account_shipping_address_state","value":"Shipping State"},{"name":"account_shipping_address_postalcode","value":"Shipping Postal Code"},{"name":"account_shipping_address_country","value":"Shipping Country"},{"name":"account_email1","value":"Email Address"},{"name":"account_email_addresses_non_primary","value":"Non Primary E-mails"},{"name":"account_parent_id","value":"Parent Account ID"},{"name":"account_sic_code","value":"SIC Code"},{"name":"account_email","value":"Any Email"},{"name":"account_jjwg_maps_address_c","value":""},{"name":"account_jjwg_maps_geocode_status_c","value":""},{"name":"account_jjwg_maps_lat_c","value":""},{"name":"account_jjwg_maps_lng_c","value":""}],"Users":[{"name":"contact_user_user_name","value":"User Name"},{"name":"contact_user_pwd_last_changed","value":"Password Last Changed"},{"name":"contact_user_first_name","value":"First Name"},{"name":"contact_user_last_name","value":"Last Name"},{"name":"contact_user_full_name","value":"Full Name"},{"name":"contact_user_name","value":"Full Name"},{"name":"contact_user_description","value":"Description"},{"name":"contact_user_title","value":"Title"},{"name":"contact_user_photo","value":"Photo"},{"name":"contact_user_department","value":"Department"},{"name":"contact_user_phone_home","value":"Home Phone"},{"name":"contact_user_phone_mobile","value":"Mobile"},{"name":"contact_user_phone_work","value":"Work Phone"},{"name":"contact_user_phone_other","value":"Other Phone"},{"name":"contact_user_phone_fax","value":"Fax"},{"name":"contact_user_status","value":"Status"},{"name":"contact_user_address_street","value":"Address Street"},{"name":"contact_user_address_city","value":"Address City"},{"name":"contact_user_address_state","value":"Address State"},{"name":"contact_user_address_country","value":"Address Country"},{"name":"contact_user_address_postalcode","value":"Address Postal Code"},{"name":"contact_user_usertype","value":"User Type"},{"name":"contact_user_employee_status","value":"Employee Status"},{"name":"contact_user_messenger_id","value":"IM Name"},{"name":"contact_user_messenger_type","value":"IM Type"},{"name":"contact_user_email1","value":"Email Address"},{"name":"contact_user_email_link_type","value":"Email Client"},{"name":"contact_user_securitygroup_noninherit_id","value":"LBL_securitygroup_noninherit_id"}]};';
        $actual = $emailTemplate->generateFieldDefsJS();
        // $this->assertSame($expected, $actual);

        self::assertGreaterThan(0, strlen((string) $actual));
    }

    public function testget_summary_text(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // test without setting name
        self::assertEquals(null, $emailTemplate->get_summary_text());

        // test with name set
        $emailTemplate->name = 'test';
        self::assertEquals('test', $emailTemplate->get_summary_text());
    }

    public function testcreate_export_query(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // test with empty string params
        $expected = " SELECT  email_templates.*  , jt0.user_name assigned_user_name , jt0.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod FROM email_templates   LEFT JOIN  users jt0 ON email_templates.assigned_user_id=jt0.id AND jt0.deleted=0\n\n AND jt0.deleted=0 where email_templates.deleted=0";
        $actual = $emailTemplate->create_export_query('', '');
        self::assertSame($expected, $actual);

        // test with valid string params
        $expected = " SELECT  email_templates.*  , jt0.user_name assigned_user_name , jt0.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod FROM email_templates   LEFT JOIN  users jt0 ON email_templates.assigned_user_id=jt0.id AND jt0.deleted=0\n\n AND jt0.deleted=0 where (email_templates.name=\"\") AND email_templates.deleted=0";
        $actual = $emailTemplate->create_export_query('email_templates.id', 'email_templates.name=""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $emailTemplate->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // test with attributes preset and verify template variables are set accordingly
        $emailTemplate->created_by = 1;
        $emailTemplate->modified_user_id = 1;
        $emailTemplate->assigned_user_id = 1;
        $emailTemplate->body_html = '<b>some html text</b>';

        $emailTemplate->fill_in_additional_detail_fields();

        self::assertEquals('Administrator', $emailTemplate->created_by_name);
        self::assertEquals('Administrator', $emailTemplate->modified_by_name);
        self::assertEquals('Administrator', $emailTemplate->assigned_user_name);
        self::assertEquals('some html text', $emailTemplate->body);
    }

    public function testfill_in_additional_detail_fields_body_to_text(): void
    {
        // simple examples
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = htmlentities('<h1>Hello</h1><br><a href="https://suitecrm.com">text</b>');
        $emailTemplate->fill_in_additional_detail_fields();
        self::assertEquals("Hello\n\n[text](https://suitecrm.com)", $emailTemplate->body);

        // entities and tags
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = htmlentities('&#60;a&#62;<b>');
        $emailTemplate->fill_in_additional_detail_fields();
        self::assertEquals("<a>", $emailTemplate->body);

        // invalid html
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = htmlentities('foo<bar');
        $emailTemplate->fill_in_additional_detail_fields();
        self::assertEquals("foo", $emailTemplate->body);

        // variables
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = htmlentities('Hello <b>$foo</b>bar');
        $emailTemplate->fill_in_additional_detail_fields();
        self::assertEquals('Hello $foo bar', $emailTemplate->body);

        // variables in URLs (opt-in confirmation emails etc)
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = htmlentities('<a href="$url/index.php?foo=$bar&quux=$baz">text</a>');
        $emailTemplate->fill_in_additional_detail_fields();
        self::assertEquals('[text]($url/index.php?foo=$bar&quux=$baz)', $emailTemplate->body);

        // decoding latin-1 html
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplate->body_html = htmlentities('<meta charset="ISO-8859-1">' . "\xe4", ENT_QUOTES, "ISO-8859-1");
        $emailTemplate->fill_in_additional_detail_fields();
        self::assertEquals("\xc3\xa4", $emailTemplate->body);
    }

    public function testfill_in_additional_parent_fields(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $emailTemplate->fill_in_additional_parent_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // execute the method and verify that it retunrs expected results
        $expected = array(
                'DELETED' => 0,
        );

        $actual = $emailTemplate->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testparse_email_templateAndParse_tracker_urls(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        //test parse_email_template
        $account = BeanFactory::newBean('Accounts');
        $macro_nv = array();

        $expected = array(
                    'subject' => 'test subject', 'body_html' => 'test html', 'body' => 'test body text',
                    );
        $actual = $emailTemplate->parse_email_template(array('subject' => 'test subject', 'body_html' => 'test html', 'body' => 'test body text'), 'Accounts', $account, $macro_nv);
        self::assertSame($expected, $actual);

        // test parse_tracker_urls
        $tracker_url_template = 'localhost/index.php?entryPoint=campaign_trackerv2&track=%s&identifier=tracker_key';
        $removeme_url_template = 'localhost/index.php?entryPoint=removeme&identifier=tracker_key';
        $tracker_urls = array();

        $result = $emailTemplate->parse_tracker_urls($actual, $tracker_url_template, $tracker_urls, $removeme_url_template);
        self::assertSame($expected, $result);
    }

    public function test_convertToType(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        self::assertEquals(10, $emailTemplate->_convertToType('float', 10));
        self::assertEquals('test text', $emailTemplate->_convertToType('text', 'test text'));
        self::assertEquals('$10.00', $emailTemplate->_convertToType('currency', 10));
    }

    /**
     * @todo: NEEDS REVISION: _parseUserValues returns different values in php5 and php7
     * for keys:
     *  contact_user_sugar_login, contact_user_is_admin, contact_user_external_auth_only,
     *  contact_user_receive_notifications, contact_user_modified_by_name, contact_user_created_by_name,
     *  contact_user_deleted, contact_user_portal_only, contact_user_show_on_employees
     *
     */
    public function test_parseUserValues(): void
    {
        /*
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $user = new User(1);
        $repl_arr = array('contact_user_name' => '');

        //execute the method and verify that it retunrs expected results

        $expected = array(
            'contact_user_name' => '',
                'contact_user_id' => '',
                'contact_user_user_name' => '',
                'contact_user_user_hash' => '',
                'contact_user_system_generated_password' => '',
                'contact_user_pwd_last_changed' => '',
                'contact_user_authenticate_id' => '',
                'contact_user_sugar_login' => '1',
                'contact_user_first_name' => '',
                'contact_user_last_name' => '',
                'contact_user_full_name' => '',
                'contact_user_is_admin' => '0',
                'contact_user_external_auth_only' => '0',
                'contact_user_receive_notifications' => '1',
                'contact_user_description' => '',
                'contact_user_date_entered' => '',
                'contact_user_date_modified' => '',
                'contact_user_title' => '',
                'contact_user_photo' => '',
                'contact_user_department' => '',
                'contact_user_phone_home' => '',
                'contact_user_phone_mobile' => '',
                'contact_user_phone_work' => '',
                'contact_user_phone_other' => '',
                'contact_user_phone_fax' => '',
                'contact_user_status' => '',
                'contact_user_address_street' => '',
                'contact_user_address_city' => '',
                'contact_user_address_state' => '',
                'contact_user_address_country' => '',
                'contact_user_address_postalcode' => '',
                'contact_user_UserType' => '',
                'contact_user_deleted' => 0,
                'contact_user_portal_only' => '0',
                'contact_user_show_on_employees' => '1',
                'contact_user_employee_status' => '',
                'contact_user_messenger_id' => '',
                'contact_user_messenger_type' => '',
                'contact_user_calls' => '',
                'contact_user_meetings' => '',
                'contact_user_contacts_sync' => '',
                'contact_user_reports_to_id' => '',
                'contact_user_reports_to_link' => '',
                'contact_user_reportees' => '',
                'contact_user_email1' => '',
                'contact_user_email_addresses' => '',
                'contact_user_email_addresses_primary' => '',
                'contact_user_email_link_type' => '',
                'contact_user_aclroles' => '',
                'contact_user_is_group' => '',
                'contact_user_accept_status_id' => '',
                'contact_user_accept_status_name' => '',
                'contact_user_prospect_lists' => '',
                'contact_user_emails_users' => '',
                'contact_user_holidays' => '',
                'contact_user_eapm' => '',
                'contact_user_oauth_tokens' => '',
                'contact_user_project_resource' => '',
                'contact_user_project_users_1' => '',
                'contact_user_SecurityGroups' => '',
                'contact_user_securitygroup_noninherit_id' => '',
                'contact_user_securitygroup_noninheritable' => '',
                'contact_user_securitygroup_primary_group' => '',
            );

        $actual = $emailTemplate->_parseUserValues($repl_arr, $user);
        $this->assertSame($expected, $actual);
        */
        self::markTestIncomplete("Different values for php5 and php7");
    }

    public function testparse_template_bean(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $contact = BeanFactory::newBean('Contacts');
        $user = new User(1);
        $account = BeanFactory::newBean('Accounts');

        $contact->name = 'test';
        $account->name = 'test';

        // test with empty string
        $actual = $emailTemplate->parse_template_bean('', 'Contacts', $contact);
        self::assertEquals('', $actual);

        // test with valid string
        $actual = $emailTemplate->parse_template_bean('test', 'Users', $user);
        self::assertEquals('test', $actual);

        // test with empty string and different module
        $actual = $emailTemplate->parse_template_bean('', 'Accounts', $account);
        self::assertEquals('', $actual);
    }

    public function testparse_template(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $bean_arr = array('Users' => 1, 'Leads' => 1);

        // test with empty string
        $result = $emailTemplate->parse_template('', $bean_arr);
        self::assertEquals('', $result);

        // test with valid string
        $result = $emailTemplate->parse_template('some value', $bean_arr);
        self::assertEquals('some value', $result);
    }

    public function testbean_implements(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // test with blank value
        self::assertEquals(false, $emailTemplate->bean_implements(''));
        // test with invalid value
        self::assertEquals(false, $emailTemplate->bean_implements('test'));
        // test with valid value
        self::assertEquals(true, $emailTemplate->bean_implements('ACL'));
    }

    public function testgetTypeOptionsForSearch(): void
    {
        // execute the method and verify that it returns expected results
        $expected = [
            '' => '',
            'campaign' => 'Campaign',
            'email' => 'Email',
            'event' => 'Event'
        ];
        $actual = EmailTemplate::getTypeOptionsForSearch();
        self::assertSame($expected, $actual);
    }

    public function testis_used_by_email_marketing(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // test without id attribute
        self::assertEquals(false, $emailTemplate->is_used_by_email_marketing());

        // test with id attribute
        $emailTemplate->id = 1;
        self::assertEquals(false, $emailTemplate->is_used_by_email_marketing());
    }

    public function testcleanBean(): void
    {
        $emailTemplate = BeanFactory::newBean('EmailTemplates');

        // test without body_html attribute
        $emailTemplate->cleanBean();
        self::assertEquals('', $emailTemplate->body_html);

        // test with body_html attribute
        $emailTemplate->body_html = '<h1>text</h1>';
        $emailTemplate->cleanBean();
        self::assertEquals('&lt;h1&gt;text&lt;/h1&gt;', $emailTemplate->body_html);
    }
}
