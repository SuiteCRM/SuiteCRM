<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class OpportunityTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testOpportunity(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $opportunity = BeanFactory::newBean('Opportunities');

        self::assertInstanceOf('Opportunity', $opportunity);
        self::assertInstanceOf('SugarBean', $opportunity);

        self::assertEquals('opportunities', $opportunity->table_name);
        self::assertEquals('accounts_opportunities', $opportunity->rel_account_table);
        self::assertEquals('opportunities_contacts', $opportunity->rel_contact_table);
        self::assertEquals('Opportunities', $opportunity->module_dir);
        self::assertEquals('Opportunity', $opportunity->object_name);

        self::assertEquals(true, $opportunity->new_schema);
        self::assertEquals(true, $opportunity->importable);
    }

    public function testget_summary_text(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        //test without setting name
        self::assertEquals(null, $opportunity->get_summary_text());

        //test with name set
        $opportunity->name = 'test';
        self::assertEquals('test', $opportunity->get_summary_text());
    }

    public function testcreate_list_query(): void
    {
        self::markTestIncomplete('Breaks on php 7.1');
        $opportunity = BeanFactory::newBean('Opportunities');

        //test with empty string params
        $expected = "SELECT \n                            accounts.id as account_id,\n                            accounts.name as account_name,\n                            accounts.assigned_user_id account_id_owner,\n                            users.user_name as assigned_user_name ,opportunities_cstm.* ,opportunities.*\n                            FROM opportunities LEFT JOIN users\n                            ON opportunities.assigned_user_id=users.id LEFT JOIN accounts_opportunities\n                            ON opportunities.id=accounts_opportunities.opportunity_id\n                            LEFT JOIN accounts\n                            ON accounts_opportunities.account_id=accounts.id  LEFT JOIN opportunities_cstm ON opportunities.id = opportunities_cstm.id_c where \n			(accounts_opportunities.deleted is null OR accounts_opportunities.deleted=0)\n			AND (accounts.deleted is null OR accounts.deleted=0)\n			AND opportunities.deleted=0 ORDER BY opportunities.name";
        $actual = $opportunity->create_list_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT \n                            accounts.id as account_id,\n                            accounts.name as account_name,\n                            accounts.assigned_user_id account_id_owner,\n                            users.user_name as assigned_user_name ,opportunities_cstm.* ,opportunities.*\n                            FROM opportunities LEFT JOIN users\n                            ON opportunities.assigned_user_id=users.id LEFT JOIN accounts_opportunities\n                            ON opportunities.id=accounts_opportunities.opportunity_id\n                            LEFT JOIN accounts\n                            ON accounts_opportunities.account_id=accounts.id  LEFT JOIN opportunities_cstm ON opportunities.id = opportunities_cstm.id_c where (accounts.name=\"\") AND \n			(accounts_opportunities.deleted is null OR accounts_opportunities.deleted=0)\n			AND (accounts.deleted is null OR accounts.deleted=0)\n			AND opportunities.deleted=0 ORDER BY accounts.id";
        $actual = $opportunity->create_list_query('accounts.id', 'accounts.name=""');
        self::assertSame($expected, $actual);
    }

    public function testcreate_export_query(): void
    {
        self::markTestIncomplete('Breaks on php 7.1');
        $opportunity = BeanFactory::newBean('Opportunities');

        //test with empty string params
        $expected = "SELECT \n                            accounts.id as account_id,\n                            accounts.name as account_name,\n                            accounts.assigned_user_id account_id_owner,\n                            users.user_name as assigned_user_name ,opportunities_cstm.* ,opportunities.*\n                            FROM opportunities LEFT JOIN users\n                            ON opportunities.assigned_user_id=users.id LEFT JOIN accounts_opportunities\n                            ON opportunities.id=accounts_opportunities.opportunity_id\n                            LEFT JOIN accounts\n                            ON accounts_opportunities.account_id=accounts.id  LEFT JOIN opportunities_cstm ON opportunities.id = opportunities_cstm.id_c where \n			(accounts_opportunities.deleted is null OR accounts_opportunities.deleted=0)\n			AND (accounts.deleted is null OR accounts.deleted=0)\n			AND opportunities.deleted=0 ORDER BY opportunities.name";
        $actual = $opportunity->create_list_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT \n                            accounts.id as account_id,\n                            accounts.name as account_name,\n                            accounts.assigned_user_id account_id_owner,\n                            users.user_name as assigned_user_name ,opportunities_cstm.* ,opportunities.*\n                            FROM opportunities LEFT JOIN users\n                            ON opportunities.assigned_user_id=users.id LEFT JOIN accounts_opportunities\n                            ON opportunities.id=accounts_opportunities.opportunity_id\n                            LEFT JOIN accounts\n                            ON accounts_opportunities.account_id=accounts.id  LEFT JOIN opportunities_cstm ON opportunities.id = opportunities_cstm.id_c where (accounts.name=\"\") AND \n			(accounts_opportunities.deleted is null OR accounts_opportunities.deleted=0)\n			AND (accounts.deleted is null OR accounts.deleted=0)\n			AND opportunities.deleted=0 ORDER BY accounts.id";
        $actual = $opportunity->create_list_query('accounts.id', 'accounts.name=""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            //test without force_load_details
            $opportunity->fill_in_additional_list_fields();

            //test without force_load_details
            $opportunity->force_load_details = true;
            $opportunity->fill_in_additional_list_fields();

            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $opportunity->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_contacts(): void
    {
        $result = BeanFactory::newBean('Opportunities')->get_contacts();
        self::assertIsArray($result);
    }

    public function testupdate_currency_id(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $opportunity->update_currency_id(array('GBP', 'EUR'), 'USD');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        $opportunity->name = 'test';

        $expected = array(
                'NAME' => 'test',
                'DELETED' => 0,
                'SALES_STAGE' => '',
                'ENCODED_NAME' => 'test',
                );

        $actual = $opportunity->get_list_view_data();
        //$this->assertSame($expected, $actual);
        self::assertEquals($expected['NAME'], $actual['NAME']);
        self::assertEquals($expected['DELETED'], $actual['DELETED']);
        self::assertEquals($expected['SALES_STAGE'], $actual['SALES_STAGE']);
        self::assertEquals($expected['ENCODED_NAME'], $actual['ENCODED_NAME']);
    }

    public function testget_currency_symbol(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        //te4st without currency id
        self::assertEquals('', $opportunity->get_currency_symbol());

        //test with invalid currency id
        $opportunity->currency_id = 1;
        self::assertEquals('', $opportunity->get_currency_symbol());
    }

    public function testbuild_generic_where_clause(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        //test with empty string params
        $expected = "opportunities.name like '%' or accounts.name like '%'";
        $actual = $opportunity->build_generic_where_clause('');
        self::assertSame($expected, $actual);
    }

    public function testsave(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        $opportunity->name = 'test';
        $opportunity->description = 'test description';
        $opportunity->sales_stage = 'Value Proposition';

        $result = $opportunity->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($opportunity->id));
        self::assertEquals(36, strlen((string) $opportunity->id));
        self::assertEquals(-99, $opportunity->currency_id);
        self::assertEquals(30, $opportunity->probability);

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $opportunity->mark_deleted($opportunity->id);
        $result = $opportunity->retrieve($opportunity->id);
        self::assertEquals(null, $result);
    }

    public function testsave_relationship_changes(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');
        $opportunity->account_id = 1;

        try {
            $opportunity->save_relationship_changes(true);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testset_opportunity_contact_relationship(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        try {
            $opportunity->set_opportunity_contact_relationship('1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testset_notification_body(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        //test with attributes preset and verify template variables are set accordingly

        $opportunity->name = 'test';
        $opportunity->amount = '100';
        $opportunity->date_closed = '2015-02-11 17:30:00';
        $opportunity->description = 'tes description';
        $opportunity->sales_stage = 'Value Proposition';

        $result = $opportunity->set_notification_body(new Sugar_Smarty(), $opportunity);

        self::assertEquals($opportunity->name, $result->tpl_vars['OPPORTUNITY_NAME']->value);
        self::assertEquals($opportunity->amount, $result->tpl_vars['OPPORTUNITY_AMOUNT']->value);
        self::assertEquals($opportunity->date_closed, $result->tpl_vars['OPPORTUNITY_CLOSEDATE']->value);
        self::assertEquals($opportunity->sales_stage, $result->tpl_vars['OPPORTUNITY_STAGE']->value);
        self::assertEquals($opportunity->description, $result->tpl_vars['OPPORTUNITY_DESCRIPTION']->value);
    }

    public function testbean_implements(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        self::assertEquals(false, $opportunity->bean_implements('')); //test with blank value
        self::assertEquals(false, $opportunity->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $opportunity->bean_implements('ACL')); //test with valid value
    }

    public function testlistviewACLHelper(): void
    {
        $opportunity = BeanFactory::newBean('Opportunities');

        $expected = array('MAIN' => 'a', 'ACCOUNT' => 'a');
        $actual = $opportunity->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testget_account_detail(): void
    {
        $result = BeanFactory::newBean('Opportunities')->get_account_detail('1');
        self::assertIsArray($result);
    }

    public function testgetCurrencyType(): void
    {
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            getCurrencyType();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('This method has no implementation');
    }
}
