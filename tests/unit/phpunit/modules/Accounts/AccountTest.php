<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AccountTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testgetProductsServicesPurchasedQuery(): void
    {
        $Account = BeanFactory::newBean('Accounts');

        //without account id
        $expected = "
			SELECT
				aos_products_quotes.*
			FROM
				aos_products_quotes
			JOIN aos_quotes ON aos_quotes.id = aos_products_quotes.parent_id AND aos_quotes.stage LIKE 'Closed Accepted' AND aos_quotes.deleted = 0 AND aos_products_quotes.deleted = 0
			JOIN accounts ON accounts.id = aos_quotes.billing_account_id AND accounts.id = ''

			";
        $actual = $Account->getProductsServicesPurchasedQuery();
        self::assertSame($expected, $actual);

        //with account id
        $expected = "
			SELECT
				aos_products_quotes.*
			FROM
				aos_products_quotes
			JOIN aos_quotes ON aos_quotes.id = aos_products_quotes.parent_id AND aos_quotes.stage LIKE 'Closed Accepted' AND aos_quotes.deleted = 0 AND aos_products_quotes.deleted = 0
			JOIN accounts ON accounts.id = aos_quotes.billing_account_id AND accounts.id = '1234'

			";
        $Account->id = '1234';
        $actual = $Account->getProductsServicesPurchasedQuery();
        self::assertSame($expected, $actual);
    }

    public function testAccount(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $Account = BeanFactory::newBean('Accounts');
        self::assertInstanceOf('Account', $Account);
        self::assertInstanceOf('Company', $Account);
        self::assertInstanceOf('SugarBean', $Account);
        self::assertIsArray($Account->field_name_map);
        self::assertIsArray($Account->field_defs);
    }

    public function testget_summary_text(): void
    {
        //test without name setting attribute
        $Account = BeanFactory::newBean('Accounts');
        $name = $Account->get_summary_text();
        self::assertEquals(null, $name);

        //test with  name attribute set
        $Account->name = 'test account';
        $name = $Account->get_summary_text();
        self::assertEquals('test account', $name);
    }

    public function testget_contacts(): void
    {
        //execute the method and verify that it returns an array
        $contacts = (new Account(''))->get_contacts();
        self::assertIsArray($contacts);
    }

    public function testremove_redundant_http(): void
    {
        $Account = BeanFactory::newBean('Accounts');

        //this method has no implementation. so test for exceptions only.
        try {
            $Account->remove_redundant_http();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_list_fields(): void
    {
        $Account = new Account('');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $Account->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $Account = new Account('');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $Account->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        self::markTestIncomplete('Breaks on php 7.1');
        $this->
        $expected = array(
            'DELETED' => 0,
            'JJWG_MAPS_LNG_C' => '0.00000000',
            'JJWG_MAPS_LAT_C' => '0.00000000',
            'EMAIL1' => '',
            'EMAIL1_LINK' => '            <a class="email-link" href="mailto:"
                    onclick="$(document).openComposeViewModal(this);"
                    data-module="Accounts" data-record-id=""
                    data-module-name=" " data-email-address=""
                ></a>',
            'ENCODED_NAME' => null,
            'CITY' => null,
            'BILLING_ADDRESS_STREET' => null,
            'SHIPPING_ADDRESS_STREET' => null,
        );

        //execute the method and verify that it retunrs expected results
        $actual = BeanFactory::newBean('Accounts')->get_list_view_data();

        foreach ($expected as $key => $value) {
            self::assertSame($expected[$key], $actual[$key]);
        }
    }

    public function testbuild_generic_where_clause(): void
    {
        $Account = BeanFactory::newBean('Accounts');

        //execute the method with a string as parameter and verify that it retunrs expected results
        $expected = "accounts.name like 'value%'";
        $actual = $Account->build_generic_where_clause('value');
        self::assertSame($expected, $actual);

        //execute the method with number as parameter and verify that it retunrs expected results
        $expected = "accounts.name like '1234%' or accounts.phone_alternate like '%1234%' or accounts.phone_fax like '%1234%' or accounts.phone_office like '%1234%'";
        $actual = $Account->build_generic_where_clause('1234');
        self::assertSame($expected, $actual);
    }

    public function testset_notification_body(): void
    {
        //execute the method and test if populates provided sugar_smarty
        $result = BeanFactory::newBean('Accounts')->set_notification_body(new Sugar_Smarty(), BeanFactory::newBean('Accounts'));
        self::assertInstanceOf('Sugar_Smarty', $result);
        self::assertNotEquals(new Sugar_Smarty(), $result);
    }

    public function testbean_implements(): void
    {
        $Account = BeanFactory::newBean('Accounts');

        self::assertTrue($Account->bean_implements('ACL')); //test with valid value
        self::assertFalse($Account->bean_implements('')); //test with empty value
        self::assertFalse($Account->bean_implements('Basic'));//test with invalid value
    }

    public function testget_unlinked_email_query(): void
    {
        $Account = BeanFactory::newBean('Accounts');

        //without setting type parameter
        $expected = "SELECT emails.id FROM emails  JOIN (select DISTINCT email_id from emails_email_addr_rel eear

	join email_addr_bean_rel eabr on eabr.bean_id ='' and eabr.bean_module = 'Accounts' and
	eabr.email_address_id = eear.email_address_id and eabr.deleted=0
	where eear.deleted=0 and eear.email_id not in
	(select eb.email_id from emails_beans eb where eb.bean_module ='Accounts' and eb.bean_id = '')
	) derivedemails on derivedemails.email_id = emails.id";
        $actual = $Account->get_unlinked_email_query();
        self::assertSame($expected, $actual);

        //with type parameter set
        $expected = array(
            'select' => 'SELECT emails.id ',
            'from' => 'FROM emails ',
            'where' => '',
            'join' => " JOIN (select DISTINCT email_id from emails_email_addr_rel eear

	join email_addr_bean_rel eabr on eabr.bean_id ='' and eabr.bean_module = 'Accounts' and
	eabr.email_address_id = eear.email_address_id and eabr.deleted=0
	where eear.deleted=0 and eear.email_id not in
	(select eb.email_id from emails_beans eb where eb.bean_module ='Accounts' and eb.bean_id = '')
	) derivedemails on derivedemails.email_id = emails.id",
            'join_tables' => array(''),
        );

        $actual = $Account->get_unlinked_email_query(array('return_as_array' => 'true'));
        self::assertSame($expected, $actual);
    }
}
