<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_ProductsTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_Products(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosProducts = BeanFactory::newBean('AOS_Products');
        self::assertInstanceOf('AOS_Products', $aosProducts);
        self::assertInstanceOf('Basic', $aosProducts);
        self::assertInstanceOf('SugarBean', $aosProducts);

        self::assertEquals('AOS_Products', $aosProducts->module_dir);
        self::assertEquals('AOS_Products', $aosProducts->object_name);
        self::assertEquals('aos_products', $aosProducts->table_name);
        self::assertEquals(true, $aosProducts->new_schema);
        self::assertEquals(true, $aosProducts->disable_row_level_security);
        self::assertEquals(true, $aosProducts->importable);
    }

    public function testsave(): void
    {
        $aosProducts = BeanFactory::newBean('AOS_Products');

        $aosProducts->name = 'test';
        $aosProducts->category = 1;
        $aosProducts->product_image = 'test img';
        $_POST['deleteAttachment'] = '1';

        $aosProducts->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosProducts->id));
        self::assertEquals(36, strlen((string) $aosProducts->id));
        self::assertEquals('', $aosProducts->product_image);

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosProducts->mark_deleted($aosProducts->id);
        $result = $aosProducts->retrieve($aosProducts->id);
        self::assertEquals(null, $result);
    }

    public function testgetCustomersPurchasedProductsQuery(): void
    {
        self::markTestIncomplete('environment dependency');

        $aosProducts = BeanFactory::newBean('AOS_Products');
        $aosProducts->id = 1;

        //execute the method and verify that it returns expected results
        $expected = "SELECT * FROM (
                SELECT
                    aos_quotes.*,
                    accounts.id AS account_id,
                    accounts.name AS billing_account,

                    opportunity_id AS opportunity,
                    billing_contact_id AS billing_contact,
                    '' AS created_by_name,
                    '' AS modified_by_name,
                    '' AS assigned_user_name
                FROM
                    aos_products

                JOIN aos_products_quotes ON aos_products_quotes.product_id = aos_products.id AND aos_products.id = '1' AND aos_products_quotes.deleted = 0 AND aos_products.deleted = 0
                JOIN aos_quotes ON aos_quotes.id = aos_products_quotes.parent_id AND aos_quotes.stage = 'Closed Accepted' AND aos_quotes.deleted = 0
                JOIN accounts ON accounts.id = aos_quotes.billing_account_id -- AND accounts.deleted = 0

                GROUP BY accounts.id
            ) AS aos_quotes";
        $actual = $aosProducts->getCustomersPurchasedProductsQuery();
        self::assertSame(trim($expected), trim($actual));
    }
}
