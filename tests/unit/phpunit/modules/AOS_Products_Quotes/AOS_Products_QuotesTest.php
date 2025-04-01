<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_Products_QuotesTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testsave(): void
    {
        $aosProductsQuotes = BeanFactory::newBean('AOS_Products_Quotes');

        $aosProductsQuotes->name = 'test';
        $aosProductsQuotes->product_id = 1;
        $aosProductsQuotes->product_unit_price = 100;

        $aosProductsQuotes->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosProductsQuotes->id));
        self::assertEquals(36, strlen((string) $aosProductsQuotes->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosProductsQuotes->mark_deleted($aosProductsQuotes->id);
        $result = $aosProductsQuotes->retrieve($aosProductsQuotes->id);
        self::assertEquals(null, $result);
    }

    public function testsave_lines(): void
    {
        DBManagerFactory::getInstance()->query('DELETE FROM aos_products_quotes');

        $aosProductsQuotes = BeanFactory::newBean('AOS_Products_Quotes');

        //populate required values
        $post_data = array();
        $post_data['name'] = array('test1', 'test2');
        $post_data['group_number'] = array('1', '2');
        $post_data['product_id'] = array('1', '1');
        $post_data['product_unit_price'] = array(100, 200);

        //create parent bean
        $aosQuote = BeanFactory::newBean('AOS_Quotes');
        $aosQuote->id = 1;

        $aosProductsQuotes->save_lines($post_data, $aosQuote);

        //get the linked beans and verify if records created
        $product_quote_lines = $aosQuote->get_linked_beans('aos_products_quotes', $aosQuote->object_name);
        self::assertCount(count($post_data['name']), $product_quote_lines);
    }

    public function testAOS_Products_Quotes(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosProductsQuotes = BeanFactory::newBean('AOS_Products_Quotes');
        self::assertInstanceOf('AOS_Products_Quotes', $aosProductsQuotes);
        self::assertInstanceOf('Basic', $aosProductsQuotes);
        self::assertInstanceOf('SugarBean', $aosProductsQuotes);

        self::assertEquals('AOS_Products_Quotes', $aosProductsQuotes->module_dir);
        self::assertEquals('AOS_Products_Quotes', $aosProductsQuotes->object_name);
        self::assertEquals('aos_products_quotes', $aosProductsQuotes->table_name);
        self::assertEquals(true, $aosProductsQuotes->new_schema);
        self::assertEquals(true, $aosProductsQuotes->disable_row_level_security);
        self::assertEquals(true, $aosProductsQuotes->importable);
    }

    public function testmark_lines_deleted(): void
    {
        $aosProductsQuotes = BeanFactory::newBean('AOS_Products_Quotes');

        //create parent bean
        $aosQuote = BeanFactory::newBean('AOS_Quotes');
        $aosQuote->id = 1;

        //get the linked beans and get record count before deletion
        $product_quote_lines = $aosQuote->get_linked_beans('aos_products_quotes', $aosQuote->object_name);
        $expected = is_countable($product_quote_lines) ? count($product_quote_lines) : 0;
        $product_quote_lines = null;

        $aosProductsQuotes->mark_lines_deleted($aosQuote);
        unset($aosQuote);

        //get the linked beans and get record count after deletion
        $aosQuote = BeanFactory::newBean('AOS_Quotes');
        $aosQuote->id = 1;
        $product_quote_lines = $aosQuote->get_linked_beans('aos_products_quotes', $aosQuote->object_name);
        $actual = is_countable($product_quote_lines) ? count($product_quote_lines) : 0;

        self::assertLessThanOrEqual($expected, $actual);
    }
}
