<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_QuotesTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_Quotes(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosQuotes = BeanFactory::newBean('AOS_Quotes');
        self::assertInstanceOf('AOS_Quotes', $aosQuotes);
        self::assertInstanceOf('Basic', $aosQuotes);
        self::assertInstanceOf('SugarBean', $aosQuotes);

        self::assertEquals('AOS_Quotes', $aosQuotes->module_dir);
        self::assertEquals('AOS_Quotes', $aosQuotes->object_name);
        self::assertEquals('aos_quotes', $aosQuotes->table_name);
        self::assertEquals(true, $aosQuotes->new_schema);
        self::assertEquals(true, $aosQuotes->disable_row_level_security);
        self::assertEquals(true, $aosQuotes->importable);
        self::assertEquals(true, $aosQuotes->lineItems);
    }

    public function testSaveAndMark_deleted(): void
    {
        $aosQuotes = BeanFactory::newBean('AOS_Quotes');

        $aosQuotes->name = 'test';
        $aosQuotes->total_amt = 100;
        $aosQuotes->total_amt_usdollar = 100;

        $aosQuotes->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosQuotes->id));
        self::assertEquals(36, strlen((string) $aosQuotes->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosQuotes->mark_deleted($aosQuotes->id);
        $result = $aosQuotes->retrieve($aosQuotes->id);
        self::assertEquals(null, $result);
    }
}
