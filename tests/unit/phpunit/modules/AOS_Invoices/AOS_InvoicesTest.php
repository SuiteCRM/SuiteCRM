<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_InvoicesTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_Invoices(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosInvoices = BeanFactory::newBean('AOS_Invoices');
        self::assertInstanceOf('AOS_Invoices', $aosInvoices);
        self::assertInstanceOf('Basic', $aosInvoices);
        self::assertInstanceOf('SugarBean', $aosInvoices);

        self::assertEquals('AOS_Invoices', $aosInvoices->module_dir);
        self::assertEquals('AOS_Invoices', $aosInvoices->object_name);
        self::assertEquals('aos_invoices', $aosInvoices->table_name);
        self::assertEquals(true, $aosInvoices->new_schema);
        self::assertEquals(true, $aosInvoices->disable_row_level_security);
        self::assertEquals(true, $aosInvoices->importable);
    }

    public function testSaveAndMark_deleted(): void
    {
        $aosInvoices = BeanFactory::newBean('AOS_Invoices');
        $aosInvoices->name = 'test';

        $aosInvoices->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosInvoices->id));
        self::assertEquals(36, strlen((string) $aosInvoices->id));
        self::assertGreaterThan(0, $aosInvoices->number);

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosInvoices->mark_deleted($aosInvoices->id);
        $result = $aosInvoices->retrieve($aosInvoices->id);
        self::assertEquals(null, $result);
    }
}
