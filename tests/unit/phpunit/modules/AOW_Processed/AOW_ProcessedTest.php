<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOW_ProcessedTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOW_Processed(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aowProcessed = BeanFactory::newBean('AOW_Processed');
        self::assertInstanceOf('AOW_Processed', $aowProcessed);
        self::assertInstanceOf('Basic', $aowProcessed);
        self::assertInstanceOf('SugarBean', $aowProcessed);

        self::assertEquals('AOW_Processed', $aowProcessed->module_dir);
        self::assertEquals('AOW_Processed', $aowProcessed->object_name);
        self::assertEquals('aow_processed', $aowProcessed->table_name);
        self::assertEquals(true, $aowProcessed->new_schema);
        self::assertEquals(true, $aowProcessed->disable_row_level_security);
        self::assertEquals(false, $aowProcessed->importable);
    }

    public function testbean_implements(): void
    {
        $aowProcessed = BeanFactory::newBean('AOW_Processed');
        self::assertEquals(false, $aowProcessed->bean_implements('')); //test with blank value
        self::assertEquals(false, $aowProcessed->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $aowProcessed->bean_implements('ACL')); //test with valid value
    }
}
