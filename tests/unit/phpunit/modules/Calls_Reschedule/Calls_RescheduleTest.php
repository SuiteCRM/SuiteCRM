<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class Calls_RescheduleTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testCalls_Reschedule(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $callsReschedule = BeanFactory::newBean('Calls_Reschedule');
        self::assertInstanceOf('Calls_Reschedule', $callsReschedule);
        self::assertInstanceOf('Basic', $callsReschedule);
        self::assertInstanceOf('SugarBean', $callsReschedule);

        self::assertEquals('Calls_Reschedule', $callsReschedule->module_dir);
        self::assertEquals('Calls_Reschedule', $callsReschedule->object_name);
        self::assertEquals('calls_reschedule', $callsReschedule->table_name);
        self::assertEquals(true, $callsReschedule->new_schema);
        self::assertEquals(true, $callsReschedule->disable_row_level_security);
        self::assertEquals(true, $callsReschedule->importable);
        self::assertEquals(false, $callsReschedule->tracker_visibility);
    }
}
