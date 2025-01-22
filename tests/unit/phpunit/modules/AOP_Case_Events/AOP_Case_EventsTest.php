<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOP_Case_EventsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOP_Case_Events(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $aopCaseEvents = BeanFactory::newBean('AOP_Case_Events');
        self::assertInstanceOf('AOP_Case_Events', $aopCaseEvents);
        self::assertInstanceOf('Basic', $aopCaseEvents);
        self::assertInstanceOf('SugarBean', $aopCaseEvents);

        self::assertEquals('AOP_Case_Events', $aopCaseEvents->module_dir);
        self::assertEquals('AOP_Case_Events', $aopCaseEvents->object_name);
        self::assertEquals('aop_case_events', $aopCaseEvents->table_name);
        self::assertEquals(true, $aopCaseEvents->new_schema);
        self::assertEquals(true, $aopCaseEvents->disable_row_level_security);
        self::assertEquals(false, $aopCaseEvents->importable);
        self::assertEquals(false, $aopCaseEvents->tracker_visibility);
    }
}
