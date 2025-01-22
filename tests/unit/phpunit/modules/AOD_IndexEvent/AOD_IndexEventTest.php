<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOD_IndexEventTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOD_IndexEvent(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $aod_indexEvent = BeanFactory::newBean('AOD_IndexEvent');
        self::assertInstanceOf('AOD_IndexEvent', $aod_indexEvent);
        self::assertInstanceOf('Basic', $aod_indexEvent);
        self::assertInstanceOf('SugarBean', $aod_indexEvent);

        self::assertEquals('AOD_IndexEvent', $aod_indexEvent->module_dir);
        self::assertEquals('AOD_IndexEvent', $aod_indexEvent->object_name);
        self::assertEquals('aod_indexevent', $aod_indexEvent->table_name);
        self::assertEquals(true, $aod_indexEvent->new_schema);
        self::assertEquals(true, $aod_indexEvent->disable_row_level_security);
        self::assertEquals(false, $aod_indexEvent->importable);
        self::assertEquals(false, $aod_indexEvent->tracker_visibility);
    }
}
