<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class FP_Event_LocationsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testFP_Event_Locations(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $fpEventLoc = BeanFactory::newBean('FP_Event_Locations');
        self::assertInstanceOf('FP_Event_Locations', $fpEventLoc);
        self::assertInstanceOf('Basic', $fpEventLoc);
        self::assertInstanceOf('SugarBean', $fpEventLoc);

        self::assertEquals('FP_Event_Locations', $fpEventLoc->module_dir);
        self::assertEquals('FP_Event_Locations', $fpEventLoc->object_name);
        self::assertEquals('fp_event_locations', $fpEventLoc->table_name);
        self::assertEquals(true, $fpEventLoc->new_schema);
        self::assertEquals(false, $fpEventLoc->importable);
        self::assertEquals(true, $fpEventLoc->disable_row_level_security);
    }
}
