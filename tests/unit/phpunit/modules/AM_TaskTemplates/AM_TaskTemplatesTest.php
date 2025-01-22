<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AM_TaskTemplatesTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAM_TaskTemplates(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $am_taskTemplates = BeanFactory::newBean('AM_TaskTemplates');
        self::assertInstanceOf('AM_TaskTemplates', $am_taskTemplates);
        self::assertInstanceOf('Basic', $am_taskTemplates);
        self::assertInstanceOf('SugarBean', $am_taskTemplates);

        self::assertEquals('AM_TaskTemplates', $am_taskTemplates->module_dir);
        self::assertEquals('AM_TaskTemplates', $am_taskTemplates->object_name);
        self::assertEquals('am_tasktemplates', $am_taskTemplates->table_name);
        self::assertEquals(true, $am_taskTemplates->new_schema);
        self::assertEquals(true, $am_taskTemplates->disable_row_level_security);
        self::assertEquals(false, $am_taskTemplates->importable);
    }
}
