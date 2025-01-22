<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AM_ProjectTemplatesTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAM_ProjectTemplates(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $am_projectTemplate = BeanFactory::newBean('AM_ProjectTemplates');
        self::assertInstanceOf('AM_ProjectTemplates', $am_projectTemplate);
        self::assertInstanceOf('Basic', $am_projectTemplate);
        self::assertInstanceOf('SugarBean', $am_projectTemplate);

        self::assertEquals('AM_ProjectTemplates', $am_projectTemplate->module_dir);
        self::assertEquals('AM_ProjectTemplates', $am_projectTemplate->object_name);
        self::assertEquals('am_projecttemplates', $am_projectTemplate->table_name);
        self::assertEquals(true, $am_projectTemplate->new_schema);
        self::assertEquals(true, $am_projectTemplate->disable_row_level_security);
        self::assertEquals(true, $am_projectTemplate->importable);
    }
}
