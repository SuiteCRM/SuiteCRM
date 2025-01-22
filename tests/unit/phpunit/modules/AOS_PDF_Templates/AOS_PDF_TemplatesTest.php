<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_PDF_TemplatesTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_PDF_Templates(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosPdfTemplates = BeanFactory::newBean('AOS_PDF_Templates');
        self::assertInstanceOf('AOS_PDF_Templates', $aosPdfTemplates);
        self::assertInstanceOf('Basic', $aosPdfTemplates);
        self::assertInstanceOf('SugarBean', $aosPdfTemplates);

        self::assertEquals('AOS_PDF_Templates', $aosPdfTemplates->module_dir);
        self::assertEquals('AOS_PDF_Templates', $aosPdfTemplates->object_name);
        self::assertEquals('aos_pdf_templates', $aosPdfTemplates->table_name);
        self::assertEquals(true, $aosPdfTemplates->new_schema);
        self::assertEquals(true, $aosPdfTemplates->disable_row_level_security);
        self::assertEquals(true, $aosPdfTemplates->importable);
    }
}
