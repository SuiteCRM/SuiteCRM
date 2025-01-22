<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOK_Knowledge_Base_CategoriesTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOK_Knowledge_Base_Categories(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $aok_KBCategories = BeanFactory::newBean('AOK_Knowledge_Base_Categories');
        self::assertInstanceOf('AOK_Knowledge_Base_Categories', $aok_KBCategories);
        self::assertInstanceOf('Basic', $aok_KBCategories);
        self::assertInstanceOf('SugarBean', $aok_KBCategories);

        self::assertEquals('AOK_Knowledge_Base_Categories', $aok_KBCategories->module_dir);
        self::assertEquals('AOK_Knowledge_Base_Categories', $aok_KBCategories->object_name);
        self::assertEquals('aok_knowledge_base_categories', $aok_KBCategories->table_name);
        self::assertEquals(true, $aok_KBCategories->new_schema);
        self::assertEquals(true, $aok_KBCategories->disable_row_level_security);
        self::assertEquals(false, $aok_KBCategories->importable);
    }
}
