<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOK_KnowledgeBaseTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOK_KnowledgeBase(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $aok_KnowledgeBase = BeanFactory::newBean('AOK_KnowledgeBase');
        self::assertInstanceOf('AOK_KnowledgeBase', $aok_KnowledgeBase);
        self::assertInstanceOf('Basic', $aok_KnowledgeBase);
        self::assertInstanceOf('SugarBean', $aok_KnowledgeBase);

        self::assertEquals('AOK_KnowledgeBase', $aok_KnowledgeBase->module_dir);
        self::assertEquals('AOK_KnowledgeBase', $aok_KnowledgeBase->object_name);
        self::assertEquals('aok_knowledgebase', $aok_KnowledgeBase->table_name);
        self::assertEquals(true, $aok_KnowledgeBase->new_schema);
        self::assertEquals(true, $aok_KnowledgeBase->disable_row_level_security);
        self::assertEquals(false, $aok_KnowledgeBase->importable);
    }
}
