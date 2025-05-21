<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_Product_CategoriesTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_Product_Categories(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosProductCategories = BeanFactory::newBean('AOS_Product_Categories');
        self::assertInstanceOf('AOS_Product_Categories', $aosProductCategories);
        self::assertInstanceOf('Basic', $aosProductCategories);
        self::assertInstanceOf('SugarBean', $aosProductCategories);

        self::assertEquals('AOS_Product_Categories', $aosProductCategories->module_dir);
        self::assertEquals('AOS_Product_Categories', $aosProductCategories->object_name);
        self::assertEquals('aos_product_categories', $aosProductCategories->table_name);
        self::assertEquals(true, $aosProductCategories->new_schema);
        self::assertEquals(true, $aosProductCategories->disable_row_level_security);
        self::assertEquals(true, $aosProductCategories->importable);
    }

    public function testsave(): void
    {
        $aosProductCategories = BeanFactory::newBean('AOS_Product_Categories');
        $aosProductCategories->name = 'test';
        $aosProductCategories->parent_category_id = 1;

        $aosProductCategories->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosProductCategories->id));
        self::assertEquals(36, strlen((string) $aosProductCategories->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosProductCategories->mark_deleted($aosProductCategories->id);
        $result = $aosProductCategories->retrieve($aosProductCategories->id);
        self::assertEquals(null, $result);
    }
}
