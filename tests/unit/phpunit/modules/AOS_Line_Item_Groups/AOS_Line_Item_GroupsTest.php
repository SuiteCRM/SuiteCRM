<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_Line_Item_GroupsTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_Line_Item_Groups(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aosLineItemGroup = BeanFactory::newBean('AOS_Line_Item_Groups');
        self::assertInstanceOf('AOS_Line_Item_Groups', $aosLineItemGroup);
        self::assertInstanceOf('Basic', $aosLineItemGroup);
        self::assertInstanceOf('SugarBean', $aosLineItemGroup);

        self::assertEquals('AOS_Line_Item_Groups', $aosLineItemGroup->module_dir);
        self::assertEquals('AOS_Line_Item_Groups', $aosLineItemGroup->object_name);
        self::assertEquals('aos_line_item_groups', $aosLineItemGroup->table_name);
        self::assertEquals(true, $aosLineItemGroup->new_schema);
        self::assertEquals(true, $aosLineItemGroup->disable_row_level_security);
        self::assertEquals(true, $aosLineItemGroup->importable);
        self::assertEquals(false, $aosLineItemGroup->tracker_visibility);
    }

    public function testsave_groups(): void
    {
        $aosLineItemGroup = BeanFactory::newBean('AOS_Line_Item_Groups');

        //populate required values
        $post_data = array();
        $post_data['group_number'] = array(0, 0);
        $post_data['name'] = array('test1', 'test2');
        $post_data['total_amount'] = array('100', '200');
        $post_data['total_amount_usdollar'] = array('100', '200');

        //create parent bean for line item groups
        $aosContract = BeanFactory::newBean('AOS_Contracts');
        $aosContract->id = 1;

        $aosLineItemGroup->save_groups($post_data, $aosContract);

        //get the linked beans and verify if records created
        $line_item_groups = $aosContract->get_linked_beans('aos_line_item_groups', $aosContract->object_name);

        //cleanup afterwards
        foreach ($line_item_groups as $lineItem) {
            $lineItem->mark_deleted($lineItem->id);
        }
    }

    public function testsave(): void
    {
        $aosLineItemGroup = BeanFactory::newBean('AOS_Line_Item_Groups');
        $aosLineItemGroup->name = 'test';
        $aosLineItemGroup->total_amount = 100;
        $aosLineItemGroup->total_amount_usdollar = 100;

        $aosLineItemGroup->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosLineItemGroup->id));
        self::assertEquals(36, strlen((string) $aosLineItemGroup->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosLineItemGroup->mark_deleted($aosLineItemGroup->id);
        $result = $aosLineItemGroup->retrieve($aosLineItemGroup->id);
        self::assertEquals(null, $result);
    }
}
