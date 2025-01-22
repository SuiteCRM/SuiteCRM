<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOW_ConditionTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOW_Condition(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aowCondition = BeanFactory::newBean('AOW_Conditions');
        self::assertInstanceOf('AOW_Condition', $aowCondition);
        self::assertInstanceOf('Basic', $aowCondition);
        self::assertInstanceOf('SugarBean', $aowCondition);

        self::assertEquals('AOW_Conditions', $aowCondition->module_dir);
        self::assertEquals('AOW_Condition', $aowCondition->object_name);
        self::assertEquals('aow_conditions', $aowCondition->table_name);
        self::assertEquals(true, $aowCondition->new_schema);
        self::assertEquals(true, $aowCondition->disable_row_level_security);
        self::assertEquals(false, $aowCondition->importable);
        self::assertEquals(false, $aowCondition->tracker_visibility);
    }

    public function testbean_implements(): void
    {
        $aowCondition = BeanFactory::newBean('AOW_Conditions');
        self::assertEquals(false, $aowCondition->bean_implements('')); //test with blank value
        self::assertEquals(false, $aowCondition->bean_implements('test')); //test with invalid value
        self::assertEquals(false, $aowCondition->bean_implements('ACL')); //test with valid value
    }

    public function testsave_lines(): void
    {
        $aowCondition = BeanFactory::newBean('AOW_Conditions');

        //populate required values
        $post_data = array();
        $post_data['name'] = array('test1', 'test2');
        $post_data['field'] = array('field1', 'field2');
        $post_data['operator'] = array('=', '!=');
        $post_data['value_type'] = array('int', 'string');
        $post_data['value'] = array('1', 'abc');

        //create parent bean
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');
        $aowWorkFlow->id = 1;

        $aowCondition->save_lines($post_data, $aowWorkFlow);

        //get the linked beans and verify if records created
        $aow_conditions = $aowWorkFlow->get_linked_beans('aow_conditions', $aowWorkFlow->object_name);
        self::assertCount(count($post_data['field']), $aow_conditions);

        //cleanup afterwards
        foreach ($aow_conditions as $lineItem) {
            $lineItem->mark_deleted($lineItem->id);
        }
    }
}
