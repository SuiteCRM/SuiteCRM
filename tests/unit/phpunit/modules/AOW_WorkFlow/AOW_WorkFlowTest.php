<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOW_WorkFlowTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOW_WorkFlow(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');
        self::assertInstanceOf('AOW_WorkFlow', $aowWorkFlow);
        self::assertInstanceOf('Basic', $aowWorkFlow);
        self::assertInstanceOf('SugarBean', $aowWorkFlow);

        self::assertEquals('AOW_WorkFlow', $aowWorkFlow->module_dir);
        self::assertEquals('AOW_WorkFlow', $aowWorkFlow->object_name);
        self::assertEquals('aow_workflow', $aowWorkFlow->table_name);
        self::assertEquals(true, $aowWorkFlow->new_schema);
        self::assertEquals(true, $aowWorkFlow->disable_row_level_security);
        self::assertEquals(false, $aowWorkFlow->importable);
    }

    public function testbean_implements(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');
        self::assertEquals(false, $aowWorkFlow->bean_implements('')); //test with blank value
        self::assertEquals(false, $aowWorkFlow->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $aowWorkFlow->bean_implements('ACL')); //test with valid value
    }

    public function testmark_delete_related(): void
    {
        // Create a workflow and a related condition
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');
        $aowWorkFlow->name = 'test';
        $aowWorkFlow->flow_module = 'test';
        $aowWorkFlow->save();

        $condition = BeanFactory::newBean('AOW_Conditions');
        $condition->aow_workflow_id = $aowWorkFlow->id;
        $condition->save();

        $linked = $aowWorkFlow->get_linked_beans('aow_conditions');
        self::assertCount(1, $linked);
        $conditionID = $linked[0]->id;

        // Deleting the workflow should delete also the condition
        BeanFactory::unregisterBean('AOW_Conditions', $conditionID);
        $cond = BeanFactory::getBean('AOW_Conditions', $conditionID);
        self::assertNotEmpty($cond);
        $aowWorkFlow->mark_deleted($aowWorkFlow->id);
        BeanFactory::unregisterBean('AOW_Conditions', $conditionID);
        $cond = BeanFactory::getBean('AOW_Conditions', $conditionID);
        self::assertEmpty($cond);
    }

    public function testsave(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        $aowWorkFlow->name = 'test';
        $aowWorkFlow->flow_module = 'test';

        $aowWorkFlow->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aowWorkFlow->id));
        self::assertEquals(36, strlen((string) $aowWorkFlow->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aowWorkFlow->mark_deleted($aowWorkFlow->id);
        $result = $aowWorkFlow->retrieve($aowWorkFlow->id);
        self::assertEquals(null, $result);
    }

    public function testload_flow_beans(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aowWorkFlow->load_flow_beans();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testrun_flows(): void
    {
        $result = BeanFactory::newBean('AOW_WorkFlow')->run_flows();
        self::assertTrue($result);
    }

    public function testrun_flow(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aowWorkFlow->run_flow();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testrun_bean_flows(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //test with different modules. it always returns true

        $result = $aowWorkFlow->run_bean_flows(BeanFactory::newBean('AOS_Quotes'));
        self::assertTrue($result);

        $result = $aowWorkFlow->run_bean_flows(BeanFactory::newBean('Calls'));
        self::assertTrue($result);
    }

    public function testget_flow_beans(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //test for AOS_Quotes. it will return null as no test data is available
        $aowWorkFlow->flow_module = 'AOS_Quotes';
        $result = $aowWorkFlow->get_flow_beans();
        self::assertGreaterThanOrEqual(0, count((array)$result));
    }

    public function testbuild_flow_query_join(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');
        $query = array();

        //test with type custom
        $expected = array('join' => array('c' => 'LEFT JOIN calls_cstm c ON calls.id = c.id_c '));
        $result = $aowWorkFlow->build_flow_custom_query_join('calls', 'c', BeanFactory::newBean('Calls'), array());
        self::assertSame($expected, $result);

        //test with type relationship
        $expected = array(
            'join' => array('aos_products_quotes' => "LEFT JOIN aos_products_quotes `aos_products_quotes` ON aos_quotes.id=`aos_products_quotes`.parent_id AND `aos_products_quotes`.deleted=0\n\n"),
            'select' => array("`aos_products_quotes`.id AS 'aos_products_quotes_id'"),
        );
        $result = $aowWorkFlow->build_flow_relationship_query_join('aos_products_quotes', BeanFactory::newBean('AOS_Quotes'), array());
        self::assertSame($expected, $result);
    }

    public function testbuild_flow_query_where(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //test without presetting required object attributes
        $expected = array();
        $query = $aowWorkFlow->build_flow_query_where();
        self::assertSame($expected, $query);

        //test with module required attributes set
        $aowWorkFlow->id = '1';
        $aowWorkFlow->flow_module = 'Calls';
        $expected = array(
                'where' => array('NOT EXISTS (SELECT * FROM aow_processed WHERE aow_processed.aow_workflow_id=\'1\' AND aow_processed.parent_id=calls.id AND aow_processed.status = \'Complete\' AND aow_processed.deleted = 0)',
                'calls.deleted = 0 ', ),
                );
        $query = $aowWorkFlow->build_flow_query_where();
        self::assertSame($expected, $query);

        //test with flow_run_on and multiple_runs attributes set
        $expected = array(
                        'where' => array('calls.date_entered > \'\'', 'calls.deleted = 0 '),
                    );
        $aowWorkFlow->flow_run_on = 'New_Records';
        $aowWorkFlow->multiple_runs = 1;
        $query = $aowWorkFlow->build_flow_query_where();
        self::assertSame($expected, $query);
    }

    public function testbuild_query_where(): void
    {
        self::markTestIncomplete('[PHPUnit_Framework_Exception] unserialize(): Error at offset 0 of 5 bytes');
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //populate required values
        $call = BeanFactory::newBean('Calls');
        $aowCondition = BeanFactory::newBean('AOW_Conditions');
        $aowCondition->name = 'test';
        $aowCondition->module_path = base64_encode(serialize(array('')));
        $aowCondition->field = 'name';
        $aowCondition->value = 'testval';

        //test with contains operator
        $aowCondition->operator = 'Contains';
        $aowCondition->value_type = 'Value';
        $expected = array(
                'where' => array(".name LIKE CONCAT('%', 'testval' ,'%')"),
        );
        $query = $aowWorkFlow->build_query_where($aowCondition, $call);
        self::assertEquals($expected, $query);

        //test for starts with operator
        $aowCondition->operator = 'Starts_With';
        $aowCondition->value_type = 'Value';

        $expected = array(
            'where' => array(".name LIKE CONCAT('testval' ,'%')"),
        );
        $query = $aowWorkFlow->build_query_where($aowCondition, $call);
        self::assertEquals($expected, $query);

        //test for Equal_To operator
        $aowCondition->operator = 'Equal_To';
        $aowCondition->value_type = 'Value';

        $expected = array(
                'where' => array(".name = 'testval'"),
        );
        $query = $aowWorkFlow->build_query_where($aowCondition, $call);
        self::assertEquals($expected, $query);

        //test with value type Date
        $aowCondition->operator = 'Equal_To';
        $aowCondition->value_type = 'Date';

        $expected = array(
                'where' => array('.name = DATE_ADD(calls., INTERVAL   )'),
        );

        $query = $aowWorkFlow->build_query_where($aowCondition, $call);

        self::assertEquals($expected, $query);

        //test with value type Field
        $aowCondition->operator = 'Equal_To';
        $aowCondition->value_type = 'Field';

        $expected = array(
                'where' => array('.name = calls.testval'),
        );

        $query = $aowWorkFlow->build_query_where($aowCondition, $call);
        self::assertEquals($expected, $query);
    }

    public function testcheck_valid_bean(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');
        $aowWorkFlow->flow_run_on = 'New_Records';

        $aosQuotes = BeanFactory::newBean('AOS_Quotes');

        $result = $aowWorkFlow->check_valid_bean($aosQuotes);
        self::assertTrue($result);
    }

    public function testcompare_condition(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //execute the method and verify that it returns valid values for all operators

        self::assertTrue($aowWorkFlow->compare_condition(1, 1));
        self::assertTrue($aowWorkFlow->compare_condition(1, 2, 'Not_Equal_To'));
        self::assertTrue($aowWorkFlow->compare_condition(2, 1, 'Greater_Than'));
        self::assertTrue($aowWorkFlow->compare_condition(1, 2, 'Less_Than'));
        self::assertTrue($aowWorkFlow->compare_condition(5, 4, 'Greater_Than_or_Equal_To'));
        self::assertTrue($aowWorkFlow->compare_condition(2, 3, 'Less_Than_or_Equal_To'));
        self::assertTrue($aowWorkFlow->compare_condition('', '', 'is_null'));
        self::assertTrue($aowWorkFlow->compare_condition('test2', array('test1', 'test2'), 'One_of'));
        self::assertTrue($aowWorkFlow->compare_condition('test', array('test1', 'test2'), 'Not_One_of'));


        //$this->assertNotFalse($aowWorkFlow->compare_condition('test1', 'test', 'Contains'));
        self::assertEquals(true, $aowWorkFlow->compare_condition('test1', 'test', 'Contains'));

        //$this->assertNotFalse($aowWorkFlow->compare_condition('test1', 'test', 'Starts_With'));
        self::assertEquals(true, $aowWorkFlow->compare_condition('test1', 'test', 'Starts_With'));

        //$this->assertNotFalse($aowWorkFlow->compare_condition('test1', '1', 'Ends_With'));
        self::assertEquals(true, $aowWorkFlow->compare_condition('test1', '1', 'Ends_With'));
    }

    public function testcheck_in_group(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //test with two different modules
        self::assertFalse($aowWorkFlow->check_in_group(1, 'Users', 1));
        self::assertFalse($aowWorkFlow->check_in_group(1, 'Calls', 1));
    }

    public function testrun_actions(): void
    {
        $aowWorkFlow = BeanFactory::newBean('AOW_WorkFlow');

        //prepare the required objects and variables
        $aowWorkFlow->id = 1;

        $call = BeanFactory::newBean('Calls');
        $call->id = 1;

        //execute the method and verify if it creates records in processed table
        $result = $aowWorkFlow->run_actions($call);

        //test for a entry in AOW_Processed table.
        $processed = BeanFactory::newBean('AOW_Processed');
        $processed->retrieve_by_string_fields(array('aow_workflow_id' => 1, 'parent_id' => 1));

        //test for record ID to verify that record is saved
        self::assertTrue(isset($processed->id));
        self::assertEquals(36, strlen((string) $processed->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $processed->mark_deleted($processed->id);
        $result = $processed->retrieve($processed->id);
        self::assertEquals(null, $result);
    }
}
