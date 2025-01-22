<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOR_ConditionTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOR_Condition(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aor_Condition = BeanFactory::newBean('AOR_Conditions');
        self::assertInstanceOf('AOR_Condition', $aor_Condition);
        self::assertInstanceOf('Basic', $aor_Condition);
        self::assertInstanceOf('SugarBean', $aor_Condition);

        self::assertEquals('AOR_Conditions', $aor_Condition->module_dir);
        self::assertEquals('AOR_Condition', $aor_Condition->object_name);
        self::assertEquals('aor_conditions', $aor_Condition->table_name);
        self::assertEquals(true, $aor_Condition->new_schema);
        self::assertEquals(true, $aor_Condition->disable_row_level_security);
        self::assertEquals(true, $aor_Condition->importable);
        self::assertEquals(false, $aor_Condition->tracker_visibility);
    }

    public function testsave_lines(): void
    {
        $aor_Condition = BeanFactory::newBean('AOR_Conditions');

        //preset the required data
        $post_data = array();
        $post_data['field'][] = 'test field';
        $post_data['name'][] = 'test';
        $post_data['parameter'][] = '1';
        $post_data['module_path'][] = 'test path';
        $post_data['operator'][] = 'test';
        $post_data['value_type'][] = 'test type';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aor_Condition->save_lines($post_data, BeanFactory::newBean('AOR_Reports'));
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }
}
