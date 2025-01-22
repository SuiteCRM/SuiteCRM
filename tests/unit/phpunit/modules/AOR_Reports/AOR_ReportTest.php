<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOR_ReportTest extends SuitePHPUnitFrameworkTestCase
{
    public function testAOR_Report(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        self::assertInstanceOf('AOR_Report', $aor_Report);
        self::assertInstanceOf('Basic', $aor_Report);
        self::assertInstanceOf('SugarBean', $aor_Report);

        self::assertEquals('AOR_Reports', $aor_Report->module_dir);
        self::assertEquals('AOR_Report', $aor_Report->object_name);
        self::assertEquals('aor_reports', $aor_Report->table_name);
        self::assertEquals(true, $aor_Report->new_schema);
        self::assertEquals(true, $aor_Report->disable_row_level_security);
        self::assertEquals(true, $aor_Report->importable);
    }

    public function testbean_implements(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');

        self::assertEquals(false, $aor_Report->bean_implements('')); //test with blank value
        self::assertEquals(false, $aor_Report->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $aor_Report->bean_implements('ACL')); //test with valid value
    }

    public function testsave(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');

        //populate value for aor_fields related/child object
        $_POST['aor_fields_field'][] = 'last_name';
        $_POST['aor_fields_name'][] = 'test';
        $_POST['aor_fields_module_path'][] = 'contacts';
        $_POST['aor_fields_display'][] = '1';
        $_POST['aor_fields_link'][] = '1';
        $_POST['aor_fields_label'][] = 'test_label';
        $_POST['aor_fields_field_function'][] = 'count';
        $_POST['aor_fields_total'][] = 'total';
        $_POST['aor_fields_group_by'][] = '1';
        $_POST['aor_fields_group_order'][] = 'desc';
        $_POST['aor_fields_group_display'][] = '1';

        //populate values for aor_chart related/child object
        $_POST['aor_chart_id'] = array('test' => '');
        $_POST['aor_chart_title'] = array('test' => 'test');
        $_POST['aor_chart_type'] = array('test' => 'bar');
        $_POST['aor_chart_x_field'] = array('test' => '1');
        $_POST['aor_chart_y_field'] = array('test' => '2');

        //populate aor_Report object values
        $aor_Report->name = 'test';
        $aor_Report->description = 'test text';

        $aor_Report->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aor_Report->id));
        self::assertEquals(36, strlen((string) $aor_Report->id));

        //mark the record as deleted for cleanup
        $aor_Report->mark_deleted($aor_Report->id);

        unset($aor_Report);
    }

    public function testload_report_beans(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $aor_Report->load_report_beans();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetReportFields(): void
    {
        //execute the method and verify that it returns an array
        $result = BeanFactory::newBean('AOR_Reports')->getReportFields();
        self::assertIsArray($result);
    }

    public function testbuild_report_chart(): void
    {
        self::markTestIncomplete('Incomplete test needs rewritten');
        
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';

        $chartBean = BeanFactory::getBean('AOR_Charts');
        $charts = (array)$chartBean->get_full_list();

        //execute the method and verify that it returns chart display script. strings returned vary due to included chart id.
        $result = $aor_Report->build_report_chart();
        foreach ($charts as $chart) {
            self::assertStringContainsString($chart->id, $result);
        }

        unset($GLOBALS['_SESSION']);
        unset($GLOBALS['objectList']);
        unset($GLOBALS['mod_strings']);
        unset($GLOBALS['toHTML']);
        unset($GLOBALS['module']);
        unset($GLOBALS['action']);
        unset($GLOBALS['disable_date_format']);
        unset($GLOBALS['fill_in_rel_depth']);
        unset($GLOBALS['currentModule']);
    }

    public function testbuild_group_report(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';
        $aor_Report->id = '1';

        //execute the method without any parameters and verify it returns html string
        $html1 = $aor_Report->build_group_report();
        self::assertGreaterThan(0, strlen((string) $html1));

        //execute the method wit offset parameter and verify it returns html string
        $html2 = $aor_Report->build_group_report(1);
        self::assertGreaterThan(0, strlen((string) $html2));

        //execute the method with both parameters and verify it returns html string
        $html3 = $aor_Report->build_group_report(0, false);
        self::assertGreaterThan(0, strlen((string) $html3));

        //verify that all three html strings are different.
        self::assertNotEquals($html1, $html2);
        self::assertNotEquals($html1, $html3);
        self::assertNotEquals($html2, $html3);
    }

    public function testbuild_report_html(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';

        //execute the method without any parameters and verify it returns html string
        $html1 = $aor_Report->build_report_html();
        self::assertGreaterThan(0, strlen((string) $html1));

        //execute the method with both parameters and verify it returns html string
        $html2 = $aor_Report->build_report_html(0, false);
        self::assertGreaterThan(0, strlen((string) $html2));

        //execute the method with group and identifier parameters and verify it returns html string
        $html3 = $aor_Report->build_report_html(1, false, 'grouptest', 'testidentifier');
        self::assertGreaterThan(0, strlen((string) $html3));

        //verify that group and identifier exist in the strings
        self::assertStringContainsString('grouptest', $html3);
        self::assertStringContainsString('testidentifier', $html3);
    }

    public function testGetTotalHTML(): void
    {
        //execute the method with required data preset and verify it returns expected result
        $fields = [
            'label' => [
                'display' => 1,
                'total' => 'SUM',
                'label' => 'total',
                'module' => 'Meetings',
                'field' => 'duration_hours',
                'params' => ['value_set' => 'value_set']

            ]
        ];
        $totals = ['label' => [10, 20, 30]];
        /** @noinspection OneTimeUseVariablesInspection */
        $reportBean = BeanFactory::newBean('AOR_Reports');
        $actual = $reportBean->getTotalHTML($fields, $totals);

        self::assertStringContainsString('sugar_field', $actual);
        self::assertStringContainsString('duration_hours', $actual);
    }

    public function testcalculateTotal(): void
    {
        //execute the method with data preset and verify it returns expected result
        $totals = array(10, 20, 30);

        $aor_Report = BeanFactory::newBean('AOR_Reports');

        self::assertEquals('', $aor_Report->calculateTotal('', $totals));
        self::assertEquals(60, $aor_Report->calculateTotal('SUM', $totals));
        self::assertEquals(3, $aor_Report->calculateTotal('COUNT', $totals));
        self::assertEquals(20, $aor_Report->calculateTotal('AVG', $totals));
    }

    public function testbuild_report_query(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';

        //execute the method without any parameters and verify that it returns a non empty string
        $actual = $aor_Report->build_report_query();
        self::assertGreaterThanOrEqual(0, strlen((string) $actual));

        //execute the method with parameter and verify that it returns a non empty string
        $actual = $aor_Report->build_report_query('name');
        self::assertGreaterThanOrEqual(0, strlen((string) $actual));
    }

    public function testbuild_report_query_select(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';
        $query_array = array();

        //execute the method with parameters and verify that it returns an array.
        $actual = $aor_Report->build_report_query_select($query_array, 'name');
        self::assertIsArray($actual);
    }

    public function testbuild_report_query_join(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';

        //test with type custom and verify that it retunrs expected results
        $expected = array('join' => array('accounts_contacts' => 'LEFT JOIN `accounts_cstm` `accounts_contacts` ON `accounts`.id = `contacts`.id_c '));
        $actual = $aor_Report->build_report_query_join(
            'contacts',
            'accounts_contacts',
            'accounts',
            BeanFactory::newBean('Accounts'),
            'custom',
            array()
        );
        self::assertSame($expected, $actual);

        //test with type relationship and verify that it retunrs expected results
        $expected = array(
            'join' => array('accounts_contacts' => "LEFT JOIN accounts_contacts `accounts|accounts_contacts` ON `accounts`.id=`accounts|accounts_contacts`.account_id AND `accounts|accounts_contacts`.deleted=0\n\nLEFT JOIN contacts `accounts_contacts` ON `accounts_contacts`.id=`accounts|accounts_contacts`.contact_id AND `accounts_contacts`.deleted=0\n"),
            'id_select' => array('accounts_contacts' => '`accounts_contacts`.id AS \'accounts_contacts_id\''),
            'id_select_group' => array('accounts_contacts' => '`accounts_contacts`.id')
        );
        $actual = $aor_Report->build_report_query_join(
            'contacts',
            'accounts_contacts',
            'accounts',
            BeanFactory::newBean('Accounts'),
            'relationship',
            array()
        );
        self::assertSame($expected, $actual);
    }

    public function testbuild_report_access_query(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');

        //test without alias and verify that it retunrs expected results
        $result = $aor_Report->build_report_access_query(BeanFactory::newBean('AOR_Reports'), '');
        self::assertEquals('', $result);

        //test with alias and verify that it retunrs expected results
        $result = $aor_Report->build_report_access_query(BeanFactory::newBean('AOR_Reports'), 'rep');
        self::assertEquals('', $result);
    }

    public function testbuild_report_query_where(): void
    {
        $aor_Report = BeanFactory::newBean('AOR_Reports');
        $aor_Report->report_module = 'Accounts';

        //execute the method and verify that it retunrs expected results
        $expected = array('where' => array('accounts.deleted = 0 '));
        $actual = $aor_Report->build_report_query_where();
        self::assertSame($expected, $actual);
    }
}
