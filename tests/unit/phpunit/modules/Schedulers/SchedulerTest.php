<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'include/SugarQueue/SugarJobQueue.php';
require_once 'install/install_utils.php';

class SchedulerTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
        $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], 'Schedulers');
    }

    public function test__construct(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $scheduler = BeanFactory::newBean('Schedulers');

        self::assertInstanceOf('Scheduler', $scheduler);
        self::assertInstanceOf('SugarBean', $scheduler);

        self::assertEquals('schedulers', $scheduler->table_name);
        self::assertEquals('Schedulers', $scheduler->module_dir);
        self::assertEquals('Scheduler', $scheduler->object_name);

        self::assertEquals(true, $scheduler->new_schema);
        self::assertEquals(true, $scheduler->process_save_dates);
    }

    public function testinitUser(): void
    {
        $user = Scheduler::initUser();
        self::assertInstanceOf('User', $user);
    }

    public function testfireQualified(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        //test without setting any attributes
        $result = $scheduler->fireQualified();
        self::assertEquals(false, $result);

        //test with required attributes set
        $scheduler->id = 1;
        $scheduler->job_interval = '0::3::*::*::*';
        $scheduler->date_time_start = '2015-01-01 10:30:01';

        $result = $scheduler->fireQualified();
        self::assertEquals(true, $result);
    }

    public function testcreateJob(): void
    {
        $result = BeanFactory::newBean('Schedulers')->createJob();

        self::assertInstanceOf('SchedulersJob', $result);
    }

    public function testcheckPendingJobs(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $scheduler->checkPendingJobs(new SugarJobQueue());
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testderiveDBDateTimes(): void
    {
        self::markTestIncomplete('Need to implement!');

//        $scheduler = BeanFactory::newBean('Schedulers');
//
//        $scheduler->id = 1;
//        $scheduler->date_time_start = '2016-01-01 10:30:01';
//
//        //execute the method with different job intervals
//
//        $scheduler->job_interval = '0::3::3::*::*';
//        $result = $scheduler->deriveDBDateTimes($scheduler);
//        $this->assertEquals(false, (bool)$result);
//
//        // NOTE: add this valid test case:
//        //$scheduler->job_interval = '*::*::*::*::3';
//        //$result = $scheduler->deriveDBDateTimes($scheduler);
//        //$this->assertEquals(false, (bool)$result);
//
//        $scheduler->job_interval = '0::*::3::*::*';
//        $result = $scheduler->deriveDBDateTimes($scheduler);
//        $this->assertEquals(false, (bool)$result);
    }

    public function testhandleIntervalType(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        //execute the method with different job intervals
        self::assertEquals('On thehour', $scheduler->handleIntervalType('0', '0', '2', '2'));
        self::assertEquals('00:02', $scheduler->handleIntervalType('1', '0', '2', '2'));
        self::assertEquals('30th', $scheduler->handleIntervalType('2', '0', '2', '2'));
        self::assertEquals('December', $scheduler->handleIntervalType('3', '0', '2', '2'));
        self::assertEquals('Sunday', $scheduler->handleIntervalType('4', '0', '2', '2'));
    }

    public function testsetIntervalHumanReadable(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        //execute the method with different job intervals
        $scheduler->job_interval = '0::3::3::*::*';
        $scheduler->parseInterval();
        $scheduler->setIntervalHumanReadable();
        self::assertEquals('On thehour; 03:00; 3rd', $scheduler->intervalHumanReadable);

        $scheduler->job_interval = '0::3::3::3::3';
        $scheduler->parseInterval();
        $scheduler->setIntervalHumanReadable();
        self::assertEquals('On thehour; 03:00; 3rd; March; Wednesday', $scheduler->intervalHumanReadable);
    }

    public function testsetStandardArraysAttributes(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        //execute the method and verify related attributes

        $scheduler->setStandardArraysAttributes();

        self::assertEquals(array('*', 1, 2, 3, 4, 5, 6, 0), $scheduler->dayInt);
        self::assertEquals(array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), $scheduler->monthsInt);
        self::assertEquals(array('', 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'), $scheduler->monthsLabel);
        self::assertEquals(array('*', '/', '-', ','), $scheduler->metricsVar);
        self::assertEquals(array(' every ', '', ' thru ', ' and '), $scheduler->metricsVal);
    }

    public function testparseInterval(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        $scheduler->job_interval = '0::3::3::*::*';

        $expected = array(
                      'raw' => array('0', '3', '3', '*', '*'),
                      'hours' => '3:::0',
                      'months' => '*:::3',
                    );

        //execute the method and verify related attributes
        $scheduler->parseInterval();

        self::assertIsArray($scheduler->intervalParsed);
        self::assertSame($expected, $scheduler->intervalParsed);
    }

    public function testcheckCurl(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $scheduler->checkCurl();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdisplayCronInstructions(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        //execute the method and capture the echo output
        ob_start();

        $scheduler->displayCronInstructions();

        $renderedContent = ob_get_contents();
        ob_end_clean();

        self::assertGreaterThanOrEqual(0, strlen($renderedContent));
    }

    public function testrebuildDefaultSchedulers(): void
    {
        self::markTestIncomplete('enviroment dependency');

        $scheduler = BeanFactory::newBean('Schedulers');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $scheduler->rebuildDefaultSchedulers();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcreate_export_query(): void
    {
        self::markTestIncomplete('environment dependency');
        $scheduler = BeanFactory::newBean('Schedulers');

        //test with empty string params
        $expected = " SELECT  schedulers.*  , jt0.user_name created_by_name , jt0.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt1.user_name modified_by_name , jt1.created_by modified_by_name_owner  , 'Users' modified_by_name_mod FROM schedulers   LEFT JOIN  users jt0 ON jt0.id=schedulers.created_by AND jt0.deleted=0\n AND jt0.deleted=0  LEFT JOIN  users jt1 ON schedulers.modified_user_id=jt1.id AND jt1.deleted=0\n\n AND jt1.deleted=0 where schedulers.deleted=0";
        $actual = $scheduler->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = " SELECT  schedulers.*  , jt0.user_name created_by_name , jt0.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt1.user_name modified_by_name , jt1.created_by modified_by_name_owner  , 'Users' modified_by_name_mod FROM schedulers   LEFT JOIN  users jt0 ON jt0.id=schedulers.created_by AND jt0.deleted=0\n AND jt0.deleted=0  LEFT JOIN  users jt1 ON schedulers.modified_user_id=jt1.id AND jt1.deleted=0\n\n AND jt1.deleted=0 where (schedulers.name = \"\") AND schedulers.deleted=0";
        $actual = $scheduler->create_export_query('schedulers.id', 'schedulers.name = ""');
        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_list_fields(): void
    {
        self::markTestIncomplete('environment dependency');
        $scheduler = BeanFactory::newBean('Schedulers');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $scheduler->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $scheduler = BeanFactory::newBean('Schedulers');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $scheduler->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('method has no implementation');
    }

    public function testget_list_view_data(): void
    {
        self::markTestIncomplete('environment dependency');
        $scheduler = BeanFactory::newBean('Schedulers');

        //preset required attributes
        $scheduler->job_interval = '0::3::*::*::*';
        $scheduler->date_time_start = '2015-01-01 10:30:01';
        $scheduler->name = 'test';
        $scheduler->created_by = 1;
        $scheduler->modified_user_id = 1;

        $expected = array(
                'DELETED' => '0',
                'CREATED_BY' => 1,
                'MODIFIED_USER_ID' => 1,
                'NAME' => 'test',
                'DATE_TIME_START' => '2015-01-01 10:30:01',
                'JOB_INTERVAL' => '03:00',
                'CATCH_UP' => '1',
                'ENCODED_NAME' => 'test',
                'DATE_TIME_END' => null,
        );

        $actual = $scheduler->get_list_view_data();
        self::assertSame($expected, $actual);
    }

    public function testget_summary_text(): void
    {
        self::markTestIncomplete('environment dependency');
        $scheduler = BeanFactory::newBean('Schedulers');

        //test without setting name
        self::assertEquals(null, $scheduler->get_summary_text());

        //test with name set
        $scheduler->name = 'test';
        self::assertEquals('test', $scheduler->get_summary_text());
    }

    public function testgetJobsList(): void
    {
        self::markTestIncomplete('environment dependency');

        $result = Scheduler::getJobsList();
        self::assertIsArray($result);
    }
}
