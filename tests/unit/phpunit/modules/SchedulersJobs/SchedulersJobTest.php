<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class SchedulersJobTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testSchedulersJob(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        self::assertInstanceOf('SchedulersJob', $schedulersJob);
        self::assertInstanceOf('Basic', $schedulersJob);
        self::assertInstanceOf('SugarBean', $schedulersJob);

        self::assertEquals('job_queue', $schedulersJob->table_name);
        self::assertEquals('SchedulersJobs', $schedulersJob->module_dir);
        self::assertEquals('SchedulersJob', $schedulersJob->object_name);

        self::assertEquals(true, $schedulersJob->new_schema);
        self::assertEquals(true, $schedulersJob->process_save_dates);
        self::assertEquals(30, $schedulersJob->min_interval);
    }

    public function testcheck_date_relationships_load(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');
        $schedulersJob->execute_time = '2015-01-01 00:00:00';

        $schedulersJob->check_date_relationships_load();

        self::assertEquals('2015-01-01 00:00:00', $schedulersJob->execute_time_db);
    }

    public function testhandleDateFormat(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        //test with default params
        $result = $schedulersJob->handleDateFormat();
        self::assertGreaterThan(0, strlen((string) $result));

        //test with a valid date param
        $result = $schedulersJob->handleDateFormat('2015-01-01');
        self::assertEquals('2015-01-01 00:00:00', $result);
    }

    public function testfireUrl(): void
    {
        self::markTestIncomplete('environment dependency: curl_setopt(): CURLOPT_DNS_USE_GLOBAL_CACHE cannot be activated when thread safety is enabled ');

        //test with invalid param
        $result = BeanFactory::newBean('SchedulersJobs')->fireUrl('');
        self::assertEquals(false, $result);

        //test with valid param
        self::markTestIncomplete();
        //$result = $schedulersJob->fireUrl('https://suitecrm.com/');
        //$this->assertEquals(true, $result);
    }

    public function testget_list_view_data(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        $schedulersJob->job_name = 'test';
        $schedulersJob->job = 'function::test';

        $expected = array(
                'DELETED' => '0',
                'REQUEUE' => '0',
                'JOB_DELAY' => 0,
                'JOB_NAME' => 'test',
                'JOB' => 'function::test',
        );
        $actual = $schedulersJob->get_list_view_data();

        self::assertSame($expected, $actual);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $schedulersJob->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfailJob(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        $result = $schedulersJob->failJob();
        self::assertEquals(true, $result);

        //test for record ID to verify that record is saved
        self::assertTrue(isset($schedulersJob->id));
        self::assertEquals(36, strlen((string) $schedulersJob->id));

        self::assertEquals(SchedulersJob::JOB_STATUS_DONE, $schedulersJob->status);
        self::assertEquals(SchedulersJob::JOB_FAILURE, $schedulersJob->resolution);

        $schedulersJob->mark_deleted($schedulersJob->id);
    }

    public function testsucceedJob(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        $result = $schedulersJob->succeedJob();
        self::assertEquals(true, $result);

        //test for record ID to verify that record is saved
        self::assertTrue(isset($schedulersJob->id));
        self::assertEquals(36, strlen((string) $schedulersJob->id));

        self::assertEquals(SchedulersJob::JOB_STATUS_DONE, $schedulersJob->status);
        self::assertEquals(SchedulersJob::JOB_SUCCESS, $schedulersJob->resolution);

        $schedulersJob->mark_deleted($schedulersJob->id);
    }

    public function testonFailureRetry(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $schedulersJob->onFailureRetry();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('method has no implementation: logic hooks not defined');
    }

    public function testOnFinalFailure(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $schedulersJob->onFinalFailure();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('method has no implementation: logic hooks not defined');
    }

    public function testresolveJob(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        //test for JOB_FAILURE
        $result = $schedulersJob->resolveJob(SchedulersJob::JOB_FAILURE, 'test');
        self::assertEquals(true, $result);

        //test for record ID to verify that record is saved
        self::assertTrue(isset($schedulersJob->id));
        self::assertEquals(36, strlen((string) $schedulersJob->id));

        self::assertEquals(SchedulersJob::JOB_STATUS_DONE, $schedulersJob->status);
        self::assertEquals(SchedulersJob::JOB_FAILURE, $schedulersJob->resolution);

        //tst for JOB_SUCCESS
        $result = $schedulersJob->resolveJob(SchedulersJob::JOB_SUCCESS, 'test');
        self::assertEquals(true, $result);

        self::assertEquals(SchedulersJob::JOB_STATUS_DONE, $schedulersJob->status);
        self::assertEquals(SchedulersJob::JOB_SUCCESS, $schedulersJob->resolution);

        $schedulersJob->mark_deleted($schedulersJob->id);
    }

    public function testpostponeJobAndMark_deleted(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        $result = $schedulersJob->postponeJob('test message', 1);
        self::assertEquals(true, $result);

        //test for record ID to verify that record is saved
        self::assertTrue(isset($schedulersJob->id));
        self::assertEquals(36, strlen((string) $schedulersJob->id));

        //verify the related attributes
        self::assertEquals(SchedulersJob::JOB_STATUS_QUEUED, $schedulersJob->status);
        self::assertEquals(SchedulersJob::JOB_PARTIAL, $schedulersJob->resolution);

        //test mark deleted method and verify record doesn't exist after deletion
        $schedulersJob->mark_deleted($schedulersJob->id);
        $result = $schedulersJob->retrieve($schedulersJob->id);
        self::assertEquals(null, $result);
    }

    public function testunexpectedExit(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        //create conditions to mark job_done as false
        $schedulersJob->client = 'test';
        $schedulersJob->status = SchedulersJob::JOB_STATUS_RUNNING;
        $schedulersJob->save();
        $result = SchedulersJob::runJobId($schedulersJob->id, 'test');

        //execute the method
        $schedulersJob->unexpectedExit();
        $schedulersJob->retrieve($schedulersJob->id);
        self::assertEquals(SchedulersJob::JOB_STATUS_DONE, $schedulersJob->status);
        self::assertEquals(SchedulersJob::JOB_FAILURE, $schedulersJob->resolution);

        $schedulersJob->mark_deleted($schedulersJob->id);
    }

    public function testrunJobId(): void
    {
        //test with invalid job id
        $result = SchedulersJob::runJobId('1', '');
        self::assertEquals('Job 1 not found.', $result);

        //test with valid job id
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');
        $schedulersJob->status = SchedulersJob::JOB_STATUS_DONE;
        $schedulersJob->save();

        $result = SchedulersJob::runJobId($schedulersJob->id, '');
        self::assertEquals('Job '.$schedulersJob->id.' is not marked as running.', $result);

        //test with valid job id and status but mismatch client
        $schedulersJob->client = 'client';
        $schedulersJob->status = SchedulersJob::JOB_STATUS_RUNNING;
        $schedulersJob->save();

        $result = SchedulersJob::runJobId($schedulersJob->id, 'test_client');

        self::assertEquals('Job '.$schedulersJob->id.' belongs to another client, can not run as test_client.', $result);

        $schedulersJob->mark_deleted($schedulersJob->id);
    }

    public function testerrorHandler(): void
    {
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');

        //execute the method with different Error Types
        $schedulersJob->errors = '';
        $schedulersJob->errorHandler(E_USER_WARNING, 'test err', 'testfile', '1');
        self::assertEquals("Warning [512]: test err in testfile on line 1\n", $schedulersJob->errors);

        $schedulersJob->errors = '';
        $schedulersJob->errorHandler(E_ERROR, 'test err', 'testfile', '1');
        self::assertEquals("Fatal Error [1]: test err in testfile on line 1\n", $schedulersJob->errors);

        $schedulersJob->errors = '';
        $schedulersJob->errorHandler(E_PARSE, 'test err', 'testfile', '1');
        self::assertEquals("Parse Error [4]: test err in testfile on line 1\n", $schedulersJob->errors);

        $schedulersJob->errors = '';
        $schedulersJob->errorHandler(E_RECOVERABLE_ERROR, 'test err', 'testfile', '1');
        self::assertEquals("Recoverable Error [4096]: test err in testfile on line 1\n", $schedulersJob->errors);
    }

    public function testrunJob(): void
    {
        //test without a valid user
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');
        $schedulersJob->target = 'function::processAOW_Workflow';
        $result = $schedulersJob->runJob();
        self::assertEquals(false, $result);
        $schedulersJob->mark_deleted($schedulersJob->id);

        //test with valid user
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');
        $schedulersJob->assigned_user_id = 1;

        $schedulersJob->target = 'function::processAOW_Workflow';
        $result = $schedulersJob->runJob();
        self::assertEquals(true, $result);
        $schedulersJob->mark_deleted($schedulersJob->id);

        //test with valid user
        $schedulersJob = BeanFactory::newBean('SchedulersJobs');
        $schedulersJob->assigned_user_id = 1;

        self::markTestIncomplete();
//        $schedulersJob->target = 'url::https://suitecrm.com/';
//        $result = $schedulersJob->runJob();
//        $this->assertEquals(true, $result);
//        $schedulersJob->mark_deleted($schedulersJob->id);
    }
}
