<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOR_Scheduled_ReportsTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testSaveAndGet_email_recipients(): void
    {
        $aorScheduledReports = BeanFactory::newBean('AOR_Scheduled_Reports');
        $aorScheduledReports->name = "test";
        $aorScheduledReports->description = "test description";
        $_POST['email_recipients']= array('email_target_type'=> array('Email Address','all','Specify User')  ,'email' =>array('test@test.com','','1') );


        //test save and test for record ID to verify that record is saved
        $aorScheduledReports->save();
        self::assertTrue(isset($aorScheduledReports->id));
        self::assertEquals(36, strlen((string) $aorScheduledReports->id));



        //test get_email_recipients
        $expected = array('test@test.com','','1');
        $aorScheduledReports->retrieve($aorScheduledReports->id);
        $emails = $aorScheduledReports->get_email_recipients();

        self::assertIsArray($emails);
        self::assertEquals('test@test.com', $emails[0]);


        $aorScheduledReports->mark_deleted($aorScheduledReports->id);
        unset($aorScheduledReports);
    }

    public function testAOR_Scheduled_Reports(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aorScheduledReports = BeanFactory::newBean('AOR_Scheduled_Reports');
        self::assertInstanceOf('AOR_Scheduled_Reports', $aorScheduledReports);
        self::assertInstanceOf('Basic', $aorScheduledReports);
        self::assertInstanceOf('SugarBean', $aorScheduledReports);

        self::assertEquals('AOR_Scheduled_Reports', $aorScheduledReports->module_dir);
        self::assertEquals('AOR_Scheduled_Reports', $aorScheduledReports->object_name);
        self::assertEquals('aor_scheduled_reports', $aorScheduledReports->table_name);
        self::assertEquals(true, $aorScheduledReports->new_schema);
        self::assertEquals(true, $aorScheduledReports->disable_row_level_security);
        self::assertEquals(false, $aorScheduledReports->importable);
    }

    public function test_ReportRelation(): void
    {
        $_POST['aor_fields_field'] = [];
        $report = BeanFactory::newBean('AOR_Reports');
        $report->name = "Foobar";
        $report->save();

        $aorScheduledReports = BeanFactory::newBean('AOR_Scheduled_Reports');
        $aorScheduledReports->save();
        $aorScheduledReports->load_relationships();
        $aorScheduledReports->aor_report->add($report);
        $aorScheduledReports->retrieve($aorScheduledReports->id);
        self::assertEquals($report->name, $aorScheduledReports->aor_report_name);
        self::assertEquals($report->id, $aorScheduledReports->aor_report_id);
    }

    public function testbean_implements(): void
    {
        $aorScheduledReports = BeanFactory::newBean('AOR_Scheduled_Reports');
        self::assertEquals(false, $aorScheduledReports->bean_implements('')); //test with blank value
        self::assertEquals(false, $aorScheduledReports->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $aorScheduledReports->bean_implements('ACL')); //test with valid value
    }

    public function testshouldRun(): void
    {
        $aorScheduledReports = BeanFactory::newBean('AOR_Scheduled_Reports');
        $aorScheduledReports->schedule = " 8 * * * *";

        //test without a last_run date
        //@todo: NEEDS FIXING - are we sure?
        //$this->assertFalse($aorScheduledReports->shouldRun(new DateTime()) );

        //test without a older last_run date
        $aorScheduledReports->last_run = date("d-m-y H:i:s", mktime(0, 0, 0, 10, 3, 2014));
        self::assertTrue($aorScheduledReports->shouldRun(new DateTime()));


        //test without a current last_run date
        $aorScheduledReports->last_run = new DateTime();
        self::assertFalse($aorScheduledReports->shouldRun(new DateTime()));
    }
}
