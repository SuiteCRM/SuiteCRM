<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class CampaignTrackerTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testCampaignTracker(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $campaignTracker = BeanFactory::newBean('CampaignTrackers');
        self::assertInstanceOf('CampaignTracker', $campaignTracker);
        self::assertInstanceOf('SugarBean', $campaignTracker);

        self::assertEquals('CampaignTrackers', $campaignTracker->module_dir);
        self::assertEquals('CampaignTracker', $campaignTracker->object_name);
        self::assertEquals('campaign_trkrs', $campaignTracker->table_name);
        self::assertEquals(true, $campaignTracker->new_schema);
    }

    public function testsave(): void
    {
        $campaignTracker = BeanFactory::newBean('CampaignTrackers');

        $campaignTracker->tracker_name = 'test';
        $campaignTracker->is_optout = 1;

        $campaignTracker->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($campaignTracker->id));
        self::assertEquals(36, strlen((string) $campaignTracker->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $campaignTracker->mark_deleted($campaignTracker->id);
        $result = $campaignTracker->retrieve($campaignTracker->id);
        self::assertEquals(null, $result);
    }

    public function testget_summary_text(): void
    {
        $campaignTracker = BeanFactory::newBean('CampaignTrackers');

        //test without setting name
        self::assertEquals(null, $campaignTracker->get_summary_text());

        //test with name set
        $campaignTracker->tracker_name = 'test';
        self::assertEquals('test', $campaignTracker->get_summary_text());
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $campaignTracker = BeanFactory::newBean('CampaignTrackers');

        //test without is_optout set
        $campaignTracker->fill_in_additional_detail_fields();
        self::assertStringEndsWith('/index.php?entryPoint=campaign_trackerv2&track=', $campaignTracker->message_url);

        //test with is_optout set
        $campaignTracker->is_optout = 1;
        $campaignTracker->fill_in_additional_detail_fields();
        self::assertStringEndsWith('/index.php?entryPoint=removeme&identifier={MESSAGE_ID}', $campaignTracker->message_url);
    }
}
