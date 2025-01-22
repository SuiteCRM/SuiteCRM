<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class CampaignLogTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testCampaignLog(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $campaignLog = BeanFactory::newBean('CampaignLog');
        self::assertInstanceOf('CampaignLog', $campaignLog);
        self::assertInstanceOf('SugarBean', $campaignLog);

        self::assertEquals('CampaignLog', $campaignLog->module_dir);
        self::assertEquals('CampaignLog', $campaignLog->object_name);
        self::assertEquals('campaign_log', $campaignLog->table_name);
        self::assertEquals(true, $campaignLog->new_schema);
    }

    public function testget_list_view_data(): void
    {
        //execute the method and verify it returns an array
        $actual = BeanFactory::newBean('CampaignLog')->get_list_view_data();
        self::assertIsArray($actual);
        self::assertSame(array(), $actual);
    }

    public function testretrieve_email_address(): void
    {
        $actual = BeanFactory::newBean('CampaignLog')->retrieve_email_address();
        self::assertGreaterThanOrEqual('', $actual);
    }

    public function testGetRelatedName(): void
    {
        $campaignLog = BeanFactory::newBean('CampaignLog');

        // Execute the method and verify that it returns expected results for all type parameters
        self::assertEquals('1Emails', $campaignLog::get_related_name(1, 'Emails'));
        self::assertEquals('1Contacts', $campaignLog::get_related_name(1, 'Contacts'));
        self::assertEquals('1Leads', $campaignLog::get_related_name(1, 'Leads'));
        self::assertEquals('1Prospects', $campaignLog::get_related_name(1, 'Prospects'));
        self::assertEquals('1CampaignTrackers', $campaignLog::get_related_name(1, 'CampaignTrackers'));
        self::assertEquals('1Accounts', $campaignLog::get_related_name(1, 'Accounts'));
    }
}
