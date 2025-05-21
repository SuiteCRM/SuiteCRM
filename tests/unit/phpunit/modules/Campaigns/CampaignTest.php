<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'modules/Campaigns/utils.php';

class CampaignTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testSubscribeUnsubscribeFromNewsLetterCampaign(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');
        $campaign->name = create_guid();
        $campaign->campaign_type = "NewsLetter";
        $campaign->save();
        $campaign->load_relationship('prospectlists');

        // Add the lists to the campaign
        $exempt_list = BeanFactory::newBean('ProspectLists');
        $exempt_list->list_type = "exempt";
        $exempt_list->save();
        $campaign->prospectlists->add($exempt_list->id);

        $default_list = BeanFactory::newBean('ProspectLists');
        $default_list->list_type = "default";
        $default_list->save();
        $campaign->prospectlists->add($default_list->id);

        $test_list = BeanFactory::newBean('ProspectLists');
        $test_list->list_type = "test";
        $test_list->save();
        $campaign->prospectlists->add($test_list->id);

        $lead = BeanFactory::newBean('Leads');
        $lead->save();

        // Subscribe
        subscribe($campaign->id, null, $lead, true);
        $keyed = get_subscription_lists_keyed($lead);
        self::assertArrayHasKey($campaign->name, $keyed['subscribed']);
        self::assertEquals($default_list->id, $keyed['subscribed'][$campaign->name]['prospect_list_id']);

        // Unsubscribe
        unsubscribe($campaign->id, $lead);
        $keyed = get_subscription_lists_keyed($lead);
        self::assertArrayNotHasKey($campaign->name, $keyed['subscribed']);
        self::assertArrayHasKey($campaign->name, $keyed['unsubscribed']);
        self::assertEquals($default_list->id, $keyed['unsubscribed'][$campaign->name]['prospect_list_id']);
    }

    public function testCampaign(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $campaign = BeanFactory::newBean('Campaigns');
        self::assertInstanceOf('Campaign', $campaign);
        self::assertInstanceOf('SugarBean', $campaign);

        self::assertEquals('Campaigns', $campaign->module_dir);
        self::assertEquals('Campaign', $campaign->object_name);
        self::assertEquals('campaigns', $campaign->table_name);
        self::assertEquals(true, $campaign->new_schema);
        self::assertEquals(true, $campaign->importable);
    }

    public function testlist_view_parse_additional_sections(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //test with attributes preset and verify template variables are set accordingly
        $tpl = new Sugar_Smarty();
        $campaign->list_view_parse_additional_sections($tpl);
        self::assertEquals('', isset($tpl->tpl_vars['ASSIGNED_USER_NAME']->value) ? $tpl->tpl_vars['ASSIGNED_USER_NAME']->value : null);
    }

    public function testget_summary_text(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //test without setting name
        self::assertEquals(null, $campaign->get_summary_text());

        //test with name set
        $campaign->name = 'test';
        self::assertEquals('test', $campaign->get_summary_text());
    }

    public function testcreate_export_query(): void
    {
        self::markTestIncomplete('#Warning: Strings contain different line endings!');
        $campaign = BeanFactory::newBean('Campaigns');

        //test with empty string params
        $expected = "SELECT\n            campaigns.*,\n            users.user_name as assigned_user_name  FROM campaigns LEFT JOIN users\n                      ON campaigns.assigned_user_id=users.id where  campaigns.deleted=0 ORDER BY campaigns.name";
        $actual = $campaign->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT\n            campaigns.*,\n            users.user_name as assigned_user_name  FROM campaigns LEFT JOIN users\n                      ON campaigns.assigned_user_id=users.id where campaigns.name=\"\" AND  campaigns.deleted=0 ORDER BY campaigns.id";
        $actual = $campaign->create_export_query('campaigns.id', 'campaigns.name=""');
        self::assertSame($expected, $actual);
    }

    public function testclear_campaign_prospect_list_relationship(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $campaign->clear_campaign_prospect_list_relationship('');
            $campaign->clear_campaign_prospect_list_relationship('1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testmark_relationships_deleted(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $campaign->mark_relationships_deleted('');
            $campaign->mark_relationships_deleted('1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_list_fields(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $campaign->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $campaign->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testupdate_currency_id(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $campaign->update_currency_id('', '');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_list_view_data(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //execute the method and verify that it returns expected results
        $expected = array(
                'DELETED' => 0,
                'TRACKER_COUNT' => '0',
                'REFER_URL' => 'http://',
                'IMPRESSIONS' => '0',
                'OPTIONAL_LINK' => 'display:none',
                'TRACK_CAMPAIGN_TITLE' => 'View Status',
            // The theme may fallback to default so we only care that the icon is the same.
                'TRACK_CAMPAIGN_IMAGE' => '~images/view_status~',
                'LAUNCH_WIZARD_TITLE' => 'Launch Wizard',
            // The theme may fallback to default so we only care that the icon is the same.
                'LAUNCH_WIZARD_IMAGE' => '~images/edit_wizard~',
                'TRACK_VIEW_ALT_TEXT' => 'View Status',
                'LAUNCH_WIZ_ALT_TEXT' => 'Launch Wizard',
        );

        $actual = $campaign->get_list_view_data();
        foreach ($expected as $expectedKey => $expectedVal) {
            if ($expectedKey == 'LAUNCH_WIZARD_IMAGE' || $expectedKey == 'TRACK_CAMPAIGN_IMAGE') {
                self::assertMatchesRegularExpression($expected[$expectedKey], $actual[$expectedKey]);
            } else {
                self::assertSame($expected[$expectedKey], $actual[$expectedKey]);
            }
        }
    }

    public function testbuild_generic_where_clause(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //test with blank parameter
        $expected = "campaigns.name like '%'";
        $actual = $campaign->build_generic_where_clause('');
        self::assertSame($expected, $actual);

        //test with valid parameter
        $expected = "campaigns.name like '1%'";
        $actual = $campaign->build_generic_where_clause(1);
        self::assertSame($expected, $actual);
    }

    public function testSaveAndMarkDeleted(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');
        $campaign->name = 'test';
        $campaign->amount = 100;

        $campaign->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($campaign->id));
        self::assertEquals(36, strlen((string) $campaign->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $campaign->mark_deleted($campaign->id);
        $result = $campaign->retrieve($campaign->id);
        self::assertEquals(null, $result);
    }

    public function testset_notification_body(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //test with attributes preset and verify template variables are set accordingly
        $campaign->name = 'test';
        $campaign->budget = '1000';
        $campaign->end_date = '10/01/2015';
        $campaign->status = 'Planned';
        $campaign->content = 'some text';

        $result = $campaign->set_notification_body(new Sugar_Smarty(), $campaign);

        self::assertEquals($campaign->name, $result->tpl_vars['CAMPAIGN_NAME']->value);
        self::assertEquals($campaign->budget, $result->tpl_vars['CAMPAIGN_AMOUNT']->value);
        self::assertEquals($campaign->end_date, $result->tpl_vars['CAMPAIGN_CLOSEDATE']->value);
        self::assertEquals($campaign->status, $result->tpl_vars['CAMPAIGN_STATUS']->value);
        self::assertEquals($campaign->content, $result->tpl_vars['CAMPAIGN_DESCRIPTION']->value);
    }

    public function testtrack_log_leads(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        $expected = "SELECT campaign_log.*  FROM campaign_log WHERE campaign_log.campaign_id = '' AND campaign_log.deleted=0 AND activity_type = 'lead' AND archived = 0 AND target_id IS NOT NULL ";
        $actual = $campaign->track_log_leads();
        self::assertSame($expected, $actual);
    }

    public function testtrack_log_entries(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //test without parameters
        $expected = "SELECT campaign_log.*  FROM campaign_log WHERE campaign_log.campaign_id = '' AND campaign_log.deleted=0 AND activity_type='targeted' AND archived=0 ";
        $actual = $campaign->track_log_entries();
        self::assertSame($expected, $actual);

        //test with parameters
        $expected = "SELECT campaign_log.*  FROM campaign_log WHERE campaign_log.campaign_id = '' AND campaign_log.deleted=0 AND activity_type='test1' AND archived=0 ";
        $actual = $campaign->track_log_entries(array('test1', 'test2'));
        self::assertSame($expected, $actual);
    }

    public function testget_queue_items(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //without parameters
        $expected = " SELECT emailman.*,
       campaigns.NAME       AS campaign_name,
       email_marketing.NAME AS message_name,
       ( CASE related_type
           WHEN 'Contacts' THEN
         Ltrim(Rtrim(
               Concat(Ifnull(contacts.first_name, ''), ' ',
               Ifnull(contacts.last_name, ''))))
           WHEN 'Leads' THEN
       Ltrim(Rtrim(
       Concat(Ifnull(leads.first_name, ''), ' ',
       Ifnull(leads.last_name, ''))))
       WHEN 'Accounts' THEN accounts.NAME
       WHEN 'Users' THEN
       Ltrim(Rtrim(
       Concat(Ifnull(users.first_name, ''), ' ',
       Ifnull(users.last_name, ''))))
       WHEN 'Prospects' THEN Ltrim(
                               Rtrim(
       Concat(Ifnull(prospects.first_name, ''), ' ',
                   Ifnull(prospects.last_name, ''))))
       END )                recipient_name
FROM   emailman
       LEFT JOIN users
              ON users.id = emailman.related_id
                 AND emailman.related_type = 'Users'
       LEFT JOIN contacts
              ON contacts.id = emailman.related_id
                 AND emailman.related_type = 'Contacts'
       LEFT JOIN leads
              ON leads.id = emailman.related_id
                 AND emailman.related_type = 'Leads'
       LEFT JOIN accounts
              ON accounts.id = emailman.related_id
                 AND emailman.related_type = 'Accounts'
       LEFT JOIN prospects
              ON prospects.id = emailman.related_id
                 AND emailman.related_type = 'Prospects'
       LEFT JOIN prospect_lists
              ON prospect_lists.id = emailman.list_id
       LEFT JOIN email_addr_bean_rel
              ON email_addr_bean_rel.bean_id = emailman.related_id
                 AND emailman.related_type = email_addr_bean_rel.bean_module
                 AND email_addr_bean_rel.primary_address = 1
                 AND email_addr_bean_rel.deleted = 0
       LEFT JOIN campaigns
              ON campaigns.id = emailman.campaign_id
       LEFT JOIN email_marketing
              ON email_marketing.id = emailman.marketing_id
WHERE  emailman.campaign_id = ''
       AND emailman.deleted = 0
       AND emailman.deleted = 0  ";
        $expected = trim($expected);
        $expected = str_replace(' ', '', $expected);
        $expected = str_replace("\n", '', $expected);
        $expected = str_replace("\t", '', $expected);
        $expected = str_replace("\r", '', $expected);
        $expected = strtolower($expected);

        $actual = $campaign->get_queue_items();
        $actual = trim($actual);
        $actual = str_replace(' ', '', $actual);
        $actual = str_replace("\n", '', $actual);
        $actual = str_replace("\t", '', $actual);
        $actual = str_replace("\t", '', $actual);
        $actual = strtolower($actual);

        self::assertSame($expected, $actual);

        //with parameters
        $expected = "SELECT emailman.*,
       campaigns.NAME       AS campaign_name,
       email_marketing.NAME AS message_name,
       ( CASE related_type
           WHEN 'Contacts' THEN
         Ltrim(Rtrim(
               Concat(Ifnull(contacts.first_name, ''), ' ',
               Ifnull(contacts.last_name, ''))))
           WHEN 'Leads' THEN
       Ltrim(Rtrim(
       Concat(Ifnull(leads.first_name, ''), ' ',
       Ifnull(leads.last_name, ''))))
       WHEN 'Accounts' THEN accounts.NAME
       WHEN 'Users' THEN
       Ltrim(Rtrim(
       Concat(Ifnull(users.first_name, ''), ' ',
       Ifnull(users.last_name, ''))))
       WHEN 'Prospects' THEN Ltrim(
                               Rtrim(
       Concat(Ifnull(prospects.first_name, ''), ' ',
                   Ifnull(prospects.last_name, ''))))
       END )                recipient_name
FROM   emailman
       LEFT JOIN users
              ON users.id = emailman.related_id
                 AND emailman.related_type = 'Users'
       LEFT JOIN contacts
              ON contacts.id = emailman.related_id
                 AND emailman.related_type = 'Contacts'
       LEFT JOIN leads
              ON leads.id = emailman.related_id
                 AND emailman.related_type = 'Leads'
       LEFT JOIN accounts
              ON accounts.id = emailman.related_id
                 AND emailman.related_type = 'Accounts'
       LEFT JOIN prospects
              ON prospects.id = emailman.related_id
                 AND emailman.related_type = 'Prospects'
       LEFT JOIN prospect_lists
              ON prospect_lists.id = emailman.list_id
       LEFT JOIN email_addr_bean_rel
              ON email_addr_bean_rel.bean_id = emailman.related_id
                 AND emailman.related_type = email_addr_bean_rel.bean_module
                 AND email_addr_bean_rel.primary_address = 1
                 AND email_addr_bean_rel.deleted = 0
       LEFT JOIN campaigns
              ON campaigns.id = emailman.campaign_id
       LEFT JOIN email_marketing
              ON email_marketing.id = emailman.marketing_id
       INNER JOIN (SELECT Min(id) AS id
                   FROM   emailman em
                   GROUP  BY users.id) secondary
               ON emailman.id = secondary.id
WHERE  emailman.campaign_id = ''
       AND emailman.deleted = 0
       AND marketing_id = '1'
       AND emailman.deleted = 0  ";

        $expected = trim($expected);
        $expected = str_replace(' ', '', $expected);
        $expected = str_replace("\n", '', $expected);
        $expected = str_replace("\r", '', $expected);
        $expected = str_replace("\t", '', $expected);
        $expected = strtolower($expected);

        $actual = $campaign->get_queue_items(array('EMAIL_MARKETING_ID_VALUE' => 1, 'group_by' => 'users.id'));
        $actual = trim($actual);
        $actual = str_replace(' ', '', $actual);
        $actual = str_replace("\n", '', $actual);
        $actual = str_replace("\r", '', $actual);
        $actual = str_replace("\t", '', $actual);
        $actual = strtolower($actual);
        self::assertSame($expected, $actual);
    }

    public function testbean_implements(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');
        self::assertEquals(false, $campaign->bean_implements('')); //test with blank value
        self::assertEquals(false, $campaign->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $campaign->bean_implements('ACL')); //test with valid value
    }

    public function testcreate_list_count_query(): void
    {
        $campaign = BeanFactory::newBean('Campaigns');

        //test without parameters
        $expected = '';
        $actual = $campaign->create_list_count_query('');
        self::assertSame($expected, $actual);

        //test with query parameters
        $expected = 'SELECT count(*) c FROM campaigns';
        $actual = $campaign->create_list_count_query('select * from campaigns');
        self::assertSame($expected, $actual);

        //test with distinct
        $expected = 'SELECT count(DISTINCT campaigns.id) c FROM campaigns';
        $actual = $campaign->create_list_count_query('SELECT distinct marketing_id FROM campaigns');
        self::assertSame($expected, $actual);
    }

    public function testgetDeletedCampaignLogLeadsCount(): void
    {
        $result = BeanFactory::newBean('Campaigns')->getDeletedCampaignLogLeadsCount();
        self::assertEquals(0, $result);
    }
}
