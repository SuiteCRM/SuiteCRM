<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class SugarFeedTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testSugarFeed(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $sugarFeed = BeanFactory::newBean('SugarFeed');

        self::assertInstanceOf('SugarFeed', $sugarFeed);
        self::assertInstanceOf('Basic', $sugarFeed);
        self::assertInstanceOf('SugarBean', $sugarFeed);

        self::assertEquals('sugarfeed', $sugarFeed->table_name);
        self::assertEquals('SugarFeed', $sugarFeed->module_dir);
        self::assertEquals('SugarFeed', $sugarFeed->object_name);

        self::assertEquals(true, $sugarFeed->new_schema);
        self::assertEquals(false, $sugarFeed->importable);
    }

    public function testactivateAndDisableModuleFeed(): void
    {
        self::markTestIncomplete('environment dependency');

        $admin = BeanFactory::newBean('Administration');

        //test activateModuleFeed method
        SugarFeed::activateModuleFeed('Accounts');
        $admin->retrieveSettings('sugarfeed');
        self::assertEquals(1, $admin->settings['sugarfeed_module_Accounts']);

        //test disableModuleFeed method
        SugarFeed::disableModuleFeed('Accounts');
        $admin->retrieveSettings('sugarfeed');
        self::assertEquals(0, $admin->settings['sugarfeed_module_Accounts']);
    }

    public function testflushBackendCache(): void
    {
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SugarFeed::flushBackendCache();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetModuleFeedFiles(): void
    {
        //test with invalid module
        $expected = array();
        $result = SugarFeed::getModuleFeedFiles('Accounts');
        self::assertEquals($expected, $result);

        //test with valid module
        $expected = array('CaseFeed.php' => 'modules/Cases/SugarFeeds/CaseFeed.php');
        $result = SugarFeed::getModuleFeedFiles('Cases');
        self::assertEquals($expected, $result);
    }

    public function testgetActiveFeedModules(): void
    {
        $result = SugarFeed::getActiveFeedModules();

        $expected = array(
                'UserFeed' => 'UserFeed',
                'Cases' => 'Cases',
                'Contacts' => 'Contacts',
                'Leads' => 'Leads',
                'Opportunities' => 'Opportunities',
        );

        self::assertEquals($expected, $result);
    }

    public function testgetAllFeedModules(): void
    {
        $result = SugarFeed::getAllFeedModules();
        $expected = array(
                      'UserFeed' => 'UserFeed',
                      'Cases' => 'Cases',
                      'Contacts' => 'Contacts',
                      'Leads' => 'Leads',
                      'Opportunities' => 'Opportunities',
                    );

        self::assertEquals($expected, $result);
    }

    public function testpushFeed2(): void
    {
        $lead = BeanFactory::newBean('Leads');
        $lead->id = 1;
        $lead->assigned_user_id = 1;

        SugarFeed::pushFeed2('some text 2', $lead, 'Link', 'some url');

        //retrieve newly created bean
        $sugarFeed = BeanFactory::newBean('SugarFeed');
        $result = $sugarFeed->retrieve_by_string_fields(array('related_id' => '1', 'related_module' => 'Leads'));

        //test for record ID to verify that record is saved
        self::assertTrue(isset($sugarFeed->id));
        self::assertEquals(36, strlen((string) $sugarFeed->id));

        //mark the record as deleted
        $sugarFeed->mark_deleted($sugarFeed->id);
    }

    public function testpushFeed(): void
    {
        SugarFeed::pushFeed('some text', 'SugarFeed', 1, 1, 'Link', 'some url');

        //retrieve newly created bean
        $sugarFeed = BeanFactory::newBean('SugarFeed');
        $result = $sugarFeed->retrieve_by_string_fields(array('related_id' => '1', 'related_module' => 'SugarFeed'));

        //test for record ID to verify that record is saved
        self::assertTrue(isset($sugarFeed->id));
        self::assertEquals(36, strlen((string) $sugarFeed->id));

        //test fetchReplies method
        $this->fetchReplies();

        //mark the record as deleted
        $sugarFeed->mark_deleted($sugarFeed->id);
    }

    public function fetchReplies(): void
    {
        $actual = BeanFactory::newBean('SugarFeed')->fetchReplies(array('ID' => '1'));
        self::assertGreaterThan(0, strlen((string) $actual));
    }

    public function testgetLinkTypes(): void
    {
        $result = SugarFeed::getLinkTypes();

        $expected = array(
                'Image' => 'Image',
                'Link' => 'Link',
                'YouTube' => 'YouTube',
        );
        self::assertEquals($expected, $result);
    }

    public function testgetLinkClass(): void
    {
        //test with invalid LinkType
        $result = SugarFeed::getLinkClass('test');
        self::assertEquals(false, $result);

        //test with LinkType Image
        $result = SugarFeed::getLinkClass('Image');
        self::assertInstanceOf('FeedLinkHandlerImage', $result);

        //test with LinkType Link
        $result = SugarFeed::getLinkClass('Link');
        self::assertInstanceOf('FeedLinkHandlerLink', $result);

        //test with LinkType YouTube
        $result = SugarFeed::getLinkClass('YouTube');
        self::assertInstanceOf('FeedLinkHandlerYoutube', $result);
    }

    public function testget_list_view_data(): void
    {
        $result = BeanFactory::newBean('SugarFeed')->get_list_view_data();
        self::assertIsArray($result);
    }

    public function testgetTimeLapse(): void
    {
        $result = SugarFeed::getTimeLapse('2016-01-15 11:16:02');
        self::assertTrue(isset($result));
        self::assertGreaterThanOrEqual(0, strlen((string) $result));
    }

    public function testparseMessage(): void
    {
        // test with a string with no links
        $html = 'some text with no urls';
        $result = SugarFeed::parseMessage($html);
        self::assertEquals($html, $result);

        // test with a string with links
        $html = 'some text http://www.url.com with no urls';
        $expected = "some text <a href='http://www.url.com' target='_blank'>http://www.url.com</a> with no urls";
        $result = SugarFeed::parseMessage($html);
        self::assertEquals($expected, $result);
    }
}
