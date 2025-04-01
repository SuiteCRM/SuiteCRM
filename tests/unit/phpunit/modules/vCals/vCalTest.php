<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class vCalTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testvCal(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $vcal = BeanFactory::newBean('vCals');

        self::assertInstanceOf('vCal', $vcal);
        self::assertInstanceOf('SugarBean', $vcal);

        self::assertEquals('vcals', $vcal->table_name);
        self::assertEquals('vCals', $vcal->module_dir);
        self::assertEquals('vCal', $vcal->object_name);

        self::assertEquals(true, $vcal->new_schema);
        self::assertEquals(false, $vcal->tracker_visibility);
        self::assertEquals(true, $vcal->disable_row_level_security);
    }

    public function testget_summary_text(): void
    {
        $vcal = BeanFactory::newBean('vCals');

        //test without setting name
        self::assertEquals(null, $vcal->get_summary_text());

        //test with name set
        $vcal->name = 'test';
        self::assertEquals('', $vcal->get_summary_text());
    }

    public function testfill_in_additional_list_fields(): void
    {
        $vcal = BeanFactory::newBean('vCals');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $vcal->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('method has no implementation');
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $vcal = BeanFactory::newBean('vCals');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $vcal->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('method has no implementation');
    }

    public function testget_list_view_data(): void
    {
        $vcal = BeanFactory::newBean('vCals');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $vcal->get_list_view_data();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::markTestIncomplete('method has no implementation');
    }

    public function testget_freebusy_lines_cache(): void
    {
        self::markTestIncomplete('Asserting String Start Width is imposible if expected is empty srting');

        $vcal = BeanFactory::newBean('vCals');
        $user_bean = new User('1');

        $expectedStart = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//SugarCRM//SugarCRM Calendar//EN\r\nBEGIN:VFREEBUSY\r\nORGANIZER;CN= :VFREEBUSY\r\n";
        $expectedEnd = "END:VFREEBUSY\r\nEND:VCALENDAR\r\n";

        $result = $vcal->get_freebusy_lines_cache($user_bean);

        self::assertStringStartsWith($expectedStart, $result);
        self::assertStringEndsWith($expectedEnd, $result);
    }

    public function testcreate_sugar_freebusy(): void
    {
        global $locale, $timedate;

        $vcal = BeanFactory::newBean('vCals');
        $user_bean = new User('1');

        $now_date_time = $timedate->getNow(true);
        $start_date_time = $now_date_time->get('yesterday');
        $end_date_time = $now_date_time->get('tomorrow');

        $result = $vcal->create_sugar_freebusy($user_bean, $start_date_time, $end_date_time);
        self::assertGreaterThanOrEqual(0, strlen((string) $result));
    }

    public function testget_vcal_freebusy(): void
    {
        $vcal = BeanFactory::newBean('vCals');
        $user_focus = new User('1');

        $expectedStart = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//SugarCRM//SugarCRM Calendar//EN\r\nBEGIN:VFREEBUSY\r\nORGANIZER;CN= :VFREEBUSY\r\n";
        $expectedEnd = "END:VFREEBUSY\r\nEND:VCALENDAR\r\n";

        $result = $vcal->get_vcal_freebusy($user_focus);

        self::assertStringStartsWith($expectedStart, $result);
        self::assertStringEndsWith($expectedEnd, $result);
    }

    public function testcache_sugar_vcal(): void
    {
        $vcal = BeanFactory::newBean('vCals');
        $user_focus = new User('1');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $vcal::cache_sugar_vcal($user_focus);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcache_sugar_vcal_freebusy(): void
    {
        $vcal = BeanFactory::newBean('vCals');
        $user_focus = new User('1');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $vcal::cache_sugar_vcal_freebusy($user_focus);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfold_ical_lines(): void
    {
        //test with short strings
        $result = vCal::fold_ical_lines('testkey', 'testvalue');
        self::assertEquals('testkey:testvalue', $result);

        //test with longer strings
        $expected = "testkey11111111111111111111111111111111111111111111111111111111111111111111\r\n	11111111111111111111111111111111:testvalue11111111111111111111111111111111\r\n	11111111111111111111111111111111111111111111111111111111111111111111";
        $result = vCal::fold_ical_lines('testkey'.str_repeat('1', 100), 'testvalue'.str_repeat('1', 100));
        self::assertEquals($expected, $result);
    }

    public function testcreate_ical_array_from_string(): void
    {
        $iCalString = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//SugarCRM//SugarCRM Calendar//EN\r\nBEGIN:VFREEBUSY\r\nORGANIZER;CN= :VFREEBUSY\r\nDTSTART:2016-01-09 00:00:00\r\nDTEND:2016-03-09 00:00:00\r\nDTSTAMP:2016-01-10 11:07:15\r\nEND:VFREEBUSY\r\nEND:VCALENDAR\r\n";
        $expected = array(
                        array('BEGIN', 'VCALENDAR'),
                        array('VERSION', '2.0'),
                        array('PRODID', '-//SugarCRM//SugarCRM Calendar//EN'),
                        array('BEGIN', 'VFREEBUSY'),
                        array('ORGANIZER;CN= ', 'VFREEBUSY'),
                        array('DTSTART', '2016-01-09 00:00:00'),
                        array('DTEND', '2016-03-09 00:00:00'),
                        array('DTSTAMP', '2016-01-10 11:07:15'),
                        array('END', 'VFREEBUSY'),
                        array('END', 'VCALENDAR'),
                    );
        $actual = vCal::create_ical_array_from_string($iCalString);
        self::assertSame($expected, $actual);
    }

    public function testcreate_ical_string_from_array(): void
    {
        $expected = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//SugarCRM//SugarCRM Calendar//EN\r\nBEGIN:VFREEBUSY\r\nORGANIZER;CN= :VFREEBUSY\r\nDTSTART:2016-01-09 00:00:00\r\nDTEND:2016-03-09 00:00:00\r\nDTSTAMP:2016-01-10 11:07:15\r\nEND:VFREEBUSY\r\nEND:VCALENDAR\r\n";
        $iCalArray = array(
                array('BEGIN', 'VCALENDAR'),
                array('VERSION', '2.0'),
                array('PRODID', '-//SugarCRM//SugarCRM Calendar//EN'),
                array('BEGIN', 'VFREEBUSY'),
                array('ORGANIZER;CN= ', 'VFREEBUSY'),
                array('DTSTART', '2016-01-09 00:00:00'),
                array('DTEND', '2016-03-09 00:00:00'),
                array('DTSTAMP', '2016-01-10 11:07:15'),
                array('END', 'VFREEBUSY'),
                array('END', 'VCALENDAR'),
        );
        $actual = vCal::create_ical_string_from_array($iCalArray);
        self::assertSame($expected, $actual);
    }

    public function testescape_ical_chars(): void
    {
        self::assertSame('', vCal::escape_ical_chars(''));
        self::assertSame('\;\,', vCal::escape_ical_chars(';,'));
    }

    public function testunescape_ical_chars(): void
    {
        self::assertSame('', vCal::unescape_ical_chars(''));
        self::assertSame('; , \\', vCal::unescape_ical_chars('\\; \\, \\\\'));
    }

    public function testget_ical_event(): void
    {
        $user = new User(1);
        $meeting = BeanFactory::newBean('Meetings');

        $meeting->id = 1;
        $meeting->date_start = '2016-02-11 17:30:00';
        $meeting->date_end = '2016-02-11 17:30:00';
        $meeting->name = 'test';
        $meeting->location = 'test location';
        $meeting->description = 'test description';

        $expectedStart = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//SugarCRM//SugarCRM Calendar//EN\r\nBEGIN:VEVENT\r\nUID:1\r\nORGANIZER;CN=:mailto:\r\nDTSTART:20160211T173000Z\r\nDTEND:20160211T173000Z\r\n";
        $expectedEnd = "\r\nSUMMARY:test\r\nLOCATION:test location\r\nDESCRIPTION:test description\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n";

        $result = vCal::get_ical_event($meeting, $user);

        self::assertStringStartsWith($expectedStart, $result);
        self::assertStringEndsWith($expectedEnd, $result);
    }
}
