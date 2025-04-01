<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

use SuiteCRM\Utility\SuiteValidator;

require_once __DIR__ . '/../../../../../include/GoogleSync/GoogleSync.php';
require_once __DIR__ . '/GoogleSyncMock.php';

class GoogleSyncTest extends SuitePHPUnitFrameworkTestCase
{
    /** @var UnitTester */
    protected $tester;

    /**
     *
     * @param string $googleAuthJson
     * @return string
     */
    protected function getFakeSugarConfig($googleAuthJson = null)
    {
        return [
            'google_auth_json' => $this->getFakeGoogleAuthJson($googleAuthJson),
        ];
    }

    /**
     *
     * @param string $googleAuthJson
     * @return string
     */
    protected function getFakeGoogleAuthJson($googleAuthJson): string
    {
        return base64_encode($googleAuthJson);
    }

    // GoogleSyncBase.php

    /**
     *
     *
     */
    public function test__construct(): void
    {

        $sugar_config = [];
        // Set up object for testing

        // base64 encoded of {"web":"test"}
        $sugar_config['google_auth_json'] = 'eyJ3ZWIiOiJ0ZXN0In0=';

        // Set Log Level
        if (!empty($_SERVER['GSYNC_LOGLEVEL'])) {
            $expectedLogLevel = $_SERVER['GSYNC_LOGLEVEL'];
        } else {
            $_SERVER['GSYNC_LOGLEVEL'] = 'debug';
            $expectedLogLevel = 'debug';
        }

        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        // Test GoogleSync::timezone
        $timezone = $object->getProperty('timezone');
        self::assertNotEmpty($timezone);
        self::assertEquals("string", gettype($timezone));

        // Test GoogleSync::authJson
        $authJson = $object->getProperty('authJson');
        self::assertNotEmpty($authJson);
        self::assertEquals("array", gettype($authJson));

        // Test GoogleSync::db
        $expectedClass = DBManager::class;
        $actualClass = $object->getProperty('db');
        self::assertInstanceOf($expectedClass, $actualClass);
    }

    /**
     *
     *
     */
    public function testGetAuthJson(): void
    {
        global $sugar_config;
        $sugar_config = $sugar_config ?? [];
        // base64 encoded of {"web":"test"}
        $sugar_config['google_auth_json'] = 'eyJ3ZWIiOiJ0ZXN0In0=';

        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        $expectedAuthJson = json_decode(base64_decode('eyJ3ZWIiOiJ0ZXN0In0'), true, 512, JSON_THROW_ON_ERROR);
        $actualAuthJson = $object->callMethod('getAuthJson', [$this->getFakeSugarConfig('{"web":"test"}')]);

        self::assertEquals($expectedAuthJson, $actualAuthJson);
        self::assertArrayHasKey('web', $actualAuthJson);

        try {
            $object->callMethod('getAuthJson', [['google_auth_json' => 'INVALID']]);
            self::assertTrue(false);
        } catch (GoogleSyncException $e) {
            self::assertTrue(true);
            self::assertEquals(GoogleSyncException::JSON_CORRUPT, $e->getCode());
        }

        try {
            $object->callMethod('getAuthJson', [$this->getFakeSugarConfig('{"foo":"bar"}')]);
            self::assertTrue(false);
        } catch (GoogleSyncException $e) {
            self::assertTrue(true);
            self::assertEquals(GoogleSyncException::JSON_KEY_MISSING, $e->getCode());
        }

        $ret = $object->callMethod('getAuthJson', [null]);
        self::assertFalse($ret);
    }

    /**
     *
     *
     */
    public function testSetClient(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('setClient', [null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INVALID_CLIENT_ID, $e->getCode());
        }
    }

    public function testGetClient(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('getClient', [null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INVALID_CLIENT_ID, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testGetGoogleClient(): void
    {
        try {
            $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test","client_id":"testID"'));
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::JSON_CORRUPT, $e->getCode(), 'It should throws an exception with code 0.');
            self::assertEquals('google_auth_json not vaild json', $e->getMessage(), 'It should throws an exception with a proper message.');
        }

        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test","client_id":"testID"}'));
        try {
            $object->callMethod('getGoogleClient', [null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::ACCSESS_TOKEN_PARAMETER_MISSING, $e->getCode(), 'It should throws an exception with code 0.');
            self::assertEquals('Access Token Parameter Missing', $e->getMessage(), 'It should throws an exception with a proper message.');
        }
    }

    /**
     *
     *
     */
    public function testInitUserService(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('initUserService', [null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INVALID_CLIENT_ID, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testGetUserMeetings(): void
    {
        // Create a User
        $user = BeanFactory::getBean('Users');
        $user->last_name = 'UNIT_TESTS';
        $user->user_name = 'UNIT_TESTS';
        $user->save();

        // Create three meetings and save them to the DB for testing.
        $meeting1 = BeanFactory::getBean('Meetings');
        $meeting1->name = 'UNIT_TEST_1';
        $meeting1->assigned_user_id = $user->id;
        $meeting1->status = 'Not Held';
        $meeting1->type = 'Sugar';
        $meeting1->description = 'test description';
        $meeting1->duration_hours = 1;
        $meeting1->duration_minutes = 1;
        $meeting1->date_start = '2016-02-11 17:30:00';
        $meeting1->date_end = '2016-02-11 17:30:00';
        $meeting1->save();

        $meeting2 = BeanFactory::getBean('Meetings');
        $meeting2->name = 'UNIT_TEST_2';
        $meeting2->assigned_user_id = $user->id;
        $meeting2->status = 'Not Held';
        $meeting2->type = 'Sugar';
        $meeting2->description = 'test description';
        $meeting2->duration_hours = 1;
        $meeting2->duration_minutes = 1;
        $meeting2->date_start = '2016-02-11 17:30:00';
        $meeting2->date_end = '2016-02-11 17:30:00';
        $meeting2->save();

        $meeting3 = BeanFactory::getBean('Meetings');
        $meeting3->name = 'UNIT_TEST_3';
        $meeting3->assigned_user_id = $user->id;
        $meeting3->status = 'Not Held';
        $meeting3->type = 'Sugar';
        $meeting3->description = 'test description';
        $meeting3->duration_hours = 1;
        $meeting3->duration_minutes = 1;
        $meeting3->date_start = '2016-02-11 17:30:00';
        $meeting3->date_end = '2016-02-11 17:30:00';
        $meeting3->save();



        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        $return_count = $object->callMethod('getUserMeetings', [$user->id]);

        // Test for invalid user id exception handling
        try {
            $return = $object->callMethod('getUserMeetings', ['INVALID!+']);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INVALID_USER_ID, $e->getCode());
        }

        self::assertCount(3, $return_count);
    }

    /**
     *
     *
     */
    public function testSetUsersGoogleCalendar(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        self::assertEquals(false, $object->callMethod('setUsersGoogleCalendar'));
    }

    /**
     *
     *
     */
    public function testGetSuiteCRMCalendar(): void
    {
        $result = (new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}')))->callMethod('getSuiteCRMCalendar', [new Google_Service_Calendar_CalendarList()]);
        self::assertEquals(null, $result);
    }

    /**
     *
     *
     */
    public function testGetUserGoogleEvents(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        self::assertEquals(false, $object->callMethod('getUserGoogleEvents'));
    }

    /**
     *
     *
     */
    public function testIsServiceExists(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        self::assertEquals(false, $object->callMethod('isServiceExists'));
    }

    /**
     *
     *
     */
    public function testIsCalendarExists(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        self::assertEquals(false, $object->callMethod('isCalendarExists'));
    }

    /**
     *
     *
     */
    public function testGetGoogleEventById(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('getGoogleEventById', [null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::EVENT_ID_IS_EMPTY, $e->getCode());
            self::assertEquals('event ID is empty', $e->getMessage());
        }
    }

    /**
     *
     *
     */
    public function testGetMeetingByEventId(): void
    {
        $db = DBManagerFactory::getInstance();


        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        // Create three meetings and save them to the DB for testing.
        $meeting1 = BeanFactory::getBean('Meetings');
        $meeting1->name = 'test1';
        $meeting1->assigned_user_id = '666';
        $meeting1->status = 'Not Held';
        $meeting1->type = 'Sugar';
        $meeting1->description = 'test description';
        $meeting1->duration_hours = 1;
        $meeting1->duration_minutes = 1;
        $meeting1->date_start = '2016-02-11 17:30:00';
        $meeting1->date_end = '2016-02-11 17:30:00';
        $meeting1_id = $meeting1->save();

        $meeting2 = BeanFactory::getBean('Meetings');
        $meeting2->name = 'test2';
        $meeting2->assigned_user_id = '666';
        $meeting2->status = 'Not Held';
        $meeting2->type = 'Sugar';
        $meeting2->description = 'test description';
        $meeting2->duration_hours = 1;
        $meeting2->duration_minutes = 1;
        $meeting2->date_start = '2016-02-11 17:30:00';
        $meeting2->date_end = '2016-02-11 17:30:00';
        $meeting2_id = $meeting2->save();

        $meeting3 = BeanFactory::getBean('Meetings');
        $meeting3->name = 'test3';
        $meeting3->assigned_user_id = '666';
        $meeting3->status = 'Not Held';
        $meeting3->type = 'Sugar';
        $meeting3->description = 'test description';
        $meeting3->duration_hours = 1;
        $meeting3->duration_minutes = 1;
        $meeting3->date_start = '2016-02-11 17:30:00';
        $meeting3->date_end = '2016-02-11 17:30:00';
        $meeting3_id = $meeting3->save();

        // --- separated test
        // Give meeting 1 a gsync_id
        $sql1 = "UPDATE meetings SET gsync_id = 'valid_gsync_id' WHERE id = '{$meeting1_id}'";
        $res1 = $db->query($sql1);
        self::assertEquals(true, $res1);

        // --- separated test
        // Give meetings 2 and 3 a duplicate gsync_id
        $sql2 = "UPDATE meetings SET gsync_id = 'duplicate_gsync_id' WHERE id = '{$meeting2_id}' OR id = '{$meeting3_id}'";
        $res2 = $db->query($sql2);
        self::assertEquals(true, $res2);

        // --- separated test
        $meeting = $object->callMethod('getMeetingByEventId', ['valid_gsync_id']);
        self::assertEquals($meeting1_id, $meeting->id);

        // --- separated test
        try {
            $object->callMethod('getMeetingByEventId', ['duplicate_gsync_id']);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e11) {
            self::assertEquals(GoogleSyncException::AMBIGUOUS_MEETING_ID, $e11->getCode());
            self::assertEquals('More than one meeting matches Google Id!', $e11->getMessage());
        }

        // --- separated test
        $ret4 = $object->callMethod('getMeetingByEventId', ['NOTHING_MATCHES']);
        self::assertNull($ret4);
    }

    /**
     *
     *
     */
    public function testSetGService(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('setGService');
            self::assertTrue(false);
        } catch (GoogleSyncException $e) {
            self::assertTrue(true);
            self::assertEquals(GoogleSyncException::NO_GCLIENT_SET, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testPushEvent(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        try {
            $object->callMethod('pushEvent', [BeanFactory::getBean('Meetings'), null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::NO_GSERVICE_SET, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testReturnExtendedProperties(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        // BEGIN: Create Google Event Object
        $Google_Event = new Google\Service\Calendar\Event();

        $Google_Event->setSummary('Unit Test Event');
        $Google_Event->setDescription('Unit Test Event');
        $Google_Event->setLocation('123 Seseme Street');

        // Set start date/time
        $startDateTime = new Google\Service\Calendar\EventDateTime;
        $startDateTime->setDateTime(date(DATE_ATOM, strtotime('2018-01-01 01:00:00 UTC')));
        $startDateTime->setTimeZone('Etc/UTC');
        $Google_Event->setStart($startDateTime);

        // Set end date/time
        $endDateTime = new Google\Service\Calendar\EventDateTime;
        $endDateTime->setDateTime(date(DATE_ATOM, strtotime('2018-01-01 02:00:00 UTC')));
        $endDateTime->setTimeZone('Etc/UTC');
        $Google_Event->setEnd($endDateTime);

        // Set extended properties
        $extendedProperties = new Google\Service\Calendar\EventExtendedProperties;
        $private = array();
        $private['suitecrm_id'] = 'INVALID';
        $private['suitecrm_type'] = 'INVALID';
        $private['remain_unchanged'] = 'VALID';
        $extendedProperties->setPrivate($private);
        $Google_Event->setExtendedProperties($extendedProperties);

        // Set popup reminder
        $reminders_remote = new Google\Service\Calendar\EventReminders;
        $reminders_remote->setUseDefault(false);
        $reminders_array = array();
        $reminder_remote = new Google\Service\Calendar\EventReminder;
        $reminder_remote->setMethod('popup');
        $reminder_remote->setMinutes('15');
        $reminders_array[] = $reminder_remote;
        $reminders_remote->setOverrides($reminders_array);
        $Google_Event->setReminders($reminders_remote);

        // END: Create Google Event Object

        // Create SuiteCRM Meeting Object
        $CRM_Meeting = BeanFactory::getBean('Meetings');

        $CRM_Meeting->id = 'FAKE_MEETING_ID';
        $CRM_Meeting->name = 'Unit Test Event';
        $CRM_Meeting->description = 'Unit Test Event';
        $CRM_Meeting->location = '123 Sesame Street';
        $CRM_Meeting->date_start = '2018-01-01 12:00:00';
        $CRM_Meeting->date_end = '2018-01-01 13:00:00';
        $CRM_Meeting->module_name = 'Meeting';

        $return = $object->callMethod('returnExtendedProperties', [$Google_Event, $CRM_Meeting]);
        $returnPrivate = $return->getPrivate();

        self::assertEquals('FAKE_MEETING_ID', $returnPrivate['suitecrm_id']);
        self::assertEquals('Meeting', $returnPrivate['suitecrm_type']);
        self::assertEquals('VALID', $returnPrivate['remain_unchanged']);
    }

    /**
     *
     *
     */
    public function testPullEvent(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        try {
            $object->callMethod('pullEvent', [new Google\Service\Calendar\Event(), null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::NO_REMOVE_EVENT_START_IS_NOT_SET, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testDelMeeting(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        try {
            $object->callMethod('delMeeting', [BeanFactory::getBean('Meetings')]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INCORRECT_WORKING_USER_TYPE, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testDelEvent(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        try {
            $object->callMethod('delEvent', [new Google\Service\Calendar\Event(), null]);
            self::assertTrue(false);
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::NO_GSERVICE_SET, $e->getCode());
        }


        // --- separated test
        $Google_Event = new Google\Service\Calendar\Event();
        $Google_Event->setSummary('Unit Test Event');
        $Google_Event->setDescription('Unit Test Event');

        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        $object->setProperty('gService', true);

        try {
            $object->callMethod('delEvent', [new Google\Service\Calendar\Event(), null]);

            self::assertTrue(false, 'It should throw an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::MEETING_ID_IS_EMPTY, $e->getCode());
        }
        // -- another test
        try {
            $object->callMethod('delEvent', [$Google_Event, 'INVALID_ID+!']);
            self::assertTrue(false, 'It should throw an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::RECORD_VALIDATION_FAILURE, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testClearPopups(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('clearPopups', [null]);
            self::assertTrue(false, 'It should throw an exception.');
        } catch (InvalidArgumentException $e) {
            self::assertTrue(true);
            self::assertEquals(0, $e->getCode());
        }
        $ret = $object->callMethod('clearPopups', '123456');
    }

    /**
     *
     *
     */
    public function testCreateSuitecrmMeetingEvent(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        date_default_timezone_set('Etc/UTC');

        // BEGIN: Create Google Event Object
        $Google_Event = new Google\Service\Calendar\Event();

        $Google_Event->setSummary('Unit Test Event');
        $Google_Event->setDescription('Unit Test Event');
        $Google_Event->setLocation('123 Seseme Street');

        // Set start date/time
        $startDateTime = new Google\Service\Calendar\EventDateTime;
        $startDateTime->setDateTime(date(DATE_ATOM, strtotime('2018-01-01 01:00:00 UTC')));
        $startDateTime->setTimeZone('Etc/UTC');
        $Google_Event->setStart($startDateTime);

        // Set end date/time
        $endDateTime = new Google\Service\Calendar\EventDateTime;
        $endDateTime->setDateTime(date(DATE_ATOM, strtotime('2018-01-01 02:00:00 UTC')));
        $endDateTime->setTimeZone('Etc/UTC');
        $Google_Event->setEnd($endDateTime);

        // Set extended properties
        $extendedProperties = new Google\Service\Calendar\EventExtendedProperties;
        $private = array();
        $private['suitecrm_id'] = 'RECORD_ID';
        $private['suitecrm_type'] = 'Meeting';
        $extendedProperties->setPrivate($private);
        $Google_Event->setExtendedProperties($extendedProperties);

        // Set popup reminder
        $reminders_remote = new Google\Service\Calendar\EventReminders;
        $reminders_remote->setUseDefault(false);
        $reminders_array = array();
        $reminder_remote = new Google\Service\Calendar\EventReminder;
        $reminder_remote->setMethod('popup');
        $reminder_remote->setMinutes('15');
        $reminders_array[] = $reminder_remote;
        $reminders_remote->setOverrides($reminders_array);
        $Google_Event->setReminders($reminders_remote);

        // END: Create Google Event Object

        // Set id of fake user
        $user = BeanFactory::getBean('Users');
        $user->id = 'FAKEUSER';
        $object->setProperty('workingUser', $user);

        $return = $object->callMethod('createSuitecrmMeetingEvent', [$Google_Event]);

        self::assertInstanceOf(\Meeting::class, $return);
        self::assertNotNull($return->id);
        self::assertEquals('0', $return->deleted);
        self::assertEquals('Unit Test Event', $return->name);
        self::assertEquals('Unit Test Event', $return->description);
        self::assertEquals('123 Seseme Street', $return->location);
        self::assertEquals('2018-01-01 01:00:00', $return->date_start);
        self::assertEquals('2018-01-01 02:00:00', $return->date_end);
        self::assertEquals('1', $return->duration_hours);
        self::assertEquals('0', $return->duration_minutes);
        self::assertEquals('FAKEUSER', $return->assigned_user_id);
    }

    /**
     *
     *
     */
    public function testCreateGoogleCalendarEvent(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        $ret = $object->callMethod('setTimeZone', ['Etc/UTC']);
        self::assertTrue($ret);
        $testid = create_guid();

        $timedate = new TimeDate;
        $startTime = $timedate->to_display_date_time('2018-01-01 12:00:00');
        $endTime = $timedate->to_display_date_time('2018-01-01 13:00:00');

        // Create SuiteCRM Meeting Object
        $CRM_Meeting = BeanFactory::getBean('Meetings');

        $CRM_Meeting->id = $testid;
        $CRM_Meeting->name = 'Unit Test Event';
        $CRM_Meeting->description = 'Unit Test Event';
        $CRM_Meeting->location = '123 Sesame Street';
        $CRM_Meeting->date_start = $startTime;
        $CRM_Meeting->date_end = $endTime;
        $CRM_Meeting->module_name = 'Meeting';

        $return = $object->callMethod('createGoogleCalendarEvent', [$CRM_Meeting]);

        $this->assertEquals('Google\Service\Calendar\Event', get_class($return));
        $this->assertEquals('Unit Test Event', $return->getSummary());
        $this->assertEquals('Unit Test Event', $return->getDescription());
        $this->assertEquals('123 Sesame Street', $return->getLocation());

        $start = $return->getStart();
        $end = $return->getEnd();
        self::assertEquals('2018-01-01T12:00:00+00:00', $start->getDateTime());
        self::assertEquals('Etc/UTC', $start->getTimeZone());
        self::assertEquals('2018-01-01T13:00:00+00:00', $end->getDateTime());
        self::assertEquals('Etc/UTC', $end->getTimeZone());

        $private = $return->getExtendedProperties()->getPrivate();
        self::assertEquals($testid, $private['suitecrm_id']);
        self::assertEquals('Meeting', $private['suitecrm_type']);
    }

    /**
     *
     *
     */
    public function testSetTimezone(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        $property = $object->getProperty('timezone');
        self::assertEquals('Etc/UTC', $property);

        $expectedTimezone = 'Etc/GMT';

        $return = $object->callMethod('setTimezone', [$expectedTimezone]);
        $property = $object->getProperty('timezone');

        self::assertTrue($return);
        self::assertEquals($expectedTimezone, $property);
        self::assertEquals($expectedTimezone, date_default_timezone_get());
    }

    /**
     *
     *
     */
    public function testSetLastSync(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        try {
            $object->callMethod('setLastSync', [BeanFactory::getBean('Meetings'), null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INCORRECT_WORKING_USER_TYPE, $e->getCode());
        }
    }

    // GoogleSync.php

    /**
     *
     *
     */
    public function testGetTitle(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        // Create SuiteCRM Meeting Object
        $CRM_Meeting = BeanFactory::getBean('Meetings');

        $CRM_Meeting->id = create_guid();
        $CRM_Meeting->name = 'Unit Test Event';
        $CRM_Meeting->description = 'Unit Test Event';

        // Create Google Event Object
        $Google_Event = new Google\Service\Calendar\Event();

        $Google_Event->setSummary('Unit Test Event');
        $Google_Event->setDescription('Unit Test Event');

        self::assertEquals('Unit Test Event / Unit Test Event', $object->callMethod('getTitle', [$CRM_Meeting, $Google_Event]));
        self::assertEquals('Unit Test Event', $object->callMethod('getTitle', [$CRM_Meeting, null]));
        self::assertEquals('Unit Test Event', $object->callMethod('getTitle', [null, $Google_Event]));
        self::assertEquals('Unit Test Event', $object->callMethod('getTitle', [$CRM_Meeting, null]));
        self::assertEquals('UNNAMED RECORD', $object->callMethod('getTitle', [null, null]));
    }

    /**
     *
     *
     */
    public function testDoAction(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        try {
            $object->callMethod('doAction', ['INVALID']);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INVALID_ACTION, $e->getCode());
        }


        try {
            $object->callMethod('doAction', ['push']);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (InvalidArgumentException $e) {
            self::assertTrue(true, 'It should be an InvalidArgumentException as a first parameter of GoogleSyncBase::pushEvent() should be an instance of meeting but this test implacate that it is null.');
        }


        try {
            $object->callMethod('doAction', ['pull']);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (InvalidArgumentException $e) {
            self::assertTrue(true, 'It should be an InvalidArgumentException as a first parameter of GoogleSyncBase::pullEvent() should be an instance of Google_Service_Calendar_Event but this test implacate that it is null.');
        }

        $CRM_Meeting = BeanFactory::getBean('Meetings');
        $CRM_Meeting->id = 'NOTHINGHERE';
        $CRM_Meeting->name = 'Unit Test Event';
        $CRM_Meeting->description = 'Unit Test Event';


        try {
            $object->callMethod('doAction', ['push_delete', $CRM_Meeting, null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (InvalidArgumentException $e) {
            self::assertTrue(true, 'It should be an InvalidArgumentException as a first parameter of GoogleSyncBase::delEvent() should be an instance of Google_Service_Calendar_Event but this test implacate that it is null.');
        }


        try {
            $object->callMethod('doAction', ['pull_delete']);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (InvalidArgumentException $e) {
            self::assertTrue(true, 'It should be an InvalidArgumentException as a first parameter of GoogleSyncBase::delMeeting() should be an instance of Meeting but this test implacate that it is null.');
        }

        $return = $object->callMethod('doAction', ['skip']);
        self::assertEquals(true, $return);
    }

    /**
     *
     *
     */
    public function testDoSync(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        try {
            $object->callMethod('doSync', [null]);
            self::assertTrue(false, 'It should throws an exception.');
        } catch (GoogleSyncException $e) {
            self::assertEquals(GoogleSyncException::INVALID_CLIENT_ID, $e->getCode());
        }
    }

    /**
     *
     *
     */
    public function testAddUser(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        $return = $object->callMethod('addUser', ['ABC123', 'End User']);

        self::assertTrue($return);
        self::assertArrayHasKey('ABC123', $object->getProperty('users'));
    }

    /**
     *
     *
     */
    public function testPushPullSkip(): void
    {
        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));

        // Create SuiteCRM Meeting Object
        $CRM_Meeting = BeanFactory::getBean('Meetings');

        $CRM_Meeting->id = create_guid();
        $CRM_Meeting->name = 'Unit Test Event';
        $CRM_Meeting->description = 'Unit Test Event';

        // Create Google Event Object
        $Google_Event = new Google\Service\Calendar\Event();

        $Google_Event->setSummary('Unit Test Event');
        $Google_Event->setDescription('Unit Test Event');

        // The event needs a start time method to pass
        $startDateTime = new Google\Service\Calendar\EventDateTime;
        $startDateTime->setDateTime(date(DATE_ATOM, strtotime('2018-01-01 13:00:00 UTC')));
        $Google_Event->setStart($startDateTime);

        // Test with just an active Meeting. Should return 'push'
        self::assertEquals('push', $object->callMethod('pushPullSkip', [$CRM_Meeting, null]));

        // Test with just deleted Meeting. Should return 'skip'
        $CRM_Meeting->deleted = '1';
        self::assertEquals('skip', $object->callMethod('pushPullSkip', [$CRM_Meeting, null]));


        // Test with just an active Google Event. Should return 'pull'
        self::assertEquals('pull', $object->callMethod('pushPullSkip', [null, $Google_Event]));

        // Test with just a canceled Google Event. Should return 'skip'
        $Google_Event->status = 'cancelled';
        self::assertEquals('skip', $object->callMethod('pushPullSkip', [null, $Google_Event]));

        // Test compare both Meeting & Event, but both deleted. Should return 'skip'
        self::assertEquals('skip', $object->callMethod('pushPullSkip', [$CRM_Meeting, $Google_Event]));

        // Set records to active
        $CRM_Meeting->deleted = '0';
        $Google_Event->status = '';

        // Test with newer Meeting. Should return 'push'
        $CRM_Meeting->fetched_row['date_modified'] = '2018-01-02 12:00:00';
        $CRM_Meeting->fetched_row['gsync_lastsync'] = strtotime('2018-01-01 13:00:00 UTC');
        $Google_Event->updated = '2018-01-01 12:00:00 UTC';
        self::assertEquals('push', $object->callMethod('pushPullSkip', [$CRM_Meeting, $Google_Event]));

        // Test with newer Google Event. Should return 'pull'
        $CRM_Meeting->fetched_row['date_modified'] = '2018-01-01 12:00:00';
        $CRM_Meeting->fetched_row['gsync_lastsync'] = strtotime('2018-01-01 13:00:00 UTC');
        $Google_Event->updated = '2018-01-02 12:00:00 UTC';
        self::assertEquals('pull', $object->callMethod('pushPullSkip', [$CRM_Meeting, $Google_Event]));

        // Test with newer, deleted meeting. Should return 'push_delete'
        $CRM_Meeting->deleted = '1';
        $CRM_Meeting->fetched_row['date_modified'] = '2018-01-03 12:00:00';
        $CRM_Meeting->fetched_row['gsync_lastsync'] = strtotime('2018-01-02 12:00:00 UTC');
        $Google_Event->updated = '2018-01-01 12:00:00 UTC';
        self::assertEquals('push_delete', $object->callMethod('pushPullSkip', [$CRM_Meeting, $Google_Event]));
        $CRM_Meeting->deleted = '0';

        // Test with newer, deleted Google Event Should return 'pull_delete'
        $Google_Event->status = 'cancelled';
        $CRM_Meeting->fetched_row['date_modified'] = '2018-01-01 12:00:00';
        $CRM_Meeting->fetched_row['gsync_lastsync'] = strtotime('2018-01-01 13:00:00 UTC');
        $Google_Event->updated = '2018-01-02 12:00:00 UTC';
        self::assertEquals('pull_delete', $object->callMethod('pushPullSkip', [$CRM_Meeting, $Google_Event]));
        $Google_Event->status = '';

        // Set records to active
        $CRM_Meeting->deleted = '0';
        $Google_Event->status = '';

        // Test with synced event
        $CRM_Meeting->fetched_row['date_modified'] = '2018-01-01 12:00:00';
        $CRM_Meeting->fetched_row['gsync_lastsync'] = strtotime('2018-01-01 13:00:00 UTC');
        $Google_Event->updated = '2018-01-01 12:00:00 UTC';
        self::assertEquals('skip', $object->callMethod('pushPullSkip', [$CRM_Meeting, $Google_Event]));
    }

    /**
     *
     *
     */
    public function testSetSyncUsers(): void
    {
        $cnt = 0;
        // base64 encoded of {"web":"test"}
        $json = 'eyJ3ZWIiOiJ0ZXN0In0=';
        $query = "SELECT COUNT(*) AS cnt FROM users";
        $db = DBManagerFactory::getInstance();
        $count = $db->getOne($query);

        self::assertNotFalse($count);

        $user1 = BeanFactory::getBean('Users');
        $user1->last_name = 'UNIT_TESTS1';
        $user1->user_name = 'UNIT_TESTS1';
        $user1->full_name = 'UNIT_TESTS1';
        $user1->save(false);
        $validator = new SuiteValidator();
        self::assertTrue($validator->isValidId($id1 = $user1->id));
        $user1->setPreference('GoogleApiToken', $json, false, 'GoogleSync');
        $user1->setPreference('syncGCal', 1, 0, 'GoogleSync');
        $user1->savePreferencesToDB();

        $cnt = $db->getOne($query);
        self::assertEquals(++$count, $cnt );

        $user2 = BeanFactory::getBean('Users');
        $user2->last_name = 'UNIT_TESTS2';
        $user2->user_name = 'UNIT_TESTS2';
        $user2->full_name = 'UNIT_TESTS2';
        $user2->save(false);
        self::assertTrue($validator->isValidId($id2 = $user2->id));
        $user2->setPreference('GoogleApiToken', $json, false, 'GoogleSync');
        $user2->setPreference('syncGCal', 1, 0, 'GoogleSync');
        $user2->savePreferencesToDB();

        self::assertNotSame($id1, $id2);

        $cnt = $db->getOne($query);
        self::assertEquals(++$count, $cnt);

        $object = new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}'));
        $tempData = [];
        $countOfSyncUsers = $object->callMethod('setSyncUsers', [&$tempData]);
        
        self::assertEquals($count, $tempData['founds']);
        self::assertGreaterThanOrEqual(2, $countOfSyncUsers);
    }

    /**
     *
     *
     */
    public function testSyncAllUsers(): void
    {
        $ret = (new GoogleSyncMock($this->getFakeSugarConfig('{"web":"test"}')))->syncAllUsers();
        // TODO: it needs more test
        self::assertEquals(true, $ret);
    }

    //GoogleSyncHelper.php

    /**
     *
     *
     */
    public function testSingleEventAction(): void
    {
        $ret1 = (new GoogleSyncHelper)->singleEventAction(null, null);
        self::assertEquals(false, $ret1);
        // The rest of this method is tested by testPushPullSkip
    }

    /**
     *
     *
     */
    public function testGetTimeStrings(): void
    {
        $helper = new GoogleSyncHelper;

        // Create SuiteCRM Meeting Object
        $CRM_Meeting = BeanFactory::getBean('Meetings');

        $CRM_Meeting->id = create_guid();
        $CRM_Meeting->name = 'Unit Test Event';
        $CRM_Meeting->description = 'Unit Test Event';

        // Create Google Event Object
        $Google_Event = new Google\Service\Calendar\Event();

        $Google_Event->setSummary('Unit Test Event');
        $Google_Event->setDescription('Unit Test Event');
        $Google_Event->updated = '2018-01-01 12:00:00 UTC';

        // The event needs a start time method to pass
        $startDateTime = new Google\Service\Calendar\EventDateTime;
        $startDateTime->setDateTime(date(DATE_ATOM, strtotime('2018-01-01 12:00:00 UTC')));
        $Google_Event->setStart($startDateTime);

        $CRM_Meeting->fetched_row['date_modified'] = '2018-01-01 12:00:00';
        $CRM_Meeting->fetched_row['gsync_lastsync'] = strtotime('2018-01-01 12:00:00 UTC');

        $ret = $helper->getTimeStrings($CRM_Meeting, $Google_Event);

        self::assertEquals('1514808000', $ret['gModified']);
        self::assertEquals('1514808000', $ret['sModified']);
        self::assertEquals('1514808000', $ret['lastSync']);
    }

    /**
     *
     *
     */
    public function testWipeLocalSyncData(): void
    {
        // Create a User
        $user = BeanFactory::getBean('Users');
        $user->last_name = 'UNIT_TESTS';
        $user->user_name = 'UNIT_TESTS';
        $user->save();

        // Create a meeting with gsync fields populated and save it to the DB for testing.
        $meeting1 = BeanFactory::getBean('Meetings');
        $meeting1->name = 'UNIT_TEST_1';
        $meeting1->assigned_user_id = $user->id;
        $meeting1->status = 'Not Held';
        $meeting1->type = 'Sugar';
        $meeting1->description = 'test description';
        $meeting1->duration_hours = 1;
        $meeting1->duration_minutes = 1;
        $meeting1->date_start = '2016-02-11 17:30:00';
        $meeting1->date_end = '2016-02-11 17:30:00';
        $meeting1->gsync_id = 'GSYNCID';
        $meeting1->gsync_lastsync = '1234567890';
        $meeting_id = $meeting1->save();

        // Get a DB object
        $db = DBManagerFactory::getInstance();

        // Make sure the values we set are saved in the DB
        $query = "SELECT gsync_id, gsync_lastsync FROM meetings WHERE id = {$db->quoted($meeting_id)}";
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        self::assertEquals('GSYNCID', $row['gsync_id']);
        self::assertEquals('1234567890', $row['gsync_lastsync']);

        // Call the tested function to wipe the gsync data
        $helper = new GoogleSyncHelper;
        $helper->wipeLocalSyncData($user->id);

        // Check the raw DB values
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        self::assertEquals('', $row['gsync_id']);
        self::assertEquals('', $row['gsync_lastsync']);
    }
}
