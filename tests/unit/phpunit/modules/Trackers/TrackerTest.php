<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class TrackerTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testTracker(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $tracker = BeanFactory::newBean('Trackers');

        self::assertInstanceOf('Tracker', $tracker);
        self::assertInstanceOf('SugarBean', $tracker);

        self::assertEquals('tracker', $tracker->table_name);
        self::assertEquals('Trackers', $tracker->module_dir);
        self::assertEquals('Tracker', $tracker->object_name);

        self::assertEquals(true, $tracker->disable_var_defs);

        self::assertEquals('Tracker', $tracker->acltype);
        self::assertEquals('Trackers', $tracker->acl_category);
        self::assertEquals(true, $tracker->disable_custom_fields);
    }

    public function testget_recently_viewed(): void
    {
        $result = BeanFactory::newBean('Trackers')->get_recently_viewed(1);

        self::assertInstanceOf('BreadCrumbStack', $_SESSION['breadCrumbs']);
        self::assertIsArray($result);
    }

    public function testmakeInvisibleForAll(): void
    {
        $tracker = BeanFactory::newBean('Trackers');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $tracker->makeInvisibleForAll(1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testbean_implements(): void
    {
        $tracker = BeanFactory::newBean('Trackers');

        self::assertEquals(false, $tracker->bean_implements('')); //test with blank value
        self::assertEquals(false, $tracker->bean_implements('test')); //test with invalid value
        self::assertEquals(false, $tracker->bean_implements('ACL')); //test with valid value
    }

    public function testlogPage(): void
    {
        self::markTestIncomplete('Test parameters and local variables are not set');

        //test without setting headerDisplayed
        Tracker::logPage();
        self::assertEquals(null, $_SESSION['lpage']);

        //test with headerDisplayed set
        $GLOBALS['app']->headerDisplayed = 1;
        Tracker::logPage();
        self::assertEquals(time(), $_SESSION['lpage']);

        //$this->assertEquals(time(), null);
    }
}
