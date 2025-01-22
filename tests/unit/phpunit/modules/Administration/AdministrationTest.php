<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AdministrationTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAdministration(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $admin = BeanFactory::newBean('Administration');
        self::assertInstanceOf('Administration', $admin);
        self::assertInstanceOf('SugarBean', $admin);

        self::assertEquals('Administration', $admin->module_dir);
        self::assertEquals('Administration', $admin->object_name);
        self::assertEquals('config', $admin->table_name);
        self::assertEquals(true, $admin->new_schema);
        self::assertEquals(true, $admin->disable_custom_fields);
    }

    public function testretrieveSettings(): void
    {
        $admin = BeanFactory::newBean('Administration');

        //execute with default parameters and test if it returns object itself
        $result = $admin->retrieveSettings();
        self::assertInstanceOf('Administration', $result);
        self::assertSame($admin, $result);

        //execute with a invalid category and test if it returns object itself.
        $result = $admin->retrieveSettings('test');
        self::assertInstanceOf('Administration', $result);
        self::assertSame($admin, $result);
        self::assertEquals(true, $admin->settings['test']);

        //execute with a valid category and test if it returns object itself.
        $result = $admin->retrieveSettings('notify');
        self::assertInstanceOf('Administration', $result);
        self::assertSame($admin, $result);

        //execute with a valid category and clean=true and test if it returns object itself.
        $admin = BeanFactory::newBean('Administration');
        $result = $admin->retrieveSettings('notify', true);
        self::assertInstanceOf('Administration', $result);
        self::assertSame($admin, $result);
    }

    public function testsaveConfig(): void
    {
        self::markTestIncomplete('environment dependency');

        // test
        $admin = BeanFactory::newBean('Administration');

        $_POST['proxy_test'] = 'test value';

        //execute the method and verify that it sets the correct config key
        $admin->saveConfig();
        $actual = $admin->settings['proxy_test'];
        self::assertEquals('test value', $actual);
    }

    public function testsaveSetting(): void
    {
        self::markTestIncomplete('environment dependency');

        // test
        $admin = BeanFactory::newBean('Administration');

        //execute the method and verify that sets the correct config key
        $result = $admin->saveSetting('category', 'key', 'test value');
        $admin->retrieveSettings('category');
        $actual = $admin->settings['category_key'];
        self::assertEquals('test value', $actual);
    }

    public function testget_config_prefix(): void
    {
        $admin = BeanFactory::newBean('Administration');

        //test with empty string
        $expected = array(false, false);
        $actual = $admin->get_config_prefix('');
        self::assertSame($expected, $actual);

        //test with a valid string
        $expected = array('category', 'test');
        $actual = $admin->get_config_prefix('category_test');
        self::assertSame($expected, $actual);
    }
}
