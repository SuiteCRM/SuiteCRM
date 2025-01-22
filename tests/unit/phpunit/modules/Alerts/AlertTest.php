<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AlertTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAlert(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $alert = BeanFactory::newBean('Alerts');
        self::assertInstanceOf('Alert', $alert);
        self::assertInstanceOf('Basic', $alert);
        self::assertInstanceOf('SugarBean', $alert);

        self::assertEquals('Alerts', $alert->module_dir);
        self::assertEquals('Alert', $alert->object_name);
        self::assertEquals('alerts', $alert->table_name);
        self::assertEquals(true, $alert->new_schema);
        self::assertEquals(true, $alert->disable_row_level_security);
        self::assertEquals(false, $alert->importable);
    }

    public function testbean_implements(): void
    {
        $alert = BeanFactory::newBean('Alerts');

        self::assertEquals(false, $alert->bean_implements('')); //test with empty value
        self::assertEquals(false, $alert->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $alert->bean_implements('ACL')); //test with valid value
    }
}
