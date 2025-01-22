<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class GroupTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        $current_user = BeanFactory::newBean('Users');
        get_sugar_config_defaults();
    }

    public function testGroup(): void
    {
        //execute the constructor and check for the Object type and attributes
        $group = BeanFactory::newBean('Groups');
        self::assertInstanceOf('Group', $group);
        self::assertInstanceOf('User', $group);
        self::assertInstanceOf('SugarBean', $group);

        self::assertEquals('Group', $group->status);
        self::assertEquals('', $group->password);
        self::assertEquals(false, $group->importable);
    }

    public function testmark_deleted(): void
    {
        self::markTestIncomplete('environment dependency (php7: Incorrect state hash: Hash doesn\'t match at key "database::users".)');

        $group = BeanFactory::newBean('Groups');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $group->mark_deleted('');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcreate_export_query(): void
    {
        $group = BeanFactory::newBean('Groups');

        //test with empty string params
        $expected = 'SELECT users.* FROM users  WHERE  users.deleted = 0 ORDER BY users.user_name';
        $actual = $group->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = 'SELECT users.* FROM users  WHERE users.user_name="" AND  users.deleted = 0 ORDER BY users.id';
        $actual = $group->create_export_query('users.id', 'users.user_name=""');
        self::assertSame($expected, $actual);
    }
}
