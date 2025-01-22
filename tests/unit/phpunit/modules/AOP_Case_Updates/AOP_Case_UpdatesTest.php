<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOP_Case_UpdatesTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOP_Case_Updates(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $aopCaseUpdates = BeanFactory::newBean('AOP_Case_Updates');
        self::assertInstanceOf('AOP_Case_Updates', $aopCaseUpdates);
        self::assertInstanceOf('Basic', $aopCaseUpdates);
        self::assertInstanceOf('SugarBean', $aopCaseUpdates);

        self::assertEquals('AOP_Case_Updates', $aopCaseUpdates->module_dir);
        self::assertEquals('AOP_Case_Updates', $aopCaseUpdates->object_name);
        self::assertEquals('aop_case_updates', $aopCaseUpdates->table_name);
        self::assertEquals(true, $aopCaseUpdates->new_schema);
        self::assertEquals(true, $aopCaseUpdates->disable_row_level_security);
        self::assertEquals(false, $aopCaseUpdates->importable);
        self::assertEquals(false, $aopCaseUpdates->tracker_visibility);
    }

    public function testsave(): void
    {
        self::markTestIncomplete('environment dependency');

        $aopCaseUpdates = BeanFactory::newBean('AOP_Case_Updates');
        $aopCaseUpdates->name = 'test name';
        $aopCaseUpdates->description = 'test description';
        $aopCaseUpdates->case_id = 'test case id';
        $aopCaseUpdates->contact_id = 'test id';
        $aopCaseUpdates->internal = 1;
        $aopCaseUpdates->save();

        //test for record ID to verify that record is saved
        self::assertEquals(36, strlen((string) $aopCaseUpdates->id));

        //mark the record as deleted for cleanup
        $aopCaseUpdates->mark_deleted($aopCaseUpdates->id);
    }

    public function testgetCase(): void
    {
        //execute the method and verify that it returns a Case object
        $result = BeanFactory::newBean('AOP_Case_Updates')->getCase();

        self::assertInstanceOf('aCase', $result);
    }

    public function testgetContacts(): void
    {
        //execute the method and verify that it returns an array
        $result = BeanFactory::newBean('AOP_Case_Updates')->getContacts();
        self::assertIsArray($result);
    }

    public function testgetUpdateContact(): void
    {
        $aopCaseUpdates = BeanFactory::newBean('AOP_Case_Updates');

        //execute the method without contact_id and verify that it returns null
        $result = $aopCaseUpdates->getUpdateContact();
        self::assertEquals(null, $result);

        //execute the method without contact_id and verify that it returns false
        $aopCaseUpdates->contact_id = 1;
        $result = $aopCaseUpdates->getUpdateContact();
        self::assertEquals(false, $result);
    }

    public function testgetUser(): void
    {
        //execute the method and verify that it returns an instance of User
        $result = BeanFactory::newBean('AOP_Case_Updates')->getUser();
        self::assertInstanceOf('User', $result);
    }

    public function testgetUpdateUser(): void
    {
        //execute the method and verify that it returns an instance of User
        $result = BeanFactory::newBean('AOP_Case_Updates')->getUpdateUser();
        self::assertInstanceOf('User', $result);
    }
}
