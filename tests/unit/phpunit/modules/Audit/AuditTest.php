<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'modules/Audit/Audit.php';
class AuditTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAudit(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $audit = BeanFactory::newBean('Audit');
        self::assertInstanceOf('Audit', $audit);
        self::assertInstanceOf('SugarBean', $audit);
        self::assertEquals('Audit', $audit->module_dir);
        self::assertEquals('Audit', $audit->object_name);
    }

    public function testget_summary_text(): void
    {
        $audit = BeanFactory::newBean('Audit');

        //test without setting name
        self::assertEquals(null, $audit->get_summary_text());

        //test with name set
        $audit->name = 'test';
        self::assertEquals('test', $audit->get_summary_text());
    }
    public function testget_audit_list(): void
    {
        global $focus;
        $focus = BeanFactory::newBean('Accounts'); //use audit enabbled module object

        //execute the method and verify that it returns an array
        $result = BeanFactory::newBean('Audit')->get_audit_list();
        self::assertIsArray($result);
    }

    public function testgetAssociatedFieldName(): void
    {
        global $focus;
        $focus = BeanFactory::newBean('Accounts'); //use audit enabbled module object

        $audit = BeanFactory::newBean('Audit');

        //test with name field
        $result = $audit->getAssociatedFieldName('name', '1');
        self::assertEquals('1', $result);

        //test with parent_id field
        $result = $audit->getAssociatedFieldName('parent_id', '1');
        self::assertEquals(null, $result);
    }
}
