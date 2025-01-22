<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ProjectTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testProject(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $project = BeanFactory::newBean('Project');

        self::assertInstanceOf('Project', $project);
        self::assertInstanceOf('SugarBean', $project);


        self::assertEquals('project', $project->table_name);
        self::assertEquals('Project', $project->module_dir);
        self::assertEquals('Project', $project->object_name);

        self::assertEquals(true, $project->new_schema);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $project = BeanFactory::newBean('Project');

        //test without setting assigned_user_id
        $project->fill_in_additional_detail_fields();
        self::assertEquals("", $project->assigned_user_name);

        //test with assigned_user_id set
        $project->assigned_user_id = 1;
        $project->fill_in_additional_detail_fields();
        self::assertEquals("Administrator", $project->assigned_user_name);
    }

    public function testfill_in_additional_list_fields(): void
    {
        $project = BeanFactory::newBean('Project');

        //test without setting assigned_user_id
        $project->fill_in_additional_list_fields();
        self::assertEquals("", $project->assigned_user_name);

        //test with assigned_user_id set
        $project->assigned_user_id = 1;
        $project->fill_in_additional_list_fields();
        self::assertEquals("Administrator", $project->assigned_user_name);
    }

    public function testsave_relationship_changes(): void
    {
        $project = BeanFactory::newBean('Project');

        $project->id =1;
        $_REQUEST['relate_id'] = 2;
        $_REQUEST['relate_to'] = "contacts";

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $project->save_relationship_changes(true);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_summary_text(): void
    {
        $project = BeanFactory::newBean('Project');

        //test without setting name
        self::assertEquals(null, $project->get_summary_text());

        //test with name set
        $project->name = "test";
        self::assertEquals('test', $project->get_summary_text());
    }

    public function testbuild_generic_where_clause(): void
    {
        $project = BeanFactory::newBean('Project');

        //test with empty string params
        $expected = "project.name LIKE '%%'";
        $actual = $project->build_generic_where_clause('');
        self::assertSame($expected, $actual);


        //test with valid string params
        $expected = "project.name LIKE '%test%'";
        $actual = $project->build_generic_where_clause('test');
        self::assertSame($expected, $actual);
    }

    /**
     * @todo: NEEDS FIXING!
     */
    public function testget_list_view_data(): void
    {
        /*
        $project = BeanFactory::newBean('Project');

        $project->user_name = "tes user";
        $project->assigned_user_name = "test assigned user";

        $expected = array (
            'DELETED' => '0',
            'ASSIGNED_USER_NAME' => 'test assigned user',
            'USER_NAME' => 'tes user'
        );

        $actual = $project->get_list_view_data();

        $this->assertSame($expected, $actual);
        */
        self::assertTrue(true, "NEEDS FIXING!");
    }

    public function testbean_implements(): void
    {
        $project = BeanFactory::newBean('Project');

        self::assertEquals(false, $project->bean_implements('')); //test with blank value
        self::assertEquals(false, $project->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $project->bean_implements('ACL')); //test with valid value
    }

    public function testgetAllProjectTasks(): void
    {
        $project = BeanFactory::newBean('Project');

        $project->id = 1;
        $result = $project->getAllProjectTasks();
        self::assertIsArray($result);
    }
}
