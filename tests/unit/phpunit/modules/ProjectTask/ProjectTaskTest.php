<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ProjectTaskTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testcreate_export_query(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        //test with empty string params
        $expected = "SELECT
				project_task.*,
                users.user_name as assigned_user_name  FROM project_task LEFT JOIN project ON project_task.project_id=project.id AND project.deleted=0  LEFT JOIN users
                   	ON project_task.assigned_user_id=users.id where  project_task.deleted=0 ";
        $actual = $projectTask->create_export_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "SELECT
				project_task.*,
                users.user_name as assigned_user_name  FROM project_task LEFT JOIN project ON project_task.project_id=project.id AND project.deleted=0  LEFT JOIN users
                   	ON project_task.assigned_user_id=users.id where (users.user_name= \"\") AND  project_task.deleted=0  ORDER BY project_task.id";
        $actual = $projectTask->create_export_query('project_task.id', 'users.user_name= ""');
        self::assertSame($expected, $actual);
    }

    public function testProjectTask(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $projectTask = BeanFactory::newBean('ProjectTask');

        self::assertInstanceOf('ProjectTask', $projectTask);
        self::assertInstanceOf('SugarBean', $projectTask);

        self::assertEquals('project_task', $projectTask->table_name);
        self::assertEquals('ProjectTask', $projectTask->module_dir);
        self::assertEquals('ProjectTask', $projectTask->object_name);

        self::assertEquals(true, $projectTask->new_schema);
        self::assertEquals(100, $projectTask->utilization);
    }

    public function testskipParentUpdate(): void
    {
        $reflectionProperty = (new ReflectionClass(ProjectTask::class))->getProperty('_skipParentUpdate');
        $reflectionProperty->setAccessible(true);
        $reflectedValue = $reflectionProperty->getValue(BeanFactory::newBean('ProjectTask'));
        self::assertEquals(false, $reflectedValue);
    }

    public function testsave(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        $projectTask->name = 'test';
        //$projectTask->project_id = "1";
        $projectTask->assigned_user_id = '1';
        $projectTask->description = 'test description';
        $projectTask->parent_task_id = 1;

        $projectTask->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($projectTask->id));
        self::assertEquals(36, strlen((string) $projectTask->id));

        //test _get_depends_on_name method
        $this->_get_depends_on_name($projectTask->id);

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $projectTask->mark_deleted($projectTask->id);
        $result = $projectTask->retrieve($projectTask->id);
        self::assertEquals(null, $result);
    }

    public function _get_depends_on_name($id): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        $result = $projectTask->_get_depends_on_name($id);

        self::assertEquals('1', $projectTask->depends_on_name_owner);
        self::assertEquals('ProjectTask', $projectTask->depends_on_name_mod);
        self::assertEquals('test', $result);
    }

    public function testupdateParentProjectTaskPercentage(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $projectTask->updateParentProjectTaskPercentage();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetProjectTaskParent(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        $projectTask->parent_task_id = 1;
        $result = $projectTask->getProjectTaskParent();
        self::assertEquals(false, $result);
    }

    public function testgetAllSubProjectTasks(): void
    {
        $result = BeanFactory::newBean('ProjectTask')->getAllSubProjectTasks();
        self::assertIsArray($result);
    }

    public function testupdateStatistic(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $projectTask->updateStatistic();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        //test without setting assigned_user_id
        $projectTask->fill_in_additional_detail_fields();
        self::assertEquals('', $projectTask->assigned_user_name);

        //test with assigned_user_id set
        $projectTask->assigned_user_id = 1;
        $projectTask->fill_in_additional_detail_fields();
        self::assertEquals('Administrator', $projectTask->assigned_user_name);
    }

    public function testget_summary_text(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        //test without setting name
        self::assertEquals(null, $projectTask->get_summary_text());

        //test with name set
        $projectTask->name = 'test';
        self::assertEquals('test', $projectTask->get_summary_text());
    }

    public function test_get_project_name(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        //test with a empty string
        $result = $projectTask->_get_project_name('');
        self::assertEquals('', $result);

        //test with a non empty invalid id
        $result = $projectTask->_get_project_name('1');
        self::assertEquals('', $result);
    }

    public function test_get_parent_name(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        //test with a empty string
        $result = $projectTask->_get_parent_name('');
        self::assertEquals('', $result);

        //test with a non empty invalid id
        $result = $projectTask->_get_parent_name('1');
        self::assertEquals('', $result);
    }

    public function testbuild_generic_where_clause(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        //test with empty string params
        $expected = "project_task.name like '%'";
        $actual = $projectTask->build_generic_where_clause('');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = "project_task.name like 'test%'";
        $actual = $projectTask->build_generic_where_clause('test');
        self::assertSame($expected, $actual);
    }

    public function testbean_implements(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        self::assertEquals(false, $projectTask->bean_implements('')); //test with blank value
        self::assertEquals(false, $projectTask->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $projectTask->bean_implements('ACL')); //test with valid value
    }

    public function testlistviewACLHelper(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        $expected = array('MAIN' => 'a', 'PARENT' => 'a', 'PARENT_TASK' => 'a');
        $actual = $projectTask->listviewACLHelper();
        self::assertSame($expected, $actual);
    }

    public function testgetUtilizationDropdown(): void
    {
        $projectTask = BeanFactory::newBean('ProjectTask');

        $expected = "<select name=\"utilization\">\n<OPTION value='0'>none</OPTION>\n<OPTION value='25'>25</OPTION>\n<OPTION value='50'>50</OPTION>\n<OPTION value='75'>75</OPTION>\n<OPTION value='100'>100</OPTION></select>";
        $actual = getUtilizationDropdown($projectTask, 'utilization', '0', 'EditView');
        self::assertSame($expected, $actual);
    }
}
