<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class RelationshipTest extends SuitePHPUnitFrameworkTestCase
{
    public function testRelationship(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $relationship = BeanFactory::newBean('Relationships');

        self::assertInstanceOf('Relationship', $relationship);
        self::assertInstanceOf('SugarBean', $relationship);

        self::assertEquals('Relationships', $relationship->module_dir);
        self::assertEquals('Relationship', $relationship->object_name);
        self::assertEquals('relationships', $relationship->table_name);

        self::assertEquals(true, $relationship->new_schema);
    }

    public function testis_self_referencing(): void
    {
        //test without setting any attributes
        $relationship = BeanFactory::newBean('Relationships');

        $result = $relationship->is_self_referencing();
        self::assertEquals(true, $result);

        //test with attributes set to different values
        $relationship = BeanFactory::newBean('Relationships');

        $relationship->lhs_table = 'lhs_table';
        $relationship->rhs_table = 'rhs_table';
        $relationship->lhs_key = 'lhs_key';
        $relationship->rhs_key = 'rhs_key';

        $result = $relationship->is_self_referencing();
        self::assertEquals(false, $result);

        //test with attributes set to same values
        $relationship = BeanFactory::newBean('Relationships');

        $relationship->lhs_table = 'table';
        $relationship->rhs_table = 'table';
        $relationship->lhs_key = 'key';
        $relationship->rhs_key = 'key';

        $result = $relationship->is_self_referencing();
        self::assertEquals(true, $result);
    }

    public function testexists(): void
    {
        // Unset and reconnect Db to resolve mysqli fetch exception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $relationship = BeanFactory::newBean('Relationships');

        //test with invalid relationship
        $result = $relationship::exists('test_test', $db);
        self::assertEquals(false, $result);

        //test with valid relationship
        $result = $relationship::exists('roles_users', $db);
        self::assertEquals(true, $result);
    }

    public function testdelete(): void
    {
        $db = DBManagerFactory::getInstance();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            Relationship::delete('test_test', $db);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testget_other_module(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $relationship = BeanFactory::newBean('Relationships');

        //test with invalid relationship
        $result = $relationship->get_other_module('test_test', 'test', $db);
        self::assertEquals(false, $result);

        //test with valid relationship
        $result = $relationship->get_other_module('roles_users', 'Roles', $db);
        self::assertEquals('Users', $result);
    }

    public function testRetrieveBySides(): void
    {
        // Unset and reconnect Db to resolve mysqli fetch exception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $relationship = BeanFactory::newBean('Relationships');

        // Test with invalid relationship
        $result = $relationship->retrieve_by_sides('test1', 'test2', $db);
        self::assertEquals(null, $result);

        // Test with valid relationship
        $result = $relationship->retrieve_by_sides('Roles', 'Users', $db);

        self::assertEquals('Users', $result['rhs_module']);
        self::assertEquals('Roles', $result['lhs_module']);

        self::assertEquals('id', $result['rhs_key']);
        self::assertEquals('id', $result['lhs_key']);

        self::assertEquals('many-to-many', $result['relationship_type']);
    }

    public function testRetrieveByModules(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $relationship = BeanFactory::newBean('Relationships');

        // Test with invalid relationship
        $result = $relationship::retrieve_by_modules('test1', 'test2', $db);
        self::assertEquals(null, $result);

        // Test with valid relationship but incorrect type
        $result = $relationship::retrieve_by_modules('Roles', 'Users', $db, 'one-to-many');
        self::assertEquals(null, $result);

        // Test with valid relationship and valid type
        $result = $relationship::retrieve_by_modules('Roles', 'Users', $db, 'many-to-many');
        self::assertEquals('roles_users', $result);
    }

    public function testretrieve_by_name(): void
    {
        $relationship = BeanFactory::newBean('Relationships');

        //test with invalid relationship
        $result = $relationship->retrieve_by_name('test_test');
        self::assertEquals(false, $result);

        //test with invalid relationship
        unset($result);
        $result = $relationship->retrieve_by_name('roles_users');
        self::assertEquals(null, $result);

        self::assertEquals('Users', $relationship->rhs_module);
        self::assertEquals('Roles', $relationship->lhs_module);

        self::assertEquals('id', $relationship->rhs_key);
        self::assertEquals('id', $relationship->lhs_key);

        self::assertEquals('many-to-many', $relationship->relationship_type);
    }

    public function testload_relationship_meta(): void
    {
        $relationship = BeanFactory::newBean('Relationships');

        $relationship->load_relationship_meta();
        self::assertTrue(isset($GLOBALS['relationships']));
    }

    public function testbuild_relationship_cache(): void
    {
        $relationship = BeanFactory::newBean('Relationships');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $relationship->build_relationship_cache();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcache_file_dir(): void
    {
        $result = Relationship::cache_file_dir();
        self::assertEquals('cache/modules/Relationships', $result);
    }

    public function testcache_file_name_only(): void
    {
        $result = Relationship::cache_file_name_only();
        self::assertEquals('relationships.cache.php', $result);
    }

    public function testdelete_cache(): void
    {
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            Relationship::delete_cache();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testtrace_relationship_module(): void
    {
        $result = BeanFactory::newBean('Relationships')->trace_relationship_module('Roles', 'Users');
        self::assertInstanceOf('User', $result);
    }
}
