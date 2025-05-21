<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class EAPMTest extends SuitePHPUnitFrameworkTestCase
{
    public function testEAPM(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $eapm = BeanFactory::newBean('EAPM');
        self::assertInstanceOf('EAPM', $eapm);
        self::assertInstanceOf('Basic', $eapm);
        self::assertInstanceOf('SugarBean', $eapm);

        self::assertEquals('EAPM', $eapm->module_dir);
        self::assertEquals('EAPM', $eapm->object_name);
        self::assertEquals('eapm', $eapm->table_name);
        self::assertEquals(true, $eapm->new_schema);
        self::assertEquals(false, $eapm->importable);
        self::assertEquals(false, $eapm->validated);
        self::assertEquals(true, $eapm->disable_row_level_security);
    }

    public function testbean_implements(): void
    {
        $eapm = BeanFactory::newBean('EAPM');
        self::assertEquals(false, $eapm->bean_implements('')); //test with blank value
        self::assertEquals(false, $eapm->bean_implements('test')); //test with invalid value
        self::assertEquals(true, $eapm->bean_implements('ACL')); //test with valid value
    }

    public function testGetLoginInfo(): void
    {
        $eapm = BeanFactory::newBean('EAPM');

        // Test with default value/false
        $result = $eapm::getLoginInfo('');
        self::assertEquals(null, $result);

        $result = $eapm::getLoginInfo('', true);
        self::assertEquals(null, $result);
    }

    public function testcreate_new_list_query(): void
    {
        $eapm = BeanFactory::newBean('EAPM');

        //test with empty string params
        $expected = " SELECT  eapm.*  , jt0.user_name modified_by_name , jt0.created_by modified_by_name_owner  , 'Users' modified_by_name_mod , jt1.user_name created_by_name , jt1.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt2.user_name assigned_user_name , jt2.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod FROM eapm   LEFT JOIN  users jt0 ON eapm.modified_user_id=jt0.id AND jt0.deleted=0\n\n AND jt0.deleted=0  LEFT JOIN  users jt1 ON eapm.created_by=jt1.id AND jt1.deleted=0\n\n AND jt1.deleted=0  LEFT JOIN  users jt2 ON eapm.assigned_user_id=jt2.id AND jt2.deleted=0\n\n AND jt2.deleted=0 where ( eapm.assigned_user_id ='' ) AND eapm.deleted=0";
        $actual = $eapm->create_new_list_query('', '');
        self::assertSame($expected, $actual);

        //test with valid string params
        $expected = " SELECT  eapm.*  , jt0.user_name modified_by_name , jt0.created_by modified_by_name_owner  , 'Users' modified_by_name_mod , jt1.user_name created_by_name , jt1.created_by created_by_name_owner  , 'Users' created_by_name_mod , jt2.user_name assigned_user_name , jt2.created_by assigned_user_name_owner  , 'Users' assigned_user_name_mod FROM eapm   LEFT JOIN  users jt0 ON eapm.modified_user_id=jt0.id AND jt0.deleted=0\n\n AND jt0.deleted=0  LEFT JOIN  users jt1 ON eapm.created_by=jt1.id AND jt1.deleted=0\n\n AND jt1.deleted=0  LEFT JOIN  users jt2 ON eapm.assigned_user_id=jt2.id AND jt2.deleted=0\n\n AND jt2.deleted=0 where (eapm.name=\"\" AND  eapm.assigned_user_id ='' ) AND eapm.deleted=0";
        $actual = $eapm->create_new_list_query('eapm.id', 'eapm.name=""');
        self::assertSame($expected, $actual);
    }

    public function testsaveAndMarkDeletedAndValidated(): void
    {
        self::markTestIncomplete('eapm table fails');
        $eapm = BeanFactory::newBean('EAPM');

        $eapm->name = 'test';
        $eapm->url = 'test_url';
        $eapm->application = 'webex';

        //test validated method initially
        self::assertEquals(false, $eapm->validated());

        $eapm->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($eapm->id));
        self::assertEquals(36, strlen((string) $eapm->id));

        //test validated method finally after save
        self::assertEquals(null, $eapm->validated());

        //retrieve back to test if validated attribute is updated in db
        $eapmValidated = $eapm->retrieve($eapm->id);
        //$this->assertEquals(1, $eapmValidated->validated);
        self::markTestSkipped('Validated column never gets updated in Db ');

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $eapm->mark_deleted($eapm->id);
        $result = $eapm->retrieve($eapm->id);
        self::assertEquals(null, $result);
    }

    public function testfill_in_additional_detail_fields(): void
    {
        $eapm = BeanFactory::newBean('EAPM');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $eapm->fill_in_additional_detail_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testfill_in_additional_list_fields(): void
    {
        $eapm = BeanFactory::newBean('EAPM');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $eapm->fill_in_additional_list_fields();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testsave_cleanup(): void
    {
        $eapm = BeanFactory::newBean('EAPM');

        //execute the method and verify attributes are set accordingly
        $eapm->save_cleanup();

        self::assertEquals('', $eapm->oauth_token);
        self::assertEquals('', $eapm->oauth_secret);
        self::assertEquals('', $eapm->api_data);
    }

    public function testdelete_user_accounts(): void
    {
        $eapm = BeanFactory::newBean('EAPM');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $eapm->delete_user_accounts(1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetEAPMExternalApiDropDown(): void
    {
        $result = getEAPMExternalApiDropDown();
        self::assertEquals(array('' => ''), $result);
    }
}
