<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class AOS_ContractsTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testAOS_Contracts(): void
    {
        // Execute the constructor and check for the Object type and attributes
        $aosContracts = BeanFactory::newBean('AOS_Contracts');
        self::assertInstanceOf('AOS_Contracts', $aosContracts);
        self::assertInstanceOf('Basic', $aosContracts);
        self::assertInstanceOf('SugarBean', $aosContracts);

        self::assertEquals('AOS_Contracts', $aosContracts->module_dir);
        self::assertEquals('AOS_Contracts', $aosContracts->object_name);
        self::assertEquals('aos_contracts', $aosContracts->table_name);
        self::assertEquals(true, $aosContracts->new_schema);
        self::assertEquals(true, $aosContracts->disable_row_level_security);
        self::assertEquals(true, $aosContracts->importable);

        self::assertTrue(isset($aosContracts->renewal_reminder_date));
    }

    public function testsaveAndDelete(): void
    {
        $aosContracts = BeanFactory::newBean('AOS_Contracts');
        $aosContracts->name = 'test';

        $aosContracts->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($aosContracts->id));
        self::assertEquals(36, strlen((string) $aosContracts->id));

        //mark the record as deleted and verify that this record cannot be retrieved anymore.
        $aosContracts->mark_deleted($aosContracts->id);
        $result = $aosContracts->retrieve($aosContracts->id);
        self::assertEquals(null, $result);
    }

    public function testCreateReminderAndCreateLinkAndDeleteCall(): void
    {
        $call = new call();

        $aosContracts = BeanFactory::newBean('AOS_Contracts');
        $aosContracts->name = 'test';

        //test createReminder()
        $aosContracts->createReminder();

        //verify record ID to check that record is saved
        self::assertTrue(isset($aosContracts->call_id));
        self::assertEquals(36, strlen((string) $aosContracts->call_id));

        //verify the parent_type value set by createReminder()
        $call->retrieve($aosContracts->call_id);
        self::assertEquals('AOS_Contracts', $call->parent_type);

        //test createLink() and verify the parent_type value
        $aosContracts->createLink();
        $call->retrieve($aosContracts->call_id);
        self::assertEquals('Accounts', $call->parent_type);

        //delete the call and verify that this record cannot be retrieved anymore.
        $aosContracts->deleteCall();
        $result = $call->retrieve($aosContracts->call_id);
        self::assertEquals(null, $result);
    }
}
