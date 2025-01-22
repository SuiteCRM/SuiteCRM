<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class SecurityGroupTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }
    public function testSecurityGroup(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $securityGroup = BeanFactory::newBean('SecurityGroups');

        self::assertInstanceOf('SecurityGroup', $securityGroup);
        self::assertInstanceOf('Basic', $securityGroup);
        self::assertInstanceOf('SugarBean', $securityGroup);

        self::assertEquals('securitygroups', $securityGroup->table_name);
        self::assertEquals('SecurityGroups', $securityGroup->module_dir);
        self::assertEquals('SecurityGroup', $securityGroup->object_name);
    }

    public function testgetGroupWhere(): void
    {
        $securityGroup = BeanFactory::newBean('SecurityGroups');

        //test with securitygroups module
        $expected = " securitygroups.id in (
                select secg.id from securitygroups secg
                inner join securitygroups_users secu on secg.id = secu.securitygroup_id and secu.deleted = 0
                    and secu.user_id = '1'
                where secg.deleted = 0
            )";
        $actual = $securityGroup::getGroupWhere('securitygroups', 'SecurityGroups', 1);
        self::assertSame($expected, $actual);

        //test with //test with securitygroups module module
        $table_name = 'users';
        $module = 'Users';
        $user_id = 1;
        $expected = " EXISTS (SELECT  1
                  FROM    securitygroups secg
                          INNER JOIN securitygroups_users secu
                            ON secg.id = secu.securitygroup_id
                               AND secu.deleted = 0
                               AND secu.user_id = '$user_id'
                          INNER JOIN securitygroups_records secr
                            ON secg.id = secr.securitygroup_id
                               AND secr.deleted = 0
                               AND secr.module = '$module'
                       WHERE   secr.record_id = ".$table_name.".id
                               AND secg.deleted = 0) ";
        $actual = $securityGroup::getGroupWhere($table_name, $module, $user_id);
        self::assertSame($expected, $actual);
    }

    public function testgetGroupUsersWhere(): void
    {
        $securityGroup = BeanFactory::newBean('SecurityGroups');

        $expected = " users.id in (
            select sec.user_id from securitygroups_users sec
            inner join securitygroups_users secu on sec.securitygroup_id = secu.securitygroup_id and secu.deleted = 0
                and secu.user_id = '1'
            where sec.deleted = 0
        )";
        $actual = $securityGroup::getGroupUsersWhere(1);

        self::assertSame($expected, $actual);
    }

    public function testgetGroupJoin(): void
    {
        $securityGroup = BeanFactory::newBean('SecurityGroups');

        //test with securitygroups module
        $expected = " LEFT JOIN (select distinct secg.id from securitygroups secg
    inner join securitygroups_users secu on secg.id = secu.securitygroup_id and secu.deleted = 0
            and secu.user_id = '1'
    where secg.deleted = 0
) securitygroup_join on securitygroup_join.id = securitygroups.id ";
        $actual = $securityGroup::getGroupJoin('securitygroups', 'SecurityGroups', 1);
        self::assertSame($expected, $actual);

        //test with //test with securitygroups module
        $expected = " LEFT JOIN (select distinct secr.record_id as id from securitygroups secg
    inner join securitygroups_users secu on secg.id = secu.securitygroup_id and secu.deleted = 0
            and secu.user_id = '1'
    inner join securitygroups_records secr on secg.id = secr.securitygroup_id and secr.deleted = 0
             and secr.module = 'Users'
    where secg.deleted = 0
) securitygroup_join on securitygroup_join.id = users.id ";
        $actual = $securityGroup::getGroupJoin('users', 'Users', 1);
        self::assertSame($expected, $actual);
    }

    public function testgetGroupUsersJoin(): void
    {
        $securityGroup = BeanFactory::newBean('SecurityGroups');

        $expected = " LEFT JOIN (
            select distinct sec.user_id as id from securitygroups_users sec
            inner join securitygroups_users secu on sec.securitygroup_id = secu.securitygroup_id and secu.deleted = 0
                and secu.user_id = '1'
            where sec.deleted = 0
        ) securitygroup_join on securitygroup_join.id = users.id ";
        $actual = $securityGroup::getGroupUsersJoin(1);
        self::assertSame($expected, $actual);
    }

    public function testgroupHasAccess(): void
    {
        //test for listview
        $result = SecurityGroup::groupHasAccess('', '[SELECT_ID_LIST]');
        self::assertEquals(true, $result);

        //test with invalid values
        $result = SecurityGroup::groupHasAccess('', '');
        self::assertEquals(false, $result);

        //test with valid values
        $result = SecurityGroup::groupHasAccess('Users', '1');
        self::assertEquals(false, $result);
    }

    public function testinherit(): void
    {
        $account = BeanFactory::newBean('Accounts');
        $account->id = 1;

        $_REQUEST['subpanel_field_name'] = 'id';

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SecurityGroup::inherit($account, false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testassign_default_groups(): void
    {
        $account = BeanFactory::newBean('Accounts');
        $account->id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SecurityGroup::assign_default_groups($account, false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testinherit_creator(): void
    {
        $account = BeanFactory::newBean('Accounts');
        $account->id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SecurityGroup::inherit_creator($account, false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testinherit_assigned(): void
    {
        $account = BeanFactory::newBean('Accounts');
        $account->id = 1;
        $account->assigned_user_id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SecurityGroup::inherit_assigned($account, false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testinherit_parent(): void
    {
        $account = BeanFactory::newBean('Accounts');
        $account->id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SecurityGroup::inherit_parent($account, false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testinherit_parentQuery(): void
    {
        $account = BeanFactory::newBean('Accounts');
        $account->id = 1;

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            SecurityGroup::inherit_parentQuery($account, 'Accounts', 1, 1, $account->module_dir);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testinheritOne(): void
    {
        $result = BeanFactory::newBean('SecurityGroups')::inheritOne(1, 1, 'Accounts');
        self::assertEquals(false, $result);
    }

    public function testgetMembershipCount(): void
    {
        $result = BeanFactory::newBean('SecurityGroups')::getMembershipCount('1');
        self::assertEquals(0, $result);
    }

    public function testSaveAndRetrieveAndRemoveDefaultGroups(): void
    {
        // Unset and reconnect Db to resolve mysqli fetch exception
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $securityGroup = BeanFactory::newBean('SecurityGroups');

        //create a security group first
        $securityGroup->name = 'test';
        $securityGroup->save();

        //execute saveDefaultGroup method
        $securityGroup::saveDefaultGroup($securityGroup->id, 'test_module');

        //execute retrieveDefaultGroups method
        $result = $securityGroup::retrieveDefaultGroups();

        //verify that default group is created
        self::assertIsArray($result);
        self::assertGreaterThan(0, count($result));

        //execute removeDefaultGroup method for each default group
        foreach ($result as $key => $value) {
            $securityGroup::removeDefaultGroup($key);
        }

        //retrieve back and verify that default securith groups are deleted
        $result = $securityGroup::retrieveDefaultGroups();
        self::assertCount(0, $result);

        //delete the security group as well for cleanup
        $securityGroup->mark_deleted($securityGroup->id);
    }

    public function testgetSecurityModules(): void
    {
        $securityGroup = BeanFactory::newBean('SecurityGroups');

        $expected = array(
            'Meetings',
            'Cases',
            'AOS_Products' => 'Products',
            'Opportunities',
            'OutboundEmailAccounts' => 'Outbound Email Accounts',
            'FP_Event_Locations' => 'Locations',
            'Tasks',
            'jjwg_Markers' => 'Maps - Markers',
            'EmailMarketing' => 'Email Marketing',
            'EmailTemplates' => 'Email - Templates',
            'Campaigns',
            'jjwg_Areas' => 'Maps - Areas',
            'Contacts',
            'AOS_Contracts' => 'Contracts',
            'AOS_Quotes' => 'Quotes',
            'Bugs',
            'Users',
            'Documents',
            'AOS_Invoices' => 'Invoices',
            'Notes',
            'AOW_WorkFlow' => 'WorkFlow',
            'ProspectLists' => 'Targets - Lists',
            'AOK_KnowledgeBase' => 'Knowledge Base',
            'AOS_PDF_Templates' => 'PDF - Templates',
            'Calls',
            'Accounts',
            'InboundEmail' => 'Inbound Email',
            'Leads',
            'Emails',
            'ExternalOAuthConnection' => 'External OAuth Connection',
            'ProjectTask' => 'Project Tasks',
            'ExternalOAuthProvider' => 'External OAuth Provider',
            'Project' => 'Projects',
            'FP_events' => 'Events',
            'AOR_Reports' => 'Reports',
            'AOR_Scheduled_Reports' => 'Scheduled Reports',
            'Prospects' => 'Targets',
            'ACLRoles' => 'Roles',
            'jjwg_Maps' => 'Maps',
            'AOS_Product_Categories' => 'Products - Categories',
            'Spots' => 'Spots',
            'SurveyQuestionOptions' => 'Survey Question Options',
            'SurveyQuestionResponses' => 'Survey Question Responses',
            'SurveyQuestions' => 'Survey Questions',
            'SurveyResponses' => 'Survey Responses',
            'Surveys' => 'Surveys',
        );

        $actual = $securityGroup::getSecurityModules();
        foreach ($expected as $expect) {
            self::assertContains($expect, $actual);
        }
    }

    public function testgetLinkName(): void
    {
        // Unset and reconnect Db to resolve mysqli fetch exceptions
        $db = DBManagerFactory::getInstance();
        $db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $securityGroup = BeanFactory::newBean('SecurityGroups');

        $result = $securityGroup::getLinkName('Accounts', 'Contacts');
        self::assertEquals('contacts', $result);

        $result = $securityGroup::getLinkName('SecurityGroups', 'ACLRoles');
        self::assertEquals('aclroles', $result);
    }

    public function testaddGroupToRecord(): void
    {
        // unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        //$db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $securityGroup = BeanFactory::newBean('SecurityGroups');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $securityGroup->addGroupToRecord('Accounts', 1, 1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testremoveGroupFromRecord(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        //$db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $securityGroup = BeanFactory::newBean('SecurityGroups');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $securityGroup::removeGroupFromRecord('Accounts', 1, 1);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetUserSecurityGroups(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        //$db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $result = BeanFactory::newBean('SecurityGroups')::getUserSecurityGroups('1');

        self::assertIsArray($result);
    }

    public function testgetAllSecurityGroups(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        //$db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $result = BeanFactory::newBean('SecurityGroups')::getAllSecurityGroups();

        self::assertIsArray($result);
    }

    public function testgetMembers(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        //$db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $result = BeanFactory::newBean('SecurityGroups')->getMembers();

        self::assertIsArray($result);
    }

    public function testgetPrimaryGroupID(): void
    {
        //unset and reconnect Db to resolve mysqli fetch exeception
        $db = DBManagerFactory::getInstance();
        //$db->disconnect();
        unset($db->database);
        $db->checkConnection();

        $result = BeanFactory::newBean('SecurityGroups')::getPrimaryGroupID();

        self::assertEquals(null, $result);
    }
}
