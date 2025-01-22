<?php /** @noinspection ALL */

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ACLActionTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    protected function tearDown(): void
    {
        parent::tearDown();
    }

    public function testACLAction()
    {
        self::markTestIncomplete('environment dependency');

        $_POST['foo'] = 'bar123ase';
        // Execute the constructor and check for the Object type and type attribute
        $aclAction = BeanFactory::newBean('ACLActions');
        $this->assertInstanceOf('ACLAction', $aclAction);
        $this->assertInstanceOf('SugarBean', $aclAction);

        $this->assertAttributeEquals('ACLActions', 'module_dir', $aclAction);
        $this->assertAttributeEquals('ACLAction', 'object_name', $aclAction);
        $this->assertAttributeEquals('acl_actions', 'table_name', $aclAction);
        $this->assertAttributeEquals(true, 'new_schema', $aclAction);
        $this->assertAttributeEquals(true, 'disable_custom_fields', $aclAction);
    }

    public function testaddActions()
    {
        self::markTestIncomplete('environment dependency');

        //take count of actions initially and then after method execution and test if action count increases
        $defaultActions = ACLAction::getDefaultActions();
        $action_count = is_countable($defaultActions) ? count($defaultActions) : 0;
        ACLAction::addActions('Test');
        $actual = ACLAction::getDefaultActions();
        $this->assertGreaterThan($action_count, is_countable($actual) ? count($actual) : 0);
    }

    public function testremoveActions()
    {
        //take count of actions initially and then after method execution and test if action count decreases
        $defaultActions = ACLAction::getDefaultActions();
        $action_count = is_countable($defaultActions) ? count($defaultActions) : 0;
        ACLAction::removeActions('Test');
        $actual = ACLAction::getDefaultActions();
        $actualCount = is_countable($actual) ? count($actual) : 0;
        $this->assertLessThanOrEqual($action_count, $actualCount, 'actual count was: ' . $actualCount);
    }

    public function testAccessName()
    {
        $this->assertFalse(ACLAction::AccessName('')); //test with invalid value
        $this->assertEquals('All', ACLAction::AccessName(90)); //test with a valid value
    }

    public function testgetDefaultActions()
    {
        global $beanList;
        $actual = ACLAction::getDefaultActions();
        $this->assertTrue(is_array($actual)); //verify that it returns an array
        foreach ($actual as $acl) {
            $this->assertInstanceOf('ACLAction', $acl);
        }
        $actual = ACLAction::getDefaultActions('module', 'list');
        $this->assertTrue(is_array($actual)); //verify that it returns an array
        foreach ($actual as $acl) {
            $this->assertInstanceOf('ACLAction', $acl);
            $this->assertEquals('list', $acl->name);
        }
    }

    public function testhasAccess()
    {
        $this->assertFalse(ACLAction::hasAccess()); //check with defaults
        $this->assertTrue(ACLAction::hasAccess(false, false, 90));  //access All with is owner false
        $this->assertTrue(ACLAction::hasAccess(true, true, 90)); //access All with is owner true
        $this->assertFalse(ACLAction::hasAccess(false, false, -98));// check access disabled
        $this->assertFalse(ACLAction::hasAccess(true, true, 89)); //check access enabled
        $this->assertTrue(ACLAction::hasAccess(true, true, 75)); //check owner access with is owner true
        $this->assertFalse(ACLAction::hasAccess(false, true, 75)); //check owner access with is owner false
    }

    public function testuserNeedsSecurityGroup()
    {
        $this->assertFalse(ACLAction::userNeedsSecurityGroup('1', '', ''));//test with empty module and action
        $this->assertFalse(ACLAction::userNeedsSecurityGroup('1', 'Accounts',
            'list')); //test with valid module and action
    }

    public function testsetupCategoriesMatrix()
    {
        //preset required data
        $categories = array();
        $categories['Accounts']['module']['list'][] = 'list';
        $categories['Accounts']['module']['edit'][] = 'edit';

        $names_expected = array('list' => 'List', 'edit' => 'Edit');

        $categories_expected = array(
            'Accounts' => array(
                'module' => array(
                    'list' => array(
                        'list',
                        'accessColor' => false,
                        'accessName' => false,
                        'accessLabel' => false,
                        'accessOptions' => array(
                            90 => 'All',
                            80 => 'Group',
                            75 => 'Owner',
                            0 => 'Not Set',
                            -99 => 'None'
                        )
                    ),
                    'edit' => array(
                        'edit',
                        'accessColor' => false,
                        'accessName' => false,
                        'accessLabel' => false,
                        'accessOptions' => array(
                            90 => 'All',
                            80 => 'Group',
                            75 => 'Owner',
                            0 => 'Not Set',
                            -99 => 'None'
                        )
                    ),
                ),
            ),
        );

        //execute the method and verify that it retunrs expected results
        $result = ACLAction::setupCategoriesMatrix($categories);
        $this->assertSame($names_expected, $result);
        $this->assertSame($categories, $categories_expected);
    }

    public function testtoArray()
    {
        $aclAction = BeanFactory::newBean('ACLActions');

        //wihout any fields set
        $expected = array('id' => null, 'aclaccess' => null);
        $actual = $aclAction->toArray();
        $this->assertSame($expected, $actual);

        //with fileds pre populated
        $aclAction->populateFromRow(array('id' => '1234', 'aclaccess' => '9999'));
        $expected = array('id' => '1234', 'aclaccess' => '9999');
        $actual = $aclAction->toArray();
        $this->assertSame($expected, $actual);
    }

    public function testfromArray()
    {
        $aclAction = BeanFactory::newBean('ACLActions');
        $arr = array('id' => '1234', 'name' => 'test');

        //execute the method and verify that it retunrs expected results
        $aclAction->fromArray($arr);
        $this->assertSame($aclAction->id, '1234');
        $this->assertSame($aclAction->name, 'test');
    }

    public function testclearSessionCache()
    {
        $aclAction = BeanFactory::newBean('ACLActions');

        //execute the method and verify that it unsets the session cache
        $aclAction->clearSessionCache();
        $this->assertFalse(isset($_SESSION['ACL']));
    }
}
