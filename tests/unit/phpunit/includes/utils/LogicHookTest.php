<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

namespace SuiteCRM\Tests\Unit\includes\utils;

use BeanFactory;
use Exception;
use LogicHook;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class LogicHookTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class LogicHookTest extends SuitePHPUnitFrameworkTestCase
{
    public function testinitialize(): void
    {
        //execute the method and test if it returns correct class instances
        $LogicHook = LogicHook::initialize();
        self::assertInstanceOf('LogicHook', $LogicHook);
    }

    public function testLogicHook(): void
    {
        //execute the method and test if it doesn't throw an exception
        try {
            $LogicHook = new LogicHook();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testsetBean(): void
    {
        //execute the method and test if it returns correct class instances
        $result = (new LogicHook())->setBean(BeanFactory::newBean('Users'));
        self::assertInstanceOf('LogicHook', $result);
        self::assertInstanceOf('User', $result->bean);
    }

    public function testgetHooksMap(): void
    {
        //execute the method and test if it returns true
        $hook_map = (new LogicHook())->getHooksMap();
        self::assertIsArray($hook_map);
    }

    public function testgetHooksList(): void
    {
        //execute the method and test if it returns true
        $hookscan = (new LogicHook())->getHooksList();
        self::assertIsArray($hookscan);
    }

    public function testscanHooksDir(): void
    {
        //execute the method and test if it returns expected contents
        $expected_hook_map = array(
            'before_save' =>
                array(
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 0,),
                ),
            'after_save' =>
                array(
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 0,),
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 1,),
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 2,),
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 3,),
                ),
            'after_relationship_add' =>
                array(
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 0,),
                ),
            'after_relationship_delete' =>
                array(
                    array('file' => 'custom/modules/Accounts/logic_hooks.php', 'index' => 0,),
                ),
        );


        $expected_hookscan = array(
            'before_save' =>
                array(
                    array(
                        77,
                        'updateGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateGeocodeInfo',
                    ),
                ),
            'after_save' =>
                array(
                    array(
                        77,
                        'updateRelatedMeetingsGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedMeetingsGeocodeInfo',
                    ),
                    array(
                        78,
                        'updateRelatedProjectGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedProjectGeocodeInfo',
                    ),
                    array(
                        79,
                        'updateRelatedOpportunitiesGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedOpportunitiesGeocodeInfo',
                    ),
                    array(
                        80,
                        'updateRelatedCasesGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedCasesGeocodeInfo',
                    ),
                ),
            'after_relationship_add' =>
                array(
                    array(
                        77,
                        'addRelationship',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'addRelationship',
                    ),
                ),
            'after_relationship_delete' =>
                array(
                    array(
                        77,
                        'deleteRelationship',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'deleteRelationship',
                    ),
                ),
        );

        $LogicHook = new LogicHook();
        $LogicHook->scanHooksDir('custom/modules/Accounts');
        $hook_map = $LogicHook->getHooksMap();
        $hookscan = $LogicHook->getHooksList();


        if (file_exists('custom/modules/Accounts') && is_dir('custom/modules/Accounts')) {
            self::assertSame($expected_hook_map, $hook_map);
            self::assertSame($expected_hookscan, $hookscan);
        } else {
            self::assertEmpty($hook_map);
            self::assertEmpty($hookscan);
        }
    }

    public function testrefreshHooks(): void
    {
        //execute the method and test if it doesn't throws an exception
        try {
            LogicHook::refreshHooks();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testloadHooks(): void
    {
        //execute the method and test if it returns expected contents
        $expected_accounts = array(
            'before_save' => array(
                array(
                    77,
                    'updateGeocodeInfo',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'updateGeocodeInfo'
                ),
            ),
            'after_save' => array(
                array(
                    77,
                    'updateRelatedMeetingsGeocodeInfo',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'updateRelatedMeetingsGeocodeInfo'
                ),
                array(
                    78,
                    'updateRelatedProjectGeocodeInfo',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'updateRelatedProjectGeocodeInfo'
                ),
                array(
                    79,
                    'updateRelatedOpportunitiesGeocodeInfo',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'updateRelatedOpportunitiesGeocodeInfo'
                ),
                array(
                    80,
                    'updateRelatedCasesGeocodeInfo',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'updateRelatedCasesGeocodeInfo'
                ),
            ),
            'after_relationship_add' => array(
                array(
                    77,
                    'addRelationship',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'addRelationship'
                ),
            ),
            'after_relationship_delete' => array(
                array(
                    77,
                    'deleteRelationship',
                    'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                    'AccountsJjwg_MapsLogicHook',
                    'deleteRelationship'
                ),
            ),
        );

        $expected_default = array(
            'after_save' => array(
                array(
                    1,
                    'AOD Index Changes',
                    'modules/AOD_Index/AOD_LogicHooks.php',
                    'AOD_LogicHooks',
                    'saveModuleChanges'
                ),
                array(30, 'popup_select', 'modules/SecurityGroups/AssignGroups.php', 'AssignGroups', 'popup_select'),
                array(99, 'AOW_Workflow', 'modules/AOW_WorkFlow/AOW_WorkFlow.php', 'AOW_WorkFlow', 'run_bean_flows'),
            ),
            'after_delete' => array(
                array(
                    1,
                    'AOD Index changes',
                    'modules/AOD_Index/AOD_LogicHooks.php',
                    'AOD_LogicHooks',
                    'saveModuleDelete'
                ),
            ),
            'after_restore' => array(
                array(
                    1,
                    'AOD Index changes',
                    'modules/AOD_Index/AOD_LogicHooks.php',
                    'AOD_LogicHooks',
                    'saveModuleRestore'
                ),
            ),
            'after_ui_footer' => array(
                array(10, 'popup_onload', 'modules/SecurityGroups/AssignGroups.php', 'AssignGroups', 'popup_onload'),
            ),
            'after_ui_frame' => array(
                array(20, 'mass_assign', 'modules/SecurityGroups/AssignGroups.php', 'AssignGroups', 'mass_assign'),
                array(1, 'Load Social JS', 'include/social/hooks.php', 'hooks', 'load_js'),
            ),
        );

        $LogicHook = new LogicHook();

        //test with a valid module
        $accounts_hooks = $LogicHook->loadHooks('Accounts');
        if (
            file_exists("custom/modules/Accounts/logic_hooks.php") ||
            file_exists("custom/modules/Accounts/Ext/LogicHooks/logichooks.ext.php")
        ) {
            self::assertSame($expected_accounts, $accounts_hooks);
        } else {
            self::assertEmpty($accounts_hooks);
        }

        //test with an invalid module, it will get the application hooks
        $default_hooks = $LogicHook->loadHooks('');
        if (
            file_exists("custom/modules/logic_hooks.php") ||
            file_exists("custom/application/Ext/LogicHooks/logichooks.ext.php")
        ) {
            //$this->assertSame($expected_default, $default_hooks);
        } else {
            self::assertEmpty($default_hooks);
        }
    }

    public function testgetHooks(): void
    {
        //execute the method and test if it returns expected contents

        $expected = array(
            /*'after_ui_frame' => Array (),*/
            'before_save' =>
                array(
                    array(
                        77,
                        'updateGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateGeocodeInfo',
                    ),
                ),
            'after_save' =>
                array(
                    array(
                        77,
                        'updateRelatedMeetingsGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedMeetingsGeocodeInfo',
                    ),
                    array(
                        78,
                        'updateRelatedProjectGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedProjectGeocodeInfo',
                    ),
                    array(
                        79,
                        'updateRelatedOpportunitiesGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedOpportunitiesGeocodeInfo',
                    ),
                    array(
                        80,
                        'updateRelatedCasesGeocodeInfo',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'updateRelatedCasesGeocodeInfo',
                    ),
                ),
            'after_relationship_add' =>
                array(
                    array(
                        77,
                        'addRelationship',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'addRelationship',
                    ),
                ),
            'after_relationship_delete' =>
                array(
                    array(
                        77,
                        'deleteRelationship',
                        'modules/Accounts/AccountsJjwg_MapsLogicHook.php',
                        'AccountsJjwg_MapsLogicHook',
                        'deleteRelationship',
                    ),
                ),
        );

        $LogicHook = new LogicHook();

        //test with refresh false/default
        $hooks = $LogicHook->getHooks('Accounts');
        if (
            file_exists("custom/modules/Accounts/logic_hooks.php") ||
            file_exists("custom/modules/Accounts/Ext/LogicHooks/logichooks.ext.php")
        ) {
            self::assertEquals($expected, $hooks);
        } else {
            self::assertEmpty($hooks);
        }

        //test wit hrefresh true
        $hooks = $LogicHook->getHooks('Accounts', true);
        if (
            file_exists("custom/modules/Accounts/logic_hooks.php") ||
            file_exists("custom/modules/Accounts/Ext/LogicHooks/logichooks.ext.php")
        ) {
            self::assertSame($expected, $hooks);
        } else {
            self::assertEmpty($hooks);
        }
    }

    public function testcall_custom_logic(): void
    {
        //execute the method and test if it doesn't throws an exception
        $LogicHook = new LogicHook();
        $LogicHook->setBean(BeanFactory::newBean('Accounts'));

        try {
            $LogicHook->call_custom_logic('', 'after_ui_footer');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    /*
    public function testloadHooks()
    {
        //execute the method and test if it returns expected contents

        $expected_accounts = array (
                'after_ui_frame' => Array (),
                'before_save' =>
                array (
                        array (77, 'updateGeocodeInfo', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'updateGeocodeInfo',),
                ),
                'after_save' =>
                array (
                        array (77, 'updateRelatedMeetingsGeocodeInfo', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'updateRelatedMeetingsGeocodeInfo',),
                        array (78, 'updateRelatedProjectGeocodeInfo', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'updateRelatedProjectGeocodeInfo', ),
                        array (79, 'updateRelatedOpportunitiesGeocodeInfo', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'updateRelatedOpportunitiesGeocodeInfo',),
                        array (80, 'updateRelatedCasesGeocodeInfo', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'updateRelatedCasesGeocodeInfo',),
                ),
                'after_relationship_add' =>
                array (
                        array ( 77, 'addRelationship', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'addRelationship',),
                ),
                'after_relationship_delete' =>
                array (
                        array ( 77, 'deleteRelationship', 'modules/Accounts/AccountsJjwg_MapsLogicHook.php', 'AccountsJjwg_MapsLogicHook', 'deleteRelationship',),
                ),
        );

        $expected_default = array (
                    'after_ui_footer' =>
                    array (
                            array (10,'popup_onload','modules/SecurityGroups/AssignGroups.php','AssignGroups','popup_onload',),
                    ),
                    'after_ui_frame' =>
                    array (
                            array (20, 'mass_assign', 'modules/SecurityGroups/AssignGroups.php', 'AssignGroups', 'mass_assign',),
                            array ( 1, 'Load Social JS', 'include/social/hooks.php', 'hooks', 'load_js',),
                    ),
                    'after_save' =>
                    array (
                            array ( 30,'popup_select', 'modules/SecurityGroups/AssignGroups.php','AssignGroups','popup_select',),
                            array ( 1, 'AOD Index Changes', 'modules/AOD_Index/AOD_LogicHooks.php', 'AOD_LogicHooks', 'saveModuleChanges',),
                            array ( 99, 'AOW_Workflow', 'modules/AOW_WorkFlow/AOW_WorkFlow.php', 'AOW_WorkFlow','run_bean_flows',),
                    ),
                    'after_delete' =>
                    array (
                            array ( 1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php', 'AOD_LogicHooks','saveModuleDelete',),
                    ),
                    'after_restore' =>
                    array (
                            array ( 1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php', 'AOD_LogicHooks', 'saveModuleRestore',),
                    ),
                );



        $LogicHook = new LogicHook();

        //test with a valid module
        $accounts_hooks = $LogicHook->loadHooks('Accounts');
        $this->assertSame($expected_accounts, $accounts_hooks);

        //test with an invalid module, it will get the application hooks
        $default_hooks = $LogicHook->loadHooks('');
        $this->assertSame($expected_default, $default_hooks);

    }
*/

    public function testprocess_hooks(): void
    {
        // execute the method and test if it doesn't throws an exception
        $LogicHook = new LogicHook();
        $LogicHook->setBean(BeanFactory::newBean('Accounts'));
        $hooks = $LogicHook->loadHooks('');

        try {
            $LogicHook->process_hooks($hooks, 'after_ui_footer', array());
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    protected function setUp(): void
    {
        parent::setUp();
    }

    protected function tearDown(): void
    {
        parent::tearDown();
    }
}
