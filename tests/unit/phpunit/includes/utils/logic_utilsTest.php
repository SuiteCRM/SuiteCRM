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

use Exception;
use org\bovigo\vfs\vfsStream;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once __DIR__ . '/../../../../../include/utils/logic_utils.php';

/**
 * Class logic_utilsTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class logic_utilsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testget_hook_array(): void
    {
        //test with a vaid module. it will return an array
        if (file_exists("custom/modules/Accounts/logic_hooks.php")) {
            $AccountsHooks = get_hook_array('Accounts');
            self::assertIsArray($AccountsHooks);
        }

        //test with an invalid array. it will throw an file include exception.
        $BugsHooks = '';
        try {
            $BugsHooks = get_hook_array('Bugs');
        } catch (Exception $e) {
        }

        self::assertIsNotArray($BugsHooks);
    }

    public function check_existing_elementProvider(): array
    {
        $hook_array = $this->getTestHook();

        return [
            [$hook_array, 'after_save', [0, 'popup_select'], true],
            [$hook_array, 'after_save', [0, 'foo'], false],
            [$hook_array, 'foo', [0, 'popup_select'], false],
        ];
    }

    private function getTestHook(): array
    {
        //array containing hooks array to test

        $hook_array = array();
        $hook_array['after_ui_footer'] = array();
        $hook_array['after_ui_footer'][] = array(
            10,
            'popup_onload',
            'modules/SecurityGroups/AssignGroups.php',
            'AssignGroups',
            'popup_onload'
        );
        $hook_array['after_ui_frame'] = array();
        $hook_array['after_ui_frame'][] = array(
            20,
            'mass_assign',
            'modules/SecurityGroups/AssignGroups.php',
            'AssignGroups',
            'mass_assign'
        );
        $hook_array['after_ui_frame'][] = array(
            1,
            'Load Social JS',
            'custom/include/social/hooks.php',
            'hooks',
            'load_js'
        );
        $hook_array['after_save'] = array();
        $hook_array['after_save'][] = array(
            30,
            'popup_select',
            'modules/SecurityGroups/AssignGroups.php',
            'AssignGroups',
            'popup_select'
        );
        $hook_array['after_save'][] = array(
            1,
            'AOD Index Changes',
            'modules/AOD_Index/AOD_LogicHooks.php',
            'AOD_LogicHooks',
            'saveModuleChanges'
        );
        $hook_array['after_delete'] = array();
        $hook_array['after_delete'][] = array(
            1,
            'AOD Index changes',
            'modules/AOD_Index/AOD_LogicHooks.php',
            'AOD_LogicHooks',
            'saveModuleDelete'
        );
        $hook_array['after_restore'] = array();
        $hook_array['after_restore'][] = array(
            1,
            'AOD Index changes',
            'modules/AOD_Index/AOD_LogicHooks.php',
            'AOD_LogicHooks',
            'saveModuleRestore'
        );

        return $hook_array;
    }

    /**
     * @dataProvider check_existing_elementProvider
     */
    public function testcheck_existing_element($hook_array, $event, $action_array, $expected): void
    {
        //test with dataset containing valid and invalid cases
        self::assertSame(check_existing_element($hook_array, $event, $action_array), $expected);
    }

    public function testreplace_or_add_logic_type(): void
    {
        //execute the method and test if it returns expected values

        $hook_array = $this->getTestHook();
        $expected = "<?php\n// Do not store anything in this file that is not part of the array or the hook version.  This file will	\n// be automatically rebuilt in the future. \n \$hook_version = 1; \n\$hook_array = Array(); \n// position, file, function \n\$hook_array['after_ui_footer'] = Array(); \n\$hook_array['after_ui_footer'][] = Array(10, 'popup_onload', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'popup_onload'); \n\$hook_array['after_ui_frame'] = Array(); \n\$hook_array['after_ui_frame'][] = Array(20, 'mass_assign', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'mass_assign'); \n\$hook_array['after_ui_frame'][] = Array(1, 'Load Social JS', 'custom/include/social/hooks.php','hooks', 'load_js'); \n\$hook_array['after_save'] = Array(); \n\$hook_array['after_save'][] = Array(30, 'popup_select', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'popup_select'); \n\$hook_array['after_save'][] = Array(1, 'AOD Index Changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleChanges'); \n\$hook_array['after_delete'] = Array(); \n\$hook_array['after_delete'][] = Array(1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleDelete'); \n\$hook_array['after_restore'] = Array(); \n\$hook_array['after_restore'][] = Array(1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleRestore'); \n\n\n\n?>";
        self::assertEquals($expected, replace_or_add_logic_type($hook_array));
    }

    public function testwrite_logic_file(): void
    {
        //execute the method and test if it returns expected values,
        //check if file is created and contains expected contents

        $vfs = vfsStream::setup('custom/modules/TEST_Test');

        if ($vfs->hasChild('logic_hooks.php') == true) {
            unlink('custom/modules/TEST_Test/logic_hooks.php');
            rmdir('custom/modules/TEST_Test');
        }

        $expectedContents = "<?php\n// Do not store anything in this file that is not part of the array or the hook version.  This file will	\n// be automatically rebuilt in the future. \n \$hook_version = 1; \n\$hook_array = Array(); \n// position, file, function \n\$hook_array['after_ui_footer'] = Array(); \n\$hook_array['after_ui_footer'][] = Array(10, 'popup_onload', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'popup_onload'); \n\$hook_array['after_ui_frame'] = Array(); \n\$hook_array['after_ui_frame'][] = Array(20, 'mass_assign', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'mass_assign'); \n\$hook_array['after_ui_frame'][] = Array(1, 'Load Social JS', 'custom/include/social/hooks.php','hooks', 'load_js'); \n\$hook_array['after_save'] = Array(); \n\$hook_array['after_save'][] = Array(30, 'popup_select', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'popup_select'); \n\$hook_array['after_save'][] = Array(1, 'AOD Index Changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleChanges'); \n\$hook_array['after_delete'] = Array(); \n\$hook_array['after_delete'][] = Array(1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleDelete'); \n\$hook_array['after_restore'] = Array(); \n\$hook_array['after_restore'][] = Array(1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleRestore'); \n\n\n\n?>";
        write_logic_file('TEST_Test', $expectedContents);

        //Check file created
        self::assertFileExists('custom/modules/TEST_Test/logic_hooks.php');
        $actualContents = file_get_contents('custom/modules/TEST_Test/logic_hooks.php');
        self::assertSame($expectedContents, $actualContents);

        $expectedArray = $this->getTestHook();

        $actualArray = get_hook_array('TEST_Test');

        self::assertSame($expectedArray, $actualArray);

        unlink('custom/modules/TEST_Test/logic_hooks.php');
        rmdir('custom/modules/TEST_Test');
    }

    public function testbuild_logic_file(): void
    {
        //execute the method and test if it returns expected values

        $hook_array = $this->getTestHook();
        $expected = "// Do not store anything in this file that is not part of the array or the hook version.  This file will	\n// be automatically rebuilt in the future. \n \$hook_version = 1; \n\$hook_array = Array(); \n// position, file, function \n\$hook_array['after_ui_footer'] = Array(); \n\$hook_array['after_ui_footer'][] = Array(10, 'popup_onload', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'popup_onload'); \n\$hook_array['after_ui_frame'] = Array(); \n\$hook_array['after_ui_frame'][] = Array(20, 'mass_assign', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'mass_assign'); \n\$hook_array['after_ui_frame'][] = Array(1, 'Load Social JS', 'custom/include/social/hooks.php','hooks', 'load_js'); \n\$hook_array['after_save'] = Array(); \n\$hook_array['after_save'][] = Array(30, 'popup_select', 'modules/SecurityGroups/AssignGroups.php','AssignGroups', 'popup_select'); \n\$hook_array['after_save'][] = Array(1, 'AOD Index Changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleChanges'); \n\$hook_array['after_delete'] = Array(); \n\$hook_array['after_delete'][] = Array(1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleDelete'); \n\$hook_array['after_restore'] = Array(); \n\$hook_array['after_restore'][] = Array(1, 'AOD Index changes', 'modules/AOD_Index/AOD_LogicHooks.php','AOD_LogicHooks', 'saveModuleRestore'); \n\n\n";
        self::assertEquals($expected, build_logic_file($hook_array));
    }
}
