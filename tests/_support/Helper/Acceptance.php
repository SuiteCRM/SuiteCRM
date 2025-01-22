<?php
/**
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

namespace Helper;

// here you can define custom actions
// all public methods declared in helper class will be available in $I

use Codeception\Exception\ModuleException;
use Codeception\Module;
use PHPUnit\Framework\AssertionFailedError;
use SuiteCRM\Test\Driver\WebDriver;

/**
 * Class Acceptance
 * @package Helper
 */
#[\AllowDynamicProperties]
class Acceptance extends Module
{
    public function seePageHas($text, $selector = null): bool
    {
        try {
            $this->getModule(WebDriver::class)->see($text, $selector);
        } catch (AssertionFailedError | ModuleException $f) {
            return false;
        }

        return true;
    }

    // Clean up any files left behind by module builder tests.
    // This needs to be run both before _and_ after to handle the case where
    // the test run is cancelled or fails and the afterSuite() hook isn't run.
    public function _beforeSuite($settings = [])
    {
        $this->deleteModulesHelper();
    }

    // Clean up any files left behind by module builder tests.
    public function _afterSuite()
    {
        $this->deleteModulesHelper();
    }

    // Clean up any files left behind by module builder tests.
    private function deleteModulesHelper(): void
    {
        $modules = [
            'BasicTestModule',
            'CompanyTestModule',
            'FileTestModule',
            'IssueTestModule',
            'PersonTestModule',
            'SaleTestModule',
            'TestModuleFields'
        ];

        foreach ($modules as $module) {
            try {
                $this->deleteModuleFiles($module);
            } catch (ModuleException $e) {
                return;
            }
        }
    }

    /**
     * Deletes module files and directories created by the module builder.
     * This allows the acceptance tests to be re-run.
     * @param string $module
     * @throws ModuleException
     */
    private function deleteModuleFiles(string $module): void
    {
        $directories = [
            "custom/modulebuilder/builds/{$module}",
            "custom/modulebuilder/packages/{$module}",
            "modules/Test_{$module}"
        ];

        $files = [
            "custom/application/Ext/Include/modules.ext.php",
            "custom/Extension/application/Ext/Include/{$module}.php"
        ];

        foreach ($directories as $directory) {
            if (is_dir($directory)) {
                $this->getModule('Filesystem')->deleteDir($directory);
            }
        }

        foreach ($files as $file) {
            if (is_file($file)) {
                unlink($file);
            }
        }
    }
}
