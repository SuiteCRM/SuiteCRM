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

namespace SuiteCRM\Test;

use SugarBean;
use SuiteCRM\Exception\Exception;

/**
 * Class BeanFactoryTestCase
 * @package SuiteCRM\Tests\SuiteCRM\Test
 */
class BeanFactoryTestCase extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var bool
     */
    protected $testAllModules = false;

    /**
     * @var string
     */
    protected $testSingleModule = 'Accounts';

    /**
     * @return void
     * @throws Exception in TestCaseAbstract::setup
     */
    public function setup(): void
    {
        $this->removeCoreModuleAllExtension();

        parent::setup();
    }

    /**
     * @return void
     */
    public function tearDown(): void
    {
        $this->removeCoreModuleAllExtension();

        $this->refreshModuleGlobals();

        parent::tearDown();
    }

    /**
     * @return array
     */
    public function moduleConfigProvider(): array
    {
        $this->refreshModuleGlobals();

        global $customBeanList, $objectList, $customObjectList, $beanFiles, $customBeanFiles;

        $moduleList = $this->getModuleList();

        $modulesConfig = [];
        foreach ($moduleList as $moduleName => $moduleClass) {
            if ($this->shouldSkipModule($moduleClass)) {
                continue;
            }

            $moduleConfig = [
                'name' => $moduleName,
                'meta' => [
                    'objectName' => !empty($objectList[$moduleName]) ? $objectList[$moduleName] : $moduleClass,
                    'className' => $moduleClass,
                    'classFile' => $beanFiles[$moduleClass],
                    'customObjectName' => !empty($customObjectList[$moduleName])
                        ? $customObjectList[$moduleName]
                        : null,
                    'customClassName' => !empty($customBeanList[$moduleName])
                        ? $customBeanList[$moduleName]
                        : null,
                ],
            ];

            $moduleConfig['meta']['customClassFile'] = null;

            if (!empty($customBeanList[$moduleName])) {
                if (!empty($customBeanFiles[$customBeanList[$moduleName]])) {
                    $moduleConfig['meta']['customClassFile'] = $customBeanFiles[$customBeanList[$moduleName]];
                }

                if (!empty($customBeanFiles[$moduleClass])) {
                    $moduleConfig['meta']['customClassFile'] = $customBeanFiles[$moduleClass];
                }
            }

            $modulesConfig[$moduleName] = $moduleConfig;
        }

        return $modulesConfig;
    }

    /**
     * @return array
     */
    protected function getModuleList(): array
    {
        global $beanList;

        return $this->testAllModules
            ? $beanList
            : [
                $this->testSingleModule => $beanList[$this->testSingleModule],
            ];
    }

    /**
     * @param $moduleClass
     * @return bool
     */
    protected function shouldSkipModule($moduleClass): bool
    {
        global $beanFiles;

        if (empty($beanFiles[$moduleClass]) || !file_exists($beanFiles[$moduleClass])) {
            return true;
        }

        $bean = new $moduleClass();
        if (!$bean instanceof SugarBean || empty($bean->table_name)) {
            return true;
        }

        return false;
    }

    /**
     * @return void
     */
    public function compileIncludeExtFiles(): void
    {
        $extensionContents = '<?php' . PHP_EOL;
        $compiledIncludePath = 'custom/application/Ext/Include';
        $extIncludePath = 'custom/Extension/application/Ext/Include';
        $noExtensions = true;

        if (!file_exists($extIncludePath)) {
            mkdir_recursive($extIncludePath, true);
        }

        $extIncludeDir = dir($extIncludePath);
        while ($file = $extIncludeDir->read()) {
            if ($this->shouldSkipFileEntry($file, $extIncludePath)) {
                continue;
            }

            $noExtensions = false;
            $contents = file_get_contents("$extIncludePath/$file");
            $extensionContents .= PHP_EOL . trim(
                    str_replace(['<?php', '?>', '<?PHP', '<?'], '', $contents)
                ) . PHP_EOL;
        }

        if (!file_exists($compiledIncludePath)) {
            mkdir_recursive($compiledIncludePath, true);
        }

        self::assertTrue(is_writable($compiledIncludePath), 'Not writable: ' . $compiledIncludePath);

        if (!$noExtensions && is_writable($compiledIncludePath)) {
            file_put_contents("$compiledIncludePath/modules.ext.php", $extensionContents);
        }

        if ($noExtensions && file_exists("$compiledIncludePath/modules.ext.php")) {
            unlink("$compiledIncludePath/modules.ext.php");
        }
    }

    /**
     * @param $file
     * @param $path
     * @return bool
     */
    protected function shouldSkipFileEntry($file, $path): bool
    {
        if ($file === '.' || $file === '..' || strtolower(substr((string) $file, -4)) !== '.php') {
            return true;
        }

        if (!is_file("$path/$file")) {
            return true;
        }

        return false;
    }

    /**
     * @param string $moduleName
     * @param string $className
     * @return void
     */
    public function addCoreModuleExtension(string $moduleName, string $className): void
    {
        $extIncludePath = 'custom/Extension/application/Ext/Include';
        $customClassPath = "custom/modules/$moduleName";
        $fileStart = '<?php' . PHP_EOL . PHP_EOL;

        $extIncludeContents = $fileStart . <<<EOT
\$customBeanList['$moduleName'] = 'TestCustom$className';
\$customObjectList['$moduleName'] = 'TestCustom$className';
\$customBeanFiles['$className'] = 'custom/modules/$moduleName/TestCustom$className.php';

EOT;

        $classContents = $fileStart . <<<EOT
class TestCustom$className extends $className
{
}

EOT;

        if (!file_exists($extIncludePath)) {
            mkdir_recursive($extIncludePath, true);
        }

        self::assertTrue(is_writable($extIncludePath), 'Directory not writable: ' . $extIncludePath);

        if (is_writable($extIncludePath)) {
            file_put_contents("$extIncludePath/ZzzTestCustom$moduleName.php", $extIncludeContents);
        }

        if (!file_exists($customClassPath)) {
            mkdir_recursive($customClassPath, true);
        }

        self::assertTrue(is_writable($customClassPath), 'Directory not writable: ' . $customClassPath);

        if (is_writable($customClassPath)) {
            file_put_contents("$customClassPath/TestCustom$className.php", $classContents);
        }

        $this->compileIncludeExtFiles();
    }

    /**
     * @param string $moduleName
     * @param string $className
     * @return void
     */
    public function removeCoreModuleExtension(string $moduleName, string $className): void
    {
        if (file_exists("custom/Extension/application/Ext/Include/ZzzTestCustom$moduleName.php")) {
            unlink("custom/Extension/application/Ext/Include/ZzzTestCustom$moduleName.php");
        }

        if (file_exists("custom/modules/$moduleName/TestCustom$className.php")) {
            unlink("custom/modules/$moduleName/TestCustom$className.php");
        }

        $this->compileIncludeExtFiles();
    }

    /**
     * @return void
     */
    public function removeCoreModuleAllExtension(): void
    {
        foreach ($this->moduleConfigProvider() as $moduleName => $moduleConfig) {
            if (file_exists("custom/Extension/application/Ext/Include/ZzzTestCustom$moduleName.php")) {
                unlink("custom/Extension/application/Ext/Include/ZzzTestCustom$moduleName.php");
            }

            if (file_exists("custom/modules/$moduleName/TestCustom{$moduleConfig['meta']['className']}.php")) {
                unlink("custom/modules/$moduleName/TestCustom{$moduleConfig['meta']['className']}.php");
            }
        }

        $this->compileIncludeExtFiles();
    }

    /**
     * @return void
     */
    public function refreshModuleGlobals(): void
    {
        $beanList = $customBeanList = $objectList = $customObjectList = $beanFiles = $customBeanFiles = [];

        require 'include/modules.php';

        $tmpBeanList = $beanList;
        $tmpCustomBeanList = $customBeanList;
        $tmpObjectList = $objectList;
        $tmpCustomObjectList = $customObjectList;
        $tmpBeanFiles = $beanFiles;
        $tmpCustomBeanFiles = $customBeanFiles;

        global $beanList, $customBeanList, $objectList, $customObjectList, $beanFiles, $customBeanFiles;

        $beanList = $tmpBeanList;
        $customBeanList = $tmpCustomBeanList;
        $objectList = $tmpObjectList;
        $customObjectList = $tmpCustomObjectList;
        $beanFiles = $tmpBeanFiles;
        $customBeanFiles = $tmpCustomBeanFiles;
    }
}
