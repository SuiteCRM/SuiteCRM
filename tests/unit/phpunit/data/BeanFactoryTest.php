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

use SuiteCRM\Test\BeanFactoryTestCase;

/**
 * Class BeanFactoryTest
 * TODO: BeanFactoryTest::convertParams
 * TODO: BeanFactoryTest::testInitBeanRegistry
 * TODO: BeanFactoryTest::testHasEncodeFlag
 * TODO: BeanFactoryTest::testHasDeletedFlag
 * TODO: BeanFactoryTest::testRegisterBean
 * TODO: BeanFactoryTest::testUnregisterBean
 *
 */
class BeanFactoryTest extends BeanFactoryTestCase
{
    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     * @return void
     */
    public function testNewBean($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $bean = BeanFactory::newBean($moduleName);
        self::assertNotFalse($bean, 'Unable to get core bean for module: ' . $moduleName);
        self::assertInstanceOf(
            $moduleConfig['className'],
            $bean,
            'Loaded bean not instance of core class: ' . $moduleConfig['className']
        );

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $customBean = BeanFactory::newBean($moduleName);
        self::assertNotFalse($customBean, 'Unable to get custom bean for module: ' . $moduleName);
        self::assertInstanceOf(
            $refreshedConfig['customClassName'],
            $customBean,
            'Loaded bean not instance of custom class: ' . $refreshedConfig['customClassName']
        );

        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);
    }


    /**
     * @dataProvider moduleConfigProvider

     *
     * @param string $moduleName
     * @param array $moduleConfig
     * @return void
     */
    public function testGetBean($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $bean = BeanFactory::getBean($moduleName);
        self::assertNotFalse($bean, 'Unable to get core test record bean for module: ' . $moduleName);
        self::assertInstanceOf(
            $moduleConfig['className'],
            $bean,
            'Loaded bean not instance of core class: ' . $moduleConfig['className']
        );

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $customBean = BeanFactory::getBean($moduleName);
        self::assertNotFalse($customBean, 'Unable to get custom test record bean for module: ' . $moduleName);
        self::assertInstanceOf(
            $refreshedConfig['customClassName'],
            $customBean,
            'Loaded bean not instance of custom class: ' . $refreshedConfig['customClassName']
        );

        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);
    }

    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     * @return void
     */
    public function testGetBeanMeta($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $beanMeta = BeanFactory::getBeanMeta($moduleName);
        self::assertEquals($moduleConfig['className'], $beanMeta['beanName']);
        self::assertEquals($moduleConfig['className'], $beanMeta['beanClass']);
        self::assertEquals($moduleConfig['objectName'], $beanMeta['objectName']);
        self::assertEquals($moduleConfig['classFile'], $beanMeta['classFile']);

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $customBeanMeta = BeanFactory::getBeanMeta($moduleName);
        self::assertEquals($refreshedConfig['customClassName'], $customBeanMeta['customBeanName']);
        self::assertEquals($refreshedConfig['customClassName'], $customBeanMeta['beanClass']);
        self::assertEquals($refreshedConfig['customObjectName'], $customBeanMeta['customObjectName']);
        self::assertEquals($refreshedConfig['customClassFile'], $customBeanMeta['customClassFile']);

        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);
    }

    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     * @return void
     */
    public function testGetBeanClass($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $coreBeanClass = BeanFactory::getBeanClass($moduleName);
        self::assertEquals($moduleConfig['className'], $coreBeanClass);

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $customBeanClass = BeanFactory::getBeanClass($moduleName);
        self::assertEquals($refreshedConfig['customClassName'], $customBeanClass);

        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);
    }

    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     * @return void
     */
    public function testGetBeanName($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $beanName = BeanFactory::getBeanName($moduleName);
        self::assertEquals($moduleConfig['className'], $beanName);

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $beanName = BeanFactory::getBeanName($moduleName);
        self::assertNotEquals($refreshedConfig['customClassName'], $beanName);

        $this->subTestGetCustomBeanName(compact('moduleName', 'refreshedConfig'));
    }

    /**
     * @depends testGetBeanName
     *
     * @param array $meta
     * @return void
     */
    public function subTestGetCustomBeanName($meta): void
    {
        $customBeanName = BeanFactory::getCustomBeanName($meta['moduleName']);
        self::assertEquals($meta['refreshedConfig']['customClassName'], $customBeanName);

        $this->removeCoreModuleExtension($meta['moduleName'], $meta['refreshedConfig']['className']);
    }

    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     * @return void
     */
    public function testGetObjectName($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $objectName = BeanFactory::getObjectName($moduleName);
        self::assertEquals($moduleConfig['objectName'], $objectName);

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $objectName = BeanFactory::getObjectName($moduleName);
        self::assertNotEquals($refreshedConfig['customObjectName'], $objectName);

        $this->subTestGetCustomObjectName(compact('moduleName', 'refreshedConfig'));
    }

    /**
     * @depends testGetObjectName
     *
     * @param array $meta
     * @return void
     */
    public function subTestGetCustomObjectName($meta): void
    {
        $customObjectName = BeanFactory::getCustomObjectName($meta['moduleName']);
        self::assertEquals($meta['refreshedConfig']['customObjectName'], $customObjectName);

        $this->removeCoreModuleExtension($meta['moduleName'], $meta['refreshedConfig']['className']);
    }

    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     *
     * @return void
     */
    public function testGetBeanFile($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $beanFile = BeanFactory::getBeanFile($moduleName);
        self::assertEquals($moduleConfig['classFile'], $beanFile);

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $beanFile = BeanFactory::getBeanFile($moduleName);
        self::assertNotEquals($refreshedConfig['customClassFile'], $beanFile);

        $this->subTestGetCustomBeanFile(compact('moduleName', 'refreshedConfig'));
    }

    /**
     * @depends testGetBeanFile
     *
     * @param array $meta
     * @return void
     */
    public function subTestGetCustomBeanFile($meta): void
    {
        $customBeanFile = BeanFactory::getCustomBeanFile($meta['moduleName']);
        self::assertEquals($meta['refreshedConfig']['customClassFile'], $customBeanFile);

        $this->removeCoreModuleExtension($meta['moduleName'], $meta['refreshedConfig']['className']);
    }

    /**
     * @dataProvider moduleConfigProvider
     *
     * @param string $moduleName
     * @param array $moduleConfig
     */
    public function testLoadBeanFile($moduleName, $moduleConfig): void
    {
        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);

        $coreFileLoaded = BeanFactory::loadBeanFile($moduleName);
        self::assertTrue($coreFileLoaded);
        self::assertTrue(class_exists($moduleConfig['className']));

        $this->addCoreModuleExtension($moduleName, $moduleConfig['className']);
        $refreshedConfig = $this->moduleConfigProvider()[$moduleName]['meta'];

        $customFileLoaded = BeanFactory::loadBeanFile($moduleName);
        self::assertTrue($customFileLoaded);
        self::assertTrue(class_exists($refreshedConfig['customClassName']));

        $this->removeCoreModuleExtension($moduleName, $moduleConfig['className']);
    }
}
