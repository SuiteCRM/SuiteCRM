<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/*
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

require_once 'data/SugarBean.php';

/**
 * Factory to create SugarBeans.
 *
 * @api
 */
#[\AllowDynamicProperties]
class BeanFactory
{
    /**
     * @var array
     */
    protected static $loadedBeans = [];

    /**
     * @var array
     */
    protected static $shallowBeans = [];

    /**
     * @var int
     */
    protected static $maxLoaded = 10;

    /**
     * @var int
     */
    protected static $total = 0;

    /**
     * @var array
     */
    protected static $loadOrder = [];

    /**
     * @var array
     */
    protected static $touched = [];

    /**
     * @var int
     */
    public static $hits = 0;

    /**
     * Returns a SugarBean object by id.
     * The Last 10 loaded beans are cached in memory to prevent multiple retrieves per request.
     * If no id is passed, a new bean is created.
     *
     * @static
     *
     * @param string $module
     * @param string $id
     * @param array $params
     *        A name/value array of parameters. Names: encode, deleted.
     *        If $params is boolean we revert to the old arguments (encode, deleted), and use $params as $encode.
     *        This will be changed to using only $params in later versions.
     * @param bool $deleted
     *        @see SugarBean::retrieve
     *
     * @return SugarBean|bool
     */
    public static function getBean($module, $id = null, $params = [], $deleted = true)
    {
        $params = self::convertParams($params);
        $encode = self::hasEncodeFlag($params);
        $deleted = self::hasDeletedFlag($params, $deleted);

        self::initBeanRegistry($module);

        $beanClass = self::getBeanClass($module);

        if (!self::loadBeanFile($module)) {
            return false;
        }

        if (empty($id)) {
            return new $beanClass();
        }

        if (empty(self::$loadedBeans[$module][$id])) {
            /* @var SugarBean $bean */
            $bean = new $beanClass();

            $result = $bean->retrieve($id, $encode, $deleted);

            if ($result === null) {
                return false;
            }

            self::registerBean($module, $bean, $id);

            return $bean;
        }

        ++self::$hits;

        ++self::$touched[$module][$id];

        $bean = self::$loadedBeans[$module][$id];

        if ($deleted && $bean->deleted) {
            return false;
        }

        return $bean;
    }

    /**
     * Returns a SugarBean object by id.
     *
     * @static
     *
     * @param string $module
     * @param string $id
     * @param array $params
     *        A name/value array of parameters. Names: encode, deleted.
     *        If $params is boolean we revert to the old arguments (encode, deleted), and use $params as $encode.
     *        This will be changed to using only $params in later versions.
     * @param bool $deleted
     *        @see SugarBean::retrieve
     *
     * @return SugarBean|bool
     */
    public static function getReloadedBean($module, $id = null, $params = [], $deleted = true)
    {
        $params = self::convertParams($params);
        $encode = self::hasEncodeFlag($params);
        $deleted = self::hasDeletedFlag($params, $deleted);

        $beanClass = self::getBeanClass($module);

        if (!self::loadBeanFile($module)) {
            return false;
        }

        if (empty($id)) {
            return new $beanClass();
        }

        /* @var SugarBean $bean */
        $bean = new $beanClass();

        $result = $bean->retrieve($id, $encode, $deleted);

        if ($result === null) {
            return false;
        }

        return $bean;
    }


    /**
     * Shallow beans are created by SugarBean during the fill_in_relationship_fields method, and they differ from
     * 'complete' bean in that they do not have their own relate fields completed.
     *
     * We can use these beans for filling relate fields, but we should not be caching them and serving them anywhere
     * else.
     *
     * @param $module
     * @param null $id
     * @param array $params
     * @param bool $deleted
     * @return bool|mixed|SugarBean
     */
    public static function getShallowBean($module, $id = null, $params = array(), $deleted = true)
    {
        if (isset(self::$loadedBeans[$module][$id])) {
            return self::getBean($module, $id, $params, $deleted);
        }
        $key = $module . $id;
        if (isset(self::$shallowBeans[$key])) {
            return self::$shallowBeans[$key];
        }
        if (count(self::$shallowBeans) > self::$maxLoaded) {
            array_shift(self::$shallowBeans);
        }
        $bean = self::getBean($module, $id, $params, $deleted);
        self::$shallowBeans[$key] = $bean;
        self::unregisterBean($module, $id);
        return $bean;
    }

    /**
     * @param array|bool $params
     * @return array
     */
    protected static function convertParams($params)
    {
        if (!is_array($params)) {
            $params = [
                'encode' => $params,
            ];
        }

        return $params;
    }

    /**
     * initialises loadedBeans and touched registry arrays.
     *
     * @param string $module
     *
     * @return void
     */
    protected static function initBeanRegistry($module)
    {
        if (!isset(self::$loadedBeans[$module])) {
            self::$loadedBeans[$module] = [];

            self::$touched[$module] = [];
        }
    }

    /**
     * Pulls encoded flag from params array if set or true if not.
     *
     * @param array $params
     *
     * @return bool|mixed
     */
    protected static function hasEncodeFlag($params)
    {
        return isset($params['encode'])
            ? $params['encode']
            : true;
    }

    /**
     * Pulls deleted flag from params array if set or the one given if not.
     *
     * @param array $params
     * @param bool|mixed $deleted
     *
     * @return bool
     */
    protected static function hasDeletedFlag($params, $deleted)
    {
        return isset($params['deleted'])
            ? $params['deleted']
            : $deleted;
    }

    /**
     * Gets a new bean for given module.
     *
     * @param $module
     *
     * @return SugarBean|bool
     */
    public static function newBean($module)
    {
        return self::getBean($module);
    }

    /**
     * Gets the bean meta info for given module.
     *
     * @param $module
     *
     * @return array
     */
    public static function getBeanMeta($module)
    {
        return [
            'moduleName' => $module,
            'beanName' => self::getBeanName($module),
            'customBeanName' => self::getCustomBeanName($module),
            'beanClass' => self::getBeanClass($module),
            'objectName' => self::getObjectName($module),
            'customObjectName' => self::getCustomObjectName($module),
            'classFile' => self::getBeanFile($module),
            'customClassFile' => self::getCustomBeanFile($module),
        ];
    }

    /**
     * Gets the core bean name/class for given module or false.
     *
     * @param $module
     *
     * @return string|bool
     */
    public static function getBeanName($module)
    {
        global $beanList;

        if (empty($beanList[$module])) {
            return false;
        }

        return $beanList[$module];
    }

    /**
     * Gets custom bean name/class for given module or false.
     *
     * @param $module
     *
     * @return string|bool
     */
    public static function getCustomBeanName($module)
    {
        global $customBeanList;

        if (empty($customBeanList[$module])) {
            return false;
        }

        return $customBeanList[$module];
    }

    /**
     * Gets custom bean class if exists or core if not for given module.
     *
     * @param $module
     *
     * @return string|bool
     */
    public static function getBeanClass($module)
    {
        global $customBeanList;

        if (empty($customBeanList[$module])) {
            return self::getBeanName($module);
        }

        return $customBeanList[$module];
    }

    /**
     * Returns the core object name / dictionary key for a given module.
     * This should normally be the same as the bean name, but may not for special case modules (ex. Case vs aCase).
     *
     * @param string $module
     *
     * @return string|bool
     */
    public static function getObjectName($module)
    {
        global $objectList;

        if (empty($objectList[$module])) {
            return self::getBeanName($module);
        }

        return $objectList[$module];
    }

    /**
     * Returns the custom object name / dictionary key for a given module.
     * This should normally be the same as the bean name, but may not for special case modules (ex. Case vs aCase).
     *
     * @param string $module
     *
     * @return string|bool
     */
    public static function getCustomObjectName($module)
    {
        global $customObjectList;

        if (empty($customObjectList[$module])) {
            return self::getCustomBeanName($module);
        }

        return $customObjectList[$module];
    }

    /**
     * Gets core bean file path as string for given module or false.
     *
     * @param string $module
     *
     * @return string|bool
     */
    public static function getBeanFile($module)
    {
        global $beanFiles;

        $beanClass = self::getBeanName($module);

        if (!empty($beanFiles[$beanClass])) {
            return $beanFiles[$beanClass];
        }

        return false;
    }

    /**
     * Gets custom bean file path as string for given module or false.
     *
     * @param string $module
     *
     * @return string|bool
     */
    public static function getCustomBeanFile($module)
    {
        global $customBeanFiles;

        $beanClass = self::getBeanName($module);

        $customBeanClass = self::getBeanClass($module);

        if (!empty($customBeanFiles[$customBeanClass])) {
            return $customBeanFiles[$customBeanClass];
        }

        if (!empty($customBeanFiles[$beanClass])) {
            return $customBeanFiles[$beanClass];
        }

        return false;
    }

    /**
     * Loads core bean class and then custom bean class if exists for given module.
     *
     * @param string $module
     *
     * @return bool
     */
    public static function loadBeanFile($module)
    {
        global $log;

        $beanFile = self::getBeanFile($module);

        if (empty($beanFile)) {
            $log->warn('Cannot find bean file for module: ' . $module);

            return false;
        }

        if (!file_exists($beanFile)) {
            $log->fatal('Bean file does not exist in path: ' . $beanFile);

            return false;
        }

        $customBeanFile = self::getCustomBeanFile($module);

        if (!empty($customBeanFile) && !file_exists($customBeanFile)) {
            $log->fatal('Custom Bean file does not exist in path: ' . $customBeanFile);

            return false;
        }

        require_once $beanFile;

        if (!empty($customBeanFile)) {
            require_once $customBeanFile;
        }

        return true;
    }

    /**
     * This function registers a bean with the bean factory so that it can be access from across the code without doing
     * multiple retrieves. Beans should be registered as soon as they have an id.
     *
     * @param string $module
     * @param SugarBean $bean
     * @param string|bool $id
     *
     * @return bool
     */
    public static function registerBean($module, $bean, $id = false)
    {
        global $beanList;

        if (empty($beanList[$module])) {
            return false;
        }

        if (!isset(self::$loadedBeans[$module])) {
            self::$loadedBeans[$module] = [];
        }

        //Do not double register a bean
        if (!empty($id) && isset(self::$loadedBeans[$module][$id])) {
            return true;
        }

        $info = [];

        $index = 'i'.(self::$total % self::$maxLoaded);

        //We should only hold a limited number of beans in memory at a time.
        //Once we have the max, unload the oldest bean.
        if (count(self::$loadOrder) >= self::$maxLoaded - 1) {
            for ($i = 0; $i < self::$maxLoaded; ++$i) {
                if (!isset(self::$loadOrder[$index])) {
                    break;
                }
                $info = self::$loadOrder[$index];

                // If a bean isn't in the database yet, we need to hold onto it.
                $beanInSave = !empty(self::$loadedBeans[$info['module']][$info['id']]->in_save);

                // Beans that have been used recently should be held in memory if possible
                $beanInMemory = (
                    !empty(self::$touched[$info['module']][$info['id']])
                    && self::$touched[$info['module']][$info['id']] > 0
                );

                if (!$beanInSave && !$beanInMemory) {
                    break;
                }

                if ($beanInMemory) {
                    --self::$touched[$info['module']][$info['id']];
                }

                ++self::$total;

                $index = 'i'.(self::$total % self::$maxLoaded);
            }

            if (isset(self::$loadOrder[$index])) {
                unset(
                    self::$loadedBeans[$info['module']][$info['id']],
                    self::$touched[$info['module']][$info['id']],
                    self::$loadOrder[$index]
                );
            }
        }

        if (!empty($bean->id)) {
            $id = $bean->id;
        }

        if ($id) {
            self::$loadedBeans[$module][$id] = $bean;

            ++self::$total;

            self::$loadOrder[$index] = [
                'module' => $module,
                'id' => $id,
            ];

            self::$touched[$module][$id] = 0;

            return true;
        }

        return false;
    }

    /**
     * Clears a bean from cache so that it will be retrieved from DB next time
     *
     * @param string $module
     * @param string $id
     *
     * @return bool
     */
    public static function unregisterBean($module, $id)
    {
        if (empty($id)) {
            return false;
        }

        if (!isset(self::$loadedBeans[$module][$id])) {
            return false;
        }

        unset(
            self::$loadedBeans[$module][$id],
            self::$touched[$module][$id]
        );

        return true;
    }
}
