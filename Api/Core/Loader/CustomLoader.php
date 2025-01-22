<?php
/**
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

namespace Api\Core\Loader;

use Exception;
use LoggerManager;
use Slim\App;

/**
 * CustomLoader
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class CustomLoader
{
    public const ERR_NO_ERROR = 0;
    public const ERR_FILE_NOT_FOUND = 1;
    public const ERR_ROUTE_FILE_NOT_FOUND = 2;
    public const ERR_WRONG_CUSTOM_FORMAT = 3;
    
    /**
     *
     * @var int
     */
    protected static $lastError = self::ERR_NO_ERROR;
    
    /**
     *
     * @var string
     */
    protected static $customPath = 'custom/application/Ext/Api/V8/';
    
    public static function setCustomPath($customPath = 'custom/application/Ext/Api/V8/')
    {
        self::$customPath = $customPath;
    }
    
    public static function getCustomPath()
    {
        return self::$customPath;
    }
    
    /**
     *
     * @return int
     */
    public static function getLastError()
    {
        $ret = self::$lastError;
        self::$lastError = self::ERR_NO_ERROR;
        return $ret;
    }
    
    /**
     * merge multidimensional arrays
     *
     * @param array $arrays
     * @return array
     */
    public static function arrayMerge($arrays)
    {
        $result = [];
        foreach ((array)$arrays as $array) {
            foreach ($array as $key => $value) {
                if (is_int($key)) {
                    // is indexed?
                    $result[] = $value;
                } elseif (isset($result[$key]) && is_array($value) && is_array($result[$key])) {
                    // is associative?
                    $result[$key] = self::arrayMerge([$result[$key], $value]);
                } else {
                    $result[$key] = $value;
                }
            }
        }
        return $result;
    }
    
    /**
     * include and merge custom arrays (custom file should return an array)
     *
     * @param array $array
     * @param string $customFile
     * @return array
     * @throws Exception
     */
    public static function mergeCustomArray($array, $customFile)
    {
        self::getLastError();
        $customFile = self::$customPath . $customFile;
        if (!file_exists($customFile)) {
            self::$lastError = self::ERR_FILE_NOT_FOUND;
            LoggerManager::getLogger()->debug('Custom file is not exists: ' . $customFile);
        } else {
            $customs = include $customFile;
            if (!is_array($customs)) {
                throw new Exception('Custom file should return an array.', self::ERR_WRONG_CUSTOM_FORMAT);
            }
            $array = self::arrayMerge([$array, $customs]);
        }
        
        return $array;
    }
    

    /**
     *
     * @param App $app
     * @return App
     */
    public static function loadCustomRoutes(App $app, $customRoutesFile = 'Config/routes.php')
    {
        self::getLastError();
        $customRoutesFile = self::$customPath . $customRoutesFile;
        if (!file_exists($customRoutesFile)) {
            self::$lastError = self::ERR_ROUTE_FILE_NOT_FOUND;
            LoggerManager::getLogger()->debug('Custom routes file is not exists: ' . $customRoutesFile);
        } else {
            include $customRoutesFile;
        }
        return $app;
    }
}
