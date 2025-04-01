<?php
/**
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

namespace SuiteCRM\Utility;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use InvalidArgumentException;
use Symfony\Component\Yaml\Yaml;

/**
 * Class ArrayMapper maps an array using a mapping definition.
 */
#[\AllowDynamicProperties]
class ArrayMapper
{
    /** @var array|object */
    private $mappable = null;
    /** @var array */
    private $mappings = [];
    /** @var null|array */
    private $regexMappings = [];
    /** @var array */
    private $blacklist = [];
    /** @var bool */
    private $hideEmptyValues = true;

    /** @var array */
    private $path = [];
    /** @var array */
    private $cleanArray = [];

    //region Getters and Setters

    /**
     * @return array
     */
    public function getMappings()
    {
        return $this->mappings;
    }

    /**
     * @param array $mappings
     *
     * @return ArrayMapper fluent setter
     */
    public function setMappings($mappings)
    {
        $this->mappings = $mappings;

        return $this;
    }

    /**
     * @return array
     */
    public function getMappable()
    {
        return $this->mappable;
    }

    /**
     * @param array|object $mappable
     *
     * @return ArrayMapper fluent setter
     */
    public function setMappable(&$mappable)
    {
        if (!is_object($mappable) && !is_array($mappable)) {
            throw new InvalidArgumentException('Argument must be either a an array or an object');
        }

        $this->mappable = $mappable;
        $this->cleanArray = [];

        return $this;
    }

    /**
     * @return array|null
     */
    public function getRegexMappings()
    {
        return $this->regexMappings;
    }

    /**
     * @param mixed[] $regexMappings
     *
     * @return ArrayMapper fluent setter
     */
    public function setRegexMappings(array $regexMappings)
    {
        $this->regexMappings = $regexMappings;

        return $this;
    }

    /**
     * @return array
     */
    public function getBlacklist()
    {
        return $this->blacklist;
    }

    /**
     * @param array $blacklist
     *
     * @return ArrayMapper fluent setter
     */
    public function setBlacklist(array $blacklist)
    {
        $this->blacklist = $blacklist;

        return $this;
    }

    /**
     * @return bool
     */
    public function isHideEmptyValues()
    {
        return $this->hideEmptyValues;
    }

    /**
     * @param bool $hideEmptyValues
     *
     * @return ArrayMapper fluent setter
     */
    public function setHideEmptyValues($hideEmptyValues)
    {
        $this->hideEmptyValues = $hideEmptyValues;

        return $this;
    }
    //endregion

    /**
     * Factory method for cleaner syntax with fluent setters.
     *
     * @return ArrayMapper
     */
    public static function make()
    {
        return new self();
    }

    /**
     * Maps the given array using the given parameters.
     *
     * @param array|null $keys
     *
     * @return array
     */
    public function map(array $keys = null)
    {
        if (is_array($this->mappable)) {
            $this->mapArray($this->mappable, $keys);
        }

        if (is_object($this->mappable)) {
            $this->mapObject($this->mappable, $keys);
        }

        return $this->cleanArray;
    }

    /**
     * Loads configuration from a Yaml file.
     *
     * @param $file
     *
     * @return ArrayMapper fluent setter
     */
    public function loadYaml($file)
    {
        $parse = new Yaml();
        $parsed = $parse->parseFile($file);

        if (isset($parsed['mappings'])) {
            $this->mappings = $parsed['mappings'];
        }

        if (isset($parsed['regexMappings'])) {
            $this->regexMappings = $parsed['regexMappings'];
        }

        if (isset($parsed['blacklist'])) {
            $this->blacklist = $parsed['blacklist'];
        }

        return $this;
    }

    /**
     * Starts mapping an array recursively.
     *
     * @param array      $array
     * @param array|null $keys
     */
    private function mapArray(array $array, array $keys = null)
    {
        if ($keys === null) {
            $keys = array_keys($array);
        }

        foreach ($keys as $key) {
            if (array_key_exists($key, $array)) {
                $this->loop($key, $array[$key]);
            }
        }
    }

    /**
     * Starts mapping an array recursively.
     *
     * @param object     $obj
     * @param array|null $keys
     */
    private function mapObject($obj, array $keys = null)
    {
        if ($keys === null) {
            $keys = array_keys(get_object_vars($obj));
        }

        foreach ($keys as $key) {
            $this->loop($key, !isset($obj->$key) ? null : $obj->$key);
        }
    }

    /**
     * Main loop action
     *
     * @param string $key
     * @param mixed  $value
     */
    private function loop($key, $value)
    {
        $value = $this->fixStringValue($value);

        if ($this->shouldSkipEmpty($value)) {
            return;
        }

        $path = $this->updatePath($key);

        if ($this->isBlackListed($path)) {
            return;
        }

        if ($this->handleStructure($value)) {
            return;
        }

        if ($this->handleMap($value, $path)) {
            return;
        }

        if ($this->handleRegex($value, $path)) {
            return;
        }

        if ($this->handleDefault($value, $path)) {
            return;
        }
    }

    /**
     * Handles an object or an array.
     *
     * @param mixed $structure
     *
     * @return bool
     */
    private function handleStructure($structure)
    {
        if (is_array($structure)) {
            $this->mapArray($structure);
            array_pop($this->path);
            return true;
        }

        if (is_object($structure)) {
            $this->mapObject($structure);
            array_pop($this->path);
            return true;
        }

        return false;
    }

    /**
     * @param mixed  $value
     * @param string $path
     *
     * @return bool
     */
    private function handleMap($value, $path)
    {
        if (!array_key_exists($path, $this->mappings)) {
            return false;
        }

        $mappedPath = $this->mappings[$path];

        $this->handleValue($value, $mappedPath);

        array_pop($this->path);
        return true;
    }

    /**
     * @param mixed  $value
     * @param string $path
     *
     * @return bool
     */
    private function handleRegex($value, $path)
    {
        foreach ($this->regexMappings as $regex => $mappedPath) {
            if (!preg_match($regex, $path, $matches)) {
                continue;
            }

            foreach ($matches as $key => $match) {
                $mappedPath = str_replace("@$key", $match, (string) $mappedPath);
            }

            $this->handleValue($value, $mappedPath);

            array_pop($this->path);
            return true;
        }

        return false;
    }

    /**
     * @param mixed  $value
     * @param string $path
     *
     * @return bool
     */
    private function handleDefault($value, $path)
    {
        $this->handleValue($value, $path);
        array_pop($this->path);
        return true;
    }

    /**
     * @param $key
     *
     * @return string
     */
    private function updatePath($key)
    {
        $this->path[] = $key;

        $path = implode('.', $this->path);
        return $path;
    }

    /**
     * @param $value
     *
     * @return null|string|string[]
     */
    private function fixStringValue($value)
    {
        if (is_string($value)) {
            preg_match_all("/&#?\w+;/", $value, $entities, PREG_SET_ORDER);
            $entities = array_unique(array_column($entities, 0));
            foreach ($entities as $entity) {
                $decoded = mb_convert_encoding($entity, 'UTF-8', 'HTML-ENTITIES');
                $value = str_replace($entity, $decoded, $value);
            }
        }
        return $value;
    }

    /**
     * @param $value
     *
     * @return bool
     */
    private function shouldSkipEmpty($value)
    {
        return $this->hideEmptyValues && ($value === null || $value === '');
    }

    /**
     * Puts a value at the given value.
     *
     * @param mixed  $value
     * @param string $path
     */
    private function putInPath($value, $path)
    {
        $array = &$this->getArrayAtPath($path);

        $array = $value;
    }

    /**
     * @param mixed  $value
     * @param string $path
     */
    private function appendInPath($value, $path)
    {
        $array = &$this->getArrayAtPath($path);

        $array[] = $value;
    }

    /**
     * @param string $path
     *
     * @return array
     */
    private function &getArrayAtPath($path)
    {
        $explode = explode('.', $path);

        $array = &$this->cleanArray;

        foreach ($explode as $node) {
            $array = &$array[$node];
        }

        return $array;
    }

    /**
     * @param $path
     *
     * @return bool
     */
    private function isBlackListed($path)
    {
        $result = in_array($path, $this->blacklist);

        if ($result === false) {
            return false;
        }

        array_pop($this->path);
        return true;
    }

    /**
     * @param $value
     * @param $mappedPath
     */
    private function handleValue($value, $mappedPath)
    {
        if (strpos((string) $mappedPath, '+') === 0) {
            $mappedPath = substr((string) $mappedPath, 1);
            $this->appendInPath($value, $mappedPath);
        } else {
            $this->putInPath($value, $mappedPath);
        }
    }
}
