<?php
namespace Consolidation\AnnotatedCommand\Parser;

/**
 * An associative array that maps from key to default value;
 * each entry can also have a description and suggested values.
 */
class DefaultsWithDescriptions
{
    /**
     * @var array Associative array of key : default mappings
     */
    protected $values;

    /**
     * @var array Associative array used like a set to indicate default value
     * exists for the key.
     */
    protected $hasDefault;

    /**
     * @var array Associative array of key : description mappings
     */
    protected $descriptions;

    /**
     * @var array Associative array of key : suggestions mappings
     */
    protected $suggestedValues;

    /**
     * @var mixed Default value that the default value of items in
     * the collection should take when not specified in the 'add' method.
     */
    protected $defaultDefault;

    public function __construct($values = [], $defaultDefault = null)
    {
        $this->values = $values;
        $this->hasDefault = array_filter($this->values, function ($value) {
            return isset($value);
        });
        $this->descriptions = [];
        $this->suggestedValues = [];
        $this->defaultDefault = $defaultDefault;
    }

    /**
     * Return just the key : default values mapping
     *
     * @return array
     */
    public function getValues()
    {
        return $this->values;
    }

    /**
     * Return true if this set of options is empty
     *
     * @return
     */
    public function isEmpty()
    {
        return empty($this->values);
    }

    /**
     * Check to see whether the speicifed key exists in the collection.
     *
     * @param string $key
     * @return boolean
     */
    public function exists($key)
    {
        return array_key_exists($key, $this->values);
    }

    /**
     * Get the value of one entry.
     *
     * @param string $key The key of the item.
     * @return string
     */
    public function get($key)
    {
        if (array_key_exists($key, $this->values)) {
            return $this->values[$key];
        }
        return $this->defaultDefault;
    }

    /**
     * Remove a matching entry, if it exists.
     *
     * @param string $key The key of the value to remove
     * @return string The value of the removed item, or empty
     */
    public function removeMatching($key)
    {
        $key = $this->approximatelyMatchingKey($key);
        if (!$key) {
            return '';
        }
        $result = $this->values[$key];
        unset($this->values[$key]);
        return $result;
    }

    public function approximatelyMatchingKey($key)
    {
        $key = $this->simplifyKey($key);
        foreach ($this->values as $k => $v) {
            if ($key === $this->simplifyKey($k)) {
                return $k;
            }
        }
        return '';
    }

    protected function simplifyKey($key)
    {
        return strtolower(preg_replace('#[-_]#', '', $key));
    }

    /**
     * Get the description of one entry.
     *
     * @param string $key The key of the item.
     * @return string
     */
    public function getDescription($key)
    {
        if (array_key_exists($key, $this->descriptions)) {
            return $this->descriptions[$key];
        }
        return '';
    }

    /**
     * Get the suggested values for an item.
     *
     * @param string $key The key of the item.
     * @return array|\Closure
     */
    public function getSuggestedValues($key)
    {
        if (array_key_exists($key, $this->suggestedValues)) {
            return $this->suggestedValues[$key];
        }
        return [];
    }

    /**
     * Add another argument to this command.
     *
     * @param string $key Name of the argument.
     * @param string $description Help text for the argument.
     * @param mixed $defaultValue The default value for the argument.
     * @param array|\Closure $suggestions Possible values for the argument or option.
     */
    public function add($key, $description = '', $defaultValue = null, $suggestedValues = [])
    {
        if (!$this->exists($key) || isset($defaultValue)) {
            $this->values[$key] = isset($defaultValue) ? $defaultValue : $this->defaultDefault;
        }
        unset($this->descriptions[$key]);
        if (!empty($description)) {
            $this->descriptions[$key] = $description;
        }
        unset($this->suggestedValues[$key]);
        if (!empty($suggestedValues)) {
            $this->suggestedValues[$key] = $suggestedValues;
        }
    }

    /**
     * Change the default value of an entry.
     *
     * @param string $key
     * @param mixed $defaultValue
     */
    public function setDefaultValue($key, $defaultValue)
    {
        $this->values[$key] = $defaultValue;
        $this->hasDefault[$key] = true;
        return $this;
    }

    /**
     * Check to see if the named argument definitively has a default value.
     *
     * @param string $key
     * @return bool
     */
    public function hasDefault($key)
    {
        return array_key_exists($key, $this->hasDefault);
    }

    /**
     * Remove an entry
     *
     * @param string $key The entry to remove
     */
    public function clear($key)
    {
        unset($this->values[$key]);
        unset($this->descriptions[$key]);
        unset($this->suggestedValues[$key]);
    }

    /**
     * Rename an existing option to something else.
     */
    public function rename($oldName, $newName)
    {
        $this->add($newName, $this->getDescription($oldName), $this->get($oldName));
        $this->clear($oldName);
    }
}
