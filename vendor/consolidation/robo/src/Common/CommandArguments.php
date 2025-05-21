<?php

namespace Robo\Common;

use Robo\Common\ProcessUtils;

/**
 * Use this to add arguments and options to the $arguments property.
 */
trait CommandArguments
{
    /**
     * @var string
     */
    protected $arguments = '';

    /**
     * Pass argument to executable. Its value will be automatically escaped.
     *
     * @param string $arg
     *
     * @return $this
     */
    public function arg($arg)
    {
        return $this->args($arg);
    }

    /**
     * Pass methods parameters as arguments to executable. Argument values
     * are automatically escaped.
     *
     * @param string|string[] $args
     *
     * @return $this
     */
    public function args($args)
    {
        $func_args = func_get_args();
        if (!is_array($args)) {
            $args = $func_args;
        }
        $this->arguments .= ' ' . implode(' ', array_map([static::class, 'escape'], $args));
        return $this;
    }

    /**
     * Pass the provided string in its raw (as provided) form as an argument to executable.
     *
     * @param string $arg
     *
     * @return $this
     */
    public function rawArg($arg)
    {
        $this->arguments .= " $arg";

        return $this;
    }

    /**
     * Escape the provided value, unless it contains only alphanumeric
     * plus a few other basic characters.
     *
     * @param string $value
     *
     * @return string
     */
    public static function escape($value)
    {
        if (preg_match('/^[a-zA-Z0-9\/\.@~_-]+$/', $value)) {
            return $value;
        }
        return ProcessUtils::escapeArgument($value);
    }

    /**
     * Pass option to executable. Options are prefixed with `--` , value can be provided in second parameter.
     * Option values are automatically escaped.
     *
     * @param string $option
     * @param string $value
     * @param string $separator
     *
     * @return $this
     */
    public function option($option, $value = null, $separator = ' ')
    {
        if ($option !== null and strpos($option, '-') !== 0) {
            $option = "--$option";
        }
        $this->arguments .= null == $option ? '' : " " . $option;
        $this->arguments .= null == $value ? '' : $separator . static::escape($value);
        return $this;
    }

    /**
     * Pass multiple options to executable. The associative array contains
     * the key:value pairs that become `--key value`, for each item in the array.
     * Values are automatically escaped.
     *
     * @param array $options
     * @param string $separator
     *
     * @return $this
     */
    public function options(array $options, $separator = ' ')
    {
        foreach ($options as $option => $value) {
            $this->option($option, $value, $separator);
        }
        return $this;
    }

    /**
     * Pass an option with multiple values to executable. Value can be a string or array.
     * Option values are automatically escaped.
     *
     * @param string $option
     * @param string|array $value
     * @param string $separator
     *
     * @return $this
     */
    public function optionList($option, $value = array(), $separator = ' ')
    {
        if (is_array($value)) {
            foreach ($value as $item) {
                $this->optionList($option, $item, $separator);
            }
        } else {
            $this->option($option, $value, $separator);
        }

        return $this;
    }
}
