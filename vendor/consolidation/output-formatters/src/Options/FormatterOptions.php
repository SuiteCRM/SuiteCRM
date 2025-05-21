<?php

namespace Consolidation\OutputFormatters\Options;

use Symfony\Component\Console\Input\InputInterface;
use Consolidation\OutputFormatters\Transformations\PropertyParser;
use Consolidation\OutputFormatters\StructuredData\Xml\XmlSchema;
use Consolidation\OutputFormatters\StructuredData\Xml\XmlSchemaInterface;

/**
 * FormatterOptions holds information that affects the way a formatter
 * renders its output.
 *
 * There are three places where a formatter might get options from:
 *
 * 1. Configuration associated with the command that produced the output.
 *    This is passed in to FormatterManager::write() along with the data
 *    to format.  It might originally come from annotations on the command,
 *    or it might come from another source.  Examples include the field labels
 *    for a table, or the default list of fields to display.
 *
 * 2. Options specified by the user, e.g. by commandline options.
 *
 * 3. Default values associated with the formatter itself.
 *
 * This class caches configuration from sources (1) and (2), and expects
 * to be provided the defaults, (3), whenever a value is requested.
 */
class FormatterOptions
{
    /** var array */
    protected $configurationData = [];
    /** var array */
    protected $options = [];
    /** var InputInterface */
    protected $input;

    const FORMAT = 'format';
    const DEFAULT_FORMAT = 'default-format';
    const TABLE_STYLE = 'table-style';
    const TABLE_EMPTY_MESSAGE = 'table-empty-message';
    const LIST_ORIENTATION = 'list-orientation';
    const FIELDS = 'fields';
    const FIELD = 'field';
    const INCLUDE_FIELD_LABELS = 'include-field-labels';
    const ROW_LABELS = 'row-labels';
    const FIELD_LABELS = 'field-labels';
    const DEFAULT_FIELDS = 'default-fields';
    const DEFAULT_TABLE_FIELDS = 'default-table-fields';
    const DEFAULT_STRING_FIELD = 'default-string-field';
    const DELIMITER = 'delimiter';
    const CSV_ENCLOSURE = 'csv-enclosure';
    const CSV_ESCAPE_CHAR = 'csv-escape-char';
    const LIST_DELIMITER = 'list-delimiter';
    const TERMINAL_WIDTH = 'width';
    const METADATA_TEMPLATE = 'metadata-template';
    const HUMAN_READABLE = 'human-readable';

    /**
     * Create a new FormatterOptions with the configuration data and the
     * user-specified options for this request.
     *
     * @see FormatterOptions::setInput()
     * @param array $configurationData
     * @param array $options
     */
    public function __construct($configurationData = [], $options = [])
    {
        $this->configurationData = $configurationData;
        $this->options = $options;
    }

    /**
     * Create a new FormatterOptions object with new configuration data (provided),
     * and the same options data as this instance.
     *
     * @param array $configurationData
     * @return FormatterOptions
     */
    public function override($configurationData)
    {
        $override = new self();
        $override
            ->setConfigurationData($configurationData + $this->getConfigurationData())
            ->setOptions($this->getOptions());
        return $override;
    }

    public function setTableStyle($style)
    {
        return $this->setConfigurationValue(self::TABLE_STYLE, $style);
    }

    public function setDelimiter($delimiter)
    {
        return $this->setConfigurationValue(self::DELIMITER, $delimiter);
    }

    public function setCsvEnclosure($enclosure)
    {
        return $this->setConfigurationValue(self::CSV_ENCLOSURE, $enclosure);
    }

    public function setCsvEscapeChar($escapeChar)
    {
        return $this->setConfigurationValue(self::CSV_ESCAPE_CHAR, $escapeChar);
    }

    public function setListDelimiter($listDelimiter)
    {
        return $this->setConfigurationValue(self::LIST_DELIMITER, $listDelimiter);
    }



    public function setIncludeFieldLables($includFieldLables)
    {
        return $this->setConfigurationValue(self::INCLUDE_FIELD_LABELS, $includFieldLables);
    }

    public function setListOrientation($listOrientation)
    {
        return $this->setConfigurationValue(self::LIST_ORIENTATION, $listOrientation);
    }

    public function setRowLabels($rowLabels)
    {
        return $this->setConfigurationValue(self::ROW_LABELS, $rowLabels);
    }

    public function setDefaultFields($fields)
    {
        return $this->setConfigurationValue(self::DEFAULT_FIELDS, $fields);
    }

    public function setFieldLabels($fieldLabels)
    {
        return $this->setConfigurationValue(self::FIELD_LABELS, $fieldLabels);
    }

    public function setTableEmptyMessage($emptyMessage)
    {
        return $this->setConfigurationValue(FormatterOptions::TABLE_EMPTY_MESSAGE, $emptyMessage);
    }

    public function setTableDefaultFields($defaultTableFields)
    {
        return $this->setConfigurationValue(FormatterOptions::DEFAULT_TABLE_FIELDS, $defaultTableFields);
    }

    public function setDefaultStringField($defaultStringField)
    {
        return $this->setConfigurationValue(self::DEFAULT_STRING_FIELD, $defaultStringField);
    }

    public function setWidth($width)
    {
        return $this->setConfigurationValue(self::TERMINAL_WIDTH, $width);
    }

    public function setHumanReadable($isHumanReadable = true)
    {
        return $this->setConfigurationValue(self::HUMAN_READABLE, $isHumanReadable);
    }

    /**
     * Get a formatter option
     *
     * @param string $key
     * @param array $defaults
     * @param mixed $default
     * @return mixed
     */
    public function get($key, $defaults = [], $default = false)
    {
        $value = $this->fetch($key, $defaults, $default);
        return $this->parse($key, $value);
    }

    /**
     * Get the fields based on the selections made by the user and
     * the available annotation data. The fields are reported as the
     * user selected them, and therefore may be either the field machine
     * name, or its corresponding human-readable label.
     *
     * @param array $defaults
     * @param mixed $default
     * @return mixed
     */
    public function fields($defaults = [], $default = false)
    {
        $fieldShortcut = $this->get(self::FIELD, $defaults);
        if (!empty($fieldShortcut)) {
            return [$fieldShortcut];
        }
        $result = $this->get(self::FIELDS, $defaults);
        if (!empty($result)) {
            return $result;
        }
        $isHumanReadable = $this->get(self::HUMAN_READABLE, $defaults);
        if ($isHumanReadable) {
            $result = $this->get(self::DEFAULT_TABLE_FIELDS, $defaults);
            if (!empty($result)) {
                return $result;
            }
        }
        return $this->get(self::DEFAULT_FIELDS, $defaults);
    }

    /**
     * Returns 'true' iff the fields selected by the user (or the default
     * fields, if none explicitly selected) contain the specified field name.
     * Note that the provided field name may be either the machine name for
     * the field, or the human-readable field label.
     */
    public function fieldsContain($fieldName)
    {
        $fields = explode(',', $this->fields());

        $fieldAlias = $this->fieldAlias($fieldName);

        return in_array($fieldName, $fields) || in_array($fieldAlias, $fields);
    }

    protected function fieldAlias($fieldName)
    {
        $availableFields = $this->get(FormatterOptions::FIELD_LABELS);
        if (!$availableFields) {
            return $fieldName;
        }

        if (array_key_exists($fieldName, $availableFields)) {
            return $availableFields[$fieldName];
        }

        $availableLabels = array_flip($availableFields);
        if (array_key_exists($fieldName, $availableLabels)) {
            return $availableLabels[$fieldName];
        }

        return $fieldName;
    }

    /**
     * Return the XmlSchema to use with --format=xml for data types that support
     * that.  This is used when an array needs to be converted into xml.
     *
     * @return XmlSchema
     */
    public function getXmlSchema()
    {
        return new XmlSchema();
    }

    /**
     * Determine the format that was requested by the caller.
     *
     * @param array $defaults
     * @return string
     */
    public function getFormat($defaults = [])
    {
        return $this->get(self::FORMAT, [], $this->get(self::DEFAULT_FORMAT, $defaults, ''));
    }

    /**
     * Look up a key, and return its raw value.
     *
     * @param string $key
     * @param array $defaults
     * @param mixed $default
     * @return mixed
     */
    protected function fetch($key, $defaults = [], $default = false)
    {
        $defaults = $this->defaultsForKey($key, $defaults, $default);
        $values = $this->fetchRawValues($defaults);
        return $values[$key];
    }

    /**
     * Reduce provided defaults to the single item identified by '$key',
     * if it exists, or an empty array otherwise.
     *
     * @param string $key
     * @param array $defaults
     * @return array
     */
    protected function defaultsForKey($key, $defaults, $default = false)
    {
        if (array_key_exists($key, $defaults)) {
            return [$key => $defaults[$key]];
        }
        return [$key => $default];
    }

    /**
     * Look up all of the items associated with the provided defaults.
     *
     * @param array $defaults
     * @return array
     */
    protected function fetchRawValues($defaults = [])
    {
        return array_merge(
            $defaults,
            $this->getConfigurationData(),
            $this->getOptions(),
            $this->getInputOptions($defaults)
        );
    }

    /**
     * Given the raw value for a specific key, do any type conversion
     * (e.g. from a textual list to an array) needed for the data.
     *
     * @param string $key
     * @param mixed $value
     * @return mixed
     */
    protected function parse($key, $value)
    {
        $optionFormat = $this->getOptionFormat($key);
        if (!empty($optionFormat) && is_string($value)) {
            return $this->$optionFormat($value);
        }
        return $value;
    }

    /**
     * Convert from a textual list to an array
     *
     * @param string $value
     * @return array
     */
    public function parsePropertyList($value)
    {
        return PropertyParser::parse($value);
    }

    /**
     * Given a specific key, return the class method name of the
     * parsing method for data stored under this key.
     *
     * @param string $key
     * @return string
     */
    protected function getOptionFormat($key)
    {
        $propertyFormats = [
            self::ROW_LABELS => 'PropertyList',
            self::FIELD_LABELS => 'PropertyList',
        ];
        if (array_key_exists($key, $propertyFormats)) {
            return "parse{$propertyFormats[$key]}";
        }
        return '';
    }

    /**
     * Change the configuration data for this formatter options object.
     *
     * @param array $configurationData
     * @return FormatterOptions
     */
    public function setConfigurationData($configurationData)
    {
        $this->configurationData = $configurationData;
        return $this;
    }

    /**
     * Change one configuration value for this formatter option.
     *
     * @param string $key
     * @param mixed $value
     * @return FormatterOptions
     */
    protected function setConfigurationValue($key, $value)
    {
        $this->configurationData[$key] = $value;
        return $this;
    }

    /**
     * Change one configuration value for this formatter option, but only
     * if it does not already have a value set.
     *
     * @param string $key
     * @param mixed $value
     * @return FormatterOptions
     */
    public function setConfigurationDefault($key, $value)
    {
        if (!array_key_exists($key, $this->configurationData)) {
            return $this->setConfigurationValue($key, $value);
        }
        return $this;
    }

    /**
     * Return a reference to the configuration data for this object.
     *
     * @return array
     */
    public function getConfigurationData()
    {
        return $this->configurationData;
    }

    /**
     * Set all of the options that were specified by the user for this request.
     *
     * @param array $options
     * @return FormatterOptions
     */
    public function setOptions($options)
    {
        $this->options = $options;
        return $this;
    }

    /**
     * Change one option value specified by the user for this request.
     *
     * @param string $key
     * @param mixed $value
     * @return FormatterOptions
     */
    public function setOption($key, $value)
    {
        $this->options[$key] = $value;
        return $this;
    }

    /**
     * Return a reference to the user-specified options for this request.
     *
     * @return array
     */
    public function getOptions()
    {
        return $this->options;
    }

    /**
     * Provide a Symfony Console InputInterface containing the user-specified
     * options for this request.
     *
     * @param InputInterface $input
     * @return type
     */
    public function setInput(InputInterface $input)
    {
        $this->input = $input;
    }

    /**
     * Return all of the options from the provided $defaults array that
     * exist in our InputInterface object.
     *
     * @param array $defaults
     * @return array
     */
    public function getInputOptions($defaults)
    {
        if (!isset($this->input)) {
            return [];
        }
        $options = [];
        foreach ($defaults as $key => $value) {
            if ($this->input->hasOption($key)) {
                $result = $this->input->getOption($key);
                if (isset($result)) {
                    $options[$key] = $this->input->getOption($key);
                }
            }
        }
        return $options;
    }
}
