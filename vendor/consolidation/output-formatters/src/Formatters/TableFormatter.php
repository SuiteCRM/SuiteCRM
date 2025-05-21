<?php

namespace Consolidation\OutputFormatters\Formatters;

use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Helper\Table;
use Symfony\Component\Console\Helper\TableStyle;
use Consolidation\OutputFormatters\Validate\ValidDataTypesInterface;
use Consolidation\OutputFormatters\Options\FormatterOptions;
use Consolidation\OutputFormatters\Validate\ValidDataTypesTrait;
use Consolidation\OutputFormatters\StructuredData\TableDataInterface;
use Consolidation\OutputFormatters\Transformations\ReorderFields;
use Consolidation\OutputFormatters\Exception\IncompatibleDataException;
use Consolidation\OutputFormatters\Transformations\WordWrapper;
use Consolidation\OutputFormatters\Formatters\HumanReadableFormat;

/**
 * Display a table of data with the Symfony Table class.
 *
 * This formatter takes data of either the RowsOfFields or
 * PropertyList data type.  Tables can be rendered with the
 * rows running either vertically (the normal orientation) or
 * horizontally.  By default, associative lists will be displayed
 * as two columns, with the key in the first column and the
 * value in the second column.
 */
class TableFormatter implements FormatterInterface, ValidDataTypesInterface, RenderDataInterface, MetadataFormatterInterface, HumanReadableFormat
{
    use ValidDataTypesTrait;
    use RenderTableDataTrait;
    use MetadataFormatterTrait;

    protected $fieldLabels;
    protected $defaultFields;

    public function __construct()
    {
    }

    public function validDataTypes()
    {
        return
            [
                new \ReflectionClass('\Consolidation\OutputFormatters\StructuredData\RowsOfFields'),
                new \ReflectionClass('\Consolidation\OutputFormatters\StructuredData\PropertyList')
            ];
    }

    /**
     * @inheritdoc
     */
    public function validate($structuredData)
    {
        // If the provided data was of class RowsOfFields
        // or PropertyList, it will be converted into
        // a TableTransformation object by the restructure call.
        if (!$structuredData instanceof TableDataInterface) {
            throw new IncompatibleDataException(
                $this,
                $structuredData,
                $this->validDataTypes()
            );
        }
        return $structuredData;
    }

    /**
     * @inheritdoc
     */
    public function write(OutputInterface $output, $tableTransformer, FormatterOptions $options)
    {
        $headers = [];
        $defaults = [
            FormatterOptions::TABLE_STYLE => 'consolidation',
            FormatterOptions::INCLUDE_FIELD_LABELS => true,
            FormatterOptions::TABLE_EMPTY_MESSAGE => false,
        ];

        $table = new Table($output);

        static::addCustomTableStyles($table);

        $table->setStyle($options->get(FormatterOptions::TABLE_STYLE, $defaults));
        $isList = $tableTransformer->isList();
        $includeHeaders = $options->get(FormatterOptions::INCLUDE_FIELD_LABELS, $defaults);
        $listDelimiter = $options->get(FormatterOptions::LIST_DELIMITER, $defaults);
        $emptyMessage = $options->get(FormatterOptions::TABLE_EMPTY_MESSAGE, $defaults);

        $headers = $tableTransformer->getHeaders();
        $data = $tableTransformer->getTableData($includeHeaders && $isList);

        // Do not print table headers for an empty table if the
        // TABLE_EMPTY_MESSAGE has been set.
        if (empty($data) && ($emptyMessage !== false)) {
            if (!empty($emptyMessage)) {
                $output->writeln($emptyMessage);
            }
            return;
        }

        if ($listDelimiter) {
            if (!empty($headers)) {
                array_splice($headers, 1, 0, ':');
            }
            $data = array_map(function ($item) {
                array_splice($item, 1, 0, ':');
                return $item;
            }, $data);
        }

        if ($includeHeaders && !$isList) {
            $table->setHeaders($headers);
        }

        // todo: $output->getFormatter();
        $data = $this->wrap($headers, $data, $table->getStyle(), $options);
        $table->setRows($data);
        $table->render();
    }

    /**
     * Wrap the table data
     * @param array $data
     * @param TableStyle $tableStyle
     * @param FormatterOptions $options
     * @return array
     */
    protected function wrap($headers, $data, TableStyle $tableStyle, FormatterOptions $options)
    {
        $wrapper = new WordWrapper($options->get(FormatterOptions::TERMINAL_WIDTH));
        $wrapper->setPaddingFromStyle($tableStyle);
        if (!empty($headers)) {
            $headerLengths = array_map(function ($item) {
                return strlen($item);
            }, $headers);
            $wrapper->setMinimumWidths($headerLengths);
        }
        return $wrapper->wrap($data);
    }

    /**
     * Add our custom table style(s) to the table.
     */
    protected static function addCustomTableStyles($table)
    {
        // The 'consolidation' style is the same as the 'symfony-style-guide'
        // style, except it maintains the colored headers used in 'default'.
        $consolidationStyle = new TableStyle();

        if (method_exists($consolidationStyle, 'setHorizontalBorderChars')) {
            $consolidationStyle
                ->setHorizontalBorderChars('-')
                ->setVerticalBorderChars(' ')
                ->setDefaultCrossingChar(' ')
            ;
        } else {
            $consolidationStyle
                ->setHorizontalBorderChar('-')
                ->setVerticalBorderChar(' ')
                ->setCrossingChar(' ')
            ;
        }
        $table->setStyleDefinition('consolidation', $consolidationStyle);
    }
}
