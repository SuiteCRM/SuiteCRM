<?php

namespace Consolidation\OutputFormatters\Transformations;

use Consolidation\OutputFormatters\Options\FormatterOptions;

class UnstructuredDataListTransformation extends \ArrayObject implements StringTransformationInterface
{
    protected $originalData;

    public function __construct($data, $fields)
    {
        $this->originalData = $data;
        $rows = static::transformRows($data, $fields);
        parent::__construct($rows);
    }

    protected static function transformRows($data, $fields)
    {
        $rows = [];
        foreach ($data as $rowid => $row) {
            $rows[$rowid] = UnstructuredDataTransformation::transformRow($row, $fields);
        }
        return $rows;
    }

    public function simplifyToString(FormatterOptions $options)
    {
        $result = '';
        $iterator = $this->getIterator();
        while ($iterator->valid()) {
            $simplifiedRow = UnstructuredDataTransformation::simplifyRow($iterator->current());
            if (isset($simplifiedRow)) {
                $result .= "$simplifiedRow\n";
            }

            $iterator->next();
        }
        return $result;
    }
}
