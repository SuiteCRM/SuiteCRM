<?php

namespace Consolidation\OutputFormatters\Formatters;

use Consolidation\OutputFormatters\Options\FormatterOptions;
use Consolidation\OutputFormatters\Validate\ValidationInterface;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * Var_export formatter
 *
 * Run provided date thruogh var_export.
 */
class VarExportFormatter implements FormatterInterface, ValidationInterface
{
    /**
     * @inheritdoc
     */
    public function write(OutputInterface $output, $data, FormatterOptions $options)
    {
        $output->writeln(var_export($data, true));
    }

    public function isValidDataType(\ReflectionClass $dataType)
    {
        return true;
    }

    public function validate($structuredData)
    {
        return $structuredData;
    }
}
