<?php

namespace Consolidation\OutputFormatters\Formatters;

use Consolidation\OutputFormatters\Options\FormatterOptions;
use Consolidation\OutputFormatters\Validate\ValidationInterface;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Output\StreamOutput;
use Symfony\Component\VarDumper\Cloner\VarCloner;
use Symfony\Component\VarDumper\Dumper\CliDumper;

/**
 * Var_dump formatter
 *
 * Run provided data through Symfony VarDumper component.
 */
class VarDumpFormatter implements FormatterInterface, ValidationInterface
{
    /**
     * @inheritdoc
     */
    public function write(OutputInterface $output, $data, FormatterOptions $options)
    {
        $dumper = new CliDumper();
        $cloned_data = (new VarCloner())->cloneVar($data);

        if ($output instanceof StreamOutput) {
            // When stream output is used the dumper is smart enough to
            // determine whether or not to apply colors to the dump.
            // @see Symfony\Component\VarDumper\Dumper\CliDumper::supportsColors
            $dumper->dump($cloned_data, $output->getStream());
        } else {
            // @todo Use dumper return value to get output once we stop support
            // VarDumper v2.
            $stream = fopen('php://memory', 'r+b');
            $dumper->dump($cloned_data, $stream);
            $output->writeln(stream_get_contents($stream, -1, 0));
            fclose($stream);
        }
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
