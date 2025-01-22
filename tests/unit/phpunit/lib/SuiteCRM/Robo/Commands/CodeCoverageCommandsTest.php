<?php

use \SuiteCRM\Robo\Plugin\Commands\CodeCoverageCommands;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class CodeCoverageCommandsTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var \UnitTester
     */
    protected $tester;

    /** @var \SuiteCRM\Robo\Plugin\Commands\CodeCoverageCommands **/
    protected static $testClass;

    protected function setUp(): void
    {
        parent::setUp();

        if (self::$testClass === null) {
            self::$testClass = new CodeCoverageCommands();
        }
    }

    public function testIsEnvironmentTravisCI(): void
    {
        $method = (new ReflectionClass(CodeCoverageCommands::class))->getMethod('isEnvironmentTravisCI');
        $method->setAccessible(true);

        $actual = $method->invoke(
            self::$testClass
        );

        $returnType = is_string($actual) || is_array($actual) || is_bool($actual);
        self::assertTrue($returnType);
    }

    public function testGetCommitRangeForTravisCi(): void
    {
        $method = (new ReflectionClass(CodeCoverageCommands::class))->getMethod('getCommitRangeForTravisCi');
        $method->setAccessible(true);

        $actual = $method->invoke(
            self::$testClass
        );

        $returnType = is_string($actual) || is_array($actual) || is_bool($actual);
        self::assertTrue($returnType);
    }

    public function testGetCodeCoverageCommand(): void
    {
        $commandExpected = './vendor/bin/phpunit --configuration ./tests/phpunit.xml.dist --coverage-clover ./tests/_output/coverage.xml ./tests/unit/phpunit';
        // Run tests
        $method = (new ReflectionClass(CodeCoverageCommands::class))->getMethod('getCodeCoverageCommand');
        $method->setAccessible(true);

        $actual = $method->invoke(
            self::$testClass
        );

        self::assertEquals($commandExpected, $actual);
    }
}
