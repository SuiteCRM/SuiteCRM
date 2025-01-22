<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SuiteCRM\Utility\StringValidator;

class StringValidatorTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var \UnitTester
     */
    protected $tester;

    public function testStartsWith(): void
    {
        $testString = 'foobarbaz';
        self::assertTrue(StringValidator::startsWith($testString, 'foo'));
        self::assertFalse(StringValidator::startsWith($testString, 'bar'));
    }

    public function testEndsWith(): void
    {
        $testString = 'foobarbaz';
        self::assertTrue(StringValidator::endsWith($testString, 'baz'));
        self::assertFalse(StringValidator::endsWith($testString, 'bar'));
    }
}
