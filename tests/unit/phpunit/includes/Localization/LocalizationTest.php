<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once __DIR__ . '/../../../../../include/Localization/Localization.php';

/**
 * Class LocalizationTest
 */
class LocalizationTest extends SuitePHPUnitFrameworkTestCase
{
    public function testaddBOM(): void
    {
        $local = new Localization();
        $utf8 = 'foo';
        self::assertEquals("\xef\xbb\xbf" . 'foo', $local->addBOM($utf8, 'UTF-8'));
        $utf16le = $local->translateCharset($utf8, 'UTF-8', 'UTF-16LE');
        self::assertEquals("\xFF\xFE" . $utf16le, $local->addBOM($utf16le, 'UTF-16LE'));
        $utf16be = $local->translateCharset($utf8, 'UTF-16LE', 'UTF-16BE');
        self::assertEquals("\xFE\xFF" . $utf16be, $local->addBOM($utf16be, 'UTF-16BE'));

        $this->expectException(UnexpectedValueException::class);
        $local->addBOM('foobar', 'ASCII');
    }
}
