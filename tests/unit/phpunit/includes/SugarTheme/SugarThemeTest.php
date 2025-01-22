<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class SugarThemeTest extends SuitePHPUnitFrameworkTestCase
{
    public function testGetMimeType(): void
    {
        $theme = SugarThemeRegistry::current();
        self::assertEquals('image/svg+xml', $theme->getMimeType('svg'));
        self::assertEquals('image/gif', $theme->getMimeType('gif'));
        self::assertEquals('image/png', $theme->getMimeType('png'));
        self::assertEquals(null, $theme->getMimeType('notanextension'));
    }
}
