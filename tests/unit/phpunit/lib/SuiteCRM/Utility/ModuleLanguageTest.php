<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ModuleLanguageTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var \UnitTester
     */
    protected $tester;

    /**
     * @var \SuiteCRM\Utility\ModuleLanguage $paths
     */
    private static $language;


    protected function setUp(): void
    {
        parent::setUp();
        if (self::$language === null) {
            self::$language = new \SuiteCRM\Utility\ModuleLanguage();
        }
    }

    public function testGetCurrentLanguage(): void
    {
        $language = self::$language->getModuleLanguageStrings(new \SuiteCRM\Utility\CurrentLanguage(), 'Accounts');
        self::assertNotEmpty($language);
        self::assertArrayHasKey('LBL_MODULE_NAME', $language);
    }
}
