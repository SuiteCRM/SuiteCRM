<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once 'modules/Administration/UpgradeHistory.php';

class UpgradeHistoryTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @dataProvider installedVersionsProvider
     * @param string[] $installedVersions
     * @param string $requiredVersion
     * @param bool $satisfied
     */
    public function testCheckDependenciesRequiresMinimumVersion(
        array $installedVersions,
        string $requiredVersion,
        bool $satisfied
    ): void {
        $installed = [];
        foreach ($installedVersions as $version) {
            $package = new stdClass();
            $package->version = $version;
            $installed[] = $package;
        }

        $upgradeHistory = $this->createPartialMock(UpgradeHistory::class, ['getList']);
        $upgradeHistory->method('getList')->willReturn($installed);

        $notFound = $upgradeHistory->checkDependencies([
            ['id_name' => 'some_package', 'version' => $requiredVersion],
        ]);

        self::assertSame($satisfied ? [] : ['some_package'], $notFound);
    }

    public static function installedVersionsProvider(): array
    {
        return [
            'same version' => [['1.2.3'], '1.2.3', true],
            'newer patch version' => [['1.2.4'], '1.2.3', true],
            'newer major version' => [['2.0.0'], '1.2.3', true],
            'version numbers are not compared as text' => [['1.10.0'], '1.9.0', true],
            'newer with more version parts' => [['1.2.3.1'], '1.2.3', true],
            'older patch version' => [['1.2.2'], '1.2.3', false],
            'older major version' => [['0.9.9'], '1.2.3', false],
            'older with fewer version parts' => [['1.2'], '1.2.3', false],
            'any installed version can match' => [['1.0.0', '1.2.4'], '1.2.3', true],
            'not installed' => [[], '1.2.3', false],
        ];
    }
}
