<?php
namespace Step\Acceptance;

use SuiteCRM\Enumerator\SugarObjectType;

class ModuleBuilder extends Administration
{
    /**
     * @param string $packageName
     * @param string $moduleName
     * @param string $moduleType
     * @see SugarObjectType
     */
    public function createModule($packageName, $moduleName, $moduleType): void
    {
        $I = $this;

        $I->visitPage('Administration', 'index');

        // Go To Module Builder
        $I->click('#moduleBuilder');

        $packageExists = $I->seePageHas($packageName, '#Buttons');
        if ($packageExists === false) {
            $I->waitForElementVisible('#newPackageLink');
            // Create new package
            $I->click('#newPackageLink');

            $I->waitForElementVisible(['name' => 'author']);
            $I->fillField(['name' => 'name'], $packageName);
            $I->fillField(['name' => 'author'], 'Acceptance Tester');
            $I->fillField(['name' => 'key'], 'Test');
            $I->fillField(['name' => 'description'], 'test module');
            $I->click('Save');

            // Wait until confirmation window has disappeared
            $this->waitUntilPopupSuccessDisappeared();

            // Create new module
            $I->click('New Module');
            $I->waitForElement('[name="label"]');
            $I->fillField(['name' => 'name'], $moduleName);
            $I->fillField(['name' => 'label'], $moduleName);
            $I->checkOption('[name=importable]');


            switch ($moduleType) {
                case SugarObjectType::basic:
                    $I->click('#type_basic');
                    break;
                case SugarObjectType::company:
                    $I->click('#type_company');
                    break;
                case SugarObjectType::file:
                    $I->click('#type_file');
                    break;
                case SugarObjectType::issue:
                    $I->click('#type_issue');
                    break;
                case SugarObjectType::person:
                    $I->click('#type_person');
                    break;
                case SugarObjectType::sale:
                    $I->click('#type_sale');
                    break;
            }

            $I->wait(1);
            $I->click('Save'); // Will this be an issue with languages?

            // Deploy module

            $I->waitForElementVisible('[name="name"]');

            $I->deployPackage($packageName);
            // Redeploy @TODO separate this out to new test
            $I->deployPackage($packageName, true);
        } else {
            $I->getScenario()->skip($packageName . ' already exists. Please remove package and module manually.');
        }
    }

    /**
     * @param string $packageName
     */
    public function selectPackage($packageName): void
    {
        $I = $this;

        $I->visitPage('Administration', 'index');

        // Go To Module Builder
        $I->click('#moduleBuilder');
        $I->waitForElementVisible('.bodywrapper');
        $I->click($packageName, '.bodywrapper');
        $I->waitForElementVisible(['name' => 'author']);
    }

    /**
     * @param string $packageName
     * @param string $moduleName
     */
    public function selectModule($packageName, $moduleName): void
    {
        $I = $this;

        $I->visitPage('Administration', 'index');

        // Go To Module Builder
        $I->click('#moduleBuilder');
        $I->waitForElementVisible('.bodywrapper');
        $I->click($packageName, '.bodywrapper');
        $I->waitForElementVisible(['name' => 'author']);
        $I->click($moduleName, '#package_modules');
        $I->waitForElementVisible(['name' => 'savebtn']);
    }

    /**
     * Wait until confirmation window has disappeared
     */
    public function waitUntilPopupSuccessDisappeared()
    {
        $I = $this;
        $I->waitForElementNotVisible('#sugarMsgWindow_c');
        $I->waitForElementNotVisible('#sugarMsgWindow_mask');
    }

    /**
     * @param string $packageName
     * @param boolean $packageExists
     *
     */
    public function deployPackage($packageName, $packageExists = false): void
    {
        $I = $this;

        $I->visitPage('Administration', 'index');

        // Go To Module Builder
        $I->click('#moduleBuilder');
        $I->waitForElementVisible('.bodywrapper', 30);
        $I->click($packageName, '.bodywrapper');
        $I->waitForElementVisible('[name="name"]', 30);
        $I->click('Deploy');

        if ($packageExists) {
            $I->acceptPopup();
        }

        // Wait for page to refresh and look for new package link
        $I->waitForElement('#newPackageLink', 30);
    }
}
