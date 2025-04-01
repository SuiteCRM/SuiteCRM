<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Reports extends \AcceptanceTester
{
    /**
     * Go to user profile
     */
    public function gotoProfile()
    {
        $I = new NavigationBarTester($this->getScenario());
        $I->clickUserMenuItem('Profile');
    }

    /**
     * Create a report
     *
     * @param $name
     * @param $module
     */
    public function createReport($name, $module)
    {
        $I = new EditView($this->getScenario());
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->selectOption('#report_module', $module);
    }
}
