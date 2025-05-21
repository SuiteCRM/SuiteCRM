<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Cases extends \AcceptanceTester
{
    /**
     * Create a case
     *
     * @param $name
     */
    public function createCase($name, $account)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Case', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#resolution', $faker->text());
        $I->fillField('#account_name', $account);

        $I->selectOption('#state', 'Closed');
        $I->selectOption('#type', 'Product');
        $I->selectOption('#priority', 'Low');
        $I->selectOption('#status', 'Duplicate');

        $I->executeJS('tinyMCE.activeEditor.setContent("TinyMCE Content Test");');

        $I->seeElement('#assigned_user_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
