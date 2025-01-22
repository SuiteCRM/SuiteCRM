<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Maps extends \AcceptanceTester
{
    /**
     * Create map
     *
     * @param $name
     */
    public function createMap($name, $account)
    {
        $I = new EditView($this->getScenario());
        $ListView = new ListView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Add New Map', '.actionmenulink');
        $Sidebar->clickSideBarAction('Add New Map');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#distance', $faker->randomDigit());
        $I->fillField('#description', $faker->text());

        $I->selectOption('#unit_type', 'Kilometers');
        $I->selectOption('#module_type', 'Contacts');
        $I->selectOption('#parent_type', 'Account');

        $I->fillField('#parent_name', $account);

        $I->seeElement('#assigned_user_name');
        $I->seeElement('#assigned_user_name');

        $I->wait(10);
        $I->clickSaveButton();
        $ListView->waitForListViewVisible();
    }
}
