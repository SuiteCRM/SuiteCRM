<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class MapsAddressCache extends \AcceptanceTester
{
    /**
     * Create maps address cache
     *
     * @param $name
     */
    public function createMapsAddressCache($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Address Cache', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#lat', $faker->latitude());
        $I->fillField('#lng', $faker->longitude());

        $I->seeElement('#assigned_user_name');
        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
