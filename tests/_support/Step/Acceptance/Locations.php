<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Locations extends \AcceptanceTester
{
    /**
     * Create an event location
     *
     * @param $name
     */
    public function createEventLocation($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Locations', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#description', $faker->text());
        $I->fillField('#capacity', $faker->randomDigit());
        $I->fillField('#address', $faker->streetAddress());
        $I->fillField('#address_city', $faker->city());
        $I->fillField('#address_postalcode', $faker->postcode());
        $I->fillField('#address_state', $faker->country());
        $I->fillField('#address_country', $faker->country());

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
