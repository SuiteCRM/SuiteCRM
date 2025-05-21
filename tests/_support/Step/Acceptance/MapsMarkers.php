<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class MapsMarkers extends \AcceptanceTester
{
    /**
     * Create a map marker
     *
     * @param $name
     */
    public function createMapMarker($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Markers', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#city', $faker->city());
        $I->fillField('#state', $faker->city());
        $I->fillField('#country', $faker->country());
        $I->fillField('#description', $faker->text());
        $I->fillField('#jjwg_maps_lat', $faker->latitude());
        $I->fillField('#jjwg_maps_lng', $faker->longitude());

        $I->selectOption('#marker_image', 'Bridge');

        $I->seeElement('#assigned_user_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
