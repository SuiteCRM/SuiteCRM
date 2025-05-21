<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Events extends \AcceptanceTester
{
    /**
     * Create an event
     *
     * @param $name
     */
    public function createEvent($name, $location)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Event', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#fp_event_locations_fp_events_1_name', $location);
        $I->fillField('#description', $faker->text());
        $I->fillField('#budget', $faker->randomDigit());

        $I->fillField('#date_start_date', '01/01/2000');
        $I->selectOption('#date_start_time_section #date_start_hours', '12');
        $I->selectOption('#date_start_time_section #date_start_minutes', '45');

        $I->fillField('#date_end_date', '01/01/2000');
        $I->selectOption('#date_end_time_section #date_end_hours', '01');
        $I->selectOption('#date_end_time_section #date_end_minutes', '45');

        $I->seeElement('#assigned_user_name');
        $I->seeElement('#date_start_date');
        $I->seeElement('#date_end_date');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
