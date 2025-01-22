<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Meetings extends \AcceptanceTester
{
    /**
     * Create a meeting
     *
     * @param $name
     */
    public function createMeeting($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Schedule Meeting', '.actionmenulink');
        $Sidebar->clickSideBarAction('Schedule');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#date_start_date', '01/19/2038');
        $I->fillField('#description', $faker->text());
        $I->fillField('#location', $faker->city());

        $I->waitForElementVisible('#date_start_hours');

        $I->waitForElementVisible('#reminder_view');
        $I->selectOption('#reminder_view select[class=timer_sel_popup]', '15 minutes prior');
        $I->selectOption('#status', 'Held');
        $I->selectOption('#parent_type', 'Account');

        $I->seeElement('#assigned_user_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
