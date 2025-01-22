<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Projects extends \AcceptanceTester
{
    /**
     * Create a project
     *
     * @param $name
     */
    public function createProject($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());

        $I->see('Create Project', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#estimated_start_date', '01/19/2038');
        $I->fillField('#estimated_end_date', '01/19/2038');

        $I->checkOption('#override_business_hours');
        $I->selectOption('#status', 'In Review');
        $I->selectOption('#priority', 'Medium');

        $I->seeElement('#assigned_user_name');
        $I->seeElement('#am_projecttemplates_project_1_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
