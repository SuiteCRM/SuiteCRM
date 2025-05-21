<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class ProjectTemplates extends \AcceptanceTester
{
    /**
     * Create a project template
     *
     * @param $name
     */
    public function createProjectTemplate($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());

        $I->see('Create Project Template', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);

        $I->checkOption('#override_business_hours');
        $I->selectOption('#status', 'Underway');
        $I->selectOption('#priority', 'Low');

        $I->seeElement('#assigned_user_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
