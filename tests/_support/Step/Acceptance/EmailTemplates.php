<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class EmailTemplates extends \AcceptanceTester
{
    /**
     * Create an Email Template
     *
     * @param $name
     */
    public function createEmailTemplate($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Email Template', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#description', $faker->sentence());
        $I->fillField('#subject', 'Test Subject: ' . $faker->words(3, true));

        $I->seeElement('#body_text');
        $I->seeElement('#assigned_user_name');

        $I->executeJS('tinymce.activeEditor.setContent("TinyMCE Content Test");');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
