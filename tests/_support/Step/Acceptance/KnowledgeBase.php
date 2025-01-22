<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class KnowledgeBase extends \AcceptanceTester
{
    /**
     * Create a knowledge base
     *
     * @param $name
     */
    public function createKnowledgeBase($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Knowledge Base', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#additional_info', $faker->text());
        $I->fillField('#revision', $faker->randomDigit());

        $I->executeJS('tinyMCE.activeEditor.setContent("TinyMCE Content Test");');

        $I->seeElement('#author');
        $I->seeElement('#approver');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
