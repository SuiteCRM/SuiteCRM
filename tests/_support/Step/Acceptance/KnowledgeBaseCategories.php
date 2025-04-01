<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class KnowledgeBaseCategories extends \AcceptanceTester
{
    /**
     * Create KnowledgeBase Category
     *
     * @param $name
     */
    public function createKnowledgeBaseCategory($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create KB Categories', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#description', $faker->text());

        $I->seeElement('#created_by_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
