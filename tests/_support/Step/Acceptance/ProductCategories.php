<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class ProductCategories extends \AcceptanceTester
{
    /**
     * Create product category
     *
     * @param $name
     */
    public function createProductCategory($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Product Categories', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#description', $faker->text());

        $I->checkOption('#is_parent');

        $I->seeElement('#assigned_user_name');
        $I->seeElement('#parent_category_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
