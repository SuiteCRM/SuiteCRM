<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Products extends \AcceptanceTester
{
    /**
     * Create a product
     *
     * @param $name
     */
    public function createProduct($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Product', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#part_number', $faker->randomDigit());
        $I->fillField('#cost', $faker->randomDigit());
        $I->fillField('#price', $faker->randomDigit());
        $I->fillField('#url', $faker->url());
        $I->fillField('#description', $faker->text());

        $I->selectOption('#currency_id_select', 'US Dollars : $');
        $I->selectOption('#type', 'Service');

        $I->seeElement('#aos_product_category_name');
        $I->seeElement('#contact');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
