<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Opportunities extends \AcceptanceTester
{
    /**
     * Create an opportunity
     *
     * @param $name
     */
    public function createOpportunity($name, $account)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Opportunity', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#account_name', $account);
        $I->fillField('#amount', $faker->randomDigit());
        $I->fillField('#probability', $faker->randomDigit());
        $I->fillField('#next_step', $faker->randomDigit());
        $I->fillField('#description', $faker->text());
        $I->fillField('#account_name', $account);
        $I->fillField('#date_closed', '01/19/2038');

        $I->selectOption('#currency_id_select', 'US Dollars : $');
        $I->selectOption('#sales_stage', 'Prospecting');
        $I->selectOption('#opportunity_type', 'Existing Business');
        $I->selectOption('#lead_source', 'Existing Customer');

        $I->seeElement('#campaign_name');
        $I->seeElement('#assigned_user_name');

        $I->wait(3);
        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
