<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Contracts extends \AcceptanceTester
{
    /**
     * Create a contract
     *
     * @param $name
     */
    public function createContract($name, $account)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Contract', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#contract_account', $account);
        $I->fillField('#total_contract_value', $faker->randomDigit());
        $I->fillField('#description', $faker->text());
        $I->fillField('#total_amt', $faker->randomDigit());
        $I->fillField('#discount_amount', $faker->randomDigit());
        $I->fillField('#subtotal_amount', $faker->randomDigit());
        $I->fillField('#shipping_amount', $faker->randomDigit());
        $I->fillField('#shipping_tax_amt', $faker->randomDigit());
        $I->fillField('#tax_amount', $faker->randomDigit());
        $I->fillField('#total_amount', $faker->randomDigit());

        $I->selectOption('#status', 'In Progress');
        $I->selectOption('#contract_type', 'Type');
        $I->selectOption('#currency_id_select', 'US Dollars : $');
        $I->selectOption('#shipping_tax', '7.5%');

        $I->seeElement('#start_date');
        $I->seeElement('#end_date');
        $I->seeElement('#renewal_reminder_date_date');
        $I->seeElement('#customer_signed_date');
        $I->seeElement('#company_signed_date');
        $I->seeElement('#assigned_user_name');
        $I->seeElement('#contact');
        $I->seeElement('#opportunity');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
