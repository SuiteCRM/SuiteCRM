<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Invoices extends \AcceptanceTester
{
    /**
     * Create an invoice
     *
     * @param $name
     */
    public function createInvoice($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Invoice', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#name', $name);
        $I->fillField('#quote_number', $faker->randomDigit());
        $I->fillField('#due_date', '01/01/1970');
        $I->fillField('#description', $faker->text());
        $I->fillField('#billing_address_street', $faker->streetAddress());
        $I->fillField('#billing_address_city', $faker->city());
        $I->fillField('#billing_address_state', $faker->city());
        $I->fillField('#billing_address_postalcode', $faker->postcode());
        $I->fillField('#billing_address_country', $faker->country());
        $I->fillField('#total_amt', $faker->randomDigit());
        $I->fillField('#discount_amount', $faker->randomDigit());
        $I->fillField('#subtotal_amount', $faker->randomDigit());
        $I->fillField('#shipping_amount', $faker->randomDigit());
        $I->fillField('#shipping_tax_amt', $faker->randomDigit());
        $I->fillField('#tax_amount', $faker->randomDigit());
        $I->fillField('#total_amount', $faker->randomDigit());

        $I->selectOption('#status', 'Unpaid');
        $I->selectOption('#currency_id_select', 'US Dollars : $');
        $I->selectOption('#shipping_tax', '7.5%');

        $I->seeElement('#shipping_checkbox');
        $I->seeElement('#assigned_user_name');
        $I->seeElement('#quote_date');
        $I->seeElement('#invoice_date');
        $I->seeElement('#billing_account');
        $I->seeElement('#billing_contact');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
