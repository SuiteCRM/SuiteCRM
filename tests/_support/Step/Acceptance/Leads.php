<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Leads extends \AcceptanceTester
{
    /**
     * Create a lead
     *
     * @param $name
     */
    public function createLead($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Lead', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#first_name', $faker->name());
        $I->fillField('#last_name', $name);
        $I->fillField('#title', $faker->title());
        $I->fillField('#department', $faker->title());
        $I->fillField('#EditView_account_name', $faker->title());
        $I->fillField('#phone_work', $faker->phoneNumber());
        $I->fillField('#phone_mobile', $faker->phoneNumber());
        $I->fillField('#phone_fax', $faker->phoneNumber());
        $I->fillField('#website', $faker->url());

        $I->fillField('#primary_address_street', $faker->streetAddress());
        $I->fillField('#primary_address_city', $faker->city());
        $I->fillField('#primary_address_state', $faker->city());
        $I->fillField('#primary_address_postalcode', $faker->postcode());
        $I->fillField('#primary_address_country', $faker->country());
        $I->fillField('#description', $faker->text());
        $I->fillField('#status_description', $faker->text());
        $I->fillField('#lead_source_description', $faker->text());
        $I->fillField('#opportunity_amount', $faker->randomDigit());
        $I->fillField('#refered_by', $faker->name());
        $I->fillField('#Leads0emailAddress0', $faker->email());

        $I->checkOption('#alt_checkbox');
        $I->selectOption('#salutation', 'Mr.');
        $I->selectOption('#status', 'Assigned');
        $I->selectOption('#lead_source', 'Cold Call');

        $I->seeElement('#assigned_user_name');
        $I->seeElement('#campaign_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
