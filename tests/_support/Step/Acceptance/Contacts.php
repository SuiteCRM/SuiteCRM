<?php

namespace Step\Acceptance;

#[\AllowDynamicProperties]
class Contacts extends \AcceptanceTester
{
    /**
     * Create a contact
     *
     * @param $name
     */
    public function createContact($name)
    {
        $I = new EditView($this->getScenario());
        $DetailView = new DetailView($this->getScenario());
        $Sidebar = new SideBar($this->getScenario());
        $faker = $this->getFaker();

        $I->see('Create Contact', '.actionmenulink');
        $Sidebar->clickSideBarAction('Create');
        $I->waitForEditViewVisible();
        $I->fillField('#first_name', $faker->lastName());
        $I->fillField('#last_name', $name);
        $I->fillField('#phone_work', $faker->phoneNumber());
        $I->fillField('#phone_mobile', $faker->phoneNumber());
        $I->fillField('#title', $faker->title());
        $I->fillField('#phone_fax', $faker->phoneNumber());
        $I->fillField('#Contacts0emailAddress0', $faker->email());
        $I->fillField('#primary_address_street', $faker->streetAddress());
        $I->fillField('#primary_address_city', $faker->city());
        $I->fillField('#primary_address_postalcode', $faker->postcode());
        $I->fillField('#primary_address_country', $faker->country());
        $I->fillField('#description', $faker->text());

        $I->checkOption('#alt_checkbox');
        $I->selectOption('#salutation', 'Mr.');
        $I->selectOption('#lead_source', 'Cold Call');

        $I->seeElement('#account_name');
        $I->seeElement('#assigned_user_name');
        $I->seeElement('#report_to_name');
        $I->seeElement('#campaign_name');

        $I->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
