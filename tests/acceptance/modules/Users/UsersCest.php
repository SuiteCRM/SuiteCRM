<?php

use Faker\Factory;
use Faker\Generator;
use Step\Acceptance\AccountsTester;
use Step\Acceptance\DetailView;
use Step\Acceptance\EditView;
use Step\Acceptance\ListView;
use Step\Acceptance\UsersTester;

#[\AllowDynamicProperties]
class UsersCest
{
    /**
     * @var Generator $fakeData
     */
    protected $fakeData;

    /**
     * @var integer $fakeDataSeed
     */
    protected $fakeDataSeed;

    /**
     * @param AcceptanceTester $I
     */
    public function _before(AcceptanceTester $I)
    {
        if (!$this->fakeData) {
            $this->fakeData = Factory::create();
        }

        $this->fakeDataSeed = mt_rand(0, 2048);
        $this->fakeData->seed($this->fakeDataSeed);
    }

    public function testEmailSettingsMailAccountAdd(AcceptanceTester $I, UsersTester $Users, \Codeception\Scenario $scenario)
    {
        $scenario->skip('Needs to be re-implemented');
        $I->loginAsAdmin();
        $Users->gotoProfile();
        $I->see('User Profile', '.panel-heading');
        $I->click('Settings');
        $I->see('Inbound Email Accounts');
        $I->click('Inbound Email Accounts');
        $I->click('Add');
        $I->executeJS('javascript:SUGAR.email2.accounts.fillInboundGmailDefaults();'); // <-- instead of $I->click('Prefill Gmail™ Defaults');
        $I->fillField('ie_name', 'testuser_acc');
        $I->fillField('email_user', 'testuser_name');
        $I->fillField('email_password', 'testuser_pass');
        $I->click('Test Settings');
        $I->waitForText('Connection completed successfully.');
        $I->see('Connection completed successfully.');
    }

    public function testShowCollapsedSubpanelHint(
        AcceptanceTester $I,
        DetailView $DetailView,
        UsersTester $Users,
        ListView $listView,
        EditView $EditView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sideBar
    ) {
        $I->wantTo('View the collapsed subpanel hints on Accounts');

        // Navigate to Users list-view
        $I->loginAsAdmin();

        $Users->gotoProfile();

        $I->see('User Profile', '.panel-heading');

        $I->click("Layout Options");
        $I->waitForElementVisible('input[name="user_count_collapsed_subpanels"]');
        $I->seeElement('input', ['name' => 'user_count_collapsed_subpanels']);
        $I->checkOption(['name' => 'user_count_collapsed_subpanels']);
        $EditView->clickSaveButton();
        $DetailView->waitForDetailViewVisible();

        // Create & Navigate to Accounts
        // @TODO - Need to include dummy data to utilise these tests efficiently
        $I->wantTo('Create an Account');

        // Navigate to accounts list-view
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $faker = $I->getFaker();
        $account_name = 'Test_'. $this->fakeData->company();

        $I->see('Create Account', '.actionmenulink');
        $sideBar->clickSideBarAction('Create');
        $editView->waitForEditViewVisible();
        $I->fillField('#name', $account_name);
        $I->fillField('#phone_office', $faker->phoneNumber());
        $I->fillField('#website', $faker->url());
        $I->fillField('#phone_fax', $faker->phoneNumber());
        $I->fillField('#Accounts0emailAddress0', $faker->email());
        $I->fillField('#billing_address_street', $faker->streetAddress());
        $I->fillField('#billing_address_city', $faker->city());
        $I->fillField('#billing_address_state', $faker->city());
        $I->fillField('#billing_address_postalcode', $faker->postcode());
        $I->fillField('#billing_address_country', $faker->country());
        $I->fillField('#description', $faker->text());
        $I->fillField('#annual_revenue', $faker->randomDigit());
        $I->fillField('#employees', $faker->randomDigit());

        $I->checkOption('#shipping_checkbox');
        $I->selectOption('#account_type', 'Analyst');
        $I->selectOption('#industry', 'Apparel');

        $I->seeElement('#assigned_user_name');
        $I->seeElement('#parent_name');
        $I->seeElement('#campaign_name');

        $editView->clickSaveButton();
        $DetailView->waitForDetailViewVisible();

        // View the Subpanels Hint
        $I->see('Leads (0)', '//*[@id="subpanel_title_leads"]/div/div');

        // Delete account
        $DetailView->clickActionMenuItem('Delete');
        $DetailView->acceptPopup();
        $listView->waitForListViewVisible();

        // Reset the collapsed subpanels
        $Users->gotoProfile();
        $I->see('User Profile', '.panel-heading');
        $I->click("Layout Options");
        $I->seeElement('input', ['name' => 'user_count_collapsed_subpanels']);
        $I->uncheckOption(['name' => 'user_count_collapsed_subpanels']);
        $EditView->clickSaveButton();
        $DetailView->waitForDetailViewVisible();
    }
}
