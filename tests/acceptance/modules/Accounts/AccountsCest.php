<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class AccountsCest
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
            $this->fakeData = Faker\Factory::create();
        }

        $this->fakeDataSeed = mt_rand(0, 2048);
        $this->fakeData->seed($this->fakeDataSeed);
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     *
     * As an administrator I want to view the accounts module.
     */
    public function testScenarioViewAccountsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the accounts module for testing');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        $I->see('Accounts', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\SideBar $sideBar
     *
     * As administrative user I want to create a report with the reports module so that I can test
     * the standard fields.
     */
    public function testScenarioCreateAccount(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sideBar
    ) {
        $I->wantTo('Create an Account');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $faker = $I->getFaker();

        $I->see('Create Account', '.actionmenulink');
        $sideBar->clickSideBarAction('Create');
        $editView->waitForEditViewVisible();
        $I->fillField('#name', 'Test_' . $this->fakeData->company());
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
        $detailView->waitForDetailViewVisible();

        // Delete account
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Accounts $accounts
     *
     * As administrative user I want to inline edit a field on the list-view
     */
    public function testScenarioInlineEditListView(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sideBar
    ) {
        $I->wantTo('Inline edit an account on the list-view');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
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
        $detailView->waitForDetailViewVisible();

        // Inline edit
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();
        $I->waitForElementVisible( '.inlineEditIcon');
        $I->doubleClick('.inlineEditIcon');
        $I->fillField('#name', 'InlineAccountNameEdit');
        $I->clickWithLeftButton('.suitepicon-action-confirm');
        $I->see('InlineAccountNameEdit');
    }

    public function testScenarioCreateAccountChild(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Accounts $accounts
    ) {
        $I->wantTo('Create a child Account');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $parentAccountName = 'Test_' . $this->fakeData->company();
        $accountId = $accounts->createAccount($parentAccountName);

        $I->visitPage('Accounts', 'DetailView', $accountId);
        $detailView->waitForDetailViewVisible();

        // Click on Member Organizations subpanel
        $I->click('#subpanel_title_accounts');
        $I->waitForElementVisible('#member_accounts_create_button');

        // Add child account
        $accountName = 'Test_' . $this->fakeData->company();
        $I->click('#member_accounts_create_button');
        $I->waitForElementVisible('#Accounts_subpanel_full_form_button');
        $I->click('#Accounts_subpanel_full_form_button');
        $editView->waitForEditViewVisible();
        $I->fillfield('#name', $accountName);
        $editView->clickSaveButton();

        // View child account in parent account subpanel
        $detailView->waitForDetailViewVisible();
        $I->waitForElementVisible( '//*[@id="list_subpanel_accounts"]/table/tbody/tr/td[2]/a');
        $I->see($accountName, '//*[@id="list_subpanel_accounts"]/table/tbody/tr/td[2]/a');

        // Delete account
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();

        // Select record from list view
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $listView->fillField('#name_basic', $accountName);
        $listView->click('Search', '.submitButtons');
        $listView->waitForListViewVisible();
        $listView->clickNameLink($accountName);
        $detailView->waitForDetailViewVisible();

        // Delete account
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
