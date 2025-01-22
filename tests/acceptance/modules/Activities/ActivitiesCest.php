<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class ActivitiesCest
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
     * @param \Step\Acceptance\AccountsTester $accounts
     * @param \Step\Acceptance\Calls $calls
     * @param \Step\Acceptance\NavigationBarTester $NavigationBar
     *
     * As a user I want to see the due date on the activities module
     */
    public function testSeeDueDateSubpanelView(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\AccountsTester $accounts,
        \Step\Acceptance\Calls $calls,
        \Step\Acceptance\NavigationBarTester $NavigationBar
    ) {
        $I->wantTo('See the due date field on Account Activities subpanel');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $account_name = 'Test_'. $this->fakeData->company();
        $accounts->createAccount($account_name);

        // Create a Call and relate to an Account
        $NavigationBar->clickAllMenuItem('Calls');

        // Create call
        $this->fakeData->seed($this->fakeDataSeed);
        $callName = 'Test_'. $this->fakeData->company();
        $calls->createCallRelateModule($callName, $account_name, "Account");

        // Navigate to the Account's Detail View and confirm the due date contains data
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Select record from list view
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $listView->fillField('#name_basic', $account_name);
        $listView->click('Search', '.submitButtons');
        $listView->waitForListViewVisible();
        $listView->clickNameLink($account_name);

        //Click on Activites subpanel
        $I->waitForElementVisible(['id'=>'subpanel_title_activities']);
        $I->click(['id'=>'subpanel_title_activities']);
        $I->waitForElementVisible('#Activities_createtask_button');
        $I->expect('the due date is visible');
        $I->seeInSource('01/19/2038');

        // Delete the Account
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();

        // Delete the Call
        $NavigationBar->clickAllMenuItem('Calls');
        $listView->waitForListViewVisible();

        // Select record from list view
        $I->wait(3);
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $listView->fillField('#name_basic', $callName);
        $listView->click('Search', '.submitButtons');
        $listView->waitForListViewVisible();
        $listView->clickNameLink($callName);

        // Delete record from detail view
        $detailView->waitForDetailViewVisible();

        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
