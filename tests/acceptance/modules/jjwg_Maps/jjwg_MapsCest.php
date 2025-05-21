<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class jjwg_MapsCest
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
     * As an administrator I want to view the maps module.
     */
    public function testScenarioViewMapsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the maps module for testing');

        // Navigate to maps list-view
        $I->loginAsAdmin();
        $I->visitPage('jjwg_Maps', 'index');
        $listView->waitForListViewVisible();

        $I->see('Maps', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Maps $map
     * @param \Step\Acceptance\AccountsTester $accounts
     *
     * As administrative user I want to create a map so that I can test
     * the standard fields.
     */
    public function testScenarioCreateMap(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Maps $map,
        \Step\Acceptance\AccountsTester $accounts
    ) {
        $I->wantTo('Create a Map');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $account_name = 'Test_'. $this->fakeData->company();
        $accounts->createAccount($account_name);

        // Navigate to maps list-view
        $I->visitPage('jjwg_Maps', 'index');
        $listView->waitForListViewVisible();

        // Create map
        $this->fakeData->seed($this->fakeDataSeed);
        $name = 'Test_'. $this->fakeData->company();
        $map->createMap($name, $account_name);

        // Delete map
        $listView->clickNameLink($account_name);
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();

        // Delete account
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();
        $I->wait(5);
        $listView->clickFilterButton();
        $I->fillField('#name_basic', $account_name);
        $I->click('#search_form_submit');
        $listView->waitForListViewVisible();
        $listView->clickNameLink($account_name);
        $detailView->waitForDetailViewVisible();
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
