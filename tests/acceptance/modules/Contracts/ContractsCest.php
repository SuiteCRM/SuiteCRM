<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class ContractsCest
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
     * As an administrator I want to view the contracts module.
     */
    public function testScenarioViewContractsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the contracts module for testing');

        // Navigate to contracts list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Contracts', 'index');
        $listView->waitForListViewVisible();

        $I->see('Contracts', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Contracts $contract
     * @param \Step\Acceptance\AccountsTester $account
     *
     * As administrative user I want to create a contract so that I can test
     * the standard fields.
     */
    public function testScenarioCreateContract(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Contracts $contract,
        \Step\Acceptance\AccountsTester $account
    ) {
        $I->wantTo('Create a Contract');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $account_name = 'Test_'. $this->fakeData->company();
        $account->createAccount($account_name);

        // Navigate to contracts list-view
        $I->visitPage('AOS_Contracts', 'index');
        $listView->waitForListViewVisible();

        // Create contract
        $this->fakeData->seed($this->fakeDataSeed);
        $contract->createContract('Test_'. $this->fakeData->company(), $account_name);

        // Delete contract
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();

        // Delete account
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();
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
