<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class CasesCest
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
     * As an administrator I want to view the cases module.
     */
    public function testScenarioViewCasesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the cases module for testing');

        // Navigate to cases list-view
        $I->loginAsAdmin();
        $I->visitPage('Cases', 'index');
        $listView->waitForListViewVisible();

        $I->see('Cases', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Cases $cases
     * @param \Step\Acceptance\Cases $account
     *
     * As administrative user I want to create a case so that I can test
     * the standard fields.
     */
    public function testScenarioCreateCase(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Cases $cases,
        \Step\Acceptance\AccountsTester $account
    ) {
        $I->wantTo('Create a Case');

        // Navigate to accounts list-view
        $I->loginAsAdmin();
        $I->visitPage('Accounts', 'index');
        $listView->waitForListViewVisible();

        // Create account
        $this->fakeData->seed($this->fakeDataSeed);
        $account_name = 'Test_'. $this->fakeData->company();
        $account->createAccount($account_name);

        // Navigate to cases list-view
        $I->visitPage('Cases', 'index');
        $listView->waitForListViewVisible();

        // Create case
        $this->fakeData->seed($this->fakeDataSeed);
        $cases->createCase('Test_'. $this->fakeData->company(), $account_name);

        // Delete case
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
