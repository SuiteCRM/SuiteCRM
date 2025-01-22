<?php

#[\AllowDynamicProperties]
class CallsCest
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
     * As an administrator I want to view the calls module.
     */
    public function testScenarioViewCallsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the calls module for testing');

        // Navigate to calls list-view
        $I->loginAsAdmin();
        $I->visitPage('Calls', 'index');
        $listView->waitForListViewVisible();

        $I->see('Calls', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\NavigationBarTester $NavigationBar
     * @param \Step\Acceptance\Calls $calls
     * @param \Step\Acceptance\DetailView $detailView
     *
     * As an administrator I want to verify the date field of a call
     */
    public function testScenarioCallDate(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\NavigationBarTester $NavigationBar,
        \Step\Acceptance\Calls $calls,
        \Step\Acceptance\DetailView $detailView
    ) {
        $I->wantTo('Create a call');

        // Navigate to Calls
        $I->loginAsAdmin();
        $I->visitPage('Calls', 'index');

        // Create call
        $this->fakeData->seed($this->fakeDataSeed);
        $callName = 'Test_'. $this->fakeData->company();
        $calls->createCall($callName);

        // Delete Record
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
