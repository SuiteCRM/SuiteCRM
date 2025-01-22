<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class LeadsCest
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
     * As an administrator I want to view the leads module.
     */
    public function testScenarioViewLeadsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the leads module for testing');

        // Navigate to leads list-view
        $I->loginAsAdmin();
        $I->visitPage('Leads', 'index');
        $listView->waitForListViewVisible();

        $I->see('Leads', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Leads $lead
     *
     * As administrative user I want to create a laed so that I can test
     * the standard fields.
     */
    public function testScenarioCreateLead(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Leads $lead
    ) {
        $I->wantTo('Create a Lead');

        // Navigate to leads list-view
        $I->loginAsAdmin();
        $I->visitPage('Leads', 'index');
        $listView->waitForListViewVisible();

        // Create lead
        $this->fakeData->seed($this->fakeDataSeed);
        $lead->createLead('Test_'. $this->fakeData->company());

        // Delete lead
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
