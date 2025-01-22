<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class DocumentsCest
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
     * As an administrator I want to view the documents module.
     */
    public function testScenarioViewDocumentsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the documents module for testing');

        // Navigate to documents list-view
        $I->loginAsAdmin();
        $I->visitPage('Documents', 'index');
        $listView->waitForListViewVisible();

        $I->see('Documents', '.module-title-text');
    }
}
