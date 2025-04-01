<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class QuotesCest
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
     * As an administrator I want to view the quotes module.
     */
    public function testScenarioViewQuotesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the quotes module for testing');

        // Navigate to quotes list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Quotes', 'index');
        $listView->waitForListViewVisible();

        $I->see('Quotes', '.module-title-text');
    }
}
