<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class SurveysCest
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
     * As an administrator I want to view the surveys module.
     */
    public function testScenarioViewSurveysModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the surveys module for testing');

        // Navigate to surveys list-view
        $I->loginAsAdmin();
        $I->visitPage('Surveys', 'index');
        $listView->waitForListViewVisible();

        $I->see('Surveys', '.module-title-text');
    }
}
