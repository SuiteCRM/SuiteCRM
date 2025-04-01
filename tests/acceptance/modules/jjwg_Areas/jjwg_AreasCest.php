<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class jjwg_AreasCest
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
     * As an administrator I want to view the mapsAreas module.
     */
    public function testScenarioViewMapsAreasModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the mapsAreas module for testing');

        $I->loginAsAdmin();
        // Navigate to mapsAreas list-view
        $I->visitPage('jjwg_Areas', 'index');
        $listView->waitForListViewVisible();

        $I->see('Maps - Areas', '.module-title-text');
    }
}
