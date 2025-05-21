<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class jjwg_MarkersCest
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
     * As an administrator I want to view the mapsMarkers module.
     */
    public function testScenarioViewMapsMarkersModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the mapsMarkers module for testing');

        // Navigate to mapsMarkers list-view
        $I->loginAsAdmin();
        $I->visitPage('jjwg_Markers', 'index');
        $listView->waitForListViewVisible();

        $I->see('Maps - Markers', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\MapsMarkers $mapMarker
     *
     * As administrative user I want to create a map marker so that I can test
     * the standard fields.
     */
    public function testScenarioCreateMapMarker(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\MapsMarkers $mapMarker
    ) {
        $I->wantTo('Create a Map Marker');

        // Navigate to map markers list-view
        $I->loginAsAdmin();
        $I->visitPage('jjwg_Markers', 'index');
        $listView->waitForListViewVisible();

        // Create map marker
        $this->fakeData->seed($this->fakeDataSeed);
        $mapMarker->createMapMarker('Test_'. $this->fakeData->company());

        // Delete map marker
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
