<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class jjwg_Address_CacheCest
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
     * As an administrator I want to view the mapsAddressCache module.
     */
    public function testScenarioViewMapsAddressCacheModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the mapsAddressCache module for testing');

        // Navigate to mapsAddressCache list-view
        $I->loginAsAdmin();
        $I->visitPage('jjwg_Address_Cache', 'index');
        $listView->waitForListViewVisible();

        $I->see('Maps - Address Cache', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\MapsAddressCache $mapsAddressCache
     *
     * As administrative user I want to create a maps address cache so that I can test
     * the standard fields.
     */
    public function testScenarioCreateMapsAddressCache(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\MapsAddressCache $mapsAddressCache
    ) {
        $I->wantTo('Create maps address cache');

        // Navigate to maps address cache list-view
        $I->loginAsAdmin();
        $I->visitPage('jjwg_Address_Cache', 'index');
        $listView->waitForListViewVisible();

        // Create maps address cache
        $this->fakeData->seed($this->fakeDataSeed);
        $mapsAddressCache->createMapsAddressCache('Test_'. $this->fakeData->company());

        // Delete maps address cache
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
