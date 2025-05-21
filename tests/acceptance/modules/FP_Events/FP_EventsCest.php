<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class EventsCest
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
     * As an administrator I want to view the events module.
     */
    public function testScenarioViewEventsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the events module for testing');

        // Navigate to events list-view
        $I->loginAsAdmin();
        $I->visitPage('FP_events', 'index');
        $listView->waitForListViewVisible();

        $I->see('Events', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Events $event
     * @param \Step\Acceptance\Locations $location
     *
     * As administrative user I want to create an event so that I can test
     * the standard fields.
     */
    public function testScenarioCreateEvent(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Events $event,
        \Step\Acceptance\Locations $location
    ) {
        $I->wantTo('Create an Event');

        // Navigate to locations list-view
        $I->loginAsAdmin();
        $I->visitPage('FP_Event_Locations', 'index');
        $listView->waitForListViewVisible();

        // Create location
        $this->fakeData->seed($this->fakeDataSeed);
        $location_name = 'Test_'. $this->fakeData->company();
        $location->createEventLocation($location_name);

        // Navigate to events list-view
        $I->visitPage('FP_events', 'index');
        $listView->waitForListViewVisible();

        // Create event
        $this->fakeData->seed($this->fakeDataSeed);
        $event->createEvent('Test_'. $this->fakeData->company(), $location_name);

        $I->see('01/01/2000 12:45');
        $I->see('01/01/2000 01:45');
        $I->see('1h 0m');

        // Delete event
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();

        // Delete location
        $I->visitPage('FP_Event_Locations', 'index');
        $listView->waitForListViewVisible();
        $listView->clickFilterButton();
        $I->fillField('#name_basic', $location_name);
        $I->click('#search_form_submit');
        $listView->waitForListViewVisible();
        $listView->clickNameLink($location_name);
        $detailView->waitForDetailViewVisible();
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
