<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class MeetingsCest
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
     * As an administrator I want to view the meetings module.
     */
    public function testScenarioViewMeetingsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the meetings module for testing');

        // Navigate to meetings list-view
        $I->loginAsAdmin();
        $I->visitPage('Meetings', 'index');
        $listView->waitForListViewVisible();

        $I->see('Meetings', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Meetings $meeting
     *
     * As administrative user I want to create a meeting so that I can test
     * the standard fields.
     */
    public function testScenarioCreateMeeting(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Meetings $meeting
    ) {
        $I->wantTo('Create a meeting');

        // Navigate to meetings list-view
        $I->loginAsAdmin();
        $I->visitPage('Meetings', 'index');
        $listView->waitForListViewVisible();

        // Create meeting
        $this->fakeData->seed($this->fakeDataSeed);
        $meeting->createMeeting('Test_'. $this->fakeData->company());

        // Delete meeting
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Meetings $meeting
     *
     * As administrative user I want to inline edit the start date
     */
    public function testScenarioEditStartDate(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Meetings $meeting
    ) {
        $I->wantTo('Create a meeting');

        // Navigate to meetings list-view
        $I->loginAsAdmin();
        $I->visitPage('Meetings', 'index');
        $listView->waitForListViewVisible();

        // Create meeting
        $this->fakeData->seed($this->fakeDataSeed);
        $meeting->createMeeting('Test_'. $this->fakeData->company());

        // Inline edit
        $I->doubleClick('#date_start');
        $I->waitForElementVisible('#inline_edit_field');
        $I->fillField('#date_start_date', '01/01/2000');
        $I->selectOption('#date_start_hours', '01');
        $I->selectOption('#date_start_minutes', '00');
        $I->click('#inlineEditSaveButton');
        $I->waitForText('01/01/2000 01:00', 30);
        $I->see('01/01/2000 01:00');

        // Delete meeting
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
