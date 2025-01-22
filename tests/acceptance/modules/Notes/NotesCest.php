<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class NotesCest
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
     * As an administrator I want to view the notes module.
     */
    public function testScenarioViewNotesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the notes module for testing');

        // Navigate to notes list-view
        $I->loginAsAdmin();
        $I->visitPage('Notes', 'index');
        $listView->waitForListViewVisible();

        $I->see('Notes', '.module-title-text');
    }
}
