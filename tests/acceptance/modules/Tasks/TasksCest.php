<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class TasksCest
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
     * As an administrator I want to view the tasks module.
     */
    public function testScenarioViewTasksModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the tasks module for testing');

        // Navigate to tasks list-view
        $I->loginAsAdmin();
        $I->visitPage('Tasks', 'index');
        $listView->waitForListViewVisible();

        $I->see('Tasks', '.module-title-text');
    }
}
