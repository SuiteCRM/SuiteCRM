<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class ProjectsCest
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
     * As an administrator I want to view the projects module.
     */
    public function testScenarioViewProjectsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the projects module for testing');

        // Navigate to projects list-view
        $I->loginAsAdmin();
        $I->visitPage('Project', 'index');
        $listView->waitForListViewVisible();

        $I->see('Projects', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Projects $project
     *
     * As administrative user I want to create a project so that I can test
     * the standard fields.
     */
    public function testScenarioCreateProject(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Projects $project
    ) {
        $I->wantTo('Create a Project');

        // Navigate to projects list-view
        $I->loginAsAdmin();
        $I->visitPage('Project', 'index');
        $listView->waitForListViewVisible();

        // Create project
        $this->fakeData->seed($this->fakeDataSeed);
        $project->createProject('Test_'. $this->fakeData->company());

        // Delete project
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
