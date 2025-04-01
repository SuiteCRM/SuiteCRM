<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class AOR_ReportsCest
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
     * @param \Step\Acceptance\Reports $reports
     *
     * As an administrator I want to view the reports module.
     */
    public function testScenarioViewReportsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Reports $reports
    ) {
        $I->wantTo('View the reports module for testing');

        // Navigate to reports list-view
        $I->loginAsAdmin();
        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        $I->see('Reports', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\SideBar $sidebar
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Reports $reports
     *
     * As administrative user I want to create a report with the reports module so that I can test
     * the standard fields.
     */
    public function testScenarioCreateReport(
        \AcceptanceTester $I,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\SideBar $sidebar,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Reports $reports
    ) {
        $I->wantTo('Create a Report');

        // Navigate to reports list-view
        $I->loginAsAdmin();
        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        // Select create report from sidebar
        $I->see('Create Report', '.actionmenulink');
        $sidebar->clickSideBarAction('Create');

        // Create a report
        $this->fakeData->seed($this->fakeDataSeed);
        $reports->createReport('Test_'. $this->fakeData->company(), 'Accounts');
        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();

        // Delete report
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\SideBar $sidebar
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\Reports $reports
     *
     * As administrative user I want to view the report by selecting it in the list view
     */
    public function testScenarioViewReportFromListView(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sidebar,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\Reports $reports
    ) {
        $I->wantTo('Select Report from list view');

        // Navigate to reports list-view
        $I->loginAsAdmin();
        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        // Select create report from sidebar
        $I->see('Create Report', '.actionmenulink');
        $sidebar->clickSideBarAction('Create');

        // Create a report
        $this->fakeData->seed($this->fakeDataSeed);
        $reportName = 'Test_'. $this->fakeData->company();
        $reports->createReport($reportName, 'Accounts');
        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();

        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        $listView->clickNameLink($reportName);
        $detailView->waitForDetailViewVisible();

        // Delete report
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\SideBar $sidebar
     * @param \Step\Acceptance\Reports $reports
     *
     * As administrative user I want to edit the record by selecting it in the detail view
     */
    public function testScenarioEditReportFromDetailView(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sidebar,
        \Step\Acceptance\Reports $reports
    ) {
        $I->wantTo('Edit a Report from the detail view');

        // Navigate to a report
        $I->loginAsAdmin();
        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        // Select create report from sidebar
        $I->see('Create Report', '.actionmenulink');
        $sidebar->clickSideBarAction('Create');

        // Create a report
        $this->fakeData->seed($this->fakeDataSeed);
        $reportName = 'Test_'. $this->fakeData->company();
        $reports->createReport($reportName, 'Accounts');
        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();

        // Edit Record
        $detailView->clickActionMenuItem('Edit');

        // Save record
        $editView->click('Save');
        $detailView->waitForDetailViewVisible();

        // Delete report
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\SideBar $sidebar
     * @param \Step\Acceptance\Reports $reports
     *
     * As administrative user I want to duplicate the report
     */
    public function testScenarioDuplicateReportFromDetailView(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sidebar,
        \Step\Acceptance\Reports $reports
    ) {
        $I->wantTo('Duplicate Report from detail view');

        // Navigate to a report
        $I->loginAsAdmin();
        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        // Select create report from sidebar
        $I->see('Create Report', '.actionmenulink');
        $sidebar->clickSideBarAction('Create');

        // Create a report
        $this->fakeData->seed($this->fakeDataSeed);
        $reportName = 'Test_'. $this->fakeData->company();
        $reports->createReport($reportName, 'Accounts');
        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();

        // duplicate Record
        $detailView->clickActionMenuItem('Duplicate');
        $editView->fillField('#name', $reportName . '_1');

        // Save record
        $editView->click('Save');

        $detailView->waitForDetailViewVisible();

        // Delete report
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\SideBar $sidebar
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\Reports $reports
     *
     * As administrative user I want to delete the report by selecting it in the detail view
     */
    public function testScenarioDeleteRecordFromDetailView(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\SideBar $sidebar,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\Reports $reports
    ) {
        $I->wantTo('Delete Report from detail view');

        // Navigate to a report
        $I->loginAsAdmin();
        $I->visitPage('AOR_Reports', 'index');
        $listView->waitForListViewVisible();

        // Select create report from sidebar
        $I->see('Create Report', '.actionmenulink');
        $sidebar->clickSideBarAction('Create');

        // Create a report
        $this->fakeData->seed($this->fakeDataSeed);
        $reportName = 'Test_'. $this->fakeData->company();
        $reports->createReport($reportName, 'Accounts');
        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();

        // Delete Record
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
