<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class AOS_PDF_TemplatesCest
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
     * As an administrator I want to view the pdfTemplates module.
     */
    public function testScenarioViewPDFTemplatesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the pdfTemplates module for testing');

        // Navigate to pdfTemplates list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_PDF_Templates', 'index');
        $listView->waitForListViewVisible();

        $I->see('PDF - Templates', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\PDFTemplates $pdfTemplate
     *
     * As administrative user I want to create a PDF template so that I can test
     * the standard fields.
     */
    public function testScenarioCreatePDFTemplate(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\PDFTemplates $pdfTemplate
    ) {
        $I->wantTo('Create a PDF Template');

        // Navigate to PDF Template list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_PDF_Templates', 'index');
        $listView->waitForListViewVisible();

        // Create PDF Template
        $this->fakeData->seed($this->fakeDataSeed);
        $pdfTemplate->createPDFTemplate('Test_'. $this->fakeData->company());

        // Delete PDF Template
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
