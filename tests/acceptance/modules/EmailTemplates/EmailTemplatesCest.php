<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class EmailTemplatesCest
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
     * As an administrator I want to view the emailTemplate module.
     */
    public function testScenarioViewEmailTemplatesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the emailTemplate module for testing');

        // Navigate to emailTemplate list-view
        $I->loginAsAdmin();
        $I->visitPage('EmailTemplates', 'index');
        $listView->waitForListViewVisible();

        $I->see('Email - Templates', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\EmailTemplates $emailTemplates
     *
     * As administrative user I want to create an Email Template so that I can test
     * the TinyMCE editor in the body_html field.
     */
    public function testScenarioCreateEmailTemplate(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EmailTemplates $emailTemplates
    ) {
        $I->wantTo('Create an Email Template with TinyMCE editor');

        // Navigate to Email Templates list-view
        $I->loginAsAdmin();
        $I->visitPage('EmailTemplates', 'index');
        $listView->waitForListViewVisible();

        // Create Email Template
        $this->fakeData->seed($this->fakeDataSeed);
        $emailTemplates->createEmailTemplate('Test_'. $this->fakeData->company());

        // Delete Email Template
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
