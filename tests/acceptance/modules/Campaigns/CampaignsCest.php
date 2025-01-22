<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class CampaignsCest
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
     * As an administrator I want to view the campaigns module.
     */
    public function testScenarioViewCampaignsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the campaigns module for testing');

        // Navigate to campaigns list-view
        $I->loginAsAdmin();
        $I->visitPage('Campaigns', 'index');
        $listView->waitForListViewVisible();

        $I->see('Campaigns', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Campaigns $campaign
     *
     * As an admin user, I want to create a non-emails campaign so that I can
     * test the standard fields.
     */
    public function testScenarioCreateNonEmailCampaign(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Campaigns $campaign
    ) {
        $I->wantTo('Create Non-Email Campaign');

        // Navigate to campaigns list-view
        $I->loginAsAdmin();
        $I->visitPage('Campaigns', 'index');
        $listView->waitForListViewVisible();

        // Create campaign
        $this->fakeData->seed($this->fakeDataSeed);
        $name = 'Test_'. $this->fakeData->company();
        $campaign->createNonEmailCampaign($name);

        // Delete campaign
        $listView->waitForListViewVisible();
        $listView->clickFilterButton();
        $I->fillField('name_basic', $name);
        $I->click('#search_form_submit');
        $listView->waitForListViewVisible();
        $listView->clickNameLink($name);
        $detailView->waitForDetailViewVisible();
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
        $listView->clearFilterButton();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Campaigns $campaign
     * @param \Step\Acceptance\InboundEmailTester $inboundEmailTester
     * @param \Step\Acceptance\EmailMan $EmailManTester
     *
     * As an admin user I want to create a Newsletter campaign so that I can
     * test the standard fields.
     */
    public function testScenarioCreateNewsletterCampaign(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Campaigns $campaign,
        \Step\Acceptance\EmailManTester $EmailManTester,
        \Step\Acceptance\InboundEmailTester $inboundEmailTester
    ) {
        $I->wantTo('Create Newsletter Campaign');

//        $I->loginAsAdmin();
//
//        // Setup email settings
//        $emailMan->createEmailSettings();
//        $inboundEmailTester->createBounceEmail();
//
//        // Navigate to campaigns list-view
//        $I->visitPage('Campaigns', 'index');
//        $listView->waitForListViewVisible();
//
//        // Create Newsletter campaign
//        $this->fakeData->seed($this->fakeDataSeed);
//        $name = 'Test_'. $this->fakeData->firstname();
//        $campaign->createNewletterCampaign($name);
//
//        // Check that campaign is ready to send
//        $I->visitPage('Campaigns', 'index');
//        $listView->waitForListViewVisible();
//        $listView->clickFilterButton();
//        $I->click('Quick Filter');
//        $I->wait(3);
//        $I->fillField('name_basic', $name);
//        $I->click('#search_form_submit');
//        $listView->waitForListViewVisible();
//        $listView->clickNameLink($name);
//        $detailView->clickActionMenuItem('Launch Wizard');
//        $I->wait(5);
//        $I->dontSee('You cannot send a marketing email until your subscription list has at least one entry. You can populate your list after finishing.');
//        $I->visitPage('Campaigns', 'index');
//        $listView->clearFilterButton();
//
//        // Delete campaign
//        $listView->waitForListViewVisible();
//        $listView->clickFilterButton();
//        $I->click('Quick Filter');
//        $I->wait(3);
//        $I->fillField('name_basic', $name);
//        $I->click('#search_form_submit');
//        $listView->waitForListViewVisible();
//        $listView->clickNameLink($name);
//        $detailView->waitForDetailViewVisible();
//        $detailView->clickActionMenuItem('Delete');
//        $detailView->acceptPopup();
//        $listView->waitForListViewVisible();
//        $listView->clearFilterButton();
    }
}
