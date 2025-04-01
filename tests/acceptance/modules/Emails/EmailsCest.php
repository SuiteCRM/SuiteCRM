<?php

use Faker\Factory;
use Faker\Generator;
use Helper\WebDriverHelper;
use Step\Acceptance\Emails;
use Step\Acceptance\EmailsTester;
use Step\Acceptance\ListView;

#[\AllowDynamicProperties]
class EmailsCest
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
            $this->fakeData = Factory::create();
        }

        $this->fakeDataSeed = mt_rand(0, 2048);
        $this->fakeData->seed($this->fakeDataSeed);
    }

    /**
     * @param AcceptanceTester $I
     * @param ListView $listView
     *
     * As an administrator I want to view the emails module.
     */
    public function testScenarioViewEmailsModule(
        AcceptanceTester $I,
        ListView $listView
    ) {
        $I->wantTo('View the emails module for testing');

        // Navigate to emails list-view
        $I->loginAsAdmin();

        $I->visitPage('Emails', 'index');
        $listView->waitForListViewVisible();

        $I->see('Emails', '.module-title-text');
    }

    /**
     * @param AcceptanceTester $I
     * @param WebDriverHelper $webDriverHelper
     *
     * As an administrator I want to view an email body and check that it's not cached.
     */
    public function testScenarioViewEmailBodyHTML(
        AcceptanceTester $I,
        WebDriverHelper $webDriverHelper,
        \Codeception\Scenario $scenario
    ) {

        // TODO: Refactor
        $scenario->skip('Needs to be re-implemented');

        $I->wantTo('View the HTML of two emails');

        $I->loginAsAdmin();

        // Check for HTML caching issue
        $I->amOnUrl($webDriverHelper->getInstanceURL() . '/index.php?module=Emails&action=DetailView&record=eae65b87-6852-e43c-4213-5b213b39f2aa');
        $I->see('Test Description 1');
        $I->amOnUrl($webDriverHelper->getInstanceURL() . '/index.php?module=Emails&action=DetailView&record=eae65b87-6852-e43c-4213-5b213b39f2ab');
        $I->see('Test Description 2');
    }
}
