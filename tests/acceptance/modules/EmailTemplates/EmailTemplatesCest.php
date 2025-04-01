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
}
