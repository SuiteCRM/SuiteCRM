<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class AOK_KnowledgeBaseCest
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
     * As an administrator I want to view the knowledgeBase module.
     */
    public function testScenarioViewKnowledgeBaseModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the knowledgeBase module for testing');

        // Navigate to knowledgeBase list-view
        $I->loginAsAdmin();
        $I->visitPage('AOK_KnowledgeBase', 'index');
        $listView->waitForListViewVisible();

        $I->see('Knowledge Base', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\KnowledgeBase $knowledgeBase
     *
     * As administrative user I want to create a Knowledge Base so that I can test
     * the standard fields.
     */
    public function testScenarioCreateKnowledgeBase(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\KnowledgeBase $knowledgeBase
    ) {
        $I->wantTo('Create a Knowledge Base');

        // Navigate to Knowledge Base list-view
        $I->loginAsAdmin();
        $I->visitPage('AOK_KnowledgeBase', 'index');
        $listView->waitForListViewVisible();

        // Create Knowledge Base
        $this->fakeData->seed($this->fakeDataSeed);
        $knowledgeBase->createKnowledgeBase('Test_'. $this->fakeData->company());

        // Delete Knowledge Base
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
