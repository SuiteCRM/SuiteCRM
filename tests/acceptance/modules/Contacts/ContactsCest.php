<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class ContactsCest
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
     * As an administrator I want to view the contacts module.
     */
    public function testScenarioViewContactsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the contacts module for testing');

        // Navigate to contacts list-view
        $I->loginAsAdmin();
        $I->visitPage('Contacts', 'index');
        $listView->waitForListViewVisible();

        $I->see('Contacts', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Contacts $contact
     *
     * As administrative user I want to create a contact so that I can test
     * the standard fields.
     */
    public function testScenarioCreateContact(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Contacts $contact
    ) {
        $I->wantTo('Create a Contact');

        // Navigate to contacts list-view
        $I->loginAsAdmin();
        $I->visitPage('Contacts', 'index');
        $listView->waitForListViewVisible();

        // Create contact
        $this->fakeData->seed($this->fakeDataSeed);
        $contact->createContact('Test_'. $this->fakeData->company());

        // Delete contact
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
