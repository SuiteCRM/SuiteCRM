<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class QuotesCest
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
     * As an administrator I want to view the quotes module.
     */
    public function testScenarioViewQuotesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the quotes module for testing');

        // Navigate to quotes list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Quotes', 'index');
        $listView->waitForListViewVisible();

        $I->see('Quotes', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     *
     * As an administrator I want to convert a Quote to Invoice and verify defaults are applied.
     */
    public function testQuoteToInvoiceConversion(AcceptanceTester $I)
    {
        $I->wantTo('Convert a Quote to Invoice and verify defaults are applied');

        // Login and navigate
        $I->loginAsAdmin();

        // Create a Quote
        $I->visitPage('AOS_Quotes', 'EditView');
        $I->fillField('#name', 'Test Quote for Conversion');
        // Fill required fields
        $I->fillField('#billing_account', 'Test Account');
        $I->click('Save');

        // Wait for detail view
        $I->waitForText('Test Quote for Conversion', 10);

        // Convert to Invoice
        $I->click('Convert to Invoice');

        // Verify we're on Invoice edit page
        $I->seeInCurrentUrl('module=AOS_Invoices');
        $I->seeInCurrentUrl('action=EditView');

        // Verify default values are present
        $I->seeInField('#invoice_date', date('Y-m-d'));
        $I->seeInField('#name', 'Test Quote for Conversion');
    }
}
