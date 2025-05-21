<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class InvoicesCest
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
     * As an administrator I want to view the invoices module.
     */
    public function testScenarioViewInvoicesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the invoices module for testing');

        // Navigate to invoices list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Invoices', 'index');
        $listView->waitForListViewVisible();

        $I->see('Invoices', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Invoices $invoice
     *
     * As administrative user I want to create an invoice so that I can test
     * the standard fields.
     */
    public function testScenarioCreateInvoice(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Invoices $invoice
    ) {
        $I->wantTo('Create an Invoice');

        // Navigate to invoices list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Invoices', 'index');
        $listView->waitForListViewVisible();

        // Create invoice
        $this->fakeData->seed($this->fakeDataSeed);
        $invoice->createInvoice('Test_'. $this->fakeData->company());

        // Delete invoice
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \\Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\Invoices $invoice
     *
     * As administrative user I want to create an invoice and check the number rounding
     */
    public function testScenarioInvoiceRounding(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\Invoices $invoice
    ) {
        $I->wantTo('Create an Invoice');

        // Navigate to invoices list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Invoices', 'index');
        $listView->waitForListViewVisible();

        // Create invoice
        $this->fakeData->seed($this->fakeDataSeed);
        $invoice->createInvoice('Test_'. $this->fakeData->company());

        // Set total
        $detailView->clickActionMenuItem('Edit');
        $editView->waitForEditViewVisible();
        $I->fillField('#total_amt', 0);
        $I->fillField('#discount_amount', 0);
        $I->fillField('#subtotal_amount', 0);
        $I->fillField('#shipping_amount', 475.99999999999994);
        $I->fillField('#shipping_tax_amt', 0);
        $I->fillField('#tax_amount', 0);
        $I->selectOption('#status', 'Unpaid');
        $I->selectOption('#currency_id_select', 'US Dollars : $');
        $I->selectOption('#shipping_tax', '0%');

        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();

        $detailView->clickActionMenuItem('Edit');
        $editView->waitForEditViewVisible();
        $I->scrollTo('#shipping_amount');
        $I->seeInSource('476.00');
        $editView->clickSaveButton();

        // Delete invoice
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
