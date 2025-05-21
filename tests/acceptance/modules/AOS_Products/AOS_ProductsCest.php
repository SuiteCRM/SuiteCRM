<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class ProductsCest
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
     * As an admin user, I want to view the products module.
     */
    public function testScenarioViewProductsModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the products module for testing');

        // Navigate to products list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Products', 'index');
        $listView->waitForListViewVisible();

        $I->see('Products', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\Products $product
     *
     * As an admin user, I want to create a product so I can test
     * the standard fields.
     */
    public function testScenarioCreateAccount(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\Products $product
    ) {
        $I->wantTo('Create a product');

        // Navigate to products list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Products', 'index');
        $listView->waitForListViewVisible();

        // Create product
        $this->fakeData->seed($this->fakeDataSeed);
        $product->createProduct('Test_'. $this->fakeData->company());

        // Delete product
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
