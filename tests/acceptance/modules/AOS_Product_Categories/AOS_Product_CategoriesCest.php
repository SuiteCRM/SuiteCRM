<?php

use Faker\Generator;

#[\AllowDynamicProperties]
class AOS_Product_CategoriesCest
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
     * As an administrator I want to view the productCategories module.
     */
    public function testScenarioViewProductCategoriesModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ListView $listView
    ) {
        $I->wantTo('View the productCategories module for testing');

        // Navigate to productCategories list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Product_Categories', 'index');
        $listView->waitForListViewVisible();

        $I->see('Products - Categories', '.module-title-text');
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\ProductCategories $productCategory
     *
     * As administrative user I want to create a product category so that I can test
     * the standard fields.
     */
    public function testScenarioCreateAccount(
        \AcceptanceTester $I,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\ProductCategories $productCategory
    ) {
        $I->wantTo('Create a product category');

        // Navigate to product category list-view
        $I->loginAsAdmin();
        $I->visitPage('AOS_Product_Categories', 'index');
        $listView->waitForListViewVisible();

        // Create product category
        $this->fakeData->seed($this->fakeDataSeed);
        $productCategory->createProductCategory('Test_'. $this->fakeData->company());

        // Delete product category
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();
        $listView->waitForListViewVisible();
    }
}
