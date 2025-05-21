<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

use Faker\Generator;
use SuiteCRM\Enumerator\SugarObjectType;

#[\AllowDynamicProperties]
class CompanyModuleCest
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
    public function _before(AcceptanceTester $I): void
    {
        if (!$this->fakeData) {
            $this->fakeData = Faker\Factory::create();
            $this->fakeData->addProvider(new Faker\Provider\en_US\Address($this->fakeData));
            $this->fakeData->addProvider(new Faker\Provider\en_US\PhoneNumber($this->fakeData));
            $this->fakeData->addProvider(new Faker\Provider\en_US\Company($this->fakeData));
            $this->fakeDataSeed = mt_rand(0, 2048);
        }
        $this->fakeData->seed($this->fakeDataSeed);
    }

    /**
     * @param AcceptanceTester $I
     */
    public function _after(AcceptanceTester $I): void
    {
    }

    // Tests
    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\ModuleBuilder $moduleBuilder
     *
     * As an administrator I want to create and deploy a company module so that I can test
     * that the company functionality is working. Given that I have already created a module I expect to deploy
     * the module before testing.
     */
    public function testScenarioCreateCompanyModule(
        \AcceptanceTester $I,
        \Step\Acceptance\ModuleBuilder $moduleBuilder
    ): void {
        $I->wantTo('Create a company module for testing');
        $I->loginAsAdmin();

        $moduleBuilder->createModule(
            \Page\CompanyModule::$PACKAGE_NAME,
            \Page\CompanyModule::$NAME,
            SugarObjectType::company
        );
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\NavigationBarTester $navigationBar
     * @param \Step\Acceptance\ListView $listView
     *
     * As administrative user I want to view my company test module so that I can see if it has been
     * deployed correctly.
     */
    public function testScenarioViewCompanyTestModule(
        \AcceptanceTester $I,
        \Step\Acceptance\NavigationBarTester $navigationBar,
        \Step\Acceptance\ListView $listView
    ): void {
        $I->wantTo('View Company Test Module');

        $I->loginAsAdmin();

        // Navigate to module
        $navigationBar->clickAllMenuItem(\Page\CompanyModule::$NAME);

        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\NavigationBarTester $navigationBar
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\EditView $editView
     * @param \Step\Acceptance\DetailView $detailView
     *
     * As administrative user I want to create a record with my company test module so that I can test
     * the standard fields.
     */
    public function testScenarioCreateRecord(
        \AcceptanceTester $I,
        \Step\Acceptance\NavigationBarTester $navigationBar,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\EditView $editView,
        \Step\Acceptance\DetailView $detailView
    ): void {
        $I->wantTo('Create Company Test Module Record');

        $I->loginAsAdmin();

        // Go to Company Test Module
        $navigationBar->clickAllMenuItem(\Page\CompanyModule::$NAME);
        $listView->waitForListViewVisible();

        // Select create Company Test Module form the current menu
        $navigationBar->clickCurrentMenuItem('Create ' . \Page\CompanyModule::$NAME);

        // Create a record
        $this->fakeData->seed($this->fakeDataSeed);
        $editView->waitForEditViewVisible();
        $editView->fillField('#name', $this->fakeData->company);
        $editView->fillField('#phone_office', $this->fakeData->phoneNumber);
        $editView->fillField('#phone_fax', $this->fakeData->phoneNumber);
        $editView->fillField('#phone_alternate', $this->fakeData->phoneNumber);
        $editView->fillField('#ticker_symbol', $this->fakeData->randomAscii);
        $editView->fillField('#rating', $this->fakeData->randomAscii);
        $editView->fillField('#ownership', $this->fakeData->randomAscii);
        $editView->selectOption('#industry', 'Other');
        $editView->selectOption('test_'.strtolower(\Page\CompanyModule::$NAME).'_type', 'Other');
        $editView->fillField('#billing_address_street', $this->fakeData->streetAddress);
        $editView->fillField('#billing_address_city', $this->fakeData->city);
        $editView->fillField('#billing_address_state', $this->fakeData->randomAscii);
        $editView->fillField('#billing_address_postalcode', $this->fakeData->postcode);
        $editView->fillField('#billing_address_country', $this->fakeData->country);
        $editView->checkOption('#shipping_checkbox');
        $editView->fillField('#Test_'.\Page\CompanyModule::$NAME.'0emailAddress0', $this->fakeData->companyEmail);
        $editView->fillField('#description', $this->fakeData->paragraph);
        $editView->clickSaveButton();
        $detailView->waitForDetailViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\NavigationBarTester $navigationBar
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\DetailView $detailView
     *
     * As administrative user I want to view the record by selecting it in the list view
     */
    public function testScenarioViewRecordFromListView(
        \AcceptanceTester $I,
        \Step\Acceptance\NavigationBarTester $navigationBar,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView
    ): void {
        $I->wantTo('Select Record from list view');

        $I->loginAsAdmin();

        // Go to Company Test Module
        $navigationBar->clickAllMenuItem(\Page\CompanyModule::$NAME);
        $listView->waitForListViewVisible();

        $this->fakeData->seed($this->fakeDataSeed);
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->fillField('#name_basic', $this->fakeData->company);
        $listView->click('Search', '.submitButtons');
        $listView->wait(1);
        $listView->dontSee('No results found');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->clickNameLink($this->fakeData->company);

        $detailView->waitForDetailViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\NavigationBarTester $navigationBar
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\EditView $editView
     *
     * As administrative user I want to edit the record by selecting it in the detail view
     */
    public function testScenarioEditRecordFromDetailView(
        \AcceptanceTester$I,
        \Step\Acceptance\NavigationBarTester $navigationBar,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\EditView $editView
    ): void {
        $I->wantTo('Edit Company Test Module Record from detail view');

        $I->loginAsAdmin();

        // Go to Company Test Module
        $navigationBar->clickAllMenuItem(\Page\CompanyModule::$NAME);
        $listView->waitForListViewVisible();

        // Select record from list view
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->fillField('#name_basic', $this->fakeData->company);
        $listView->click('Search', '.submitButtons');
        $listView->wait(1);
        $listView->dontSee('No results found');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->clickNameLink($this->fakeData->company);

        // Edit Record
        $detailView->clickActionMenuItem('Edit');

        // Save record
        $editView->click('Save');

        $detailView->waitForDetailViewVisible();
        $this->lastView = 'DetailView';
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\NavigationBarTester $navigationBar
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\DetailView $detailView
     * @param \Step\Acceptance\EditView $editView
     *
     * As administrative user I want to duplicate the record
     */
    public function testScenarioDuplicateRecordFromDetailView(
        \AcceptanceTester $I,
        \Step\Acceptance\NavigationBarTester $navigationBar,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView,
        \Step\Acceptance\EditView $editView
    ): void {
        $I->wantTo('Duplicate Company Test Module Record from detail view');
        $I->loginAsAdmin();

        // Go to Company Test Module
        $navigationBar->clickAllMenuItem(\Page\CompanyModule::$NAME);
        $listView->waitForListViewVisible();

        // Select record from list view
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->fillField('#name_basic', $this->fakeData->company);
        $listView->click('Search', '.submitButtons');
        $listView->wait(1);
        $listView->dontSee('No results found');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->clickNameLink($this->fakeData->company);

        // Edit Record
        $detailView->clickActionMenuItem('Duplicate');

        $this->fakeData->seed($this->fakeDataSeed);
        $editView->fillField('#name', $this->fakeData->company . '1');

        // Save record
        $editView->click('Save');

        $detailView->waitForDetailViewVisible();
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();

        $listView->waitForListViewVisible();
    }

    /**
     * @param \AcceptanceTester $I
     * @param \Step\Acceptance\NavigationBarTester $navigationBar
     * @param \Step\Acceptance\ListView $listView
     * @param \Step\Acceptance\DetailView $detailView
     *
     * As administrative user I want to delete the record by selecting it in the detail view
     */
    public function testScenarioDeleteRecordFromDetailView(
        \AcceptanceTester $I,
        \Step\Acceptance\NavigationBarTester $navigationBar,
        \Step\Acceptance\ListView $listView,
        \Step\Acceptance\DetailView $detailView
    ): void {
        $I->wantTo('Delete Company Test Module Record from detail view');

        $I->loginAsAdmin();

        // Go to Company Test Module
        $navigationBar->clickAllMenuItem(\Page\CompanyModule::$NAME);
        $listView->waitForListViewVisible();

        // Select record from list view
        $listView->clickFilterButton();
        $listView->click('Quick Filter');
        $this->fakeData->seed($this->fakeDataSeed);
        $listView->fillField('#name_basic', $this->fakeData->company);
        $listView->click('Search', '.submitButtons');
        $listView->wait(1);
        $listView->dontSee('No results found');

        $this->fakeData->seed($this->fakeDataSeed);
        $listView->clickNameLink($this->fakeData->company);

        // Delete Record
        $detailView->clickActionMenuItem('Delete');
        $detailView->acceptPopup();

        $listView->waitForListViewVisible();
    }
}
