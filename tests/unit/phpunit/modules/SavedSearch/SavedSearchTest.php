<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class SavedSearchTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function testSavedSearch(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $savedSearch = BeanFactory::newBean('SavedSearch');

        self::assertInstanceOf('SavedSearch', $savedSearch);
        self::assertInstanceOf('SugarBean', $savedSearch);

        self::assertEquals('saved_search', $savedSearch->table_name);
        self::assertEquals('SavedSearch', $savedSearch->module_dir);
        self::assertEquals('SavedSearch', $savedSearch->object_name);

        //test with parameters
        $savedSearch = new SavedSearch(['id', 'name'], 'id', 'ASC');

        self::assertEquals('id', $savedSearch->orderBy);
        self::assertEquals('ASC', $savedSearch->sortOrder);
    }

    public function testgetForm(): void
    {
        $result = (new SavedSearch(array('id', 'name'), 'id', 'ASC'))->getForm('Leads');

        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function testgetSelect(): void
    {
        $result = (new SavedSearch(array('id', 'name'), 'id', 'ASC'))->getSelect('Leads');

        self::assertGreaterThan(0, strlen((string) $result));
    }

    public function handleSaveAndRetrieveSavedSearch($id): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');
        $searchModuleBean = BeanFactory::newBean('Leads');

        $_REQUEST['search_module'] = 'Leads';
        $_REQUEST['description'] = 'test description';
        $_REQUEST['test_content'] = 'test text';

        $expected = array('search_module' => 'Leads', 'description' => 'test description', 'test_content' => 'test text', 'advanced' => true);

        //execute the method and then retrieve back to verify contents attribute
        $savedSearch->handleSave('', false, false, $id, $searchModuleBean);
        $savedSearch->retrieveSavedSearch($id);
        self::assertSame($expected, $savedSearch->contents);
    }

    public function handleDelete($id): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');

        $savedSearch->handleDelete($id);

        $result = $savedSearch->retrieve($id);
        self::assertEquals(null, $result);
    }

    public function returnSavedSearch($id): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $savedSearch->returnSavedSearch($id);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function returnSavedSearchContents($id): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $result = $savedSearch->returnSavedSearchContents($id);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testhandleRedirect(): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');

        $search_query = '&orderBy=&sortOrder=&query=&searchFormTab=&showSSDIV=';

        //$savedSearch->handleRedirect("Leads", $search_query, 1, 'true');
        self::markTestIncomplete('method uses die');
    }

    public function testfill_in_additional_list_fields(): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');

        $savedSearch->assigned_user_id = 1;
        $savedSearch->contents = array('search_module' => 'Leads');

        $savedSearch->fill_in_additional_list_fields();

        self::assertEquals('Leads', $savedSearch->search_module);
        self::assertEquals('Administrator', $savedSearch->assigned_user_name);
    }

    public function testpopulateRequest(): void
    {
        $savedSearch = BeanFactory::newBean('SavedSearch');

        $savedSearch->contents = array('search_module' => 'Accounts',
                                        'description' => 'test text',
                                        'test_content' => 'some content',
                                        'advanced' => true, );

        $savedSearch->populateRequest();

        // verify that Request parameters are set
        self::assertEquals('Accounts', $_REQUEST['search_module']);
        self::assertEquals('test text', $_REQUEST['description']);
        self::assertEquals('some content', $_REQUEST['test_content']);
    }
}
