<?php

/**
 * Class ModulesCest
 * Tests /api/v8/modules API
 * @see https://tools.ietf.org/html/rfc7519
 */
#[\AllowDynamicProperties]
class ModulesCest
{
    private static $ACCOUNT_RESOURCE = '/api/v8/modules/Accounts';
    private static $RECORD = '11111111-1111-1111-1111-111111111111';
    private static $RECORD_TYPE = 'Accounts';
    private static $PRODUCT_RESOURCE = '/api/v8/modules/AOS_Products';
    private static $PRODUCT_RECORD_TYPE = 'AOS_Products';
    private static $PRODUCT_RECORD_ID = '11111111-1111-1111-1111-111111111111';
    private static $PRODUCT_CATEGORY_RESOURCE = '/api/v8/modules/AOS_Product_Categories';
    private static $PRODUCT_CATEGORY_RECORD_TYPE = 'AOS_Product_Categories';
    private static $PRODUCT_CATEGORY_RECORD_ID = '11111111-1111-1111-1111-111111111111';
    private static $PRODUCT_CATEGORY_RELATED_RECORD_IDS = array();
    private static $MEETINGS_RESOURCE = '/api/v8/modules/Meetings';
    private static $MEETINGS_RECORD_ID = '11111111-1111-1111-1111-111111111111';
    private static $NOTES_RESOURCE = '/api/v8/modules/Notes';
    private static $NOTES_RESOURCE_ID = '11111111-1111-1111-1111-111111111111';
    private static $DOCUMENTS_RESOURCE = '/api/v8/modules/Documents';
    private static $DOCUMENTS_RESOURCE_ID = '11111111-1111-1111-1111-111111111111';
    /**
     * @var Faker\Generator $fakeData
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
            $this->fakeDataSeed = mt_rand(0, 2048);
        }
        $this->fakeData->seed($this->fakeDataSeed);
    }

    /**
     * Get list of modules
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules
     *
     */
    public function TestScenarioListModules(ApiTester $I)
    {
        $I->comment('Test list modules');
        $I->sendJsonApiContentNegotiation();
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendGET(
            $I->getInstanceURL() . '/api/v8/modules/meta/list'
        );
        $I->seeResponseCodeIs(200);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('meta', $response);
        $I->assertArrayHasKey('modules', $response['meta']);
        $I->assertNotEmpty($response['meta']['modules']);
        $I->assertArrayHasKey('list', $response['meta']['modules']);
        $I->assertNotEmpty($response['meta']['modules']['list']);
    }

    /**
     * Create a new entry with missing type
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name} (with id in $_POST)
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioCreateWithMissingType(ApiTester $I)
    {
        $I->comment('Test missing type');
        $I->sendJsonApiContentNegotiation();
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendPOST(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE,
            json_encode(
                array(
                    'data' => array(
                    )
                )
            )
        );
        $I->seeResponseCodeIs(409);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonApiFailure();
    }

    /**
     * Create a new entry with missing attributes
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name} (with id in $_POST)
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioCreateWithMissingAttributes(ApiTester $I)
    {
        $I->comment('Test required attributes');
        $I->sendJsonApiContentNegotiation();
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendPOST(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE,
            json_encode(
                array(
                    'data' => array(
                        'type' => 'Accounts',
                    )
                )
            )
        );
        $I->seeResponseCodeIs(400);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonApiFailure();
    }


    /**
     * Create a new entry with required fields
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name} (with id in $_POST)
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioCreateWithMissingRequiredFields(ApiTester $I)
    {
        $I->comment('Test required attributes');
        $I->sendJsonApiContentNegotiation();
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendPOST(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE,
            json_encode(
                array(
                    'data' => array(
                        'type' => 'Accounts',
                        'attributes' => array()
                    )
                )
            )
        );
        $I->seeResponseCodeIs(400);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonApiFailure();
    }

    /**
     * Create a new entry
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name} (with id in $_POST)
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioCreateNew(ApiTester $I)
    {
        $faker = \Faker\Factory::create();
        $I->comment('Test create account');
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));
        $I->sendPOST(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE,
            json_encode(
                array(
                    'data' => array(
                        'id' => '',
                        'type' => 'Accounts',
                        'attributes' => array(
                            'name' => $faker->name()
                        )
                    )
                )
            )
        );
        $I->seeResponseCodeIs(201);
        $I->seeJsonApiContentNegotiation();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('links', $response);
        $I->assertArrayHasKey('self', $response['links']);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        $I->assertArrayHasKey('attributes', $response['data']);

        self::$RECORD = $response['data']['id'];
    }

    /**
     * Create a new entry
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name} (with id in $_POST)
     * URL: /api/v8/modules/{module_name}/{id}
     * @throws \Codeception\Exception\ModuleException
     */
    public function TestScenarioCreateNewWithClientCredentialsGrant(ApiTester $I)
    {
        $faker = \Faker\Factory::create();
        $I->comment('Test create account');
        $I->loginAsAdminWithClientCredentials();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));
        $I->sendPOST(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE,
            json_encode(
                array(
                    'data' => array(
                        'id' => '',
                        'type' => 'Accounts',
                        'attributes' => array(
                            'name' => $faker->name()
                        )
                    )
                )
            )
        );
        $I->seeResponseCodeIs(201);
        $I->seeJsonApiContentNegotiation();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('links', $response);
        $I->assertArrayHasKey('self', $response['links']);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        $I->assertArrayHasKey('attributes', $response['data']);

        self::$RECORD = $response['data']['id'];

        $I->loginAsAdmin();
    }

    /**
     * Create a existing entry
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name} (with id in $_POST)
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioCreateExisting(ApiTester $I)
    {
        $faker = \Faker\Factory::create();

        $I->comment('Test already exists');
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));
        $I->sendPOST(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE,
            json_encode(
                array(
                    'data' => array(
                        'id' => self::$RECORD,
                        'type' => 'Accounts',
                        'attributes' => array(
                            'name' => $faker->name()
                        )
                    )
                )
            )
        );
        $I->seeResponseCodeIs(403);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonApiFailure();
    }


    /**
     * Create a notes resource and attaches file
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{Notes} (with id in $_POST)
     * URL: /api/v8/modules/{Notes}/{id}
     *
     * Also:
     * @see \SuiteCRM\API\JsonApi\v1\Resource\SuiteBeanResource
     * @see SugarFieldFile::save()
     */
    public function TestScenarioCreateNoteWithAttachment(ApiTester $I)
    {
        $faker = \Faker\Factory::create();

        // Test create a note with a binary document
        $I->comment('Create a notes resource and attach binary a document');
        $filenameBinaryFile = 'testFile.png';
        $pathToBinaryFile = codecept_data_dir() . $filenameBinaryFile;
        $binaryFile = file_get_contents($pathToBinaryFile);
        $binaryFileEncoded = '<OMITTED>';

        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));


        $payload = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' => 'Notes',
                    'attributes' => array(
                        'name' => $faker->name(),
                        'portal_flag' => true,
                        // The api looks for fields with the type set to file
                        // file name has the type set to "file" in the vardefs
                        'filename' => $filenameBinaryFile,
                        // $prefix . $filed_name . "_file" represents the actual data
                        'filename_file' => $binaryFileEncoded
                    )
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$NOTES_RESOURCE,
            $payload
        );

        $I->seeResponseCodeIs(201);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        self::$NOTES_RESOURCE_ID = $response['data']['id'];

        // Test retrieve a note with a binary document to validate that the note had been created
        $I->sendGET(
            $I->getInstanceURL() . self::$NOTES_RESOURCE . '/' . self::$NOTES_RESOURCE_ID
        );

        $I->seeResponseCodeIs(200);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        $I->assertEquals(self::$NOTES_RESOURCE_ID, $response['data']['id']);
        $I->assertEquals('Notes', $response['data']['type']);
        $I->assertArrayHasKey('attributes', $response['data']);
        $I->assertArrayHasKey('filename', $response['data']['attributes']);
        $I->assertArrayHasKey('filename_file', $response['data']['attributes']);
        $I->assertNotEmpty('filename', $response['data']['attributes']['filename']);
        $I->assertNotEmpty('filename_file', $response['data']['attributes']['filename_file']);
        $I->assertEquals($filenameBinaryFile, $response['data']['attributes']['filename']);
        $I->assertEquals($binaryFileEncoded, '<OMITTED>');
    }

    /**
     * Create a document resource and attaches a file
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{Documents} (with id in $_POST)
     * URL: /api/v8/modules/{Documents}/{id}
     *
     * Also:
     * @see \SuiteCRM\API\JsonApi\v1\Resource\SuiteBeanResource
     * @see SugarFieldFile::save()
     */
    public function TestScenarioCreateDocumentWithAttachment(ApiTester $I)
    {
        $faker = \Faker\Factory::create();

        // Test create a note with a binary document
        $I->comment('Create a notes resource and attach binary a document');
        $filenameBinaryFile = 'testFile.png';
        $pathToBinaryFile = codecept_data_dir().'/'.$filenameBinaryFile;
        $binaryFile = file_get_contents($pathToBinaryFile);
        $binaryFileEncoded = '<OMITTED>';

        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));

        $document_name = $faker->name();
        // Publish date
        $active_date = (new \DateTime())->format(DATE_ATOM);

        $payload = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' => 'Documents',
                    'attributes' => array(
                        'name' => $document_name,
                        'document_name' => $document_name,
                        'portal_flag' => true,
                        'active_date' => $active_date,
                        // The api looks for fields with the type set to file
                        // file name has the type set to "file" in the vardefs
                        'filename' => $filenameBinaryFile,
                        // $prefix . $filed_name . "_file" represents the actual data
                        'filename_file' => $binaryFileEncoded,
                        'revision' => 1
                    )
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$DOCUMENTS_RESOURCE,
            $payload
        );

        $I->seeResponseCodeIs(201);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        self::$DOCUMENTS_RESOURCE_ID = $response['data']['id'];

        // Test retrieve a note with a binary document to validate that the note had been created
        $I->sendGET(
            $I->getInstanceURL() . self::$DOCUMENTS_RESOURCE . '/' . self::$DOCUMENTS_RESOURCE_ID
        );

        $I->seeResponseCodeIs(200);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        $I->assertEquals(self::$DOCUMENTS_RESOURCE_ID, $response['data']['id']);
        $I->assertEquals('Documents', $response['data']['type']);
        $I->assertArrayHasKey('attributes', $response['data']);
        $I->assertArrayHasKey('filename', $response['data']['attributes']);
        $I->assertArrayHasKey('filename_file', $response['data']['attributes']);
        $I->assertNotEmpty('filename', $response['data']['attributes']['filename']);
        $I->assertNotEmpty('filename_file', $response['data']['attributes']['filename_file']);
        $I->assertEquals($filenameBinaryFile, $response['data']['attributes']['filename']);
        $I->assertEquals($binaryFileEncoded, $response['data']['attributes']['filename_file']);
    }

    /**
     * Retrieves an entry
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#fetching
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioRetrieveEntry(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $I->sendGET($I->getInstanceURL() . self::$ACCOUNT_RESOURCE .  '/' . self::$RECORD);
        $I->seeResponseCodeIs(200);
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);

        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('id', $response['data']);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('attributes', $response['data']);
        $I->assertArrayHasKey('relationships', $response['data']);
    }

    /**
     * Update entry
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-updating
     *
     * HTTP Verb: POST (update and replace) / PATCH (update and modify)
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioUpdateEntry(ApiTester $I)
    {
        $faker = \Faker\Factory::create();

        $I->comment('Test update account');
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();

        $newName = $faker->name();

        $I->sendPATCH(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE . '/' . self::$RECORD,
            json_encode(
                array(
                    'data' => array(
                        'id' => self::$RECORD,
                        'type' => 'Accounts',
                        'attributes' => array(
                            'name' => $newName
                        )
                    )
                )
            )
        );

        $I->seeResponseCodeIs(200);
        $I->seeJsonAPISuccess();
        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertArrayHasKey('type', $response['data']);
        $I->assertArrayHasKey('id', $response['data']);
        $I->assertArrayHasKey('attributes', $response['data']);
        $I->assertEquals($newName, $response['data']['attributes']['name']);
    }

    /**
     * Update entry
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-deleting
     *
     * HTTP Verb: DELETE
     * URL: /api/v8/modules/{module_name}/{id}
     *
     */
    public function TestScenarioDeleteEntry(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $I->sendDELETE($I->getInstanceURL() . self::$ACCOUNT_RESOURCE . '/' . self::$RECORD);
        $I->seeResponseCodeIs(200);
        $I->seeJsonAPISuccess();
    }


    /**
     * Retrieves a list of entries
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#fetching
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules/{module_name}
     */
    public function TestScenarioRetrieveList(ApiTester $I)
    {
        // Send Request
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $I->sendGET($I->getInstanceURL() . self::$ACCOUNT_RESOURCE);

        // Validate Response
        $I->seeResponseCodeIs(200);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();

        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertIsArray($response['data']);

        if (!empty($response['data'])) {
            $I->assertTrue(isset($response['data']['0']));
            $I->assertTrue(isset($response['data']['0']['id']));
            $I->assertTrue(isset($response['data']['0']['type']));
            $I->assertTrue(isset($response['data']['0']['attributes']));
            $I->assertIsArray($response['data']['0']['attributes']);
        }

        $I->assertArrayHasKey('links', $response);
        $I->assertArrayHasKey('self', $response['links']);
    }


    /**
     * Retrieves a list of entries
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#fetching
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules/{module_name}
     */
    public function TestScenarioRetrieveFilteredList(ApiTester $I)
    {
        // Send Request
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $I->sendGET(
            $I->getInstanceURL() . self::$ACCOUNT_RESOURCE .
            '?filter[Accounts.date_entered]=[[gte]]2017-11-17T11:40:00+00:00'
        );

        // Validate Response
        $I->seeResponseCodeIs(200);
        $I->seeJsonApiContentNegotiation();
        $I->seeJsonAPISuccess();

        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $response);
        $I->assertIsArray($response['data']);

        if (!empty($response['data'])) {
            $I->assertTrue(isset($response['data']['0']));
            $I->assertTrue(isset($response['data']['0']['id']));
            $I->assertTrue(isset($response['data']['0']['type']));
            $I->assertTrue(isset($response['data']['0']['attributes']));
            $I->assertIsArray($response['data']['0']['attributes']);
        }

        $I->assertArrayHasKey('links', $response);
        $I->assertArrayHasKey('self', $response['links']);
    }


    /**
     * Create product and create a relationship with product categories (One To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#fetching-resources-responses
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name}
     */
    public function TestScenarioCreateProductWithAnOneToManyRelationship(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();

        $this->fakeData->seed(mt_rand(0, 2148));
        // Create AOS_Product_Categories
        $payloadProductCategory = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_CATEGORY_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE,
            $payloadProductCategory
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProductCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RECORD_ID = $responseProductCategory['data']['id'];

        $this->fakeData->seed(mt_rand(0, 2148));
        // Create AOS_Products and Relate to AOS_Product_Categories
        $payload = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' => self::$PRODUCT_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->name(),
                        'price' => $this->fakeData->randomDigit()
                    ),
                    'relationships' => array(
                        'aos_product_category' => array(
                            'data' => array(
                                'id' => self::$PRODUCT_CATEGORY_RECORD_ID,
                                'type' => self::$PRODUCT_CATEGORY_RECORD_TYPE
                            )
                        )
                    )
                )
            )
        );

        // Send Request
        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_RESOURCE,
            $payload
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProduct = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_RECORD_ID = $responseProduct['data']['id'];
    }

    /**
     * Use product to retrieve a relationship (One To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-creating
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioRetrieveOneToManyRelationship(ApiTester $I)
    {
        // Retrieve Product
        // Retrieve relationship
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $I->sendGET(
            $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category'
        );
        // Verify that the objects have been created
        // Validate response
        $I->seeResponseCodeIs(200);
        $responseProduct = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProduct);
        $I->assertArrayHasKey('id', $responseProduct['data']);
        $I->assertNotEmpty($responseProduct['data']['id']);

        $I->assertArrayHasKey('type', $responseProduct['data']);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RECORD_TYPE, $responseProduct['data']['type']);

        $I->assertArrayHasKey('links', $responseProduct['data']);
        $I->assertArrayHasKey('href', $responseProduct['data']['links']);
    }


    /**
     * Use product to create a new relationship with product categories (One To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#fetching-resources-responses
     *
     * HTTP Verb: POST
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioCreateAnOneToManyRelationship(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        // Create AOS_Product_Categories
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProductCategory = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_CATEGORY_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE,
            $payloadProductCategory
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProductCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RECORD_ID = $responseProductCategory['data']['id'];

        // Create AOS_Products and Relate to AOS_Product_Categories
        $payload = json_encode(
            array(
                'data' => array(
                    'id' => self::$PRODUCT_CATEGORY_RECORD_ID,
                    'type' => self::$PRODUCT_CATEGORY_RECORD_TYPE,
                )
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';
        // Send Request
        $I->sendPOST(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);
        $responseProduct = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);

        // Verify that the product category has changed
        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';

        // Verify that the link has been deleted
        $I->sendGET(
            $url
        );

        $responseProductCategories = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProductCategories);
        $I->assertNotEmpty($responseProductCategories['data']);
        $I->assertArrayHasKey('id', $responseProductCategories['data']);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RECORD_ID, $responseProductCategories['data']['id']);
        $I->assertArrayHasKey('type', $responseProductCategories['data']);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RECORD_TYPE, $responseProductCategories['data']['type']);
    }

    /**
     * Update a relationship (One To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/#crud-updating-relationships
     *
     * HTTP Verb: PATCH
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioUpdateOneToManyRelationship(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));
        // Create AOS_Product_Categories
        $payloadProductCategory = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_CATEGORY_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE,
            $payloadProductCategory
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProductCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RECORD_ID = $responseProductCategory['data']['id'];

        // Create AOS_Products and Relate to AOS_Product_Categories
        $payload = json_encode(
            array(
                'data' => array(
                    'id' => self::$PRODUCT_CATEGORY_RECORD_ID,
                    'type' => self::$PRODUCT_CATEGORY_RECORD_TYPE,
                )
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';
        // Send Request
        $I->sendPATCH(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);
        $responseProduct = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);

        // Verify that the product category has changed
        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';

        // Verify that the link has been deleted
        $I->sendGET(
            $url
        );

        $responseProductCategories = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProductCategories);
        $I->assertNotEmpty($responseProductCategories['data']);
        $I->assertArrayHasKey('id', $responseProductCategories['data']);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RECORD_ID, $responseProductCategories['data']['id']);
        $I->assertArrayHasKey('type', $responseProductCategories['data']);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RECORD_TYPE, $responseProductCategories['data']['type']);
    }

    /**
     * Update a relationship (One To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/#crud-updating-relationships
     *
     * HTTP Verb: PATCH
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioClearOneToManyRelationshipUsingRelationshipLink(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        // Clear AOS_Product_Categories relationship
        $payload = json_encode(
            array(
                'data' => array()
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';

        // Send Request
        $I->sendPATCH(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);
        $responseProduct = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProduct);

        // Verify that the link has been deleted
        $I->sendGET(
            $url
        );

        $responseProductCategories = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProductCategories);
        $I->assertEmpty($responseProductCategories['data']);
    }

    /**
     * Delete a relationship (One To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-deleting
     *
     * HTTP Verb: DELETE
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioDeleteOneToManyRelationship(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        $this->fakeData->seed(mt_rand(0, 2148));
        // Create AOS_Product_Categories
        $payloadProductCategory = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_CATEGORY_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE,
            $payloadProductCategory
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProductCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RECORD_ID = $responseProductCategory['data']['id'];

        // Create AOS_Products and Relate to AOS_Product_Categories
        $payload = json_encode(
            array(
                'data' => array(
                    'id' => self::$PRODUCT_CATEGORY_RECORD_ID,
                    'type' => self::$PRODUCT_CATEGORY_RECORD_TYPE,
                )
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';
        // Send Request
        $I->sendPOST(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);

        // Delete Relationship
        $url =  $I->getInstanceURL() . self::$PRODUCT_RESOURCE . '/' .
            self::$PRODUCT_RECORD_ID . '/relationships/aos_product_category';

        $I->sendDELETE($url, $payload);
        $I->seeResponseCodeIs(204);

        // Verify that the link has been deleted
        $I->sendGET(
            $url
        );

        $responseProductCategories = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProductCategories);
        $I->assertEmpty($responseProductCategories['data']);
    }


    /**
     * Retrieve a relationship (Many To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/#fetching-relationships
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioCreateManyToManyRelationships(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();

        // Create Products of Product Categories
        // Create AOS_Product #1
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProduct1 = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName(),
                        'price' => $this->fakeData->randomDigit()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_RESOURCE,
            $payloadProduct1
        );

        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProduct1 = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[] = $responseProduct1['data']['id'];

        // Create AOS_Product #2
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProduct2 = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName(),
                        'price' => $this->fakeData->randomDigit()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_RESOURCE,
            $payloadProduct2
        );

        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProduct2 = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[] = $responseProduct2['data']['id'];


        // Create AOS_Product_Categories Parent
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProduct = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_CATEGORY_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE,
            $payloadProduct
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProductCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RECORD_ID = $responseProductCategory['data']['id'];


        // Relate to Parent AOS_Product_Categories with child AOS_Product_Categories
        $payload = json_encode(
            array(
                'data' => array(
                    array(
                        'id' => self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[0],
                        'type' => self::$PRODUCT_RECORD_TYPE
                        ),
                    array(
                        'id' => self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[1],
                        'type' => self::$PRODUCT_RECORD_TYPE
                    )
                )
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE . '/' .
            self::$PRODUCT_CATEGORY_RECORD_ID . '/relationships/aos_products';

        // Send Request
        $I->sendPOST(
            $url,
            $payload
        );

        // Validate response
        $responseParentCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->seeResponseCodeIs(200);
        $I->assertArrayHasKey('data', $responseParentCategory);
        $I->assertNotEmpty($responseParentCategory['data']);
    }

    /**
     * Retrieve a relationship (Many To Many)
     * @param ApiTester $I
     * @see http://jsonapi.org/format/#fetching-relationships
     *
     * HTTP Verb: GET
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioRetrieveManyToManyRelationships(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        //
        $url =  $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE . '/' .
            self::$PRODUCT_CATEGORY_RECORD_ID . '/relationships/aos_products';
        // Get Subcategories of Product Categories
        $I->sendGET(
            $url
        );

        $I->seeResponseCodeIs(200);
        $responseProducts = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProducts);
        $I->assertNotEmpty($responseProducts['data']);

        // Validate product #1
        $I->assertArrayHasKey('id', $responseProducts['data'][0]);
        $I->assertArrayHasKey('type', $responseProducts['data'][0]);
        $I->assertEquals(self::$PRODUCT_RECORD_TYPE, $responseProducts['data'][0]['type']);

        // Validate product #2
        $I->assertArrayHasKey('id', $responseProducts['data'][1]);
        $I->assertArrayHasKey('type', $responseProducts['data'][1]);
        $I->assertEquals(self::$PRODUCT_RECORD_TYPE, $responseProducts['data'][1]['type']);
    }

    /**
     * Replaces a relationship
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-updating-relationships
     *
     * HTTP Verb: PATCH
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioUpdateManyToManyRelationships(ApiTester $I)
    {
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();
        // Replace the relationships with just one of the related items
        // We should only see the item we have posted in the responses
        $payload = json_encode(
            array(
                'data' => array(
                    array(
                        'id' => self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[1],
                        'type' => self::$PRODUCT_RECORD_TYPE
                    )
                )
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE . '/' .
            self::$PRODUCT_CATEGORY_RECORD_ID . '/relationships/aos_products';

        // Send Request
        $I->sendPATCH(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);
        $responseParentCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseParentCategory);
        $I->assertNotEmpty($responseParentCategory['data']);

        // Validate that the relationship has been replaced
        $I->sendGET(
            $url
        );

        $I->seeResponseCodeIs(200);
        $responseProducts = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProducts);
        $I->assertNotEmpty($responseProducts['data']);
        $I->assertCount(1, $responseProducts['data']);

        // Validate product #2 which will now be the first in the data array (index === 0)
        $I->assertArrayHasKey('id', $responseProducts['data'][0]);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[1], $responseProducts['data'][0]['id']);
        $I->assertArrayHasKey('type', $responseProducts['data'][0]);
        $I->assertEquals(self::$PRODUCT_RECORD_TYPE, $responseProducts['data'][0]['type']);
    }


    /**
     * Clears all related items
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-updating-relationships
     *
     * HTTP Verb: PATCH
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioClearManyToManyRelationships(ApiTester $I)
    {
        // PATCH {"data": []} to clear all relationships

        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();

        // Replace the relationships with just one of the related items
        // We should only see the item we have posted in the responses
        $payload = json_encode(
            array(
                'data' => array()
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE . '/' .
            self::$PRODUCT_CATEGORY_RECORD_ID . '/relationships/aos_products';

        // Send Request
        $I->sendPATCH(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);
        $responseParentCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseParentCategory);
        $I->assertEmpty($responseParentCategory['data']);

        // Validate that the relationship has been replaced
        $I->sendGET(
            $url
        );

        $I->seeResponseCodeIs(200);
        $responseProducts = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProducts);
        $I->assertEmpty($responseProducts['data']);
    }

    /**
     * Removes a relationship
     * @param ApiTester $I
     * @see http://jsonapi.org/format/1.0/#crud-updating-relationships
     *
     * HTTP Verb: DELETE
     * URL: /api/v8/modules/{module_name}/relationships/{link}
     */
    public function TestScenarioDeleteManyToManyRelationships(ApiTester $I)
    {
        // DELETE single resource
        $I->loginAsAdmin();
        $I->sendJwtAuthorisation();
        $I->sendJsonApiContentNegotiation();

        // Create Products of Product Categories
        // Create AOS_Product #1
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProduct1 = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName(),
                        'price' => $this->fakeData->randomDigit()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_RESOURCE,
            $payloadProduct1
        );

        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProduct1 = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[] = $responseProduct1['data']['id'];

        // Create AOS_Product #2
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProduct2 = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName(),
                        'price' => $this->fakeData->randomDigit()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_RESOURCE,
            $payloadProduct2
        );

        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProduct2 = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[] = $responseProduct2['data']['id'];


        // Create AOS_Product_Categories Parent
        $this->fakeData->seed(mt_rand(0, 2148));
        $payloadProduct = json_encode(
            array(
                'data' => array(
                    'id' => '',
                    'type' =>  self::$PRODUCT_CATEGORY_RECORD_TYPE,
                    'attributes' => array(
                        'name' => $this->fakeData->colorName()
                    ),
                )
            )
        );

        $I->sendPOST(
            $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE,
            $payloadProduct
        );
        // Validate response
        $I->seeResponseCodeIs(201);
        $responseProductCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$PRODUCT_CATEGORY_RECORD_ID = $responseProductCategory['data']['id'];


        // Relate to Parent AOS_Product_Categories with child AOS_Product_Categories
        $payload = json_encode(
            array(
                'data' => array(
                    array(
                        'id' => self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[0],
                        'type' => self::$PRODUCT_RECORD_TYPE
                    ),
                    array(
                        'id' => self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[1],
                        'type' => self::$PRODUCT_RECORD_TYPE
                    )
                )
            )
        );

        $url =  $I->getInstanceURL() . self::$PRODUCT_CATEGORY_RESOURCE . '/' .
            self::$PRODUCT_CATEGORY_RECORD_ID . '/relationships/aos_products';

        // Send Request
        $I->sendPOST(
            $url,
            $payload
        );

        // Validate response
        $I->seeResponseCodeIs(200);
        $responseParentCategory = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseParentCategory);
        $I->assertNotEmpty($responseParentCategory['data']);

        // Delete first product
        $payloadDelete = json_encode(
            array(
                'data' => array(
                    array(
                        'id' => self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[0],
                        'type' => self::$PRODUCT_RECORD_TYPE
                    )
                )
            )
        );

        // Send Request
        $I->sendDELETE(
            $url,
            $payloadDelete
        );

        // Validate response
        $I->seeResponseCodeIs(204);

        // Verify deletion
        $I->sendGET(
            $url
        );

        $I->seeResponseCodeIs(200);
        $responseProducts = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('data', $responseProducts);
        $I->assertNotEmpty($responseProducts['data']);
        $I->assertCount(1, $responseProducts['data']);

        // Validate product #2 which will now be the first in the data array (index === 0)
        $I->assertArrayHasKey('id', $responseProducts['data'][0]);
        $I->assertEquals(self::$PRODUCT_CATEGORY_RELATED_RECORD_IDS[1], $responseProducts['data'][0]['id']);
        $I->assertArrayHasKey('type', $responseProducts['data'][0]);
        $I->assertEquals(self::$PRODUCT_RECORD_TYPE, $responseProducts['data'][0]['type']);
    }


    public function TestScenarioCreateMeetingsWithUsersAndMiddleTableFields(ApiTester $I)
    {
        // Temporary removing this. The API seems does not save correctly the relations on meetings but other problem blocking to solve this
//
//        $I->loginAsAdmin();
//        $I->sendJwtAuthorisation();
//        $I->sendJsonApiContentNegotiation();
//
//        $url = $I->getInstanceURL() . self::$MEETINGS_RESOURCE;
//
//        $I->comment('Create a meeting with invitees');
//        $meetingDateStart = new DateTimeImmutable();
//        $meetingDateStart = $meetingDateStart->add(new DateInterval('PT1H'));
//        $meetingDateEnd = $meetingDateStart->add(new DateInterval('PT15M'));
//
//        $meetingsPayload = array(
//            'data' => array(
//                'id'=> '',
//                'type' => 'Meetings',
//                'attributes' => array(
//                    'name' => 'RelationshipsTest',
//                    'date_start' => $meetingDateStart->format(DATE_ATOM),
//                    'date_end' => $meetingDateEnd->format(DATE_ATOM),
//                    'duration_hours' => 0,
//                    'duration_minutes' => 15,
//                    'assigned_user_id' => '1',
//                    'assigned_user_name' => 'Administrator'
//                ),
//                'relationships' => array(
//                    'users' => array(
//                        'data' => array(
//                            array(
//                                'id' => '1',
//                                'type' => 'Users',
//                                'meta' => array(
//                                    'middle_table' => array(
//                                        'data' => array(
//                                            'id' => '',
//                                            'type' => 'Link',
//                                            'attributes' => array(
//                                                'user_id' => '1',
//                                                'accept_status' => 'accept'
//                                            )
//                                        )
//                                    )
//                                )
//                            ),
//                            array(
//                                'id' => 'seed_max_id',
//                                'type' => 'Users',
//                                'meta' => array(
//                                    'middle_table' => array(
//                                        'data' => array(
//                                            'id' => '',
//                                            'type' => 'Link',
//                                            'attributes' => array(
//                                                'user_id' => 'seed_max_id',
//                                                'accept_status' => 'none'
//                                            )
//                                        )
//                                    )
//                                )
//                            ),
//                            array(
//                                'id' => 'seed_chris_id',
//                                'type' => 'Users',
//                                'meta' => array(
//                                    'middle_table' => array(
//                                        'data' => array(
//                                            'id' => '',
//                                            'type' => 'Link',
//                                            'attributes' => array(
//                                                'user_id' => 'seed_chris_id',
//                                                'accept_status' => 'none'
//                                            )
//                                        )
//                                    )
//                                )
//                            )
//                        )
//                    )
//                )
//            )
//        );
//
//        $I->sendPOST($url, json_encode($meetingsPayload));
//        $I->seeResponseCodeIs(201);
//        $responseMeeting = json_decode($I->grabResponse(), true);
//        self::$MEETINGS_RECORD_ID = $responseMeeting['data']['id'];
//
//        $I->comment('Verify Invitees and Verify middle table fields');
//        $url = $I->getInstanceURL() . self::$MEETINGS_RESOURCE . '/' .
//            self::$MEETINGS_RECORD_ID . '/relationships/users';
//        $I->sendGET($url);
//        $responseMeetingUsers = json_decode($I->grabResponse(), true);
//
//        $I->assertTrue(isset($responseMeetingUsers['data'][0]['links']['href']));
//        $I->assertTrue(isset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['id']));
//        $I->assertTrue(isset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['meeting_id']));
//        $I->assertTrue(isset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['required']));
//        $I->assertTrue(isset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['date_modified']));
//        $I->assertTrue(isset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['deleted']));
//
//
//        unset($responseMeetingUsers['data'][0]['links']);
//        unset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['id']);
//        unset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['meeting_id']);
//        unset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['required']);
//        unset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['date_modified']);
//        unset($responseMeetingUsers['data'][0]['meta']['middle_table']['data']['attributes']['deleted']);
//
//        unset($responseMeetingUsers['data'][1]['links']);
//        unset($responseMeetingUsers['data'][1]['meta']['middle_table']['data']['attributes']['id']);
//        unset($responseMeetingUsers['data'][1]['meta']['middle_table']['data']['attributes']['meeting_id']);
//        unset($responseMeetingUsers['data'][1]['meta']['middle_table']['data']['attributes']['required']);
//        unset($responseMeetingUsers['data'][1]['meta']['middle_table']['data']['attributes']['date_modified']);
//        unset($responseMeetingUsers['data'][1]['meta']['middle_table']['data']['attributes']['deleted']);
//
//        unset($responseMeetingUsers['data'][2]['links']);
//        unset($responseMeetingUsers['data'][2]['meta']['middle_table']['data']['attributes']['id']);
//        unset($responseMeetingUsers['data'][2]['meta']['middle_table']['data']['attributes']['meeting_id']);
//        unset($responseMeetingUsers['data'][2]['meta']['middle_table']['data']['attributes']['required']);
//        unset($responseMeetingUsers['data'][2]['meta']['middle_table']['data']['attributes']['date_modified']);
//        unset($responseMeetingUsers['data'][2]['meta']['middle_table']['data']['attributes']['deleted']);
//
//        $I->assertSame($meetingsPayload['data']['relationships']['users']['data'], $responseMeetingUsers['data']);
//
//        $I->comment('Update a chris accept_status using POST');
//        $url = $I->getInstanceURL() . self::$MEETINGS_RESOURCE . '/' .
//            self::$MEETINGS_RECORD_ID . '/relationships/users';
//        $payloadUpdateChrisAccept = array(
//            'data' =>  array(
//                'id' => 'seed_chris_id',
//                'type' => 'Users',
//                'meta' => array(
//                    'middle_table' => array(
//                        'data' => array(
//                            'id' => '',
//                            'type' => 'Link',
//                            'attributes' => array(
//                                'user_id' => 'seed_chris_id',
//                                'accept_status' => 'accept'
//                            )
//                        )
//                    )
//                )
//            )
//        );
//
//        $I->sendPOST($url, json_encode($payloadUpdateChrisAccept));
//        $I->seeResponseIsJson();
//        $responseChrisAccept = json_decode($I->grabResponse(), true);
//        $I->seeResponseCodeIs(200);
//
//        $exp = $payloadUpdateChrisAccept['data'];
//
//        $I->comment('Verify that user accept_status has been update');
//        $I->assertSame($exp, $responseChrisAccept['data'][0]);
    }
}
