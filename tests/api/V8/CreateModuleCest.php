<?php
namespace Test\Api\V8;

use ApiTester;
use Codeception\Example;

#[\AllowDynamicProperties]
class CreateModuleCest
{
    /**
     * @param ApiTester $I
     *
     * @throws \Codeception\Exception\ModuleException
     */
    public function _before(ApiTester $I)
    {
        $I->login();
    }

    /**
     * @param ApiTester $I
     * @param Example $example
     *
     * @dataProvider shouldWorkDataProvider
     * @throws \Codeception\Exception\ModuleException
     * @throws \Exception
     */
    public function shouldWork(ApiTester $I, Example $example)
    {
        /** @var \ArrayIterator $iterator */
        $iterator = $example->getIterator();
        $payload = $iterator->offsetGet('payload');

        $id = create_guid();
        $endpoint = $I->getInstanceURL() . '/Api/V8/module';
        $response = [
            'type' => \Account::class,
        ];

        if ($iterator->current() === 'withId') {
            $payload['data']['id'] = str_replace('{id}', $id, (string) $payload['data']['id']);
            $response = $response + ['id' => $id];
        }

        $I->sendPOST($endpoint, $payload);
        $I->seeResponseCodeIs(201); // 201 or 200 - both is correct?
        $I->seeResponseIsJson();
        $I->canSeeResponseContainsJson($response);
        $I->assertGreaterThanOrEqual(2, is_countable($I->grabDataFromResponseByJsonPath('$.data.attributes')[0]) ? count($I->grabDataFromResponseByJsonPath('$.data.attributes')[0]) : 0);

        $I->deleteBean('accounts', $id);
    }

    /**
     * @param ApiTester $I
     * @param Example $example
     *
     * @dataProvider shouldNotWorkDataProvider
     * @throws \Exception
     */
    public function shouldNotWork(ApiTester $I, Example $example)
    {
        /** @var \ArrayIterator $iterator */
        $iterator = $example->getIterator();
        $detail = $iterator->offsetGet('detail');
        $payload = $iterator->offsetGet('payload');

        if ($iterator->current() === 'withExistingBean') {
            $id = $I->createAccount();
            $detail = str_replace('{id}', $id, (string) $detail);
            $payload['data']['id'] = str_replace('{id}', $id, (string) $payload['data']['id']);
        }
        $endpoint = $I->getInstanceURL() . '/Api/V8/module';
        $expectedResult = [
            'errors' => [
                'status' => 400,
                'title' => null,
                'detail' => $detail
            ]
        ];

        $I->sendPOST($endpoint, $payload);
        $I->seeResponseCodeIs(400);
        $I->seeResponseIsJson();
        $I->seeResponseEquals(json_encode($expectedResult, JSON_PRETTY_PRINT));

        if (isset($id)) {
            $I->deleteBean('accounts', $id);
        }
    }

    /**
     * @return array
     */
    protected function shouldWorkDataProvider()
    {
        return [
            [
                'shouldWork01' => 'withId',
                'payload' => [
                    'data' => [
                        'type' => 'Accounts',
                        'id' => '{id}',
                        'attributes' => [
                            'name' => 'testName',
                            'account_type' => 'testAccountType'
                        ]
                    ]
                ]
            ],
            [
                'shouldWork02' => 'withOutId',
                'payload' => [
                    'data' => [
                        'type' => 'Accounts'
                    ]
                ],
            ],
        ];
    }

    /**
     * @return array
     */
    protected function shouldNotWorkDataProvider()
    {
        return [
            [
                'shouldNotWork01' => 'withInvalidParameter',
                'payload' => [
                    'data' => [
                        'invalidParam' => 'what-eva'
                    ]
                ],
                'detail' => 'The option "invalidParam" does not exist. Defined options are: "attributes", "id", "type".'
            ],
            [
                'shouldNotWork02' => 'withInvalidType',
                'payload' => [
                    'data' => [
                        'type' => 'InvalidModule',
                        'id' => '86ee02b3-96d2-47b3-bd6d-9e1035daff3a'
                    ]
                ],
                'detail' => 'Module InvalidModule does not exist'
            ],
            [
                'shouldNotWork03' => 'withInvalidId',
                'payload' => [
                    'data' => [
                        'type' => 'Accounts',
                        'id' => '111'
                    ]
                ],
                'detail' => 'The option "id" with value "111" is invalid.'
            ],
            [
                'shouldNotWork04' => 'withInvalidAttribute',
                'payload' => [
                    'data' => [
                        'type' => 'Accounts',
                        'id' => '86ee02b3-96d2-47b3-bd6d-9e1035daff3a',
                        'attributes' => [
                            'invalidAttribute' => 'what-eva'
                        ]
                    ]
                ],
                'detail' => 'Property invalidAttribute in Account module is invalid'
            ],
            [
                'shouldNotWork05' => 'withExistingBean',
                'payload' => [
                    'data' => [
                        'type' => 'Accounts',
                        'id' => '{id}',
                        'attributes' => [
                            'name' => 'testName'
                        ]
                    ]
                ],
                'detail' => 'Bean Accounts with id {id} is already exist'
            ],
        ];
    }
}
