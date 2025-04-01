<?php

namespace Test\Api\V8;

use ApiTester;
use Codeception\Example;

#[\AllowDynamicProperties]
class GetFieldsMetaCest
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
     * @throws \Exception
     */
    public function shouldWork(ApiTester $I, Example $example)
    {
        /** @var \ArrayIterator $iterator */
        $iterator = $example->getIterator();

        $I->sendGET($I->getInstanceURL() . $iterator->offsetGet('endPoint'));
        $I->seeResponseCodeIs(200);
        $I->seeResponseIsJson();
        $I->seeResponseContainsJson(['type' => 'fields']);
        $I->seeResponseContainsJson(
            [
                'name' => [
                    'type' => 'name',
                    "dbType" => "varchar",
                ],
            ]
        );
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
        $endpoint = $I->getInstanceURL() . $iterator->offsetGet('endPoint');
        $detail = $iterator->offsetGet('detail');

        $expectedResult = [
            'errors' => [
                'status' => 400,
                'title' => null,
                'detail' => $detail
            ]
        ];

        $I->sendGET($endpoint);
        $I->seeResponseCodeIs(400);
        $I->seeResponseIsJson();
        $I->seeResponseEquals(json_encode($expectedResult, JSON_PRETTY_PRINT));
    }

    /**
     * @return array
     */
    protected function shouldWorkDataProvider()
    {
        return [
            [
                'shouldWork01' => 'returnsAccountsFields',
                'endPoint' => '/Api/V8/meta/fields/Accounts',
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
                'shouldNotWork01' => 'withInvalidModuleName',
                'endPoint' => '/Api/V8/meta/fields/FooBar',
                'detail' => '[SuiteCRM] [Not Allowed] The API user does not have access to this module.',
            ],
        ];
    }
}
