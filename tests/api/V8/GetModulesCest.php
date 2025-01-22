<?php
namespace Test\Api\V8;

use ApiTester;
use Codeception\Example;

#[\AllowDynamicProperties]
class GetModulesCest
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
     * @dataProvider shouldNotWorkDataProvider
     * @throws \Exception
     */
    public function shouldNotWork(ApiTester $I, Example $example)
    {
        /** @var \ArrayIterator $iterator */
        $iterator = $example->getIterator();

        $expectedResult = [
            'errors' => [
                'status' => 400,
                'title' => null,
                'detail' => $iterator->offsetGet('detail')
            ]
        ];

        $I->sendGET($I->getInstanceURL() . $iterator->offsetGet('endPoint'));
        $I->seeResponseCodeIs(400);
        $I->seeResponseIsJson();
        $I->seeResponseEquals(json_encode($expectedResult, JSON_PRETTY_PRINT));
    }

    /**
     * @return array
     */
    protected function shouldNotWorkDataProvider()
    {
        return [
            [
                'shouldNotWork01' => 'withInvalidPage',
                'endPoint' => '/Api/V8/module/Accounts?page[size]=-1',
                'detail' => 'The option "page" with value array is invalid.'
            ],
            [
                'shouldNotWork02' => 'withInvalidPageKey',
                'endPoint' => '/Api/V8/module/Accounts?page[invalidParameter]=1',
                'detail' => 'The option "invalidParameter" does not exist. Defined options are: "number", "size".'
            ],
            [
                'shouldNotWork03' => 'withInvalidSort',
                'endPoint' => '/Api/V8/module/Accounts?sort=invalidField',
                'detail' => 'Sort field invalidField in Account module is not found'
            ],
            [
                'shouldNotWork04' => 'withInvalidFilter',
                'endPoint' => '/Api/V8/module/Accounts?filter[operator]=and&filter[invalidProperty][eq]=Customer',
                'detail' => 'Filter field invalidProperty in Account module is not found'
            ],
            [
                'shouldNotWork05' => 'withInvalidFilterOperator',
                'endPoint' => '/Api/V8/module/Accounts?filter[operator]=and&filter[account_type][eq111]=Customer',
                'detail' => 'Filter operator eq111 is invalid'
            ],
        ];
    }
}
