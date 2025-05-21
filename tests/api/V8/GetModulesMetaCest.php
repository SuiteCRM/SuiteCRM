<?php

namespace Test\Api\V8;

use ApiTester;
use ArrayIterator;
use Codeception\Example;
use Codeception\Exception\ModuleException;
use Exception;

#[\AllowDynamicProperties]
class GetModulesMetaCest
{
    /**
     * @param ApiTester $I
     *
     * @throws ModuleException
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
     * @throws Exception
     */
    public function shouldWork(ApiTester $I, Example $example)
    {
        /** @var ArrayIterator $iterator */
        $iterator = $example->getIterator();

        $I->sendGET($I->getInstanceURL() . $iterator->offsetGet('endPoint'));
        $I->seeResponseCodeIs(200);
        $I->seeResponseIsJson();
        $I->seeResponseContainsJson(['type' => 'modules']);
        $I->seeResponseContainsJson(
            [
                'data' =>
                    [
                        'Accounts' => [
                            'label' => 'Accounts',
                            'access' => ['access']
                        ],
                    ],
            ]
        );
    }

    /**
     * @return array
     */
    protected function shouldWorkDataProvider()
    {
        return [
            [
                'shouldWork01' => 'returnsModules',
                'endPoint' => '/Api/V8/meta/modules',
            ],
        ];
    }
}
