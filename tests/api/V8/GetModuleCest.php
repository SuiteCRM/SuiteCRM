<?php
namespace Test\Api\V8;

use ApiTester;
use Codeception\Example;

#[\AllowDynamicProperties]
class GetModuleCest
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
        $id = $I->createAccount();
        $endpoint = str_replace('{id}', $id, (string) $iterator->offsetGet('endPoint'));

        $I->sendGET($I->getInstanceURL() . $endpoint);
        $I->seeResponseCodeIs(200);
        $I->seeResponseIsJson();
        $I->canSeeResponseContainsJson([
            'type' => \Account::class,
            'id' => $id
        ]);

        $I->seeResponseJsonMatchesJsonPath('$.data.attributes');
        $assert = $iterator->current() === 'withFields' ? 'assertEquals' : 'assertGreaterThan';
        $attributes = $I->grabDataFromResponseByJsonPath('$.data.attributes')[0];
        $I->{$assert}(2, is_countable($attributes) ? count($attributes) : 0);

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
        $endpoint = $I->getInstanceURL() . $iterator->offsetGet('endPoint');
        $detail = $iterator->offsetGet('detail');

        if (in_array($iterator->current(), ['withInvalidField', 'withInvalidFieldKey'], true)) {
            $id = $I->createAccount();
            $endpoint = str_replace('{id}', $id, $endpoint);
            $detail = str_replace('{id}', $id, (string) $detail);
        }

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

        if (in_array($iterator->current(), ['withInvalidField', 'withInvalidFieldKey'], true)) {
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
                'shouldWork01' => 'withoutParams',
                'endPoint' => '/Api/V8/module/Accounts/{id}'
            ],
            [
                'shouldWork02' => 'withFields',
                'endPoint' => '/Api/V8/module/Accounts/{id}?fields[Accounts]=name,account_type'
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
                'endPoint' => '/Api/V8/module/InvalidModuleName/97c3669b-607a-4b30-964f-2409b55a1551',
                'detail' => 'Module with name InvalidModuleName is not found'
            ],
            [
                'shouldNotWork02' => 'withInvalidId',
                'endPoint' => '/Api/V8/module/Accounts/111',
                'detail' => 'Accounts module with id 111 is not found'
            ],
            [
                'shouldNotWork03' => 'withIdNotExist',
                'endPoint' => '/Api/V8/module/Accounts/97c3669b-607a-4b30-964f-2409b55a1551',
                'detail' => 'Accounts module with id 97c3669b-607a-4b30-964f-2409b55a1551 is not found'
            ],
            [
                'shouldNotWork04' => 'withInvalidParameter',
                'endPoint' => '/Api/V8/module/Accounts/97c3669b-607a-4b30-964f-2409b55a1551?invalidParam',
                'detail' => 'The option "invalidParam" does not exist. Defined options are: "fields", "id", "moduleName".'
            ],
            [
                'shouldNotWork05' => 'withInvalidField',
                'endPoint' => '/Api/V8/module/Accounts/{id}?fields[Accounts]=name,not_exist_property1,not_exist_property2',
                'detail' => 'The following fields in Account module are not found: not_exist_property1, not_exist_property2'
            ],
            [
                'shouldNotWork06' => 'withInvalidFieldKey',
                'endPoint' => '/Api/V8/module/Accounts/{id}?fields[NotExist]=name,account_type',
                'detail' => 'Module NotExist does not exist'
            ],
        ];
    }
}
