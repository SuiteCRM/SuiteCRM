<?php
namespace Test\Api\V8;

use ApiTester;
use Codeception\Example;
use Codeception\Scenario;

#[\AllowDynamicProperties]
class GetRelationshipCest
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
     * @param Scenario $scenario
     *
     * @dataProvider shouldWorkDataProvider
     * @throws \Exception
     */
    public function shouldWork(ApiTester $I, Example $example, Scenario $scenario)
    {
        /** @var \ArrayIterator $iterator */
        $iterator = $example->getIterator();
        if ($iterator->current() === 'withLoadedRelationship') {
            $scenario->incomplete(
                'Loaded relationship test was skipped temporarily, because Slim has to be added to codeception'
            );
        }

        $accountId = $I->createAccount();
        $contactId = $I->createContact();

        $endpoint = str_replace('{id}', $accountId, (string) $iterator->offsetGet('endPoint'));

        $I->sendGET($I->getInstanceURL() . $endpoint);
        $I->seeResponseCodeIs(200);
        $I->seeResponseIsJson();
        $I->seeResponseJsonMatchesJsonPath('$.meta.message');
        $I->seeResponseContainsJson(
            ['message' => 'There is no relationship set in Account module with contacts link field']
        );

        $I->deleteBean('accounts', $accountId);
        $I->deleteBean('contacts', $contactId);
    }

    /**
     * @param ApiTester $I
     *
     * @throws \Exception
     */
    public function shouldNotWork(ApiTester $I)
    {
        $id = $I->createAccount();
        $endpoint = '/Api/V8/module/Accounts/{id}/relationships/invalidLinkName';
        $endpoint = str_replace('{id}', $id, $endpoint);
        $expectedResult = [
            'errors' => [
                'status' => 400,
                'title' => null,
                'detail' => 'Cannot load relationship invalidLinkName for Account module'
            ]
        ];

        $I->sendGET($I->getInstanceURL() . $endpoint);
        $I->seeResponseCodeIs(400);
        $I->seeResponseIsJson();
        $I->seeResponseEquals(json_encode($expectedResult, JSON_PRETTY_PRINT));

        $I->deleteBean('accounts', $id);
    }

    /**
     * @return array
     */
    protected function shouldWorkDataProvider()
    {
        return [
            [
                'shouldWork01' => 'withoutLoadedRelationship',
                'endPoint' => '/Api/V8/module/Accounts/{id}/relationships/contacts'
            ],
            [
                'shouldWork02' => 'withLoadedRelationship',
                'endPoint' => '/Api/V8/module/Accounts/{id}/relationships/contacts'
            ],
        ];
    }
}
