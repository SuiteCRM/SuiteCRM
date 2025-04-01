<?php
namespace Test\Api\V8;

use ApiTester;

#[\AllowDynamicProperties]
class CreateRelationShipCest
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
     *
     * @throws \Exception
     */
    public function shouldWork(ApiTester $I)
    {
        $accountId = $I->createAccount();
        $contactId = $I->createContact();

        $endpoint = $I->getInstanceURL() . '/Api/V8/module/Accounts/{id}/relationships';
        $endpoint = str_replace('{id}', $accountId, $endpoint);

        $payload = [
            'data' => [
                'type' => 'Contacts',
                'id' => '{id}',
            ]
        ];
        $payload['data']['id'] = str_replace('{id}', $contactId, $payload['data']['id']);

        $I->sendPOST($endpoint, $payload);

        $I->seeResponseCodeIs(201);
        $I->seeResponseIsJson();
        $I->seeResponseJsonMatchesJsonPath('$.meta.message');
        $I->seeResponseContainsJson(
            [
                'message' => sprintf(
                    'Contact with id %s has been added to Account with id %s',
                    $contactId,
                    $accountId
                )
            ]
        );

        $I->deleteBean('accounts', $accountId);
        $I->deleteBean('contacts', $contactId);
    }
}
