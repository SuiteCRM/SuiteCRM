<?php
namespace Test\Api\V8;

use ApiTester;
use \Exception;
use \Codeception\Exception\ModuleException;

require_once __DIR__ . '/../../../include/utils.php';

#[\AllowDynamicProperties]
class CreateRelationShipByLinkCest
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
     *
     * @throws Exception
     */
    public function shouldWork(ApiTester $I)
    {
        $accountId = $I->createAccount();

        $contactId = $I->createContact();

        $accountLabel = translate('Accounts');

        $contactLabel = translate('Contacts');

        $linkName = 'contacts';

        $expectedMessage = sprintf(
            '%s record with id %s has been related to %s record with id %s using link %s',
            $contactLabel,
            $contactId,
            $accountLabel,
            $accountId,
            $linkName
        );

        $endpoint = $I->getInstanceURL() . '/Api/V8/module/Accounts/{id}/relationships/{linkFieldName}';

        $endpoint = str_replace(
            [
                '{id}',
                '{linkFieldName}'
            ],
            [
                $accountId,
                $linkName
            ],
            $endpoint
        );

        $payload = [
            'data' => [
                'type' => 'Contacts',
                'id' => $contactId,
            ]
        ];

        $I->sendPOST($endpoint, $payload);

        $I->seeResponseCodeIs(201);

        $I->seeResponseIsJson();

        $responseJson = $I->grabResponse();

        $responseArray = json_decode((string) $responseJson, true, 512, JSON_THROW_ON_ERROR);

        $I->assertNotEmpty($responseArray);

        $I->assertArrayHasKey('meta', $responseArray);

        $I->assertNotEmpty($responseArray['meta']);

        $I->assertArrayHasKey('message', $responseArray['meta']);

        $I->assertNotEmpty($responseArray['meta']['message']);

        $I->assertEquals(
            $expectedMessage,
            $responseArray['meta']['message']
        );

        $I->assertArrayHasKey('sourceModule', $responseArray['meta']);

        $I->assertEquals(
            'Accounts',
            $responseArray['meta']['sourceModule']
        );

        $I->assertArrayHasKey('sourceModuleLabel', $responseArray['meta']);

        $I->assertEquals(
            $accountLabel,
            $responseArray['meta']['sourceModuleLabel']
        );

        $I->assertArrayHasKey('sourceId', $responseArray['meta']);

        $I->assertEquals(
            $accountId,
            $responseArray['meta']['sourceId']
        );

        $I->assertArrayHasKey('relatedModule', $responseArray['meta']);

        $I->assertEquals(
            'Contacts',
            $responseArray['meta']['relatedModule']
        );

        $I->assertArrayHasKey('relatedModuleLabel', $responseArray['meta']);

        $I->assertEquals(
            $contactLabel,
            $responseArray['meta']['relatedModuleLabel']
        );

        $I->assertArrayHasKey('relatedId', $responseArray['meta']);

        $I->assertEquals(
            $contactId,
            $responseArray['meta']['relatedId']
        );

        $I->assertArrayHasKey('relationshipLink', $responseArray['meta']);

        $I->assertEquals(
            $linkName,
            $responseArray['meta']['relationshipLink']
        );

        $I->deleteRelationship(
            [
                'tableName' => 'accounts_contacts',
                'sourceIdName' => 'account_id',
                'relatedIdName' => 'contact_id',
            ],
            [
                'sourceId' => $accountId,
                'relatedId' => $contactId,
            ]
        );

        $I->deleteBean('accounts', $accountId);

        $I->deleteBean('contacts', $contactId);
    }
}
