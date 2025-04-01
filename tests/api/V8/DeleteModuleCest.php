<?php
namespace Test\Api\V8;

use ApiTester;

#[\AllowDynamicProperties]
class DeleteModuleCest
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
     * @throws \Codeception\Exception\ModuleException
     * @throws \Exception
     */
    public function shouldWork(ApiTester $I)
    {
        $id = $I->createAccount();
        $endpoint = $I->getInstanceURL() . '/Api/V8/module/Accounts/' . $id;
        $expectedResult = [
            'meta' => [
                'message' => sprintf('Record with id %s is deleted', $id)
            ],
            'data' => []
        ];

        $I->sendDELETE($endpoint);
        $I->seeResponseCodeIs(200);
        $I->seeResponseIsJson();
        $I->seeResponseEquals(json_encode($expectedResult, JSON_PRETTY_PRINT));

        // perm delete
        $I->deleteBean('accounts', $id);
    }

    /**
     * @param ApiTester $I
     *
     * @throws \Exception
     */
    public function shouldNotWork(ApiTester $I)
    {
        $id = $I->createAccount();
        $I->deleteBean('accounts', $id);

        $endpoint = $I->getInstanceURL() . '/Api/V8/module/Accounts/{id}';
        $endpoint = str_replace('{id}', $id, $endpoint);
        $detail = str_replace('{id}', $id, 'Accounts module with id {id} is not found');

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
}
