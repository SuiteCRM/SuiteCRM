<?php
namespace Test\Api\V8;

use ApiTester;
use ArrayIterator;
use Codeception\Example;
use Codeception\Exception\ModuleException;
use Exception;

#[\AllowDynamicProperties]
class GetRecordsCest
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
        $responseJson = $I->grabResponse();
        $responseArray = json_decode((string) $responseJson, true, 512, JSON_THROW_ON_ERROR);
        $I->assertNotEmpty($responseArray);
        $I->assertArrayHasKey('email1', $responseArray['data']['attributes']);

        $I->assertEquals(
            'vegan.vegan.the@example.net',
            $responseArray['data']['attributes']['email1']
        );
    }

    /**
     * @return array
     */
    protected function shouldWorkDataProvider()
    {
        return [
            [
                'endPoint' => '/Api/V8/module/Accounts/11c34eba-76e0-8f0d-6f1d-5e567b77e52c',
            ],
        ];
    }
}
