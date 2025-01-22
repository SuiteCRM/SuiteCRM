<?php
namespace Test\Api\V8;

use ApiTester;
use Codeception\Scenario;

#[\AllowDynamicProperties]
class DeleteRelationShipCest
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
     * @param Scenario $scenario
     */
    public function shouldWork(ApiTester $I, Scenario $scenario)
    {
        $scenario->incomplete(
            'Delete successfully relationship test was skipped temporarily, because Slim has to be added to codeception'
        );
    }

    /**
     * @param ApiTester $I
     * @param Scenario $scenario
     */
    public function shouldNotWork(ApiTester $I, Scenario $scenario)
    {
        $scenario->incomplete(
            'Delete unsuccessfully relationship test was skipped temporarily, because Slim has to be added to codeception'
        );
    }
}
