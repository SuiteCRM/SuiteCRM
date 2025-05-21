<?php

/**
 * Class LoginCest
 *
 * Test login page
 */
#[\AllowDynamicProperties]
class LoginCest
{
    /**
     * @param AcceptanceTester $I
     */
    public function _before(AcceptanceTester $I)
    {
    }

    /**
     * @param AcceptanceTester $I
     */
    public function _after(AcceptanceTester $I)
    {
    }

    // tests
    public function testScenarioLoginAsAdministrator(AcceptanceTester $I)
    {
        $I->wantTo('Login as an administrator');
        // Login as Administrator
        $I->loginAsAdmin();
    }
}
