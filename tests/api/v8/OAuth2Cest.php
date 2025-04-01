<?php

/**
 * Class JwtCest
 * Tests Jwt Token Authentication
 * @see https://tools.ietf.org/html/rfc7519
 */
#[\AllowDynamicProperties]
class OAuth2Cest
{
    /**
     * As a Rest API Client, I want to ensure that I get the correct response when I send incorrect details
     * @param ApiTester $I
     * @param \Helper\PhpBrowserDriverHelper $browserDriverHelper
     *
     * HTTP Verb: POST
     * URL: /api/oauth/access_token
     * @throws \Codeception\Exception\ModuleException
     */
    public function TestScenarioInvalidLogin(ApiTester $I, \Helper\PhpBrowserDriverHelper $browserDriverHelper)
    {
        $I->sendPOST(
            $browserDriverHelper->getInstanceURL().'/api/oauth/access_token',
            array(
                'username' => '',
                'password' => ''
            )
        );
        $I->seeResponseCodeIs(400);
    }

    /**
     * I want to ensure that I get the correct response when I send incorrect client details
     * @param ApiTester $I
     * @param \Helper\PhpBrowserDriverHelper $browserDriverHelper
     *
     * HTTP Verb: POST
     * URL: /api/oauth/access_token
     * @throws \Codeception\Exception\ModuleException
     */
    public function TestScenarioInvalidClient(ApiTester $I, \Helper\PhpBrowserDriverHelper $browserDriverHelper)
    {
        $I->sendPOST(
            $I->getInstanceURL().'/api/oauth/access_token',
            array(
                'username' => $I->getAdminUser(),
                'password' => $I->getAdminPassword(),
                'grant_type' => 'password',
                'scope' => '',
                'client_id' => '',
                'client_secret' => ''
            )
        );
        $I->seeResponseCodeIs(401);
    }

    /**
     * I want to make sure only the allowed grant types can be requested for any client
     * @param ApiTester $I
     *
     * HTTP Verb: POST
     * URL: /api/oauth/access_token
     * @throws \Codeception\Exception\ModuleException
     */
    public function TestScenarioGrantTypeNotAllowed(ApiTester $I)
    {
        $I->sendPOST(
            $I->getInstanceURL().'/api/oauth/access_token',
            array(
                'grant_type' => 'password',
                'client_id' => $I->getPasswordGrantClientId(),
                'client_secret' => $I->getPasswordGrantClientSecret(),
            )
        );

        $I->canSeeResponseIsJson();
        $I->seeResponseCodeIs(400);
    }

    /**
     * As a Rest API Client, I want to login so that I can get a JWT token
     * @param ApiTester $I
     *
     * HTTP Verb: POST
     * URL: /api/oauth/access_token
     * @throws \Codeception\Exception\ModuleException
     */
    public function TestScenarioLoginWithPasswordGrant(ApiTester $I)
    {
        $I->loginAsAdminWithPassword();
    }

    /**
     * I want to be able to login with Client Credentials grant type
     * @param ApiTester $I
     *
     * HTTP Verb: POST
     * URL: /api/oauth/access_token
     * @throws \Codeception\Exception\ModuleException
     */
    public function TestScenarioLoginWithClientCredentialsGrant(ApiTester $I)
    {
        $I->loginAsAdminWithClientCredentials();

        // Now log back in with password grant
        $I->loginAsAdminWithPassword();
    }
}
