<?php

use Api\V8\Controller\BaseController;
use Codeception\Actor;
use Codeception\Exception\ModuleException;
use Codeception\Lib\Friend;
use Helper\PhpBrowserDriverHelper;


/**
 * Inherited Methods
 * @method void wantToTest($text)
 * @method void wantTo($text)
 * @method void execute($callable)
 * @method void expectTo($prediction)
 * @method void expect($prediction)
 * @method void amGoingTo($argumentation)
 * @method void am($role)
 * @method void lookForwardTo($achieveValue)
 * @method void comment($description)
 * @method Friend haveFriend($name, $actorClass = null)
 * @method getInstanceURL()
 * @method sendGET(string $string)
 * @method seeResponseCodeIs(int $int)
 * @method seeResponseIsJson()
 * @method seeResponseContainsJson(array $array)
 *
 * @SuppressWarnings(PHPMD)
 */
#[\AllowDynamicProperties]
class ApiTester extends Actor
{
    use _generated\ApiTesterActions;

    public const CONTENT_TYPE = 'Content-Type';
    public const CONTENT_TYPE_JSON_API = 'application/vnd.api+json';
    public const CONTENT_TYPE_JSON = 'application/json';

    /**
     * @var $string $accessToken
     */
    private static $accessToken;

    /**
     * @var $string $refreshToken
     */
    private static $refreshToken;

    /**
     * @var $string $tokenType - eg Bearer
     */
    private static $tokenType;

    /**
     * @var int $tokenExpiresIn - eg 3600
     */
    private static $tokenExpiresIn;

    /**
     * @throws ModuleException
     */
    public function loginAsAdmin()
    {
        $this->loginAsAdminWithPassword();
    }

    /**
     * @throws ModuleException
     */
    public function loginAsAdminWithPassword()
    {
        $this->loginWithPasswordGrant(
            $this->getPasswordGrantClientId(),
            $this->getPasswordGrantClientSecret(),
            $this->getAdminUser(),
            $this->getAdminPassword()
        );
    }

    /**
     * @throws ModuleException
     */
    public function loginAsAdminWithClientCredentials()
    {
        $this->loginWithClientCredentialsGrant(
            $this->getClientCredentialsGrantClientId(),
            $this->getClientCredentialsGrantClientSecret()
        );
    }

    /**
     * Logins into API with Password grant type
     * @param string $client
     * @param string $secret
     * @param string $username
     * @param string $password
     * @throws ModuleException
     */
    public function loginWithPasswordGrant($client, $secret, $username, $password)
    {
        $I = $this;

        if (!empty(self::$accessToken)) {
            return;
        }

        /**
         * @var PhpBrowserDriverHelper $browserDriverHelper
         */
        $I->sendPOST(
            $I->getInstanceURL() . '/api/oauth/access_token',
            [
                'username' => $username,
                'password' => $password,
                'grant_type' => 'password',
                'scope' => '',
                'client_id' => $client,
                'client_secret' => $secret
            ]
        );
        $I->canSeeResponseIsJson();
        $I->seeResponseCodeIs(200);

        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$tokenType = $response['token_type'];
        self::$tokenExpiresIn = (int)$response['expires_in'];
        self::$accessToken = $response['access_token'];
        self::$refreshToken = $response['refresh_token'];
    }

    /**
     * Logins into API with Client Credentials grant type
     * @param string $client
     * @param string $secret
     * @throws ModuleException
     */
    public function loginWithClientCredentialsGrant($client, $secret)
    {
        $I = $this;

        $I->sendPOST(
            $I->getInstanceURL() . '/api/oauth/access_token',
            [
                'grant_type' => 'client_credentials',
                'client_id' => $client,
                'client_secret' => $secret
            ]
        );
        $I->canSeeResponseIsJson();
        $I->seeResponseCodeIs(200);

        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        self::$tokenType = $response['token_type'];
        self::$tokenExpiresIn = (int)$response['expires_in'];
        self::$accessToken = $response['access_token'];
    }

    /**
     * Clicks the logout link in the users menu
     */
    public function logout()
    {
    }


    /**
     * Set the Jwt Token
     */
    public function sendJwtAuthorisation()
    {
        $I = $this;
        $I->setHeader('Authorization', self::$tokenType . ' ' . self::$accessToken);
    }

    /**
     * Set the Required Headers for authentication
     */
    public function sendJwtContentNegotiation()
    {
        $I = $this;
        $I->setHeader(self::CONTENT_TYPE, self::CONTENT_TYPE_JSON);
        $I->setHeader('Accept', 'application/json');
    }

    /**
     * Test to ensure that the response is Jwt
     */
    public function seeJwtContent()
    {
        $I = $this;
        $I->seeHttpHeader(self::CONTENT_TYPE, self::CONTENT_TYPE_JSON);
    }

    /**
     * Set the Required Headers for the Json API content
     */
    public function sendJsonApiContentNegotiation()
    {
        $I = $this;
        $I->setHeader(self::CONTENT_TYPE, self::CONTENT_TYPE_JSON_API);
        $I->setHeader('Accept', 'application/vnd.api+json');
    }

    /**
     * Test to ensure that the response is JSON Api
     */
    public function seeJsonApiContentNegotiation()
    {
        $I = $this;
        $I->seeHttpHeader(self::CONTENT_TYPE, self::CONTENT_TYPE_JSON_API);
    }

    /**
     * Test to ensure that the response is successful
     */
    public function seeJsonAPISuccess()
    {
        $I = $this;
        $I->canSeeResponseIsJson();

        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayNotHasKey('errors', $response);
    }

    /**
     * Test to ensure that the response isn't successful
     */
    public function seeJsonApiFailure()
    {
        $I = $this;
        $I->canSeeResponseIsJson();

        $response = json_decode((string) $I->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $I->assertArrayHasKey('errors', $response);
    }

    /**
     * This is only temporary till we fix this.
     * Please set your environment variables up for your test fw settings.
     *
     * @throws ModuleException
     */
    public function login()
    {
        $this->sendPOST($this->getInstanceURL() . '/Api/access_token', [
            'username' => $this->getAdminUser(),
            'password' => $this->getAdminPassword(),
            'grant_type' => 'password',
            'scope' => '',
            'client_id' => $this->getPasswordGrantClientId(),
            'client_secret' => $this->getPasswordGrantClientSecret(),
        ]);

        $response = json_decode((string) $this->grabResponse(), true, 512, JSON_THROW_ON_ERROR);
        $this->setHeader('Authorization', sprintf('%s %s', $response['token_type'], $response['access_token']));
        $this->setHeader('Content-Type', BaseController::MEDIA_TYPE);

        $this->seeResponseCodeIs(200);
        $this->canSeeResponseIsJson();
    }

    /**
     * This is also temporary till we fix this.
     *
     * @return string
     */
    public function createAccount()
    {
        $id = create_guid();
        $name = 'testAccount';
        $accountType = 'Customer';
        $db = DBManagerFactory::getInstance();

        $query = sprintf(
            "INSERT INTO accounts (id, name, account_type, date_entered) VALUES (%s, %s, %s, %s)",
            $db->quoted($id),
            $db->quoted($name),
            $db->quoted($accountType),
            $db->quoted(date('Y-m-d H:i:s'))
        );
        $db->query($query);

        return $id;
    }

    /**
     * This is also temporary till we fix this.
     *
     * @return string
     */
    public function createContact()
    {
        $id = create_guid();
        $db = DBManagerFactory::getInstance();

        $query = sprintf(
            "INSERT INTO contacts (id, date_entered) VALUES (%s,%s)",
            $db->quoted($id),
            $db->quoted(date('Y-m-d H:i:s'))
        );
        $db->query($query);

        return $id;
    }

    /**
     * This is also temporary till we fix this.
     *
     * @param string $tableName
     * @param string $id
     */
    public function deleteBean($tableName, $id)
    {
        $db = DBManagerFactory::getInstance();
        $query = sprintf("DELETE FROM %s WHERE id = %s", $tableName, $db->quoted($id));
        $db->query($query);
    }

    /**
     * This is also temporary till we fix this.
     *
     * @param array $relationshipMeta
     * @param array $ids
     */
    public function deleteRelationship($relationshipMeta, $ids)
    {
        $tableName = $relationshipMeta['tableName'];
        $sourceIdName = $relationshipMeta['sourceIdName'];
        $relatedIdName = $relationshipMeta['relatedIdName'];

        $sourceId = $ids['sourceId'];
        $relatedId = $ids['relatedId'];

        $db = DBManagerFactory::getInstance();

        $query = sprintf(
            'DELETE FROM %s WHERE %s = %s AND %s = %s',
            $tableName,
            $sourceIdName,
            $sourceId,
            $relatedIdName,
            $relatedId
        );

        $db->query($query);
    }
}
