<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class OAuthTokenTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }

    public function test__construct(): void
    {
        // Execute the constructor and check for the Object type and  attributes
        $oauthToken = BeanFactory::newBean('OAuthTokens');

        self::assertInstanceOf('OAuthToken', $oauthToken);
        self::assertInstanceOf('SugarBean', $oauthToken);

        self::assertEquals('OAuthTokens', $oauthToken->module_dir);
        self::assertEquals('OAuthToken', $oauthToken->object_name);
        self::assertEquals('oauth_tokens', $oauthToken->table_name);

        self::assertEquals(true, $oauthToken->disable_row_level_security);
    }

    public function testsetState(): void
    {
        $oauthToken = BeanFactory::newBean('OAuthTokens');
        $oauthToken->setState($oauthToken::REQUEST);

        self::assertEquals($oauthToken::REQUEST, $oauthToken->tstate);
    }

    public function testsetConsumer(): void
    {
        $oauthToken = BeanFactory::newBean('OAuthTokens');

        $oauthKey = BeanFactory::newBean('OAuthKeys');
        $oauthKey->id = '1';

        $oauthToken->setConsumer($oauthKey);

        self::assertEquals($oauthKey->id, $oauthToken->consumer);
        self::assertEquals($oauthKey, $oauthToken->consumer_obj);
    }

    public function testsetCallbackURL(): void
    {
        $oauthToken = BeanFactory::newBean('OAuthTokens');

        $url = 'test url';
        $oauthToken->setCallbackURL($url);

        self::assertEquals($url, $oauthToken->callback_url);
    }

    public function testgenerate(): void
    {
        $result = OAuthToken::generate();

        self::assertInstanceOf('OAuthToken', $result);
        self::assertGreaterThan(0, strlen((string) $result->token));
        self::assertGreaterThan(0, strlen((string) $result->secret));
    }

    public function testSaveAndOthers(): void
    {
        $oauthToken = OAuthToken::generate();

        $oauthToken->save();

        //test for record ID to verify that record is saved
        self::assertTrue(isset($oauthToken->id));
        self::assertEquals(12, strlen((string) $oauthToken->id));

        //test load method
        $this->load($oauthToken->id);

        //test invalidate method
        $token = OAuthToken::load($oauthToken->id);
        $this->invalidate($token);

        //test authorize method
        $this->authorize($token);

        //test mark_deleted method
        $this->mark_deleted($oauthToken->id);
    }

    public function load($id): void
    {
        $token = OAuthToken::load($id);

        self::assertInstanceOf('OAuthToken', $token);
        self::assertTrue(isset($token->id));
    }

    public function invalidate($token): void
    {
        $token->invalidate();

        self::assertEquals($token::INVALID, $token->tstate);
        self::assertEquals(false, $token->verify);
    }

    public function authorize($token): void
    {
        $result = $token->authorize('test');
        self::assertEquals(false, $result);

        $token->tstate = $token::REQUEST;
        $result = $token->authorize('test');

        self::assertEquals('test', $token->authdata);
        self::assertGreaterThan(0, strlen((string) $result));
        self::assertEquals($result, $token->verify);
    }

    public function mark_deleted($id): void
    {
        $oauthToken = BeanFactory::newBean('OAuthTokens');

        //execute the method
        $oauthToken->mark_deleted($id);

        //verify that record can not be loaded anymore
        $token = OAuthToken::load($id);
        self::assertEquals(null, $token);
    }

    public function testcreateAuthorized(): void
    {
        $oauthKey = BeanFactory::newBean('OAuthKeys');
        $oauthKey->id = '1';

        $user = BeanFactory::newBean('Users');
        $user->retrieve('1');

        $oauthToken = OAuthToken::createAuthorized($oauthKey, $user);

        self::assertEquals($oauthKey->id, $oauthToken->consumer);
        self::assertEquals($oauthKey, $oauthToken->consumer_obj);
        self::assertEquals($oauthToken::ACCESS, $oauthToken->tstate);
        self::assertEquals($user->id, $oauthToken->assigned_user_id);

        //execute copyAuthData method
        $oauthToken->authdata = 'test';
        $this->copyAuthData($oauthToken);

        //finally mark deleted for cleanup
        $oauthToken->mark_deleted($oauthToken->id);
    }

    public function copyAuthData($token): void
    {
        $oauthToken = BeanFactory::newBean('OAuthTokens');

        $oauthToken->copyAuthData($token);
        self::assertEquals($token->authdata, $oauthToken->authdata);
        self::assertEquals($token->assigned_user_id, $oauthToken->assigned_user_id);
    }

    public function testqueryString(): void
    {
        $oauthToken = BeanFactory::newBean('OAuthTokens');

        $result = $oauthToken->queryString();
        self::assertEquals('oauth_token=&oauth_token_secret=', $result);

        //test with attributes set
        $oauthToken->token = 'toekn';
        $oauthToken->secret = 'secret';
        $result = $oauthToken->queryString();
        self::assertEquals('oauth_token=toekn&oauth_token_secret=secret', $result);
    }

    public function testcleanup(): void
    {
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            OAuthToken::cleanup();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdeleteByConsumer(): void
    {
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            OAuthToken::deleteByConsumer('1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdeleteByUser(): void
    {
        // Execute the method and test that it works and doesn't throw an exception.
        try {
            OAuthToken::deleteByUser('1');
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdisplayDateFromTs(): void
    {
        //test with empty array
        $result = displayDateFromTs(array('' => ''), 'timestamp', '');
        self::assertEquals('', $result);

        //test with a valid array
        $result = displayDateFromTs(array('TIMESTAMP' => '1272508903'), 'timestamp', '');
        self::assertEquals('04/29/2010 02:41', $result);
    }
}
