<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

include_once __DIR__ . '/GoogleApiKeySaverEntryPointMock.php';

/**
 * GoogleApiKeySaverEntryPointTest
 *
 * @author gyula
 */


use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class GoogleApiKeySaverEntryPointTest extends SuitePHPUnitFrameworkTestCase
{
    public function testHandleRequestError(): void
    {
        $cfg = [];
        $request = [];
        $user = BeanFactory::getBean('Users');
        $cfg['site_url'] = 'http://foo/bar.org';
        $cfg['google_auth_json'] = base64_encode('{"web":{"client_id":"UNIT_TEST_client_id","project_id":"UNIT_TEST_project_id","auth_uri":"https://accounts.google.com/o/oauth2/auth","token_uri":"https://www.googleapis.com/oauth2/v3/token","auth_provider_x509_cert_url":"https://www.googleapis.com/oauth2/v1/certs","client_secret":"UNIT_TEST_client_secret","redirect_uris":["http://www.example.com/index.php?entryPoint=saveGoogleApiKey"]}}');
        $client = new Google\Client();
        $request['error'] = 'ERR_NOT_ADMIN';
        $epMock = new GoogleApiKeySaverEntryPointMock($user, $cfg, $client, $request);
        $dieOk = $epMock->getDieOk();
        $exitString = $epMock->getExitString();
        self::assertTrue($dieOk);
        self::assertEquals('<html>
    <head>
        <title>SuiteCRM Google Sync - ERROR</title>
    </head>
    <body>
        <h1>There was an error: Unauthorized access to administration.</h1>
        <br>
        <p>
            <a href="http://foo/bar.org/index.php?module=Users&action=EditView&record=">Click here</a> to continue.
    </body>
</html>', $exitString);
    }

    public function testHandleRequestGetnew(): void
    {
        $cfg = [];
        $request = [];
        $user = BeanFactory::getBean('Users');
        $cfg['site_url'] = 'http://foo/bar.org';
        $cfg['google_auth_json'] = base64_encode('{"web":{"client_id":"UNIT_TEST_client_id","project_id":"UNIT_TEST_project_id","auth_uri":"https://accounts.google.com/o/oauth2/auth","token_uri":"https://www.googleapis.com/oauth2/v3/token","auth_provider_x509_cert_url":"https://www.googleapis.com/oauth2/v1/certs","client_secret":"UNIT_TEST_client_secret","redirect_uris":["http://www.example.com/index.php?entryPoint=saveGoogleApiKey"]}}');
        $client = new Google\Client();
        $request['getnew'] = 'ERR_NOT_ADMIN';
        $epMock = new GoogleApiKeySaverEntryPointMock($user, $cfg, $client, $request);
        $expected = "https://accounts.google.com/o/oauth2/v2/auth?response_type=code&access_type=offline&client_id=UNIT_TEST_client_id&redirect_uri=http%3A%2F%2Fwww.example.com%2Findex.php%3FentryPoint%3DsaveGoogleApiKey&state&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcalendar&approval_prompt=force";
        $redirectString = $epMock->getRedirectUrl();
        self::assertEquals($expected, $redirectString);
    }

    public function testHandleRequestCode(): void
    {
        $cfg = [];
        $request = [];
        $e = null;
        $user = BeanFactory::getBean('Users');
        $user->last_name = 'UNIT_TESTS';
        $user->user_name = 'UNIT_TESTS';
        $user->save();

        $cfg['site_url'] = 'http://foo/bar.org';
        $cfg['google_auth_json'] = base64_encode('{"web":{"client_id":"UNIT_TEST_client_id","project_id":"UNIT_TEST_project_id","auth_uri":"https://accounts.google.com/o/oauth2/auth","token_uri":"https://www.googleapis.com/oauth2/v3/token","auth_provider_x509_cert_url":"https://www.googleapis.com/oauth2/v1/certs","client_secret":"UNIT_TEST_client_secret","redirect_uris":["http://www.example.com/index.php?entryPoint=saveGoogleApiKey"]}}');
        $client = new Google\Client();
        $request['code'] = '1234567890';
        try {
            $epMock = new GoogleApiKeySaverEntryPointMock($user, $cfg, $client, $request);
            self::assertTrue(false, "This should have thrown an exception");
        } catch (Exception $e) {
            self::assertEquals(10, $e->getCode());
        }
        self::assertNotNull($e);
    }

    public function testHandleRequestSetInvalid(): void
    {
        $cfg = [];
        $request = [];
        $user = BeanFactory::getBean('Users');
        $user->last_name = 'UNIT_TESTS';
        $user->user_name = 'UNIT_TESTS';
        $user->save();

        $cfg['site_url'] = 'http://foo/bar.org';
        $cfg['google_auth_json'] = base64_encode('{"web":{"client_id":"UNIT_TEST_client_id","project_id":"UNIT_TEST_project_id","auth_uri":"https://accounts.google.com/o/oauth2/auth","token_uri":"https://www.googleapis.com/oauth2/v3/token","auth_provider_x509_cert_url":"https://www.googleapis.com/oauth2/v1/certs","client_secret":"UNIT_TEST_client_secret","redirect_uris":["http://www.example.com/index.php?entryPoint=saveGoogleApiKey"]}}');
        $client = new Google\Client();
        $request['setinvalid'] = '';
        $epMock = new GoogleApiKeySaverEntryPointMock($user, $cfg, $client, $request);
        $expected = "http://foo/bar.org/index.php?module=Users&action=EditView&record=" . $user->id;
        $redirectString = $epMock->getRedirectUrl();
        self::assertEquals($expected, $redirectString);
    }


    public function testHandleRequestUnknown(): void
    {
        $cfg = [];
        $request = [];
        $user = BeanFactory::getBean('Users');
        $user->last_name = 'UNIT_TESTS';
        $user->user_name = 'UNIT_TESTS';
        $user->save();
        $cfg['site_url'] = 'http://foo/bar.org';
        $cfg['google_auth_json'] = base64_encode('{"web":{"client_id":"UNIT_TEST_client_id","project_id":"UNIT_TEST_project_id","auth_uri":"https://accounts.google.com/o/oauth2/auth","token_uri":"https://www.googleapis.com/oauth2/v3/token","auth_provider_x509_cert_url":"https://www.googleapis.com/oauth2/v1/certs","client_secret":"UNIT_TEST_client_secret","redirect_uris":["http://www.example.com/index.php?entryPoint=saveGoogleApiKey"]}}');
        $client = new Google\Client();
        $request['INVALID'] = 'INVALID';
        $epMock = new GoogleApiKeySaverEntryPointMock($user, $cfg, $client, $request);
        $expected = "http://foo/bar.org/index.php?module=Users&action=EditView&record=" . $user->id;
        $redirectString = $epMock->getRedirectUrl();
        self::assertEquals($expected, $redirectString);
    }
}
