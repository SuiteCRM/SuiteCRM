<?php

use SuiteCRM\LangText;

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
/**
 * Entry Point for saving Google API tokens during account authorization.
 *
 * @license https://raw.githubusercontent.com/salesagility/SuiteCRM/master/LICENSE.txt
 * GNU Affero General Public License version 3
 * @author Benjamin Long <ben@offsite.guru>
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * class GoogleApiKeySaverEntryPoint
 */
#[\AllowDynamicProperties]
class GoogleApiKeySaverEntryPoint
{

    /**
     *
     * @var User
     */
    protected $currentUser;

    /**
     *
     * @var array
     */
    protected $sugarConfig;

    /**
     *
     * @var Google\Client
     */
    protected $client;

    /**
     *
     * @var array
     */
    protected $request;

    /**
     *
     * @param User $current_user
     * @param array $sugar_config
     * @param Google\Client $client
     * @param array $request
     */
    public function __construct(User $current_user, $sugar_config, Google\Client $client, $request)
    {
        $this->currentUser = $current_user;
        $this->sugarConfig = $sugar_config;
        $this->client = $client;
        $this->request = $request;

        $this->handleEntryPoint();
    }

    /**
     *
     * @throws Exception 1 - google_auth_json requested variable is missing, 2 - Invalid json for auth config
     */
    protected function handleEntryPoint()
    {
        $this->client->setApplicationName('SuiteCRM');
        $this->client->setScopes(Google\Service\Calendar::CALENDAR);
        if (!isset($this->sugarConfig['google_auth_json'])) {
            throw new Exception('google_auth_json requested variable is missing', 1);
        }
        $json = base64_decode($this->sugarConfig['google_auth_json']);
        $config = json_decode($json, true);
        if (!$config) {
            throw new Exception('Invalid json for auth config', 2);
        }
        $this->validateConfig($config);
        $this->client->setAuthConfig($config);
        $this->client->setAccessType('offline');
        $this->client->setApprovalPrompt('force');

        $this->handleRequest();
    }

    /**
     *
     * @param array $config
     * @throws Exception 2 - web is not set in the config json, 3 - client_id is not set in config json, 4 - client_secret is not set in config json
     */
    protected function validateConfig($config)
    {
        if (!isset($config['web'])) {
            throw new Exception('web is not set in the config json', 2);
        }
        if (!isset($config['web']['client_id'])) {
            throw new Exception('client_id is not set in config json', 3);
        }
        if (!isset($config['web']['client_secret'])) {
            throw new Exception('client_secret is not set in config json', 4);
        }
    }

    /**
     * handle requested action handler method
     */
    protected function handleRequest()
    {
        if (isset($this->request['getnew'])) {
            $this->handleRequestGetnew();
        } elseif (isset($this->request['code'])) {
            $this->handleRequestCode();
        } elseif (isset($this->request['setinvalid'])) {
            $this->handleRequestSetinvalid();
        } elseif (isset($this->request['error'])) {
            $this->handleRequestError();
        } else {
            $this->handleRequestUnknown();
        }
    }

    /**
     * create and redirect to auth URL
     */
    protected function handleRequestGetnew()
    {
        $authUrl = $this->client->createAuthUrl();
        $this->redirect($authUrl);
    }

    /**
     * set google api token
     *
     * @throws Exception 1 - Unable to get User bean. 2 - Unable to retrive user by ID
     */
    protected function handleRequestCode()
    {
        $user = BeanFactory::getBean('Users');
        if (!$user) {
            throw new Exception('Unable to get User bean.', 1);
        }
        $ret = $user->retrieve($this->currentUser->id);
        if (!$ret) {
            throw new Exception('Unable to retrive user by ID: ' . $this->currentUser->id, 2);
        }
        $accessToken = $this->client->fetchAccessTokenWithAuthCode($this->request['code']);
        if (array_key_exists('error', $accessToken)) {
            throw new Exception('Unable to fetch access token: ' . $accessToken['error'] . '|' . $accessToken['error_description'], 10);
        }
        $user->setPreference('GoogleApiToken', base64_encode(json_encode($accessToken)), false, 'GoogleSync');
        $accessRefreshToken = $accessToken['refresh_token'];
        if (isset($accessRefreshToken)) {
            $user->setPreference('GoogleApiRefreshToken', base64_encode($accessRefreshToken), false, 'GoogleSync');
        }
        $user->savePreferencesToDB();
        $url = $this->sugarConfig['site_url'] . "/index.php?module=Users&action=EditView&record=" . $this->currentUser->id;
        $this->redirect($url);
    }

    /**
     * set google api token to invalid
     *
     * @throws Exception 1 - Unable to get User bean. 2 - Unable to retrive user by ID
     */
    protected function handleRequestSetinvalid()
    {
        $user = BeanFactory::getBean('Users');
        if (!$user) {
            throw new Exception('Unable to get User bean.', 1);
        }
        $ret = $user->retrieve($this->currentUser->id);
        if (!$ret) {
            throw new Exception('Unable to retrive user by ID: ' . $this->currentUser->id, 2);
        }
        $user->setPreference('GoogleApiToken', '', false, 'GoogleSync');
        $user->savePreferencesToDB();
        $url = $this->sugarConfig['site_url'] . "/index.php?module=Users&action=EditView&record=" . $this->currentUser->id;
        $this->redirect($url);
    }

    /**
     * shows an error - pick error message from language file instead
     * using simple requested text as it is an XSS vulnerability issue
     */
    protected function handleRequestError()
    {
        $url = $this->sugarConfig['site_url'] . "/index.php?module=Users&action=EditView&record=" . $this->currentUser->id;
        $tpl = new Sugar_Smarty();
        $txtKey = $this->request['error'];
        $tpl->assign('error', LangText::get($txtKey));
        $tpl->assign('url', $url);
        $exitstring = $tpl->fetch(__DIR__ . '/googleApiKeySaverEntryPointError.tpl');
        $this->protectedDie($exitstring);
    }

    /**
     * redirect to user edit view if unknown function given.
     */
    protected function handleRequestUnknown()
    {
        LoggerManager::getLogger()->error('Unkown entry point function given.');
        // If we don't get a known return, we just silently return to the user profile.
        $url = $this->sugarConfig['site_url'] . "/index.php?module=Users&action=EditView&record=" . $this->currentUser->id;
        $this->redirect($url);
    }

    /**
     * protected function for SugarApplication::redirect() so test mock can override it
     * @param string $url
     */
    protected function redirect($url)
    {
        SugarApplication::redirect($url);
    }

    /**
     * protected function for die() so test mock can override it
     * @param string $exitstring
     */
    protected function protectedDie($exitstring)
    {
        die($exitstring);
    }
}
