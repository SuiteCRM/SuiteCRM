<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Api\Core\Loader\ContainerLoader;
use League\OAuth2\Server\AuthorizationServer;
use League\OAuth2\Server\Exception\OAuthServerException;
use Slim\App;

require_once get_custom_file_if_exists('modules/OAuth2AuthCodes/services/OAuthCodeGrantManager.php');

/**
 * Class OAuth2AuthCodesController
 */
class OAuth2AuthCodesController extends SugarController
{
    /**
     *
     */
    public function action_Authorize()
    {
        $this->view = 'authorize';
    }

    public function action_authorize_confirm()
    {
        $this->view = 'ajax';

        global $log;
        $app = new App(ContainerLoader::configure());

        /** @var AuthorizationServer $server */
        $server = $app->getContainer()->get(AuthorizationServer::class);
        $request = $app->getContainer()->get('request');
        $response = $app->getContainer()->get('response');

        $manager = new OAuthCodeGrantManager();

        $manager->validateConfirmationRequest($request);

        $authRequest = null;
        try {
            $authRequest = $manager->loadAuthorizationRequest($server, $request);
        } catch (Exception $e) {
        }

        if ($authRequest === null) {
            $log->error('No OAuth2 authorization in progress. Cannot load authorization request from session.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if ($request->getParam('oauth2_authcode_logout') === '1') {
            $log->info('Logging out user as part of OAuth2 authorization flow (oauth2_authcode_logout received).');
            session_destroy();
        }

        $manager->cleanupSession();

        try {
            $authRequest->setAuthorizationApproved($request->getParam('confirmed') === 'always' || $request->getParam('confirmed') === 'once');
            $response = $server->completeAuthorizationRequest($authRequest, $response);
        } catch (OAuthServerException $exception) {
            $response = $exception->generateHttpResponse($response);
            sugar_cleanup();
            // send response directly, because $app->respond($response) does not work due to some reason (?)
            print($response);
        }

        sugar_cleanup();
        $app->respond($response);
    }

}
