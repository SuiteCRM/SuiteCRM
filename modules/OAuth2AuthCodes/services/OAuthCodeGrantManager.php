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


use Api\V8\OAuth2\Entity\UserEntity;
use League\OAuth2\Server\AuthorizationServer;
use League\OAuth2\Server\Exception\OAuthServerException;
use League\OAuth2\Server\RequestTypes\AuthorizationRequest;
use Psr\Http\Message\ServerRequestInterface;

class OAuthCodeGrantManager
{
    /**
     * Save the authorization request details to the session
     *
     * @param ServerRequestInterface $httpRequest The request object
     * @param AuthorizationRequest $authRequest The authorization request object
     * @param string $requestHash The hash of the request
     */
    public function saveRequestToSession(
        ServerRequestInterface $httpRequest,
        AuthorizationRequest $authRequest,
        string $requestHash,
        string $process_id
    ): void {
        global $log;

        $_SESSION['oauth2_authcode_response_type'] = $this->getQueryParam('response_type', $httpRequest);
        $_SESSION['oauth2_authcode_client_id'] = $this->getQueryParam('client_id', $httpRequest);
        $_SESSION['oauth2_authcode_redirect_uri'] = $this->getQueryParam('redirect_uri', $httpRequest);
        $_SESSION['oauth2_authcode_state'] = $this->getQueryParam('state', $httpRequest);
        $_SESSION['oauth2_authcode_scope'] = $this->getQueryParam('scope', $httpRequest);
        $_SESSION['oauth2_authcode_code_challenge'] = $this->getQueryParam('code_challenge', $httpRequest);
        $_SESSION['oauth2_authcode_code_challenge_method'] = $this->getQueryParam('code_challenge_method', $httpRequest);

        $_SESSION['oauth2_authcode_logout'] = strpos($_SERVER['HTTP_REFERER'], 'action=Login') !== false;
        $_SESSION['oauth2_authcode_hash'] = $requestHash;
        $_SESSION['oauth2_authcode_process_id'] = $process_id;
    }


    /**
     * Load the authorization request from the session
     *
     * @param AuthorizationServer $server The authorization server
     * @param ServerRequestInterface $parentRequest The original request object
     * @return AuthorizationRequest|null The authorization request or null if not found
     * @throws OAuthServerException
     */
    public function loadAuthorizationRequest(AuthorizationServer $server, ServerRequestInterface $parentRequest): ?AuthorizationRequest
    {
        global $log;
        global $current_user;

        $responseType = $_SESSION['oauth2_authcode_response_type'] ?? null;
        $clientId = $_SESSION['oauth2_authcode_client_id'] ?? null;
        $redirectUri = $_SESSION['oauth2_authcode_redirect_uri'] ?? null;
        $state = $_SESSION['oauth2_authcode_state'] ?? null;
        $scope = $_SESSION['oauth2_authcode_scope'] ?? null;
        $codeChallenge = $_SESSION['oauth2_authcode_code_challenge'] ?? null;
        $codeChallengeMethod = $_SESSION['oauth2_authcode_code_challenge_method'] ?? null;

        if (empty($responseType) || empty($clientId)) {
            $log->error('No OAuth2 authorization in progress. Missing session data.');
            return null;
        }

        $params = [
            'response_type' => $responseType,
            'client_id' => $clientId,
            'redirect_uri' => $redirectUri,
            'state' => $state,
            'scope' => $scope,
            'code_challenge' => $codeChallenge,
            'code_challenge_method' => $codeChallengeMethod
        ];

        $request = $parentRequest->withQueryParams(array_filter($params));


        $authRequest = $server->validateAuthorizationRequest($request);

        if ($authRequest === null) {
            $log->error('No OAuth2 authorization in progress. Cannot load authorization request from session.');
            return null;
        }

        $authRequest->setUser(new UserEntity($current_user->id)); // an instance of UserEntityInterface

        return $authRequest;
    }

    /**
     * Clean up the session variables used for the authorization request
     */
    public function cleanupSession(): void
    {
        unset(
            $_SESSION['oauth2_authcode_response_type'],
            $_SESSION['oauth2_authcode_client_id'],
            $_SESSION['oauth2_authcode_redirect_uri'],
            $_SESSION['oauth2_authcode_state'],
            $_SESSION['oauth2_authcode_scope'],
            $_SESSION['oauth2_authcode_code_challenge'],
            $_SESSION['oauth2_authcode_code_challenge_method'],
            $_SESSION['oauth2_authcode_logout'],
            $_SESSION['oauth2_authcode_hash'],
            $_SESSION['oauth2_authcode_process_id']
        );
    }

    /**
     * Check if an authorization is in progress by validating session and request parameters
     *
     * @param ServerRequestInterface $request The request object
     * @throws InvalidArgumentException If the authorization is not valid or not in progress
     */
    public function validateConfirmationRequest(ServerRequestInterface $request): void
    {
        global $log;

        if (empty($_SESSION['oauth2_authcode_hash'])) {
            $log->error('No OAuth2 authorization in progress. Missing oauth2_authcode_hash.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if (empty($request->getParam('oauth2_authcode_hash'))) {
            $log->error('Missing oauth2_authcode_hash parameter, cannot validate authenticity of the request.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if ($_SESSION['oauth2_authcode_hash'] !== $request->getParam('oauth2_authcode_hash')) {
            $log->error('Invalid oauth2_authcode_hash parameter, cannot validate authenticity of the request.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if (empty($_SESSION['oauth2_authcode_process_id'])) {
            $log->error('No OAuth2 authorization in progress. Missing oauth2_authcode_process_id.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if (empty($request->getParam('oauth2_authcode_process_id'))) {
            $log->error('Missing oauth2_authcode_process_id parameter, cannot validate authenticity of the request.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if ($_SESSION['oauth2_authcode_process_id'] !== $request->getParam('oauth2_authcode_process_id')) {
            $log->error('Invalid oauth2_authcode_hash parameter, cannot validate authenticity of the request.');
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        $confirmed = $request->getParam('confirmed');
        if ($confirmed !== 'once' && $confirmed !== 'always' && $confirmed !== 'abort') {
            $log->error('Invalid value for confirmed parameter: ' . $confirmed);
            throw new InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }
    }

    /**
     * Helper to get a query parameter from the request
     *
     * @param string $parameter The parameter name
     * @param ServerRequestInterface $request The request object
     * @param string|null $default The default value if the parameter is not set
     * @return string|null The parameter value or the default value
     */
    protected function getQueryParam(string $parameter, ServerRequestInterface $request, string $default = null): ?string
    {
        return $request->getQueryParams()[$parameter] ?? $default;
    }

}