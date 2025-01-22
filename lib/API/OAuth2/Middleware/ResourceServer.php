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

namespace SuiteCRM\API\OAuth2\Middleware;

use League\OAuth2\Server\Exception\OAuthServerException;
use League\OAuth2\Server\ResourceServer as OAuthResourceServer;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use SuiteCRM\API\v8\Exception\NotAllowedException;
use SuiteCRM\Enumerator\ExceptionCode;
use SuiteCRM\Utility\SuiteLogger as Logger;

#[\AllowDynamicProperties]
class ResourceServer
{
    /**
     * @var ResourceServer
     */
    private $server;

    private static $ROUTES_EXEMPT_FROM_AUTH = [
        'oauth/access_token',
        'v8/swagger.json',
    ];

    /**
     * @param OAuthResourceServer $server
     */
    public function __construct(OAuthResourceServer $server)
    {
        $this->server = $server;
    }

    /**
     * @param ServerRequestInterface $request
     * @param ResponseInterface      $response
     * @param callable               $next
     *
     * @return \Psr\Http\Message\ResponseInterface
     */
    public function __invoke(ServerRequestInterface $request, ResponseInterface $response, callable $next)
    {
        try {
            if (!in_array($request->getUri()->getPath(), self::$ROUTES_EXEMPT_FROM_AUTH)) {
                $request = $this->server->validateAuthenticatedRequest($request);

                $this->setCurrentUserGlobal($request);
            }
        } catch (OAuthServerException $exception) {
            $log = new Logger();
            $log->error(
                '[ResourceServer] '.
                ' Code: ' . $exception->getCode().
                ' Message: ' . $exception->getMessage().
                ' ErrorType: ' . $exception->getErrorType().
                ' Hint: ' . $exception->getHint()
            );
            return $exception->generateHttpResponse($response);
            // @codeCoverageIgnoreStart
        } catch (\Exception $exception) {
            $log = new Logger();
            $log->error(
                '[ResourceServer] '.
                $exception->getCode().' '.$exception->getMessage()
            );
            return (new OAuthServerException($exception->getMessage(), 0, 'unknown_error', 500))
                ->generateHttpResponse($response);
            // @codeCoverageIgnoreEnd
        }

        // Pass the request and response on to the next responder in the chain
        return $next($request, $response);
    }

    /**
     * Suite needs a current_user global for roles, security groups etc.
     *
     * @param ServerRequestInterface $request
     * @throws NotAllowedException
     */
    private function setCurrentUserGlobal(ServerRequestInterface $request)
    {
        global $current_user;

        $user = $this->getUserFromRequest($request);

        // validate user is still active
        if ($user->status === 'Inactive') {
            throw new NotAllowedException('[User Not Active]', ExceptionCode::API_USER_NOT_ACTIVE);
        }

        $current_user = $user;
    }

    /**
     * @param ServerRequestInterface $request
     * @return \User
     *
     * @throws NotAllowedException
     */
    private function getUserFromRequest(ServerRequestInterface $request)
    {
        $user = new \User();

        $user->retrieve($request->getAttribute('oauth_user_id'));

        if ($user->id) {
            return $user;
        }

        // We need a User to take ownership of actions, so if we are using a grant type that does not have
        // an associated User we fall back on the User defined in the OAuth2Clients
        $client = new \OAuth2Clients();
        $client->retrieve($request->getAttribute('oauth_client_id'));

        $user->retrieve($client->assigned_user_id);
        if ($user->id) {
            return $user;
        }

        throw new NotAllowedException('[User Not Active]', ExceptionCode::API_USER_NOT_ACTIVE);
    }
}
