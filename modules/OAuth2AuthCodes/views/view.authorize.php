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
use Api\V8\OAuth2\Entity\UserEntity;
use Slim\App;

require_once 'modules/OAuth2AuthCodes/services/OAuthCodeGrantManager.php';

/**
* Class Oauth2AuthCodesViewAuthorize
*/
class Oauth2AuthCodesViewAuthorize extends SugarView
{
    /**
    * @var array $options
    * Options for what UI elements to hide/show/
    */
    public $options = array(
        'show_header' => false,
        'show_title' => false,
        'show_subpanels' => false,
        'show_search' => false,
        'show_footer' => false,
        'show_javascript' => true,
        'view_print' => false,
    );

    /**
     * @throws SmartyException
     * @throws \Psr\Container\ContainerExceptionInterface
     * @throws \Psr\Container\NotFoundExceptionInterface
     */
    public function display()
    {
        global $log;
        $app = new App(ContainerLoader::configure());
         
        /** @var AuthorizationServer $server */
        $server   = $app->getContainer()->get(AuthorizationServer::class);
        $request  = $app->getContainer()->get('request');
        $response = $app->getContainer()->get('response');

        try {
            $authRequest = $server->validateAuthorizationRequest($request);
        } catch (OAuthServerException $exception) {
            $log->error('OAuth2 Authorization Request validation failed: '.$exception->getMessage());
            throw new \InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        if ($authRequest === null) {
            throw new \InvalidArgumentException($GLOBALS['mod_strings']['LBL_INVALID_REQUEST']);
        }

        global $current_user;
        $authRequest->setUser(new UserEntity($current_user->id));

        /** @var \OAuth2AuthCodes $authCode */
        $authCode = BeanFactory::newBean('OAuth2AuthCodes');

        if ($authCode->is_scope_authorized($authRequest)) {
            try {
                $authRequest->setAuthorizationApproved(true);
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

        $hash = md5(serialize($authRequest));
        $process_id = uniqid('', true);
        $manager = new OAuthCodeGrantManager();
        $manager->saveRequestToSession($request, $authRequest, $hash, $process_id);

        $sugar_smarty = new Sugar_Smarty();
        echo SugarThemeRegistry::current()->getJS();
        echo SugarThemeRegistry::current()->getCSS();
        echo '<link rel="stylesheet" type="text/css" media="all" href="' . getJSPath('modules/Users/login.css') . '">';

        $sugar_smarty->assign('cssStyles', get_custom_file_contents('modules/OAuth2AuthCodes/css/style.css'));

        $sugar_smarty->assign('oauth2_authcode_logout', strpos($_SERVER['HTTP_REFERER'], 'action=Login') !== false);
        $sugar_smarty->assign('oauth2_authcode_hash', $hash);
        $sugar_smarty->assign('oauth2_authcode_process_id', $process_id);
        $sugar_smarty->assign('scope', $authRequest->getScopes());
        $sugar_smarty->assign('grants', [
            ['icon' => 'oauth_module.svg', 'iconClass' => 'fill-dark', 'name' => 'LBL_OAUTH2_GRANT_MODULE_ACCESS', 'description' => 'LBL_OAUTH2_GRANT_MODULE_ACCESS_DESC'],
            ['icon' => 'oauth_user.svg', 'iconClass' => 'stroke-dark', 'name' => 'LBL_OAUTH2_GRANT_USER_DATA_ACCESS', 'description' => 'LBL_OAUTH2_GRANT_USER_DATA_ACCESS_DESC']
        ]);

        $sugar_smarty->assign('user', [
            'full_name' => $current_user->full_name ?? $current_user->name ?? $current_user->username ?? '',
        ]);

        $sugar_smarty->assign('client', array(
            'name' => $authRequest->getClient()->getName(),
            'redirectUri' => $authRequest->getClient()->getRedirectUri()
        ));

        $sugar_smarty->assign('LOGO_IMAGE', SugarThemeRegistry::current()->getImageURL('company_logo.png'));

        echo $sugar_smarty->fetch('modules/OAuth2AuthCodes/tpl/authorize.tpl');
    }


}
