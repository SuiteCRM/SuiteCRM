<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/entryPoint.php';

global $sugar_config, $log;

require_once __DIR__ . '/../services/OAuthAuthorizationService.php';
$oAuth = new OAuthAuthorizationService();

$providerId = $_GET['provider'] ?? '';
if (empty($providerId) && !empty($_SESSION['provider'])) {
    $providerId = $_SESSION['provider'];
}

if (!$oAuth->hasProvider($providerId)) {
    $oAuth->cleanSession();
    $log->fatal("The specified OAuth2 provider '$providerId' is not supported or not properly configured");
    exit('The specified OAuth2 providers is not supported or not properly configured');
}

if (empty($_GET['state']) || ($_GET['state'] !== $_SESSION['oauth2state'])) {
    $oAuth->cleanSession();
    exit('Invalid state');
}

$code = $_GET['code'] ?? '';
$request = json_encode($_GET, JSON_THROW_ON_ERROR) ?? '';

$oAuth->log('debug', 'setExternalOAuthToken::request', $request);

$token = $oAuth->getAccessToken($providerId, $code);

if ($token === null) {
    exit('Not able to get access token. Check logs for more details');
}

$mappedToken = $oAuth->mapToken($providerId, $token);

$ss = new Sugar_Smarty();
$ss->assign('token', $mappedToken);
$tpl = __DIR__ . '/../tpl/setToken.tpl';

echo $ss->fetch($tpl);

exit;
