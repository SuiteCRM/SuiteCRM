<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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



require_once 'include/SugarOAuthServer.php';

#[\AllowDynamicProperties]
class OauthTokensViewAuthorize extends SugarView
{
    public function display()
    {
        if (!SugarOAuthServer::enabled()) {
            sugar_die($GLOBALS['mod_strings']['LBL_OAUTH_DISABLED']);
        }
        global $current_user;
        if (!isset($_REQUEST['token']) && isset($_REQUEST['oauth_token'])) {
            $_REQUEST['token'] = $_REQUEST['oauth_token'];
        }
        $sugar_smarty = new Sugar_Smarty();
        $sugar_smarty->assign('APP', $GLOBALS['app_strings']);
        $sugar_smarty->assign('MOD', $GLOBALS['mod_strings']);
        $sugar_smarty->assign('token', $_REQUEST['token']);
        $sugar_smarty->assign('sid', session_id());
        $token = OAuthToken::load($_REQUEST['token']);
        if (empty($token) || empty($token->consumer) || $token->tstate != OAuthToken::REQUEST || empty($token->consumer_obj)) {
            sugar_die('Invalid token');
        }

        if (empty($_REQUEST['confirm'])) {
            $sugar_smarty->assign('consumer', sprintf($GLOBALS['mod_strings']['LBL_OAUTH_CONSUMERREQ'], $token->consumer_obj->name));
            // SM: roles disabled for now
//            $roles = array('' => '');
//            $allroles = ACLRole::getAllRoles();
//            foreach($allroles as $role) {
//                $roles[$role->id] = $role->name;
//            }
//            $sugar_smarty->assign('roles', $roles);
            $hash = md5(mt_rand());
            $_SESSION['oauth_hash'] = $hash;
            $sugar_smarty->assign('hash', $hash);
            echo $sugar_smarty->fetch('modules/OAuthTokens/tpl/authorize.tpl');
        } else {
            if ($_REQUEST['sid'] != session_id() || $_SESSION['oauth_hash'] != $_REQUEST['hash']) {
                sugar_die('Invalid request');
            }
            $verify = $token->authorize(array("user" => $current_user->id));
            if (!empty($token->callback_url)) {
                $redirect_url=$token->callback_url;
                if (strstr((string) $redirect_url, "?") !== false) {
                    $redirect_url .= '&';
                } else {
                    $redirect_url .= '?';
                }
                $redirect_url .= "oauth_verifier=".$verify.'&oauth_token='.$_REQUEST['token'];
                SugarApplication::redirect($redirect_url);
            }
            $sugar_smarty->assign('VERIFY', $verify);
            $sugar_smarty->assign('token', '');
            echo $sugar_smarty->fetch('modules/OAuthTokens/tpl/authorized.tpl');
        }
    }
}
