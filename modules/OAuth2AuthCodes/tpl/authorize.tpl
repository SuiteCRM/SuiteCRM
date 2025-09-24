{*
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

*}

<div class="p_login">
    <style>{$cssStyles}</style>
    <div class="p_login_top oauth-navbar" >
        <div>
            <img src="{$LOGO_IMAGE}" class="oauth-app-logo" alt="SuiteCRM"/>
        </div>
        <div class="oauth-user">
            <div class="oauth-user-icon stroke-white">
                {sugar_getimage name="oauth_user.svg" alt="Grant Type" class="oauth-logo" }
            </div>
            <div class="oauth-user-name">
                {$user.full_name}
            </div>
        </div>
    </div>
    <div class="p_login_middle bootstrap-container oauth-codes-authorization" style="margin-top:0;">
        <div class="row oauth-codes-authorization-row">
            <div class="center-block oauth-codes-authorization-col col-xs-12 col-sm-8 col-md-6 col-lg-6">

                <form name="OAuthAuthorizeForm" method="POST" action="index.php" >
                    <input type='hidden' name='action' value='authorize'/>
                    <input type='hidden' name='module' value='OAuth2AuthCodes'/>
                    <input type='hidden' name='oauth2authcode_logout' value='{$oauth2authcode_logout}'/>
                    <input type='hidden' name='session_id' value='{$session_id}'/>
                    <input type='hidden' name='oauth2authcode_hash' value='{$oauth2authcode_hash}'/>
                    <input type='hidden' name='confirmed' value=''/>


                    <div style="display: flex; align-items: center; justify-content: center; width: 100%;">
                        <h2 class="oauth-content-title mt-0">{sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_AUTHORIZE"} {$client.name}</h2>
                    </div>
                    <div class="oauth-container panel panel-primary">
                        <div class="panel-body">
                            <div class="oauth-content">
                                <div class="oauth-grants-title text-strong">{sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_CLIENT_INFO"}:</div>
                                <div class="oauth-client-info">
                                    <div class="oauth-client-info-icon fill-dark" style="margin-right: 1em;">
                                        {sugar_getimage name="oauth_client.svg" alt="Grant Type" class="oauth-logo" }
                                    </div>
                                    <div style="display: flex; flex-direction: column; margin-right: 1em;">
                                        <div class="oauth-client-info-title">
                                            {$client.name}
                                        </div>
                                        <div class="oauth-client-info-description">
                                            {sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_CLIENT_INFO_DESCRIPTION"}
                                        </div>

                                    </div>
                                </div>

                                <div class="oauth-grants">
                                    <div class="oauth-grants-title text-strong">{sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_REQUESTED_PERMISSIONS"}:</div>

                                    {foreach from=$grants item=grant}
                                        <div class="oauth-grant">
                                            <div class="oauth-grant-icon {$grant.iconClass}" style="margin-right: 1em;">
                                                {sugar_getimage name=$grant.icon alt="Grant Type" class="oauth-logo" }
                                            </div>
                                            <div style="display: flex; flex-direction: column; margin-right: 1em;">
                                                <div class="oauth-grant-title">
                                                    {sugar_translate module="OAuth2AuthCodes" label=$grant.name}
                                                </div>
                                                <div class="oauth-grant-description">
                                                    {sugar_translate module="OAuth2AuthCodes" label=$grant.description}
                                                </div>

                                            </div>

                                        </div>
                                    {/foreach}


                                </div>

                                <div class="oauth-info">
                                    <div class="text-strong">{sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_NOTE"}</div>
                                    <div>{sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_INFO_1"}</div>
                                    <div>{sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_INFO_2"}</div>
                                </div>

                                <div class="oauth-actions">
                                    <button class="btn btn-sm btn-default text-strong oauth-action-reject"
                                            onclick="document.OAuthAuthorizeForm.confirmed.value='abort'; document.OAuthAuthorizeForm.submit();">
                                        {sugar_translate module='OAuth2AuthCodes' label='LBL_OAUTH_ABORT'}
                                    </button>

                                    <button class="btn btn-sm btn-info text-strong oauth-action-once"
                                            onclick="document.OAuthAuthorizeForm.confirmed.value='once'; document.OAuthAuthorizeForm.submit();">
                                        {sugar_translate module='OAuth2AuthCodes' label='LBL_OAUTH_AUTHORIZE_ONCE'}
                                    </button>

                                    <button class="btn btn-sm btn-info text-strong oauth-action-always"
                                            onclick="document.OAuthAuthorizeForm.confirmed.value='always'; document.OAuthAuthorizeForm.submit();">
                                        {sugar_translate module='OAuth2AuthCodes' label='LBL_OAUTH_AUTHORIZE_AND_SAVE'}
                                    </button>
                                </div>
                                <div class="oauth-footer-redirect">
                                    <div class="oauth-redirect-description">
                                        {sugar_translate module="OAuth2AuthCodes" label="LBL_OAUTH_AUTHORIZING_WILL_REDIRECT"}
                                    </div>
                                    <div class="oauth-redirect-url">
                                        {$client.redirectUri}
                                    </div>
                                </div>


                            </div>
                        </div>
                    </div>
                </form>
            </div>
        </div>
    </div>
    <div class="p_login_bottom oauth-page-footer">
        <a id="admin_options">&copy; Supercharged by SuiteCRM</a>
        <a id="powered_by">&copy; Powered By SugarCRM</a>
    </div>
</div>


