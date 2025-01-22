{*
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
*}
<script type='text/javascript'>
    var LBL_LOGIN_SUBMIT = '{sugar_translate module="Users" label="LBL_LOGIN_SUBMIT"}';
    var LBL_REQUEST_SUBMIT = '{sugar_translate module="Users" label="LBL_REQUEST_SUBMIT"}';
    var LBL_SHOWOPTIONS = '{sugar_translate module="Users" label="LBL_SHOWOPTIONS"}';
    var LBL_HIDEOPTIONS = '{sugar_translate module="Users" label="LBL_HIDEOPTIONS"}';
</script>

<!-- Start login container -->

<div class="p_login">

	<div class="p_login_top">
		
		<a title="SuiteCRM" href="https://www.suitecrm.com">SuiteCRM</a>
		
	</div>
    
    <div class="p_login_middle">
        {if $LOGIN_ERROR_MESSAGE}
            <p align='center' class='error'>{$LOGIN_ERROR_MESSAGE}</p>
        {/if}


    <div id="loginform">
        
        <form class="form-signin" role="form" action="index.php" method="post" name="DetailView" id="form"
              onsubmit="return document.getElementById('cant_login').value == ''" autocomplete="off">
            <div class="companylogo">{$LOGIN_IMAGE}</div>
        <span class="error" id="browser_warning" style="display:none">
            {sugar_translate label="WARN_BROWSER_VERSION_WARNING"}
        </span>
		<span class="error" id="ie_compatibility_mode_warning" style="display:none">
		{sugar_translate label="WARN_BROWSER_IE_COMPATIBILITY_MODE_WARNING"}
		</span>
            {if $LOGIN_ERROR !=''}
                <span class="error">{$LOGIN_ERROR}</span>
                {if $WAITING_ERROR !=''}
                    <span class="error">{$WAITING_ERROR}</span>
                {/if}
            {else}
                <span id='post_error' class="error"></span>
            {/if}
            <input type="hidden" name="module" value="Users">
            <input type="hidden" name="action" value="Authenticate">
            <input type="hidden" name="return_module" value="Users">
            <input type="hidden" name="return_action" value="Login">
            <input type="hidden" id="cant_login" name="cant_login" value="">
            {foreach from=$LOGIN_VARS key=key item=var}
                <input type="hidden" name="{$key}" value="{$var}">
            {/foreach}
            {if !empty($SELECT_LANGUAGE)}
                <div class="login-language-chooser" >
                    {sugar_translate module="Users" label="LBL_LANGUAGE"}:
                    <select name='login_language' onchange="switchLanguage(this.value)">{$SELECT_LANGUAGE}</select>
                </div>
            {/if}
            <br>
            <div class="input-group">
                <input type="text" class="form-control"
                       placeholder="{sugar_translate module="Users" label="LBL_USER_NAME"}" required autofocus
                       tabindex="1" id="user_name" name="user_name" value='{$LOGIN_USER_NAME}' autocomplete="off">
            </div>
            <br>
            <div class="input-group">
                <input type="password" class="form-control"
                       placeholder="{sugar_translate module="Users" label="LBL_PASSWORD"}" tabindex="2"
                       id="username_password" name="username_password" value='{$LOGIN_PASSWORD}' autocomplete="off">
            </div>
            <br>
            <input id="bigbutton" class="btn btn-lg btn-primary btn-block" type="submit"
                   title="{sugar_translate module="Users" label="LBL_LOGIN_BUTTON_TITLE"}" tabindex="3" name="Login"
                   value="{sugar_translate module="Users" label="LBL_LOGIN_BUTTON_LABEL"}">
            <div id="forgotpasslink" style="cursor: pointer; display:{$DISPLAY_FORGOT_PASSWORD_FEATURE};"
                 onclick='toggleDisplay("forgot_password_dialog");'>
                <a href='javascript:void(0)'>{sugar_translate module="Users" label="LBL_LOGIN_FORGOT_PASSWORD"}</a>
            </div>
        </form>
        
        <form class="form-signin passform" role="form" action="index.php" method="post" name="DetailView" id="form" name="fp_form" id="fp_form" autocomplete="off">
            <div id="forgot_password_dialog" style="display:none">
                <input type="hidden" name="entryPoint" value="GeneratePassword">
                <div id="generate_success" class='error' style="display:inline;"></div>
                <br>
                <div class="input-group">
                    {*<span class="input-group-addon logininput glyphicon glyphicon-user"></span>*}
                    <input type="text" class="form-control" size='26' id="fp_user_name" name="fp_user_name"
                           value='{$LOGIN_USER_NAME}'
                           placeholder="{sugar_translate module="Users" label="LBL_USER_NAME"}" autocomplete="off">
                </div>
                <br>
                <div class="input-group">
                    {*<span class="input-group-addon logininput glyphicon glyphicon-envelope"></span>*}
                    <input type="text" class="form-control" size='26' id="fp_user_mail" name="fp_user_mail" value=''
                           placeholder="{sugar_translate module="Users" label="LBL_EMAIL"}" autocomplete="off">
                </div>
                <br>
                {$CAPTCHA}
                <div id='wait_pwd_generation'></div>
                <input title="Email Temp Password" class="button  btn-block" type="button" style="display:inline"
                       onclick="validateAndSubmit(); return document.getElementById('cant_login').value == ''"
                       id="generate_pwd_button" name="fp_login"
                       value="{sugar_translate module="Users" label="LBL_LOGIN_SUBMIT"}" autocomplete="off">
            </div>
        </form>
        
    </div>
    </div>
    
    <div class="p_login_bottom">

    		<a id="admin_options">&copy; Supercharged by SuiteCRM</a>
            <a id="powered_by">&copy; Powered By SugarCRM</a>
    	
	</div>
    
</div>
<!-- End login container -->



