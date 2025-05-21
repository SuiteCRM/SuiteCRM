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
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html {$langHeader}>
<head>
    <link rel="SHORTCUT ICON" href="{$FAVICON_URL}">
    <meta http-equiv="Content-Type" content="text/html; charset={$APP.LBL_CHARSET}">
    <title>{$MOD.LBL_WIZARD_TITLE}</title>
    {literal}
        <script type='text/javascript'>
            function disableReturnSubmission(e) {
                var key = window.event ? window.event.keyCode : e.which;
                return (key != 13);
            }
        </script>
    {/literal}
    {$SUGAR_JS}
    {$SUGAR_CSS}
    {$CSS}
</head>
<body class="yui-skin-sam">
<div id="main">
<div id="content">
<table style="width:auto;height:600px;" align="center"><tr><td align="center">

<form id="UserWizard" name="UserWizard" enctype='multipart/form-data' method="POST" action="index.php" onkeypress="return disableReturnSubmission(event);">
<input type='hidden' name='action' value='SaveUserWizard'/>
<input type='hidden' name='module' value='Users'/>
<span class='error'>{$error.main}</span>
<script type="text/javascript" src="{sugar_getjspath file='cache/include/javascript/sugar_grp_yui_widgets.js'}"></script>
<script type="text/javascript" src="{sugar_getjspath file='modules/Emails/javascript/vars.js'}"></script>
<script type="text/javascript" src="{sugar_getjspath file='cache/include/javascript/sugar_grp_emails.js'}"></script>
<script type="text/javascript" src="{sugar_getjspath file='modules/Users/User.js'}"></script>

<div class="dashletPanelMenu wizard">

<div class="bd">


<div id="welcome" class="screen">
    <table width="100%" border="0" cellspacing="0" cellpadding="0">
        <tr>
            <td>
                <div class="edit view">
                    <table width="100%" border="0" cellspacing="0" cellpadding="0">
                        <tr>
                            <th align="left" scope="row" colspan="4"><h2>{$MOD.LBL_WIZARD_WELCOME_TITLE}</h2></th>
                        </tr>
                        <tr>
                            <td scope="row">
                                <p> {$MOD.LBL_WIZARD_WELCOME_NOSMTP}</p>
                                <div class="userWizWelcome"><img src='include/images/sugar_wizard_welcome.jpg' border='0' alt='{$MOD.LBL_WIZARD_WELCOME_TAB}' width='765px' height='325px'></div>
                            </td>
                        </tr>
                    </table>
                </div>
            </td>
        </tr>
    </table>
    <div class="nav-buttons">
        <input title="{$MOD.LBL_WIZARD_NEXT_BUTTON}"
               class="button primary" type="button" name="next_tab1" value="  {$MOD.LBL_WIZARD_NEXT_BUTTON}  "
               onclick="SugarWizard.changeScreen('personalinfo',false);" id="next_tab_personalinfo" />
    </div>
</div>

<div id="personalinfo" class="screen">
    <table width="100%" border="0" cellspacing="0" cellpadding="0">
        <tr>
            <td>
                <div class="edit view">
                    <table width="100%" border="0" cellspacing="0" cellpadding="0">
                        <tr>
                            <th align="left" scope="row" colspan="4"><h2><span>{$MOD.LBL_WIZARD_PERSONALINFO}</span></h2></th>
                        </tr>
                        <tr>
                            <td align="left" scope="row" colspan="4"><i>{$MOD.LBL_WIZARD_PERSONALINFO_DESC}</i></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_FIRST_NAME}:</span></td>
                            <td width="33%"><span><input id='first_name' name='first_name' tabindex='1' size='25' maxlength='25' type="text" value="{$FIRST_NAME}"></span></td>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_LAST_NAME}: <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></span></td>
                            <td width="33%"><span><input id='last_name' name='last_name' tabindex='2' size='25' maxlength='25' type="text" value="{$LAST_NAME}"></span></td>
                        </tr>
                        <tr>
                            <td scope="row" width="17%">
                                {$MOD.LBL_EMAIL}: {if $REQUIRED_EMAIL_ADDRESS}<span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span>{/if}
                            </td>
                            <td width="33%"><span><input name='email1' tabindex='3' size='30' maxlength='100' value='{$EMAIL1}' id='email1' /></span></td>
                            <td scope="row" nowrap="nowrap"><span>&nbsp;</span></td>
                            <td><span>&nbsp;</span></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_OFFICE_PHONE}:</span></td>
                            <td width="33%" ><span><input name='phone_work' type="text" tabindex='4' size='20' maxlength='25' value='{$PHONE_WORK}'></span></td>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_MESSENGER_TYPE}:</span></td>
                            <td  ><span>{$MESSENGER_TYPE_OPTIONS}</span></td>
                        </tr>
                        <tr>
                            <td scope="row"><span>{$MOD.LBL_MOBILE_PHONE}:</span></td>
                            <td  ><span><input name='phone_mobile' type="text" tabindex='6' size='20' maxlength='25' value='{$PHONE_MOBILE}'></span></td>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_MESSENGER_ID}:</span></td>
                            <td  ><span><input name='messenger_id' type="text" tabindex='6' size='30' maxlength='100' value='{$MESSENGER_ID}'></span></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_PRIMARY_ADDRESS}:</span></td>
                            <td width="33%" ><span><textarea name='address_street' rows="2" tabindex='8' cols="30">{$ADDRESS_STREET}</textarea></span></td>
                            <td scope="row" nowrap="nowrap"><span>&nbsp;</span></td>
                            <td><span>&nbsp;</span></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_CITY}:</span></td>
                            <td width="33%" ><span><input name='address_city' tabindex='8' size='15' maxlength='100' value='{$ADDRESS_CITY}'></span></td>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_STATE}:</span></td>
                            <td><span><input name='address_state' tabindex='9' size='15' maxlength='100' value='{$ADDRESS_STATE}'></span></td>
                        </tr>
                        <tr>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_POSTAL_CODE}:</span></td>
                            <td><span><input name='address_postalcode' tabindex='9' size='10' maxlength='20' value='{$ADDRESS_POSTALCODE}'></span></td>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_COUNTRY}:</span></td>
                            <td><span><input name='address_country' tabindex='10' size='10' maxlength='20' value='{$ADDRESS_COUNTRY}'></span></td>
                        </tr>
                    </table>
                </div>
            </td>
        </tr>
    </table>
    <div class="nav-buttons">
        {if $SKIP_WELCOME}
            <input title="{$MOD.LBL_BACK}"
                   onclick="document.location.href='index.php?module=Configurator&action=AdminWizard&page=smtp';" class="button"
                   type="button" name="cancel" value="  {$MOD.LBL_WIZARD_BACK_BUTTON}  " id="wizard_cancel"/>&nbsp;
        {else}
            <input title="{$MOD.LBL_WIZARD_BACK_BUTTON}"
                   class="button" type="button" name="next_tab1" value="  {$MOD.LBL_WIZARD_BACK_BUTTON}  "
                   onclick="SugarWizard.changeScreen('welcome',true);" id="previous_tab_welcome" />&nbsp;
        {/if}
        <input title="{$MOD.LBL_WIZARD_NEXT_BUTTON}"
               class="button primary" type="button" name="next_tab1" value="  {$MOD.LBL_WIZARD_NEXT_BUTTON}  "
               onclick="SugarWizard.changeScreen('locale',false);" id="next_tab_locale" />
    </div>
</div>

<div id="locale" class="screen">
    <table width="100%" border="0" cellspacing="0" cellpadding="0">
        <tr>
            <td>
                <div class="edit view">
                    <table width="100%" border="0" cellspacing="0" cellpadding="0">
                        <tr>
                            <th width="100%" align="left" scope="row" colspan="4">
                                <h2><span>{$MOD.LBL_WIZARD_LOCALE}</span></h2></th>
                        </tr>
                        <tr>
                            <td align="left" scope="row" colspan="4"><i>{$MOD.LBL_WIZARD_LOCALE_DESC}</i></td>
                        </tr>
                        <tr>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_TIMEZONE}:</span>&nbsp;{sugar_help text=$MOD.LBL_TIMEZONE_TEXT }</td>
                            <td colspan="3"><span><select tabindex='14' name='timezone'>{html_options options=$TIMEZONEOPTIONS selected=$TIMEZONE_CURRENT}</select></span></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_DATE_FORMAT}:</span>&nbsp;{sugar_help text=$MOD.LBL_DATE_FORMAT_TEXT }</td>
                            <td width="33%"><span><select tabindex='14' name='dateformat'>{$DATEOPTIONS}</select></span></td>
                            <td scope="row" nowrap="nowrap"><span>{$MOD.LBL_TIME_FORMAT}:</span>&nbsp;{sugar_help text=$MOD.LBL_TIME_FORMAT_TEXT }</td>
                            <td ><span><select tabindex='14' name='timeformat'>{$TIMEOPTIONS}</select></span></td>

                        </tr>
                        <tr>
                            <td colspan="4"><hr /></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_CURRENCY}:</span>&nbsp;{sugar_help text=$MOD.LBL_CURRENCY_TEXT }</td>
                            <td ><span>
                                    <select tabindex='14' id='currency_select' name='currency' onchange='setSymbolValue(this.selectedIndex);setSigDigits();'>{$CURRENCY}</select>
                                    <input type="hidden" id="symbol" value="">
                                </span></td>
                            <td width="17%" scope="row" nowrap="nowrap"></td>
                            <td></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>
                                    {$MOD.LBL_CURRENCY_SIG_DIGITS}:
                                </span></td>
                            <td ><span>
                                    <select id='sigDigits' onchange='setSigDigits(this.value);' name='default_currency_significant_digits'>{$sigDigits}</select>
                                </span></td>
                            <td width="17%" scope="row" nowrap="nowrap"><span>
                                    <i>{$MOD.LBL_LOCALE_EXAMPLE_NAME_FORMAT}:</i>
                                </span></td>
                            <td ><span>
                                    <input type="text" disabled id="sigDigitsExample" name="sigDigitsExample">
                                </span></td>
                        </tr>
                        <tr>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_DECIMAL_SEP}:</span>&nbsp;{sugar_help text=$MOD.LBL_DECIMAL_SEP_TEXT }</td>
                            <td ><span>
                                    <input tabindex='14' name='dec_sep' id='default_decimal_seperator'
                                           type='text' maxlength='1' size='1' value='{$DEC_SEP}'
                                           onkeydown='setSigDigits();' onkeyup='setSigDigits();'>
                                </span></td>
                            <td width="17%" scope="row" nowrap="nowrap"><span>{$MOD.LBL_NUMBER_GROUPING_SEP}:</span>&nbsp;{sugar_help text=$MOD.LBL_NUMBER_GROUPING_SEP_TEXT }</td>
                            <td><input tabindex='14' name='num_grp_sep' id='default_number_grouping_seperator'
                                       type='text' maxlength='1' size='1' value='{$NUM_GRP_SEP}'
                                       onkeydown='setSigDigits();' onkeyup='setSigDigits();'></td>
                        </tr>
                        <tr>
                            <td colspan="4"><hr /></td>
                        </tr>
                        <tr>
                            {capture name=SMARTY_LOCALE_NAME_FORMAT_DESC}&nbsp;{$MOD.LBL_LOCALE_NAME_FORMAT_DESC}{/capture}
                            <td nowrap="nowrap" scope="row" valign="top">{$MOD.LBL_LOCALE_DEFAULT_NAME_FORMAT}:&nbsp;{sugar_help text=$smarty.capture.SMARTY_LOCALE_NAME_FORMAT_DESC }</td>
                            <td><span><select id="default_locale_name_format" tabindex='14' name="default_locale_name_format" selected="{$default_locale_name_format}">{$NAMEOPTIONS}</select></span></td>
                        </tr>
                        <tr>
                            <td nowrap="nowrap" scope="row" valign="top">{$MOD.LBL_USER_LANGUAGE}:&nbsp;</td>
                            <td><span><select id="user_language" tabindex='14' name="user_language" >{$user_language}</select></span></td>
                        </tr>
                    </table>
                </div>
            </td>
        </tr>
    </table>
    <div class="nav-buttons">
        <input title="{$MOD.LBL_WIZARD_BACK_BUTTON}"
               class="button" type="button" name="next_tab1" value="  {$MOD.LBL_WIZARD_BACK_BUTTON}  "
               onclick="SugarWizard.changeScreen('personalinfo',true);" id="previous_tab_personalinfo" />&nbsp;
        <input title="{$MOD.LBL_WIZARD_NEXT_BUTTON}"
               class="button primary" type="button" name="next_tab1" value="  {$MOD.LBL_WIZARD_NEXT_BUTTON}  "
        onclick="SugarWizard.changeScreen('finish',false);" id="next_tab_finish" />
    </div>
</div>
<div id="finish" class="screen">
    <table width="100%" border="0" cellspacing="0" cellpadding="0">
        <tr>
            <td>
                <div class="edit view">
                    <table width="100%" border="0" cellspacing="0" cellpadding="0">
                        <tr>
                            <th align="left" scope="row" colspan="4"><h2>{$MOD.LBL_WIZARD_FINISH_TITLE}</h2></th>
                        </tr>
                        <tr>
                            <td scope="row">
                                <h3>{$MOD.LBL_WIZARD_FINISH1}</h3>

                                <table cellpadding=0 cellspacing=0><input id='whatnext' name='whatnext' type="hidden" value='finish' />
                                    {if $IS_ADMIN}
                                        <tr><td><img src=include/images/start.png style="margin-right: 5px;"></td><td><a onclick='document.UserWizard.whatnext.value="finish";document.UserWizard.submit()' href="#" ><b> {$MOD.LBL_WIZARD_FINISH2}  </b></a><br> {$MOD.LBL_WIZARD_FINISH2DESC}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/import.png style="margin-right: 5px;"></td><td><a onclick='document.UserWizard.whatnext.value="import";document.UserWizard.submit()' href="#" ><b> {$MOD.LBL_WIZARD_FINISH3} </b></a><br> {$MOD.LBL_WIZARD_FINISH4}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/create_users.png style="margin-right: 5px;"></td><td><a onclick='document.UserWizard.whatnext.value="users";document.UserWizard.submit()' href="#"  ><b> {$MOD.LBL_WIZARD_FINISH5} </b></a><br>{$MOD.LBL_WIZARD_FINISH6}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/settings.png style="margin-right: 5px;"></td><td><a  onclick='document.UserWizard.whatnext.value="settings";document.UserWizard.submit()' href="#" ><b> {$MOD.LBL_WIZARD_FINISH7} </b></a><br>{$MOD.LBL_WIZARD_FINISH8}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/configure.png style="margin-right: 5px;"></td><td><a onclick='document.UserWizard.whatnext.value="studio";document.UserWizard.submit()' href="#"  ><b> {$MOD.LBL_WIZARD_FINISH9} </b></a><br>{$MOD.LBL_WIZARD_FINISH10}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/university.png style="margin-right: 5px;"></td><td><a href="https://community.suitecrm.com" target="_blank"><b> {$MOD.LBL_WIZARD_FINISH11} </b></a></b><br>{$MOD.LBL_WIZARD_FINISH12}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                    {else}
                                        <tr><td><img src=include/images/university2.png style="margin-right: 5px;"></td><td><a href="https://community.suitecrm.com" target="_blank"><b> {$MOD.LBL_WIZARD_FINISH11} </b></a></b><br>{$MOD.LBL_WIZARD_FINISH12}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/docs.png style="margin-right: 5px;"></td><td><a href="https://docs.suitecrm.com/user/" target="_blank"><b> {$MOD.LBL_WIZARD_FINISH14} </b></a></b><br>{$MOD.LBL_WIZARD_FINISH15}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                        <tr><td><img src=include/images/forums.png style="margin-right: 5px;"></td><td><a href="https://community.suitecrm.com/" target="_blank"><b> {$MOD.LBL_WIZARD_FINISH18} </b></a></b><br>{$MOD.LBL_WIZARD_FINISH19}</td></tr>
                                        <tr><td colspan=2><hr style="margin: 5px 0px;"></td></tr>
                                    {/if}
                                </table>
                            </td>
                        </tr>
                    </table>
                </div>
            </td>
        </tr>
    </table>
    <div class="nav-buttons">
        <input title="{$MOD.LBL_WIZARD_BACK_BUTTON}"
               class="button" type="button" name="next_tab1" value="  {$MOD.LBL_WIZARD_BACK_BUTTON}  "
        onclick="SugarWizard.changeScreen('locale',true);" id="previous_tab_locale" />&nbsp;
        <input title="{$MOD.LBL_WIZARD_FINISH_BUTTON}" class="button primary"
               type="submit" name="save" value="  {$MOD.LBL_WIZARD_FINISH_BUTTON}  " />&nbsp;
    </div>
</div>




</div>

</div>

{literal}
<script type='text/javascript'>
<!--
var SugarWizard = new function()
{
    this.currentScreen = 'welcome';

    this.handleKeyStroke = function(e)
    {
        // get the key pressed
        var key;
        if (window.event) {
            key = window.event.keyCode;
        }
        else if(e.which) {
            key = e.which
        }

        switch(key) {
            case 13:
                primaryButton = YAHOO.util.Selector.query('input.primary',SugarWizard.currentScreen,true);
                primaryButton.click();
                break;
        }
    }

    this.changeScreen = function(screen,skipCheck)
    {
        if ( !skipCheck ) {
            clear_all_errors();
            var form = document.getElementById('UserWizard');
            var isError = false;

            switch(this.currentScreen) {
                case 'personalinfo':
                    if ( document.getElementById('last_name').value == '' ) {
                        add_error_style('UserWizard',form.last_name.name,
                                '{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS} {$MOD.LBL_LAST_NAME}{literal}' );
                        isError = true;
                    }
                {/literal}
                {if $REQUIRED_EMAIL_ADDRESS}
                {literal}
                    if ( document.getElementById('email1').value == '' ) {
                        document.getElementById('email1').focus();
                        add_error_style('UserWizard',form.email1.name,
                                '{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS} {$MOD.LBL_EMAIL}{literal}' );
                        isError = true;
                    }
                {/literal}
                {/if}
                {literal}
                    break;
            }
            if (isError == true)
                return false;
        }

        document.getElementById(this.currentScreen).style.display = 'none';
        document.getElementById(screen).style.display = 'block';

        this.currentScreen = screen;
    }
}

$(document).ready(function() {
  $('.screen').each(function() {
    $(this).hide();
  });

  {/literal}
  {if $SKIP_WELCOME}
  SugarWizard.changeScreen('personalinfo');
  {else}
  SugarWizard.changeScreen('welcome');
  {/if}
  {literal}
});

document.onkeypress = SugarWizard.handleKeyStroke;

var mail_smtpport = '{/literal}{$MAIL_SMTPPORT}{literal}';
var mail_smtpssl = '{/literal}{$MAIL_SMTPSSL}{literal}';

EmailMan = {};

function startOutBoundEmailSettingsTest()
{
    var loader = new YAHOO.util.YUILoader({
        require : ["element","sugarwidgets"],
        loadOptional: true,
        skin: { base: 'blank', defaultSkin: '' },
        onSuccess: testOutboundSettings,
        allowRollup: true,
        base: "include/javascript/yui/build/"
    });
    loader.addModule({
        name :"sugarwidgets",
        type : "js",
        fullpath: "include/javascript/sugarwidgets/SugarYUIWidgets.js",
        varName: "YAHOO.SUGAR",
        requires: ["datatable", "dragdrop", "treeview", "tabview"]
    });
    loader.insert();

}

function testOutboundSettings()
{
    var errorMessage = '';
    var isError = false;
    var fromAddress = document.getElementById("outboundtest_from_address").value;
    var errorMessage = '';
    var isError = false;
    var smtpServer = document.getElementById('mail_smtpserver').value;

    var mailsmtpauthreq = document.getElementById('mail_smtpauth_req');
    if(trim(smtpServer) == '' || trim(mail_smtpport) == '')
    {
        isError = true;
        errorMessage += "{/literal}{$MOD.LBL_MISSING_DEFAULT_OUTBOUND_SMTP_SETTINGS}{literal}" + "<br/>";
        overlay("{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS}{literal}", errorMessage, 'alert');
        return false;
    }

    if(document.getElementById('mail_smtpuser') && trim(document.getElementById('mail_smtpuser').value) == '')
    {
        isError = true;
        errorMessage += "{/literal}{$APP.LBL_EMAIL_ACCOUNTS_SMTPUSER}{literal}" + "<br/>";
    }

    if(document.getElementById('mail_smtppass') && trim(document.getElementById('mail_smtppass').value) == '')
    {
        isError = true;
        errorMessage += "{/literal}{$APP.LBL_EMAIL_ACCOUNTS_SMTPPASS}{literal}" + "<br/>";
    }

    if(isError) {
        overlay("{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS}{literal}", errorMessage, 'alert');
        return false;
    }

    testOutboundSettingsDialog();
}

function sendTestEmail()
{
    var fromAddress = document.getElementById("outboundtest_from_address").value;

    if (trim(fromAddress) == "")
    {
        overlay("{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS}{literal}", "{{/literal}$APP.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR}{literal}", 'alert');
        return;
    }
    else if (!isValidEmail(fromAddress)) {
        overlay("{/literal}{$APP.ERR_INVALID_REQUIRED_FIELDS}{literal}", "{/literal}{$APP.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR}{literal}", 'alert');
        return;
    }

    //Hide the email address window and show a message notifying the user that the test email is being sent.
    EmailMan.testOutboundDialog.hide();
    overlay("{/literal}{$APP.LBL_EMAIL_PERFORMING_TASK}{literal}", "{/literal}{$APP.LBL_EMAIL_ONE_MOMENT}{literal}", 'alert');

    var callbackOutboundTest = {
        success	: function(o) {
            hideOverlay();
            overlay("{/literal}{$APP.LBL_EMAIL_TEST_OUTBOUND_SETTINGS}{literal}", "{/literal}{$APP.LBL_EMAIL_TEST_NOTIFICATION_SENT}{literal}", 'alert');
        }
    };
    var smtpServer = document.getElementById('mail_smtpserver').value;
    if(document.getElementById('mail_smtpuser') && document.getElementById('mail_smtppass'))
    {
        var postDataString = 'mail_sendtype=SMTP&mail_smtpserver=' + smtpServer + "&mail_smtpport=" + mail_smtpport + "&mail_smtpssl=" + mail_smtpssl + "&mail_smtpauth_req=true&mail_smtpuser=" + trim(document.getElementById('mail_smtpuser').value) + "&mail_smtppass=" + trim(document.getElementById('mail_smtppass').value) + "&outboundtest_from_address=" + fromAddress;
    }
    else
    {
        var postDataString = 'mail_sendtype=SMTP&mail_smtpserver=' + smtpServer + "&mail_smtpport=" + mail_smtpport + "&mail_smtpssl=" + mail_smtpssl + "&outboundtest_from_address=" + fromAddress;
    }
    YAHOO.util.Connect.asyncRequest("POST", "index.php?action=testOutboundEmail&module=EmailMan&to_pdf=true&sugar_body_only=true", callbackOutboundTest, postDataString);
}
function testOutboundSettingsDialog() {
    // lazy load dialogue
    if(!EmailMan.testOutboundDialog) {
        EmailMan.testOutboundDialog = new YAHOO.widget.Dialog("testOutboundDialog", {
            modal:true,
            visible:true,
            fixedcenter:true,
            constraintoviewport: true,
            width	: 600,
            shadow	: false
        });
        EmailMan.testOutboundDialog.setHeader("{/literal}{$APP.LBL_EMAIL_TEST_OUTBOUND_SETTINGS}{literal}");
        YAHOO.util.Dom.removeClass("testOutboundDialog", "yui-hidden");
    } // end lazy load

    EmailMan.testOutboundDialog.render();
    EmailMan.testOutboundDialog.show();
} // fn

function overlay(reqtitle, body, type) {
    var config = { };
    config.type = type;
    config.title = reqtitle;
    config.msg = body;
    YAHOO.SUGAR.MessageBox.show(config);
}

function hideOverlay() {
    YAHOO.SUGAR.MessageBox.hide();
}
-->
</script>
{/literal}
{$JAVASCRIPT}
{literal}
<script type="text/javascript" language="Javascript">
    {/literal}
    {$getNameJs}
    {$getNumberJs}
    {$currencySymbolJs}
    setSymbolValue(document.getElementById('currency_select').selectedIndex);
    setSigDigits();

    {$confirmReassignJs}
</script>
</form>

<div id="testOutboundDialog" class="yui-hidden">
    <div id="testOutbound">
        <form>
            <table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
                <tr>
                    <td scope="row">
                        {$APP.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR}
                        <span class="required">
						{$APP.LBL_REQUIRED_SYMBOL}
					</span>
                    </td>
                    <td >
                        <input type="text" id="outboundtest_from_address" name="outboundtest_from_address" size="35" maxlength="64" value="">
                    </td>
                </tr>
                <tr>
                    <td scope="row" colspan="2">
                        <input type="button" class="button" value="   {$APP.LBL_EMAIL_SEND}   " onclick="javascript:sendTestEmail();">&nbsp;
                        <input type="button" class="button" value="   {$APP.LBL_CANCEL_BUTTON_LABEL}   " onclick="javascript:EmailMan.testOutboundDialog.hide();">&nbsp;
                    </td>
                </tr>
            </table>
        </form>
    </div>
</div>
