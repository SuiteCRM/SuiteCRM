<script type="text/javascript" src="cache/include/javascript/sugar_grp_yui_widgets.js"></script>
<div id="smtpButtonGroup" class="yui-buttongroup">
    <span id="gmail" class="yui-button yui-radio-button{if $mail_smtptype == 'gmail'} yui-button-checked{/if}">
        <span class="first-child">
            <button type="button" name="mail_smtptype" value="gmail">
                &nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_GMAIL}&nbsp;&nbsp;&nbsp;&nbsp;
            </button>
        </span>
    </span>
    <span id="yahoomail" class="yui-button yui-radio-button{if $mail_smtptype == 'yahoomail'} yui-button-checked{/if}">
        <span class="first-child">
            <button type="button" name="mail_smtptype" value="yahoomail">
                &nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_YAHOO}&nbsp;&nbsp;&nbsp;&nbsp;
            </button>
        </span>
    </span>
    <span id="exchange" class="yui-button yui-radio-button{if $mail_smtptype == 'exchange'} yui-button-checked{/if}">
        <span class="first-child">
            <button type="button" name="mail_smtptype" value="exchange">
                &nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_EXCHANGE}&nbsp;&nbsp;&nbsp;&nbsp;
            </button>
        </span>
    </span>
    <span id="other" class="yui-button yui-radio-button{if $mail_smtptype == 'other' || empty($mail_smtptype)} yui-button-checked{/if}">
        <span class="first-child">
            <button type="button" name="mail_smtptype" value="other">
                &nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_OTHER}&nbsp;&nbsp;&nbsp;&nbsp;
            </button>
        </span>
    </span>
</div>

{literal}
<script type="text/javascript">
    $(function(){
        $('#mail_smtpserver').closest('tr').attr('id', 'mailsettings1');
        $('#mail_smtpauth_req').closest('tr').attr('id', 'mailsettings2');
    });

    function changeEmailScreenDisplay(smtptype, clear)
    {
        if(clear) {
            document.getElementById("mail_smtpserver").value = '';
            document.getElementById("mail_smtpport").value = '25';
            document.getElementById("mail_smtpauth_req").checked = true;
        }

        switch (smtptype) {
            case "yahoomail":
                document.getElementById("mail_smtpserver").value = 'smtp.mail.yahoo.com';
                document.getElementById("mail_smtpport").value = '465';
                document.getElementById("mail_smtpauth_req").checked = true;
                var ssl = document.getElementById("mail_smtpssl");
                for(var j=0;j<ssl.options.length;j++) {
                    if(ssl.options[j].text == 'SSL') {
                        ssl.options[j].selected = true;
                        break;
                    }
                }
                document.getElementById("mailsettings1").style.display = 'none';
                document.getElementById("mailsettings2").style.display = 'none';
                //document.getElementById("password_change_label").innerHTML =
                        document.getElementById("password_change_label").innerHTML = '{/literal}{$MOD.LBL_YAHOOMAIL_SMTPPASS}{literal}';
                document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_YAHOOMAIL_SMTPUSER}{literal}';
                break;
            case "gmail":
                if(document.getElementById("mail_smtpserver").value == "" || document.getElementById("mail_smtpserver").value == 'smtp.mail.yahoo.com') {
                    document.getElementById("mail_smtpserver").value = 'smtp.gmail.com';
                    document.getElementById("mail_smtpport").value = '587';
                    document.getElementById("mail_smtpauth_req").checked = true;
                    var ssl = document.getElementById("mail_smtpssl");
                    for(var j=0;j<ssl.options.length;j++) {
                        if(ssl.options[j].text == 'TLS') {
                            ssl.options[j].selected = true;
                            break;
                        }
                    }
                }
                //document.getElementById("mailsettings1").style.display = 'none';
                //document.getElementById("mailsettings2").style.display = 'none';
                document.getElementById("password_change_label").innerHTML = '{/literal}{$MOD.LBL_GMAIL_SMTPPASS}{literal}';
                document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_GMAIL_SMTPUSER}{literal}';
                break;
            case "exchange":
                if ( document.getElementById("mail_smtpserver").value == 'smtp.mail.yahoo.com'
                        || document.getElementById("mail_smtpserver").value == 'smtp.gmail.com' ) {
                    document.getElementById("mail_smtpserver").value = '';
                }
                document.getElementById("mail_smtpport").value = '465';
                //document.getElementById("mail_smtpauth_req").checked = true; bug 40998
                document.getElementById("mailsettings1").style.display = '';
                document.getElementById("mailsettings2").style.display = '';
                document.getElementById("password_change_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPPASS}{literal}';
                document.getElementById("mail_smtpport_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPPORT}{literal}';
                document.getElementById("mail_smtpserver_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPSERVER}{literal}';
                document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPUSER}{literal}';
                break;
        }
        setDefaultSMTPPort();
        notify_setrequired(document.ConfigureSettings);
    }
    var oButtonGroup = new YAHOO.widget.ButtonGroup("smtpButtonGroup");
    oButtonGroup.subscribe('checkedButtonChange', function(e)
    {
        changeEmailScreenDisplay(e.newValue.get('value'), true);
        document.getElementById('smtp_settings').style.display = '';
        document.getElementById('EditView').mail_smtptype.value = e.newValue.get('value');
    });
</script>
{/literal}