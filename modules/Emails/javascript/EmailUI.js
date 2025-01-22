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


(function () {
  var sw = YAHOO.SUGAR,
    Event = YAHOO.util.Event,
    Connect = YAHOO.util.Connect,
    Dom = YAHOO.util.Dom
  SE = SUGAR.email2;

///////////////////////////////////////////////////////////////////////////////
////    EMAIL ACCOUNTS
  SE.accounts = {
    outboundDialog: null,
    inboundAccountEditDialog: null,
    inboundAccountsSettingsTable: null,
    outboundAccountsSettingsTable: null,
    testOutboundDialog: null,
    errorStyle: 'input-error',
    normalStyle: '',
    newAddedOutboundId: '',

    /**
     * makes async call to retrieve an outbound instance for editting
     */
    //EXT111
    editOutbound: function (obi) {

      AjaxObject.startRequest(AjaxObject.accounts.callbackEditOutbound, urlStandard + "&emailUIAction=editOutbound&outbound_email=" + obi);

    },
    deleteOutbound: function (obi) {

      if (obi.match(/^(add|line|sendmail)+/)) {
        alert('Invalid Operation');
      } else {
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_DELETING_OUTBOUND, app_strings.LBL_EMAIL_ONE_MOMENT);
        AjaxObject.startRequest(AjaxObject.accounts.callbackDeleteOutbound, urlStandard + "&emailUIAction=deleteOutbound&outbound_email=" + obi);
      }
    },
    //EXT111
    getReplyAddress: function () {
      var primary = '';

      if (!(document.getElementById('reply_to_addr').value = typeof SE === 'undefined' || typeof SE.userPrefs === 'undefined' || typeof SE.userPrefs.current_user === 'undefined' || typeof SE.userPrefs.current_user.full_name === 'undefined')) {
        for (var i = 0; i < SE.userPrefs.current_user.emailAddresses.length; i++) {
          var addy = SE.userPrefs.current_user.emailAddresses[i];

          if (addy.primary_address == "1") {
            primary = addy.email_address;
          }

          if (addy.reply_to == "1") {
            return addy.email_address;
          }
        }
      }
      return primary;
    },

    /**
     * Called on "Accounts" tab activation event
     */
    lazyLoad: function (user) {

      this._setupInboundAccountTable(user);
      this._setupOutboundAccountTable(user);

    },

    _setupInboundAccountTable: function (user) {
      //Setup the inbound mail settings
      if (!this.inboundAccountsSettingsTable) {
        this.customImageFormatter = function (elLiner, oRecord, oColumn, oData) {
          var clckEvent = oColumn.key;
          var imgSrc = "";
          var is_group = oRecord.getData("is_group");
          if (!is_group) {
            if (oColumn.key == 'edit') {
              clckEvent = "SUGAR.email2.accounts.getIeAccount('" + oRecord.getData('id') + "')";
              imgSrc = 'index.php?entryPoint=getImage&amp;themeName=Sugar&amp;imageName=' + oColumn.key + '_inline.gif';
            }
            else if (oColumn.key == 'delete') {
              clckEvent = "SUGAR.email2.accounts.deleteIeAccount('" + oRecord.getData('id') + "','" + oRecord.getData('group_id') + "')";
              imgSrc = 'index.php?entryPoint=getImage&amp;themeName=Sugar&amp;imageName=' + oColumn.key + '_inline.gif';
            }
            elLiner.innerHTML = '<img onclick="' + clckEvent + '" src="' + imgSrc + '" align="absmiddle" border="0"/>';
          }
        };

        this.showBoolean = function (el, oRecord, oColumn, oData) {
          var is_group = oRecord.getData("is_group");
          var bChecked = oData;
          bChecked = (bChecked) ? " checked" : "";
          if (!is_group) {
            el.innerHTML = "<input type=\"radio\"" + bChecked +
              " name=\"col" + oColumn.getId() + "-radio\"" +
              " class=\"yui-dt-radio\">";
          }
        };


        YAHOO.widget.DataTable.Formatter.customImage = this.customImageFormatter;
        YAHOO.widget.DataTable.Formatter.showBoolean = this.showBoolean;

        var typeHoverHelp = '&nbsp;<div id="rollover"><a href="#" class="rollover">' +
          '<img border="0" src="index.php?entryPoint=getImage&amp;imageName=helpInline.png">' +
          '<div style="text-align:left"><span>' + mod_strings.LBL_EMAIL_INBOUND_TYPE_HELP + '</span></div></a></div>';


        this.ieColumnDefs = [{key: 'name', label: app_strings.LBL_EMAIL_SETTINGS_NAME}, {
          key: 'server_url',
          label: ie_mod_strings.LBL_SERVER_URL
        },
          {
            key: 'is_active',
            label: ie_mod_strings.LBL_STATUS_ACTIVE,
            formatter: "checkbox",
            className: 'yui-cstm-cntrd-liner'
          },
          {
            key: 'is_default',
            label: app_strings.LBL_EMAIL_ACCOUNTS_SMTPDEFAULT,
            formatter: "showBoolean",
            className: 'yui-cstm-cntrd-liner'
          },
          {key: 'type', label: mod_strings.LBL_LIST_TYPE + typeHoverHelp},
          {
            key: 'edit',
            label: mod_strings.LBL_BUTTON_EDIT,
            formatter: "customImage",
            className: 'yui-cstm-cntrd-liner'
          },
          {
            key: 'delete',
            label: app_strings.LBL_EMAIL_DELETE,
            formatter: "customImage",
            className: 'yui-cstm-cntrd-liner'
          }];
        var query = "index.php?module=Emails&action=EmailUIAjax&to_pdf=true&emailUIAction=rebuildShowAccount" + (user ? '&user=' + user : '');
        this.ieDataSource = new YAHOO.util.DataSource(query);
        this.ieDataSource.responseType = YAHOO.util.DataSource.TYPE_JSON;
        this.ieDataSource.responseSchema = {
          resultsList: "account_list",
          fields: [{key: 'id'}, {key: 'name'}, 'is_active', {key: 'server_url'}, 'is_group', 'group_id', 'is_default', 'has_groupfolder', 'type']
        };
        this.inboundAccountsSettingsTable = new YAHOO.widget.DataTable("inboundAccountsTable", this.ieColumnDefs, this.ieDataSource);
        this.inboundAccountsSettingsTable.subscribe("checkboxClickEvent", function (oArgs) {

          var elCheckbox = oArgs.target;
          var oColumn = this.getColumn(elCheckbox);
          if (oColumn.key == 'is_active') {

            SUGAR.email2.folders.setFolderSelection();

          }
        });
        var lastDefaultSelectedId = "";
        this.inboundAccountsSettingsTable.subscribe("radioClickEvent", function (oArgs) {

          var elRadio = oArgs.target;
          var oColumn = this.getColumn(elRadio);
          if (oColumn.key == 'is_default') {
            var oRecord = this.getRecord(elRadio);
            var t_id = oRecord.getData('id');
            var t_isGroup = oRecord.getData('is_group');
            if (t_id != lastDefaultSelectedId && !t_isGroup) {
              SUGAR.default_inbound_accnt_id = t_id; //Set in the global space for access during compose
              lastDefaultSelectedId = t_id;
              AjaxObject.startRequest(callbackDefaultOutboundSave, urlStandard + "&emailUIAction=saveDefaultOutbound&id=" + t_id);
            }
            else if (t_isGroup)
              YAHOO.util.Event.preventDefault(oArgs.event); //Do not allow users to select group mailboxes as a default.

          }
        });
      }
    },
    _setupOutboundAccountTable: function (user) {
      if (!this.outboundAccountsSettingsTable) {
        this.obImageFormatter = function (elLiner, oRecord, oColumn, oData) {
          var clckEvent = oColumn.key;
          var imgSrc = "";
          var isEditable = oRecord.getData("is_editable");
          var type = oRecord.getData("type");
          if (isEditable) {
            if (oColumn.key == 'edit') {
              clckEvent = "SUGAR.email2.accounts.editOutbound('" + oRecord.getData('id') + "')";
              imgSrc = 'index.php?entryPoint=getImage&amp;themeName=Sugar&amp;imageName=' + oColumn.key + '_inline.gif';
            }
            else if (oColumn.key == 'delete' && type == 'user') {
              clckEvent = "SUGAR.email2.accounts.deleteOutbound('" + oRecord.getData('id') + "')";
              imgSrc = 'index.php?entryPoint=getImage&amp;themeName=Sugar&amp;imageName=' + oColumn.key + '_inline.gif';
            }
            if (imgSrc != '')
              elLiner.innerHTML = '<img onclick="' + clckEvent + '" src="' + imgSrc + '" align="absmiddle" border="0"/>';
          }
        };

        //Custom formatter to display any error messages.
        this.messageDisplay = function (elLiner, oRecord, oColumn, oData) {

          if (SUGAR.email2.composeLayout.outboundAccountErrors == null)
            SUGAR.email2.composeLayout.outboundAccountErrors = {};

          var id = oRecord.getData('id');
          var message = oRecord.getData("errors");
          if (message != '') {
            elLiner.innerHTML = '<span class="required">' + message + '</span>';
            //Add the id and message for all outbound accounts.
            SUGAR.email2.composeLayout.outboundAccountErrors[id] = message;
          }
          else {
            if (typeof(SUGAR.email2.composeLayout.outboundAccountErrors[id]) != 'undefined')
              delete SUGAR.email2.composeLayout.outboundAccountErrors[id];
          }
        };
        YAHOO.widget.DataTable.Formatter.actionsImage = this.obImageFormatter;
        YAHOO.widget.DataTable.Formatter.messageDisplay = this.messageDisplay;

        this.obAccntsColumnDefs = [{key: 'name', label: app_strings.LBL_EMAIL_ACCOUNTS_NAME}, {
          key: 'mail_smtpserver',
          label: app_strings.LBL_EMAIL_ACCOUNTS_SMTPSERVER
        },
          {
            key: 'edit',
            label: mod_strings.LBL_BUTTON_EDIT,
            formatter: "actionsImage",
            className: 'yui-cstm-cntrd-liner'
          },
          {
            key: 'delete',
            label: app_strings.LBL_EMAIL_DELETE,
            formatter: "actionsImage",
            className: 'yui-cstm-cntrd-liner'
          },
          {key: 'messages', label: '', formatter: "messageDisplay", className: 'yui-cstm-cntrd-liner'}];

        var query = "index.php?module=Emails&action=EmailUIAjax&to_pdf=true&emailUIAction=retrieveAllOutbound" + (user ? '&user=' + user : '');
        this.obDataSource = new YAHOO.util.DataSource(query);
        this.obDataSource.responseType = YAHOO.util.DataSource.TYPE_JSON;
        this.obDataSource.responseSchema = {

          resultsList: "outbound_account_list",
          fields: ['id', 'name', 'is_editable', 'mail_smtpserver', 'type', 'errors']
        };

        this.outboundAccountsSettingsTable = new YAHOO.widget.DataTable("outboundAccountsTable", this.obAccntsColumnDefs, this.obDataSource);


        this.outboundAccountsSettingsTable.subscribe("rowMouseoverEvent", this.outboundAccountsSettingsTable.onEventHighlightRow);
        this.outboundAccountsSettingsTable.subscribe("rowMouseoutEvent", this.outboundAccountsSettingsTable.onEventUnhighlightRow);
        this.outboundAccountsSettingsTable.subscribe("postRenderEvent", this.rebuildMailerOptions);
      }
    },
    /**
     * Displays a modal diaglogue to edit outbound account settings
     */
    showEditInboundAccountDialogue: function (clear) {

      if (!this.inboundAccountEditDialog) {
        var EAD = this.inboundAccountEditDialog = new YAHOO.widget.Dialog("editAccountDialogue", {
          modal: true,
          visible: true,
          //fixedcenter:true,
          //constraintoviewport: true,
          width: "600px",
          shadow: true
        });
        EAD.showEvent.subscribe(function () {
          var el = this.element;
          var viewH = YAHOO.util.Dom.getViewportHeight();
          if (this.header && el && viewH - 50 < el.clientHeight) {
            var body = this.header.nextElementSibling;
            body.style.overflow = "hidden";
            body.style.height = "100%";
          }
        }, EAD);
        EAD.setHeader(mod_strings.LBL_EMAIL_ACCOUNTS_INBOUND);
        Dom.removeClass("editAccountDialogue", "yui-hidden");

      } // end lazy load

      if (clear == undefined || clear == true) {
        SE.accounts.clearInboundAccountEditScreen();
        //Set default protocol to IMAP when creating new records
        document.forms['ieAccount'].elements['protocol'].value = "imap";
        SE.accounts.setPortDefault();
      }

      //Check if we should display username/password fields for outbound account if errors were detected.
      this.checkOutBoundSelection();

      this.inboundAccountEditDialog.render();
      this.inboundAccountEditDialog.show();
      SUGAR.util.setEmailPasswordDisplay('email_password', clear == false);
    },

    /**
     *  Set all fields on the outbound edit form to either enabled/disabled
     *  except for the username/password.
     *
     */
    toggleOutboundAccountDisabledFields: function (disable) {
      var fields = ['mail_name', 'mail_smtpserver', 'mail_smtpport', 'mail_smtpauth_req'];
      for (var i = 0; i < fields.length; i++) {
        document.getElementById(fields[i]).disabled = disable;
      }
      if (disable)
        Dom.addClass("mail_smtpssl_row", "yui-hidden");
      else
        Dom.removeClass('mail_smtpssl_row', "yui-hidden");

    },
    /**
     * Refresh the inbound accounts table.
     */
    refreshInboundAccountTable: function () {
      this.inboundAccountsSettingsTable.getDataSource().sendRequest('',
        {
          success: this.inboundAccountsSettingsTable.onDataReturnInitializeTable,
          scope: this.inboundAccountsSettingsTable
        }
      );
    },
    /**
     * Refresh the outbound accounts table.
     */
    refreshOuboundAccountTable: function () {
      this.outboundAccountsSettingsTable.getDataSource().sendRequest('',
        {
          success: this.outboundAccountsSettingsTable.onDataReturnInitializeTable,
          scope: this.outboundAccountsSettingsTable
        }
      );
    },
    /**
     * Displays a modal diaglogue to add a SMTP server
     */
    showAddSmtp: function () {
      // lazy load dialogue
      if (!this.outboundDialog) {
        this.outboundDialog = new YAHOO.widget.Dialog("outboundDialog", {
          modal: true,
          visible: true,
          fixedcenter: true,
          constraintoviewport: true,
          width: "750px",
          shadow: true
        });
        this.outboundDialog.setHeader(app_strings.LBL_EMAIL_ACCOUNTS_OUTBOUND);
        this.outboundDialog.hideEvent.subscribe(function () {
          //If add was used to bring this dialog up, and we are hiding without creating one, then set it back to the first option
          var out = Dom.get("outbound_email");
          if (out && out.value == "SYSTEM_ADD") {
            out.value = out.options[0].value;
          }
          //Check if we should display username/password for system account.
          SE.accounts.checkOutBoundSelection();
          return true;
        });

        Dom.removeClass("outboundDialog", "yui-hidden");
      } // end lazy load

      // clear out form
      var form = document.getElementById('outboundEmailForm');
      for (i = 0; i < form.elements.length; i++) {
        if (form.elements[i].name == 'mail_smtpport') {
          form.elements[i].value = 25;
        } else if (form.elements[i].type != 'button' && form.elements[i].type != 'checkbox') {
          form.elements[i].value = '';
        } else if (form.elements[i].type == 'checkbox') {
          form.elements[i].checked = false;
        }
      }
      //Render the SMTP buttons
      if (!SUGAR.smtpButtonGroup) {
        SUGAR.smtpButtonGroup = new YAHOO.widget.ButtonGroup("smtpButtonGroup");
        SUGAR.smtpButtonGroup.subscribe('checkedButtonChange', function (e) {
          SUGAR.email2.accounts.changeEmailScreenDisplay(e.newValue.get('value'));
          document.getElementById('smtp_settings').style.display = '';
          form.mail_smtptype.value = e.newValue.get('value');
        });
        YAHOO.widget.Button.addHiddenFieldsToForm(form);
      }
      //Hide Username/Password
      SUGAR.email2.accounts.smtp_authenticate_field_display();
      //Unset readonly fields
      SUGAR.email2.accounts.toggleOutboundAccountDisabledFields(false);
      SUGAR.email2.accounts.changeEmailScreenDisplay('other');
      this.outboundDialog.render();
      this.outboundDialog.show();
    },

    /**
     * Accounts' Advanced Settings view toggle
     */
    toggleAdv: function () {
      var adv = document.getElementById("ie_adv");
      if (adv.style.display == 'none') {
        adv.style.display = "";
      } else {
        adv.style.display = 'none';
      }
    },

    smtp_authenticate_field_display: function () {
      var smtpauth_req = document.getElementById("mail_smtpauth_req");
      document.getElementById("smtp_auth1").style.display = smtpauth_req.checked ? "" : "none";
      document.getElementById("smtp_auth2").style.display = smtpauth_req.checked ? "" : "none";
    },

    smtp_setDefaultSMTPPort: function () {
      useSSLPort = !document.getElementById("mail_smtpssl").options[0].selected;

      if (useSSLPort && document.getElementById("mail_smtpport").value == '25') {
        document.getElementById("mail_smtpport").value = '465';
      }
      if (!useSSLPort && document.getElementById("mail_smtpport").value == '465') {
        document.getElementById("mail_smtpport").value = '25';
      }
    },

    /**
     * Changes the display used in the outbound email SMTP dialog to match the
     */
    changeEmailScreenDisplay: function (smtptype, isSystemAccount) {
      document.getElementById("smtpButtonGroupTD").style.display = '';
      document.getElementById("chooseEmailProviderTD").style.display = '';
      document.getElementById("mailsettings1").style.display = '';
      document.getElementById("mailsettings2").style.display = '';
      document.getElementById("mail_smtppass_label").innerHTML = mod_strings.LBL_MAIL_SMTPPASS;
      document.getElementById("mail_smtpport_label").innerHTML = mod_strings.LBL_MAIL_SMTPPORT;
      document.getElementById("mail_smtpserver_label").innerHTML = mod_strings.LBL_MAIL_SMTPSERVER;
      document.getElementById("mail_smtpuser_label").innerHTML = mod_strings.LBL_MAIL_SMTPUSER;

      switch (smtptype) {
        case "yahoomail":
          document.getElementById("mail_smtpserver").value = 'smtp.mail.yahoo.com';
          document.getElementById("mail_smtpport").value = '465';
          document.getElementById("mail_smtpauth_req").checked = true;
          var ssl = document.getElementById("mail_smtpssl");
          for (var j = 0; j < ssl.options.length; j++) {
            if (ssl.options[j].text == 'SSL') {
              ssl.options[j].selected = true;
              break;
            }
          }
          document.getElementById("mailsettings1").style.display = 'none';
          document.getElementById("mailsettings2").style.display = 'none';
          document.getElementById("mail_smtppass_label").innerHTML =
            document.getElementById("mail_smtppass_label").innerHTML = mod_strings.LBL_YAHOOMAIL_SMTPPASS;
          document.getElementById("mail_smtpuser_label").innerHTML = mod_strings.LBL_YAHOOMAIL_SMTPUSER;
          break;
        case "gmail":
          if (document.getElementById("mail_smtpserver").value == "" || document.getElementById("mail_smtpserver").value == 'smtp.mail.yahoo.com') {
            document.getElementById("mail_smtpserver").value = 'smtp.gmail.com';
            document.getElementById("mail_smtpport").value = '587';
            document.getElementById("mail_smtpauth_req").checked = true;
            var ssl = document.getElementById("mail_smtpssl");
            for (var j = 0; j < ssl.options.length; j++) {
              if (ssl.options[j].text == 'TLS') {
                ssl.options[j].selected = true;
                break;
              }
            }
          }
          //document.getElementById("mailsettings1").style.display = 'none';
          //document.getElementById("mailsettings2").style.display = 'none';
          document.getElementById("mail_smtppass_label").innerHTML = mod_strings.LBL_GMAIL_SMTPPASS;
          document.getElementById("mail_smtpuser_label").innerHTML = mod_strings.LBL_GMAIL_SMTPUSER;
          break;
        case "exchange":
          if (document.getElementById("mail_smtpserver").value == 'smtp.mail.yahoo.com'
            || document.getElementById("mail_smtpserver").value == 'smtp.gmail.com') {
            document.getElementById("mail_smtpserver").value = '';
          }
          document.getElementById("mail_smtpport").value = '25';
          document.getElementById("mail_smtpauth_req").checked = true;
          document.getElementById("mailsettings1").style.display = '';
          document.getElementById("mailsettings2").style.display = '';
          document.getElementById("mail_smtppass_label").innerHTML = mod_strings.LBL_EXCHANGE_SMTPPASS;
          document.getElementById("mail_smtpport_label").innerHTML = mod_strings.LBL_EXCHANGE_SMTPPORT;
          document.getElementById("mail_smtpserver_label").innerHTML = mod_strings.LBL_EXCHANGE_SMTPSERVER;
          document.getElementById("mail_smtpuser_label").innerHTML = mod_strings.LBL_EXCHANGE_SMTPUSER;
          break;
      }
      if ((typeof isSystemAccount != 'undefined') && isSystemAccount) {
        document.getElementById("smtpButtonGroupTD").style.display = 'none';
        document.getElementById("chooseEmailProviderTD").style.display = 'none';
        document.getElementById("mailsettings2").style.display = 'none';
      }

      SUGAR.email2.accounts.smtp_authenticate_field_display();
      SUGAR.email2.accounts.smtp_setDefaultSMTPPort()
    },

    /**
     * Fill the gmail default values for inbound accounts.
     */
    fillInboundGmailDefaults: function () {

      document.forms['ieAccount'].elements['server_url'].value = "imap.gmail.com";
      document.forms['ieAccount'].elements['ssl'].checked = true;
      document.forms['ieAccount'].elements['protocol'].value = "imap";
      SUGAR.email2.accounts.setPortDefault();
      SUGAR.util.setEmailPasswordDisplay('email_password', false);
    },
    /**
     * Sets Port field to selected protocol and SSL settings defaults
     */
    setPortDefault: function () {
      var prot = document.getElementById('protocol');
      var ssl = document.getElementById('ssl');
      var port = document.getElementById('port');
      var stdPorts = new Array("110", "143", "993", "995");
      var stdBool = new Boolean(false);
      var mailboxdiv = document.getElementById("mailboxdiv");
      var trashFolderdiv = document.getElementById("trashFolderdiv");
      var sentFolderdiv = document.getElementById("sentFolderdiv");
      var monitoredFolder = document.getElementById("subscribeFolderButton");
      if (port.value == '') {
        stdBool.value = true;
      } else {
        for (i = 0; i < stdPorts.length; i++) {
          if (stdPorts[i] == port.value) {
            stdBool.value = true;
          }
        }
      }

      if (stdBool.value == true) {
        if (prot.value == 'imap' && ssl.checked == false) { // IMAP
          port.value = "143";
        } else if (prot.value == 'imap' && ssl.checked == true) { // IMAP-SSL
          port.value = '993';
        } else if (prot.value == 'pop3' && ssl.checked == false) { // POP3
          port.value = '110';
        } else if (prot.value == 'pop3' && ssl.checked == true) { // POP3-SSL
          port.value = '995';
        }
      }

      if (prot.value == 'imap') {
        mailboxdiv.style.display = "";
        trashFolderdiv.style.display = "";
        sentFolderdiv.style.display = "";
        monitoredFolder.style.display = "";
        if (document.getElementById('mailbox').value == "") {
          document.getElementById('mailbox').value = "INBOX";
        }
      } else {
        mailboxdiv.style.display = "none";
        trashFolderdiv.style.display = "none";
        sentFolderdiv.style.display = "none";
        monitoredFolder.style.display = "none";
        document.getElementById('mailbox').value = "";
      } // else
    },

    /**
     * Draws/removes red boxes around required fields.
     */
    ieAccountError: function (style) {
      document.getElementById('server_url').className = style;
      document.getElementById('email_user').className = style;
      document.getElementById('email_password').className = style;
      document.getElementById('protocol').className = style;
      document.getElementById('port').className = style;
    },

    checkOutBoundSelection: function () {
      var select = Dom.get('outbound_email');
      if (!select || select.selectedIndex == -1) {
        return;
      }

      var v = select.options[select.selectedIndex].value;

      if (v == '') {
        select.options[select.selectedIndex].selected = false;
        v = select.options[0].value;
      }
      else if (v == 'SYSTEM_ADD')
        SUGAR.email2.accounts.showAddSmtp();

      var foundError = false;
      var errorAccounts = SUGAR.email2.composeLayout.outboundAccountErrors;
      for (i in errorAccounts) {
        if (v == i) {
          foundError = true;
          break;
        }
      }

      //Should username/password fields for outbound account.
      if (foundError)
        this.toggleInboundOutboundFields(true);
      else
        this.toggleInboundOutboundFields(false);


    },
    toggleInboundOutboundFields: function (display) {
      if (display) {
        Dom.removeClass("inboundAccountRequiredUsername", "yui-hidden");
        Dom.removeClass("inboundAccountRequiredPassword", "yui-hidden");
      }
      else {
        Dom.addClass("inboundAccountRequiredUsername", "yui-hidden");
        Dom.addClass("inboundAccountRequiredPassword", "yui-hidden");
      }
    },
    /**
     * rebuilds the select options for mailer options
     */
    rebuildMailerOptions: function () {
      var select = document.forms['ieAccount'].elements['outbound_email'];
      SE.util.emptySelectOptions(select);

      //Get the available sugar mailers
      var a_outbound = SE.accounts.outboundAccountsSettingsTable.getRecordSet().getRecords();

      for (i = 0; i < a_outbound.length; i++) {
        var t_record = a_outbound[i];
        var key = t_record.getData('id');
        var display = t_record.getData('name') + ' - ' + t_record.getData('mail_smtpserver');

        var opt = new Option(display, key);
        select.options.add(opt);
        if (key == SE.accounts.newAddedOutboundId) {
          select.options.selectedIndex = i;
        }
      }

      select.options.add(new Option('', ''));
      select.options.add(new Option(mod_strings.LBL_ADD_OUTBOUND_ACCOUNT, 'SYSTEM_ADD'));
      //Hide/Show username password fields if necessary.
      SE.accounts.checkOutBoundSelection();
    },
    /**
     * Empties all the fields in the accounts edit view
     */
    clearInboundAccountEditScreen: function () {

      document.getElementById('ie_id').value = '';
      document.getElementById('reply_to_addr').value = '';
      document.getElementById('server_url').value = '';
      document.getElementById('email_user').value = '';
      document.getElementById('email_password').value = '';
      document.getElementById('port').value = '';
      document.getElementById('inbound_mail_smtpuser').value = '';
      document.getElementById('inbound_mail_smtppass').value = '';
      document.ieAccount.protocol.options[0].selected = true;
      // handle SSL
      document.getElementById('ssl').checked = false;
      SUGAR.util.setEmailPasswordDisplay('email_password', false);
    },

    /**
     * Populates an account's fields in Settings->Accounts
     */
    fillIeAccount: function (jsonstr) {
      var o = YAHOO.lang.JSON.parse(jsonstr);

      document.getElementById('ie_id').value = o.id;
      document.getElementById('ie_name').value = o.name;
      if (o.stored_options != null) {
        if (document.getElementById('ie_from_name')) {
          document.getElementById('ie_from_name').value =
            o.stored_options.from_name == 'undefined' ? '' : o.stored_options.from_name;
        }
        if (document.getElementById('ie_from_addr')) {
          document.getElementById('ie_from_addr').value =
            o.stored_options.from_addr == 'undefined' ? '' : o.stored_options.from_addr;
        }
        if (document.getElementById('reply_to_addr')) {
          document.getElementById('reply_to_addr').value =
            typeof(o.stored_options.reply_to_addr) == 'undefined' ? '' : o.stored_options.reply_to_addr;
        }
        if (o.stored_options.trashFolder != null) {
          document.getElementById('trashFolder').value = o.stored_options.trashFolder;
        }
        if (o.stored_options.sentFolder != null) {
          document.getElementById('sentFolder').value = o.stored_options.sentFolder;
        }
      }
      document.getElementById('server_url').value = o.server_url;
      document.getElementById('email_user').value = o.email_user;
      document.getElementById('port').value = o.port;
      document.getElementById('group_id').value = o.group_id;
      document.getElementById('mailbox').value = o.mailbox;


      if (typeof o.email_account_signatures !== "undefined") {
        jQuery('#account_signature_id').replaceWith(o.email_account_signatures);
      }
      $('#account_signature_id').val(o.email_signatures);

      var i = 0;

      // handle SSL
      if (typeof(o.service[2]) != 'undefined') {
        document.getElementById('ssl').checked = true;
      }

      // handle protocol
      if (document.getElementById('protocol').value != o.protocol) {
        var prot = document.getElementById('protocol');
        for (i = 0; i < prot.options.length; i++) {
          if (prot.options[i].value == o.service[3]) {
            prot.options[i].selected = true;
            this.setPortDefault();
          }
        }
      }
      // handle SMTP selection
      if (o.stored_options != null && typeof(o.stored_options.outbound_email) != 'undefined') {
        var opts = document.getElementById('outbound_email').options;
        for (i = 0; i < opts.length; i++) {
          if (opts[i].value == o.stored_options.outbound_email) {
            opts[i].selected = true;
          }
        }
      }
    },

    deleteIeAccount: function (IeAccountID, IeGroupID) {
      if (confirm(app_strings.LBL_EMAIL_IE_DELETE_CONFIRM)) {
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_IE_DELETE, app_strings.LBL_EMAIL_ONE_MOMENT);

        AjaxObject.target = 'frameFlex';
        AjaxObject.startRequest(callbackAccountDelete, urlStandard + '&emailUIAction=deleteIeAccount&ie_id=' + IeAccountID + '&group_id=' + IeGroupID);
        SUGAR.email2.accounts.refreshInboundAccountTable();
      }
    },

    // Null check for Outbound Settings.
    checkOutboundSettings: function () {
      var errorMessage = '';
      var isError = false;
      if (typeof document.forms['outboundEmailForm'] != 'undefined') {
        var mailName = document.getElementById('mail_name').value;
        var smtpServer = document.getElementById('mail_smtpserver').value;
        var smtpPort = document.getElementById('mail_smtpport').value;

        var mailsmtpauthreq = document.getElementById('mail_smtpauth_req');
        if (trim(mailName) == '') {
          isError = true;
          errorMessage += app_strings.LBL_EMAIL_ACCOUNTS_NAME + "<br/>";
        }
        if (trim(smtpServer) == '') {
          isError = true;
          errorMessage += app_strings.LBL_EMAIL_ACCOUNTS_SMTPSERVER + "<br/>";
        }
        if (trim(smtpPort) == '') {
          isError = true;
          errorMessage += app_strings.LBL_EMAIL_ACCOUNTS_SMTPPORT + "<br/>";
        }
        if (mailsmtpauthreq.checked) {
          if (trim(document.getElementById('mail_smtpuser').value) == '') {
            isError = true;
            errorMessage += app_strings.LBL_EMAIL_ACCOUNTS_SMTPUSER + "<br/>";
          }
        }
      }
      if (isError) {
        SUGAR.showMessageBox(mod_strings.ERR_MISSING_REQUIRED_FIELDS, errorMessage, 'alert');
        return false;
      } else {
        return true;
      }
    },

    testOutboundSettings: function () {
      var errorMessage = '';
      var isError = false;
      var fromAddress = document.getElementById("outboundtest_from_address").value;
      if (trim(fromAddress) == "") {
        errorMessage += app_strings.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR + "<br/>";
        SUGAR.showMessageBox(mod_strings.ERR_MISSING_REQUIRED_FIELDS, errorMessage, 'alert');
        return false;

      }
      else if (!isValidEmail(fromAddress)) {
        errorMessage += app_strings.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR + "<br/>";
        SUGAR.showMessageBox(mod_strings.ERR_INVALID_REQUIRED_FIELDS, errorMessage, 'alert');
        return false;
      }

      //Hide the dialogue and show an in progress indicator.
      SE.accounts.testOutboundDialog.hide();
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_PERFORMING_TASK, app_strings.LBL_EMAIL_ONE_MOMENT, 'plain');

      //If the outbound mail type is a system override we need to re-enable the post fields otherwise
      //nothing is sent in the request.
      var outboundType = document.forms['outboundEmailForm'].elements['type'].value;
      SUGAR.email2.accounts.toggleOutboundAccountDisabledFields(false);

      YAHOO.util.Connect.setForm(document.getElementById("outboundEmailForm"));
      if (outboundType == 'system-override')
        SUGAR.email2.accounts.toggleOutboundAccountDisabledFields(true);

      var data = "&emailUIAction=testOutbound&outboundtest_from_address=" + fromAddress;
      AjaxObject.startRequest(callbackOutboundTest, urlStandard + data);

    },

    testOutboundSettingsDialog: function () {
      //Ensure that all settings are correct before proceeding to send test email.
      if (!SE.accounts.checkOutboundSettings())
        return;

      // lazy load dialogue
      if (!SE.accounts.testOutboundDialog) {
        SE.accounts.testOutboundDialog = new YAHOO.widget.Dialog("testOutboundDialog", {
          modal: true,
          visible: true,
          fixedcenter: true,
          constraintoviewport: true,
          width: 600,
          shadow: true
        });
        SE.accounts.testOutboundDialog.setHeader(app_strings.LBL_EMAIL_TEST_OUTBOUND_SETTINGS);
        Dom.removeClass("testOutboundDialog", "yui-hidden");
      } // end lazy load
      SE.accounts.testOutboundDialog.render();
      SE.accounts.testOutboundDialog.show();
    },

    /**
     * Saves Outbound email settings
     */
    saveOutboundSettings: function () {
      if (SE.accounts.checkOutboundSettings()) {
        //Enable the form fields for the post.
        SUGAR.email2.accounts.toggleOutboundAccountDisabledFields(false);
        YAHOO.util.Connect.setForm(document.getElementById("outboundEmailForm"));
        AjaxObject.startRequest(callbackOutboundSave, urlStandard + "&emailUIAction=saveOutbound");
      } else {
        return false;
      }
    },

    saveIeAccount: function (user) {

      //Before saving check if there are any error messages associated with the outbound account.
      var outboundID = document.getElementById('outbound_email').value;

      if (SE.accounts.checkIeCreds({
          'valiateTrash': true, 'validateFromAddr': true, 'validateOutbound': true,
          'validateSMTPCreds': true
        })) {
        document.getElementById('saveButton').disabled = true;

        SUGAR.showMessageBox(app_strings.LBL_EMAIL_IE_SAVE, app_strings.LBL_EMAIL_ONE_MOMENT);

        var formObject = document.getElementById('ieAccount');
        YAHOO.util.Connect.setForm(formObject);

        AjaxObject._reset();
        AjaxObject.target = 'frameFlex';
        AjaxObject.startRequest(callbackAccount, urlStandard + '&emailUIAction=saveIeAccount' + (user ? '&user=' + user : ''));
      }
    },

    testSettings: function () {
      form = document.getElementById('ieAccount');

      if (SE.accounts.checkIeCreds()) {
        ie_test_open_popup_with_submit("InboundEmail", "Popup", "Popup", 400, 300, trim(form.server_url.value), form.protocol.value, trim(form.port.value), trim(form.email_user.value), Rot13.write(form.email_password.value), trim(form.mailbox.value), form.ssl.checked, true, "ieAccount", form.ie_id.value, form.ie_id.auth_type, '', '');
      }
    },

    getFoldersListForInboundAccountForEmail2: function () {
      form = document.getElementById('ieAccount');
      if (SE.accounts.checkIeCreds()) {
        var mailBoxValue = form.mailbox.value;
        if (form.searchField.value.length > 0) {
          mailBoxValue = "";
        } // if
        getFoldersListForInboundAccount("InboundEmail", "ShowInboundFoldersList", "Popup", 400, 300, form.server_url.value, form.protocol.value, form.port.value, form.email_user.value, Rot13.write(form.email_password.value), mailBoxValue, form.ssl.checked, true, form.searchField.value);
      } // if

    },

    checkIeCreds: function (validateRules) {
      if (typeof(validateRules) == 'undefined')
        validateRules = {};

      var errors = new Array();
      var out = new String();

      var ie_name = Dom.get('ie_name').value;
      var server_url = Dom.get('server_url').value;
      var email_user = Dom.get('email_user').value;
      var email_password = Dom.get('email_password').value;
      var protocol = Dom.get('protocol').value;
      var port = Dom.get('port').value;
      var oe = Dom.get('outbound_email');

      // Bug 44392: IE9 and possibly previous versions have a quirk where selectedIndex is -1 if you have nothing selected vs 0 for
      // other browsers. And if you check options[-1] it returns "unknown" instead of undefined. Also other options out of index
      // return null instead of undefined for other browsers, thus we need to check for all the possible outcomes.
      var oe_value = (typeof(oe.options[oe.selectedIndex]) === 'undefined' || typeof(oe.options[oe.selectedIndex]) === 'unknown' || typeof(oe.options[oe.selectedIndex]) === null) ? "" : oe.options[oe.selectedIndex].value;

      var outboundUserName = Dom.get('inbound_mail_smtpuser').value;
      var outboundPass = Dom.get('inbound_mail_smtppass').value;

      //If the username and password were provided then ignore the error messge

      var outboundCredentialsFound = false;

      if (outboundUserName != "" && outboundPass != "")
        outboundCredentialsFound = true;

      var validateSMTPCreds = (typeof(validateRules.validateSMTPCreds) != 'undefined' && validateRules.validateSMTPCreds);

      if (SE.composeLayout.outboundAccountErrors != null && SE.composeLayout.outboundAccountErrors[oe_value] != null
        && validateSMTPCreds) {
        if (trim(outboundUserName) == "") {
          errors.push(app_strings.LBL_EMAIL_ACCOUNTS_SMTPUSER);
        }
        if (trim(outboundPass) == "") {
          errors.push(app_strings.LBL_EMAIL_ACCOUNTS_SMTPPASS);
        }
      }

      if (trim(ie_name) == "") {
        errors.push(app_strings.LBL_EMAIL_ERROR_NAME);
      }


      if ((typeof(validateRules.validateOutbound) != 'undefined' && validateRules.validateOutbound) && ( trim(oe_value) == ""
        || trim(oe_value) == "SYSTEM_ADD")) {
        errors.push(app_strings.LBL_EMAIL_ERROR_NO_OUTBOUND);
      }
      if (trim(server_url) == "") {
        errors.push(app_strings.LBL_EMAIL_ERROR_SERVER);
      }
      if (trim(email_user) == "") {
        errors.push(app_strings.LBL_EMAIL_ERROR_USER);
      }
      if (protocol == "") {
        errors.push(app_strings.LBL_EMAIL_ERROR_PROTOCOL);
      }
      if (protocol == 'imap') {
        var mailbox = document.getElementById('mailbox').value;
        if (trim(mailbox) == "") {
          errors.push(app_strings.LBL_EMAIL_ERROR_MONITORED_FOLDER);
        } // if
        if (typeof(validateRules.valiateTrash) != 'undefined' && validateRules.valiateTrash) {
          var trashFolder = document.getElementById('trashFolder').value;
          if (trim(trashFolder) == "") {
            errors.push(app_strings.LBL_EMAIL_ERROR_TRASH_FOLDER);
          } // if
        } // if
      }
      if (port == "") {
        errors.push(app_strings.LBL_EMAIL_ERROR_PORT);
      }

      if (errors.length > 0) {
        out = app_strings.LBL_EMAIL_ERROR_DESC;
        for (i = 0; i < errors.length; i++) {
          if (out != "") {
            out += "\n";
          }
          out += errors[i];
        }

        alert(out);
        return false;
      } else {

        return true;
      }
    },

    getIeAccount: function (ieId) {
      if (ieId == '')
        return;

      SUGAR.showMessageBox(app_strings.LBL_EMAIL_SETTINGS_RETRIEVING_ACCOUNT, app_strings.LBL_EMAIL_ONE_MOMENT);
      var query = "&emailUIAction=getIeAccount&ieId=" + ieId;

      console.log(urlStandard + query);
      AjaxObject.startRequest(callbackIeAccountRetrieve, urlStandard + query);
    },

    /**
     * Iterates through TreeView nodes to apply styles dependent nature of node
     */
    renderTree: function () {
      SE.util.cascadeNodes(SE.tree.getRoot(), SE.accounts.setNodeStyle);
      SE.tree.render();
    },

    //Sets the style for any nodes that need it.
    setNodeStyle: function (node) {
      if (!node.data.origText) {
        node.data.origText = node.data.text;
      }
      //Set unread
      if (typeof(node.data.unseen) != 'undefined') {
        if (node.data.unseen > 0) {
          node.setUpLabel(node.data.origText + '(' + node.data.unseen + ')');
          // Add bold style to label, kinda hacky
          node.labelStyle += " ygtvlabelbold";
        }
        else {
          node.setUpLabel(node.data.origText);
        }
      } else {
        node.setUpLabel(node.data.origText);
      }
      SE.accounts.setupDDTarget(node);
    },

    setupDDTarget: function (node) {
      if (node.ddTarget) {
        node.ddTarget.removeFromGroup();
        delete node.ddTarget;
      }
      var id = node.getElId();
      var num = id.substring(4);
      if (node.data.origText != SUGAR.language.get("Emails", "LNK_MY_INBOX") &&
        node.data.origText != SUGAR.language.get("Emails", "LNK_MY_DRAFTS") &&
        node.data.origText != SUGAR.language.get("Emails", "LNK_SENT_EMAIL_LIST")) {

        node.ddTarget = new SUGAR.email2.folders.folderDD("ygtvcontentel" + num);
      }
      else if (node.data.origText == SUGAR.language.get("Emails", "LNK_MY_INBOX")) {
        node.ddTarget = new YAHOO.util.DDTarget("ygtvcontentel" + num);
      }
    },

    /**
     * Async call to rebuild the folder list.  After a folder delete or account delete
     */
    rebuildFolderList: function () {
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_REBUILDING_FOLDERS, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackFolders, urlStandard + '&emailUIAction=rebuildFolders');
    },

    /**
     * Returns the number of remote accounts the user has active.
     */
    getAccountCount: function () {
      var tree = SE.tree;
      var count = 0;
      for (i = 0; i < tree._nodes.length; i++) {
        var node = tree._nodes[i];

        if (typeof(node) != 'undefined' && node.data.ieId) {
          count++;
        }
      }
      return count;
    }
  };
////    END ACCOUNTS
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////    CONTEXT MENU CALLS
  SE.contextMenus = {

    assignToDialogue: null,
    /**
     * Archives from context menu
     * @param Array uids
     * @param string ieId
     * @param string folder
     */
    _archiveToSugar: function (uids, ieId, folder) {
      var ser = '';

      for (var i = 0; i < uids.length; i++) { // using 1 index b/c getSelectedRowIds doubles the first row's id
        if (ser != "") ser += app_strings.LBL_EMAIL_DELIMITER;
        ser += uids[i];
      }
      AjaxObject.startRequest(callbackImportOneEmail, urlStandard + '&emailUIAction=getImportForm&uid=' + ser + "&ieId=" + ieId + "&mbox=" + folder);
    },

    /**
     * Archives from context menu
     */
    archiveToSugar: function (menuItem) {
      SE.contextMenus.emailListContextMenu.hide();

      var rows = SE.grid.getSelectedRows();
      var uids = [];
      /* iterate through available rows JIC a row is deleted - use first available */
      for (var i = 0; i < rows.length; i++) {
        uids[i] = SE.grid.getRecord(rows[0]).getData().uid;
      }
      var data = SE.grid.getRecord(rows[0]).getData();
      SE.contextMenus._archiveToSugar(uids, data.ieId, data.mbox);
    },

    /**
     * Popup the printable version and start system's print function.
     */
    viewPrintable: function (menuItem) {
      var rows = SE.grid.getSelectedRows();
      var data = SE.grid.getRecord(rows[0]).getData();
      SE.detailView.viewPrintable(data.ieId, data.uid, data.mbox);
    },

    /**
     * Marks email flagged on mail server
     */
    markRead: function (type, contextMenuId) {
      SE.contextMenus.markEmail('read');
    },

    /**
     * Assign this emails to people based on assignment rules
     */
    assignEmailsTo: function (type, contextMenuId) {
      if (!SE.contextMenus.assignToDialogue) {
        SE.contextMenus.assignToDialogue = new YAHOO.widget.Dialog("assignToDiv", {
          modal: true,
          visible: false,
          //fixedcenter:true,
          //constraintoviewport: true,
          width: "600px",
          shadow: true
        });
        SE.contextMenus.assignToDialogue.setHeader(app_strings.LBL_EMAIL_ASSIGN_TO);
        enableQS(true);
      }

      Dom.removeClass("assignToDiv", "yui-hidden");
      SE.contextMenus.assignToDialogue.render();
      SE.contextMenus.assignToDialogue.show();
    },

    /**
     * Marks email flagged on mail server
     */
    markFlagged: function (contextMenuId) {
      SE.contextMenus.markEmail('flagged');
    },

    /**
     * Marks email unflagged on mail server
     */
    markUnflagged: function (contextMenuId) {
      SE.contextMenus.markEmail('unflagged');
    },

    /**
     * Marks email unread on mail server
     */
    markUnread: function () {
      SE.contextMenus.markEmail('unread');
    },

    /**
     * Deletes an email from context menu
     */
    markDeleted: function () {
      if (confirm(app_strings.LBL_EMAIL_DELETE_CONFIRM)) {
        document.getElementById('_blank').innerHTML = "";
        SE.contextMenus.markEmail('deleted');
      }
    },

    /**
     * generic call API to apply a flag to emails on the server and on sugar
     * @param string type "read" | "unread" | "flagged" | "deleted"
     */
    markEmail: function (type) {
      SE.contextMenus.emailListContextMenu.hide();

      //var dm = SE.grid.getStore();
      //var uids = SE.grid.getSelectedRowIds();
      //var indexes = SE.grid.getSelectedRowIndexes();
      var rows = SE.grid.getSelectedRows();
      if (rows.length == 0)
        rows = [SE.contextMenus.currentRow];
      var ser = [];

      for (var i = 0; i < rows.length; i++) {
        ser.push(SE.grid.getRecord(rows[i]).getData().uid);
      }

      ser = YAHOO.lang.JSON.stringify(ser);

      var ieId = SE.grid.getRecord(rows[0]).getData().ieId;
      var folder = SE.grid.getRecord(rows[0]).getData().mbox;


      var count = 0;


      if (type == 'read' || type == 'deleted') {
        // mark read
        for (var j = 0; j < rows.length; j++) {
          if (SE.grid.getRecord(rows[j]).getData().seen == '0') {
            count = count + 1;
            SE.grid.getRecord(rows[j]).setData("seen", "1");
          }
        }
        //bug# 40257 - adding if condition to check the ieId (Id of a sugar mail box) , which would be null for search email results
        if (ieId) {
          var node = SE.folders.getNodeFromIeIdAndMailbox(ieId, folder);
          var unseenCount = node.data.unseen;
          if (isNaN(unseenCount)) {
            unseenCount = 0;
          }
          var finalCount = parseInt(unseenCount) - count;
          node.data.unseen = finalCount;

          SE.accounts.renderTree();
        }
      } else if (type == 'unread') {
        // mark unread
        for (var j = 0; j < rows.length; j++) {
          if (SE.grid.getRecord(rows[j]).getData().seen == '1') { // index [9] is the seen flag
            count = count + 1;
          }
        }

        var node = SE.folders.getNodeFromIeIdAndMailbox(ieId, folder);
        var unseenCount = node.data.unseen;
        if (isNaN(unseenCount)) {
          unseenCount = 0;
        }
        var finalCount = parseInt(unseenCount) + count;
        node.data.unseen = finalCount;
        SE.accounts.renderTree();
      }

      if (type == 'unread') {
        for (var i = 0; i < rows.length; i++) {
          SE.cache[folder + SE.grid.getRecord(rows[i]).getData().uid] = null;
        } // for
      }

      SUGAR.showMessageBox(app_strings.LBL_EMAIL_PERFORMING_TASK, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackContextmenus.markUnread, urlStandard + '&emailUIAction=markEmail&type=' + type + '&uids=' + ser + "&ieId=" + ieId + "&folder=" + folder);
    },

    /**
     * refreshes the ListView to show changes to cache
     */
    markEmailCleanup: function () {
      SE.accounts.renderTree();
      SUGAR.hideMessageBox();
      SE.listView.refreshGrid();
    },

    showAssignmentDialog: function () {
      if (SE.contextMenus.assignmentDialog == null) {
        AjaxObject.startRequest(callbackAssignmentDialog, urlStandard + '&emailUIAction=getAssignmentDialogContent');
      } else {
        SE.contextMenus.assignmentDialog.show();
      } // else
    },

    /**
     * shows the import dialog with only relate visible.
     */
    relateTo: function () {
      SE.contextMenus.emailListContextMenu.hide();

      var rows = SE.grid.getSelectedRows();
      var data = SE.grid.getRecord(rows[0]).getData();
      var ieId = data.ieId;
      var folder = data.mbox;
      var uids = [];
      /* iterate through available rows JIC a row is deleted - use first available */
      for (var i = 0; i < rows.length; i++) {
        uids[i] = SE.grid.getRecord(rows[i]).getData().uid;
      }
      var ser = YAHOO.lang.JSON.stringify(uids);

      AjaxObject.startRequest(callbackRelateEmail, urlStandard + '&emailUIAction=getRelateForm&uid=' + ser + "&ieId=" + ieId + "&mbox=" + folder);
    },

    /**
     * shows the import dialog with only relate visible.
     */
    showDetailView: function () {
      SE.contextMenus.emailListContextMenu.hide();
      var rows = SE.grid.getSelectedRows();
      if (rows.length > 1) {
        alert(app_strings.LBL_EMAIL_SELECT_ONE_RECORD);
        return;
      }
      var ieId = SE.grid.getRecord(rows[0]).getData().ieId;
      var folder = SE.grid.getRecord(rows[0]).getData().mbox;
      /* iterate through available rows JIC a row is deleted - use first available */
      var uid = SE.grid.getRecord(rows[0]).getData().uid;
      SE.contextMenus.showEmailDetailViewInPopup(ieId, uid, folder);
    },

    /**
     *
     */
    showEmailDetailViewInPopup: function (ieId, uid, folder) {
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_RETRIEVING_RECORD, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackEmailDetailView, urlStandard + '&emailUIAction=getEmail2DetailView&uid=' + uid + "&ieId=" + ieId + "&mbox=" + folder + "&record=" + uid);
    },

    /**
     * Opens multiple messages from ListView context click
     */
    openMultiple: function () {
      SE.contextMenus.emailListContextMenu.hide();

      var rows = SE.grid.getSelectedRows();
      var uids = SE.listView.getUidsFromSelection();

      if (uids.length > 0) {
        var mbox = SE.grid.getRecord(rows[0]).getData().mbox;
        var ieId = SE.grid.getRecord(rows[0]).getData().ieId;
        SE.detailView.populateDetailViewMultiple(uids, mbox, ieId, true);
      }
    },

    /**
     * Replies/forwards email
     */
    replyForwardEmailContext: function () {
      SE.contextMenus.emailListContextMenu.hide();

      var indexes = SE.grid.getSelectedRows();
      //var dm = SE.grid.getDataModel();
      var type = this.id;

      for (var i = 0; i < indexes.length; i++) {
        var row = SE.grid.getRecord(indexes[i]).getData();
        SE.composeLayout.c0_replyForwardEmail(row.ieId, row.uid, row.mbox, type);
      }
    },

    //show menu functions
    showEmailsListMenu: function (grid, row) {

      var data = row.getData();
      var draft = (data.type == "draft");
      var menu = SE.contextMenus.emailListContextMenu;
      var folderNode;

      if (SE.tree) {
        if (data.mbox == 'sugar::Emails')
          folderNode = SE.folders.getNodeFromIeIdAndMailbox('folder', data.ieId);
        else
          folderNode = SE.folders.getNodeFromIeIdAndMailbox(data.ieId, data.mbox);

        if (folderNode != null && typeof(folderNode) != "undefined" && typeof(folderNode.data) != "undefined"
          && ((folderNode.data.is_group != null) && (folderNode.data.is_group == 'true'))
          || (folderNode != null && folderNode.data.isGroup != null && folderNode.data.isGroup == "true"))
          menu.getItem(menu.itemsMapping.assignTo).cfg.setProperty("disabled", false); //Assign emails item
        else
          menu.getItem(menu.itemsMapping.assignTo).cfg.setProperty("disabled", true); //Assign emails item
      }
      else
        menu.getItem(menu.itemsMapping.assignTo).cfg.setProperty("disabled", true);

      menu.getItem(menu.itemsMapping.archive).cfg.setProperty("disabled", draft);
      menu.getItem(menu.itemsMapping.reply).cfg.setProperty("disabled", draft);
      menu.getItem(menu.itemsMapping.replyAll).cfg.setProperty("disabled", draft);
      menu.getItem(menu.itemsMapping.forward).cfg.setProperty("disabled", draft);
      menu.getItem(menu.itemsMapping.mark).cfg.setProperty("disabled", draft);


      if (data.mbox == "sugar::Emails") {
        //Allow users to reassign imported emails
        menu.getItem(menu.itemsMapping.assignTo).cfg.setProperty("disabled", false);
        menu.getItem(menu.itemsMapping.archive).cfg.setProperty("disabled", true);
        menu.getItem(menu.itemsMapping.viewRelationships).cfg.setProperty("disabled", false);
        menu.getItem(menu.itemsMapping.relateTo).cfg.setProperty("disabled", false);
      }
      else {
        menu.getItem(menu.itemsMapping.viewRelationships).cfg.setProperty("disabled", true);
        menu.getItem(menu.itemsMapping.relateTo).cfg.setProperty("disabled", true);
      }
    },

    showFolderMenu: function (grid, rowIndex, event) {
      event.stopEvent();
      var coords = event.getXY();
      SE.contextMenus.emailListContextMenu.showAt([coords[0], coords[1]]);
    }
  };

  SE.contextMenus.dv = {
    archiveToSugar: function (contextMenuId) {

      SE.contextMenus._archiveToSugar(uids, ieId, folder);
    },

    replyForwardEmailContext: function (all) {
      SE.contextMenus.detailViewContextMenu.hide();
    }

  };


////    END SE.contextMenus
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////    DETAIL VIEW
  SE.detailView = {
    consumeMetaDetail: function (ret) {
      // handling if the Email drafts
      if (ret.type == 'draft') {
        SE.composeLayout.c0_composeDraft();
        return;
      }


      // cache contents browser-side
      SE._setDetailCache(ret);

      var displayTemplate = new YAHOO.SUGAR.Template(SE.templates['displayOneEmail']);
      // 2 below must be in global context
      meta = ret.meta;
      meta['panelId'] = SE.util.getPanelId();

      email = ret.meta.email;
      var out = displayTemplate.exec({
        'app_strings': app_strings,
        'theme': theme,
        'idx': targetDiv.id,
        'meta': meta,
        'email': meta.email,
        'linkBeans': linkBeans
      });
      var tabLabel = meta.email.name;
      if (tabLabel != null && tabLabel.length > 25) {
        tabLabel = tabLabel.substring(0, 25) + "...";
      } // if
      targetDiv.set("label", tabLabel);
      targetDiv.set("content", out);

      var displayEmailFrameDiv = document.getElementById('displayEmailFrameDiv' + targetDiv.id);
      if (SUGAR.email2.util.isIe()) {
        displayEmailFrameDiv.style.height = "390px";
      } else {
        displayEmailFrameDiv.style.height = "410px";
      }

      var displayFrame = document.getElementById('displayEmailFrame' + targetDiv.id);
      displayFrame.contentWindow.document.write(email.description);
      displayFrame.contentWindow.document.close();

      // hide archive links
      if (ret.meta.is_sugarEmail) {
        document.getElementById("archiveEmail" + targetDiv.id).style.display = "none";
        document.getElementById("btnEmailView" + targetDiv.id).style.display = "none";
      } else {
        if (document.getElementById("showDeialViewForEmail" + targetDiv.id))
          document.getElementById("showDeialViewForEmail" + targetDiv.id).style.display = "none";
      } // else

    },

    consumeMetaPreview: function (ret) {
      // cache contents browser-side
      SE._setDetailCache(ret);


      var currrow = SE.grid.getLastSelectedRecord();
      currrow = SE.grid.getRecord(currrow);
      if (!currrow) {
        document.getElementById('_blank').innerHTML = '';
        return;
      }
      // handling if the Email drafts
      if (ret.type == 'draft') {
        if (currrow.getData().uid == ret.uid) {
          SE.composeLayout.c0_composeDraft();
        }
        return;
      }

      if (currrow.getData().uid != ret.meta.uid) {
        return;
      }

      // remove loading sprite
      document.getElementById('_blank').innerHTML = '<iframe id="displayEmailFramePreview"/>';
      var displayTemplate = new YAHOO.SUGAR.Template(SE.templates['displayOneEmail']);
      meta = ret.meta;
      meta['panelId'] = SE.util.getPanelId();
      email = ret.meta.email;

      document.getElementById('_blank').innerHTML = displayTemplate.exec({
        'app_strings': app_strings,
        'theme': theme,
        'idx': 'Preview',
        'meta': meta,
        'email': meta.email,
        'linkBeans': linkBeans
      });
      // document.getElementById('_blank').innerHTML = meta.email;
      /* displayTemplate.append('_blank', {
       'app_strings' : app_strings,
       'theme' : theme,
       'idx' : 'Preview',
       'meta' : meta,
       'email' :meta.email,
       'linkBeans' : linkBeans
       });*/

      var displayFrame = document.getElementById('displayEmailFramePreview');
      displayFrame.contentWindow.document.write(email.description);
      displayFrame.contentWindow.document.close();

      SE.listViewLayout.resizePreview();

      // hide archive links
      if (ret.meta.is_sugarEmail) {
        document.getElementById("archiveEmailPreview").innerHTML = "&nbsp;";
        document.getElementById("btnEmailViewPreview").style.display = "none";
        document.getElementById("archiveEmail" + meta['panelId']).style.display = "none";
      } else {
        //hide view relationship link
        document.getElementById("showDeialViewForEmail" + meta['panelId']).style.display = "none";
      }
    },

    /**
     * wraps emailDelete() for single messages, comes from preview or tab
     */
    emailDeleteSingle: function (ieId, uid, mbox) {
      if (confirm(app_strings.LBL_EMAIL_DELETE_CONFIRM)) {
        // find active panel and close if the user double clicked the email to view.
        var activeTabId = SE.util.getPanelId();
        if (activeTabId != 'Preview')
          SE.innerLayout.get("activeTab").close();

        document.getElementById('_blank').innerHTML = "";
        var ser = [];
        ser.push(uid);
        uid = YAHOO.lang.JSON.stringify(ser);
        this.emailDelete(ieId, uid, mbox);
      }
    },

    /**
     * Sends async call to delete a given message
     * @param
     */
    emailDelete: function (ieId, uid, mbox) {
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_DELETING_MESSAGE, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackContextmenus.markUnread, urlStandard + '&emailUIAction=markEmail&type=deleted&uids=' +
        uid + "&ieId=" + ieId + "&folder=" + mbox);
    },

    /**
     * retrieves one email to display in the preview pane.
     */
    getEmailPreview: function () {
      var row = SUGAR.email2.listView.currentRow;
      var data = row.getData();
      if (data && !(SUGAR.email2.contextMenus.emailListContextMenu.cfg.getProperty("visible") && data.type == 'draft')) {
        var setRead = (data['seen'] == 0) ? true : false;
        SUGAR.email2.listView.markRead(SUGAR.email2.listView.currentRowIndex, row);
        SUGAR.email2.detailView.populateDetailView(data['uid'], data['mbox'], data['ieId'], setRead, SUGAR.email2.previewLayout);
      }
    },

    /**
     * Imports one email into Sugar
     */
    importEmail: function (ieId, uid, mbox) {
      SE.util.clearHiddenFieldValues('emailUIForm');

      SUGAR.showMessageBox(app_strings.LBL_EMAIL_IMPORTING_EMAIL, app_strings.LBL_EMAIL_ONE_MOMENT);

      var vars = "&ieId=" + ieId + "&uid=" + uid + "&mbox=" + mbox;
      AjaxObject.target = '';
      AjaxObject.startRequest(callbackImportOneEmail, urlStandard + '&emailUIAction=getImportForm' + vars);
    },

    /**
     * Populates the frameFlex div with the contents of an email
     */
    populateDetailView: function (uid, mbox, ieId, setRead, destination) {
      SUGAR.email2.util.clearHiddenFieldValues('emailUIForm');

      var mboxStr = new String(mbox);
      var compKey = mbox + uid;

      if (setRead == true) {
        SE.listView.boldUnreadRows()
        SE.folders.decrementUnreadCount(ieId, mbox, 1);
      }

      if (destination == SE.innerLayout) {
        /*
         * loading email into a tab, peer with ListView
         * targetDiv must remain in the global namespace as it is used by AjaxObject
         */
        //Check if we already have a tab of the email open
        var tabs = SE.innerLayout.get("tabs");
        for (var t in tabs) {
          if (tabs[t].id && tabs[t].id == uid) {
            SE.innerLayout.set("activeTab", tabs[t]);
            return;
          }
        }

        targetDiv = new YAHOO.SUGAR.ClosableTab({
          label: loadingSprite,
          scroll: true,
          content: "",
          active: true
        }, SE.innerLayout);
        targetDiv.id = uid;
        SE.innerLayout.addTab(targetDiv);

        // use cache if available
        if (SE.cache[compKey]) {
          SE.detailView.consumeMetaDetail(SE.cache[compKey]);
        } else {
          // open email as peer-tab to listView
          SE.detailView.requestEmailContents(mboxStr, uid, mbox, ieId, AjaxObject.detailView.callback.emailDetail);
          // AjaxObject.startRequest(AjaxObject.detailView.callback.emailDetail, null);
        }
      } else {
        // loading email into preview pane
        document.getElementById('_blank').innerHTML = loadingSprite;

        // use cache if available
        if (SE.cache[compKey]) {
          SE.detailView.consumeMetaPreview(SE.cache[compKey]);
        } else {
          AjaxObject.forceAbort = true;
          // open in preview window
          SE.detailView.requestEmailContents(mboxStr, uid, mbox, ieId, AjaxObject.detailView.callback.emailPreview);
          // AjaxObject.startRequest(AjaxObject.detailView.callback.emailPreview, null);
        }
      }
    },

    requestEmailContents: function (mboxStr, uid, mbox, ieId, callback) {
      if (mboxStr.substring(0, 7) == 'sugar::') {
        // display an email from Sugar
        document.getElementById('emailUIAction').value = 'getSingleMessageFromSugar';
      } else {
        // display an email from an email server
        document.getElementById('emailUIAction').value = 'getSingleMessage';
      }
      document.getElementById('mbox').value = mbox;
      document.getElementById('ieId').value = ieId;
      document.getElementById('uid').value = uid;

      YAHOO.util.Connect.setForm(document.getElementById('emailUIForm'));

      AjaxObject.forceAbort = true;
      AjaxObject.target = '_blank';
      AjaxObject.startRequest(callback, null);
    },

    /**
     * Retrieves multiple emails for DetailView
     */
    populateDetailViewMultiple: function (uids, mbox, ieId, setRead) {
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_RETRIEVING_MESSAGE, app_strings.LBL_EMAIL_ONE_MOMENT);
      SE.util.clearHiddenFieldValues('emailUIForm');

      var mboxStr = new String(mbox);

      uids = SE.util.cleanUids(uids);

      if (mboxStr.substring(0, 7) == 'sugar::') {
        // display an email from Sugar
        document.getElementById('emailUIAction').value = 'getMultipleMessagesFromSugar';
        document.getElementById('uid').value = uids;
      } else {
        // display an email from an email server
        document.getElementById('emailUIAction').value = 'getMultipleMessages';
        document.getElementById('mbox').value = mbox;
        document.getElementById('ieId').value = ieId;
        document.getElementById('uid').value = uids;
      }

      var formObject = document.getElementById('emailUIForm');
      YAHOO.util.Connect.setForm(formObject);

      AjaxObject.target = 'frameFlex';
      AjaxObject.startRequest(callbackEmailDetailMultiple, null);

      if (setRead == true) {
        var c = uids.split(",");
        SE.folders.decrementUnreadCount(ieId, mbox, c.length);
      }
    },

    /**
     * Makes async call to get QuickCreate form
     * Renders a modal edit view for a given module
     */
    quickCreate: function (module, ieId, uid, mailbox) {
      var get = "&qc_module=" + module + "&ieId=" + ieId + "&uid=" + uid + "&mailbox=" + mailbox;

      if (ieId == null || ieId == "null" || mailbox == 'sugar::Emails') {
        get += "&sugarEmail=true";
      }

      AjaxObject.startRequest(callbackQuickCreate, urlStandard + '&emailUIAction=getQuickCreateForm' + get);
    },

    /**
     * Makes async call to save a quick create
     * @param bool
     */
    saveQuickCreate: function (action) {
      var qcd = SE.detailView.quickCreateDialog;
      if (check_form('form_EmailQCView_' + qcd.qcmodule)) {
        var formObject = document.getElementById('form_EmailQCView_' + qcd.qcmodule);
        var theCallback = callbackQuickCreateSave;
        var accountType = '&sugarEmail=true';
        if (qcd.ieId != 'null' && qcd.mbox != 'sugar::Emails') {
          accountType = '&ieId=' + qcd.ieId;
        }

        if (action == 'reply') {
          theCallback = callbackQuickCreateSaveAndReply;
        } else if (action == true) {
          theCallback = callbackQuickCreateSaveAndAddToAddressBook;
        }
        formObject.action.value = 'EmailUIAjax';
        YAHOO.util.Connect.setForm(formObject);
        SUGAR.showMessageBox('Saving', app_strings.LBL_EMAIL_ONE_MOMENT);
        AjaxObject.startRequest(theCallback, "to_pdf=true&emailUIAction=saveQuickCreate&qcmodule=" + qcd.qcmodule + '&uid=' + qcd.uid +
          accountType + '&mbox=' + qcd.mbox);
      }
    },

    /**
     * Code to show/hide long list of email address in DetailView
     */
    showCroppedEmailList: function (el) {
      el.style.display = 'none';
      el.previousSibling.style.display = 'inline'
    },
    showFullEmailList: function (el) {
      el.style.display = 'none';
      el.nextSibling.style.display = 'inline';
    },

    /**
     * Shows the QuickCreate overlay
     * @param string ieId
     * @param string uid
     * @param string mailbox
     */
    showQuickCreate: function (ieId, uid, mailbox) {
      var panelId = SE.util.getPanelId();
      var context = document.getElementById("quickCreateSpan" + panelId);

      if (!SE.detailView.cqMenus)
        SE.detailView.cqMenus = {};

      if (SE.detailView.cqMenus[context])
        SE.detailView.cqMenus[context].destroy();

      var menu = SE.detailView.cqMenus[context] = new YAHOO.widget.Menu("qcMenuDiv" + panelId, {
        lazyload: true,
        context: ["quickCreateSpan" + panelId, "tr", "br", ["beforeShow", "windowResize"]]
      });

      for (var i = 0; i < this.qcmodules.length; i++) {
        var module = this.qcmodules[i];
        menu.addItem({
          text: app_strings['LBL_EMAIL_QC_' + module.toUpperCase()],
          modulename: module,
          value: module,
          onclick: {
            fn: function () {
              SE.detailView.quickCreate(this.value, ieId, uid, mailbox);
            }
          }
        });
      }

      menu.render(document.body);
      menu.show();
    },

    /**
     * Displays the "View" submenu in the detailView
     * @param string ieId
     * @param string uid
     * @param string mailbox
     */
    showViewMenu: function (ieId, uid, mailbox) {
      var panelId = SE.util.getPanelId();
      var context = "btnEmailView" + panelId;
      if (!SE.detailView.viewMenus)
        SE.detailView.viewMenus = {};

      if (SE.detailView.viewMenus[context])
        SE.detailView.viewMenus[context].destroy();

      var menu = SE.detailView.viewMenus[context] = new YAHOO.widget.Menu("menuDiv" + panelId, {
        lazyload: true,
        context: ["btnEmailView" + panelId, "tl", "bl", ["beforeShow", "windowResize"]],
        clicktohide: true
      });
      menu.addItems(
        (ieId == 'null' || ieId == null) ?
          //No ieId - Sugar Email
          [{
            text: app_strings.LBL_EMAIL_VIEW_RAW,
            onclick: {
              fn: function () {
                SE.detailView.viewRaw(ieId, uid, mailbox);
              }
            }
          }]
          :
          //IeID exists, on a remote server
          [{
            text: app_strings.LBL_EMAIL_VIEW_HEADERS,
            onclick: {
              fn: function () {
                SE.detailView.viewHeaders(ieId, uid, mailbox);
              }
            }
          }, {
            text: app_strings.LBL_EMAIL_VIEW_RAW,
            onclick: {
              fn: function () {
                SE.detailView.viewRaw(ieId, uid, mailbox);
              }
            }
          }]
      );
      menu.render(document.body);
      menu.show();


      /*
       //#23108 jchi@07/17/2008
       menu.render('quickCreateSpan'+ panelId);*/
      //this.viewMenu = menu;
      //this.viewMenu.show(context);
    },
    /**
     * Makes async call to get an email's headers
     */
    viewHeaders: function (ieId, uid, mailbox) {
      var get = "&type=headers&ieId=" + ieId + "&uid=" + uid + "&mailbox=" + mailbox;
      AjaxObject.startRequest(AjaxObject.detailView.callback.viewRaw, urlStandard + "&emailUIAction=displayView" + get);
    },

    /**
     * Makes async call to get a printable version
     */
    viewPrintable: function (ieId, uid, mailbox) {
      if (mailbox == 'sugar::Emails') {
        // display an email from Sugar
        var emailUIAction = '&emailUIAction=getSingleMessageFromSugar';
      } else {
        // display an email from an email server
        var emailUIAction = '&emailUIAction=getSingleMessage';
      }

      var get = "&type=printable&ieId=" + ieId + "&uid=" + uid + "&mbox=" + mailbox;
      AjaxObject.startRequest(AjaxObject.detailView.callback.viewPrint, urlStandard + emailUIAction + get);
    },

    /**
     * Makes async call to get an email's raw source
     */
    viewRaw: function (ieId, uid, mailbox) {
      var get = "&type=raw&ieId=" + ieId + "&uid=" + uid + "&mailbox=" + mailbox;
      AjaxObject.startRequest(AjaxObject.detailView.callback.viewRaw, urlStandard + "&emailUIAction=displayView" + get);
    },

    /**
     * Display all email addresses in detailview.
     */
    displayAllAddrs: function (el) {
      el.style.display = 'none';
      Dom.getNextSibling(el).style.display = 'inline';
    }
  };
////    END SE.detailView
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////    SE.folders
  SE.folders = {
    contextMenuFocus: new Object(),

    /**
     * Generates a standardized identifier that allows reconstruction of I-E ID-folder strings or
     * SugarFolder ID - folder strings
     */
    _createFolderId: function (node) {
      var ret = '';

      if (!node.data.id)
        return ret;

      if (node.data.ieId) {
        /* we have a local Sugar folder */
        if (node.data.ieId == 'folder') {
          ret = "sugar::" + node.data.id; // FYI: folder_id is also stored in mbox field
        } else if (node.data.ieId.match(SE.reGUID)) {
          ret = "remote::" + node.data.ieId + "::" + node.data.mbox.substr(node.data.mbox.indexOf("INBOX"), node.data.mbox.length);
        }
      } else {
        ret = node.data.id;
      }

      return ret;
    },

    addChildNode: function (parentNode, childNode) {
      var is_group = (childNode.properties.is_group == 'true') ? 1 : 0;
      var is_dynamic = (childNode.properties.is_dynamic == 'true') ? 1 : 0;
      var node = this.buildTreeViewNode(childNode.label, childNode.properties.id, is_group, is_dynamic, childNode.properties.unseen, parentNode, childNode.expanded);

      if (childNode.nodes) {
        if (childNode.nodes.length > 0) {
          for (j = 0; j < childNode.nodes.length; j++) {
            var newChildNode = childNode.nodes[j];
            this.addChildNode(node, newChildNode);
          }
        }
      }
    },

    /**
     * Builds and returns a new TreeView Node
     * @param string name
     * @param string id
     * @param int is_group
     * @return object
     */
    buildTreeViewNode: function (name, id, is_group, is_dynamic, unseen, parentNode, expanded) {
      var node = new YAHOO.widget.TextNode(name, parentNode, true);

      //node.href = " SE.listView.populateListFrameSugarFolder(YAHOO.namespace('frameFolders').selectednode, '" + id + "', 'false');";
      node.expanded = expanded;
      node.data = new Object;
      node.data['id'] = id;
      node.data['mbox'] = id; // to support DD imports into BRAND NEW folders
      node.data['label'] = name;
      node.data['ieId'] = 'folder';
      node.data['isGroup'] = (is_group == 1) ? 'true' : 'false';
      node.data['isDynamic'] = (is_dynamic == 1) ? 'true' : 'false';
      node.data['unseen'] = unseen;
      return node;
    },

    /**
     * ensures that a new folder has a valid name
     */
    checkFolderName: function (name) {
      if (name == "")
        return false;

      this.folderAdd(name);
    },

    /**
     * Pings email servers for new email - forces refresh of folder pane
     */
    checkEmailAccounts: function () {
      this.checkEmailAccountsSilent(true);
    },

    checkEmailAccountsSilent: function (showOverlay) {
      if (typeof(SE.folders.checkingMail)) {
        clearTimeout(SE.folders.checkingMail);
      }

      // don't stomp an on-going request
      if (AjaxObject.currentRequestObject.conn == null) {
        if (showOverlay) {
          SUGAR.showMessageBox(app_strings.LBL_EMAIL_CHECKING_NEW,
            app_strings.LBL_EMAIL_ONE_MOMENT + "<br>&nbsp;<br><i>" + app_strings.LBL_EMAIL_CHECKING_DESC + "</i>");
        }
        var user = getUserEditViewUserId();
        AjaxObject.startRequest(AjaxObject.folders.callback.checkMail, urlStandard + '&emailUIAction=checkEmail&all=true' + (user ? '&user=' + user : ''));
      } else {
        // wait 5 secs before trying again.
        SE.folders.checkingMail = setTimeout("SE.folders.checkEmailAccountsSilent(false);", 5000);
      }
    },

    /**
     * Starts check of all email Accounts using a loading bar for large POP accounts
     */
    startEmailAccountCheck: function () {
      // don't do two checks at the same time
      console.warn("deprecated call startEmailAccountCheck");
    },

    /**
     * Checks a single Account check based on passed ieId
     */
    startEmailCheckOneAccount: function (ieId, synch) {
      if (synch) {
        synch = true;
      } else {
        synch = false;
      }
      var mbox = "";
      var node = SE.clickedFolderNode;
      if (node && !synch) {
        mbox = node.data.mbox;
      } // if
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_CHECKING_NEW, app_strings.LBL_EMAIL_CHECKING_DESC, 'progress');
      SE.accounts.ieIds = [ieId];
      AjaxObject.startRequest(AjaxObject.accounts.callbackCheckMailProgress, urlStandard +
        '&emailUIAction=checkEmailProgress&mbox=' + mbox + '&ieId=' + ieId + "&currentCount=0&synch=" + synch);
    },


    /**
     * Empties trash for subscribed accounts
     */
    emptyTrash: function () {
      SE.contextMenus.frameFoldersContextMenu.hide();
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_EMPTYING_TRASH, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackEmptyTrash, urlStandard + '&emailUIAction=emptyTrash');
    },

    /**
     * Clears Cache files of the inboundemail account
     */
    clearCacheFiles: function (ieId) {
      SE.contextMenus.frameFoldersContextMenu.hide();
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_CLEARING_CACHE_FILES, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackClearCacheFiles, urlStandard + '&ieId=' + ieId + '&emailUIAction=clearInboundAccountCache');
    },


    /**
     * Returns an array of all the active accounts in the folder view
     */
    getIeIds: function () {
      var ieIds = [];
      var root = SE.tree.getRoot();
      for (i = 0; i < root.children.length; i++) {
        if ((root.children[i].data.cls == "ieFolder" && root.children[i].children.length > 0) ||
          (root.children[i].data.isGroup != null && root.children[i].data.isGroup == "true" && root.children[i].children.length > 0)) {
          ieIds.push(root.children[i].children[0].data.ieId);
        }
      }
      return ieIds;
    },

    /**
     * loads folder lists in Settings->Folders
     */
    lazyLoadSettings: function (user) {
      AjaxObject.timeout = 300000; // 5 min timeout for long checks
      AjaxObject.startRequest(callbackSettingsFolderRefresh, urlStandard + '&emailUIAction=getFoldersForSettings' + (user ? '&user=' + user : ''));
    },

    /**
     * After the add new folder is done via folders tab on seetings, this function should get called
     * It will refresh the folder list after inserting an entry on the UI to update the new folder list
     */
    loadSettingFolder: function (user) {
      AjaxObject.timeout = 300000; // 5 min timeout for long checks
      AjaxObject.startRequest(callbackLoadSettingFolder, urlStandard + '&emailUIAction=getFoldersForSettings' + (user ? '&user=' + user : ''));
    },

    /**
     * Recursively removes nodes from the TreeView of type Sugar (data.ieId = 'folder')
     */
    removeSugarFolders: function () {
      var tree = SE.tree;
      var root = tree.getRoot();
      var folder = SE.util.findChildNode(root, "ieId", "folder");
      while (folder) {
        tree.removeNode(folder);
        folder = SE.util.findChildNode(root, "ieId", "folder");
      }
      if (!root.childrenRendered) {
        root.childrenRendered = true;
      }
    },

    rebuildFolders: function (silent, user) {
      if (!silent) SUGAR.showMessageBox(app_strings.LBL_EMAIL_REBUILDING_FOLDERS, app_strings.LBL_EMAIL_ONE_MOMENT);
      AjaxObject.startRequest(callbackFolders, urlStandard + '&emailUIAction=getAllFoldersTree' + (user ? '&user=' + user : ''));
    },


    /**
     * Updates TreeView with Sugar Folders
     */
    setSugarFolders: function (delay, user) {
      this.removeSugarFolders();
      //AjaxObject.forceAbort = true;
      AjaxObject.startRequest(callbackRefreshSugarFolders, urlStandard + "&emailUIAction=refreshSugarFolders" + (user ? '&user=' + user : ''));
    },

    /**
     * Takes async data object and creates the sugar folders in TreeView
     */
    setSugarFoldersEnd: function (o) {
      var root = SE.tree.getRoot();
      addChildNodes(root, {nodes: o});
      SE.accounts.renderTree();
      //If nothing is loaded in the grid, load "My Inbox"
      if (SE.grid.params.ieId == "undefined") {
        SE.listView.populateListFrameSugarFolder({data: o[0]}, o[0].id, false);
      }
    },

    startCheckTimer: function () {
      if (SE.userPrefs.emailSettings.emailCheckInterval && SE.userPrefs.emailSettings.emailCheckInterval != -1) {
        var ms = SE.userPrefs.emailSettings.emailCheckInterval * 60 * 1000;

        if (typeof(SE.folders.checkTimer) != 'undefined') {
          clearTimeout(SE.folders.checkTimer);
        }

        SE.folders.checkTimer = setTimeout("SE.folders.checkEmailAccountsSilent(false);", ms);
        if (!SE.userPrefs.emailSettings.firstAutoCheck) {
          SE.userPrefs.emailSettings.firstAutoCheck = true;
          SE.folders.checkEmailAccountsSilent(false);
        }
      }
    },

    /**
     * makes an async call to save user preference and refresh folder view
     * @param object SELECT list object
     */
    setFolderSelection: function () {
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_REBUILDING_FOLDERS, app_strings.LBL_EMAIL_ONE_MOMENT);

      var a_rs = SE.accounts.inboundAccountsSettingsTable.getRecordSet().getRecords();
      var a_active_accnts = "";
      for (i = 0; i < a_rs.length; i++) {
        var t_record = a_rs[i];
        var is_active = $('#' + t_record._sId + ' input[type="checkbox"]').prop('checked');

        if (is_active)
          a_active_accnts += ("&ieIdShow[]=" + t_record.getData('id'));
      }

      if (a_active_accnts == "")
        a_active_accnts = "&ieIdShow[]=";

      getRequestedParameter = function (name) {
        if (name = (new RegExp('[?&]' + encodeURIComponent(name) + '=([^&]*)')).exec(location.search))
          return decodeURIComponent(name[1]);
      }

      var currentUserRecord = getRequestedParameter('record');

      if(typeof currentUserRecord === 'undefined') {
        currentUserRecord = $('input[name="record"]').val();
      }

      var query = "&emailUIAction=setFolderViewSelection" + a_active_accnts + '&user=' + currentUserRecord;

      AjaxObject.startRequest(callbackFolders, urlStandard + query);
    },

    /**
     * makes async call to save user preference for a given node's open state
     * @param object node YUI TextNode object
     */
    setOpenState: function (node) {
      SE.util.clearHiddenFieldValues('emailUIForm');
      var nodePath = node.data.id;
      var nodeParent = node.parent;

      while (nodeParent != null) {
        // root node has no ID param
        if (nodeParent.data != null) {
          nodePath = nodeParent.data.id + "::" + nodePath;
        }

        var nodeParent = nodeParent.parent;
      }

      document.getElementById('emailUIAction').value = 'setFolderOpenState';
      document.getElementById('focusFolder').value = nodePath;

      if (node.expanded == true) {
        document.getElementById('focusFolderOpen').value = 'open';
      } else {
        document.getElementById('focusFolderOpen').value = 'closed';
      }

      var formObject = document.getElementById('emailUIForm');
      YAHOO.util.Connect.setForm(formObject);

      AjaxObject.startRequest(null, null);
    },

    getNodeFromMboxPath: function (path) {
      var tree = YAHOO.widget.TreeView.getTree('frameFolders');
      var a = YAHOO.lang.JSON.parse(path);

      var node = tree.getRoot();

      var i = 0;
      while (i < a.length) {
        node = this.getChildNodeFromLabel(node, a[i]);
        i++;
      }

      return node;
    },

    getChildNodeFromLabel: function (node, nodeLabel) {
      for (i = 0; i < node.children.length; i++) {
        if (node.children[i].data.id == nodeLabel) {
          return node.children[i];
        }
      }
    },

    /**
     * returns the node that presumably under the user's right-click
     */
    getNodeFromContextMenuFocus: function () {
      //// get the target(parent) node
      var tree = YAHOO.widget.TreeView.trees.frameFolders;
      var index = -1;
      var target = SE.contextMenus.frameFoldersContextMenu.contextEventTarget;

      // filter local folders
      if (target.className == 'localFolder' || target.className == 'groupInbox') {
        while (target && (target.className == 'localFolder' || target.className == 'groupInbox')) {
          if (target.id == '') {
            target = target.parentNode;
          } else {
            break;
          }
        }
      }

      var targetNode = document.getElementById(target.id);
      re = new RegExp(/ygtv[a-z]*(\d+)/i);

      try {
        var matches = re.exec(targetNode.id);
      } catch (ex) {
        return document.getElementById(ygtvlabelel1);
      }

      if (matches) {
        index = matches[1];
      } else {
        // usually parent node
        matches = re.exec(targetNode.parentNode.id);

        if (matches) {
          index = matches[1];
        }
      }

      var parentNode = (index == -1) ? tree.getNodeByProperty('id', 'Home') : tree.getNodeByIndex(index);
      parentNode.expand();

      return parentNode;
    },

    /**
     * Decrements the Unread Email count in folder text
     * @param string ieId ID to look for
     * @param string mailbox name
     * @param count how many to decrement
     */
    decrementUnreadCount: function (ieId, mbox, count) {

      if (mbox == null)
        return;

      if (mbox.indexOf("sugar::") === 0) {
        var node = this.getNodeFromId(ieId);
      } else {
        var node = this.getNodeFromIeIdAndMailbox(ieId, mbox);
      }
      if (node) {
        var unseen = node.data.unseen;
        if (unseen > 0) {
          var check = unseen - count;
          var finalCount = (check >= 0) ? check : 0;
          node.data.unseen = finalCount;
        }
        SE.accounts.renderTree();
      }
    },

    /**
     * gets the TreeView node with a given ID/ieId
     * @param string id ID to look for
     * @return object Node
     */
    getNodeFromId: function (id) {
      SE.folders.focusNode = null;
      SE.util.cascadeNodes(SE.tree.getRoot(), function (ieId) {
        if ((this.data.id && this.data.id == ieId) || (this.data.ieId && this.data.ieId == ieId)) {
          SE.folders.focusNode = this;
          return false;
        }
      }, null, [id]);
      return SE.folders.focusNode;
    },

    /**
     * Uses ieId and mailbox to try to find a node in the tree
     */
    getNodeFromIeIdAndMailbox: function (id, mbox) {
      SE.folders.focusNode = null;
      if (mbox == "sugar::Emails") {
        mbox = id;
        id = "folder";
      } // if
      SE.util.cascadeNodes(SE.tree.getRoot(), function (varsarray) {
        if (varsarray instanceof Array) {
          if (this.data.ieId && this.data.ieId == varsarray[0]
            && this.data.mbox == varsarray[1]) {
            SE.folders.focusNode = this;
            return false;
          }
        }
        else {
          if (this.data.ieId && this.data.ieId == varsarray) {
            SE.folders.focusNode = this;
            return false;
          }
        }
      }, null, [id, mbox]);
      return SE.folders.focusNode;
    },

    unhighliteAll: function () {
      SE.util.cascadeNodes(SE.tree.getRoot(), function () {
        this.unhighlight()
      });
    },

    /**
     * Displays a short form
     */
    folderAdd: function () {
      SE.contextMenus.frameFoldersContextMenu.hide();

      var node = SE.clickedFolderNode;

      if (node != null && node.data) {
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_FOLDERS_ADD_DIALOG_TITLE,
          app_strings.LBL_EMAIL_FOLDERS_NEW_FOLDER,
          'prompt', {
            fn: SE.folders.folderAddXmlCall,
            beforeShow: SE.folders.folderAddRegisterEnter,
            beforeHide: SE.folders.folderRemoveRegisterEnter
          });
      } else {
        alert(app_strings.LBL_EMAIL_FOLDERS_NO_VALID_NODE);
      }
    },

    folderAddRegisterEnter: function () {
      this.enterKeyListener = new YAHOO.util.KeyListener(YAHOO.util.Dom.get("sugar-message-prompt"),
        {keys: YAHOO.util.KeyListener.KEY.ENTER},
        this.buttons[1].handler);

      this.enterKeyListener.enable();
    },

    folderRemoveRegisterEnter: function () {
      this.enterKeyListener.disable();
    },

    folderAddXmlCall: function (name) {
      if (trim(name) == "") {
        alert(mod_strings.LBL_ENTER_FOLDER_NAME);
        return false;
      }
      name = escape(name);
      var post = '';
      var type = 'sugar';

      var parentNode = SE.clickedFolderNode;

      this.contextMenuFocus = parentNode;

      if (parentNode.data.ieId) {
        if (parentNode.data.ieId != 'folder' && parentNode.data.ieId.match(SE.reGUID)) {
          type = 'imap';
        }
      }
      if (type == 'imap') {
        // make an IMAP folder
        post = "&newFolderName=" + name + "&mbox=" + parentNode.data.mbox + "&ieId=" + parentNode.data.ieId;
        AjaxObject.startRequest(callbackFolderRename, urlStandard + '&emailUIAction=saveNewFolder&folderType=imap' + post);
      } else if (type == 'sugar') {
        // make a Sugar folder
        if (SE.folders.isUniqueFolderName(name)) {
          post = "&parentId=" + parentNode.data.id + "&nodeLabel=" + name;
          AjaxObject.startRequest(callbackFolderSave, urlStandard + '&emailUIAction=saveNewFolder&folderType=sugar&' + post);
        } else {
          alert(app_strings.LBL_EMAIL_ERROR_DUPE_FOLDER_NAME);
          SE.folders.folderAdd();
          return;
        }
      } else {
        alert(app_strings.LBL_EMAIL_ERROR_CANNOT_FIND_NODE);
      }

      // hide add-folder diaglogue
      SE.e2overlay.hide();
    },

    /**
     * Removes either an IMAP folder or a Sugar Folder
     */
    folderDelete: function () {
      SE.contextMenus.frameFoldersContextMenu.hide();

      if (confirm(app_strings.LBL_EMAIL_FOLDERS_DELETE_CONFIRM)) {
        var post = '';
        var parentNode = SE.clickedFolderNode;

        if (parentNode != null && parentNode.data) {
          if (parentNode.data.mbox == 'INBOX' || parentNode.data.id == 'Home') {
            SUGAR.showMessageBox(app_strings.LBL_EMAIL_ERROR_GENERAL_TITLE, app_strings.LBL_EMAIL_FOLDERS_CHANGE_HOME, 'alert');
            return;
          }

          AjaxObject.target = 'frameFlex';

          if (parentNode.data.ieId != 'folder') {
            // delete an IMAP folder
            post = "&folderType=imap&mbox=" + parentNode.data.mbox + "&ieId=" + parentNode.data.ieId;
          } else {
            // delete a sugar folder
            post = "&folderType=sugar&folder_id=" + parentNode.data.id;
          }
          SUGAR.showMessageBox("Deleting folder", app_strings.LBL_EMAIL_ONE_MOMENT);
          AjaxObject.startRequest(callbackFolderDelete, urlStandard + '&emailUIAction=deleteFolder' + post);
        } else {
          alert(app_strings.LBL_EMAIL_ERROR_CANNOT_FIND_NODE);
        }
      }
    },

    /**
     * Rename folder form
     */
    //EXT111
    folderRename: function () {
      SE.contextMenus.frameFoldersContextMenu.hide();
      var node = SE.clickedFolderNode;

      if (node != null) {
        if (node.id == 'Home' || !node.data || node.data.mbox == 'INBOX') {
          SUGAR.showMessageBox(app_strings.LBL_EMAIL_ERROR_GENERAL_TITLE, app_strings.LBL_EMAIL_FOLDERS_CHANGE_HOME, 'alert');
          return;
        }

        SUGAR.showMessageBox(app_strings.LBL_EMAIL_FOLDERS_RENAME_DIALOG_TITLE + " - " + node.data.text,
          app_strings.LBL_EMAIL_SETTINGS_NAME,
          'prompt',
          {
            fn: SE.folders.submitFolderRename,
            beforeShow: SE.folders.folderAddRegisterEnter,
            beforeHide: SE.folders.folderRemoveRegisterEnter
          });
      } else {
        alert(app_strings.LBL_EMAIL_FOLDERS_NO_VALID_NODE);
      }
    },

    /**
     * fills an Object with key-value pairs of available folders
     */
    getAvailableFoldersObject: function () {
      var ret = new Object();
      var tree = SE.tree.root;

      if (tree.children) {
        for (var i = 0; i < tree.children.length; i++) {
          ret = this.getFolderFromChild(ret, tree.children[i], '', app_strings.LBL_EMAIL_SPACER_MAIL_SERVER);
        }
      } else {
        ret['none'] = app_strings.LBL_NONE;
      }

      return ret;
    },

    /**
     * Fills in key-value pairs for dependent dropdowns
     * @param object ret Associative array
     * @param object node TreeView node in focus
     * @param string currentPath Built up path thus far
     * @param string spacer Defined in app_strings, visual separator b/t Sugar and Remote folders
     */
    getFolderFromChild: function (ret, node, currentPath, spacer) {
      if (node.data != null && node.depth > 0) {
        /* handle visual separtors differentiating b/t mailserver and local */
        if (node.data.ieId && node.data.ieId == 'folder') {
          spacer = app_strings.LBL_EMAIL_SPACER_LOCAL_FOLDER;
        }

        if (!ret.spacer0) {
          ret['spacer0'] = spacer;
        } else if (ret.spacer0 != spacer) {
          ret['spacer1'] = spacer
        }

        var theLabel = node.data.label.replace(/<[^>]+[\w\/]+[^=>]*>/gi, '');
        var depthMarker = currentPath;
        var retIndex = SE.folders._createFolderId(node);
        ret[retIndex] = depthMarker + theLabel;
      }

      if (node.children != null) {
        if (theLabel) {
          currentPath += theLabel + "/";
        }

        for (var i = 0; i < node.children.length; i++) {
          ret = this.getFolderFromChild(ret, node.children[i], currentPath, spacer);
        }
      }

      return ret;
    },

    /**
     * Wrapper to refresh folders tree
     */
    getFolders: function () {
      SE.accounts.rebuildFolderList();
    },

    /**
     * handles events around folder-rename input field changes
     * @param object YUI event object
     */
    handleEnter: function (e) {
      switch (e.browserEvent.type) {
        case 'click':
          e.preventDefault(); // click in text field
          break;

        case 'blur':
          SE.folders.submitFolderRename(e);
          break;

        case 'keypress':
          var kc = e.browserEvent.keyCode;
          switch (kc) {
            case 13: // enter
              e.preventDefault();
              SE.folders.submitFolderRename(e);
              break;

            case 27: // esc
              e.preventDefault(e);
              SE.folders.cancelFolderRename(e);
              break;
          }
          break;
      }
    },
    /**
     * Called when a node is clicked on in the folder tree
     * @param node, The node clicked on
     * @param e, The click event
     */
    handleClick: function (o) {
      var node = o.node;
      //If the click was on a sugar folder
      if (node.data.ieId == "folder") {
        SE.listView.populateListFrameSugarFolder(node, node.id, false);
      }
      else {
        SE.listView.populateListFrame(node, node.data.ieId, false);
      }
    },

    /**
     * Called when a node is right-clicked on in the folder tree
     */
    handleRightClick: function (e) {
      YAHOO.util.Event.preventDefault(e);
      //Get the Tree Node
      var node = SUGAR.email2.tree.getNodeByElement(YAHOO.util.Event.getTarget(e));
      var menu = SUGAR.email2.contextMenus.frameFoldersContextMenu;

      //If the click was on a sugar folder
      SE.clickedFolderNode = node;
      var inbound = (node.data.ieId && node.data.ieId != 'folder');
      var disableNew = (inbound && (typeof(node.data.mbox) == 'undefined'));
      menu.getItem(0).cfg.setProperty("disabled", !inbound);
      menu.getItem(1).cfg.setProperty("disabled", !inbound);
      menu.getItem(2).cfg.setProperty("disabled", disableNew);
      menu.getItem(3).cfg.setProperty("disabled", false);
      menu.getItem(4).cfg.setProperty("disabled", false);
      menu.getItem(5).cfg.setProperty("disabled", false);
      menu.getItem(6).cfg.setProperty("disabled", true);
      //Group folder
      if (inbound && node.data.isGroup != null && node.data.isGroup == "true") {
        menu.getItem(0).cfg.setProperty("disabled", true);
        menu.getItem(1).cfg.setProperty("disabled", true);
        menu.getItem(2).cfg.setProperty("disabled", true);
        menu.getItem(3).cfg.setProperty("disabled", true);
        menu.getItem(4).cfg.setProperty("disabled", true);
      }
      if (node.data.protocol != null) {
        menu.getItem(6).cfg.setProperty("disabled", false);
      }
      if (node.data.folder_type != null && (node.data.folder_type == "inbound" ||
        node.data.folder_type == "sent" || node.data.folder_type == "draft")) {
        //Sent or Draft folders
        menu.getItem(3).cfg.setProperty("disabled", true);
        menu.getItem(4).cfg.setProperty("disabled", true);
        menu.getItem(5).cfg.setProperty("disabled", true);
      }

      //For group with auto inbound, disable everything.
      if ((typeof(node.data.is_group) != 'undefined') && node.data.is_group == 'true') {
        menu.getItem(0).cfg.setProperty("disabled", true);
        menu.getItem(1).cfg.setProperty("disabled", true);
        menu.getItem(2).cfg.setProperty("disabled", true);
        menu.getItem(3).cfg.setProperty("disabled", true);
        menu.getItem(4).cfg.setProperty("disabled", true);
        menu.getItem(5).cfg.setProperty("disabled", true);
        menu.getItem(6).cfg.setProperty("disabled", true);
      }

      menu.cfg.setProperty("xy", YAHOO.util.Event.getXY(e));
      menu.show();
    },

    /**
     * Called when a row is dropped on a node
     */
    handleDrop: function (rows, targetFolder) {
      var rowData = rows[0].getData();
      if (rowData.mbox != targetFolder.data.mbox) {
        var srcIeId = rowData.ieId;
        var srcFolder = rowData.mbox;
        var destIeId = targetFolder.data.ieId;
        var destFolder = targetFolder.data.mbox;
        var uids = [];
        for (var i = 0; i < rows.length; i++) {
          uids[i] = rows[i].getData().uid;
        }
        SE.listView.moveEmails(srcIeId, srcFolder, destIeId, destFolder, uids, rows);
      }
    },

    /**
     * Called when something is dragged over a Folder Node
     */
    dragOver: function (dragObject) {
      return true;
    },

    /**
     * Determines if a folder name is unique to the folder tree
     * @param string name
     */
    isUniqueFolderName: function (name) {
      uniqueFolder = true;
      var root = SE.tree.getRoot();
      SE.util.cascadeNodes(SE.tree.getRoot(), function (name) {
        if (this.attributes && this.attributes.ieId == "folder") {
          if (this.attributes.text == name) {
            uniqueFolder = false;
            return false;
          }
        }
      }, null, [name]);
      return uniqueFolder;
    },

    /**
     * Makes async call to rename folder in focus
     * @param object e Event Object
     */
    submitFolderRename: function (newName) {
      if (trim(newName) == "") {
        alert(mod_strings.LBL_ENTER_FOLDER_NAME);
        return false;
      }
      newName = escape(newName);
      var node = SE.clickedFolderNode;
      var origName = node.data.text
      //Ignore no change
      if (newName == origName) {
        return true;
      }
      if (SE.folders.isUniqueFolderName(newName)) {
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_MENU_RENAMING_FOLDER, app_strings.LBL_EMAIL_ONE_MOMENT);
        if (node.data.ieId == "folder") {
          //Sugar Folder
          AjaxObject.startRequest(callbackFolderRename, urlStandard + "&emailUIAction=renameFolder&folderId=" + node.data.id + "&newFolderName=" + newName);
        }
        else {
          //IMAP folder or POP mailbox
          var nodePath = node.data.mbox.substring(0, node.data.mbox.lastIndexOf(".") + 1);
          AjaxObject.startRequest(callbackFolderRename, urlStandard + "&emailUIAction=renameFolder&ieId="
            + node.data.ieId + "&oldFolderName=" + node.data.mbox + "&newFolderName=" + nodePath + newName);
        }
        return true;
      } else {
        alert(app_strings.LBL_EMAIL_ERROR_DUPE_FOLDER_NAME);
        return false;
      }
    },

    moveFolder: function (folderId, parentFolderId) {
      if (folderId != parentFolderId) {
        AjaxObject.startRequest(callbackFolderRename, urlStandard + "&emailUIAction=moveFolder&folderId="
          + folderId + "&newParentId=" + parentFolderId);
      }
    },

    /**
     * makes async call to do a full synchronization of all accounts
     */
    synchronizeAccounts: function () {
      if (confirm(app_strings.LBL_EMAIL_SETTINGS_FULL_SYNC_WARN)) {
        SUGAR.showMessageBoxModal(app_strings.LBL_EMAIL_SETTINGS_FULL_SYNC, app_strings.LBL_EMAIL_ONE_MOMENT + "<br>&nbsp;<br>" + app_strings.LBL_EMAIL_COFFEE_BREAK);
        AjaxObject.startRequest(callbackFullSync, urlStandard + '&emailUIAction=synchronizeEmail');
      }
    },

    /**
     * Updates user's folder subscriptsion (Sugar only)
     * @param object SELECT DOM object in focus
     * @param string type of Folder selection
     */
    updateSubscriptions: function () {
      SUGAR.showMessageBox(app_strings.LBL_EMAIL_REBUILDING_FOLDERS, app_strings.LBL_EMAIL_ONE_MOMENT);

      var active = "";

      select = document.getElementById('userFolders');

      for (i = 0; i < select.options.length; i++) {
        var opt = select.options[i];
        if (opt.selected && opt.value != "") {
          if (active != "") {
            active += "::";
          }
          active += opt.value;
        }
      }

      //Add the group folder ids.
      var group_folders = SUGAR.email2.folders.retrieveGroupFolderSubscriptions();
      for (i = 0; i < group_folders.length; i++) {
        active += ("::" + group_folders[i]);
      }

      AjaxObject.startRequest(callbackFolderSubscriptions, urlStandard + '&emailUIAction=updateSubscriptions&subscriptions=' + active);
    },
    /**
     * Updates user's group folder subscriptsion (Sugar only)
     * @param ieID The group folder to add to the tree view
     * @see SE.folders.setFolderSelection()
     */
    retrieveGroupFolderSubscriptions: function () {

      var a_rs = SE.accounts.inboundAccountsSettingsTable.getRecordSet().getRecords();
      var activeGroupFolders = "";
      var activeGroupIds = [];
      for (i = 0; i < a_rs.length; i++) {
        var t_record = a_rs[i];

        var is_active = t_record.getData('is_active');
        var isGroupFolder = t_record.getData('has_groupfolder');
        var ieID = t_record.getData('id');
        if (isGroupFolder) {
          if (is_active)
            activeGroupIds.push(ieID);
        }
      }

      return activeGroupIds;
    }

  };

  SE.folders.checkEmail2 = function () {
    AjaxObject.startRequest(callbackCheckEmail2, urlStandard + "&emailUIAction=checkEmail2");
  }
////    END FOLDERS OBJECT
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////    SE.keys
  /**
   * Keypress Event capture and processing
   */
  SE.keys = {
    overall: function (e) {
      switch (e.charCode) {
        case 119: // "w"
          if (e.ctrlKey || e.altKey) {
            var focusRegion = SE.innerLayout.regions.center;
            if (focusRegion.activePanel.closable == true) {
              focusRegion.remove(focusRegion.activePanel);
            }
          }
          break;
      }
    }
  };
////    END SE.keys
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
////    SE.listView
  /**
   * ListView object methods and attributes
   */
  SE.listView = {
    currentRowId: -1,

    /**
     * Fills the ListView pane with detected messages.
     */
    populateListFrame: function (node, ieId, forceRefresh) {
      SE.innerLayout.selectTab(0);
      YAHOO.util.Connect.abort(AjaxObject.currentRequestObject, null, false);

      Dom.get('_blank').innerHTML = "";
      SE.grid.params['emailUIAction'] = 'getMessageList';
      SE.grid.params['mbox'] = node.data.mbox;
      SE.grid.params['ieId'] = ieId;
      forcePreview = true; // loads the preview pane with first item in grid
      SE.listView.refreshGrid();
    },

    /**
     * Like populateListFrame(), but specifically for SugarFolders since the API is radically different
     */
    populateListFrameSugarFolder: function (node, folderId, forceRefresh, getUnread) {
      SE.innerLayout.selectTab(0);
      Dom.get('_blank').innerHTML = "";
      SE.grid.params['emailUIAction'] = 'getMessageListSugarFolders';
      SE.grid.params['ieId'] = node.data.id;
      SE.grid.params['mbox'] = node.data.origText ? node.data.origText : node.data.text;
      SE.grid.params['getUnread'] = getUnread;
      SE.listView.refreshGrid();
    },

    /**Mac
     * Sets sort order as user preference
     * @param
     */
    saveListViewSortOrder: function (sortBy, focusFolderPassed, ieIdPassed, ieNamePassed) {
      ieId = ieIdPassed;
      ieName = ieNamePassed;
      focusFolder = focusFolderPassed;

      SE.util.clearHiddenFieldValues('emailUIForm');
      var previousSort = document.getElementById('sortBy').value;

      document.getElementById('sortBy').value = sortBy;
      document.getElementById('emailUIAction').value = 'saveListViewSortOrder';
      document.getElementById('focusFolder').value = focusFolder;
      document.getElementById('ieId').value = ieId;

      if (sortBy == previousSort) {
        document.getElementById('reverse').value = '1';
      }

      var formObject = document.getElementById('emailUIForm');
      YAHOO.util.Connect.setForm(formObject);

      AjaxObject.startRequest(callbackListViewSortOrderChange, null);
    },


    /**
     * Enables click/arrow select of grid items which then populate the preview pane.
     */
    selectFirstRow: function () {
      SE.grid.selModel.selectFirstRow();
    },

    selectLastRow: function () {
      SE.grid.selModel.selectRow(SE.grid.dataSource.data.getCount() - 1);
    },

    setEmailListStyles: function () {
      SE.listView.boldUnreadRows();
      return;
      var ds = SE.grid.getStore();
      if (SE.grid.getSelections().length == 0) {
        document.getElementById('_blank').innerHTML = '';
      }

      var acctMbox = '';
      if (typeof(ds.baseParams.mbox) != 'undefined') {
        acctMbox = (ds.baseParams.acct) ? ds.baseParams.acct + " " + ds.baseParams.mbox : ds.baseParams.mbox;
        var cm = SE.grid.getColumnModel();
        if (ds.baseParams.mbox == mod_strings.LBL_LIST_FORM_SENT_TITLE) {
          cm.setColumnHeader(4, mod_strings.LBL_LIST_DATE_SENT_RECEIVED);
          //SE.grid.render();
        } else if (cm.config[4].header != app_strings.LBL_EMAIL_DATE_SENT_RECEIVED_BY_SENDER) {
          cm.setColumnHeader(4, app_strings.LBL_EMAIL_DATE_SENT_RECEIVED_BY_SENDER);
          //SE.grid.render();
        }
      }
      var total = (typeof(ds.totalLength) != "undefined") ? " (" + ds.totalLength + " " + app_strings.LBL_EMAIL_MESSAGES + ") " : "";
      SE.listViewLayout.setTitle(acctMbox + total);// + toggleRead + manualFit);

      if (ds.reader.xmlData.getElementsByTagName('UnreadCount').length > 0) {
        var unread = ds.reader.xmlData.getElementsByTagName('UnreadCount')[0].childNodes[0].data;
        var node = SE.folders.getNodeFromIeIdAndMailbox(ds.baseParams.ieId, ds.baseParams.mbox);
        if (node) node.data.unseen = unread;
      }
      SE.accounts.renderTree();


      // bug 15035 perhaps a heavy handed solution to stopping the loading spinner.
      if (forcePreview && ds.totalCount > 0) {
        SE.detailView.getEmailPreview();
        forcePreview = false;
      }
    },

    /**
     * Removes a row if found via its UID
     */
    removeRowByUid: function (uid) {
      uid = new String(uid);
      uids = uid.split(',');
      var dataTableRecords = SE.grid.getRecordSet().getRecords(0, SE.grid.getRecordSet().getLength());

      for (j = 0; j < uids.length; j++) {
        var theUid = uids[j];
        for (k = 0; k < SE.grid.getRecordSet().getLength(); k++) {
          if (SE.grid.getRecordSet().getRecords()[k].getData().uid == theUid) {
            SE.grid.deleteRow(SE.grid.getRecordSet().getRecords()[k]);
          }
        } // for
      }
    },

    displaySelectedEmails: function (rows) {
      var dm = SE.grid.getDataModel();
      var uids = '';

      for (i = 0; i < rows.length; i++) {
        var rowIndex = rows[i].rowIndex;
        var metadata = dm.data[rowIndex];

        if (uids != "") {
          uids += ",";
        }
        uids += metadata[5];

        // unbold unseen email
        this.unboldRow(rowIndex);
      }

      SE.detailView.populateDetailViewMultiple(uids, metadata[6], metadata[7], metadata[8], false);
    },

    /**
     * exception handler for data load failures
     */
    loadException: function (dataModel, ex, response) {
    },

    /**
     * Moves email(s) from a folder to another, from IMAP/POP3 to Sugar and vice-versa
     * @param string sourceIeId Email's source I-E id
     * @param string sourceFolder Email's current folder
     * @param destinationIeId Destination I-E id
     * @param destinationFolder Destination folder in format [root::IE::INBOX::etc]
     *
     * @param array emailUids Array of email's UIDs
     */
    moveEmails: function (sourceIeId, sourceFolder, destinationIeId, destinationFolder, emailUids, selectedRows) {
      if (destinationIeId != 'folder' && sourceIeId != destinationIeId) {
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_ERROR_MOVE_TITLE, app_strings.LBL_EMAIL_ERROR_MOVE);
      } else {
        SUGAR.showMessageBox("Moving Email(s)", app_strings.LBL_EMAIL_ONE_MOMENT);
        // remove rows from visibility
        for (row in selectedRows) {
          //SE.grid.getStore().remove(row);
        }

        var baseUrl = '&sourceIeId=' + sourceIeId +
          '&sourceFolder=' + sourceFolder +
          '&destinationIeId=' + destinationIeId +
          '&destinationFolder=' + destinationFolder;
        var uids = '';

        for (i = 0; i < emailUids.length; i++) {
          if (uids != '') {
            uids += app_strings.LBL_EMAIL_DELIMITER;
          }
          uids += emailUids[i];
        }
        if (destinationIeId == 'folder' && sourceFolder != 'sugar::Emails') {
          AjaxObject.startRequest(callbackImportOneEmail, urlStandard + '&emailUIAction=moveEmails&emailUids=' + uids + baseUrl);
        } else {
          AjaxObject.startRequest(callbackMoveEmails, urlStandard + '&emailUIAction=moveEmails&emailUids=' + uids + baseUrl);
        }
      }
    },

    /**
     * Unbolds text in the grid view to denote read status
     */
    markRead: function (index, record) {
      // unbold unseen email
      var row = SE.grid.getRecord(record);
      row.getData().seen = 1;
      SE.grid.getTrEl(record).style.fontWeight = "normal";
    },

    /**
     * grid row output, bolding unread emails
     */
    boldUnreadRows: function () {
      // bold unread emails
      var trEl = SE.grid.getFirstTrEl();
      while (trEl != null) {
        if (SE.grid.getRecord(trEl).getData().seen == "0")
          trEl.style.fontWeight = "bold";
        else
          trEl.style.fontWeight = "";
        trEl = SE.grid.getNextTrEl(trEl);
      }
    },

    /**
     * Show preview for an email if 1 and only 1 is selected
     * ---- all references must be fully qual'd since this gets wrapped by the YUI event handler
     */
    handleRowSelect: function (e) {
      if (e.selectedRows.length == 1) {
        SE.detailView.getEmailPreview();
      }
    },

    handleDrop: function (e, dd, targetId, e2) {
      switch (targetId) {
        case 'htmleditordiv':
          var rows = SE.grid.getSelectedRows();
          if (rows.length > 0) {
            SE.listView.displaySelectedEmails(rows);
          }
          break;

        default:
          var targetElId = new String(targetId);
          var targetIndex = targetElId.replace('ygtvlabelel', "");
          var targetNode = SE.tree.getNodeByIndex(targetIndex);
          var dm = SE.grid.getDataModel();
          var emailUids = new Array();
          var destinationIeId = targetNode.data.ieId;
          var destinationFolder = SE.util.generateMboxPath(targetNode.data.mbox);


          var rows = SE.grid.getSelectedRows();
          // iterate through dragged rows
          for (i = 0; i < rows.length; i++) {
            //var rowIndex = e.selModel.selectedRows[i].rowIndex;
            var rowIndex = rows[i].rowIndex;
            var dataModelRow = dm.data[rowIndex];
            var sourceIeId = dataModelRow[7];
            var sourceFolder = dataModelRow[6];
            emailUids[i] = dataModelRow[5];
          }

          // event wrapped call - need FQ
          SUGAR.showMessageBox(app_strings.LBL_EMAIL_PERFORMING_TASK, app_strings.LBL_EMAIL_ONE_MOMENT);
          SE.listView.moveEmails(sourceIeId, sourceFolder, destinationIeId, destinationFolder, emailUids, e.selModel.selectedRows);
          break;
      }
    },

    /**
     * Hack-around to get double-click and single clicks to work on the grid
     * ---- all references must be fully qual'd since this gets wrapped by the YUI event handler
     */
    handleClick: function (o) {
      SUGAR.email2.grid.clearTextSelection();

      var el = SUGAR.email2.grid.getSelectedRows();

      //Load an email preview only if a single record is selected.  For multiple selections do nothing.
      if (el.length == 1) {
        var rowId = el[0];
        SUGAR.email2.listView.currentRow = SUGAR.email2.grid.getRecord(rowId);
        SUGAR.email2.listView.currentRowIndex = rowId;
        clearTimeout(SUGAR.email2.detailView.previewTimer);
        SUGAR.email2.detailView.previewTimer = setTimeout("SUGAR.email2.detailView.getEmailPreview();", 500);
      }
    },

    /**
     * Custom handler for double-click/enter
     * ---- all references must be fully qual'd since this gets wrapped by the YUI event handler
     */
    getEmail: function (e) {
      var rows = SE.grid.getSelectedRows();
      var row = SE.grid.getRecord(rows[0]).getData();

      clearTimeout(SE.detailView.previewTimer);
      document.getElementById("_blank").innerHTML = "";

      if (row.type != "draft") {
        SE.detailView.populateDetailView(row.uid, row.mbox, row.ieId, 'true', SE.innerLayout);
      } else {
        // circumventing yui-ext tab generation, let callback handler build new view
        SE.util.clearHiddenFieldValues('emailUIForm');
        //function(uid, mbox, ieId, setRead, destination) {
        document.getElementById('emailUIAction').value = 'getSingleMessageFromSugar';
        document.getElementById('uid').value = row.uid; // uid;
        document.getElementById('mbox').value = row.mbox; // mbox;
        document.getElementById('ieId').value = row.ieId; // ieId;

        YAHOO.util.Connect.setForm(document.getElementById('emailUIForm'));
        AjaxObject.target = '_blank';
        AjaxObject.startRequest(AjaxObject.detailView.callback.emailDetail, null);
      }
    },

    /**
     * Retrieves a row if found via its UID
     * @param string
     * @return int
     */
    getRowIndexByUid: function (uid) {
      uid = new String(uid);
      uids = uid.split(',');

      for (j = 0; j < uids.length; j++) {
        var theUid = uids[j];

        for (i = 0; i < SE.grid.getStore().data.length; i++) {
          if (SE.grid.getStore().data[i].id == theUid) {
            return i;
          }
        }
      }
    },

    /**
     * Returns the UID's of the seleted rows
     *
     */
    getUidsFromSelection: function () {
      var rows = SE.grid.getSelectedRows();
      var uids = [];
      /* iterate through available rows JIC a row is deleted - use first available */
      for (var i = 0; i < rows.length; i++) {
        uids[i] = SE.grid.getRecord(rows[i]).getData().uid;
      }
      return uids;
    },

    refreshGrid: function () {
      SE.grid.getDataSource().sendRequest(
        SUGAR.util.paramsToUrl(SE.grid.params),
        SE.grid.onDataReturnInitializeTable,
        SE.grid
      );
    }

  };
////    END SE.listView
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
////    SEARCH
  SE.search = {
    /**
     * sends search criteria
     * @param reference element search field
     */
    search: function (el) {
      var searchCriteria = new String(el.value);

      if (searchCriteria == '') {
        alert(app_strings.LBL_EMAIL_ERROR_EMPTY);
        return false;
      }

      var safeCriteria = escape(searchCriteria);

      var accountListSearch = document.getElementById('accountListSearch');

      SE.grid.getStore().baseParams['emailUIAction'] = 'search';
      SE.grid.getStore().baseParams['mbox'] = app_strings.LBL_EMAIL_SEARCH_RESULTS_TITLE;
      SE.grid.getStore().baseParams['subject'] = safeCriteria;
      SE.grid.getStore().baseParams['ieId'] = accountListSearch.options[accountListSearch.selectedIndex].value;
      SE.grid.getStore().load({params: {start: 0, limit: SE.userPrefs.emailSettings.showNumInList}});

    },

    /**
     * sends advanced search criteria
     */
    searchAdvanced: function () {
      var formObject = document.getElementById('advancedSearchForm');
      var search = false;

      //Set assigned user id to blank if name is not present.
      if (formObject.elements['assigned_user_name'].value == "")
        formObject.elements['assigned_user_id'].value = "";

      for (i = 0; i < formObject.elements.length; i++) {
        if (formObject.elements[i].type != 'button' && formObject.elements[i].value != "") {
          search = true;
        }
        if (formObject.elements[i].type == 'text') {
          SE.grid.params[formObject.elements[i].name] = formObject.elements[i].value;
        }
        if (formObject.elements[i].type == 'hidden') {
          SE.grid.params[formObject.elements[i].name] = formObject.elements[i].value;
        }
        if (formObject.elements[i].type == 'select-one') {
          var el = formObject.elements[i];
          var v = el.options[el.selectedIndex].value;

          if (v != "")
            SE.grid.params[el.name] = v;
          else {
            //Clear previous search results if necessary
            if (typeof( SE.grid.params[el.name]) != 'undefined')
              delete SE.grid.params[el.name]
          }
        }
      }

      if (search) {
        if (!this.validateSearchFormInput())
          return;

        SE.grid.params['emailUIAction'] = 'searchAdvanced';
        SE.grid.params['mbox'] = app_strings.LBL_EMAIL_SEARCH_RESULTS_TITLE;
        var accountListSearch = document.getElementById('accountListSearch');
        SE.listView.refreshGrid();
      } else {
        alert(app_strings.LBL_EMAIL_ERROR_EMPTY);
      }
    },

    /**
     *  Validates the search form inputs to ensure all parameters are valid
     *  @return bool
     */
    validateSearchFormInput: function () {
      addToValidate('advancedSearchForm', 'dateTo', 'date', false, app_strings.LBL_EMAIL_SEARCH_DATE_UNTIL);
      addToValidate('advancedSearchForm', 'dateFrom', 'date', false, app_strings.LBL_EMAIL_SEARCH_DATE_FROM);
      var dateCheck = check_form('advancedSearchForm');

      //If the parent type is selected ensure the user selected a parent_id.
      if (SE.composeLayout.isParentTypeAndNameValid('_search') && dateCheck)
        return true;
      else
        return false;
    },
    /**
     *   Toggles the advanced options, either hidding or showing the selection.
     */
    toggleAdvancedOptions: function () {
      var el = YAHOO.util.Dom.getElementsByClassName('toggleClass', 'tr', 'advancedSearchTable');

      for (var i = 0; i < el.length; i++) {
        if (Dom.hasClass(el[i], "toggleClass yui-hidden"))
          Dom.replaceClass(el[i], "toggleClass yui-hidden", "toggleClass visible-search-option")
        else
          Dom.replaceClass(el[i], "toggleClass visible-search-option", "toggleClass yui-hidden")
      }
    },
    /**
     * clears adv search form fields
     */
    searchClearAdvanced: function () {
      var form = document.getElementById('advancedSearchForm');

      for (i = 0; i < form.elements.length; i++) {
        if (form.elements[i].type != 'button') {
          form.elements[i].value = '';
        }
      }
    }
  };
////    END SE.search
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
////    SE.settings
  SE.settings = {
    /******************************************************************************
     * USER SIGNATURES calls stolen from Users module
     *****************************************************************************/
    createSignature: function (record, the_user_id) {
      var URL = "index.php?module=Users&action=PopupSignature&sugar_body_only=true";
      if (record != "") {
        URL += "&record=" + record;
      }
      if (the_user_id != "") {
        URL += "&the_user_id=" + the_user_id;
      }
      var windowName = 'email_signature';
      var windowFeatures = 'width=800,height=600,resizable=1,scrollbars=1';

      var win = window.open(URL, windowName, windowFeatures);
      if (win && win.focus) {
        // put the focus on the popup if the browser supports the focus() method
        win.focus();
      }
    },

    deleteSignature: function () {
      if (confirm(app_strings.LBL_EMAIL_CONFIRM_DELETE_SIGNATURE)) {
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_IE_DELETE_SIGNATURE, app_strings.LBL_EMAIL_ONE_MOMENT);
        var singature_id = document.getElementById('signature_id').value;
        AjaxObject.startRequest(callbackDeleteSignature, urlStandard + '&emailUIAction=deleteSignature&id=' + singature_id);
      } // if
    },

    saveOptionsGeneral: function (displayMessage) {
      var formObject = document.getElementById('formEmailSettingsGeneral');
      if (!SUGAR.collection.prototype.validateTemSet('formEmailSettingsGeneral', 'team_name')) {
        alert(mod_strings.LBL_EMAILS_NO_PRIMARY_TEAM_SPECIFIED);
        return false;
      } // if
      YAHOO.util.Connect.setForm(formObject);
      SE.composeLayout.emailTemplates = null;

      AjaxObject.target = 'frameFlex';
      var user = getUserEditViewUserId();
      AjaxObject.startRequest(callbackSettings, urlStandard + '&emailUIAction=saveSettingsGeneral' + (user ? '&user=' + user : ''));

      if (displayMessage)
        alert(app_strings.LBL_EMAIL_SETTINGS_SAVED);

      SE.settings.settingsDialog.hide();
    },
    /**
     * Shows settings container screen
     */
    showSettings: function (user) {
      if (!SE.settings.settingsDialog) {
        var dlg = SE.settings.settingsDialog = new YAHOO.widget.Dialog("settingsDialog", {
          modal: true,
          visible: false,
          fixedcenter: false,
          draggable: true,
          constraintoviewport: false
        });
        dlg.showEvent.subscribe(function () {
          var el = this.element;
          var viewH = YAHOO.util.Dom.getViewportHeight();
          if (this.header && el && viewH - 50 < el.clientHeight) {
            var body = this.header.nextElementSibling;
            body.style.overflow = "auto";
            body.style.height = (viewH - 50) + "px";
          }
        }, dlg);
        dlg.setHeader(app_strings.LBL_EMAIL_SETTINGS);
        dlg.setBody('<div id="settingsTabDiv"/>');
        dlg.beforeRenderEvent.subscribe(function () {
          var dd = new YAHOO.util.DDProxy(dlg.element);
          dd.setHandleElId(dlg.header);
          dd.on('endDragEvent', function () {
            dlg.show();
          });
        }, dlg, true);
        dlg.render();

        var tp = SE.settings.settingsTabs = new YAHOO.widget.TabView("settingsTabDiv");
        var tabContent = Dom.get("tab_general");
        tp.addTab(new YAHOO.widget.Tab({
          label: app_strings.LBL_EMAIL_SETTINGS_GENERAL,
          scroll: true,
          content: tabContent.innerHTML,
          id: "generalSettings",
          active: true
        }));
        tabContent.parentNode.removeChild(tabContent);

        tp.appendTo(dlg.body);
      }

      SE.settings.settingsDialog.show();
      SE.folders.lazyLoadSettings(user);
      SE.accounts.lazyLoad(user);
      $(window).scrollLeft(0);
    },


    lazyLoadRules: function () {
      if (false) {
        AjaxObject.startRequest(callbackLoadRules, urlStandard + "&emailUIAction=loadRulesForSettings");
      }

    }
  };
////    END SE.settings
///////////////////////////////////////////////////////////////////////////////
})();

/******************************************************************************
 * UTILITIES
 *****************************************************************************/
function removeHiddenNodes(nodes, grid) {
  var el;
  for (var i = nodes.length - 1; i > -1; i--) {
    el = grid ? grid.getTrEl(nodes[i]) : nodes[i];
    if (YAHOO.util.Dom.hasClass(el, 'rowStylenone')) {
      nodes.splice(i, 1);
    }
  }
}

function strpad(val) {
  return (!isNaN(val) && val.toString().length == 1) ? "0" + val : val;
};

function refreshTodos() {
  SUGAR.email2.util.clearHiddenFieldValues('emailUIForm');
  AjaxObject.target = 'todo';
  AjaxObject.startRequest(callback, urlStandard + '&emailUIAction=refreshTodos');
};

/******************************************************************************
 * MUST STAY IN GLOBAL NAMESPACE
 *****************************************************************************/
function refresh_signature_list(signature_id, signature_name) {
  var field = document.getElementById('signature_id');
  var bfound = 0;
  for (var i = 0; i < field.options.length; i++) {
    if (field.options[i].value == signature_id) {
      if (field.options[i].selected == false) {
        field.options[i].selected = true;
      }
      bfound = 1;
    }
  }
  //add item to selection list.
  if (bfound == 0) {
    var newElement = document.createElement('option');
    newElement.text = signature_name;
    newElement.value = signature_id;
    field.options.add(newElement);
    newElement.selected = true;
  }

  //enable the edit button.
  var field1 = document.getElementById('edit_sig');
  field1.style.visibility = "inherit";
  var deleteButt = document.getElementById('delete_sig');
  deleteButt.style.visibility = "inherit";
};

function setDefaultSigId(id) {
  var checkbox = document.getElementById("signature_default");
  var default_sig = document.getElementById("signatureDefault");

  if (checkbox.checked) {
    default_sig.value = id;
  } else {
    default_sig.value = "";
  }
};

function setSigEditButtonVisibility() {
  var field = document.getElementById('signature_id');
  var editButt = document.getElementById('edit_sig');
  var deleteButt = document.getElementById('delete_sig');
  if (field.value != '') {
    editButt.style.visibility = "inherit";
    deleteButt.style.visibility = "inherit";
  } else {
    editButt.style.visibility = "hidden";
    deleteButt.style.visibility = "hidden";
  }
}
