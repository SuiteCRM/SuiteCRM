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

function showMissingMailboxCredentialsDialog() {
  var $modal = $('<div id="mail-credentials-warning">');
  var $modalText = $('<p>');
  $modalText.append(SUGAR.language.get('app_strings',"LBL_EMAIL_WARNING_MISSING_CREDS"));
  $modal.append($modalText);
  $modal.dialog();
}

function validateMailFolderRequiredFields() {
  var password = Rot13.write(inboundEmailFields.getValue('email_password'));
  var isPasswordSet = inboundEmailFields.getData('email_password', 'is-value-set');
  var authType = inboundEmailFields.getValue('auth_type');
  var externalOAuthConnectionName = inboundEmailFields.getValue('external_oauth_connection_name');

  if (authType === 'basic' && !password && isPasswordSet === false) {
    return false;
  }

  if (authType === 'oauth' && !externalOAuthConnectionName) {
    return false;
  }

  return true;
}

function getExtraMailboxListParams() {
  var authType = inboundEmailFields.getValue('auth_type');
  var recordId = inboundEmailFields.getValue('record');
  var oauthConnectionId = inboundEmailFields.getValue('external_oauth_connection_id');
  var connectionString = inboundEmailFields.getValue('connection_string');

  var extraParams = {};

  if (oauthConnectionId) {
    extraParams.external_oauth_connection_id = oauthConnectionId;
  }

  if (authType) {
    extraParams.auth_type = authType;
  }

  if (recordId) {
    extraParams.ie_id = recordId;
  }

  if (connectionString) {
    extraParams.connection_string = connectionString;
  }

  return extraParams;
}

function openTrashMailboxPopup() {
  var serverUrl = inboundEmailFields.getValue('server_url');
  var protocol = inboundEmailFields.getValue('protocol');
  var port = inboundEmailFields.getValue('port');
  var emailUser = inboundEmailFields.getValue('email_user');
  var password = Rot13.write(inboundEmailFields.getValue('email_password'));
  var trashFolder = inboundEmailFields.getValue('trashFolder');
  var useSSL = inboundEmailFields.getValue('is_ssl');
  var isPersonal = inboundEmailFields.getValue('type') === 'personal';


  if (!validateMailFolderRequiredFields()) {
    showMissingMailboxCredentialsDialog();
    return;
  }

  var extraParams = getExtraMailboxListParams();

  getFoldersListForInboundAccount(
    "InboundEmail",
    "ShowInboundFoldersList",
    "Popup",
    400,
    300,
    serverUrl,
    protocol,
    port,
    emailUser,
    password,
    trashFolder,
    useSSL,
    isPersonal,
    "trash",
    "EditView",
    extraParams
  );
}

function openMailboxPopup() {
  var serverUrl = inboundEmailFields.getValue('server_url');
  var protocol = inboundEmailFields.getValue('protocol');
  var port = inboundEmailFields.getValue('port');
  var emailUser = inboundEmailFields.getValue('email_user');
  var password = Rot13.write(inboundEmailFields.getValue('email_password'));
  var mailbox = inboundEmailFields.getValue('mailbox');
  var useSSL = inboundEmailFields.getValue('is_ssl');
  var isPersonal = inboundEmailFields.getValue('type') === 'personal';
  var searchField = inboundEmailFields.getValue('searchField');

  if (!validateMailFolderRequiredFields()) {
    showMissingMailboxCredentialsDialog();
    return;
  }

  var extraParams = getExtraMailboxListParams();

  getFoldersListForInboundAccount(
    "InboundEmail",
    "ShowInboundFoldersList",
    "Popup",
    400,
    300,
    serverUrl,
    protocol,
    port,
    emailUser,
    password,
    mailbox,
    useSSL,
    isPersonal,
    searchField,
    "EditView",
    extraParams
  );
}

function openSentMailboxPopup() {
  var serverUrl = inboundEmailFields.getValue('server_url');
  var protocol = inboundEmailFields.getValue('protocol');
  var port = inboundEmailFields.getValue('port');
  var emailUser = inboundEmailFields.getValue('email_user');
  var password = Rot13.write(inboundEmailFields.getValue('email_password'));
  var useSSL = inboundEmailFields.getValue('is_ssl');
  var isPersonal = inboundEmailFields.getValue('type') === 'personal';
  var sentFolder = inboundEmailFields.getValue('sentFolder');

  if (!validateMailFolderRequiredFields()) {
    showMissingMailboxCredentialsDialog();
    return;
  }

  var extraParams = getExtraMailboxListParams();

  getFoldersListForInboundAccount(
    "InboundEmail",
    "ShowInboundFoldersList",
    "Popup",
    400,
    300,
    serverUrl,
    protocol,
    port,
    emailUser,
    password,
    sentFolder,
    useSSL,
    isPersonal,
    "sent",
    "EditView",
    extraParams
  );

}

