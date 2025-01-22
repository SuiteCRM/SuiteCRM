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
 */SUGAR.inboundEmail={};Rot13={map:null,convert:function(a){Rot13.init();var s="";for(i=0;i<a.length;i++){var b=a.charAt(i);s+=((b>='A'&&b<='Z')||(b>='a'&&b<='z')?Rot13.map[b]:b);}
return s;},init:function(){if(Rot13.map!=null)
return;var map=new Array();var s="abcdefghijklmnopqrstuvwxyz";for(i=0;i<s.length;i++)
map[s.charAt(i)]=s.charAt((i+13)%26);for(i=0;i<s.length;i++)
map[s.charAt(i).toUpperCase()]=s.charAt((i+13)%26).toUpperCase();Rot13.map=map;},write:function(a){return Rot13.convert(a);}}
function getEncryptedPassword(login,password,mailbox){var words=new Array(login,password,mailbox);for(i=0;i<3;i++){word=words[i];if(word.indexOf('&')>0){fragment1=word.substr(0,word.indexOf('&'));fragment2=word.substr(word.indexOf('&')+1,word.length);newWord=fragment1+'::amp::'+fragment2;words[i]=newWord;word=newWord;fragment1='';fragment2='';}
if(word.indexOf('+')>0){fragment1=word.substr(0,word.indexOf('+'));fragment2=word.substr(word.indexOf('+')+1,word.length);newWord=fragment1+'::plus::'+fragment2;words[i]=newWord;word=newWord;fragment1='';fragment2='';}
if(word.indexOf('%')>0){fragment1=word.substr(0,word.indexOf('%'));fragment2=word.substr(word.indexOf('%')+1,word.length);newWord=fragment1+'::percent::'+fragment2;words[i]=newWord;word=newWord;fragment1='';fragment2='';}}
return words;}
function ie_test_open_popup_with_submit(module_name,action,pageTarget,width,height,mail_server,protocol,port,login,password,mailbox,ssl,personal,formName,ie_id,connectionString,auth_type,externalOauthConnectionName,externalOauthConnectionId){if(!formName)formName="testSettingsView";var words=getEncryptedPassword(login,password,mailbox);var isPersonal=(personal)?'true':'false';if(!isDataValid(formName,true)){return;}
if(typeof(ie_id)=='undefined'||ie_id=='')
ie_id=(typeof document.getElementById(formName).ie_id!='undefined')?document.getElementById(formName).ie_id.value:'';URL='index.php?'
+'module='+module_name
+'&to_pdf=1'
+'&action='+action
+'&target='+pageTarget
+'&target1='+pageTarget
+'&server_url='+mail_server
+'&email_user='+words[0]
+'&protocol='+protocol
+'&port='+port
+'&email_password='+encodeURIComponent(words[1])
+'&mailbox='+words[2]
+'&ssl='+ssl
+'&ie_id='+ie_id
+'&personal='+isPersonal
+'&auth_type='+auth_type;if(externalOauthConnectionName&&externalOauthConnectionId){URL+='&externalOauthConnectionName='+externalOauthConnectionName
+'&externalOauthConnectionId='+externalOauthConnectionId;}
if(connectionString){URL+='&connection_string='+encodeURIComponent(connectionString);}
var SI=SUGAR.inboundEmail;if(!SI.testDlg){SI.testDlg=new YAHOO.widget.SimpleDialog("testSettingsDiv",{fixedcenter:true,width:width+"px",draggable:true,dragOnly:true,close:true,constraintoviewport:true,modal:true,loadingText:SUGAR.language.get("app_strings","LBL_EMAIL_LOADING")});SI.testDlg._updateContent=function(o){var w=this.cfg.config.width.value+"px";this.setBody(o.responseText);if(this.evalJS)
SUGAR.util.evalScript(o.responseText);if(!SUGAR.isIE)
this.body.style.width=w}}
var title=SUGAR.language.get('Emails','LBL_TEST_SETTINGS');if(typeof(title)=="undefined"||title=="undefined")
title=SUGAR.language.get('InboundEmail','LBL_TEST_SETTINGS');SI.testDlg.setHeader(title);SI.testDlg.setBody(SUGAR.language.get("app_strings","LBL_EMAIL_LOADING"));SI.testDlg.render(document.body);var Connect=YAHOO.util.Connect;if(Connect.url)URL=Connect.url+"&"+url;Connect.asyncRequest("POST",URL,{success:SI.testDlg._updateContent,failure:SI.testDlg.hide,scope:SI.testDlg});SI.testDlg.show();}
function isDataValid(formName,validateMonitoredFolder){var formObject=document.getElementById(formName);var errors=new Array();var out=new String();if(trim(formObject.server_url.value)==""){errors.push(SUGAR.language.get('app_strings','LBL_EMAIL_ERROR_SERVER'));}
if(trim(formObject.email_user.value)==""){errors.push(SUGAR.language.get('app_strings','LBL_EMAIL_ERROR_USER'));}
if(formObject.protocol.protocol==""){errors.push(SUGAR.language.get('app_strings','LBL_EMAIL_ERROR_PROTOCOL'));}
if(formObject.protocol.value=='imap'&&validateMonitoredFolder){if(trim(formObject.mailbox.value)==""){errors.push(SUGAR.language.get('app_strings','LBL_EMAIL_ERROR_MONITORED_FOLDER'));}}
if(formObject.auth_type&&formObject.email_password){if(formObject.auth_type.value==='oauth'){if(!formObject.external_oauth_connection_name.value||!formObject.external_oauth_connection_id.value){errors.push(SUGAR.language.get('app_strings','LBL_OAUTH_CONNECTION_NOT_SET'));}}
if(formObject.auth_type.value==='basic'){if(formObject.external_oauth_connection_name){formObject.external_oauth_connection_name='';}
if(formObject.external_oauth_connection_id){formObject.external_oauth_connection_id='';}
if(formObject.email_password.value===''&&!formObject.email_password.dataset.isValueSet){errors.push(SUGAR.language.get('app_strings','LBL_EMAIL_PASSWORD_NOT_SET'));}}}
if(formObject.port.value==""){errors.push(SUGAR.language.get('app_strings','LBL_EMAIL_ERROR_PORT'));}
if(errors.length>0){out=SUGAR.language.get('app_strings','LBL_EMAIL_ERROR_DESC');for(i=0;i<errors.length;i++){if(out!=""){out+="\n";}
out+=errors[i];}
alert(out);return false;}else{return true;}}
function getFoldersListForInboundAccount(module_name,action,pageTarget,width,height,mail_server,protocol,port,login,password,mailbox,ssl,personal,searchFieldValue,formName,extraParams){if(!formName)formName="testSettingsView";var words=getEncryptedPassword(login,password,mailbox);var isPersonal=(personal)?'true':'false';URL='index.php?'
+'module='+module_name
+'&to_pdf=1'
+'&action='+action
+'&target='+pageTarget
+'&target1='+pageTarget
+'&server_url='+mail_server
+'&email_user='+words[0]
+'&protocol='+protocol
+'&port='+port
+'&email_password='+encodeURIComponent(words[1])
+'&mailbox='+words[2]
+'&ssl='+ssl
+'&personal='+isPersonal
+'&searchField='+searchFieldValue;if(extraParams&&typeof extraParams==='object'&&Object.keys(extraParams).length){Object.keys(extraParams).forEach(function(key){URL+='&'+key+'='+(extraParams[key]||'');})}
var SI=SUGAR.inboundEmail;if(!SI.listDlg){SI.listDlg=new YAHOO.widget.SimpleDialog("selectFoldersDiv",{width:width+"px",draggable:true,dragOnly:true,close:true,constraintoviewport:true,modal:true,loadingText:SUGAR.language.get("app_strings","LBL_EMAIL_LOADING")});SI.listDlg._updateContent=function(o){var w=this.cfg.config.width.value+"px";this.setBody(o.responseText);SUGAR.util.evalScript(o.responseText);if(!SUGAR.isIE)
this.body.style.width=w}}
SI.listDlg.setHeader(SUGAR.language.get("app_strings","LBL_EMAIL_LOADING"));SI.listDlg.setBody('');SI.listDlg.render(document.body);var Connect=YAHOO.util.Connect;if(Connect.url)URL=Connect.url+"&"+url;Connect.asyncRequest("POST",URL,{success:SI.listDlg._updateContent,failure:SI.listDlg.hide,scope:SI.listDlg});SI.listDlg.show();SI.listDlg.center();}
function setPortDefault(){var prot=document.getElementById('protocol');var ssl=document.getElementById('ssl');var port=document.getElementById('port');var stdPorts=new Array("110","143","993","995");var stdBool=new Boolean(false);if(port.value==''){stdBool.value=true;}else{for(i=0;i<stdPorts.length;i++){if(stdPorts[i]==port.value){stdBool.value=true;}}}
if(stdBool.value==true){if(prot.value=='imap'&&ssl.checked==false){port.value="143";}else if(prot.value=='imap'&&ssl.checked==true){port.value='993';}else if(prot.value=='pop3'&&ssl.checked==false){port.value='110';}else if(prot.value=='pop3'&&ssl.checked==true){port.value='995';}}}
function toggle_monitored_folder(field){var field1=document.getElementById('protocol');var mailbox=document.getElementById('mailbox');var label_inbox=document.getElementById('label_inbox');var subscribeFolderButton=document.getElementById('subscribeFolderButton');var trashFolderRow=document.getElementById('trashFolderRow');var trashFolderRow1=document.getElementById('trashFolderRow1');var sentFolderRow=document.getElementById('sentFolderRow');if(field1.value=='imap'){mailbox.disabled=false;try{mailbox.style.display='';trashFolderRow.style.display='';sentFolderRow.style.display='';trashFolderRow1.style.display='';subscribeFolderButton.style.display='';}catch(e){};label_inbox.style.display='';}
else{mailbox.value="INBOX";mailbox.disabled=false;try{mailbox.style.display="none";trashFolderRow.style.display="none";sentFolderRow.style.display="none";trashFolderRow1.style.display="none";subscribeFolderButton.style.display="none";}catch(e){};label_inbox.style.display="none";}}