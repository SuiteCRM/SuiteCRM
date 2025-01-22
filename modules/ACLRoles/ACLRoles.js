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
 */var aclviewer=function(){var lastDisplay='';return{view:function(role_id,role_module){YAHOO.util.Connect.asyncRequest('POST','index.php',{'success':aclviewer.display,'failure':aclviewer.failed},'module=ACLRoles&action=EditRole&record='+role_id+'&category_name='+role_module);ajaxStatus.showStatus(SUGAR.language.get('app_strings','LBL_REQUEST_PROCESSED'));},save:function(form_name){var formObject=document.getElementById(form_name);YAHOO.util.Connect.setForm(formObject);YAHOO.util.Connect.asyncRequest('POST','index.php',{'success':aclviewer.postSave,'failure':aclviewer.failed});ajaxStatus.showStatus(SUGAR.language.get('app_strings','LBL_SAVING'));},postSave:function(o){SUGAR.util.globalEval(o.responseText);aclviewer.view(result['role_id'],result['module']);},display:function(o){aclviewer.lastDisplay='';ajaxStatus.flashStatus('Done');document.getElementById('category_data').innerHTML=o.responseText;},failed:function(){ajax.flashStatus('Could Not Connect');},toggleDisplay:function(id){if(aclviewer.lastDisplay!=''&&typeof(aclviewer.lastDisplay)!='undefined'){aclviewer.hideDisplay(aclviewer.lastDisplay);}
if(aclviewer.lastDisplay!=id){aclviewer.showDisplay(id);aclviewer.lastDisplay=id;}else{aclviewer.lastDisplay='';}},hideDisplay:function(id){document.getElementById(id).style.display='none';document.getElementById(id+'link').style.display='';},showDisplay:function(id){document.getElementById(id).style.display='';document.getElementById(id+'link').style.display='none';}};}();