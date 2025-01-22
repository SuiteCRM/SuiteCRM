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
 */SUGAR.dashlets=function(){return{postForm:function(theForm,callback){var success=function(data){if(data){callback(data.responseText);}}
YAHOO.util.Connect.setForm(theForm);var cObj=YAHOO.util.Connect.asyncRequest('POST','index.php',{success:success,failure:success});return false;},callMethod:function(dashletId,methodName,postData,refreshAfter,callback){ajaxStatus.showStatus(SUGAR.language.get('app_strings','LBL_SAVING'));response=function(data){ajaxStatus.hideStatus();if(refreshAfter)SUGAR.mySugar.retrieveDashlet(dashletId);if(callback){callback(data.responseText);}}
post='to_pdf=1&module=Home&action=CallMethodDashlet&method='+methodName+'&id='+dashletId+'&'+postData;var cObj=YAHOO.util.Connect.asyncRequest('POST','index.php',{success:response,failure:response},post);}};}();if(SUGAR.util.isTouchScreen()&&typeof iScroll=='undefined'){with(document.getElementsByTagName("head")[0].appendChild(document.createElement("script"))){setAttribute("id","newScript",0);setAttribute("type","text/javascript",0);setAttribute("src","include/javascript/iscroll.js",0);}}