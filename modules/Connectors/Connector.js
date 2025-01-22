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
 */function run_test(source_id){var callback={success:function(data){var resultDiv=document.getElementById(source_id+'_result');resultDiv.innerHTML='<b>'+data.responseText+'</b>';},failure:function(data){var resultDiv=document.getElementById(source_id+'_result');resultDiv.innerHTML='<b>'+SUGAR.language.get('app_strings','ERROR_UNABLE_TO_RETRIEVE_DATA')+'</b>';},timeout:300000}
var resultDiv=document.getElementById(source_id+'_result');resultDiv.innerHTML='<img src=themes/default/images/sqsWait.gif>';document.ModifyProperties.source_id.value=source_id;document.ModifyProperties.action.value='RunTest';YAHOO.util.Connect.setForm(document.ModifyProperties);var cObj=YAHOO.util.Connect.asyncRequest('POST','index.php?module=Connectors',callback);document.ModifyProperties.action.value='SaveModifyProperties';}
var widgetTimout;function dswidget_open(elt){var wdiget_div=document.getElementById('dswidget_div');var objX=findPosX(elt);var objY=findPosY(elt);wdiget_div.style.top=(objY+15)+'px';wdiget_div.style.left=(objX)+'px';wdiget_div.style.display='block';}
function dswidget_close(){widgetTimout=setTimeout("hide_widget()",500);}
function hide_widget(){var wdiget_div=document.getElementById('dswidget_div');wdiget_div.style.display='none';}
function clearButtonTimeout(){if(widgetTimout){clearTimeout(widgetTimout);}}
function findPosX(obj){var curleft=0;if(obj.offsetParent){while(obj.offsetParent){curleft+=obj.offsetLeft;obj=obj.offsetParent;}
if(obj!=null)
curleft+=obj.offsetLeft;}
else if(obj.x)
curleft+=obj.x;return curleft;}
function findPosY(obj){var curtop=0;if(obj.offsetParent){while(obj.offsetParent){curtop+=obj.offsetTop;obj=obj.offsetParent;}
if(obj!=null)
curtop+=obj.offsetTop;}
else if(obj.y)
curtop+=obj.y;return curtop;}