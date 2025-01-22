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
 */var AlertObj=function(){this.title='Alert';this.options={};this.options.body=' ';this.options.url_redirect='';this.options.target_module='';this.options.type='info';};var Alerts=function(){};Alerts.prototype.replaceMessages=[];Alerts.prototype.enable=function(){var alert=new AlertObj();if(!("Notification"in window)){alert.title=SUGAR.language.translate('app_strings','MSG_BROWSER_NOTIFICATIONS_UNSUPPORTED');Alerts.prototype.show(alert);return;}
Notification.requestPermission(function(permission){if(permission==="granted"){alert.title=SUGAR.language.translate('app_strings','MSG_BROWSER_NOTIFICATIONS_ENABLED');}
else{alert.title=SUGAR.language.translate('app_strings','MSG_BROWSER_NOTIFICATIONS_DISABLED');}
Alerts.prototype.show(alert);});};Alerts.prototype.requestPermission=function(){if(!("Notification"in window)){return;}
Notification.requestPermission();};Alerts.prototype.show=function(AlertObj){Alerts.prototype.requestPermission();if(("Notification"in window)){if(Notification.permission==="granted"){if(typeof AlertObj.options!=="undefined"){if(typeof AlertObj.options.target_module!=="undefined"){SUGAR.themes.theme_name=undefined;AlertObj.options.icon='index.php?entryPoint=getImage&themeName='+
SUGAR.themes.theme_name+'&imageName='+
AlertObj.options.target_module+'s.gif';}
if(typeof AlertObj.options.type==="undefined"){AlertObj.options.type='info';}}
var notification=new Notification(AlertObj.title,AlertObj.options);if(typeof AlertObj.options!=="undefined"){if(typeof AlertObj.options.url_redirect!=="undefined"){notification.onclick=function(){window.open(AlertObj.options.url_redirect);}}}}
else{var message=AlertObj.title;if(typeof AlertObj.options!=="undefined"){if(typeof AlertObj.options.body!=="undefined"){message+='\n'+AlertObj.options.body;}
message+=SUGAR.language.translate('app_strings','MSG_JS_ALERT_MTG_REMINDER_CALL_MSG')+"\n\n";if(confirm(message)){if(typeof AlertObj.options!=="undefined"){if(typeof AlertObj.options.url_redirect!=="undefined"){window.location=AlertObj.options.url_redirect;}}}}}}};Alerts.prototype.addToManager=function(AlertObj){var url='index.php',name=AlertObj.title,description,url_redirect,is_read=0,target_module,type='info';if(typeof AlertObj.options!=="undefined"){if(typeof AlertObj.options.url_redirect!=="undefined"){url_redirect=AlertObj.options.url_redirect}
if(typeof AlertObj.options.body!=="undefined"){description=AlertObj.options.body}
if(typeof AlertObj.options.target_module!=="undefined"){target_module=AlertObj.options.target_module}
if(typeof AlertObj.options.type!=="undefined"){type=AlertObj.options.type}
if(typeof AlertObj.options.reminder_id!=="undefined"){reminder_id=AlertObj.options.reminder_id}}
$.post(url,{module:'Alerts',action:'add',name:name,description:description,url_redirect:url_redirect,is_read:is_read,target_module:target_module,reminder_id:reminder_id,type:type}).done(function(jsonData){data=JSON.parse(jsonData);if(typeof data!=='undefined'&&typeof data.result!=='undefined'&&data.result===1){Alerts.prototype.show(AlertObj);}}).fail(function(data){console.error(data);}).always(function(){Alerts.prototype.updateManager();});};Alerts.prototype.redirectToLogin=function(){var getQueryParams=function(qs){qs=qs.split('+').join(' ');var params={},tokens,re=/[?&]?([^=]+)=([^&]*)/g;while(tokens=re.exec(qs)){params[decodeURIComponent(tokens[1])]=decodeURIComponent(tokens[2]);}
return params;};var params=getQueryParams(document.location.search);params.entryPoint=undefined;if(params.entryPoint!=='Changenewpassword'&&params.module!=='Users'&&params.action!=='Login'){document.location.href='index.php?module=Users&action=Login&loginErrorMessage=LBL_SESSION_EXPIRED';return true;}
return false;};Alerts.prototype.updateManager=function(){var url='index.php?module=Alerts&action=get&to_pdf=1';$.ajax(url).done(function(data){if(data==='lost session'){Alerts.prototype.redirectToLogin();return false;}
for(var replaceMessage in Alerts.prototype.replaceMessages){data=data.replace(Alerts.prototype.replaceMessages[replaceMessage].search,Alerts.prototype.replaceMessages[replaceMessage].replace);}
var alertsDiv=$('.desktop_notifications #alerts');alertsDiv.html(data);var alerts=$('<div></div>');$(data).appendTo(alerts);var alertCount=$(alerts).children('.alert').length;var alertCountDiv=$('.alert_count');var desktopNotificationsDiv=$('.desktop_notifications');var alertButtonDiv=$('.alertsButton');alertCountDiv.html(alertCount);if(alertCount>0){alertsDiv.addClass('has-alerts');desktopNotificationsDiv.addClass('has-alerts');alertButtonDiv.removeClass('btn-').addClass('btn-danger');alertCountDiv.removeClass('hidden');}
else{desktopNotificationsDiv.removeClass('has-alerts');alertsDiv.removeClass('has-alerts');alertButtonDiv.removeClass('btn-danger').addClass('btn-success');alertCountDiv.addClass('hidden');}});};Alerts.prototype.markAsRead=function(id){var url='index.php?module=Alerts&action=markAsRead&record='+id+'&to_pdf=1';$.ajax(url).done(function(){Alerts.prototype.updateManager();});};$(document).ready(function(){Alerts.prototype.replaceMessages=[{search:SUGAR.language.translate("app","MSG_JS_ALERT_MTG_REMINDER_CALL_MSG"),replace:""},{search:SUGAR.language.translate("app","MSG_JS_ALERT_MTG_REMINDER_MEETING_MSG"),replace:""}];var updateMissed=function(){Alerts.prototype.updateManager();setTimeout(updateMissed,60000);};setTimeout(updateMissed,2000);});