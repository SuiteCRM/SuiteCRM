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
 */(function($){$.fn.EmailsComposeViewModal=function(options){"use strict";var self=this;var opts=$.extend({},$.fn.EmailsComposeViewModal.defaults,options);self.handleClick=function(e){"use strict";var self=this;self.emailComposeView=null;var opts=$.extend({},$.fn.EmailsComposeViewModal.defaults);var composeBox=$('<div></div>').appendTo(opts.contentSelector);composeBox.messageBox({"showHeader":false,"showFooter":false,"size":'lg'});composeBox.setBody('<div class="email-in-progress"><img src="themes/'+SUGAR.themes.theme_name+'/images/loading.gif"></div>');composeBox.show();$.ajax({type:"GET",cache:false,url:'index.php?module=Emails&action=ComposeView&in_popup=1'}).done(function(data){if(data.length===0){console.error("Unable to display ComposeView");composeBox.setBody(SUGAR.language.translate('','ERR_AJAX_LOAD'));return;}
composeBox.setBody(data);self.emailComposeView=composeBox.controls.modal.body.find('.compose-view').EmailsComposeView();$(self.emailComposeView).on('sentEmail',function(event,composeView){composeBox.hide();composeBox.remove();});$(self.emailComposeView).on('disregardDraft',function(event,composeView){if(typeof messageBox!=="undefined"){var mb=messageBox({size:'lg'});mb.setTitle(SUGAR.language.translate('','LBL_CONFIRM_DISREGARD_DRAFT_TITLE'));mb.setBody(SUGAR.language.translate('','LBL_CONFIRM_DISREGARD_DRAFT_BODY'));mb.on('ok',function(){mb.remove();composeBox.hide();composeBox.remove();});mb.on('cancel',function(){mb.remove();});mb.show();}else{if(confirm(self.translatedErrorMessage)){composeBox.hide();composeBox.remove();}}});composeBox.on('cancel',function(){composeBox.remove();});composeBox.on('hide.bs.modal',function(){composeBox.remove();});$("#emails_email_templates_name").change(function(){$.fn.EmailsComposeView.onTemplateChange()});}).fail(function(data){composeBox.controls.modal.content.html(SUGAR.language.translate('','LBL_EMAIL_ERROR_GENERAL_TITLE'));});return $(self);};self.construct=function(){"use strict";$(opts.buttonSelector).click(self.handleClick)};self.destruct=function(){};self.construct();return $(self);};$.fn.openComposeViewModal=function(source){"use strict";window.event.preventDefault();window.event.stopImmediatePropagation();var self=this;self.emailComposeView=null;var opts=$.extend({},$.fn.EmailsComposeViewModal.defaults);var composeBox=$('<div></div>').appendTo(opts.contentSelector);composeBox.messageBox({"showHeader":false,"showFooter":false,"size":'lg'});composeBox.setBody('<div class="email-in-progress"><img src="themes/'+SUGAR.themes.theme_name+'/images/loading.gif"></div>');composeBox.show();var relatedId=$('[name="record"]').val();var ids='&ids=';if($(source).attr('data-record-id')!==''){ids=ids+$(source).attr('data-record-id');relatedId=$(source).attr('data-record-id');}
else{var inputs=document.MassUpdate.elements;for(var i=0;i<inputs.length;i++){if(inputs[i].name==='mass[]'&&inputs[i].checked){ids=ids+inputs[i].value+',';}}}
var targetModule=currentModule;if($(source).attr('data-module')!==''){targetModule=$(source).attr('data-module');}
var url='index.php?module=Emails&action=ComposeView&in_popup=1&targetModule='+targetModule+ids+'&relatedModule='+currentModule+'&relatedId='+relatedId;$.ajax({type:"GET",cache:false,url:url}).done(function(data){if(data.length===0){console.error("Unable to display ComposeView");composeBox.setBody(SUGAR.language.translate('','ERR_AJAX_LOAD'));return;}
composeBox.setBody(data);self.emailComposeView=composeBox.controls.modal.body.find('.compose-view').EmailsComposeView();var targetCount=0;var targetList='';var populateModuleName='';var populateEmailAddress='';var populateModule='';var populateModuleRecord='';var dataEmailName=$(source).attr('data-module-name');var dataEmailAddress=$(source).attr('data-email-address');$('.email-compose-view-to-list').each(function(){if($('.email-relate-target'.length)){populateModule=$('.email-relate-target').attr('data-relate-module');populateModuleRecord=$('.email-relate-target').attr('data-relate-id');populateModuleName=$('.email-relate-target').attr('data-relate-name');}
else{populateModuleName=$(this).attr('data-record-name');if(dataEmailName!==''){populateModuleName=dataEmailName;}
populateModule=$(this).attr('data-record-module');populateModuleRecord=$(this).attr('data-record-id');if(populateModuleName===''){populateModuleName=populateEmailAddress;}}
populateEmailAddress=$(this).attr('data-record-email');if(dataEmailAddress!==''){populateEmailAddress=dataEmailAddress;}
if(populateEmailAddress!==''){if(targetCount>0){targetList=targetList+',';}
targetList=targetList+dataEmailName+' <'+populateEmailAddress+'>';targetCount++;}});if(targetCount>0){$(self.emailComposeView).find('#to_addrs_names').val(targetList);}
if(targetCount<2){$(self.emailComposeView).find('#parent_type').val(populateModule);$(self.emailComposeView).find('#parent_name').val(populateModuleName);$(self.emailComposeView).find('#parent_id').val(populateModuleRecord);}
$(self.emailComposeView).on('sentEmail',function(event,composeView){composeBox.hide();composeBox.remove();});$(self.emailComposeView).on('disregardDraft',function(event,composeView){if(typeof messageBox!=="undefined"){var mb=messageBox({size:'lg'});mb.setTitle(SUGAR.language.translate('','LBL_CONFIRM_DISREGARD_DRAFT_TITLE'));mb.setBody(SUGAR.language.translate('','LBL_CONFIRM_DISREGARD_DRAFT_BODY'));mb.on('ok',function(){mb.remove();composeBox.hide();composeBox.remove();});mb.on('cancel',function(){mb.remove();});mb.show();}else{if(confirm(self.translatedErrorMessage)){composeBox.hide();composeBox.remove();}}});composeBox.on('cancel',function(){composeBox.remove();});composeBox.on('hide.bs.modal',function(e){e.preventDefault();var mb=messageBox({size:'lg'});mb.setTitle(SUGAR.language.translate('','LBL_CONFIRM_DISREGARD_EMAIL_TITLE'));mb.setBody(SUGAR.language.translate('','LBL_CONFIRM_DISREGARD_EMAIL_BODY'));mb.on('ok',function(){mb.remove();composeBox.hide();composeBox.remove();});mb.on('cancel',function(){mb.remove();});mb.show();});$("#emails_email_templates_name").change(function(){$.fn.EmailsComposeView.onTemplateChange()});}).fail(function(data){composeBox.controls.modal.content.html(SUGAR.language.translate('','LBL_EMAIL_ERROR_GENERAL_TITLE'));});return $(self);};$.fn.EmailsComposeViewModal.defaults={'selected':'INBOX','buttonSelector':'[data-action=emails-show-compose-modal]','contentSelector':'#content'};}(jQuery));