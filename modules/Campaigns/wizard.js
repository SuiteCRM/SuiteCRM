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
 */function hide(divname){var elem1=document.getElementById(divname);elem1.style.display='none';}
function show(div){var elem1=document.getElementById(div);if(elem1){elem1.style.display='';}}
function showdiv(div){hideall();show(div);}
function hideall(){var last_val=document.getElementById('wiz_total_steps');var last=parseInt(last_val.value);for(i=1;i<=last;i++){hide('step'+i);}}
function showfirst(wiz_mode){showdiv('step1');var current_step=document.getElementById('wiz_current_step');current_step.value="1";var save_button=document.getElementById('wiz_submit_button');var next_button=document.getElementById('wiz_next_button');var save_button_div=document.getElementById('save_button_div');var next_button_div=document.getElementById('next_button_div');var back_button_div=document.getElementById('back_button_div');if(typeof document.getElementById('wizform').direction!='undefined'){save_button.disabled=true;back_button_div.style.display='none';save_button_div.style.display='none';next_button.focus();if(wiz_mode=='marketing'){back_button_div.style.display='';}}
else{back_button_div.style.display='none';}
hilite(current_step.value);}
function navigate(direction,noValidation,noSave){if(typeof noValidation=='undefined'){noValidation=false;}
if(typeof noSave=='undefined'){noSave=false;}
var current_step=document.getElementById('wiz_current_step');var currentValue=parseInt(current_step.value);var campaignId=$('input[name="record"]').val();if(!campaignId){campaignId=$('input[name="campaign_id"]').val();}
if(direction=='back'&&current_step.value==1){window.history.back();return;}
var validationResult=validate_wiz(current_step.value,direction);if(noValidation||validationResult){if(direction=='back'){current_step.value=currentValue-1;}
if(direction=='next'){if(currentValue==1){if(!campaignId){if(typeof document.getElementById('wizform').direction!='undefined'){if(!noSave){campaignCreateAndRefreshPage();var wizardCurrentStep=$('.nav-steps.selected').attr('data-nav-step');if($('div.moduleTitle h2').text().indexOf($('#name').val())==-1){$('div.moduleTitle h2').text($('div.moduleTitle h2').text()+' '+$('#name').val());}}}}
else{if(!noSave){campaignUpdate();}}}
current_step.value=currentValue+1;}
if(direction=='direct'){}
showdiv("step"+current_step.value);hilite(current_step.value);var total=document.getElementById('wiz_total_steps').value;var save_button=document.getElementById('wiz_submit_button');var finish_button=document.getElementById('wiz_submit_finish_button');var back_button_div=document.getElementById('back_button_div');var save_button_div=document.getElementById('save_button_div');var next_button_div=document.getElementById('next_button_div');if(current_step.value==total){save_button.disabled=false;back_button_div.style.display='';save_button_div.style.display='';next_button_div.style.display='none';if(finish_button){finish_button.style.display='none';}
if(typeof campaignBudget!='undefined'&&campaignBudget){finish_button.style.display='';}}else{if(current_step.value<1){back_button_div.style.display='none';}else{back_button_div.style.display='';}
var next_button=document.getElementById('wiz_next_button');if(typeof campaignBudget!='undefined'&&campaignBudget){var targetListStep=3;}
else{var targetListStep=2;}
if(current_step.value==targetListStep){if(typeof document.getElementById('wizform').direction!='undefined'){next_button_div.style.display='none';save_button_div.style.display='';$('#wiz_submit_button').removeAttr('disabled');}}
else{next_button_div.style.display='';save_button_div.style.display='none';}
next_button.focus();}}else{}
return false;}
function campaignCreateAndRefreshPage(){var wizform=document.getElementById('wizform');if(typeof wizform.direction!='undefined'){wizform.action.value='WizardNewsletterSave';wizform.direction.value='continue_targetList';}else{wizform.action.value='WizardNewsletterSave';wizform.direction.value='continue';}
$.post($('#wizform').attr('action'),$('#wizform').serialize(),function(data){var re=/{"record":"\w{1,}-\w{1,}-\w{1,}-\w{1,}-\w{1,}"}/g;var found=data.match(re);if(found==null){console.log('Error getting record id');}else{var response=jQuery.parseJSON(found[0]);$('input[name="record"]').val(response.record);$('input[name="campaign_id"]').val(response.record);$('input[name="action"]').val('WizardTargetListSave');}});}
function campaignUpdate(){var wizform=document.getElementById('wizform');wizform.action.value='WizardNewsletterSave';wizform.direction.value='continue';$.post($('#wizform').attr('action'),$('#wizform').serialize(),function(){});}
var already_linked='';function hilite(hilite){}
function link_navs(beg,end){if(beg==''){beg=1;}
if(end==''){var last=document.getElementById('wiz_total_steps').value;end=last;}
beg=parseInt(beg);end=parseInt(end);for(i=beg;i<=end;i++){var nav_step=document.getElementById('nav_step'+i);}}
function direct(stepnumber){var current_step=document.getElementById('wiz_current_step');var currentValue=parseInt(current_step.value);if(validate_wiz(current_step.value,'direct')){current_step.value=stepnumber;navigate('direct');}else{}}
function validate_wiz(step,direction){var total=document.getElementById('wiz_total_steps').value;var wiz_message=document.getElementById('wiz_message');if(direction=='back'){if(step=='1'){var msg=SUGAR.language.get('mod_strings','LBL_WIZARD_FIRST_STEP_MESSAGE');wiz_message.innerHTML="<font color=\'red\' size=\'2\'><b>"+msg+"</b></font>";return false;}else{wiz_message.innerHTML='';}}
if(direction=='next'){if(step==total){var msg=SUGAR.language.get('mod_strings','LBL_WIZARD_LAST_STEP_MESSAGE');wiz_message.innerHTML="<font color=\'red\' size=\'2\'><b>"+msg+"</b></font>";return false;}else{wiz_message.innerHTML='';}}
if(direction=='direct'){}
if((direction!='direct')&&(window.validate_wiz_form)&&(!validate_wiz_form('step'+step))){return false;}
return true;}
var showEmailTemplateAttachments=function(attachments,lblLnkRemove){var html='';$(attachments).each(function(i,attachment){if(attachment.filename){var secureLink='index.php?entryPoint=download&id='+attachment.id+'&type=Notes';html+='<input type="checkbox" name="remove_attachment[]" value="'+attachment.id+'"> '+lblLnkRemove+'&nbsp;&nbsp;';html+='<a href="'+secureLink+'" target="_blank">'+attachment.filename+'</a><br>';}});$('#attachments_container').html(html);};var onEmailTemplateChange=function(elem,namePrefixCopyOf,templateIdDefault,callback){var lblLnkRemove='Remove';var autoCheckUpdateCheckbox=function(){if(!$('#template_id').val()){$('input[name="update_exists_template"]').prop('checked',false);$('input[name="update_exists_template"]').prop('disabled',true);}
else{$('input[name="update_exists_template"]').prop('disabled',false);}}
autoCheckUpdateCheckbox();if($('input[name="update_exists_template"]').prop('checked')){namePrefixCopyOf='';}
var emailTemplateId=$(elem).val()?$(elem).val():(typeof templateIdDefault!='undefined'&&templateIdDefault?templateIdDefault:null);if(emailTemplateId){$('#email_template_view_html').html('');$('#email_template_view').html('');$.post('index.php?entryPoint=emailTemplateData',{'campaignId':$('input[name="campaign_id"]').val(),'emailTemplateId':emailTemplateId},function(resp){var results=JSON.parse(resp);if(!results.error){$('#email_template_view_html').html(results.data.body_html);$('#email_template_view').html(results.data.body);var htmlCode=$('<textarea />').html(results.data.body_html).text();SuiteEditor.apply(htmlCode);$('#template_id').val(results.data.id);$('input[name="update_exists_template"]').prop('checked',true);autoCheckUpdateCheckbox();$('#template_name').val(($('#update_exists_template').prop('checked')?namePrefixCopyOf:'')+results.data.name);$('#template_subject').val(results.data.subject);showEmailTemplateAttachments(results.data.attachments,lblLnkRemove);if(typeof callback!='undefined'){callback();}}
else{console.log(results.error);}});}};var onScheduleClick=function(e){$('input[name="action"]').val('WizardMarketingSave');$('input[name="module"]').val('Campaigns');$('#show_wizard_summary').val('1');$('#sendMarketingEmailSchedule').val('1');$('#sendMarketingEmailTest').val('0');$('#wizform').submit();};var onSendAsTestClick=function(e,campaignId,marketingId){$('input[name="action"]').val('WizardMarketingSave');$('input[name="module"]').val('Campaigns');$('#show_wizard_summary').val('1');$('#sendMarketingEmailSchedule').val('0');$('#sendMarketingEmailTest').val('1');if($('#wizform input[name="marketing_id"]').length==0){$('#wizform').append('<input type="hidden" name="marketing_id">');}
$('#wizform input[name="marketing_id"]').val(marketingId);$('#wizform').submit();};var addTargetListData=function(id){var result_data={"form_name":'wizform',"name_to_value_array":{popup_target_list_id:id,popup_target_list_name:targetListDataJSON[id].name,popup_target_list_type:targetListDataJSON[id].type,popup_target_list_count:targetListDataJSON[id].count,},"passthru_data":Object(),"popupConfirm":0};set_return_prospect_list(result_data);};$(function(){$('input').keydown(function(event){if(event.keyCode==13){event.preventDefault();return false;}
return true;});});this.GUID=function(){var characters=['a','b','c','d','e','f','1','2','3','4','5','6','7','8','9'];var format='0000000-0000-0000-0000-00000000000';var z=Array.prototype.map.call(format,function($obj){var min=0;var max=characters.length-1;if($obj=='0'){var index=Math.round(Math.random()*(max-min)+min);$obj=characters[index];}
return $obj;}).toString().replace(/(,)/g,'');return z}