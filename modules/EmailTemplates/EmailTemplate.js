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
 */var focus_obj=false;var label=SUGAR.language.get('app_strings','LBL_DEFAULT_LINK_TEXT');function remember_place(obj){focus_obj=obj;}
function showVariable(form){if(!form){form='EditView';}
document[form].variable_text.value=document[form].variable_name.options[document[form].variable_name.selectedIndex].value;}
function addVariables(the_select,the_module,form){the_select.options.length=0;for(var i=0;i<field_defs[the_module].length;i++){var new_option=document.createElement("option");new_option.value="$"+field_defs[the_module][i].name;new_option.text=field_defs[the_module][i].value;the_select.options.add(new_option,i);}
showVariable(form);}
function toggle_text_only(firstRun){if(typeof(firstRun)=='undefined')
firstRun=false;var text_only=document.getElementById('text_only');if(document.getElementById('toggle_textonly').checked==true){document.getElementById('body_text_div').style.display='none';document.getElementById('toggle_textarea_option').style.display='none';document.getElementById('text_div').style.display='block';text_only.value=1;}else{document.getElementById('body_text_div').style.display='inline';document.getElementById('toggle_textarea_option').style.display='inline';document.getElementById('text_div').style.display='none';text_only.value=0;}
update_textarea_button();}
function update_textarea_button(){if(document.getElementById('text_div').style.display=='none'){document.getElementById('toggle_textarea_elem').value=toggle_textarea_elem_values[0];}else{document.getElementById('toggle_textarea_elem').value=toggle_textarea_elem_values[1];}}
function toggle_textarea_edit(obj){if(document.getElementById('text_div').style.display=='none'){document.getElementById('text_div').style.display='block';}else{document.getElementById('text_div').style.display='none';}
update_textarea_button();}
function setTinyHTML(text){var tiny=tinyMCE.getInstanceById('body_text');if(tiny.getContent()!=null){tiny.setContent(text)}else{setTimeout(setTinyHTML(text),1000);}}
function stripTags(str){var theText=new String(str);if(theText!='undefined'){return theText.replace(/<\/?[^>]+>/gi,'');}}
function insert_variable_text(myField,myValue){if(document.selection){myField.focus();sel=document.selection.createRange();sel.text=myValue;}
else if(myField.selectionStart||myField.selectionStart=='0'){var startPos=myField.selectionStart;var endPos=myField.selectionEnd;myField.value=myField.value.substring(0,startPos)
+myValue
+myField.value.substring(endPos,myField.value.length);}else{myField.value+=myValue;}}
function insert_variable_html(text){SuiteEditor.insert(text);}
function insert_variable_html_link(text,url){var thelink="<a href='"+url+"' > "+text+" </a>";insert_variable_html(thelink);}
function insert_variable(text,elemId,forceIntoSubject){if(typeof forceIntoSubject==='undefined'){forceIntoSubject=false;}
if(elemId=='template_subject'||forceIntoSubject){var $subject=$('#'+elemId);var value=$subject.val();var caret=parseInt($subject.attr('data-caret-position'));var before=value.substring(0,caret);var after=value.substring(caret);$subject.val(before+$('select[name=variable_name]').val()+after);return;}
if(!elemId){throw'not element for insert variable';}
if(document.getElementById('toggle_textonly')&&document.getElementById('toggle_textonly').checked==true){insert_variable_text(document.getElementById('body_text_plain'),text);}else{SuiteEditor.insert(text,elemId);}}
var doGetCaretPosition=function(oField){var iCaretPos=0;if(document.selection){oField.focus();var oSel=document.selection.createRange();oSel.moveStart('character',-oField.value.length);iCaretPos=oSel.text.length;}
else if(oField.selectionStart||oField.selectionStart=='0')
iCaretPos=oField.selectionStart;return iCaretPos;}
var onClickTemplateSubject=function(elem){$(elem).attr('data-caret-position',doGetCaretPosition(elem));$('#insert_variable_to_subject_btn').show();$('#insert_variable_to_body_btn').hide();}
var onClickTemplateBody=function(){$('#insert_variable_to_subject_btn').hide();$('#insert_variable_to_body_btn').show();}
var $templateManagerDialogX=0;var $templateManagerDialogY=0;var $templateManagerDialog=null;function createTemplateManagerDialog(parent){$('#templateManagerDialog').dialog({width:'50%',position:{my:"left top",at:"left bottom",of:parent}});}
var showTemplateSaveMessages=function(msgs){$('#template_messages').html('');$.each(msgs,function(i,msg){$('#template_messages').append(SUGAR.language.translate('Campaigns',msg)+'<br>');});setTimeout(function(){$('#template_messages').hide(1000,function(){$('#template_messages').html('');$('#template_messages').show();});},3000);};function EmailTemplateController(action){var lastNameValue=$('#template_name').val();var lastSubjectValue=$('#template_subject').val();var revertValues=function(){$('#template_name').val(lastNameValue);$('#template_subject').val(lastSubjectValue);window.parent.$('.ui-dialog-content:visible').dialog('close');}
var save=function(update){if($('#template_name').val()==''){alert(SUGAR.language.translate('Campaigns','LBL_PROVIDE_WEB_TO_LEAD_FORM_FIELDS'));$('#template_name').focus();return;}
window.parent.$('.ui-dialog-content:visible').dialog('close');var func=emailTemplateCopyId||$('input[name="update_exists_template"]').prop('checked')?'update':'createCopy';$('#template_messages').html(SUGAR.language.translate('Campaigns','LBL_TEMPLATE_SAVING'));$.post('index.php?entryPoint=emailTemplateData&func=wizardUpdate&rand='+Math.random(),{'func':func,'emailTemplateId':emailTemplateCopyId?emailTemplateCopyId:$('#template_id').val(),'body_html':SuiteEditor.getValue(),'name':$('#template_name').val(),'subject':$('#template_subject').val(),},function(resp){resp=JSON.parse(resp);if(resp.error){console.error(resp.error);}
else{if(!emailTemplateCopyId&&func=='createCopy'){emailTemplateCopyId=resp.data.id;$('option[value='+resp.data.id+']').html($('#template_name').val());$('input[name="update_exists_template"]').prop('checked',true);}else{$('option[value='+resp.data.id+']').html($('#template_name').val());}
$('#template_messages').html('');if(resp.msgs.length){showTemplateSaveMessages(resp.msgs);}}});}
var create=function(){if($('#template_name').val()==''){alert(SUGAR.language.translate('Campaigns','LBL_PROVIDE_WEB_TO_LEAD_FORM_FIELDS'));return}
window.parent.$('.ui-dialog-content:visible').dialog('close');$('input[name="update_exists_template"]').prop('checked',false);var func=emailTemplateCopyId||$('input[name="update_exists_template"]').prop('checked')?'update':'createCopy';$('#template_messages').html(SUGAR.language.translate('Campaigns','LBL_TEMPLATE_SAVING'));$.post('index.php?entryPoint=emailTemplateData&rand='+Math.random(),{'func':func,'emailTemplateId':emailTemplateCopyId?emailTemplateCopyId:$('#template_id').val(),'body_html':SuiteEditor.getValue(),'name':$('#template_name').val(),'subject':$('#template_subject').val(),},function(resp){resp=JSON.parse(resp);if(resp.error){console.error(resp.error);}
else{if(!emailTemplateCopyId&&func=='createCopy'){emailTemplateCopyId=resp.data.id;$('#template_id').append('<option value="'+resp.data.id+'">'+resp.data.name+'</option>');$('#template_id').val(resp.data.id);$('input[name="update_exists_template"]').prop('checked',true);$('#LBL_SAVE_EMAIL_TEMPLATE_BTN').parent().removeClass('hidden');$('#LBL_SAVE_EMAIL_TEMPLATE_BTN').parent().next().removeClass('hidden');}
$('#template_messages').html('');if(resp.msgs.length){showTemplateSaveMessages(resp.msgs);}
$('#template_option_select').click();}});}
switch(action){case"create":create();break;case"save":save();break;default:break;}
console.log('EmailTemplateController()',action)}
function EmailTrackerController(action,campaignId){var _campaignId=campaignId;var lastURLValue=$('#url_text').val();var lastNameValue=$('#tracker_name').val();var lastSubjectValue=$('#template_subject').val();var revertValues=function(){$('#url_text').val(lastURLValue);$('#tracker_name').val(lastNameValue);$('#template_subject').val(lastSubjectValue);window.parent.$('.ui-dialog-content:visible').dialog('close');}
var create=function(){var trackerName=$('#url_text').val();var trackerURL=$('#tracker_url_add').val();if($('#url_text').val()==''||$('#tracker_url_add').val()==''){alert(SUGAR.language.translate('Campaigns','LBL_PROVIDE_WEB_TO_LEAD_FORM_FIELDS'));return;}
if(!trackerName){errors.push({field:'tracker_name',message:SUGAR.language.translate('Campaigns','ERR_REQUIRED_TRACKER_NAME')});}
if(!trackerURL){errors.push({field:'tracker_url',message:SUGAR.language.translate('Campaigns','ERR_REQUIRED_TRACKER_URL')});}
hideFieldErrorMessages();window.parent.$('.ui-dialog-content:visible').dialog('close');$.post('index.php?entryPoint=campaignTrackerSave',{module:'CampaignTrackers',record:'',action:'Save',campaign_id:_campaignId,tracker_name:trackerName,tracker_url:trackerURL,is_optout:$('#is_optout').prop('checked')?'on':'',response_json:true},function(resp){resp=JSON.parse(resp);if(resp.data.id){$('select[name="tracker_url"]').append('<option value="{'+trackerName+'}" data-id="'+resp.data.id+'" data-url="'+trackerURL+'">'+trackerName+' : '+trackerURL+'</option>');$('select[name="tracker_url"]').val('{'+trackerName+'}');$('#url_text').val('{'+trackerName+'}');}
setTrackerUrlSelectVisibility();});}
var save=function(){var trackerName=$('#url_text').val();var trackerURL=$('#tracker_url_add').val();if($('#url_text').val()==''||$('#tracker_url_add').val()==''){alert(SUGAR.language.translate('Campaigns','LBL_PROVIDE_WEB_TO_LEAD_FORM_FIELDS'));return;}
if(!trackerName){errors.push({field:'tracker_name',message:SUGAR.language.translate('Campaigns','ERR_REQUIRED_TRACKER_NAME')});}
if(!trackerURL){errors.push({field:'tracker_url',message:SUGAR.language.translate('Campaigns','ERR_REQUIRED_TRACKER_URL')});}
hideFieldErrorMessages();window.parent.$('.ui-dialog-content:visible').dialog('close');$.post('index.php?entryPoint=campaignTrackerSave',{module:'CampaignTrackers',record:$('select[name="tracker_url"] option:selected').attr('data-id'),action:'Save',campaign_id:_campaignId,tracker_name:trackerName,tracker_url:trackerURL,is_optout:$('#is_optout').prop('checked')?'on':'',response_json:true},function(resp){resp=JSON.parse(resp);if(resp.data.id){$('select[name="tracker_url"] option:selected').text(trackerName+' : '+trackerURL);$('select[name="tracker_url"] option:selected').attr(trackerName+' : '+trackerURL);$('select[name="tracker_url"] option:selected').val('{'+trackerName+'}');}
setTrackerUrlSelectVisibility();});}
switch(action){case"create":$('#url_text').val('');$('#tracker_name').val('');$('#tracker_url_add').val('');$('#is_optout').attr('checked',false);$('#tracker_url_add').removeAttr('disabled')
createTemplateManagerDialog($('#LBL_CREATE_TRACKER_BTN'));$('#templateManagerActionOK').val($('#LBL_CREATE_TRACKER_BTN').val());$('#templateManagerDialog').children('div').addClass('hidden');$('#emailTrackerDialog').removeClass('hidden');$('#templateManagerDialogActions').removeClass('hidden');$('#templateManagerActionOK').val(SUGAR.language.translate('Campaigns','LBL_CREATE_TRACKER_BTN'));$('#templateManagerActionOK').unbind();$('#templateManagerActionCancel').unbind();$('#templateManagerActionOK').click(create);$('#templateManagerActionCancel').click(revertValues);$('#templateManagerDialog').show();break;case"insert":if($('#trackerUrlSelect').val()=='-1'){alert(SUGAR.language.translate('Campaigns','LBL_SELECT_EMAIL_TRACKER'));return;}
var text=$('select[name="tracker_url"] option:selected').val();text=text.replace('{','');text=text.replace('}','');insert_variable_html_link(text,$('select[name="tracker_url"] option:selected').val());break;case"edit":if($('#trackerUrlSelect').val()=='-1'){alert(SUGAR.language.translate('Campaigns','LBL_SELECT_EMAIL_TRACKER'));return;}
var text=$('select[name="tracker_url"] option:selected').val();text=text.replace('{','');text=text.replace('}','');$('#url_text').val(text);$('#tracker_url_add').val($('select[name="tracker_url"] option:selected').attr('data-url'));$('#tracker_name').val('');$('#templateManagerActionOK').val(SUGAR.language.translate('Campaigns','LBL_EDIT_TRACKER_BTN'));createTemplateManagerDialog($('#LBL_CREATE_TRACKER_BTN'));$('#templateManagerDialog').children('div').addClass('hidden');$('#emailTrackerDialog').removeClass('hidden');$('#templateManagerDialogActions').removeClass('hidden');$('#templateManagerActionOK').unbind();$('#templateManagerActionCancel').unbind();$('#templateManagerActionOK').click(save);$('#templateManagerActionCancel').click(revertValues);$('#templateManagerDialog').show();break;default:break;}}
$(document).on("mousemove",function(event){$templateManagerDialogX=event.pageX;$templateManagerDialogY=event.pageY;});