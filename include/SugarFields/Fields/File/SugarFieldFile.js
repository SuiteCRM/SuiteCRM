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
 */if(typeof(SUGAR.field)=='undefined'){SUGAR.field=new Object();}
if(typeof(SUGAR.field.file)=='undefined'){SUGAR.field.file={deleteAttachment:function(elemBaseName,docTypeName,elem){ajaxStatus.showStatus(SUGAR.language.get("app_strings","LBL_REMOVING_ATTACHMENT"));elem.form.deleteAttachment.value=1;elem.form.action.value="deleteattachment";var callback=SUGAR.field.file.deleteAttachmentCallbackGen(elemBaseName,docTypeName);var success=function(data){if(data){callback(data.responseText);}}
YAHOO.util.Connect.setForm(elem.form);var cObj=YAHOO.util.Connect.asyncRequest('POST','index.php',{success:success,failure:success});elem.form.deleteAttachment.value=0;elem.form.action.value="";},deleteAttachmentCallbackGen:function(elemBaseName,docTypeName){return function(text){if(text=='true'){document.getElementById(elemBaseName+'_new').style.display='';ajaxStatus.hideStatus();document.getElementById(elemBaseName+'_old').innerHTML='';if(docTypeName){document.getElementById(docTypeName).disabled=false;}
document.getElementById(elemBaseName).value='';}else{document.getElementById(elemBaseName+'_new').style.display='none';ajaxStatus.flashStatus(SUGAR.language.get('app_strings','ERR_REMOVING_ATTACHMENT'),2000);}}},checkEapiLogin:function(res){var failedLogins=YAHOO.lang.JSON.parse(res.responseText);if(failedLogins.length==0){return;}
for(var idx in failedLogins){if(confirm(failedLogins[idx].label)){window.open(failedLogins[idx].checkURL,'EAPM_CHECK_'+idx);}else{document.getElementById(res.argument.docTypeName).value='Sugar';document.getElementById(res.argument.docTypeName).onchange();}}},setupEapiShowHide:function(elemBaseName,docTypeName,formName){var externalSearchToggle=function(){var moreElem=document.getElementById(elemBaseName+"_more");var hideMore=(moreElem.style.display=='none');if(hideMore){moreElem.style.display='';document.getElementById(elemBaseName+'_less').style.display='none';document.getElementById(elemBaseName+'_remoteNameSpan').style.display='none';document.getElementById(elemBaseName+'_file').disabled=false;}else{moreElem.style.display='none';document.getElementById(elemBaseName+'_less').style.display='';document.getElementById(elemBaseName+'_remoteNameSpan').style.display='';document.getElementById(elemBaseName+'_file').disabled=true;}}
var showHideFunc=function(){var docShowHideElem=document.getElementById(elemBaseName+"_externalApiSelector");var dropdownValue=document.getElementById(docTypeName).value;if(typeof(SUGAR.eapm)!='undefined'&&typeof(SUGAR.eapm[dropdownValue])!='undefined'&&typeof(SUGAR.eapm[dropdownValue].docSearch)!='undefined'&&SUGAR.eapm[dropdownValue].docSearch){docShowHideElem.style.display='';YAHOO.util.Connect.asyncRequest('GET','index.php?module=EAPM&action=CheckLogins&to_pdf=1&api='+dropdownValue,{success:SUGAR.field.file.checkEapiLogin,argument:{'elemBaseName':elemBaseName,'docTypeName':docTypeName}});YAHOO.util.Connect.asyncRequest('GET','index.php?module=EAPM&action=flushFileCache&to_pdf=1&api='+dropdownValue,{});}else{docShowHideElem.style.display='none';document.getElementById(elemBaseName+'_file').disabled=false;}
sqs_objects[formName+"_"+elemBaseName+"_remoteName"].api=dropdownValue;var secLevelBoxElem=document.getElementById(elemBaseName+'_securityLevelBox');var secLevelElem=document.getElementById(elemBaseName+'_securityLevel');secLevelElem.options.length=0;if(SUGAR.eapm[dropdownValue]&&SUGAR.eapm[dropdownValue].sharingOptions){var opts=SUGAR.eapm[dropdownValue].sharingOptions;var i=0;for(idx in opts){secLevelElem.options[i]=new Option(SUGAR.language.get('app_strings',opts[idx]),idx,false,false);i++;}
secLevelBoxElem.style.display='';}else{secLevelBoxElem.style.display='none';}}
document.getElementById(docTypeName).onchange=showHideFunc;document.getElementById(elemBaseName+'_externalApiLabel').onclick=externalSearchToggle;showHideFunc();},openPopup:function(elemBaseName){window.open('index.php?module=Documents&action=extdoc&isPopup=1&elemBaseName='+elemBaseName+'&apiName='+document.getElementById('doc_type').value,'sugarPopup','width=600,height=400,menubar=no,toolbar=no,status=no,resizeable=yes,scrollbars=yes');},clearRemote:function(elemBaseName){document.getElementById('doc_id').value='';document.getElementById(elemBaseName).value='';document.getElementById(elemBaseName+'_remoteName').value='';document.getElementById('doc_url').value='';},populateFromPopup:function(elemBaseName,docId,docName,docUrl,docDirectUrl){document.getElementById('doc_id').value=docId;document.getElementById(elemBaseName).value=docId;document.getElementById(elemBaseName+'_remoteName').value=docName;document.getElementById('doc_url').value=docUrl;},getFileExtension:function(fileName){var lastindex=fileName.lastIndexOf(".");if(lastindex==-1)
return'';else
return fileName.substr(++lastindex);},isFileExtensionValid:function(fileName){var docType=document.getElementById('doc_type').value;var fileExtension=this.getFileExtension(fileName);if(typeof(SUGAR.eapm[docType])=='undefined'||!SUGAR.eapm[docType].restrictUploadsByExtension){return true;}
var whiteSuffixlist=SUGAR.eapm[docType]['restrictUploadsByExtension'];if(whiteSuffixlist.constructor==Array){var results=false;for(var i=0;i<whiteSuffixlist.length;i++){if(fileExtension.toLowerCase()==whiteSuffixlist[i].toLowerCase()){return true;}}}
return results;},checkFileExtension:function(e,obj){var sff=SUGAR.field.file;var fileEl=document.getElementById(obj.fileEl);var fileName=fileEl.value;var isValid=sff.isFileExtensionValid(fileName);if(!isValid&&fileName!=''){var errorPannel=new YAHOO.widget.SimpleDialog('sugarMsgWindow',{width:'240px',visible:true,fixedcenter:true,constraintoviewport:true,draggable:true,type:'alert',modal:true,id:'sugarMsgWindow',close:true});errorPannel.setBody(SUGAR.language.get("app_strings","LBL_INVALID_FILE_EXTENSION"));errorPannel.render(document.body);errorPannel.show();fileEl.value='';document.getElementById(obj.targEl).value='';}}}}