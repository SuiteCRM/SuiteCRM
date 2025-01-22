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
 */var yahooSlots=new Array();function addNewRowToView(id){var curRow=document.getElementById(id);var parent=curRow.parentNode;var newRow=document.createElement('tr');var newRow=parent.insertRow(parent.rows.length);var re=/studiorow[0-9]+/g;var cell=newRow.insertCell(0);cell.innerHTML=curRow.cells[0].innerHTML.replace(re,'studiorow'+slotCount);cell.className=curRow.cells[0].className;for(var j=1;j<curRow.cells.length;j++){var cell=newRow.insertCell(j);cell.innerHTML='&nbsp;';cell.className=curRow.cells[j].className;}
var index=parent.rows.length;for(var i=0;i<parent.rows.length;i++){if(parent.rows[i].id==id){index=i+1;}}
newRow.id='studiorow'+slotCount;if(typeof(curRow.parentId)=='undefined'){newRow.parentId=id;}else{newRow.parentId=curRow.parentId;}
if(index<parent.rows.length){parent.insertBefore(newRow,parent.rows[index]);}else{parent.appendChild(newRow);}
document.getElementById('add_'+newRow.parentId).value=1+parseInt(document.getElementById('add_'+newRow.parentId).value);slotCount++;}
function deleteRowFromView(id,index){var curRow=document.getElementById(id);curRow.parentNode.removeChild(curRow);if(typeof(curRow.parentId)=='undefined'){document.getElementById('form_'+id).value=-1;}else{document.getElementById('add_'+curRow.parentId).value=parseInt(document.getElementById('add_'+curRow.parentId).value)-1;}}
function addNewColToView(id,index){var curCol=document.getElementById(id);var index=curCol.cellIndex;var parent=curCol.parentNode;var cell=parent.insertCell(index+1);if(parent.parentNode.rows[parent.rowIndex+1])parent.parentNode.rows[parent.rowIndex+1].insertCell(index+1)
var re=/studiocol[0-9]+/g;cell.innerHTML='[NEW]';cell.className=curCol.className;if(typeof(curCol.parentId)=='undefined'){cell.parentId=id;}else{cell.parentId=curCol.parentId;}
document.getElementById('add_'+cell.parentId).value=1+parseInt(document.getElementById('add_'+cell.parentId).value);slotCount++;}
function deleteColFromView(id,index){var curCol=document.getElementById(id);var row=curCol.parentNode;var index=curCol.cellIndex;if(typeof(row.cells[index+1].parentId)=='undefined'){row.deleteCell(index);row.deleteCell(index-1);if(row.parentNode.rows[row.rowIndex+1]){row.parentNode.rows[row.rowIndex+1].deleteCell(index);row.parentNode.rows[row.rowIndex+1].deleteCell(index-1);}}else{row.deleteCell(index+1);if(row.parentNode.rows[row.rowIndex+1])row.parentNode.rows[row.rowIndex+1].deleteCell(index+1);}
document.getElementById('add_'+curCol.id).value=parseInt(document.getElementById('add_'+curCol.id).value)-1;}
var field_count_MSI=0;var studioLoaded=false;var dyn_field_count=0;function addNewFieldType(type){var select=document.getElementById('studio_display_type').options;for(var i=0;i<select.length;i++){if(select[i].value==type){return;}}
select[i]=new Option(type,type);}
function filterStudioFields(type){var table=document.getElementById('studio_fields');for(var i=0;i<table.rows.length;i++){children=table.rows[i].cells[0].childNodes;for(var j=0;j<children.length;j++){child=children[j];if(child.nodeName=='DIV'&&typeof(child.fieldType)!='undefined'){if(type=='all'){table.rows[i].style.display='';}else if(type=='custom'){if(child.isCustom){table.rows[i].style.display=''}else{table.rows[i].style.display='none';}}else{if(child.fieldType==type){table.rows[i].style.display=''}else{table.rows[i].style.display='none';}}}}}}
function addNewField(id,name,label,html,fieldType,isCustom,table_id,top){html=replaceAll(html,"&qt;",'"');html=replaceAll(html,"&sqt;","'");var table=document.getElementById(table_id);var row=false;if(top){row=table.insertRow(1);}else{row=table.insertRow(table.rows.length);}
var cell=row.insertCell(0);var div=document.createElement('div');div.className='slot';div.setAttribute('id',id);div.fieldType=fieldType;addNewFieldType(fieldType);div.isCustom=isCustom;div.style.width='100%';var textEl=document.createElement('input');textEl.setAttribute('type','hidden')
textEl.setAttribute('name','slot_field_'+field_count_MSI);textEl.setAttribute('id','slot_field_'+field_count_MSI);textEl.setAttribute('value','add:'+name);field_list_MSI['form_'+name]=textEl;document.studio.appendChild(textEl);div.innerHTML=label;var cell2=row.insertCell(1);var div2=document.createElement('div');setMouseOverForField(div,true);div2.style.display='none';div2.setAttribute('id',id+'b');html=html.replace(/(<input)([^>]*)/g,'$1 disabled readonly $2');html=html.replace(/(<select)([^>]*)/g,'$1 disabled readonly $2');html=html.replace(/(onclick=')([^']*)/g,'$1');div2.innerHTML+=html;cell.appendChild(div);cell2.appendChild(div2);field_count_MSI++;if(top){yahooSlots[id]=new ygDDSlot(id,"studio");}else{dyn_field_count++;}
return name;}
function removeFieldFromTable(field,table){var table=document.getElementById(table);var rows=table.rows;for(i=0;i<rows.length;i++){cells=rows[i].cells;for(j=0;j<cells.length;j++){cell=rows[i].cells[j];children=cell.childNodes;for(k=0;k<children.length;k++){child=children[k];if(child.nodeType==1){if(child.getAttribute('id')=='slot_'+field){table.deleteRow(i);return;}}}}}}
function setMouseOverForField(field,on){if(on){field.onmouseover=function(){$(this).tipTip({maxWidth:"auto",edgeOffset:10,content:document.getElementById(this.id+'b').innerHTML});};field.onmouseout=function(){return nd();};}else{field.onmouseover=function(){};field.onmouseout=function(){};}}
var lastIDClick='';var lastIDClickTime=0;var dblDelay=500;function wasDoubleClick(id){var d=new Date();var now=d.getTime();if(lastIDClick==id&&(now-lastIDClickTime)<dblDelay){lastIDClick='';return true;}
lastIDClickTime=now;lastIDClick=id;return false;}
function confirmNoSave(){return confirm(SUGAR.language.get('Studio','LBL_CONFIRM_UNSAVE'));}
var labelEdit=false;SUGAR.Studio=function(){this.labelEdit=false;this.lastLabel=false;}
SUGAR.Studio.prototype.endLabelEdit=function(id){if(id=='undefined')return;document.getElementById('span'+id).style.display='none';jstransaction.record('studioLabelEdit',{'id':id,'new':document.getElementById(id).value,'old':document.getElementById('label'+id).innerHTML});document.getElementById('label'+id).innerHTML=document.getElementById(id).value;document.getElementById('label_'+id).value=document.getElementById(id).value;document.getElementById('label'+id).style.display='';this.labelEdit=false;YAHOO.util.DragDropMgr.unlock();};SUGAR.Studio.prototype.undoLabelEdit=function(transaction){var id=transaction['id'];document.getElementById('span'+id).style.display='none';document.getElementById('label'+id).innerHTML=transaction['old'];document.getElementById('label_'+id).value=transaction['old'];};SUGAR.Studio.prototype.redoLabelEdit=function(transaction){var id=transaction['id'];document.getElementById('span'+id).style.display='none';document.getElementById('label'+id).innerHTML=transaction['new'];document.getElementById('label_'+id).value=transaction['new'];};SUGAR.Studio.prototype.handleLabelClick=function(id,count){if(this.lastLabel!=''){}
if(wasDoubleClick(id)||count==1){document.getElementById('span'+id).style.display='';document.getElementById(id).focus();document.getElementById(id).select();document.getElementById('label'+id).style.display='none';this.lastLabel=id;YAHOO.util.DragDropMgr.lock();}}
jstransaction.register('studioLabelEdit',SUGAR.Studio.prototype.undoLabelEdit,SUGAR.Studio.prototype.redoLabelEdit);SUGAR.Studio.prototype.save=function(formName,publish){ajaxStatus.showStatus(SUGAR.language.get('app_strings','LBL_SAVING'));var formObject=document.forms[formName];YAHOO.util.Connect.setForm(formObject);var cObj=YAHOO.util.Connect.asyncRequest('POST','index.php',{success:SUGAR.Studio.prototype.saved,failure:SUGAR.Studio.prototype.saved,timeout:5000,argument:publish});}
SUGAR.Studio.prototype.saved=function(o){if(o){ajaxStatus.showStatus(SUGAR.language.get('app_strings','LBL_SAVED'));window.setTimeout('ajaxStatus.hideStatus();',2000);if(o.argument){studiojs.publish();}else{document.location.reload();}}else{ajaxStatus.showStatus(SUGAR.language.get('Studio','LBL_FAILED_TO_SAVE'));window.setTimeout('ajaxStatus.hideStatus();',2000);}}
SUGAR.Studio.prototype.publish=function(){ajaxStatus.showStatus(SUGAR.language.get('Studio','LBL_PUBLISHING'));var cObj=YAHOO.util.Connect.asyncRequest('GET','index.php?to_pdf=1&module=Studio&action=Publish',{success:SUGAR.Studio.prototype.published,failure:SUGAR.Studio.prototype.published},null);}
SUGAR.Studio.prototype.published=function(o){if(o){ajaxStatus.showStatus(SUGAR.language.get('Studio','LBL_PUBLISHED'));window.setTimeout('ajaxStatus.hideStatus();',2000);document.location.reload();}else{ajaxStatus.showStatus(SUGAR.language.get('Studio','LBL_FAILED_PUBLISHED'));window.setTimeout('ajaxStatus.hideStatus();',2000);}}
var studiopopup=function(){return{display:function(){if(studiojs.popupVisible)return false;studiojs.popupVisible=true;var cObj=YAHOO.util.Connect.asyncRequest('GET','index.php?to_pdf=1&module=Studio&action=wizard&wizard=EditCustomFieldsWizard&option=CreateCustomFields&popup=true',{success:studiopopup.render,failure:studiopopup.render},null);},destroy:function(){studiojs.popup.hide();},evalScript:function(text){SUGAR.util.evalScript(text);},render:function(obj){if(obj){studiojs.popup=new YAHOO.widget.Dialog("dlg",{effect:{effect:YAHOO.widget.ContainerEffect.SLIDE,duration:.5},fixedcenter:false,constraintoviewport:false,underlay:"shadow",modal:true,close:true,visible:false,draggable:true,monitorresize:true});studiojs.popup.setBody(obj.responseText);studiojs.popupAvailable=true;studiojs.popup.render(document.body);studiojs.popup.center();studiojs.popup.beforeHideEvent.fire=function(e){studiojs.popupVisible=false;}
studiopopup.evalScript(obj.responseText);}}};}();var studiojs=new SUGAR.Studio();studiojs.popupAvailable=false;studiojs.popupVisible=false;var popupSave=function(o){var errorIndex=o.responseText.indexOf('[ERROR]');if(errorIndex>-1){var error=o.responseText.substr(errorIndex+7,o.responseText.length);ajaxStatus.showStatus(error);window.setTimeout('ajaxStatus.hideStatus();',2000);return;}
var typeIndex=o.responseText.indexOf('[TYPE]');var labelIndex=o.responseText.indexOf('[LABEL]');var dataIndex=o.responseText.indexOf('[DATA]');var errorIndex=o.responseText.indexOf('[ERROR]');var name=o.responseText.substr(6,typeIndex-6);var type=o.responseText.substr(typeIndex+6,labelIndex-(typeIndex+6));var label=o.responseText.substr(labelIndex+7,dataIndex-(labelIndex+7));var data=o.responseText.substr(dataIndex+6,o.responseText.length);addNewField('dyn_field_'+field_count_MSI,name,label,data,type,1,'studio_fields',true)};function submitCustomFieldForm(isPopup){if(typeof(document.popup_form.presave)!='undefined'){document.popup_form.presave();}
if(!check_form('popup_form'))return;if(isPopup){var callback={success:popupSave,failure:popupSave,argument:''}
YAHOO.util.Connect.setForm('popup_form');var cObj=YAHOO.util.Connect.asyncRequest('POST','index.php',callback);studiopopup.destroy();}else{document.popup_form.submit();}}
function deleteCustomFieldForm(isPopup){if(confirm("WARNING\nDeleting a custom field will delete all data related to that custom field. \nYou will still need to remove the field from any layouts you have added it to.")){document.popup_form.option.value='DeleteCustomField';document.popup_form.submit();}}
function dropdownChanged(value){if(typeof(app_list_strings[value])=='undefined')return;var select=document.getElementById('default_value').options;select.length=0;var count=0;for(var key in app_list_strings[value]){select[count]=new Option(app_list_strings[value][key],key);count++;}}
function customFieldChanged(){}
var populateCustomField=function(response){var div=document.getElementById('customfieldbody');if(response.status=0){div.innerHTML='Server Connection Failed';}else{validate['popup_form']=new Array();inputsWithErrors=new Array();div.innerHTML=response.responseText;studiopopup.evalScript(response.responseText);if(studiojs.popupAvailable){var region=YAHOO.util.Dom.getRegion('custom_field_table');studiojs.popup.cfg.setProperty('width',region.right-region.left+30+'px');studiojs.popup.cfg.setProperty('height',region.bottom-region.top+30+'px');studiojs.popup.render(document.body);studiojs.popup.center();studiojs.popup.show();}}};var populateCustomFieldCallback={success:populateCustomField,failure:populateCustomField,argument:1};var COBJ=false;function changeTypeData(type){document.getElementById('customfieldbody').innerHTML='<h2>Loading...</h2>';COBJ=YAHOO.util.Connect.asyncRequest('GET','index.php?module=Studio&popup=true&action=index&&ajax=editcustomfield&to_pdf=true&type='+type,populateCustomFieldCallback,null);}
function typeChanged(obj){changeTypeData(obj.options[obj.selectedIndex].value);}
function handle_duplicate(){document.popup_form.action.value='EditView';document.popup_form.duplicate.value='true';document.popup_form.submit();}
function forceRange(field,min,max){field.value=parseInt(field.value);if(field.value=='NaN')field.value=max;if(field.value>max)field.value=max;if(field.value<min)field.value=min;}
function changeMaxLength(field,length){field.maxLength=parseInt(length);field.value=field.value.substr(0,field.maxLength);}