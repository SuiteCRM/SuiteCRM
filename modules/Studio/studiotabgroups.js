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
 */var subtabCount=[];var subtabModules=[];var tabLabelToValue=[];StudioTabGroup=function(){this.lastEditTabGroupLabel=-1;};StudioTabGroup.prototype.editTabGroupLabel=function(id,done){if(!done){if(this.lastEditTabGroupLabel!=-1)StudioTabGroup.prototype.editTabGroupLabel(this.lastEditTabGroupLabel,true);document.getElementById('tabname_'+id).style.display='none';document.getElementById('tablabel_'+id).style.display='';document.getElementById('tabother_'+id).style.display='none';try{document.getElementById('tablabel_'+id).focus();}
catch(er){}
this.lastEditTabGroupLabel=id;YAHOO.util.DragDropMgr.lock();}else{this.lastEditTabGroupLabel=-1;document.getElementById('tabname_'+id).innerHTML=escape(document.getElementById('tablabel_'+id).value);document.getElementById('tabname_'+id).style.display='';document.getElementById('tablabel_'+id).style.display='none';document.getElementById('tabother_'+id).style.display='';YAHOO.util.DragDropMgr.unlock();}}
StudioTabGroup.prototype.generateForm=function(formname){var form=document.getElementById(formname);for(var j=0;j<slotCount;j++){var ul=document.getElementById('ul'+j);var items=ul.getElementsByTagName('li');for(var i=0;i<items.length;i++){if(typeof(subtabModules[items[i].id])!='undefined'){var input=document.createElement('input');input.type='hidden';input.name=j+'_'+i;input.value=tabLabelToValue[subtabModules[items[i].id]];form.appendChild(input);}}}
form.slot_count.value=slotCount;};StudioTabGroup.prototype.generateGroupForm=function(formname){var form=document.getElementById(formname);for(j=0;j<slotCount;j++){var ul=document.getElementById('ul'+j);items=ul.getElementsByTagName('li');for(i=0;i<items.length;i++){if(typeof(subtabModules[items[i].id])!='undefined'){var input=document.createElement('input');input.type='hidden'
input.name='group_'+j+'[]';input.value=tabLabelToValue[subtabModules[items[i].id]];form.appendChild(input);}}}};StudioTabGroup.prototype.deleteTabGroup=function(id){if(document.getElementById('delete_'+id).value==0){document.getElementById('ul'+id).style.display='none';document.getElementById('tabname_'+id).style.textDecoration='line-through'
document.getElementById('delete_'+id).value=1;}else{document.getElementById('ul'+id).style.display='';document.getElementById('tabname_'+id).style.textDecoration='none'
document.getElementById('delete_'+id).value=0;}}
var lastField='';var lastRowCount=-1;var undoDeleteDropDown=function(transaction){deleteDropDownValue(transaction['row'],document.getElementById(transaction['id']),false);}
jstransaction.register('deleteDropDown',undoDeleteDropDown,undoDeleteDropDown);function deleteDropDownValue(rowCount,field,record){if(record){jstransaction.record('deleteDropDown',{'row':rowCount,'id':field.id});}
if(field.value=='0'){field.value='1';document.getElementById('slot'+rowCount+'_value').style.textDecoration='line-through';}else{field.value='0';document.getElementById('slot'+rowCount+'_value').style.textDecoration='none';}}
var studiotabs=new StudioTabGroup();