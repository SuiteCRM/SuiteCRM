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
 */var groups_arr=new Array();var chartTypesHolder=[];var groups_count=-1;var filters_arr=new Array();var filters_count_map=new Object();var filters_count=-1;var current_filter_id=-1;var groups_count_map=new Object();var current_group_id=-1;var join_refs=new Array();var group_field=null;var has_group=null;var global_report_def=null;var goto_anchor='';var all_fields=new Object();var full_table_list=new Object();full_table_list.self=new Object();full_table_list.self.parent='';full_table_list.self.value=document.EditView.self.options[document.EditView.self.options.selectedIndex].value;full_table_list.self.module=document.EditView.self.options[document.EditView.self.options.selectedIndex].value;full_table_list.self.label=document.EditView.self.options[document.EditView.self.options.selectedIndex].text;full_table_list.self.children=new Object();function hideCheckGroups(){document.getElementById('checkGroups').style.display='none';}
function table_changed(obj){if(document.EditView.report_type[1].checked){if(typeof hideCheckGroupsTimeout!='undefined')clearTimeout(hideCheckGroupsTimeout);document.getElementById('checkGroups').style.display='';hideCheckGroupsTimeout=window.setTimeout('hideCheckGroups()',6000);}
current_module=document.EditView.self.options[document.EditView.self.options.selectedIndex].value;if(obj.options[obj.selectedIndex].value==''){delete_this_join(obj.id);return;}
full_table_list[obj.id].value=obj.value;full_table_list[obj.id].label=obj.options[obj.options.selectedIndex].text;if(obj.id!='self'){var parent_id=full_table_list[obj.id].parent;var parent_module=full_table_list[parent_id].module;var parent_link_defs=getLinksByRelType(module_defs[parent_module].link_defs);full_table_list[obj.id].link_def=parent_link_defs[full_table_list[obj.id].value];full_table_list[obj.id].module=getRelatedModule(full_table_list[obj.id].link_def);full_table_list[obj.id].link_def.table_key=obj.id;}
else{full_table_list[obj.id].module=obj.value;}
if(document.getElementById('report_results')!=null){document.getElementById('report_results').style.display='none';}
delete_join_rows(obj.id);if(obj.id=='self'){reload_join_rows();deleteAllFilters();deleteAllGroups(0);reload_columns();}
else{joinChecked(obj);}
remakeGroups();document.EditView.sort_by.value='';document.EditView.sort_dir.value='';document.EditView.summary_sort_by.value='';document.EditView.summary_sort_dir.value='';}
function showDetailsClicked(obj){if(obj.checked){document.getElementById('columns_table').style.display='';document.getElementById('columns_more_div').style.display='';}
else{document.getElementById('columns_table').style.display='none';document.getElementById('columns_more_div').style.display='none';}}
function deleteFilter(index){var this_row=filters_arr[filters_count_map[index]].row;this_row.parentNode.removeChild(this_row);filters_arr.splice(filters_count_map[index],1);for(id in filters_count_map){if(filters_count_map[id]>filters_count_map[index]){filters_count_map[id]--;}}}
function deleteGroup(index){if(typeof(groups_count_map[index])=='undefined'||typeof(groups_arr[groups_count_map[index]])=='undefined'){return;}
var this_row=groups_arr[groups_count_map[index]].row;this_row.parentNode.removeChild(this_row);groups_arr.splice(groups_count_map[index],1);for(id in groups_count_map){if(groups_count_map[id]>groups_count_map[index]){groups_count_map[id]--;}}
var group_by_button=document.getElementById('group_by_button');group_by_button.style.display='inline';if(groups_arr.length==1&&document.EditView.chart_type.options.length>5){chartTypesHolder.push(document.EditView.chart_type.options[5]);document.EditView.chart_type.options[5]=null;}
reload_columns('add');}
function getReportType(){for(i=0;i<document.EditView.report_type.length;i++){if(document.EditView.report_type[i].checked==true){return document.EditView.report_type[i].value;}}}
function reportTypeChanged(){var report_type=getReportType();var columns_table=document.getElementById('columns_table');var summary_table=document.getElementById('summary_table');if(report_type=='summary'){document.getElementById('tab_li_chart_options_tab').style.display='';document.getElementById('tab_link_chart_options_tab').style.display='';document.getElementById('tab_li_group_by_tab').style.display='';document.getElementById('tab_link_group_by_tab').style.display='';document.getElementById('tab_link_columns_tab').innerHTML=SUGAR.language.get('Reports','LBL_4_CHOOSE');document.getElementById('summary_table').style.display='';document.getElementById('summary_more_div').style.display='';document.getElementById('columns_table').style.display='none';document.getElementById('columns_more_div').style.display='none';document.getElementById('group_by_div').style.display='';if(document.EditView.show_details.checked){document.getElementById('columns_table').style.display='';document.getElementById('columns_more_div').style.display='';}
else{document.getElementById('columns_table').style.display='none';document.getElementById('columns_more_div').style.display='none';}}
else if(report_type=='tabular'){document.getElementById('tab_li_chart_options_tab').style.display='none';document.getElementById('tab_link_chart_options_tab').style.display='none';document.getElementById('tab_li_group_by_tab').style.display='none';document.getElementById('tab_link_group_by_tab').style.display='none';document.getElementById('tab_link_columns_tab').innerHTML=SUGAR.language.get('Reports','LBL_3_CHOOSE');document.getElementById('columns_table').style.display='';document.getElementById('columns_more_div').style.display='';document.getElementById('summary_table').style.display='none';document.getElementById('summary_more_div').style.display='none';document.getElementById('group_by_div').style.display='none';}
document.getElementById('report_results').style.display='none';refresh_chart_tab();}
function manyColumnSelectChanged(index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];refreshFilterColumn(filter_row.column_select,default_filter,index);refreshFilterQualify(filter_row.column_select,default_filter,index);refreshFilterInput(default_filter,index);}
function moduleSelectChanged(index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];refreshFilterColumn(filter_row.column_select,default_filter,index);refreshFilterQualify(filter_row.column_select,default_filter,index);refreshFilterInput(default_filter,index);}
function moduleSelectChangedGroup(index){current_group_id=index;var group_row=groups_arr[groups_count_map[index]];refreshGroupColumn(group_row.column_select,default_group,index);refreshGroupQualify(group_row.column_select,default_group,index);reload_columns('add');}
function columnSelectChanged(index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];refreshFilterQualify(filter_row.column_select,default_filter,index);refreshFilterInput(default_filter,index);}
function columnSelectGroupChanged(index){current_group_id=index;var group_row=groups_arr[groups_count_map[index]];refreshGroupQualify(group_row.column_select,default_group,index);reload_columns('add');}
function refreshGroupQualify(obj,group,index){current_group_id=index;var group_row=groups_arr[groups_count_map[index]];group_row.qualify_cell.removeChild(group_row.qualify_select);addGroupQualify(group_row.qualify_cell,group);}
function refreshFilterQualify(obj,filter,index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];filter_row.qualify_cell.removeChild(filter_row.qualify_select);addFilterQualify(filter_row.qualify_cell,filter);}
function refreshFilterColumn(obj,filter,index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];filter_row.column_cell.removeChild(filter_row.column_select);addColumnSelectFilter(filter_row.column_cell,filter);}
function refreshGroupColumn(obj,group,index){current_group_id=index;var group_row=groups_arr[groups_count_map[index]];group_row.column_cell.removeChild(group_row.column_select);addColumnSelectGroup(group_row.column_cell,group);}
function refreshFilterModule(obj,filter,index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];filter_row.module_cell.removeChild(filter_row.module_select);addModuleSelectFilter(filter_row.module_cell,filter);}
function refreshGroupModule(obj,group,index){current_group_id=index;var group_row=groups_arr[groups_count_map[index]];group_row.module_cell.removeChild(group_row.module_select);addModuleSelectGroup(group_row.module_cell,group);}
function filterTypeChanged(index){var filter={input_name0:''};refreshFilterInput(filter,index);}
function refreshFilterInput(filter,index){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];if(typeof(filter_row.input_field0)!='undefined'&&typeof(filter_row.input_field0.value)!='undefined'){type="input";}
filter_row.input_cell.removeChild(filter_row.input_cell.firstChild);addFilterInput(filter_row.input_cell,filter);}
function addColumnSelectGroup(cell,group){var select_html_info=new Object();var options=new Array();var select_info=new Object();select_info['name']='group';select_info['onchange']='columnSelectGroupChanged('+current_group_id+');';select_html_info['select']=select_info;var module_select=groups_arr[groups_count_map[current_group_id]].module_select;var table_key=module_select.options[module_select.selectedIndex].value;if(table_key=='self'){selected_module=current_module;field_defs=module_defs[selected_module].group_by_field_defs;}
else{selected_module=table_key;var field_defs=module_defs[full_table_list[table_key].module].group_by_field_defs;}
var field_defs_arr=getOrderedFieldDefArray(field_defs,true);var selected=false;var got_selected=0;for(var index in field_defs_arr){var field=field_defs_arr[index];if(field.type=='text'||(field.type=='name'&&typeof(field.fields)!='undefined')){continue;}
var key=table_key+":"+field.name;if(typeof(all_fields[key])=='undefined'){continue;}
if(group.column_name==key){got_selected=1;selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=key;option_info['text']=field.vname;option_info['selected']=selected;options[options.length]=option_info;}
if(got_selected==0){options[0]['selected']=true;}
select_html_info['options']=options;cell.innerHTML=buildSelectHTML(select_html_info);groups_arr[groups_count_map[current_group_id]].column_select=cell.getElementsByTagName('select')[0];}
function addModuleSelectFilter(cell,filter){var select_html_info=new Object();var options=new Array();var select_info=new Object();select_info['name']='filter';select_info['onchange']='moduleSelectChanged('+current_filter_id+');';select_html_info['select']=select_info;var link_defs=getSelectedLinkDefs();var option_info=new Object();option_info['value']='self';option_info['text']=module_defs[current_module].label;option_info['selected']=selected;options[options.length]=option_info;var parts=filter.column_name.split(':');var selected_link_name=parts[0];for(var i in link_defs){var linked_field=link_defs[i];var selected=false;if(i==selected_link_name){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=i;var label=linked_field['label'];if(i!='self'){label=full_table_list[full_table_list[i].parent].label+' &gt; '+label;}
option_info['text']=label;option_info['selected']=selected;options[options.length]=option_info;}
select_html_info['options']=options;cell.innerHTML=buildSelectHTML(select_html_info);filters_arr[filters_count_map[current_filter_id]].module_select=cell.getElementsByTagName('select')[0];}
function addModuleSelectGroup(cell,filter){var select_html_info=new Object();var options=new Array();var select_info=new Object();select_info['name']='group';select_info['onchange']='moduleSelectChangedGroup('+current_group_id+');';select_html_info['select']=select_info;var link_defs=getSelectedLinkDefs();var option_info=new Object();option_info['value']='self';option_info['text']=module_defs[current_module].label;option_info['selected']=selected;options[options.length]=option_info;var parts=filter.column_name.split(':');var selected_link_name=parts[0];for(var i in link_defs){var linked_field=link_defs[i];var selected=false;if(i==selected_link_name){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=i;var label=linked_field['label'];if(i!='self'){label=full_table_list[full_table_list[i].parent].label+' &gt; '+label;}
option_info['text']=label;option_info['selected']=selected;options[options.length]=option_info;}
select_html_info['options']=options;cell.innerHTML=buildSelectHTML(select_html_info);groups_arr[groups_count_map[current_group_id]].module_select=cell.getElementsByTagName('select')[0];}
function getOrderedFieldDefArray(field_defs,show_id_field){var field_defs_arr=new Array();var id_field=null;for(field_name in field_defs){var field=field_defs[field_name];field_defs_arr.push(field);}
field_defs_arr.sort(_sort_by_field_name);if(id_field!=null&&show_id_field){field_defs_arr.unshift(id_field);}
return field_defs_arr;}
function _sort_by_field_name(a,b){if(typeof(a['vname'])=='undefined'){a['vname']=a['name'];}
else if(typeof(b['vname'])=='undefined'){b['vname']=b['name'];}
if(a['type']=='name'||a['type']=='user_name'){return-1;}
else if(b['type']=='name'||b['type']=='user_name'){return 1;}
else{return a['vname'].localeCompare(b['vname']);}}
function addColumnSelectFilter(cell,filter){var select_html_info=new Object();var options=new Array();var select_info=new Object();var field_defs=new Object();select_info['name']='filter';select_info['onchange']='columnSelectChanged('+current_filter_id+');';select_html_info['select']=select_info;var module_select=filters_arr[filters_count_map[current_filter_id]].module_select;var table_key=module_select.options[module_select.selectedIndex].value;if(table_key=='self'){selected_module=current_module;field_defs=module_defs[selected_module].field_defs;}
else{selected_module=table_key;var field_defs=module_defs[full_table_list[table_key].module].field_defs;}
var field_defs_arr=getOrderedFieldDefArray(field_defs,true);var selected=false;for(var index in field_defs_arr){var field=field_defs_arr[index];var key=table_key+":"+field.name;if(typeof(all_fields[key])=='undefined'){continue;}
if(field.type=='time'){continue;}
if(filter.column_name==key){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=key;option_info['text']=field.vname;option_info['selected']=selected;options[options.length]=option_info;}
select_html_info['options']=options;cell.innerHTML=buildSelectHTML(select_html_info);filters_arr[filters_count_map[current_filter_id]].column_select=cell.getElementsByTagName('select')[0];}
function addFilterInput(cell,filter){var filter_row=filters_arr[filters_count_map[current_filter_id]];var qualifier_name=filter_row.qualify_select.options[filter_row.qualify_select.selectedIndex].value;var module_select=filter_row.module_select;var table_key=module_select.options[module_select.selectedIndex].value;filter.table_key=table_key;var module_select=filters_arr[filters_count_map[current_filter_id]].module_select;var table_key=module_select.options[module_select.selectedIndex].value;if(table_key=='self'){selected_module=current_module;var field_defs=module_defs[selected_module].field_defs;}
else{selected_module=table_key;var field_defs=module_defs[full_table_list[table_key].module].field_defs;}
if(typeof(qualifier_name)=='undefined'||qualifier_name==''){qualifier_name='equals';}
var column_name=filter_row.column_select.options[filter_row.column_select.selectedIndex].value;if(typeof(column_name)=='undefined'||column_name==''){column_name='';}
var field=all_fields[column_name].field_def;var field_type=field.type;if(typeof(field.custom_type)!='undefined'){field_type=field.custom_type;}
cell.innerHTML="<table><tr></tr></table>";var row=cell.getElementsByTagName("tr")[0];if(qualifier_name=='between'){addFilterInputTextBetween(row,filter);}
else if(qualifier_name=='between_dates'){addFilterInputDateBetween(row,filter);}
else if(qualifier_name=='empty'||qualifier_name=='not_empty'){addFilterNoInput(row,filter);}
else if(field_type=='date'||field_type=='datetime'){if(qualifier_name.indexOf('tp_')==0){addFilterInputEmpty(row,filter);}
else{addFilterInputDate(row,filter);}}
else if(field_type=='id'||field_type=='name'){if(qualifier_name=='is'){addFilterInputRelate(row,field,filter);}
else{addFilterInputText(row,filter);}}
else if((field_type=='user_name')||(field_type=='assigned_user_name')){if(users_array==""){loadXML();}
if(qualifier_name=='one_of'){addFilterInputSelectMultiple(row,users_array,filter);}
else{addFilterInputSelectSingle(row,users_array,filter);}}
else if(field_type=='enum'||field_type=='multienum'){if(qualifier_name=='one_of'){addFilterInputSelectMultiple(row,field.options,filter);}
else{addFilterInputSelectSingle(row,field.options,filter);}}
else if(field_type=='bool'){var no=new Object();no['value']='yes';no['text']=SUGAR.language.languages.app_list_strings.checkbox_dom[1];var yes=new Object();yes['value']='no';yes['text']=SUGAR.language.languages.app_list_strings.checkbox_dom[2];var options=[yes,no];addFilterInputSelectSingle(row,options,filter);}
else{addFilterInputText(row,filter);}
return row;}
function loadXML(){var gURL='index.php?module=Reports&action=fillUserCombo';if(window.XMLHttpRequest){xmlhttp=new XMLHttpRequest();if(xmlhttp.overrideMimeType){xmlhttp.overrideMimeType('text/xml');}
xmlhttp.open("GET",gURL,false);xmlhttp.onreadystatechange=loadUsers;xmlhttp.send(null);loadUsers();}
else if(window.ActiveXObject){xmlhttp=new ActiveXObject('Microsoft.XMLHTTP');if(xmlhttp){xmlhttp.onreadystatechange=loadUsers;xmlhttp.open('GET',gURL,false);xmlhttp.send();}}}
function loadUsers(){if(xmlhttp.readyState==4){if(xmlhttp.status==200||xmlhttp.status==0){if(window.ActiveXObject)
xmlhttp.responseXML.loadXML(xmlhttp.responseText);var acc=xmlhttp.responseXML.getElementsByTagName('data');var opts='';for(var i=0;i<acc.length;i++){val=getNodeValue(acc[i],'datavalue');SUGAR.util.globalEval("users_array[users_array.length] = ("+val+")");}}}}
function getNodeValue(obj,tag){return obj.getElementsByTagName(tag)[0].firstChild.nodeValue;}
function addFilterInputText(row,filter){var cell=document.createElement("td");var new_input=document.createElement("input");new_input.type="text";if(typeof(filter.input_name0)=='undefined'){filter.input_name0='';}
new_input.value=filter.input_name0;new_input.name="text_input";new_input.size="30";new_input.maxsize="255";new_input.visible="true";cell.appendChild(new_input);row.appendChild(cell);var filter_row=filters_arr[filters_count_map[current_filter_id]];filter_row.input_field0=new_input;filter_row.input_field1=null;}
function to_display_date(date_str){var date_arr=date_str.match(/([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})/);if(date_arr!=null){var new_date=new String(cal_date_format);new_date=new_date.replace("%Y",date_arr[1]);new_date=new_date.replace("%m",date_arr[2]);new_date=new_date.replace("%d",date_arr[3]);return new_date;}
else{return date_str;}}
function addFilterInputDate(row,filter){var cell=document.createElement("td");cell.setAttribute('valign','middle');var new_input=document.createElement("input");new_input.type="text";if(typeof(filter.input_name0)!='undefined'&&filter.input_name0.length>0){new_input.value=to_display_date(filter.input_name0);}
new_input.name="text_input";new_input.size="30";new_input.maxsize="255";new_input.visible="true";new_input.setAttribute('id','jscal_field');cell.appendChild(new_input);row.appendChild(cell);var cell=document.createElement("td");cell.setAttribute('valign','middle');var img_element=document.createElement("span");img_element.className='suitepicon suitepicon-module-calendar';img_element.setAttribute('id','jscal_trigger');cell.appendChild(img_element);row.appendChild(cell);Calendar.setup({inputFieldObj:new_input,buttonObj:img_element,ifFormat:cal_date_format,showsTime:false,singleClick:true,step:1,weekNumbers:false});var filter_row=filters_arr[filters_count_map[current_filter_id]];filter_row.input_field0=new_input;filter_row.input_field1=null;}
function addFilterNoInput(row,filter){var cell=document.createElement("td");cell.setAttribute('valign','middle');var new_input=document.createElement("input");new_input.type="hidden";new_input.value=filter.qualifier_name;new_input.name="text_input";cell.appendChild(new_input);row.appendChild(cell);}
function addFilterInputEmpty(row,filter){var cell=document.createElement("td");cell.setAttribute('valign','middle');var new_input=document.createElement("input");new_input.type="hidden";new_input.value=filter.qualifier_name;new_input.name="text_input";cell.appendChild(new_input);row.appendChild(cell);var cell=document.createElement("td");row.appendChild(cell);var filter_row=filters_arr[filters_count_map[current_filter_id]];filter_row.input_field0=new_input;filter_row.input_field1=null;}
function addFilterInputSelectMultiple(row,options,filter){var cell=document.createElement('td');row.appendChild(cell);var select_html_info=new Object();var options_arr=new Array();var select_info=new Object();select_info['size']='5';select_info['multiple']=true;select_html_info['select']=select_info;var selected_map=new Object();for(i=0;i<filter.input_name0.length;i++){selected_map[filter.input_name0[i]]=1;}
for(i=0;i<options.length;i++){var option_name;var option_value;if(typeof(options[i].text)=='undefined'){option_text=options[i];option_value=options[i];}
else if(options[i].value==''){continue;}
else{option_text=options[i].text;option_value=options[i].value;}
if(typeof(selected_map[option_value])!='undefined'){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=option_value;option_info['text']=option_text;option_info['selected']=selected;options_arr[options_arr.length]=option_info;}
select_html_info['options']=options_arr;cell.innerHTML=buildSelectHTML(select_html_info)
var filter_row=filters_arr[filters_count_map[current_filter_id]];filter_row.input_field0=cell.getElementsByTagName('select')[0];filter_row.input_field1=null;}
function addFilterInputSelectSingle(row,options,filter){var cell=document.createElement('td');row.appendChild(cell);var select_html_info=new Object();var options_arr=new Array();var select_info=new Object();select_info['name']='input';select_html_info['select']=select_info;for(i=0;i<options.length;i++){if(typeof(options[i].text)=='undefined'){option_text=options[i];option_value=options[i];}
else if(options[i].value==''){continue;}
else{option_text=options[i].text;option_value=options[i].value;}
if(option_value==filter.input_name0){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=option_value;option_info['text']=option_text;option_info['selected']=selected;options_arr[options_arr.length]=option_info;}
select_html_info['options']=options_arr;cell.innerHTML=buildSelectHTML(select_html_info);var filter_row=filters_arr[filters_count_map[current_filter_id]];filter_row.input_field0=cell.getElementsByTagName('select')[0];filter_row.input_field1=null;}
function addFilterInputTextBetween(row,filter){var filter_row=filters_arr[filters_count_map[current_filter_id]];var cell=document.createElement('td');var new_input=document.createElement("input");new_input.type="text";if(typeof(filter.input_name0)=='undefined'){filter.input_name0='';}
new_input.value=filter.input_name0;cell.appendChild(new_input);row.appendChild(cell);filter_row.input_field0=new_input;var cell=document.createElement('td');var new_text=document.createTextNode(lbl_and);cell.appendChild(new_text);row.appendChild(cell);var cell=document.createElement('td');var new_input=document.createElement("input");new_input.type="text";if(typeof(filter.input_name1)=='undefined'){filter.input_name1='';}
new_input.value=filter.input_name1;cell.appendChild(new_input);row.appendChild(cell);filter_row.input_field1=new_input;}
function addFilterInputDateBetween(row,filter){var filter_row=filters_arr[filters_count_map[current_filter_id]];var cell=document.createElement("td");cell.setAttribute('valign','middle');var new_input=document.createElement("input");new_input.type="text";if(typeof(filter.input_name0)=='undefined'){filter.input_name0='';}
new_input.value=to_display_date(filter.input_name0);new_input.name="text_input";new_input.size="12";new_input.maxsize="255";new_input.visible="true";new_input.setAttribute('id','jscal_field');cell.appendChild(new_input);row.appendChild(cell);filter_row.input_field1=new_input;var cell=document.createElement("td");cell.setAttribute('valign','middle');var img_element=document.createElement("span");img_element.className='suitepicon suitepicon-module-calendar';img_element.setAttribute('id','jscal_trigger');cell.appendChild(img_element);row.appendChild(cell);Calendar.setup({inputFieldObj:new_input,buttonObj:img_element,ifFormat:cal_date_format,showsTime:false,singleClick:true,weekNumbers:false,step:1});var cell=document.createElement('td');cell.setAttribute('valign','middle');var new_text=document.createTextNode(lbl_and);cell.appendChild(new_text);row.appendChild(cell);var cell=document.createElement("td");cell.setAttribute('valign','middle');var new_input=document.createElement("input");new_input.type="text";if(typeof(filter.input_name1)=='undefined'){filter.input_name1='';}
new_input.value=to_display_date(filter.input_name1);new_input.name="text_input";new_input.size="12";new_input.maxsize="255";new_input.visible="true";new_input.setAttribute('id','jscal_field2');cell.appendChild(new_input);row.appendChild(cell);filter_row.input_field1=new_input;var cell=document.createElement("td");var img_element=document.createElement("span");img_element.className='suitepicon suitepicon-module-calendar';img_element.setAttribute('id','jscal_trigger2');cell.appendChild(img_element);row.appendChild(cell);Calendar.setup({inputFieldObj:new_input,buttonObj:img_element,ifFormat:cal_date_format,showsTime:false,singleClick:true,weekNumbers:false,step:1});}
var current_parent='';var current_parent_id='';function set_current_parent(name,value){current_parent.value=name;current_parent_id.value=value;}
function getModuleInFilter(filter){var selected_module=current_module;current_prefix='self';var view_join=filter.module_cell.getElementsByTagName('select')[0];var selected_option=view_join.options[view_join.selectedIndex].value;if(selected_option!='self'){selected_module=full_table_list[selected_option].module;}
return selected_module;}
function set_form_return_reports(popup_reply_data){var form_name=popup_reply_data.form_name;var name_to_value_array=popup_reply_data.name_to_value_array;var passthru_data=popup_reply_data.passthru_data;current_parent_id.value=name_to_value_array['id'];current_parent.value=name_to_value_array['name'];}
function addFilterInputRelate(row,field,filter){var filter_row=filters_arr[filters_count_map[current_filter_id]];var module_name=getModuleInFilter(filter_row);var field_name=module_name+":"+field.name;var field_id_name=module_name+":"+field.name+":id";var cell=document.createElement('td');var id_input=document.createElement("input");id_input.setAttribute('type','hidden');id_input.setAttribute("name",field_id_name);id_input.setAttribute("id",field_id_name);if(typeof(filter.input_name0)=='undefined'){filter.input_name0='';}
id_input.setAttribute("value",filter.input_name0);cell.appendChild(id_input);filter_row.input_field0=id_input;var name_input=document.createElement("input");name_input.setAttribute("type","text");name_input.setAttribute("readonly","true");name_input.setAttribute("name",field_name);name_input.setAttribute("id",field_name);if(typeof(filter.input_name1)=='undefined'){filter.input_name1='';}
name_input.setAttribute("value",filter.input_name1);cell.appendChild(name_input);filter_row.input_field1=name_input;row.appendChild(cell);var cell=document.createElement('td');var new_input=document.createElement("input");new_input.title=lbl_select;new_input.type="button";new_input.value=lbl_select;new_input.name=field.module;new_input.setAttribute("class","button");new_input.onclick=function(){current_parent=name_input;current_parent_id=id_input;return open_popup(module_name,600,400,"",true,false,{"call_back_function":"set_form_return_reports","form_name":"EditView","field_to_name_array":{"id":"id","name":"name"}});}
cell.appendChild(new_input);row.appendChild(cell);}
function addGroupQualify(cell,group){var group_row=groups_arr[groups_count_map[current_group_id]];var column_name=group_row.column_select.options[group_row.column_select.selectedIndex].value;var module_select=group_row.module_select;var table_key=module_select.options[module_select.selectedIndex].value;if(table_key=='self'){selected_module=current_module;}
else{selected_module=full_table_list[table_key].module;}
var field;if(typeof(column_name)=='undefined'||column_name==''){field=all_fields['self:name'].field_def;}
else{field=all_fields[column_name].field_def;}
var field_type=field.type;if(typeof(field.custom_type)!='undefined'){field_type=field.custom_type;}
var style='display: inline';if(field_type!='date'&&field_type!='datetime'){style='display: none';}
var select_html_info=new Object();var options=new Array();var select_info=new Object();select_info['name']='qualify';select_info['style']=style;select_info['onChange']='reload_columns(\'add\');';select_html_info['select']=select_info;var selected=false;for(i=0;i<date_group_defs.length;i++){if((typeof(group.qualifier_name)!='undefined'&&date_group_defs[i].name==group.qualifier_name)||(typeof(group.qualifier)!='undefined'&&date_group_defs[i].name==group.qualifier)){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=date_group_defs[i].name;option_info['text']=date_group_defs[i].value;option_info['selected']=selected;options[options.length]=option_info;}
select_html_info['options']=options;var html=buildSelectHTML(select_html_info);cell.innerHTML=html;group_row.qualify_select=cell.getElementsByTagName('select')[0];}
function addFilterQualify(cell,filter){var filter_row=filters_arr[filters_count_map[current_filter_id]];var field_key=filter_row.column_select.options[filter_row.column_select.selectedIndex].value;var field=new Object();if(typeof(field_key)!='undefined'&&field_key!=''){field=all_fields[field_key].field_def;}
var select_html_info=new Object();var options=new Array();var select_info=new Object();select_info['name']='qualify';select_info['onchange']="filterTypeChanged("+current_filter_id+");";select_html_info['select']=select_info;field_type=field.type;if(typeof(field.custom_type)!='undefined'){field_type=field.custom_type;}
var qualifiers=filter_defs[field_type];var selected=false;for(i=0;i<qualifiers.length;i++){if(qualifiers[i].name==filter.qualifier_name){selected=true;}
else{selected=false;}
var option_info=new Object();option_info['value']=qualifiers[i].name;option_info['text']=qualifiers[i].value;option_info['selected']=selected;options[options.length]=option_info;}
select_html_info['options']=options;cell.innerHTML=buildSelectHTML(select_html_info);filter_row['qualify_select']=cell.getElementsByTagName('select')[0];}
var default_group={column_name:''};function addGroupByFromButton(group){addGroupBy(group);reload_columns('add');}
function addGroupBy(group){groups_arr[groups_arr.length]=new Object();groups_count++;groups_count_map[groups_count]=groups_arr.length-1;current_group_id=groups_count;if(typeof(group)=='undefined'){group=default_group;}
group.column_name=group.table_key+":"+group.name;var the_table=document.getElementById('group_by_tbody');var row=document.createElement('tr');groups_arr[groups_count_map[groups_count]].row=row;row.valign="top";var module_cell=document.createElement('td');module_cell.valign="top";row.appendChild(module_cell);groups_arr[groups_count_map[groups_count]].module_cell=module_cell;addModuleSelectGroup(module_cell,group);var column_cell=document.createElement('td');column_cell.valign="top";row.appendChild(column_cell);groups_arr[groups_count_map[groups_count]].column_cell=column_cell;var new_select=addColumnSelectGroup(column_cell,group);var qualify_cell=document.createElement('td');qualify_cell.valign="top";row.appendChild(qualify_cell);groups_arr[groups_count_map[groups_count]].qualify_cell=qualify_cell;var new_filter=addGroupQualify(qualify_cell,group);var cell=document.createElement('td');cell.innerHTML="<input type=button onclick=\"deleteGroup("+groups_count+");\" class=button value="+lbl_remove+">";row.appendChild(cell);the_table.appendChild(row);if(groups_arr.length==1&&document.EditView.chart_type.options.length>5){chartTypesHolder.push(document.EditView.chart_type.options[5]);document.EditView.chart_type.options[5]=null;}
else if(groups_arr.length==2){document.EditView.chart_type.options[5]=chartTypesHolder.pop();}}
var default_filter={column_name:'',qualifier_name:'',input_name0:'',input_name1:''};function addFilter(filter){filters_arr[filters_arr.length]=new Object();filters_count++;filters_count_map[filters_count]=filters_arr.length-1;current_filter_id=filters_count;if(typeof(filter)=='undefined'){filter=default_filter;}
var the_table=document.getElementById('filters');var row=document.createElement('tr');filters_arr[filters_count_map[filters_count]].row=row;row.valign="top";var module_cell=document.createElement('td');module_cell.valign="top";row.appendChild(module_cell);filters_arr[filters_count_map[filters_count]].module_cell=module_cell;addModuleSelectFilter(module_cell,filter);var column_cell=document.createElement('td');column_cell.valign="top";row.appendChild(column_cell);filters_arr[filters_count_map[filters_count]].column_cell=column_cell;addColumnSelectFilter(column_cell,filter);var qualify_cell=document.createElement('td');qualify_cell.valign="top";row.appendChild(qualify_cell);filters_arr[filters_count_map[filters_count]].qualify_cell=qualify_cell;addFilterQualify(qualify_cell,filter);var input_cell=document.createElement('td');input_cell.valign="top";row.appendChild(input_cell);filters_arr[filters_count_map[filters_count]].input_cell=input_cell;addFilterInput(input_cell,filter);var cell=document.createElement('td');cell.valign="top";row.appendChild(cell);var cell=document.createElement('td');cell.innerHTML="<input type=button onclick=\"deleteFilter("+filters_count+");\" class=button value='"+lbl_remove+"'>";row.appendChild(cell);the_table.appendChild(row);}
function deleteAllFilters(){var the_table=document.getElementById('filters');var rows=the_table.rows;for(i=rows.length-1;i>=0;i--){the_table.removeChild(rows[i]);}
return;}
function deleteAllGroups(index){if(typeof(groups_arr[groups_count_map[index]])!='undefined'){var this_row=groups_arr[groups_count_map[index]].row;this_row.parentNode.removeChild(this_row);groups_arr.splice(groups_count_map[index],1);for(id in groups_count_map){if(groups_count_map[id]>groups_count_map[index]){groups_count_map[id]--;}}
var group_by_button=document.getElementById('group_by_button');group_by_button.style.display='inline';}
return;}
function remakeGroups(){document.EditView['report_offset'].value=0;if(typeof(document.EditView.show_columns)!='undefined'&&document.EditView.show_columns.checked){module_defs[current_module].group_by_field_defs=new Object();for(i=0;i<object_refs['display_columns'].options.length;i++){var field_name=object_refs['display_columns'].options[i].value;var field_def=module_defs[current_module].field_defs[field_name];module_defs[current_module].group_by_field_defs[field_def.name]=field_def;}}}
function getListFieldDef(field_key){var field_def=new Object();var vardef=all_fields[field_key].field_def;if(typeof(vardef.field_def_name)!='undefined'){field_def.name=vardef['field_def_name'];}
else{field_def.name=vardef['name'];}
field_def.label=vardef['vname'];if(typeof(vardef.group_function)!='undefined'&&vardef.group_function!=null){field_def.group_function=vardef.group_function;}
if(typeof(vardef.column_function)!='undefined'&&vardef.column_function!=null){field_def.column_function=vardef.column_function;}
field_def.table_key=all_fields[field_key].linked_field_name;return field_def;}
function fill_form(type){var report_def=new Object();var form_obj=document.EditView;if(typeof(type)!='undefined'&&type=='export'){form_obj.to_pdf.value='';form_obj.to_csv.value='on';}
var got_sort=0;var got_summary_sort=0;var got_summary_column=0;var got_error=0;var error_msgs=lbl_missing_fields+': \n';report_def['report_type']=getReportType();report_def.display_columns=new Array();var group_by_table=document.getElementById('group_by_tbody');if(document.EditView.show_details.checked==true||report_def.report_type=='tabular'){var disp_opts=object_refs['display_columns'].options;for(i=0;i<disp_opts.length;i++){var field_def=getListFieldDef(disp_opts[i].value);if(typeof(disp_opts[i].saved_text)!='undefined'){field_def['label']=disp_opts[i].saved_text;}
else{field_def['label']=disp_opts[i].text;}
report_def.display_columns.push(field_def);if(form_obj.sort_by.value==disp_opts[i].value){got_sort=1;}}}
report_def.summary_columns=new Array();if(report_def.report_type=='summary'&&object_refs['display_summary'].style.display!='none'){var sum_opts=object_refs['display_summary'].options;var summary_column_map=new Object();for(i=0;i<sum_opts.length;i++){var field_def=getListFieldDef(sum_opts[i].value);if(typeof(field_def.group_function)!='undefined'){got_summary_column=1;}
if(typeof(sum_opts[i].saved_text)!='undefined'){field_def['label']=sum_opts[i].saved_text;}
else{field_def['label']=sum_opts[i].text;}
summary_column_map[sum_opts[i].value]=field_def;report_def.summary_columns.push(field_def);if(form_obj.summary_sort_by.value==sum_opts[i].value){got_summary_sort=1;}}}
var sort_by=new Array();var summary_sort_by=new Array();var sort_dir=new Array();var summary_sort_dir=new Array();if(got_sort==0){form_obj.sort_by.value='';form_obj.sort_dir.value='';}
else{var sort_by_elem=new Object();var sort_by_elem=getListFieldDef(form_obj.sort_by.value);sort_by_elem.sort_dir=form_obj.sort_dir.value;sort_by.push(sort_by_elem);report_def.order_by=sort_by;}
if(got_summary_sort==0||document.EditView.show_details.checked){form_obj.summary_sort_by.value='';form_obj.summary_sort_dir.value='';}
else{var summary_sort_by_elem=new Object();var key_arr=form_obj.summary_sort_by.value.split(':');summary_sort_by_elem.name=key_arr[1];if(typeof(all_fields[form_obj.summary_sort_by.value].field_def.group_function)!='undefined'){summary_sort_by_elem.group_function=all_fields[form_obj.summary_sort_by.value].field_def.group_function;}
else if(typeof(all_fields[form_obj.summary_sort_by.value].field_def.column_function)!='undefined'){summary_sort_by_elem.group_function=all_fields[form_obj.summary_sort_by.value].field_def.column_function;}
summary_sort_by_elem.column_function=key_arr[2];summary_sort_by_elem.table_key=all_fields[form_obj.summary_sort_by.value].linked_field_name;summary_sort_by_elem.sort_dir=form_obj.summary_sort_dir.value;summary_sort_by.push(summary_sort_by_elem);report_def.summary_order_by=summary_sort_by;}
var group_by_table=document.getElementById('group_by_tbody');var report_type_elem=document.EditView.report_type;var group_defs=new Array();if((document.EditView.show_details.checked||report_def['report_type']=='tabular')&&object_refs['display_columns'].options.length==0){error_msgs+=lbl_at_least_one_display_column+'\n';got_error=1;}
if(report_def['report_type']=='summary'&&object_refs['display_summary'].options.length==0){error_msgs+=lbl_at_least_one_summary_column+'\n';got_error=1;}
for(var wp in full_table_list){if(typeof full_table_list[wp].value=='undefined'){error_msgs+=lbl_related_table_blank+'\n';got_error=1;break;}}
var filter_table=document.getElementById('filters');var filters_def=new Array();for(i=0;i<filter_table.rows.length;i++){var cell0=filter_table.rows[i].cells[1];var cell1=filter_table.rows[i].cells[2];var cell2=filter_table.rows[i].cells[3];var column_name=cell0.getElementsByTagName('select')[0].value;var filter_def=new Object();var field=all_fields[column_name].field_def;filter_def.name=field.name;filter_def.table_key=all_fields[column_name].linked_field_name;column_vname=all_fields[column_name].label_prefix+": "+field['vname'];filter_def.qualifier_name=cell1.getElementsByTagName('select')[0].value;var input_arr=cell2.getElementsByTagName('input');if(typeof(input_arr[0])!='undefined'){filter_def.input_name0=input_arr[0].value;if(input_arr[0].value==''){got_error=1;error_msgs+="\""+column_vname+"\""+lbl_missing_input_value+"\n";}
if(typeof(input_arr[1])!='undefined'){filter_def.input_name1=input_arr[1].value;if(input_arr[1].value==''){got_error=1;error_msgs+="\""+column_vname+"\""+lbl_missing_second_input_value+"\n";}}
if(field.type=='datetimecombo'){if(typeof(input_arr[2])!='undefined'){filter_def.input_name2=input_arr[2].value;if(input_arr[2].value==''&&input_arr[2].type!='checkbox'){got_error=1;error_msgs+="\""+column_vname+"\" "+lbl_missing_input_value+"\n";}}
if(typeof(input_arr[3])!='undefined'){filter_def.input_name3=input_arr[3].value;if(input_arr[3].value==''&&input_arr[3].type!='checkbox'){got_error=1;error_msgs+="\""+column_vname+"\" "+lbl_missing_input_value+"\n";}}
if(typeof(input_arr[4])!='undefined'){filter_def.input_name4=input_arr[4].value;if(input_arr[4].value==''&&input_arr[4].type!='checkbox'){got_error=1;error_msgs+="\""+column_vname+"\" "+lbl_missing_input_value+"\n";}}}}
else{var got_selected=0;var select_input=cell2.getElementsByTagName('select')[0];filter_def.input_name0=new Array();for(j=0;j<select_input.options.length;j++){if(select_input.options[j].selected==true){filter_def.input_name0.push(decodeURI(select_input.options[j].value));got_selected=1;}}
if(got_selected==0){error_msgs+="\""+column_vname+"\": "+lbl_missing_second_input_value+"\n";got_error=1;}}
if(field.type=='datetime'||field.type=='date'){if(typeof(filter_def.input_name0)!='undefined'&&typeof(filter_def.input_name0)!='array'){var date_match=filter_def.input_name0.match(date_reg_format);if(date_match!=null){filter_def.input_name0=date_match[date_reg_positions['Y']]+"-"+date_match[date_reg_positions['m']]+"-"+date_match[date_reg_positions['d']];}}
if(typeof(filter_def.input_name1)!='undefined'&&typeof(filter_def.input_name1)!='array'){var date_match=filter_def.input_name1.match(date_reg_format);if(date_match!=null){filter_def.input_name1=date_match[date_reg_positions['Y']]+"-"+date_match[date_reg_positions['m']]+"-"+date_match[date_reg_positions['d']];}}}else if(field.type=='datetimecombo'){if((typeof(filter_def.input_name0)!='undefined'&&typeof(filter_def.input_name0)!='array')&&(typeof(filter_def.input_name1)!='undefined'&&typeof(filter_def.input_name1)!='array')){var dbValue=convertReportDateTimeToDB(filter_def.input_name0,filter_def.input_name1);if(dbValue!=''){filter_def.input_name0=dbValue;}}
if(typeof(filter_def.input_name2)!='undefined'&&typeof(filter_def.input_name2)!='array'&&typeof(filter_def.input_name3)!='undefined'&&typeof(filter_def.input_name3)!='array'){var dbValue=convertReportDateTimeToDB(filter_def.input_name2,filter_def.input_name3);if(dbValue!=''){filter_def.input_name2=dbValue;}}}
filters_def.push(filter_def);}
if(got_error==1){alert(error_msgs);return false;}
report_def.filters_def=filters_def;report_def.filters_combiner=document.getElementById('filters_combiner').options[document.getElementById('filters_combiner').selectedIndex].value;var group_by_table=document.getElementById('group_by_tbody');var group_defs=new Array();if(report_def.report_type!='tabular'){for(i=0;i<group_by_table.rows.length;i++){var cell0=group_by_table.rows[i].cells[1];var cell1=group_by_table.rows[i].cells[2];var group_by_def=getListFieldDef(cell0.getElementsByTagName('select')[0].value);var group_key=cell0.getElementsByTagName('select')[0].value;if(typeof(cell1)!='undefined'&&cell1.getElementsByTagName('select')[0].style.display!='none'){group_by_def.qualifier=cell1.getElementsByTagName('select')[0].value;}
if(typeof(summary_column_map[group_key])=='undefined'){report_def.summary_columns.unshift(group_by_def);group_by_def.is_group_by='hidden';if(typeof(group_by_def.qualifier)!='undefined'){group_by_def.column_function=group_by_def.qualifier;}}
else{summary_column_map[group_key].is_group_by='visible';}
group_defs.push(group_by_def);}}
report_def.group_defs=group_defs;var links=getSelectedLinks();var links_def=new Array();for(var i in links){links_def.push(links[i]);}
var link_joins=getSelectedLinkJoins(links_def);report_def.full_table_list=full_table_list;report_def.module=current_module;report_def.report_name=document.EditView.save_report_as.value;report_def.chart_type=document.EditView.chart_type.value;report_def.chart_description=document.EditView.chart_description.value;report_def.numerical_chart_column=document.EditView.numerical_chart_column.value;global_report_def=report_def;report_def.assigned_user_id=document.EditView.assigned_user_id.value;report_def_str=YAHOO.lang.JSON.stringify(report_def);form_obj.report_def.value=report_def_str;return true;}
function do_export(){if(fill_form('export')==true){document.EditView.submit();}}
function set_sort(column_name,source){if(source=='undefined'){source='columns';}
var sort_by='sort_by';var sort_dir='sort_dir';if(source=='summary'){sort_by='summary_sort_by';sort_dir='summary_sort_dir';}
if(column_name==document.EditView[sort_by].value){if(document.EditView[sort_dir].value=="d"){document.EditView[sort_dir].value="a";}
else{document.EditView[sort_dir].value="d";}}
else{document.EditView[sort_by].value=column_name;document.EditView[sort_dir].value="a";}
document.EditView.to_pdf.value='';document.EditView.to_csv.value='';document.EditView['report_offset'].value=0;if(fill_form()==true){document.EditView.submit();}}
function set_offset(offset){document.EditView['report_offset'].value=offset;document.EditView.to_pdf.value='';document.EditView.to_csv.value='';if(fill_form()==true){document.EditView.submit();}}
function load_page(){reload_joins();current_module=document.EditView.self.options[document.EditView.self.options.selectedIndex].value;reload_join_rows('regular');all_fields=getAllFieldsMapped(current_module);if(form_submit!="true"){remakeGroups();reload_groups();reload_filters();}
reload_columns('regular');}
function reload_joins(){for(var index in report_def.full_table_list){var curr_table=report_def.full_table_list[index];if(index!="self"){add_related(curr_table.parent,index);option_selectbox=document.getElementById('outer_'+index);if(option_selectbox!=null){if(curr_table.optional!=null&&curr_table.optional==true){option_selectbox.checked=true;}
else{option_selectbox.checked=false;}}
else{}}
var curr_select=document.getElementById(index);curr_select.value=curr_table.value;full_table_list[index]=curr_table;}}
function getFieldKey(field_def){var func_name='';if(typeof(field_def.group_function)!='undefined'){func_name=field_def.group_function;}
else if(typeof(field_def.column_function)!='undefined'){func_name=field_def.column_function;}
if(field_def.group_function=='count'){return'count';}
else if(!(func_name=='weighted_amount'||func_name=='weighted_sum')&&func_name!=''){return field_def.table_key+":"+field_def.name+":"+func_name;}
return field_def.table_key+":"+field_def.name;}
function reload_filters(){for(index in report_def.filters_def){report_def.filters_def[index].column_name=getFieldKey(report_def.filters_def[index]);addFilter(report_def.filters_def[index]);}}
function reload_groups(){for(index in report_def.group_defs){addGroupBy(report_def.group_defs[index]);}}
function get_rel_type(linked_field,relationship){if(typeof(linked_field['link_type'])!='undefined'){return linked_field['link_type'];}
if(relationship.relationship_type=='one-to-many'){if(linked_field.bean_is_lhs==true){if(relationship['lhs_module']==linked_field['module']){return'one';}
else{return'many';}}
else{if(relationship['rhs_module']==linked_field['module']){return'one';}
else{return'many';}}}
return'many';}
function joinChecked(obj){reload_columns('add');reload_join_rows('add');var objName=obj.id;for(var index in filters_arr){current_filter_id=index;var filter_row=filters_arr[filters_count_map[index]];var filter_module=filter_row.module_select[filter_row.module_select.selectedIndex].value;if(objName.indexOf("_div")>-1)
filter_module=filter_module.substr(0,objName.indexOf("_div"));if(objName.indexOf("_div")>-1&&objName.substr(0,objName.indexOf("_div"))==filter_module){deleteFilter(index);}
else{var filter={column_name:filter_row.module_select[filter_row.module_select.selectedIndex].value,qualifier_name:'',input_name0:'',input_name1:''};refreshFilterModule(filter_row.module_select,filter,index);}}
for(var index in groups_arr){current_group_id=index;var group_row=groups_arr[groups_count_map[index]];var group_module=group_row.module_select[group_row.module_select.selectedIndex].value;if(objName.indexOf("_div")>-1)
group_module=group_module.substr(0,objName.indexOf("_div"));if(objName.indexOf("_div")>-1&&objName.substr(0,objName.indexOf("_div"))==group_module){deleteGroup(index);}
else{var group={column_name:group_row.module_select[group_row.module_select.selectedIndex].value};refreshGroupModule(group_row.module_select,group,index);refreshGroupColumn(group_row.column_select,group,index);refreshGroupQualify(group_row.qualify_select,group,index);}}}
function delete_this_join(this_id){delete_join_rows(this_id);this_obj=document.getElementById(this_id+"_div");this_obj.parentNode.removeChild(this_obj);parent_id=full_table_list[this_id].parent;if(full_table_list[parent_id]!=null){delete full_table_list[parent_id].children[this_id];}
delete full_table_list[this_id];joinChecked(this_obj);}
function delete_join_rows(parent_id){all_fields=new Array();if(full_table_list[parent_id]!=null&&full_table_list[parent_id].children!=null){var children_list=full_table_list[parent_id].children;for(var child in children_list){delete_join_rows(child);delete full_table_list[child];delete full_table_list[parent_id].children[child];}}
var children_div=document.getElementById(parent_id+"_children");if(children_div!=null){children_div.innerHTML='';children_div.style.display='none';}}
function add_related(parent_id,my_id){var options=new Array();var option_info=new Object();option_info['value']='';option_info['text']=lbl_none;option_info['selected']=true;options[options.length]=option_info;var mod_name=full_table_list[parent_id].module;if(mod_name==null){alert(lbl_alert_cant_add);return(false);}
if(module_defs[mod_name]==null){return(false);}
var link_defs=getLinksByRelType(module_defs[mod_name].link_defs);var selected_linked_field;for(linked_field_name in link_defs){var linked_field=link_defs[linked_field_name];var selected=false;var option_info=new Object();option_info['text']=linked_field['label'];option_info['value']=linked_field_name;option_info['selected']=false;options[options.length]=option_info;}
var select_html_info=new Object();var select_info=new Object();if(my_id==null){var id_num=0;while(document.getElementById(parent_id+'_link_'+id_num)!=null){id_num++;}
select_info['name']=parent_id+'_link_'+id_num;}
else{select_info['name']=my_id;}
select_info['id']=select_info['name'];select_info['onchange']='table_changed(this);';select_html_info['select']=select_info;select_html_info['options']=options;full_table_list[select_info['id']]=new Object();full_table_list[select_info['id']].parent=parent_id;full_table_list[select_info['id']].children=new Object();full_table_list[parent_id].children[select_info['id']]=select_info['id'];children_div=document.getElementById(parent_id+"_children");if(children_div.style.display=='none'){children_div.style.display='';}
new_child_div=document.createElement('div');new_child_div.style.marginLeft='10px';new_child_div.style.marginTop='5px';new_child_div.style.marginBottom='5px';new_child_div.innerHTML="<b>"+LBL_RELATED+"</b>"+buildSelectHTML(select_html_info)+' <a href="" class="button" style="padding: 2px; text-decoration: none;" onClick="add_related(\''+select_info['name']+'\'); return(false);">'+lbl_add_related+'</a> <a href="" class="button" style="padding: 2px; text-decoration: none;" onClick="delete_this_join(\''+select_info['name']+'\'); return(false);">'+lbl_del_this+'</a> '+
buildOuterJoinHTML(select_html_info)+'<div style="display: none; border-left: 2px dotted #000000; padding-left: 5px;" id="'+select_info['name']+'_children"></div>';new_child_div.id=select_info['name']+'_div';children_div.appendChild(new_child_div);}
function reload_join_rows(type){}
function build_join_rows(module,joins_table,level){join_refs=new Array();if(typeof(level)=='undefined'){level=0;}
level++;var link_defs=getLinksByRelType(module_defs[module].link_defs,'one');if(level==1){}
if(level<5){var tr=joins_table.insertRow(joins_table.rows.length);var td=tr.insertCell(tr.cells.length);var hidden_input='';join_index=0;for(linked_field_name in link_defs){var linked_field=link_defs[linked_field_name];var input_elem=document.createElement('input');input_elem.type='hidden';input_elem.name='link_'+linked_field['name'];input_elem.id='link_'+linked_field['name'];input_elem.value=linked_field['name'];join_index++;join_refs.push(input_elem);td.appendChild(input_elem);}}
if(level==1){var options=new Array();var option_info=new Object();option_info['value']='';option_info['text']=lbl_none;option_info['selected']=selected;options[options.length]=option_info;var link_defs=getLinksByRelType(module_defs[module].link_defs,'many');var selected_linked_field;var first=true;for(linked_field_name in link_defs){if(first){selected_linked_field=link_defs[linked_field_name];first=false;}
var linked_field=link_defs[linked_field_name];var selected=false;var option_info=new Object();option_info['value']=linked_field['name'];option_info['text']=linked_field_name.substring(0,1).toUpperCase()+linked_field_name.substring(1,linked_field_name.length);option_info['selected']=selected;options[options.length]=option_info;}
var select_html_info=new Object();var select_info=new Object();select_info['name']='joined';select_info['id']='multijoin';select_info['onchange']='joinChecked(this);';select_html_info['select']=select_info;var tr=joins_table.insertRow(joins_table.rows.length);var td=tr.insertCell(tr.cells.length);select_html_info['options']=options;}}
function getSelectedLinkJoins(link_array){var link_join_array=new Object();var outer_check=document.getElementById("outer_joined");if(outer_check!=null){link_name=document.forms.EditView.joined;if(outer_check.checked){link_join_array[link_name.value]=1;}
else{link_join_array[link_name.value]=0;}}
return link_join_array;}
function getSelectedLinks(){var joins_array=new Array();for(var index in full_table_list){if(index=='self'){continue;}
if(full_table_list[index]!=null&&full_table_list[index].value!=''){joins_array.push(index);}}
return joins_array;}
function getRelatedModule(link_def){if(typeof link_def=='undefined'){return;}
var rel_name=link_def.relationship_name;var rel_def=rel_defs[rel_name];if(typeof(rel_def)=='undefined'){return'';}
if(link_def.bean_is_lhs){return rel_def['rhs_module'];}
else{return rel_def['lhs_module'];}}
function viewJoinChanged(obj){reload_columns('join');}
function getSelectedLinkDefs(module){if(typeof(module)=='undefined'){module=current_module;}
var new_links=new Object();var links=getSelectedLinks()
var type='one';for(var i in links){if(typeof full_table_list[links[i]].link_def=='undefined'){return;}
var linked_field=full_table_list[links[i]].link_def;var selected=false;var relationship=rel_defs[linked_field['relationship_name']];var rel_type=get_rel_type(linked_field,relationship);new_links[links[i]]=linked_field;}
return new_links;}
function moduleIsVisible(module){if(typeof(visible_modules[module])=='undefined'){return false;}
return true;}
function getLinksByRelType(link_defs,type){var new_links=new Object();for(var i in link_defs){var linked_field=link_defs[i];var module=getRelatedModule(linked_field);if(!moduleIsVisible(module)){continue;}
var selected=false;var relationship=rel_defs[linked_field['relationship_name']];var rel_type=get_rel_type(linked_field,relationship);if(typeof(type)=='undefined'||rel_type==type){new_links[i]=link_defs[i];}}
return new_links;}
function getAllFieldsMapped(module){var all_fields=new Array();var summary_fields_str='';for(var k in module_defs[module].field_defs){all_fields["self:"+module_defs[module].field_defs[k].name]={"field_def":module_defs[module].field_defs[k],"linked_field_name":"self","label_prefix":module_defs[module].label};}
for(var k in module_defs[module].summary_field_defs){all_fields["self:"+module_defs[module].summary_field_defs[k].name]={"field_def":module_defs[module].summary_field_defs[k],"linked_field_name":"self","label_prefix":module_defs[module].label};summary_fields_str+='|'+"self:"+module_defs[module].summary_field_defs[k].name;}
all_fields["count"]=all_fields["self:count"];var link_defs=getSelectedLinkDefs(module);for(var i in link_defs){var join_module=getRelatedModule(link_defs[i]);if(typeof(module_defs[join_module])=='undefined'){continue;}
for(var j in module_defs[join_module].field_defs){all_fields[i+":"+module_defs[join_module].field_defs[j].name]={"field_def":module_defs[join_module].field_defs[j],"linked_field_name":i,"label_prefix":link_defs[i].label};}
for(var j in module_defs[join_module].summary_field_defs){var sum_field_def=module_defs[join_module].summary_field_defs[j];if(typeof(sum_field_def.field_def_name)!='undefined'){var field_def=module_defs[join_module].field_defs[sum_field_def.field_def_name];if(typeof(field_def.custom_type)!='undefined'){continue;}}
all_fields[i+":"+module_defs[join_module].summary_field_defs[j].name]={"field_def":module_defs[join_module].summary_field_defs[j],"linked_field_name":i,"label_prefix":link_defs[i].label};}}
return all_fields;}
function doRename(obj){var label=prompt("Rename label:\n"+obj.default_label,obj.text);if(label!=null){obj.text=label;obj.saved_text=label;}}
function saveLabel(type,obj){if(type=='column'){var columns_ref=object_refs['display_columns'];}
else if(type=='detailsummary'){var columns_ref=object_refs['display_summary'];}
else{return;}
if(typeof(obj.lastIndex)!='undefined'&&obj.lastIndex!=-1){var current_option=columns_ref.options[obj.lastIndex];current_option.text=obj.value+'  ['+current_option.default_label+']';current_option.saved_text=obj.value;}
else{this.value='';this.lastIndex=-1;}}
function reload_columns(reload_type){document.getElementById('column_label_editor').value='';if(typeof(reload_type)=='undefined'){reload_type='default';}
current_module=document.EditView.self.options[document.EditView.self.options.selectedIndex].value;display_columns_ref=object_refs['display_columns'];hidden_columns_ref=object_refs['hidden_columns'];display_summary_ref=object_refs['display_summary'];display_summary_ref.onchange=function(){var current_option=this.options[this.selectedIndex];document.getElementById('detailsummary_label_editor').lastIndex=this.selectedIndex;if(typeof(current_option.saved_text)=='undefined'){document.getElementById('detailsummary_label_editor').value=current_option.text;}
else{document.getElementById('detailsummary_label_editor').value=current_option.saved_text;}};hidden_summary_ref=object_refs['hidden_summary'];display_columns_ref.onchange=function(){var current_option=this.options[this.selectedIndex];document.getElementById('column_label_editor').lastIndex=this.selectedIndex;if(typeof(current_option.saved_text)=='undefined'){document.getElementById('column_label_editor').value=current_option.text;}
else{document.getElementById('column_label_editor').value=current_option.saved_text;}};if(reload_type=='default'||reload_type=='regular'){if(reload_type=='default'){visible_fields_map=new Object();document.EditView.show_details.checked=false;document.getElementById('summary_table').style.display='none';document.getElementById('summary_more_div').style.display='none';document.getElementById('columns_table').style.display='';document.getElementById('columns_more_div').style.display='';visible_summary_fields_org=module_defs[current_module].default_summary_columns;visible_summary_fields=new Array();for(i=0;i<visible_summary_fields_org.length;i++){if(visible_summary_fields_org[i]=='count'){visible_summary_fields.push(visible_summary_fields_org[i]);}
else{visible_summary_fields.push('self:'+visible_summary_fields_org[i]);}}
visible_fields=module_defs[current_module].default_table_columns;}
selected_link_name=module_defs[current_module].default_link_name;display_columns_ref.options.length=0;display_summary_ref.options.length=0;hidden_columns_ref.options.length=0;hidden_summary_ref.options.length=0;}
else if(reload_type=='add'||reload_type=='join'){visible_fields=new Array();visible_summary_fields=new Array();for(i=0;i<display_columns_ref.options.length;i++){visible_fields.push(display_columns_ref.options[i].value);if(typeof(visible_fields_map)!='undefined'&&typeof(visible_fields_map[display_columns_ref.options[i].value])=='undefined'){if(typeof(display_columns_ref.options[i].saved_text)!='undefined'){visible_fields_map[display_columns_ref.options[i].value]={"label":display_columns_ref.options[i].saved_text};}}}
for(i=0;i<display_summary_ref.options.length;i++){visible_summary_fields.push(display_summary_ref.options[i].value);if(typeof(visible_summary_fields_map)!='undefined'&&typeof(visible_summary_fields_map[display_summary_ref.options[i].value])=='undefined'){if(typeof(display_summary_ref.options[i].saved_text)!='undefined'){visible_fields_map[display_summary_ref.options[i].value]={"label":display_summary_ref.options[i].saved_text};}}}
display_columns_ref.options.length=0;display_summary_ref.options.length=0;hidden_columns_ref.options.length=0;hidden_summary_ref.options.length=0;}
var seen_visible=new Object();var view_join=document.EditView.view_join;var selected_link='';var link_defs=getSelectedLinkDefs();var view_join_value='';if(typeof(view_join.options)!='undefined'&&view_join.options.length>0){view_join_value=view_join.options[view_join.selectedIndex].value;}
view_join.options.length=0;if('self'==view_join_value){selected=true;}
else{selected=false;}
var select_counter=0;view_join.options[view_join.options.length]=new Option(module_defs[current_module].label,'self',selected);for(var i in full_table_list){if(i=="self"){continue;}
var table_def=full_table_list[i];var selected=false;select_counter++;if(i==view_join_value){selected=true;}
else{selected=false;}
var label=table_def['label'];if(i!='self'){label=full_table_list[full_table_list[i].parent].label+' > '+label;}
view_join.options[view_join.options.length]=new Option(label,i,selected);if(selected){view_join.selectedIndex=select_counter;}}
var selected_module=current_module;current_prefix='self';var table_key=view_join.options[view_join.selectedIndex].value;var field_defs=new Object();if(table_key!='self'){current_prefix=table_key;selected_module=full_table_list[table_key].module;field_defs=module_defs[selected_module].field_defs;}
else{field_defs=module_defs[selected_module].field_defs;}
all_fields=getAllFieldsMapped(current_module);for(i=0;i<visible_fields.length;i++){if(typeof(all_fields[visible_fields[i]])=='undefined'){continue;}
var field=all_fields[visible_fields[i]].field_def;var default_label=all_fields[visible_fields[i]]['label_prefix']+": "+field['vname'];if(typeof(visible_fields_map[visible_fields[i]])!='undefined'&&typeof(visible_fields_map[visible_fields[i]].label)!='undefined'){display_columns_ref.options[display_columns_ref.options.length]=new Option(visible_fields_map[visible_fields[i]].label,all_fields[visible_fields[i]]['linked_field_name']+":"+all_fields[visible_fields[i]].field_def['name']);}
else{display_columns_ref.options[display_columns_ref.options.length]=new Option(default_label,all_fields[visible_fields[i]]['linked_field_name']+":"+all_fields[visible_fields[i]].field_def['name']);}
var current_option=display_columns_ref.options[display_columns_ref.options.length-1];current_option.default_label=default_label;if(current_option.text!=current_option.default_label){current_option.saved_text=current_option.text;current_option.text+='  ['+default_label+']';}
seen_visible[visible_fields[i]]=1;}
var field_defs_arr=getOrderedFieldDefArray(field_defs,false);for(var index in field_defs_arr){var field=field_defs_arr[index];var key=current_prefix+":"+field['name'];if(typeof(all_fields[key])=='undefined'){continue;}
if(seen_visible[key]!=1){hidden_columns_ref.options[hidden_columns_ref.options.length]=new Option(field['vname'],key);}}
var seen_visible_summary=new Object();var view_join_summary=document.EditView.view_join_summary;var selected_link_value='';if(typeof(view_join_summary.options)!='undefined'&&view_join_summary.options.length>0){selected_link_value=view_join_summary.options[view_join_summary.selectedIndex].value;}
view_join_summary.options.length=0;if('self'==selected_link_value){selected=true;}
else{selected=false;}
view_join_summary.options[view_join_summary.options.length]=new Option(module_defs[current_module].label,'self',selected);var summary_select_counter=0;for(var i in full_table_list){if(i=="self"){continue;}
var linked_field=full_table_list[i];var selected=false;if(i==selected_link_value){selected=true;}
else{selected=false;}
var label=linked_field['label'];if(i!='self'){label=full_table_list[full_table_list[i].parent].label+' > '+label;}
summary_select_counter++;view_join_summary.options[view_join_summary.options.length]=new Option(label,i,selected);if(selected){view_join_summary.selectedIndex=summary_select_counter;}}
var selected_module=current_module;current_prefix='self';var sum_table_key=view_join_summary.options[view_join_summary.selectedIndex].value;if(sum_table_key!=''&&sum_table_key!='self'){current_prefix=sum_table_key;selected_module=full_table_list[sum_table_key].module;}
var valid_groups=new Object();var group_by_table=document.getElementById('group_by_tbody');has_group=null;for(i=0;i<group_by_table.rows.length;i++){has_group=1;var cell0=group_by_table.rows[i].cells[1];var cell1=group_by_table.rows[i].cells[2];var key=cell0.getElementsByTagName('select')[0].value;if(typeof(all_fields[key])=='undefined'){continue;}
var group_by_def=getListFieldDef(key);if(group_by_def.table_key!=current_prefix){continue;}
var key=group_by_def.table_key+":"+group_by_def.name;if(typeof(cell1)!='undefined'&&cell1.getElementsByTagName('select')[0].style.display!='none'){key+=":"+cell1.getElementsByTagName('select')[0].value;}
valid_groups[key]=1;}
var field_defs=module_defs[selected_module].field_defs;var numerical_chart_column=document.EditView.numerical_chart_column;numerical_chart_column.options.length=0;var which_option_selected=0;for(i=0;i<visible_summary_fields.length;i++){var key=visible_summary_fields[i];if(typeof(all_fields[key])=='undefined'||typeof(all_fields[key].field_def)=='undefined'){continue;}
if((typeof(valid_groups[key])=='undefined'&&(typeof(all_fields[key].field_def.summary_type)=='undefined'||all_fields[key].field_def.summary_type!='group'))){continue;}
var field=all_fields[key].field_def;var linked_field_name='';var label_prefix='';var default_label='';if(field.name=='count'){key='count';default_label=field['vname'];}
else{linked_field_name=all_fields[key].linked_field_name;label_prefix=all_fields[key].label_prefix;default_label=label_prefix+": "+field['vname'];}
var column_label;if(typeof(visible_summary_field_map[key])!='undefined'&&typeof(visible_summary_field_map[key].label)!='undefined'){column_label=visible_summary_field_map[key].label;}
else{column_label=default_label;}
display_summary_ref.options[display_summary_ref.options.length]=new Option(column_label,key);var split_key=key.split(':');var summary_field=all_fields[key].field_def;if(typeof(summary_field.group_function)!='undefined'){if(key==report_def['numerical_chart_column']){which_option_selected=numerical_chart_column.options.length;}
numerical_chart_column.options[numerical_chart_column.options.length]=new Option(column_label,key);}
var current_option=display_summary_ref.options[display_summary_ref.options.length-1];current_option.default_label=default_label;if(current_option.text!=current_option.default_label){current_option.saved_text=current_option.text;current_option.text+='  ['+default_label+']';}
seen_visible_summary[key]=1;}
numerical_chart_column.options.selectedIndex=which_option_selected;var summary_field_defs=module_defs[selected_module].summary_field_defs;var field_defs_arr=getOrderedFieldDefArray(field_defs,false);group_field=null;for(group_key in valid_groups){if(typeof all_fields[group_key]!='undefined'){group_field=all_fields[group_key].field_def;if(seen_visible_summary[group_key]!=1){hidden_summary_ref.options[hidden_summary_ref.options.length]=new Option(group_field['vname'],group_key);}
seen_visible_summary[group_key]=1;}}
for(var index in summary_field_defs){var field=summary_field_defs[index];if(field.summary_type!='group'){continue;}
var key=current_prefix+":"+field['name'];if(field['name']=='count'){key='count';}
if(typeof(all_fields[key])=='undefined'){continue;}
if(seen_visible_summary[key]!=1){hidden_summary_ref.options[hidden_summary_ref.options.length]=new Option(field['vname'],key);}}
refresh_chart_tab();var selected_chart_index=1;if(reload_type=='regular'){for(var i=0;i<document.EditView.chart_type.options.length;i++){if(document.EditView.chart_type.options[i].value==report_def.chart_type){selected_chart_index=i;}}
document.EditView.chart_type.selectedIndex=selected_chart_index;}}
function refresh_chart_tab(){if(document.EditView.numerical_chart_column.options.length>0&&has_group!=null){document.getElementById('no_chart_text').style.display='none';document.EditView.numerical_chart_column.disabled=false;document.EditView.chart_type.disabled=false;document.EditView.chart_type.selectedIndex=1;document.EditView.chart_description.disabled=false;}
else{document.getElementById('no_chart_text').style.display='';document.EditView.numerical_chart_column.disabled=true;document.EditView.chart_type.disabled=true;document.EditView.chart_type.selectedIndex=0;document.EditView.chart_description.disabled=true;}}
function set_readonly(form){if(form.save_report.checked){form.save_report.value='on';form.save_report_as.readOnly=false;form.save_report_as.focus();}
else{form.save_report.value='off';form.save_report_as.readOnly=true;}}
function addSelectOptionAttrs(info,obj){for(i=0;i<info['options'].length;i++){option=info['options'][i];var attr=new Object();for(var j in option){if(j!='text'&&j!='value'&&j!='selected'&&j!='name'&&j!='id'){obj.options[i][j]=option[j];}}}}
function buildSelectHTML(info){var text;text="<select";for(var key in info['select']){if(typeof(info['select'][key])!='undefined'){text+=" "+key+"=\""+info['select'][key]+"\"";}}
text+=">";var saved_attrs=new Array();for(i=0;i<info['options'].length;i++){option=info['options'][i];var attr=new Object();for(var j in option){if(j!='text'&&j!='value'&&j!='selected'&&j!='name'&&j!='id'){attr[j]=option[j];}}
saved_attrs.push(attr);text+="<option value=\""+encodeURI(option['value'])+"\" ";if(typeof(option['selected'])!='undefined'&&option['selected']==true){text+="SELECTED";}
text+=">"+option['text']+"</option>";}
text+="</select>";return text;}
function buildOuterJoinHTML(info){var text;var checked;checked='';if(report_def.link_joins!=null){for(key in report_def.link_joins){if(report_def.link_joins[key]==1){checked='CHECKED';}}}
text=" <input class='checkbox' type='checkbox' name='outer_"+info['select']['name']+"' id='outer_"+info['select']['name']+"' value=1 "+checked+" onChange='updateOuterJoin(this);'> "+lbl_outer_join_checkbox;text+='<img border="0" class="inlineHelpTip" src="'+image_path+'help.gif" onclick="SUGAR.util.showHelpTips(this,\''+lbl_optional_help+'\')"/>';return text;}
function updateOuterJoin(obj){table_key=obj.id.replace("outer_","");if(obj.checked==true){full_table_list[table_key].optional=true;}
else{full_table_list[table_key].optional=false;}}
function saved_chart_drilldown(group_value,group_key,id){var report_url='index.php?module=Reports&page=report&action=index&id='+id+'#'+group_value;document.location=report_url;}
function chart_drilldown(group_value,is_saved_report,id){var anch=document.getElementById(group_value);var elems=document.anchors;for(var i in elems){var elem=elems[i];if(typeof(elem.name)!='undefined'&&elem.name!=''&&typeof(elem.id)!='undefined'&&elem.id!=''&&elem.id==elem.name){if(group_value==elem.name){elem.focus();window.scrollBy(0,300);}}}}
function expandCollapseComboSummaryDiv(divId){if(document.getElementById(divId)){if(document.getElementById(divId).style.display=="none"){document.getElementById(divId).style.display="";document.getElementById("img_"+divId).innerHTML=document.getElementById("img_"+divId).innerHTML.replace(/advanced_search/,"basic_search");document.getElementById('expanded_combo_summary_divs').value+=divId+" ";}
else{document.getElementById(divId).style.display="none";document.getElementById("img_"+divId).innerHTML=document.getElementById("img_"+divId).innerHTML.replace(/basic_search/,"advanced_search");document.getElementById('expanded_combo_summary_divs').value=document.getElementById('expanded_combo_summary_divs').value.replace(divId,"");}}}