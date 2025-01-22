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
 */var grid2,grid3,grid4,grid3F,grid4F;var add_all_fields=SUGAR.language.get('app_strings','LBL_ADD_ALL_LEAD_FIELDS');var remove_all_fields=SUGAR.language.get('app_strings','LBL_REMOVE_ALL_LEAD_FIELDS');function addGrids(form_name){if(!check_form('WebToLeadCreation')){return false;}
else{grid3=SUGAR_GRID_grid1;grid4=SUGAR_GRID_grid2;var webFormDiv=document.getElementById('webformfields');addCols(grid3,'colsFirst',webFormDiv);addCols(grid4,'colsSecond',webFormDiv);return true;}}
function checkFields(REQUIRED_LEAD_FIELDS,LEAD_SELECT_FIELDS){grid2=SUGAR_GRID_grid0;grid3=SUGAR_GRID_grid1;grid4=SUGAR_GRID_grid2;var reqFields='';for(var i=0;i<grid2.getRecordSet().getLength();i++){if(grid2.getRecord(i).getData()[2]!=null){reqFields=reqFields+grid2.getRecord(i).getData()[0]+', ';}}
if(reqFields){reqFields=reqFields.substring(0,reqFields.lastIndexOf(','));alert(REQUIRED_LEAD_FIELDS+' '+reqFields);return false;}
else if(grid3.getRecordSet().getLength()==1&&grid4.getRecordSet().getLength()==1){alert(LEAD_SELECT_FIELDS);return false;}
else{return true;}}
function askLeadQ(direction,REQUIRED_LEAD_FIELDS,LEAD_SELECT_FIELDS){if(direction=='back'){var grid_Div=document.getElementById('grid_Div');var lead_Div=document.getElementById('lead_queries_Div');grid_Div.style.display='block';lead_Div.style.display='none';}
if(direction=='next'){if(!checkFields(REQUIRED_LEAD_FIELDS,LEAD_SELECT_FIELDS)){return false;}
else{var lead_Div=document.getElementById('lead_queries_Div');var grid_Div=document.getElementById('grid_Div');lead_Div.style.display='block';grid_Div.style.display='none';}}}
function campaignPopulated(){var camp_populated=document.getElementById('campaign_id');if(camp_populated.value==0){return true;};return true;}
function selectFields(indexes,grid){var retStr='';for(var i=0;i<indexes.length;i++){retStr=retStr+grid.getRow(indexes[i]).childNodes[0].childNodes[0].innerHTML+','+'\n';retStr=retStr+'\n';}
return retStr.substring(0,retStr.lastIndexOf(','));}
function displayAddRemoveDragButtons(Add_All_Fields,Remove_All_Fields){var addRemove=document.getElementById("lead_add_remove_button");if(grid2.getRecordSet().getLength()==0){addRemove.setAttribute('value',Remove_All_Fields);addRemove.setAttribute('title',Remove_All_Fields);}
else if(grid3.getRecordSet().getLength()==0&&grid4.getRecordSet().getLength()==0){addRemove.setAttribute('value',Add_All_Fields);addRemove.setAttribute('title',Add_All_Fields);}}
function displayAddRemoveButtons(Add_All_Fields,Remove_All_Fields){var addRemove=document.getElementById("lead_add_remove_button");if(grid2.getRecordSet().getLength()>1){addRemove.setAttribute('value',Add_All_Fields);addRemove.setAttribute('title',Add_All_Fields);}
else{addRemove.setAttribute('value',Remove_All_Fields);addRemove.setAttribute('title',Remove_All_Fields);}}
function dragDropAllFields(Add_All_Fields,Remove_All_Fields){grid2=SUGAR_GRID_grid0;grid3=SUGAR_GRID_grid1;grid4=SUGAR_GRID_grid2;var addRemove=document.getElementById("lead_add_remove_button");var availibleSet=grid2.getRecordSet();var availibleCount=availibleSet.getLength();if(addRemove.value==Add_All_Fields&&availibleCount>1){for(var i=0;i<availibleCount;i++){if(i%2==0&&availibleSet.getRecord(i).getData()[0]!=" "){grid3.addRow(availibleSet.getRecord(i).getData(),(i / 2));}
if(i%2==1&&availibleSet.getRecord(i).getData()[0]!=" "){grid4.addRow(availibleSet.getRecord(i).getData(),((i-1)/ 2));}}
for(i=availibleCount-1;i>=0;i--){if(grid2.getRecord(i)!=null&&grid2.getRecord(i).getData()[0]!=" "){grid2.deleteRow(i);}}}
else if(addRemove.value==Remove_All_Fields){var count=0;if(grid3.getRecordSet().getLength()>=grid4.getRecordSet().getLength()){count=grid3.getRecordSet().getLength();}
else{count=grid4.getRecordSet().getLength();}
for(var i=0;i<count;i++){if(grid3.getRecord(i)!=null&&grid3.getRecord(i).getData()[0]!=" "){grid2.addRow(grid3.getRecord(i).getData(),grid2.getRecordSet().getLength()-1);}
if(grid4.getRecord(i)!=null&&grid4.getRecord(i).getData()[0]!=" "){grid2.addRow(grid4.getRecord(i).getData(),grid2.getRecordSet().getLength()-1);}}
for(var i=count-1;i>=0;i--){if(grid4.getRecord(i)!=null&&grid4.getRecord(i).getData()[0]!=" "){grid4.deleteRow(i);}
if(grid3.getRecord(i)!=null&&grid3.getRecord(i).getData()[0]!=" "){grid3.deleteRow(i);}}}
displayAddRemoveButtons(Add_All_Fields,Remove_All_Fields);}
function addCols(grid,colsNumber,webFormDiv){for(var i=0;i<grid.getRecordSet().getLength()-1;i++){var selectedEl=grid.getRecord(i).getData()[1];var webField=document.createElement('input');webField.setAttribute('id',colsNumber+i);webField.setAttribute('name',colsNumber+'[]');webField.setAttribute('type','hidden');webField.setAttribute('value',selectedEl);webFormDiv.appendChild(webField);}}
function editUrl(){var chk_url_elm=document.getElementById("chk_edit_url");if(chk_url_elm.checked==true){var url_elm=document.getElementById("post_url");url_elm.disabled=false;}
if(chk_url_elm.checked==false){var url_elm=document.getElementById("post_url");url_elm.disabled=true;}}