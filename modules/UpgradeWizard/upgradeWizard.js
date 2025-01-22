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
 */var req;var uw_check_msg="";var find_done=false;function loadXMLDoc(url){req=false;if(window.XMLHttpRequest){try{req=new XMLHttpRequest();}catch(e){req=false;}}else if(window.ActiveXObject){try{req=new ActiveXObject("Msxml2.XMLHTTP");}catch(e){try{req=new ActiveXObject("Microsoft.XMLHTTP");}catch(e){req=false;}}}
if(req){req.onreadystatechange=processReqChange;req.open("GET",url,true);req.send("");}}
function preflightToggleAll(cb){var checkAll=false;var form=document.getElementById('diffs');if(cb.checked==true){checkAll=true;}
for(i=0;i<form.elements.length;i++){if(form.elements[i].type=='checkbox'){form.elements[i].checked=checkAll;}}
return;}
function checkSqlStatus(done){var schemaSelect=document.getElementById('select_schema_change');var hideShow=document.getElementById('show_sql_run');var hideShowCB=document.getElementById('sql_run');var nextButton=document.getElementById('next_button');var schemaMethod=document.getElementById('schema');document.getElementById('sql_run').checked=false;if(schemaSelect.options[schemaSelect.selectedIndex].value=='manual'&&done==false){hideShow.style.display='block';nextButton.style.display='none';hideShowCB.disabled=false;schemaMethod.value='manual';}else{if(done==true){hideShowCB.checked=true;hideShowCB.disabled=true;}else{hideShow.style.display='none';}
nextButton.style.display='inline';schemaMethod.value='sugar';}}
function toggleDisplay(targ){target=document.getElementById("targ");if(target.style.display=='none'){target.style.display='';}else{target.style.display='none';}}
function verifyMerge(cb){if(cb.value=='sugar'){var challenge="{$mod_strings['LBL_UW_OVERWRITE_DESC']}";var answer=confirm(challenge);if(!answer){cb.options[0].selected=true;}}}