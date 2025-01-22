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
 */function checkFormPre(formId){validateCronInterval(formId);var noError=check_form(formId);if(noError){return true;}else{toggleAdv('true');return false;}}
function validateCronInterval(formId){var fieldIsValid=function(value,min,max){var inRange=function(value,min,max){return(value>=min&&value<=max);}
if(value=="*"){return true;}
var result=/^\*\/(\d+)$/.exec(value);if(result&&result[0]&&inRange(result[1],min,max)){return true;}
var sets=value.split(',');var valid=true;for(var i=0;i<sets.length;i++){result=/^(\d+)(-(\d+))?$/.exec(sets[i])
if(!result||!result[0]||!inRange(result[1],min,max)||(result[3]&&!inRange(result[3],min,max))){return false;}}
return true;}
var cronFields={mins:{min:0,max:59},hours:{min:0,max:23},day_of_month:{min:1,max:31},months:{min:1,max:12},day_of_week:{min:0,max:7}}
var valid=true;for(field in cronFields){removeFromValidate(formId,field);if(document[formId][field]&&!fieldIsValid(document[formId][field].value,cronFields[field].min,cronFields[field].max)){valid=false;addToValidate(formId,field,'error',true,"{$MOD.ERR_CRON_SYNTAX}");}else{addToValidate(formId,field,'verified',true,"{$MOD.ERR_CRON_SYNTAX}");}}
return valid;}
function toggleAdv(onlyAdv){var thisForm=document.getElementById("EditView");var crontab=document.getElementById("crontab");var simple=document.getElementById("simple");var adv=document.getElementById("advTable");var use=document.getElementById("use_adv");if(crontab.style.display=="none"||onlyAdv=='true'){crontab.style.display="";adv.style.display="";simple.style.display="none";use.value="true";}else{crontab.style.display="none";adv.style.display="none";simple.style.display="";use.value="false";}
for(i=0;i<thisForm.elements.length;i++){if(thisForm.elements[i].disabled){thisForm.elements[i].disabled=false;}}}
function allDays(){var toggle=document.getElementById("all");var m=document.getElementById("mon");var t=document.getElementById("tue");var w=document.getElementById("wed");var h=document.getElementById("thu");var f=document.getElementById("fri");var s=document.getElementById("sat");var u=document.getElementById("sun");if(toggle.checked){m.checked=true;t.checked=true;w.checked=true;h.checked=true;f.checked=true;s.checked=true;u.checked=true;}else{m.checked=false;t.checked=false;w.checked=false;h.checked=false;f.checked=false;s.checked=false;u.checked=false;}}
function updateVisibility(){if($('#adv_interval').is(':checked')){$('#job_interval_advanced').parent().parent().show();$('#job_interval_basic').parent().parent().hide();$('#LBL_ADV_OPTIONS').show();}else{$('#job_interval_advanced').parent().parent().hide();$('#job_interval_basic').parent().parent().show();$('#LBL_ADV_OPTIONS').hide();}}
function initScheduler(){if(typeof(adv_interval)!="undefined"&&adv_interval){$('#adv_interval').prop("checked",true);}}
$('#EditView_tabs').ready(function(){initScheduler();updateVisibility();});$('#adv_interval').ready(function(){$('#adv_interval').click(updateVisibility);});