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
 */function password_confirmation(){var new_pwd=document.getElementById('new_password').value;var old_pwd=document.getElementById('old_password').value;var confirm_pwd=document.getElementById('confirm_pwd');if(confirm_pwd.value!=new_pwd)
confirm_pwd.style.borderColor='red';else
confirm_pwd.style.borderColor='';if(confirm_pwd.value!=(new_pwd.substring(0,confirm_pwd.value.length)))
document.getElementById('comfirm_pwd_match').style.display='inline';else
document.getElementById('comfirm_pwd_match').style.display='none';if(new_pwd!=""||confirm_pwd.value!=""||old_pwd!=""||(document.getElementById('page')&&document.getElementById('page').value=="Change"))
document.getElementById('password_change').value='true';else
document.getElementById('password_change').value='false';}
function set_password(form,rules){if(form.password_change.value=='true'){if(rules=='1'){alert(ERR_RULES_NOT_MET);return false;}
if(form.is_admin.value!=1&&(form.is_current_admin&&form.is_current_admin.value!='1')&&form.old_password.value==""){alert(ERR_ENTER_OLD_PASSWORD);return false;}
if(form.new_password.value==""){alert(ERR_ENTER_NEW_PASSWORD);return false;}
if(form.confirm_pwd.value==""){alert(ERR_ENTER_CONFIRMATION_PASSWORD);return false;}
if(form.new_password.value==form.confirm_pwd.value)
return true;else{alert(ERR_REENTER_PASSWORDS);return false;}}
else
return true;}
function newrules(minpwdlength,maxpwdlength,customregex){var good_rules=0;var passwd=document.getElementById('new_password').value;if(document.getElementById('lengths')){var length=document.getElementById('new_password').value.length;if((length<parseInt(minpwdlength)&&parseInt(minpwdlength)>0)||(length>parseInt(maxpwdlength)&&parseInt(maxpwdlength)>0)){document.getElementById('lengths').className='bad';good_rules=1;}
else{document.getElementById('lengths').className='good';}}
if(document.getElementById('1lowcase')){if(!passwd.match('[abcdefghijklmnopqrstuvwxyz]')){document.getElementById('1lowcase').className='bad';good_rules=1;}
else{document.getElementById('1lowcase').className='good';}}
if(document.getElementById('1upcase')){if(!passwd.match('[ABCDEFGHIJKLMNOPQRSTUVWXYZ]')){document.getElementById('1upcase').className='bad';good_rules=1;}
else{document.getElementById('1upcase').className='good';}}
if(document.getElementById('1number')){if(!passwd.match('[0123456789]')){document.getElementById('1number').className='bad';good_rules=1;}
else{document.getElementById('1number').className='good';}}
if(document.getElementById('1special')){var custom_regex=new RegExp('[|}{~!@#$%^&*()_+=-]');if(!custom_regex.test(passwd)){document.getElementById('1special').className='bad';good_rules=1;}
else{document.getElementById('1special').className='good';}}
if(document.getElementById('regex')){var regex=new RegExp(customregex);if(regex.test(passwd)){document.getElementById('regex').className='bad';good_rules=1;}
else{document.getElementById('regex').className='good';}}
return good_rules;}
function set_focus(){if(document.getElementById('error_pwd')){if(document.forms.length>0){for(i=0;i<document.forms.length;i++){for(j=0;j<document.forms[i].elements.length;j++){var field=document.forms[i].elements[j];if((field.type=="password")&&(field.name=="old_password")){field.focus();if(field.type=="text"){field.select();}
break;}}}}}
else{if(document.forms.length>0){for(i=0;i<document.forms.length;i++){for(j=0;j<document.forms[i].elements.length;j++){var field=document.forms[i].elements[j];if((field.type=="text"||field.type=="textarea"||field.type=="password")&&!field.disabled&&(field.name=="first_name"||field.name=="name"||field.name=="user_name"||field.name=="document_name")){field.focus();if(field.type=="text"){field.select();}
break;}}}}}}