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
 */var rhandle=new RevisionListHandler();var from_popup_return=false;function document_set_return(popup_reply_data){from_popup_return=true;var form_name=popup_reply_data.form_name;var name_to_value_array=popup_reply_data.name_to_value_array;related_doc_id='EMPTY';for(var the_key in name_to_value_array){if(the_key!='toJSON'){var displayValue=name_to_value_array[the_key];displayValue=displayValue.replace('&#039;',"'");displayValue=displayValue.replace('&amp;',"&");displayValue=displayValue.replace('&gt;',">");displayValue=displayValue.replace('&lt;',"<");displayValue=displayValue.replace('&quot; ',"\"");if(the_key=='related_doc_id'){related_doc_id=displayValue;}
window.document.forms[form_name].elements[the_key].value=displayValue;}}
related_doc_id=YAHOO.lang.JSON.stringify(related_doc_id);var conditions=new Array();conditions[conditions.length]={"name":"document_id","op":"starts_with","value":related_doc_id};var query={"module":"DocumentRevisions","field_list":['id','revision','date_entered'],"conditions":conditions,"order":{'by':'date_entered','desc':true}};result=global_rpcClient.call_method('query',query,true);rhandle.display(result);}
function RevisionListHandler(){}
RevisionListHandler.prototype.display=function(result){var names=result['list'];var rev_tag=document.getElementById('related_doc_rev_id');rev_tag.options.length=0;for(i=0;i<names.length;i++){rev_tag.options[i]=new Option(names[i].fields['revision'],names[i].fields['id'],false,false);}
rev_tag.disabled=false;}
function setvalue(source){src=new String(source.value);target=new String(source.form.document_name.value);if(target.length==0){lastindex=src.lastIndexOf("/");if(lastindex==-1){lastindex=src.lastIndexOf("\\");}
if(lastindex==-1){source.form.document_name.value=src;}else{source.form.document_name.value=src.substr(++lastindex,src.length);}}}
function toggle_template_type(istemplate){template_type=document.getElementById('template_type');if(istemplate.checked){template_type.disabled=false;}else{template_type.disabled=true;}}