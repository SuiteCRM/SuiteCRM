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
 */var popup_request_data;var close_popup;function get_popup_request_data(){return YAHOO.lang.JSON.stringify(window.document.popup_request_data);}
function get_close_popup(){return window.document.close_popup;}
function open_popup(module_name,width,height,initial_filter,close_popup,hide_clear_button,popup_request_data,popup_mode,create,metadata){if(typeof popupCount==="undefined"){var popupCount=1;}else if(popupCount==0){var popupCount=1;}
window.document.popup_request_data=popup_request_data;window.document.close_popup=close_popup;width=(width==600)?800:width;height=(height==400)?800:height;URL='index.php?'
+'module='+module_name
+'&action=Popup';if(initial_filter!=''){URL+='&query=true'+initial_filter;}
if(hide_clear_button){URL+='&hide_clear_button=true';}
windowName=module_name+'_popup_window'+popupCount;popupCount++;windowFeatures='width='+width
+',height='+height
+',resizable=1,scrollbars=1';if(popup_mode==''&&popup_mode=='undefined'){popup_mode='single';}
URL+='&mode='+popup_mode;if(create==''&&create=='undefined'){create='false';}
URL+='&create='+create;if(metadata!=''&&metadata!='undefined'){URL+='&metadata='+metadata;}
var request_data=[];if(popup_request_data.jsonObject){request_data=popup_request_data.jsonObject;}else{request_data=popup_request_data;}
var field_to_name_array_url='';if(request_data&&request_data.field_to_name_array!='undefined'){for(var key in request_data.field_to_name_array){if(key.toLowerCase()!='id'){field_to_name_array_url+='&field_to_name[]='+encodeURIComponent(key.toLowerCase());}}}
if(field_to_name_array_url){URL+=field_to_name_array_url;}
win=window.open(URL,windowName,windowFeatures);if(window.focus){win.focus();}
win.popupCount=popupCount;return win;}
var from_popup_return=false;function set_return(popup_reply_data){from_popup_return=true;var form_name=popup_reply_data.form_name;var name_to_value_array=popup_reply_data.name_to_value_array;for(var the_key in name_to_value_array){if(the_key=='toJSON'){}
else{var displayValue=name_to_value_array[the_key].replace(/&amp;/gi,'&').replace(/&lt;/gi,'<').replace(/&gt;/gi,'>').replace(/&#039;/gi,'\'').replace(/&quot;/gi,'"');;if(window.document.forms[form_name]&&window.document.forms[form_name].elements[the_key]){window.document.forms[form_name].elements[the_key].value=displayValue;SUGAR.util.callOnChangeListers(window.document.forms[form_name].elements[the_key]);}}}}
function set_return_and_save(popup_reply_data){var form_name=popup_reply_data.form_name;var name_to_value_array=popup_reply_data.name_to_value_array;for(var the_key in name_to_value_array){if(the_key=='toJSON'){}
else{window.document.forms[form_name].elements[the_key].value=name_to_value_array[the_key];SUGAR.util.callOnChangeListers(window.document.forms[form_name].elements[the_key]);}}
window.document.forms[form_name].return_module.value=window.document.forms[form_name].module.value;window.document.forms[form_name].return_action.value='DetailView';window.document.forms[form_name].return_id.value=window.document.forms[form_name].record.value;window.document.forms[form_name].action.value='Save';window.document.forms[form_name].submit();}
function set_return_and_save_targetlist(popup_reply_data){var form_name=popup_reply_data.form_name;var name_to_value_array=popup_reply_data.name_to_value_array;var form_index=document.forms.length-1;sugarListView.get_checks();var uids=document.MassUpdate.uid.value;if(uids==''){return false;}
for(var the_key in name_to_value_array){if(the_key=='toJSON'){}
else{for(i=form_index;i>=0;i--){if(form_name==window.document.forms[form_index]){form_index=i;break;}}
window.document.forms[form_index].elements[get_element_index(form_index,the_key)].value=name_to_value_array[the_key];SUGAR.util.callOnChangeListers(window.document.forms[form_index].elements[get_element_index(form_index,the_key)]);}}
if(popup_reply_data.passthru_data.do_contacts){var form=window.document.forms[form_index];var do_contacts=$('<input type="hidden" name="do_contacts" value="1"/>');$(form).append(do_contacts);}
window.document.forms[form_index].elements[get_element_index(form_index,"return_module")].value=window.document.forms[form_index].elements[get_element_index(form_index,"module")].value;window.document.forms[form_index].elements[get_element_index(form_index,"return_action")].value='ListView';window.document.forms[form_index].elements[get_element_index(form_index,"uids")].value=uids;window.document.forms[form_index].submit();}
function get_element_index(form_index,element_name){var j=0;while(j<window.document.forms[form_index].elements.length){if(window.document.forms[form_index].elements[j].name==element_name){index=j;break;}
j++;}
return index;}
function get_initial_filter_by_account(form_name){var account_id=window.document.forms[form_name].account_id.value;var account_name=escape(window.document.forms[form_name].account_name.value);var initial_filter="&account_id="+account_id+"&account_name="+account_name;return initial_filter;}