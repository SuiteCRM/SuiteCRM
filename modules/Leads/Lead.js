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
 */function set_campaignlog_and_save_background(popup_reply_data){var form_name=popup_reply_data.form_name;var name_to_value_array=popup_reply_data.name_to_value_array;var passthru_data=popup_reply_data.passthru_data;var query_array=new Array();if(name_to_value_array!='undefined'){for(var the_key in name_to_value_array){if(the_key=='toJSON'){}
else{query_array.push(the_key+'='+name_to_value_array[the_key]);}}}
var selection_list;if(popup_reply_data.selection_list){selection_list=popup_reply_data.selection_list;}
else{selection_list=popup_reply_data.name_to_value_array;}
if(selection_list!='undefined'){for(var the_key in selection_list){query_array.push('subpanel_id[]='+selection_list[the_key])}}
var module=get_module_name();var id=get_record_id();query_array.push('value=DetailView');query_array.push('module='+module);query_array.push('http_method=get');query_array.push('return_module='+module);query_array.push('return_id='+id);query_array.push('record='+id);query_array.push('isDuplicate=false');query_array.push('return_type=addcampaignlog');query_array.push('action=Save2');query_array.push('inline=1');var refresh_page=escape(passthru_data['refresh_page']);for(prop in passthru_data){if(prop=='link_field_name'){query_array.push('subpanel_field_name='+escape(passthru_data[prop]));}else{if(prop=='module_name'){query_array.push('subpanel_module_name='+escape(passthru_data[prop]));}else{query_array.push(prop+'='+escape(passthru_data[prop]));}}}
var query_string=query_array.join('&');request_map[request_id]=passthru_data['child_field'];var returnstuff=http_fetch_sync('index.php',query_string);request_id++;got_data(returnstuff,true);if(refresh_page==1){document.location.reload(true);}}