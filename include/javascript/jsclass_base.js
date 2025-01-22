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
 */function SugarClass(){this.init();}
SugarClass.prototype.init=function(){}
SugarClass.inherit=function(className,parentClassName){var str=className+".prototype = new "+parentClassName+"();";str+=className+".prototype.constructor = "+className+";";str+=className+".superclass = "+parentClassName+".prototype;";try{SUGAR.util.globalEval(str);}catch(e){}}
SugarClass.inherit("SugarContainer","SugarClass");function SugarContainer(root_div){GLOBAL_REGISTRY.container=this;this.init(root_div);}
SugarContainer.prototype.init=function(root_div){this.root_div=root_div;SugarContainer.superclass.init.call(this);}
SugarContainer.prototype.start=function(root_widget){this.root_widget=new root_widget();this.root_widget.load(this.root_div);}
if(typeof(global_request_registry)=="undefined"){var global_request_registry=new Object();}
var req_count=0;SugarClass.inherit("SugarDateTime","SugarClass");function SugarDateTime(){this.init(root_div);}
SugarDateTime.prototype.init=function(root_div){this.root_div=root_div;}
SugarDateTime.mysql2jsDateTime=function(mysql_date,mysql_time){var match=new RegExp(date_reg_format);if(((result=match.exec(mysql_date)))==null){return null;}
var match2=new RegExp(time_reg_format);if((result2=match2.exec(mysql_time))==null){result2=[0,0,0,0];}
var match3=/^0(\d)/;if((result3=match3.exec(result2[1]))!=null){result2[1]=result3[1];}
if(typeof(result2[3])!='undefined'){if(result2[3]=='pm'||result2[3]=='PM'){if(parseInt(result2[1])!=12){result2[1]=parseInt(result2[1])+12;}}
else if(result2[1]==12){result2[1]=0;}}
return new Date(result[date_reg_positions['Y']],result[date_reg_positions['m']]-1,result[date_reg_positions['d']],result2[1],result2[2],0,0);}
SugarDateTime.prototype.getFormattedDate=function(date_obj){var returnDate='';var userDateFormat=GLOBAL_REGISTRY['current_user']['fields']['date_time_format']['date'];var dow=GLOBAL_REGISTRY['calendar_strings']['dom_cal_weekdays_long'][date_obj.getDay()];var month=date_obj.getMonth()+1;month=GLOBAL_REGISTRY['calendar_strings']['dom_cal_month_long'][month];returnDate=dow;for(i=0;i<5;i++){switch(userDateFormat.charAt(i)){case"Y":returnDate+=" "+date_obj.getFullYear();break;case"m":returnDate+=" "+month;break;case"d":returnDate+=" "+date_obj.getDate();break;default:}}
return returnDate;}
SugarDateTime.getFormattedDate=SugarDateTime.prototype.getFormattedDate;SugarDateTime.prototype.getFormattedDOW=function(date_obj){var hour=config.strings.mod_strings.Calendar.dow[date_obj.getDay()];}
SugarDateTime.getFormattedDOW=SugarDateTime.prototype.getFormattedDOW;SugarDateTime.getAMPM=function(date_obj){var hour=date_obj.getHour();var am_pm='AM';if(hour>12){hour-=12;am_pm='PM';}
else if(hour==12){am_pm='PM';}
else if(hour==0){hour=12;}
return am_pm;}
SugarDateTime.getFormattedHour=SugarDateTime.prototype.getFormattedHour;SugarDateTime.prototype.parseUTCDate=function(date_string){var match=/(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z/;if(((result=match.exec(date_string)))!=null){var new_date=new Date(Date.UTC(result[1],result[2]-1,result[3],result[4],result[5],parseInt(result[6])+time_offset));return new_date;}}
SugarDateTime.parseUTCDate=SugarDateTime.prototype.parseUTCDate;SugarDateTime.prototype.parseAdjustedDate=function(date_string,dst_start,dst_end,gmt_offset_secs){var match=/(\d{4})(\d{2})(\d{2})T(\d{2})(\d{2})(\d{2})Z/;dst_start_parse=match.exec(dst_start);dst_end_parse=match.exec(dst_end);if(dst_start_parse==null||dst_end_parse==null){var new_date=new Date(result[1],result[2]-1,result[3],result[4],result[5],parseInt(result[6]));new_date=new Date(new_date.getTime()+gmt_offset_secs*1000);}else{dst_start_obj=new Date(dst_start_parse[1],dst_start_parse[2]-1,dst_start_parse[3],dst_start_parse[4],dst_start_parse[5],parseInt(dst_start_parse[6]));dst_end_obj=new Date(dst_end_parse[1],dst_end_parse[2]-1,dst_end_parse[3],dst_end_parse[4],dst_end_parse[5],parseInt(dst_end_parse[6]));if(((result=match.exec(date_string)))!=null){var new_date=new Date(result[1],result[2]-1,result[3],result[4],result[5],parseInt(result[6]));var event_ts=new_date.getTime();var dst_start_ts=dst_start_obj.getTime();var dst_end_ts=dst_end_obj.getTime();if(((event_ts>=dst_start_ts||event_ts<dst_end_ts)&&dst_start_ts>dst_end_ts)||(event_ts>=dst_start_ts&&event_ts<dst_end_ts)){new_date=new Date(new_date.getTime()+60*60*1000);}
new_date=new Date(new_date.getTime()+gmt_offset_secs*1000);}}
return new_date;}
SugarDateTime.parseAdjustedDate=SugarDateTime.prototype.parseAdjustedDate;SugarDateTime.prototype.getUTCHash=function(startdate){var month=(startdate.getUTCMonth()<10)?"0"+startdate.getUTCMonth():""+startdate.getUTCMonth();var day=(startdate.getUTCDate()<10)?"0"+startdate.getUTCDate():""+startdate.getUTCDate();var hours=(startdate.getUTCHours()<10)?"0"+startdate.getUTCHours():""+startdate.getUTCHours();var minutes=(startdate.getUTCMinutes()<10)?"0"+startdate.getUTCMinutes():""+startdate.getUTCMinutes();return startdate.getUTCFullYear()+month+day+hours+minutes;}
SugarDateTime.getUTCHash=SugarDateTime.prototype.getUTCHash;