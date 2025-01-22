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
 */var AjaxObject={ret:'',currentRequestObject:null,timeout:30000,forceAbort:false,_reset:function(){this.timeout=30000;this.forceAbort=false;},handleFailure:function(o){alert('asynchronous call failed.');},startRequest:function(callback,args,forceAbort){if(this.currentRequestObject!=null){if(this.forceAbort==true||callback.forceAbort==true){YAHOO.util.Connect.abort(this.currentRequestObject,null,false);}}
this.currentRequestObject=YAHOO.util.Connect.asyncRequest('POST',"./index.php?module=Administration&action=Async&to_pdf=true",callback,args);this._reset();},refreshEstimate:function(o){this.ret=YAHOO.lang.JSON.parse(o.responseText);document.getElementById('repairXssDisplay').style.display='inline';document.getElementById('repairXssCount').value=this.ret.count;SUGAR.Administration.RepairXSS.toRepair=this.ret.toRepair;},showRepairXssResult:function(o){var resultCounter=document.getElementById('repairXssResultCount');this.ret=YAHOO.lang.JSON.parse(o.responseText);document.getElementById('repairXssResults').style.display='inline';if(this.ret.msg=='success'){SUGAR.Administration.RepairXSS.repairedCount+=this.ret.count;resultCounter.value=SUGAR.Administration.RepairXSS.repairedCount;}else{resultCounter.value=this.ret;}
SUGAR.Administration.RepairXSS.executeRepair();}};var callbackRepairXssRefreshEstimate={success:AjaxObject.refreshEstimate,failure:AjaxObject.handleFailure,timeout:AjaxObject.timeout,scope:AjaxObject};var callbackRepairXssExecute={success:AjaxObject.showRepairXssResult,failure:AjaxObject.handleFailure,timeout:AjaxObject.timeout,scope:AjaxObject};