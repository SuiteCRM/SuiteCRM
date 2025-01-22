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
 */if(typeof(SUGAR)=='undefined'){var SUGAR={};}
SUGAR.Administration={Async:{},RepairXSS:{toRepair:new Object,currentRepairObject:"",currentRepairIds:new Array(),repairedCount:0,numberToFix:25,refreshEstimate:function(select){this.toRepair=new Object();this.repairedCount=0;var button=document.getElementById('repairXssButton');var selected=select.value;var totalDisplay=document.getElementById('repairXssDisplay');var counter=document.getElementById('repairXssCount');var repaired=document.getElementById('repairXssResults');var repairedCounter=document.getElementById('repairXssResultCount');if(selected!="0"){button.style.display='inline';repairedCounter.value=0;AjaxObject.startRequest(callbackRepairXssRefreshEstimate,"&adminAction=refreshEstimate&bean="+selected);}else{button.style.display='none';totalDisplay.style.display='none';repaired.style.display='none';counter.value=0;repaired.value=0;}},executeRepair:function(){if(this.toRepair){if(this.currentRepairIds.length<1){if(!this.loadRepairQueue()){alert(done);return;}}
var beanIds=new Array();for(var i=0;i<this.numberToFix;i++){if(this.currentRepairIds.length>0){beanIds.push(this.currentRepairIds.pop());}}
var beanId=YAHOO.lang.JSON.stringify(beanIds);AjaxObject.startRequest(callbackRepairXssExecute,"&adminAction=repairXssExecute&bean="+this.currentRepairObject+"&id="+beanId);}},loadRepairQueue:function(){var loaded=false;this.currentRepairObject='';this.currentRepairIds=new Array();for(var bean in this.toRepair){if(this.toRepair[bean].length>0){this.currentRepairObject=bean;this.currentRepairIds=this.toRepair[bean];loaded=true;}}
this.toRepair[this.currentRepairObject]=new Array();return loaded;}}}