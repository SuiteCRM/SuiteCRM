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
 */(function(){var Dom=YAHOO.util.Dom,Event=YAHOO.util.Event;SUGAR.AddressField=function(checkId,fromKey,toKey){this.fromKey=fromKey;this.toKey=toKey;Event.onAvailable(checkId,this.testCheckboxReady,this);}
SUGAR.AddressField.prototype={elems:["address_street","address_city","address_state","address_postalcode","address_country"],tHasText:false,syncAddressCheckbox:true,originalBgColor:'#FFFFFF',testCheckboxReady:function(obj){for(var x in obj.elems){var f=obj.fromKey+"_"+obj.elems[x];var t=obj.toKey+"_"+obj.elems[x];var e1=Dom.get(t);var e2=Dom.get(f);if(e1!=null&&typeof e1!="undefined"&&e2!=null&&typeof e2!="undefined"){if(!obj.tHasText&&YAHOO.lang.trim(e1.value)!=""){obj.tHasText=true;}
if(e1.value!=e2.value){obj.syncAddressCheckbox=false;break;}
obj.originalBgColor=e1.style.backgroundColor;}}
if(obj.tHasText&&obj.syncAddressCheckbox){Dom.get(this.id).checked=true;obj.syncFields();}},writeToSyncField:function(e){var fromEl=Event.getTarget(e,true);if(typeof fromEl!="undefined"){var toEl=Dom.get(fromEl.id.replace(this.fromKey,this.toKey));var update=toEl.value!=fromEl.value;toEl.value=fromEl.value;if(update)SUGAR.util.callOnChangeListers(toEl);}},syncFields:function(fromKey,toKey){var fk=this.fromKey,tk=this.toKey;for(var x in this.elems){var f=fk+"_"+this.elems[x];var e2=Dom.get(f);var t=tk+"_"+this.elems[x];var e1=Dom.get(t);if(e1!=null&&typeof e1!="undefined"&&e2!=null&&typeof e2!="undefined"){if(!Dom.get(tk+'_checkbox').checked){Dom.setStyle(e1,'backgroundColor',this.originalBgColor);e1.removeAttribute('readOnly');Event.removeListener(e2,'change',this.writeToSyncField);}else{var update=e1.value!=e2.value;e1.value=e2.value;if(update)SUGAR.util.callOnChangeListers(e1);Dom.setStyle(e1,'backgroundColor','#DCDCDC');e1.setAttribute('readOnly',true);Event.addListener(e2,'change',this.writeToSyncField,this,true);}}}}};})();