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
 */function ygDDSlot(id,sGroup){if(id){this.init(id,sGroup);this.initFrame();}
var s=this.getDragEl().style;s.borderColor="transparent";s.backgroundColor="#f6f5e5";s.opacity=0.76;s.filter="alpha(opacity=76)";this.resizeFrame=true;if(id=='s_field_delete'){this.isValidHandle=false;}}
ygDDSlot.prototype=new YAHOO.util.DDProxy();ygDDSlot.prototype.handleDelete=function(cur,curb){var parentID=(typeof(cur.parentID)=='undefined')?cur.id.substr(4,cur.id.length):cur.parentID;if(parentID.indexOf('field')==0){return false;}
var myfieldcount=field_count_MSI;addNewField('dyn_field_'+field_count_MSI,'delete','&nbsp;','&nbsp;','deleted',0,'studio_fields')
yahooSlots["dyn_field_"+myfieldcount]=new ygDDSlot("dyn_field_"+myfieldcount,"studio");ygDDSlot.prototype.handleSwap(cur,curb,document.getElementById("dyn_field_"+myfieldcount),document.getElementById("dyn_field_"+myfieldcount+'b'),true);}
ygDDSlot.prototype.undo=function(transaction){ygDDSlot.prototype.handleSwap(document.getElementById(transaction['el']),document.getElementById(transaction['elb']),document.getElementById(transaction['cur']),document.getElementById(transaction['curb']),false);}
ygDDSlot.prototype.redo=function(transaction){ygDDSlot.prototype.handleSwap(document.getElementById(transaction['el']),document.getElementById(transaction['elb']),document.getElementById(transaction['cur']),document.getElementById(transaction['curb']),false);}
ygDDSlot.prototype.handleSwap=function(cur,curb,el,elb,record){if(record){if(curb){jstransaction.record('studioSwap',{'cur':cur.id,'curb':curb.id,'el':el.id,'elb':elb.id});}else{jstransaction.record('studioSwap',{'cur':cur.id,'curb':null,'el':el.id,'elb':null});}}
var parentID1=(typeof(el.parentID)=='undefined')?el.id.substr(4,el.id.length):el.parentID;var parentID2=(typeof(cur.parentID)=='undefined')?cur.id.substr(4,cur.id.length):cur.parentID;var slot1=YAHOO.util.DDM.getElement("slot_"+parentID1);var slot2=YAHOO.util.DDM.getElement("slot_"+parentID2);var temp=slot1.value;slot1.value=slot2.value;slot2.value=temp;YAHOO.util.DDM.swapNode(cur,el);if(curb){YAHOO.util.DDM.swapNode(curb,elb);}
cur.parentID=parentID1;el.parentID=parentID2;if(parentID1.indexOf('field')==0){if(curb)curb.style.display='none';setMouseOverForField(cur,true);}else{if(curb)curb.style.display='inline';setMouseOverForField(cur,false);}
if(parentID2.indexOf('field')==0){if(elb)elb.style.display='none';setMouseOverForField(el,true);}else{if(elb)elb.style.display='inline';setMouseOverForField(el,false);}}
ygDDSlot.prototype.onDragDrop=function(e,id){var cur=this.getEl();var curb;if("string"==typeof id){curb=YAHOO.util.DDM.getElement(cur.id+"b");}else{curb=YAHOO.util.DDM.getBestMatch(cur.id+"b").getEl();}
if(id=='s_field_delete'){id=ygDDSlot.prototype.handleDelete(cur,curb);if(!id)return false;}
var el;if("string"==typeof id){el=YAHOO.util.DDM.getElement(id);}else{el=YAHOO.util.DDM.getBestMatch(id).getEl();}
id2=el.id+"b";if("string"==typeof id){elb=YAHOO.util.DDM.getElement(id2);}else{elb=YAHOO.util.DDM.getBestMatch(id2).getEl();}
ygDDSlot.prototype.handleSwap(cur,curb,el,elb,true)
var dragEl=this.getDragEl();dragEl.innerHTML='';};ygDDSlot.prototype.startDrag=function(x,y){var dragEl=this.getDragEl();var clickEl=this.getEl();dragEl.innerHTML=clickEl.innerHTML;dragEl.className=clickEl.className;dragEl.style.color=clickEl.style.color;dragEl.style.border="2px solid #aaa";this.clickContent=clickEl.innerHTML;this.clickBorder=clickEl.style.border;this.clickHeight=clickEl.style.height;clickElRegion=YAHOO.util.Dom.getRegion(clickEl);dragEl.style.height=clickEl.style.height;dragEl.style.width=(clickElRegion.right-clickElRegion.left)+'px';clickEl.style.height=(clickElRegion.bottom-clickElRegion.top)+'px';clickEl.style.border='2px dashed #cccccc';clickEl.style.opacity=.5;clickEl.style.filter="alpha(opacity=10)";};ygDDSlot.prototype.endDrag=function(e){var clickEl=this.getEl();if(this.clickHeight)
clickEl.style.height=this.clickHeight;else
clickEl.style.height='';if(this.clickBorder)
clickEl.style.border=this.clickBorder;else
clickEl.style.border='';clickEl.style.opacity=1;clickEl.style.filter="alpha(opacity=100)";};jstransaction.register('studioSwap',ygDDSlot.prototype.undo,ygDDSlot.prototype.redo);