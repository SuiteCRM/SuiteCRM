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
 */var addListStudioCount=0;var moduleTabs=[];function ygDDListStudio(id,sGroup,fromOnly){if(id){if(id=='trashcan'||id.indexOf('noselect')==0){this.initTarget(id,sGroup);}else{this.init(id,sGroup);}
this.initFrame();this.fromOnly=fromOnly;}
var s=this.getDragEl().style;s.borderColor="transparent";s.backgroundColor="#f6f5e5";s.opacity=0.76;s.filter="alpha(opacity=76)";}
ygDDListStudio.prototype=new YAHOO.util.DDProxy();ygDDListStudio.prototype.clickContent='';ygDDListStudio.prototype.clickBorder='';ygDDListStudio.prototype.clickHeight='';ygDDListStudio.prototype.lastNode=false;ygDDListStudio.prototype.startDrag
ygDDListStudio.prototype.startDrag=function(x,y){var dragEl=this.getDragEl();var clickEl=this.getEl();this.parentID=clickEl.parentNode.id;dragEl.innerHTML=clickEl.innerHTML;dragElObjects=dragEl.getElementsByTagName('object');dragEl.className=clickEl.className;dragEl.style.color=clickEl.style.color;dragEl.style.border="1px solid #aaa";this.clickContent=clickEl.innerHTML;this.clickBorder=clickEl.style.border;this.clickHeight=clickEl.style.height;clickElRegion=YAHOO.util.Dom.getRegion(clickEl);clickEl.style.height=(clickElRegion.bottom-clickElRegion.top)+'px';clickEl.style.opacity=.5;clickEl.style.filter="alpha(opacity=10)";clickEl.style.border='2px dashed #cccccc';};ygDDListStudio.prototype.updateTabs=function(){moduleTabs=[];for(j=0;j<slotCount;j++){var ul=document.getElementById('ul'+j);moduleTabs[j]=[];items=ul.getElementsByTagName("li");for(i=0;i<items.length;i++){if(items.length==1){items[i].innerHTML=SUGAR.language.get('app_strings','LBL_DROP_HERE');}else{if(items[i].innerHTML==SUGAR.language.get('app_strings','LBL_DROP_HERE')){items[i].innerHTML='';}}
moduleTabs[ul.id.substr(2,ul.id.length)][subtabModules[items[i].id]]=true;}}};ygDDListStudio.prototype.endDrag=function(e){var clickEl=this.getEl();clickEl.innerHTML=this.clickContent
var p=clickEl.parentNode;if(p.id=='trash'){p.removeChild(clickEl);this.lastNode=false;this.updateTabs();return;}
if(this.clickHeight){clickEl.style.height=this.clickHeight;if(this.lastNode)this.lastNode.style.height=this.clickHeight;}
else{clickEl.style.height='';if(this.lastNode)this.lastNode.style.height='';}
if(this.clickBorder){clickEl.style.border=this.clickBorder;if(this.lastNode)this.lastNode.style.border=this.clickBorder;}
else{clickEl.style.border='';if(this.lastNode)this.lastNode.style.border='';}
clickEl.style.opacity=1;clickEl.style.filter="alpha(opacity=100)";if(this.lastNode){this.lastNode.id='addLS'+addListStudioCount;subtabModules[this.lastNode.id]=this.lastNode.module;yahooSlots[this.lastNode.id]=new ygDDListStudio(this.lastNode.id,'subTabs',false);addListStudioCount++;this.lastNode.style.opacity=1;this.lastNode.style.filter="alpha(opacity=100)";}
this.lastNode=false;this.updateTabs();};ygDDListStudio.prototype.onDrag=function(e,id){};ygDDListStudio.prototype.onDragOver=function(e,id){var el;if(this.lastNode){this.lastNode.parentNode.removeChild(this.lastNode);this.lastNode=false;}
if(id.substr(0,7)=='modSlot'){return;}
if("string"==typeof id){el=YAHOO.util.DDM.getElement(id);}else{el=YAHOO.util.DDM.getBestMatch(id).getEl();}
dragEl=this.getDragEl();elRegion=YAHOO.util.Dom.getRegion(el);var mid=YAHOO.util.DDM.getPosY(el)+(Math.floor((elRegion.bottom-elRegion.top)/ 2));var el2=this.getEl();var p=el.parentNode;if((this.fromOnly||(el.id!='trashcan'&&el2.parentNode.id!=p.id&&el2.parentNode.id==this.parentID))){if(typeof(moduleTabs[p.id.substr(2,p.id.length)][subtabModules[el2.id]])!='undefined')return;}
if(this.fromOnly&&el.id!='trashcan'){el2=el2.cloneNode(true);el2.module=subtabModules[el2.id];el2.id='addListStudio'+addListStudioCount;this.lastNode=el2;this.lastNode.clickContent=el2.clickContent;this.lastNode.clickBorder=el2.clickBorder;this.lastNode.clickHeight=el2.clickHeight}
if(YAHOO.util.DDM.getPosY(dragEl)<mid){p.insertBefore(el2,el);}
if(YAHOO.util.DDM.getPosY(dragEl)>=mid){p.insertBefore(el2,el.nextSibling);}};ygDDListStudio.prototype.onDragEnter=function(e,id){};ygDDListStudio.prototype.onDragOut=function(e,id){}
function ygDDListStudioBoundary(id,sGroup){if(id){this.init(id,sGroup);this.isBoundary=true;}}
ygDDListStudioBoundary.prototype=new YAHOO.util.DDTarget();