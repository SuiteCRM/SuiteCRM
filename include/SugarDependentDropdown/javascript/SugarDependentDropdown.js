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
 */SUGAR.dependentDropdown={currentAction:null,debugMode:false}
SUGAR.dependentDropdown.handleDependentDropdown=function(el){}
SUGAR.dependentDropdown.generateElement=function(focusElement,elementRow,index,elementIndex){if(SUGAR.dependentDropdown.debugMode)SUGAR.dependentDropdown.utils.debugStack('generateElement');var tmp=null;if(focusElement){var sandbox=SUGAR.dependentDropdown.utils.generateElementContainer(focusElement,elementRow,index,elementIndex);if(focusElement.label){focusLabel={tag:'span',cls:'routingLabel',html:"&nbsp;"+focusElement.label+"&nbsp;"}
switch(focusElement.label_pos){case"top":focusLabel.html=focusElement.label+"<br />";break;case"bottom":focusLabel.html="<br />"+focusElement.label;break;}
if(focusElement.label_pos=='left'||focusElement.label_pos=='top'){YAHOO.ext.DomHelper.append(sandbox,focusLabel);}}
switch(focusElement.type){case'input':if(typeof(focusElement.values)=='string'){SUGAR.util.globalEval('retValue = '+focusElement.values);focusElement.values=retValue;}
var preselect=SUGAR.dependentDropdown.utils.getPreselectKey(focusElement.name);if(preselect.match(/::/))
preselect='';tmp=YAHOO.ext.DomHelper.append(sandbox,{tag:'input',id:focusElement.grouping+"::"+index+":::"+elementIndex+":-:"+focusElement.id,name:focusElement.grouping+"::"+index+"::"+focusElement.name,cls:'input',onchange:focusElement.onchange,value:preselect},true);var newElement=tmp.dom;break;case'select':tmp=YAHOO.ext.DomHelper.append(sandbox,{tag:'select',id:focusElement.grouping+"::"+index+":::"+elementIndex+":-:"+focusElement.id,name:focusElement.grouping+"::"+index+"::"+focusElement.name,cls:'input',onchange:focusElement.onchange},true);var newElement=tmp.dom;if(typeof(focusElement.values)=='string'){SUGAR.util.globalEval('retValue = '+focusElement.values);focusElement.values=retValue;}
var preselect=SUGAR.dependentDropdown.utils.getPreselectKey(focusElement.name);var i=0;for(var key in focusElement.values){var selected=(preselect==key)?true:false;newElement.options[i]=new Option(focusElement.values[key],key,selected);if(selected){newElement.options[i].selected=true;}
i++;}
break;case'none':break;case'checkbox':alert('implement checkbox pls');break;case'multiple':alert('implement multiple pls');break;default:if(SUGAR.dependentDropdown.dropdowns.debugMode){alert("Improper type defined: [ "+focusElement.type+"]");}
return;break;}
if(focusElement.label){if(focusElement.label_pos=='right'||focusElement.label_pos=='bottom'){YAHOO.ext.DomHelper.append(sandbox,focusLabel);}}
try{newElement.onchange();}catch(e){if(SUGAR.dependentDropdown.dropdowns.debugMode){debugger;}}}else{}}
SUGAR.dependentDropdown.utils={generateElementContainer:function(focusElement,elementRow,index,elementIndex){var oldElement=document.getElementById('elementContainer'+focusElement.grouping+"::"+index+":::"+elementIndex);if(oldElement){SUGAR.dependentDropdown.utils.removeChildren(oldElement);}
var tmp=YAHOO.ext.DomHelper.append(elementRow,{tag:'span',id:'elementContainer'+focusElement.grouping+"::"+index+":::"+elementIndex},true);return tmp.dom;},getPreselectKey:function(elementName){try{if(SUGAR.dependentDropdown.currentAction.action[elementName]){return SUGAR.dependentDropdown.currentAction.action[elementName];}else{return'';}}catch(e){if(SUGAR.dependentDropdown.dropdowns.debugMode){}
return'';}},debugStack:function(func){if(!SUGAR.dependentDropdown._stack){SUGAR.dependentDropdown._stack=new Array();}
SUGAR.dependentDropdown._stack.push(func);},removeChildren:function(el){for(i=el.childNodes.length-1;i>=0;i--){if(el.childNodes[i]){el.removeChild(el.childNodes[i]);}}}}