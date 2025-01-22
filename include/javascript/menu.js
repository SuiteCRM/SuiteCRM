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
 */var menuStack=new Array();var hiddenElmStack=new Array();var currentMenu=null;var closeMenusDelay=null;var openMenusDelay=null;var delayTime=75;function eraseTimeout(tId){window.clearTimeout(tId);return null;}
function tbButtonMouseOverOrig(id){closeMenusDelay=eraseTimeout(closeMenusDelay);var menuName=id.replace(/Handle/i,'Menu');var menu=getLayer(menuName);if(currentMenu){closeAllMenus();}
popupMenu(id,menu);}
function tbButtonMouseOver(id,top,left,leftOffset){closeMenusDelay=eraseTimeout(closeMenusDelay);if(openMenusDelay==null){openMenusDelay=window.setTimeout("showMenu('"+id+"','"+top+"','"+left+"','"+leftOffset+"')",delayTime);}}
function showMenu(id,top,left,leftOffset){openMenusDelay=eraseTimeout(openMenusDelay);var menuName=id.replace(/Handle/i,'Menu');var menu=getLayer(menuName);if(currentMenu){closeAllMenus();}
popupMenu(id,menu,top,left,leftOffset);}
function showSubMenu(id){closeMenusDelay=eraseTimeout(closeMenusDelay);var menuName=id.replace(/Handle/i,'Menu');var menu=getLayer(menuName);popupSubMenu(id,menu);}
function popupMenu(handleID,menu,top,left,leftOffset){var bw=checkBrowserWidth();var menuName=handleID.replace(/Handle/i,'Menu');var menuWidth=120;var imgWidth=document.getElementById(handleID).width;if(menu){var menuHandle=getLayer(handleID);var p=menuHandle;if(left==""){var left=0;while(p&&p.tagName.toUpperCase()!='BODY'){left+=p.offsetLeft;p=p.offsetParent;}
left+=parseInt(leftOffset);}
if(left+menuWidth>bw){left=left-menuWidth+imgWidth;}
setMenuVisible(menu,left,top,false);}}
function popupSubMenu(handleID,menu){if(menu){var menuHandle=getLayer(handleID);var p=menuHandle;var top=0,left=p.offsetWidth;while(p&&p.tagName.toUpperCase()!='BODY'){top+=p.offsetTop;left+=p.offsetLeft;p=p.offsetParent;}
if(is.ie&&is.mac){top-=3;left-=10;}
setMenuVisible(menu,left,top,true);}}
function closeMenusOrig(){if(currentMenu){setMenuVisibility(currentMenu,false);}}
function closeSubMenus(handle){closeMenusDelay=eraseTimeout(closeMenusDelay);if(menuStack.length>0){for(var i=menuStack.length-1;i>=0;i--){var menu=menuStack[menuStack.length-1];if(menu.id==handle.getAttribute('parentid')){currentMenu=menu;break;}else{closeMenu(menu);menuPop();}}}}
function closeMenu(menu){setMenuVisibility(menu,false);}
function closeMenusOrig(){if(menuStack.length>0){for(var i=menuStack.length-1;i>=0;i--){var menu=menuPop();closeMenu(menu);}}
currentMenu=null;}
function closeMenus(){if(closeMenusDelay==null){closeMenusDelay=window.setTimeout("closeAllMenus()",delayTime);}}
function closeAllMenus(){closeMenusDelay=eraseTimeout(closeMenusDelay);if(menuStack.length>0){for(var i=menuStack.length-1;i>=0;i--){var menu=menuPop();closeMenu(menu);}}
currentMenu=null;}
function setMenuVisible(menu,x,y,isSubMenu){if(menu){if(isSubMenu){if(menu.getAttribute('parentid')==currentMenu.getAttribute('parentid')){menuPop();setMenuVisibility(currentMenu,false);}}else{menuPop();setMenuVisibility(currentMenu,false);}
currentMenu=menu;menuPush(menu);setMenuVisibility(menu,true,x,y);}}
function getLayer(layerid){return document.getElementById(layerid);}
function setMenuVisibility(menu,on,x,y){var parent=menu;if(menu){setLayer(menu.id,!on,x,y);if(is.ie){if(!on){if(!menu.getAttribute('parentid')){showElement("SELECT");}}else{hideElement("SELECT",x,y,menu.offsetWidth,menu.offsetHeight);}}}}
function menuPop(){if(is.ie&&(is.mac||!is.ie5_5up)){var menu=menuStack[menuStack.length-1];var newMenuStack=new Array();for(var i=0;i<menuStack.length-1;i++){newMenuStack[newMenuStack.length]=menuStack[i];}
menuStack=newMenuStack;return menu;}else{return menuStack.pop();}}
function menuPush(menu){if(is.ie&&(is.mac||!is.ie5_5up)){menuStack[menuStack.length]=menu;}else{menuStack.push(menu);}}
function checkBrowserWidth(){var windowWidth;if(is.ie){windowWidth=document.body.clientWidth;}else{windowWidth=window.innerWidth-16;}
if(windowWidth>=1000){showSB('sbContent',true,'sb');}else{showSB('sbContent',false,'sb');}
return windowWidth;}
function showSB(id,hideit,imgIdPrefix){setLayer(id,!hideit,-1,-1);setLayer(imgIdPrefix+'On',!hideit,-1,-1);setLayer(imgIdPrefix+'Off',hideit,-1,-1);}
function setLayer(id,hidden,x,y){var layer=getLayer(id);setLayerElm(layer,hidden,x,y);}
function setLayerElm(layer,hideit,x,y){if(layer&&layer.style){if(hideit){layer.style.visibility='hidden';}else{layer.style.display='block';layer.style.visibility='visible';}
if(x>=0&&x!=""){layer.style.left=x+'px';}
if(y>=0&&y!=""){layer.style.top=y+'px';}}}
function hiliteItem(menuItem,changeClass){closeMenusDelay=eraseTimeout(closeMenusDelay);if(changeClass=='yes'){if(menuItem.getAttribute('avid')=='false'){menuItem.className='menuItemHiliteX';}else{menuItem.className='menuItemHilite';}}}
function unhiliteItem(menuItem){closeMenusDelay=eraseTimeout(closeMenusDelay);if(menuItem.getAttribute('avid')=='false'){menuItem.className='menuItemX';}else{menuItem.className='menuItem';}}
function showElement(elmID){for(i=0;i<document.getElementsByTagName(elmID).length;i++){obj=document.getElementsByTagName(elmID)[i];if(!obj||!obj.offsetParent)
continue;obj.style.visibility="";}}
function showElementNew(elmID){if(hiddenElmStack.length>0){for(var i=hiddenElmStack.length-1;i>=0;i--){var obj=hiddenElmStack[hiddenElmStack.length-1];obj.style.visibility="";;hiddenElmStack.pop();}}}
function hideElement(elmID,x,y,w,h){for(i=0;i<document.getElementsByTagName(elmID).length;i++){obj=document.getElementsByTagName(elmID)[i];if(!obj||!obj.offsetParent)
continue;objLeft=obj.offsetLeft;objTop=obj.offsetTop;objParent=obj.offsetParent;while(objParent.tagName.toUpperCase()!="BODY"){objLeft+=objParent.offsetLeft;objTop+=objParent.offsetTop;if(objParent.offsetParent==null)
break;else
objParent=objParent.offsetParent;}
objTop=objTop-y;if(x>(objLeft+obj.offsetWidth)||objLeft>(x+w));else if(objTop>h);else if((y+h)<=80);else{obj.style.visibility="hidden";}}}
function Is(){var agt=navigator.userAgent.toLowerCase();this.major=parseInt(navigator.appVersion);this.minor=parseFloat(navigator.appVersion);this.nav=((agt.indexOf('mozilla')!=-1)&&(agt.indexOf('spoofer')==-1)&&(agt.indexOf('compatible')==-1)&&(agt.indexOf('opera')==-1)&&(agt.indexOf('webtv')==-1)&&(agt.indexOf('hotjava')==-1));this.nav2=(this.nav&&(this.major==2));this.nav3=(this.nav&&(this.major==3));this.nav4=(this.nav&&(this.major==4));this.nav4up=(this.nav&&(this.major>=4));this.navonly=(this.nav&&((agt.indexOf(";nav")!=-1)||(agt.indexOf("; nav")!=-1)));this.nav6=(this.nav&&(this.major==5));this.nav6up=(this.nav&&(this.major>=5));this.gecko=(agt.indexOf('gecko')!=-1);this.nav7=(this.gecko&&(this.major>=5)&&(agt.indexOf('netscape/7')!=-1));this.moz1=false;this.moz1up=false;this.moz1_1=false;this.moz1_1up=false;if(this.nav6up){myRegEx=new RegExp("rv:\\d*.\\d*.\\d*");myFind=myRegEx.exec(agt);if(myFind!=null){var strVersion=myFind.toString();strVersion=strVersion.replace(/rv:/,'');var arrVersion=strVersion.split('.');var major=parseInt(arrVersion[0]);var minor=parseInt(arrVersion[1]);if(arrVersion[2])var revision=parseInt(arrVersion[2]);this.moz1=((major==1)&&(minor==0));this.moz1up=((major==1)&&(minor>=0));this.moz1_1=((major==1)&&(minor==1));this.moz1_1up=((major==1)&&(minor>=1));}}
this.ie=((agt.indexOf("msie")!=-1)&&(agt.indexOf("opera")==-1));this.ie3=(this.ie&&(this.major<4));this.ie4=(this.ie&&(this.major==4)&&(agt.indexOf("msie 4")!=-1));this.ie4up=(this.ie&&(this.major>=4));this.ie5=(this.ie&&(this.major==4)&&(agt.indexOf("msie 5.0")!=-1));this.ie5_5=(this.ie&&(this.major==4)&&(agt.indexOf("msie 5.5")!=-1));this.ie5up=(this.ie&&!this.ie3&&!this.ie4);this.ie5_5up=(this.ie&&!this.ie3&&!this.ie4&&!this.ie5);this.ie6=(this.ie&&(this.major==4)&&(agt.indexOf("msie 6.")!=-1));this.ie6up=(this.ie&&!this.ie3&&!this.ie4&&!this.ie5&&!this.ie5_5);this.mac=(agt.indexOf("mac")!=-1);}
function runPageLoadItems(){var myVar;checkBrowserWidth();}
var is=new Is();if(is.ie){document.write('<style type="text/css">');document.write('body {font-size: x-small;}');document.write('</style>');}