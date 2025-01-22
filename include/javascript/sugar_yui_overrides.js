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
 */YAHOO.widget.Panel.prototype.configClose=function(type,args,obj){var val=args[0],oClose=this.close,strings=this.cfg.getProperty("strings"),fc;if(val){if(!oClose){if(!this.m_oCloseIconTemplate){this.m_oCloseIconTemplate=document.createElement("a");this.m_oCloseIconTemplate.className="container-close";this.m_oCloseIconTemplate.href="#";}
oClose=this.m_oCloseIconTemplate.cloneNode(true);fc=this.innerElement.firstChild;if(fc){if(fc.className==this.m_oCloseIconTemplate.className){this.innerElement.replaceChild(oClose,fc);}else{this.innerElement.insertBefore(oClose,fc);}}else{this.innerElement.appendChild(oClose);}
oClose.innerHTML=(strings&&strings.close)?strings.close:"&#160;";YAHOO.util.Event.on(oClose,"click",this._doClose,this,true);this.close=oClose;}else{oClose.style.display="block";}}else{if(oClose){oClose.style.display="none";}}}
YAHOO.widget.Overlay.prototype.center=function(){var scrollX=document.documentElement.scrollLeft||document.body.scrollLeft;var scrollY=document.documentElement.scrollTop||document.body.scrollTop;var viewPortWidth=YAHOO.util.Dom.getClientWidth();var viewPortHeight=YAHOO.util.Dom.getClientHeight();var elementWidth=this.element.offsetWidth;var elementHeight=this.element.offsetHeight;var x=(viewPortWidth / 2)-(elementWidth / 2)+scrollX;var y=(viewPortHeight / 2)-(elementHeight / 2)+scrollY;this.element.style.left=parseInt(x,10)+"px";this.element.style.top=parseInt(y,10)+"px";this.syncPosition();this.cfg.refireEvent("iframe");}
YAHOO.SUGAR.DragDropTable.prototype._deleteTrEl=function(row){var rowIndex;if(!YAHOO.lang.isNumber(row)){rowIndex=Dom.get(row).sectionRowIndex;}
else{rowIndex=row;}
if(YAHOO.lang.isNumber(rowIndex)&&(rowIndex>-1)&&(rowIndex<this._elTbody.rows.length)){return this._elTbody.removeChild(this._elTbody.rows[row]);}
else{return null;}}