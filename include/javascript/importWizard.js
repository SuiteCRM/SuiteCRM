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
 */SUGAR.importWizard={};SUGAR.importWizard=function(){return{renderDialog:function(importModuleVAR,actionVar,sourceVar){var oBody=document.getElementsByTagName('BODY').item(0);if(!document.getElementById("importWizardDialog")){var importWizardDialogDiv=document.createElement("div");importWizardDialogDiv.id="importWizardDialog";importWizardDialogDiv.style.display="none";importWizardDialogDiv.className="dashletPanelMenu wizard import";importWizardDialogDiv.innerHTML='<div class="hd"><a href="javascript:void(0)" onClick="javascript:SUGAR.importWizard.closeDialog();"><div class="container-close">&nbsp;</div></a><div class="title" id="importWizardDialogTitle"></div></div><div class="bd"><div class="screen" id="importWizardDialogDiv"></div><div id="submitDiv"></div></div>';oBody.appendChild(importWizardDialogDiv);}
YAHOO.util.Event.onContentReady("importWizardDialog",function(){SUGAR.importWizard.dialog=new YAHOO.widget.Dialog("importWizardDialog",{width:"950px",height:"565px",fixedcenter:true,draggable:false,visible:false,modal:true,close:false});var oHead=document.getElementsByTagName('HEAD').item(0);if(!document.getElementById("sugar_grp_yui_widgets")){var oScript=document.createElement("script");oScript.type="text/javascript";oScript.id="sugar_grp_yui_widgets";oScript.src="cache/include/javascript/sugar_grp_yui_widgets.js";oHead.appendChild(oScript);}
var success=function(data){var response=YAHOO.lang.JSON.parse(data.responseText);importWizardDialogDiv=document.getElementById('importWizardDialogDiv');var submitDiv=document.getElementById('submitDiv');var importWizardDialogTitle=document.getElementById('importWizardDialogTitle');importWizardDialogDiv.innerHTML=response['html'];importWizardDialogTitle.innerHTML=response['title'];submitDiv.innerHTML=response['submitContent'];document.getElementById('importWizardDialog').style.display='';SUGAR.importWizard.dialog.render();SUGAR.importWizard.dialog.show();SUGAR.util.globalEval(response['script']);}
var cObj=YAHOO.util.Connect.asyncRequest('GET','index.php?module=Import&action='+actionVar+'&import_module='+importModuleVAR+'&source='+sourceVar,{success:success,failure:success});return false;});},closeDialog:function(){SUGAR.importWizard.dialog.hide();var importWizardDialogDiv=document.getElementById('importWizardDialogDiv');var submitDiv=document.getElementById('submitDiv');importWizardDialogDiv.innerHTML="";submitDiv.innerHTML="";SUGAR.importWizard.dialog.destroy();},renderLoadingDialog:function(){SUGAR.importWizard.loading=new YAHOO.widget.Panel("loading",{width:"240px",fixedcenter:true,close:false,draggable:false,constraintoviewport:false,modal:true,visible:false,effect:[{effect:YAHOO.widget.ContainerEffect.SLIDE,duration:0.5},{effect:YAHOO.widget.ContainerEffect.FADE,duration:.5}]});SUGAR.importWizard.loading.setBody('<div id="loadingPage" align="center" style="vertical-align:middle;"><img src="'+SUGAR.themes.image_server+'index.php?entryPoint=getImage&themeName='+SUGAR.themes.theme_name+'&imageName=img_loading.gif" align="absmiddle" /> <b>'+SUGAR.language.get('app_strings','LBL_LOADING_PAGE')+'</b></div>');SUGAR.importWizard.loading.render(document.body);if(document.getElementById('loading_c'))
document.getElementById('loading_c').style.display='none';}};}();