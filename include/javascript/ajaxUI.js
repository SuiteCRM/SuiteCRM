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
 */SUGAR.ajaxUI={loadingWindow:false,callback:function(o){var cont;if(typeof window.onbeforeunload=="function")
window.onbeforeunload=null;scroll(0,0);try{var r=YAHOO.lang.JSON.parse(o.responseText);cont=r.content;if(r.title){document.title=html_entity_decode(r.title);}
if(r.action){action_sugar_grp1=r.action;}
if(r.menu.module){module_sugar_grp1=r.menu.module;}
if(r.favicon){SUGAR.ajaxUI.setFavicon(r.favicon);}
var c=document.getElementById("content");c.style.visibility='hidden';c.innerHTML=cont;SUGAR.util.evalScript(cont);c.style.visibility='visible';if(r.moduleList){SUGAR.themes.setModuleTabs(r.moduleList);}
if(typeof(r.responseTime)!='undefined'){var rt=$("#responseTime");if(rt.length>0){rt.html(rt.html().replace(/[\d]+\.[\d]+/,r.responseTime));}
else if(typeof(logoStats)!="undefined"){$("#logo").attr("title",logoStats.replace(/[\d]+\.[\d]+/,r.responseTime)).tipTip({maxWidth:"auto",edgeOffset:10});}}
SUGAR.ajaxUI.hideLoadingPanel();}catch(e){SUGAR.ajaxUI.hideLoadingPanel();SUGAR.ajaxUI.showErrorMessage(o.responseText);}
SUGAR_callsInProgress--;},showErrorMessage:function(errorMessage){if(!SUGAR.ajaxUI.errorPanel){SUGAR.ajaxUI.errorPanel=new YAHOO.widget.Panel("ajaxUIErrorPanel",{modal:false,visible:true,width:"800px",height:"600px",close:true});}
var panel=SUGAR.ajaxUI.errorPanel;panel.setHeader(SUGAR.language.get('app_strings','ERR_AJAX_LOAD'));panel.setBody('<iframe id="ajaxErrorFrame" style="width:780px;height:550px;border:none;marginheight="0" marginwidth="0" frameborder="0""></iframe>');panel.setFooter(SUGAR.language.get('app_strings','ERR_AJAX_LOAD_FOOTER'));panel.render(document.body);SUGAR.util.doWhen(function(){var f=document.getElementById("ajaxErrorFrame");return f!=null&&f.contentWindow!=null&&f.contentWindow.document!=null;},function(){document.getElementById("ajaxErrorFrame").contentWindow.document.body.innerHTML=errorMessage;window.setTimeout('throw "AjaxUI error parsing response"',300);});SUGAR.ajaxUI.errorMessage=errorMessage;window.setTimeout('if((typeof(document.getElementById("ajaxErrorFrame")) == "undefined" || typeof(document.getElementById("ajaxErrorFrame")) == null  || document.getElementById("ajaxErrorFrame").contentWindow.document.body.innerHTML == "")){document.getElementById("ajaxErrorFrame").contentWindow.document.body.innerHTML=SUGAR.ajaxUI.errorMessage;}',3000);panel.show();panel.center();throw"AjaxUI error parsing response";},canAjaxLoadModule:function(module){var checkLS=/&LicState=check/.exec(window.location.search);if(checkLS||(typeof(SUGAR.config.disableAjaxUI)!='undefined'&&SUGAR.config.disableAjaxUI==true)){return false;}
var bannedModules=SUGAR.config.stockAjaxBannedModules;if(typeof(bannedModules)=='undefined')
return false;if(typeof(SUGAR.config.addAjaxBannedModules)!='undefined'){bannedModules.concat(SUGAR.config.addAjaxBannedModules);}
if(typeof(SUGAR.config.overrideAjaxBannedModules)!='undefined'){bannedModules=SUGAR.config.overrideAjaxBannedModules;}
return SUGAR.util.arrayIndexOf(bannedModules,module)==-1;},loadContent:function(url,params){if(YAHOO.lang.trim(url)!=""){var testUrl=decodeURIComponent(url);if(/^index.php?(([A-Z]|[a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+[%=+#&\[\]\.]*)+/i.test(testUrl)===false){throw"Invalid url";}
var mRegex=/module=([^&]*)/.exec(url);var module=mRegex?mRegex[1]:false;if(module&&SUGAR.ajaxUI.canAjaxLoadModule(module)){YAHOO.util.History.navigate('ajaxUILoc',url);}else{window.location=url;}}},go:function(url){if(YAHOO.lang.trim(url)!=""){var con=YAHOO.util.Connect,ui=SUGAR.ajaxUI;if(ui.lastURL==url){return;}
var testUrl=decodeURIComponent(url);if(/^index.php?(([A-Z]|[a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+[%=+#&\[\]\.]*)+/i.test(testUrl)===false){throw"Invalid url";}
var inAjaxUI=/action=ajaxui/.exec(window.location);if(typeof(window.onbeforeunload)=="function"&&window.onbeforeunload()){if(!confirm(window.onbeforeunload())){if(!inAjaxUI){window.location.hash="";}
else{YAHOO.util.History.navigate('ajaxUILoc',ui.lastURL);}
return;}
window.onbeforeunload=null;}
if(ui.lastCall&&con.isCallInProgress(ui.lastCall)){con.abort(ui.lastCall);}
var mRegex=/module=([^&]*)/.exec(url);var module=mRegex?mRegex[1]:false;if(!ui.canAjaxLoadModule(module)){window.location=url;}
ui.lastURL=url;ui.cleanGlobals();var loadLanguageJS='';if(module&&typeof(SUGAR.language.languages[module])=='undefined'){loadLanguageJS='&loadLanguageJS=1';}
if(!inAjaxUI){if(!SUGAR.isIE)
window.location.replace("index.php?action=ajaxui#ajaxUILoc="+encodeURIComponent(url));else{window.location.hash="#";window.location.assign("index.php?action=ajaxui#ajaxUILoc="+encodeURIComponent(url));}}
else{SUGAR_callsInProgress++;SUGAR.ajaxUI.showLoadingPanel();ui.lastCall=YAHOO.util.Connect.asyncRequest('GET',url+'&ajax_load=1'+loadLanguageJS,{success:SUGAR.ajaxUI.callback,failure:function(){SUGAR_callsInProgress--;SUGAR.ajaxUI.hideLoadingPanel();SUGAR.ajaxUI.showErrorMessage(SUGAR.language.get('app_strings','ERR_AJAX_LOAD_FAILURE'));}});}}},submitForm:function(formname,params){var con=YAHOO.util.Connect,SA=SUGAR.ajaxUI;if(SA.lastCall&&con.isCallInProgress(SA.lastCall)){con.abort(SA.lastCall);}
SA.cleanGlobals();var form=YAHOO.util.Dom.get(formname)||document.forms[formname];if(SA.canAjaxLoadModule(form.module.value)&&typeof(YAHOO.util.Selector.query("input[type=file]",form)[0])=="undefined"&&/action=ajaxui/.exec(window.location)){var string=con.setForm(form);var baseUrl="index.php?action=ajaxui#ajaxUILoc=";SA.lastURL="";if(string.length>200){SUGAR.ajaxUI.showLoadingPanel();form.onsubmit=function(){return true;};form.submit();}else{con.resetFormState();window.location=baseUrl+encodeURIComponent("index.php?"+string);}
return true;}else{if(typeof(YAHOO.util.Selector.query("input[type=submit]",form)[0])!="undefined"&&YAHOO.util.Selector.query("input[type=submit]",form)[0].value=="Save"){ajaxStatus.showStatus(SUGAR.language.get('app_strings','LBL_SAVING'));}
form.submit();return false;}},cleanGlobals:function(){sqs_objects={};QSProcessedFieldsArray={};collection={};if(SUGAR.EmailAddressWidget){SUGAR.EmailAddressWidget.instances={};SUGAR.EmailAddressWidget.count={};}
YAHOO.util.Event.removeListener(window,'resize');if(typeof(dialog)!='undefined'&&typeof(dialog.destroy)=='function'){dialog.destroy();delete dialog;}},firstLoad:function(){var url=YAHOO.util.History.getBookmarkedState('ajaxUILoc');var aRegex=/action=([^&#]*)/.exec(window.location);var action=aRegex?aRegex[1]:false;var mRegex=/module=([^&#]*)/.exec(window.location);var module=mRegex?mRegex[1]:false;if(module!="ModuleBuilder"){var go=url!=null||action=="ajaxui";url=url?url:'index.php?module=Home&action=index';YAHOO.util.History.register('ajaxUILoc',url,SUGAR.ajaxUI.go);YAHOO.util.History.initialize("ajaxUI-history-field","ajaxUI-history-iframe");SUGAR.ajaxUI.hist_loaded=true;if(go)
SUGAR.ajaxUI.go(url);}
SUGAR_callsInProgress--;},print:function(){var url=YAHOO.util.History.getBookmarkedState('ajaxUILoc');SUGAR.util.openWindow(url+'&print=true','printwin','menubar=1,status=0,resizable=1,scrollbars=1,toolbar=0,location=1');},showLoadingPanel:function(){if(!SUGAR.ajaxUI.loadingPanel){SUGAR.ajaxUI.loadingPanel=new YAHOO.widget.Panel("ajaxloading",{width:"240px",fixedcenter:true,close:false,draggable:false,constraintoviewport:false,modal:true,visible:false});SUGAR.ajaxUI.loadingPanel.setBody('<div id="loadingPage" align="center" style="vertical-align:middle;"><img src="'+SUGAR.themes.loading_image+'" align="absmiddle" /> <b>'+SUGAR.language.get('app_strings','LBL_LOADING_PAGE')+'</b></div>');SUGAR.ajaxUI.loadingPanel.render(document.body);}
if(document.getElementById('ajaxloading_c'))
document.getElementById('ajaxloading_c').style.display='';SUGAR.ajaxUI.loadingPanel.show();},hideLoadingPanel:function(){SUGAR.ajaxUI.loadingPanel.hide();if(document.getElementById('ajaxloading_c'))
document.getElementById('ajaxloading_c').style.display='none';},setFavicon:function(data){var head=document.getElementsByTagName("head")[0];var links=head.getElementsByTagName("link");var re=/\bicon\b/i;for(var i=0;i<links.length;i++){if(re.test(links[i].rel)){head.removeChild(links[i]);}}
var link=document.createElement("link");link.href=data.url;link.type=data.type;link.rel="icon";head.appendChild(link);}};