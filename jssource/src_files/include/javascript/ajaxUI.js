/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */


SUGAR.ajaxUI = {
    // STIC Custom 20230510 JBL - Reload js and css without cleaning browse cache
    // STIC#1075
    stic_cssjscustomversion : "",
    stic_lastversionloadstate : "Loaded", 
    // End STIC CUSTOM
    callback : function(o)
    {
        // STIC Custom 20230510 JBL - Reload js and css without cleaning browse cache
        // STIC#1075
        // How it works: 
        //   - The ajax responses have Headers with last versions
        //   - There is the current version in the Cookie
        //   - When a version change is detected:
        //        - Update current version in the Cookie
        //        - Update current Version Change state
        //        - Reload page
        //   - Version Change States:
        //        - Reload -> There is a new version in the server: Reload 
        //             - Reload page, disable Ajax to force download all resources
        //        - AjaxDisabled -> The page is reloaded without Ajax
        //             - Enable Ajax in the next call
        //        - AjaxEnabled -> Ajax calls are enabled again
        //             - Check resource versions in the next ajax call
        //        - Loaded -> Resorces are all updated, checked in an ajax call 
        if (typeof o.getResponseHeader['stic-cssjscustomversion'] !== 'undefined' &&
            typeof o.getResponseHeader['stic-cssjslangversion'] !== 'undefined') {

            var header_cssJsCustomVersion = o.getResponseHeader['stic-cssjscustomversion'];
            var header_cssjslangversion = o.getResponseHeader['stic-cssjslangversion'];
            
            SUGAR.ajaxUI.stic_cssjscustomversion = header_cssJsCustomVersion;

            // Get cookie value for current version in browser
            var cookie_cssJsCustomVersion = Get_Cookie('stic_cssjscustomversion');
            var cookie_cssjslangversion = Get_Cookie('stic_cssjslangversion');
            var sameVersionInCookie = cookie_cssJsCustomVersion == header_cssJsCustomVersion &&
                                      cookie_cssjslangversion == header_cssjslangversion;
            if (cookie_cssJsCustomVersion == null || cookie_cssjslangversion == null) {
                sameVersionInCookie = true;
                // Set last version in the cookie
                Set_Cookie('stic_cssjscustomversion', header_cssJsCustomVersion, 3000, false, false, false);
                Set_Cookie('stic_cssjslangversion', header_cssjslangversion, 3000, false, false, false);
            }
            if (!sameVersionInCookie) {
                // Set last version in the cookie
                Set_Cookie('stic_cssjscustomversion', header_cssJsCustomVersion, 3000, false, false, false);
                Set_Cookie('stic_cssjslangversion', header_cssjslangversion, 3000, false, false, false);
                // Set Last Version Load State: Reload
                Set_Cookie('stic_lastversionloadstate', "Reload", 3000, false, false, false);
                SUGAR.ajaxUI.stic_lastversionloadstate = "Reload";

                // Reload page
                window.location.reload(true);
            } else {
                //Check Last Version Load State
                SUGAR.ajaxUI.stic_lastversionloadstate = Get_Cookie('stic_lastversionloadstate');
                if (SUGAR.ajaxUI.stic_lastversionloadstate != "Loaded") {
                    // Check all loaded resource versions (not only scripts or css)
                    var resources = window.performance.getEntriesByType("resource");
                    var allResourcesLastVersion = true;
                    for (var i = 0; i < resources.length && allResourcesLastVersion; i++) {
                        if(resources[i].name && resources[i].name.includes("?v=")) {
                            allResourcesLastVersion &= (resources[i].name.includes("?v=" + cookie_cssJsCustomVersion) || 
                                                        resources[i].name.includes("?v=" + cookie_cssjslangversion));
                        }
                    }
                    if (allResourcesLastVersion) {
                        // Update Last Version Load Stated: Loaded
                        Set_Cookie('stic_lastversionloadstate', "Loaded", 3000, false, false, false);
                        SUGAR.ajaxUI.stic_lastversionloadstate = "Loaded";
                    } else {
                        // Update Last Version Load State: Reload
                        Set_Cookie('stic_lastversionloadstate', "Reload", 3000, false, false, false);
                        SUGAR.ajaxUI.stic_lastversionloadstate = "Reload";

                        window.location.reload(true);
                    }
                }
            }
        }
        // End STIC CUSTOM

        // STIC-Custom 20240911 MHP - Checking the status of the time registration button in calls with Ajax UI
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/385
        if (!window.location.href.includes('action=Login') && !window.location.href.includes('module=Users')) {
            checkTimeTrackerButtonStatus();
        }
        // END STIC-Custom

        var cont;
        if (typeof window.onbeforeunload == "function")
            window.onbeforeunload = null;
        scroll(0,0);
        try{
            var r = YAHOO.lang.JSON.parse(o.responseText);
            cont = r.content;

            if (r.title)
            {
                document.title = html_entity_decode(r.title);
            }
            if (r.action)
            {
                action_sugar_grp1 = r.action;
            }
            if (r.menu.module)
            {
                module_sugar_grp1 = r.menu.module;
            }
            if (r.favicon)
            {
                SUGAR.ajaxUI.setFavicon(r.favicon);
            }

            var c = document.getElementById("content");
            // Bug #49205 : Subpanels fail to load when selecting subpanel tab
            // hide content of placeholder before apply new one
            // @see SUGAR.util.evalScript
            c.style.visibility = 'hidden';
            c.innerHTML = cont;
            SUGAR.util.evalScript(cont);
            // all javascripts have been processed - show content of placeholder
            c.style.visibility = 'visible';

            if ( r.moduleList)
            {
                SUGAR.themes.setModuleTabs(r.moduleList);
            }


            // set response time from ajax response
            if(typeof(r.responseTime) != 'undefined'){
                var rt = $("#responseTime");
                if(rt.length > 0){
                    rt.html(rt.html().replace(/[\d]+\.[\d]+/, r.responseTime));
                }
                else if(typeof(logoStats) != "undefined"){
                	$("#logo").attr("title", logoStats.replace(/[\d]+\.[\d]+/, r.responseTime)).tipTip({maxWidth: "auto", edgeOffset: 10});
                }
            }
            // Bug #49205 : Subpanels fail to load when selecting subpanel tab
            // hide ajax loading message after all scripts are processed
            SUGAR.ajaxUI.hideLoadingPanel();
        } catch (e){
            // Bug #49205 : Subpanels fail to load when selecting subpanel tab
            // hide ajax loading message after all scripts are processed
            SUGAR.ajaxUI.hideLoadingPanel();
            SUGAR.ajaxUI.showErrorMessage(o.responseText);
        }
        SUGAR_callsInProgress--;
    },
    showErrorMessage : function(errorMessage)
    {
        if (!SUGAR.ajaxUI.errorPanel) {
                SUGAR.ajaxUI.errorPanel = new YAHOO.widget.Panel("ajaxUIErrorPanel", {
                    modal: false,
                    visible: true,
                    width	: "800px",
                    height : "600px",
                    close: true
                });
            }
            var panel = SUGAR.ajaxUI.errorPanel;
            panel.setHeader(SUGAR.language.get('app_strings','ERR_AJAX_LOAD')) ;
            panel.setBody('<iframe id="ajaxErrorFrame" style="width:780px;height:550px;border:none;marginheight="0" marginwidth="0" frameborder="0""></iframe>');
            panel.setFooter(SUGAR.language.get('app_strings','ERR_AJAX_LOAD_FOOTER')) ;
            panel.render(document.body);
            SUGAR.util.doWhen(
				function(){
					var f = document.getElementById("ajaxErrorFrame");
					return f != null && f.contentWindow != null && f.contentWindow.document != null;
				}, function(){
					document.getElementById("ajaxErrorFrame").contentWindow.document.body.innerHTML = errorMessage;
					window.setTimeout('throw "AjaxUI error parsing response"', 300);
			});

            //fire off a delayed check to make sure error message was rendered.
            SUGAR.ajaxUI.errorMessage = errorMessage;
            window.setTimeout('if((typeof(document.getElementById("ajaxErrorFrame")) == "undefined" || typeof(document.getElementById("ajaxErrorFrame")) == null  || document.getElementById("ajaxErrorFrame").contentWindow.document.body.innerHTML == "")){document.getElementById("ajaxErrorFrame").contentWindow.document.body.innerHTML=SUGAR.ajaxUI.errorMessage;}',3000);

            panel.show();
            panel.center();

            throw "AjaxUI error parsing response";
    },
    canAjaxLoadModule : function(module)
    {
        var checkLS = /&LicState=check/.exec(window.location.search);

        // Return false if ajax ui is completely disabled, or if license state is set to check
        if( checkLS || (typeof(SUGAR.config.disableAjaxUI) != 'undefined' && SUGAR.config.disableAjaxUI == true)){
            return false;
        }

        // STIC Custom 20230510 JBL - Reload js and css without cleaning browse cache
        // STIC#1075

        // Check Last Version Load State
        SUGAR.ajaxUI.stic_lastversionloadstate = Get_Cookie('stic_lastversionloadstate');
        if (SUGAR.ajaxUI.stic_lastversionloadstate == "Reload") {
            // The page is reloaded: Update Last Version Load State and Disable Ajax
            // Set Last Version Load State: AjaxDisabled
            Set_Cookie('stic_lastversionloadstate', "AjaxDisabled", 3000, false, false, false);
            SUGAR.ajaxUI.stic_lastversionloadstate = "AjaxDisabled";
            return false;
        }
        if (SUGAR.ajaxUI.stic_lastversionloadstate == "AjaxDisabled") {
            // Ajax was disabled: Enable again
            // Set Last Version Load State: AjaxEnabled
            Set_Cookie('stic_lastversionloadstate', "AjaxEnabled", 3000, false, false, false);
            SUGAR.ajaxUI.stic_lastversionloadstate = "AjaxEnabled";
        }
        // End STIC CUSTOM
        
        var bannedModules = SUGAR.config.stockAjaxBannedModules;
        //If banned modules isn't there, we are probably on a page that isn't ajaxUI compatible
        if (typeof(bannedModules) == 'undefined')
            return false;
        // Mechanism to allow for overriding or adding to this list
        if(typeof(SUGAR.config.addAjaxBannedModules) != 'undefined'){
            bannedModules.concat(SUGAR.config.addAjaxBannedModules);
        }
        if(typeof(SUGAR.config.overrideAjaxBannedModules) != 'undefined'){
            bannedModules = SUGAR.config.overrideAjaxBannedModules;
        }

        return SUGAR.util.arrayIndexOf(bannedModules, module) == -1;
    },

    loadContent : function(url, params)
    {
        if(YAHOO.lang.trim(url) != "")
        {

          // ajaxUILoc XSS protection:
          // window.location = url; is vulnerable to XSS attack
          // Check for valid url
          // Expects url encoded versions of index.php?module=Home&action=index&parentTab=All
          var testUrl = decodeURIComponent(url);
          if (
            /^index.php?(([A-Z]|[a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+[%=+#&\[\]\.]*)+/i.test(testUrl) === false
          ) {
            throw "Invalid url";
          }

          //Don't ajax load certain modules
            var mRegex = /module=([^&]*)/.exec(url);
            var module = mRegex ? mRegex[1] : false;
            if (module && SUGAR.ajaxUI.canAjaxLoadModule(module))
            {
                // STIC Custom 20230510 JBL - Reload js and css without cleaning browse cache
                // STIC#1075
                if (url.startsWith("index.php?") && !url.startsWith("index.php?v=")) {
                    if (SUGAR.ajaxUI.stic_lastversionloadstate == "Loaded" && SUGAR.ajaxUI.stic_cssjscustomversion != "") {
                        // Use current version of index.php with last references
                        url = url.replace("index.php?", "index.php?v=" + SUGAR.ajaxUI.stic_cssjscustomversion + "&");
                    } else { 
                        // Force reload index.php from server to update references
                        url = url.replace("index.php?", "index.php?v=" + Math.floor(Math.random() * 10000) + "&");
                    }
                }
                // End STIC CUSTOM
                YAHOO.util.History.navigate('ajaxUILoc',  url);
            } else {
              window.location = url;
            }
        }
    },

    go : function(url)
    {

        if(YAHOO.lang.trim(url) != "")
        {
            var con = YAHOO.util.Connect, ui = SUGAR.ajaxUI;
            if (ui.lastURL == url) {
              return;
            }

          // ajaxUILoc XSS protection:
          // window.location = url; is vulnerable to XSS attack
          // Check for valid url
          // Expects url encoded versions of index.php?module=Home&action=index&parentTab=All
          var testUrl = decodeURIComponent(url);
          if (
            /^index.php?(([A-Z]|[a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+[%=+#&\[\]\.]*)+/i.test(testUrl) === false
          ) {
            throw "Invalid url";
          }

            var inAjaxUI = /action=ajaxui/.exec(window.location);
            if (typeof (window.onbeforeunload) == "function" && window.onbeforeunload())
            {
                //If there is an unload function, we need to check it ourselves
                if (!confirm(window.onbeforeunload()))
                {
                    if (!inAjaxUI)
                    {
                        //User doesn't want to navigate
                        window.location.hash = "";
                    }
                    else
                    {
                        YAHOO.util.History.navigate('ajaxUILoc',  ui.lastURL);
                    }
                    return;
                }
                window.onbeforeunload = null;
            }
            if (ui.lastCall && con.isCallInProgress(ui.lastCall)) {
                con.abort(ui.lastCall);
            }
            var mRegex = /module=([^&]*)/.exec(url);
            var module = mRegex ? mRegex[1] : false;
            //If we can't ajax load the module (blacklisted), set the URL directly.
            if (!ui.canAjaxLoadModule(module)) {
              window.location = url;
            }
            ui.lastURL = url;
            ui.cleanGlobals();
            var loadLanguageJS = '';
            if(module && typeof(SUGAR.language.languages[module]) == 'undefined'){
                loadLanguageJS = '&loadLanguageJS=1';
            }

            if (!inAjaxUI) {
                //If we aren't in the ajaxUI yet, we need to reload the page to get setup properly
                if (!SUGAR.isIE)
                  window.location.replace("index.php?action=ajaxui#ajaxUILoc=" + encodeURIComponent(url));
                else {
                  //if we use replace under IE, it will cache the page as the replaced version and thus no longer load the previous page.
                  window.location.hash = "#";
                  window.location.assign("index.php?action=ajaxui#ajaxUILoc=" + encodeURIComponent(url));
                }
            }
            else {
                SUGAR_callsInProgress++;
                SUGAR.ajaxUI.showLoadingPanel();
                ui.lastCall = YAHOO.util.Connect.asyncRequest('GET', url + '&ajax_load=1' + loadLanguageJS, {
                    success: SUGAR.ajaxUI.callback,
                    failure: function(){
                        SUGAR_callsInProgress--;
                        SUGAR.ajaxUI.hideLoadingPanel();
                        SUGAR.ajaxUI.showErrorMessage(SUGAR.language.get('app_strings','ERR_AJAX_LOAD_FAILURE'));
                    }
                });
            }
        }
    },

    submitForm : function(formname, params)
    {
        var con = YAHOO.util.Connect, SA = SUGAR.ajaxUI;
        if (SA.lastCall && con.isCallInProgress(SA.lastCall)) {
            con.abort(SA.lastCall);
        }
        //Reset the EmailAddressWidget before loading a new page
        SA.cleanGlobals();
        //Don't ajax load certain modules
        var form = YAHOO.util.Dom.get(formname) || document.forms[formname];
        if (SA.canAjaxLoadModule(form.module.value)
            //Do not try to submit a form that contains a file input via ajax.
            && typeof(YAHOO.util.Selector.query("input[type=file]", form)[0]) == "undefined"
            //Do not try to ajax submit a form if the ajaxUI is not initialized
            && /action=ajaxui/.exec(window.location))
        {
            var string = con.setForm(form);
            var baseUrl = "index.php?action=ajaxui#ajaxUILoc=";
            SA.lastURL = "";
            //Use POST for long forms and GET for short forms (GET allow resubmit via reload)
            if(string.length > 200)
            {
                SUGAR.ajaxUI.showLoadingPanel();
                form.onsubmit = function(){ return true; };
                form.submit();
            } else {
                con.resetFormState();
                window.location = baseUrl + encodeURIComponent("index.php?" + string);
            }
            return true;
        } else {

            if( typeof(YAHOO.util.Selector.query("input[type=submit]", form)[0]) != "undefined"
                    && YAHOO.util.Selector.query("input[type=submit]", form)[0].value == "Save")
            {
                ajaxStatus.showStatus(SUGAR.language.get('app_strings', 'LBL_SAVING'));
            }

            form.submit();
            return false;
        }
    },

    cleanGlobals : function()
    {
        sqs_objects = {};
        QSProcessedFieldsArray = {};
        collection = {};
        //Reset the EmailAddressWidget before loading a new page
        if (SUGAR.EmailAddressWidget){
            SUGAR.EmailAddressWidget.instances = {};
            SUGAR.EmailAddressWidget.count = {};
        }
        YAHOO.util.Event.removeListener(window, 'resize');
        //Hide any connector dialogs
        if(typeof(dialog) != 'undefined' && typeof(dialog.destroy) == 'function'){
            dialog.destroy();
            delete dialog;
        }

    },
    firstLoad : function()
    {
        //Setup Browser History
        var url = YAHOO.util.History.getBookmarkedState('ajaxUILoc');
        var aRegex = /action=([^&#]*)/.exec(window.location);
        var action = aRegex ? aRegex[1] : false;
        var mRegex = /module=([^&#]*)/.exec(window.location);
        var module = mRegex ? mRegex[1] : false;
        if (module != "ModuleBuilder")
        {
            var go = url != null || action == "ajaxui";
            url = url ? url : 'index.php?module=Home&action=index';
            YAHOO.util.History.register('ajaxUILoc', url, SUGAR.ajaxUI.go);
            YAHOO.util.History.initialize("ajaxUI-history-field", "ajaxUI-history-iframe");
            SUGAR.ajaxUI.hist_loaded = true;
            if (go)
                SUGAR.ajaxUI.go(url);
        }
        SUGAR_callsInProgress--;
    },
    print: function()
    {
        var url = YAHOO.util.History.getBookmarkedState('ajaxUILoc');
        SUGAR.util.openWindow(
            url + '&print=true',
            'printwin',
            'menubar=1,status=0,resizable=1,scrollbars=1,toolbar=0,location=1'
        );
    },
    showLoadingPanel: function()
    {
        if (!SUGAR.ajaxUI.loadingPanel)
        {
            SUGAR.ajaxUI.loadingPanel = new YAHOO.widget.Panel("ajaxloading",
            {
                width:"240px",
                fixedcenter:true,
                close:false,
                draggable:false,
                constraintoviewport:false,
                modal:true,
                visible:false
            });
            SUGAR.ajaxUI.loadingPanel.setBody('<div id="loadingPage" align="center" style="vertical-align:middle;"><img src="' + SUGAR.themes.loading_image + '" align="absmiddle" /> <b>' + SUGAR.language.get('app_strings', 'LBL_LOADING_PAGE') +'</b></div>');
            SUGAR.ajaxUI.loadingPanel.render(document.body);
        }

        if (document.getElementById('ajaxloading_c'))
            document.getElementById('ajaxloading_c').style.display = '';
        
        SUGAR.ajaxUI.loadingPanel.show();

    },
    hideLoadingPanel: function()
    {
        SUGAR.ajaxUI.loadingPanel.hide();
        
        if (document.getElementById('ajaxloading_c'))
            document.getElementById('ajaxloading_c').style.display = 'none';
    },
    setFavicon: function(data)
    {
        var head = document.getElementsByTagName("head")[0];

        // first remove all rel="icon" links as long as updating an existing one
        // could take no effect
        var links = head.getElementsByTagName("link");
        var re = /\bicon\b/i;
        for (var i = 0; i < links.length; i++)        {
            if (re.test(links[i].rel))
            {
                head.removeChild(links[i]);
            }
        }

        var link = document.createElement("link");

        link.href = data.url;
        // type attribute is important for Google Chrome browser
        link.type = data.type;
        link.rel = "icon";
        head.appendChild(link);
    }
};
