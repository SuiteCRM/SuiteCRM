(function(){
var JSON = YAHOO.lang.JSON;

SUGAR.quickCompose = {};

SUGAR.quickCompose = function() {
	return {

		parentPanel : null,
		dceMenuPanel : null,
		options: null,
		loadingMessgPanl : null,
		frameLoaded : false,
		resourcesLoaded: false,
		tinyLoaded : false,

		/**
		 * Get the required compose package in an ajax call required for
		 * the quick compose.
		 * @method initComposePackage
		 * @param {Array} c Options containing compose package and full return url.
		 * @return {} none
		 **/
		initComposePackage: function(c)
		{
		    //Init fix for YUI 2.7.0 datatable sort.
	        SUGAR.email2.addressBook.initFixForDatatableSort();

		    //JS resources must have been loaded if we reach this step.
		    SUGAR.quickCompose.resourcesLoaded = true;
            var callback =
	        {
	           success: function(o)
	           {
	               var responseData = JSON.parse(o.responseText);
	               //Create and insert the necessary script tag
	               var scriptTag = document.createElement('script');
	               scriptTag.id = 'quickComposeScript';
                   scriptTag.setAttribute('type','text/javascript');

                   if(YAHOO.env.ua.ie > 0) //IE hack
                   		scriptTag.text = responseData.jsData;
                   else  //Everybody else
                   		scriptTag.appendChild(document.createTextNode(responseData.jsData));

                   document.getElementsByTagName("head")[0].appendChild(scriptTag);

                   //Create and insert the necessary div elements and html markup
	               var divTag = document.createElement("div");
	               divTag.innerHTML = responseData.divData;
	               divTag.id = 'quickCompose';
	               YAHOO.util.Dom.insertBefore(divTag, document.getElementsByTagName('footer')[0]);

	               //Set the flag that we loaded the compose package.
	               SUGAR.quickCompose.frameLoaded = true;
	               //Init the UI
                   SUGAR.quickCompose.initUI(c.data);
	           }
	       }

	       if(!SUGAR.quickCompose.frameLoaded)
		      YAHOO.util.Connect.asyncRequest('GET', 'index.php?entryPoint=GenerateQuickComposeFrame', callback, null);
		   else
		      SUGAR.quickCompose.initUI(c.data);

		},
		/**
		 * Initalize the UI for the quick compose
		 * the quick compose.
		 * @method initComposePackage
		 * @param {Array} options Options containing compose package and full return url.
		 * @return {} none
		 **/
		initUI: function(options)
		{
			var SQ = SUGAR.quickCompose;
			this.options = options;

			//Hide the loading div
			loadingMessgPanl.hide();

    		var dce_mode = (typeof this.dceMenuPanel != 'undefined' && this.dceMenuPanel != null) ? true : false;

			//Destroy the previous quick compose panel to get a clean slate
    		if (SQ.parentPanel != null)
    		{
    			//First clean up the tinyMCE instance
    			tinymce.remove('#' + SUGAR.email2.tinyInstances.currentHtmleditor);
    			SUGAR.email2.tinyInstances[SUGAR.email2.tinyInstances.currentHtmleditor] = null;
    			SUGAR.email2.tinyInstances.currentHtmleditor = "";
    			SQ.parentPanel.destroy();
    			SQ.parentPanel = null;
    		}

			var theme = SUGAR.themes.theme_name;

			//The quick compose utalizes the EmailUI compose functionality which allows for multiple compose
			//tabs.  Quick compose always has only one compose screen with an index of 0.
			var idx = 0;

		    //Get template engine with template
    		if (!SE.composeLayout.composeTemplate)
    			SE.composeLayout.composeTemplate = new YAHOO.SUGAR.Template(SE.templates['compose']);

    		var panel_modal = dce_mode ? false : true,
    		    panel_width = '880px',
			    panel_constrain = dce_mode ? false : true,
    		    panel_height = dce_mode ? 'auto' : '600px',
    		    panel_shadow = dce_mode ? false : true,
    		    panel_draggable = dce_mode ? false : true,
    		    panel_resize = dce_mode ? false : true,
    		    panel_close = dce_mode ? false : true;

        	SQ.parentPanel = new YAHOO.widget.Panel("container1", {
                modal: panel_modal,
				visible: true,
            	constraintoviewport: panel_constrain,
                width	: panel_width,
                height : panel_height,
                shadow	: panel_shadow,
                draggable : panel_draggable,
				resize: panel_resize,
				close: panel_close
            });

        	if(!dce_mode) {
        		SQ.parentPanel.setHeader( SUGAR.language.get('app_strings','LBL_EMAIL_QUICK_COMPOSE')) ;
        	}

            SQ.parentPanel.setBody("<div class='email'><div id='htmleditordiv" + idx + "'></div></div>");

			var composePanel = SE.composeLayout.getQuickComposeLayout(SQ.parentPanel,this.options);

			if(!dce_mode) {
				var resize = new YAHOO.util.Resize('container1', {
                    handles: ['br'],
                    autoRatio: false,
                    minWidth: 400,
                    minHeight: 550,
                    status: false
                });

                resize.on('resize', function(args) {
                    var panelHeight = args.height;
                    this.cfg.setProperty("height", panelHeight + "px");
					var layout = SE.composeLayout[SE.composeLayout.currentInstanceId];
					layout.set("height", panelHeight - 50);
					layout.resize(true);
					SE.composeLayout.resizeEditor(SE.composeLayout.currentInstanceId);
                }, SQ.parentPanel, true);
			} else {
                SUGAR.util.doWhen("typeof SE.composeLayout[SE.composeLayout.currentInstanceId] != 'undefined'", function(){
                    var panelHeight = 600;
                    SQ.parentPanel.cfg.setProperty("height", panelHeight + "px");
                    var layout = SE.composeLayout[SE.composeLayout.currentInstanceId];
                    layout.set("height", panelHeight);
                    layout.resize(true);
                    SE.composeLayout.resizeEditor(SE.composeLayout.currentInstanceId);
                });
            }

			YAHOO.util.Dom.setStyle("container1", "z-index", 1);

			if (!SQ.tinyLoaded)
			{
				//TinyMCE bug, since we are loading the js file dynamically we need to let tiny know that the
				//dom event has fired.
				tinymce.dom.Event.domLoaded = true;

				tinymce.init({
			 		 convert_urls : false,
			         theme_advanced_toolbar_align : tinyConfig.theme_advanced_toolbar_align,
                     valid_children : tinyConfig.valid_children,
			         width: tinyConfig.width,
			         theme: tinyConfig.theme,
			         theme_advanced_toolbar_location : tinyConfig.theme_advanced_toolbar_location,
			         theme_advanced_buttons1 : tinyConfig.theme_advanced_buttons1,
			         theme_advanced_buttons2 : tinyConfig.theme_advanced_buttons2,
			         theme_advanced_buttons3 : tinyConfig.theme_advanced_buttons3,
			         plugins : tinyConfig.plugins,
			         elements : tinyConfig.elements,
			         language : tinyConfig.language,
			         extended_valid_elements : tinyConfig.extended_valid_elements,
			         mode: tinyConfig.mode,
			         strict_loading_mode : true,
			         gecko_spellcheck : tinyConfig.gecko_spellcheck,
			         license_key : tinyConfig.license_key
		    	 });
				SQ.tinyLoaded = true;
			}

			SQ.parentPanel.show();

			//Re-declare the close function to handle appropriattely.
			SUGAR.email2.composeLayout.forceCloseCompose = function(o){SUGAR.quickCompose.parentPanel.hide(); }



			if(!dce_mode) {
				SQ.parentPanel.center();
			}
		},
		/**
		 * Display a loading pannel and start retrieving the quick compose requirements.
		 * @method init
		 * @param {Array} o Options containing compose package and full return url.
		 * @return {} none
		 **/
		init: function(o) {

			  if(typeof o.menu_id != 'undefined') {
			     this.dceMenuPanel = o.menu_id;
			  } else {
			     this.dceMenuPanel = null;
			  }

              loadingMessgPanl = new YAHOO.widget.SimpleDialog('loading', {
        			width: '200px',
        			close: true,
        			modal: true,
        			visible:  true,
        			fixedcenter: true,
        	        constraintoviewport: true,
        	        draggable: false
		      });

              loadingMessgPanl.setHeader(SUGAR.language.get('app_strings','LBL_EMAIL_PERFORMING_TASK'));
		      loadingMessgPanl.setBody(SUGAR.language.get('app_strings','LBL_EMAIL_ONE_MOMENT'));
		      loadingMessgPanl.render(document.body);
		      loadingMessgPanl.show();

		      //If JS files havn't been loaded, perform the load.
		      if(! SUGAR.quickCompose.resourcesLoaded )
		          this.loadResources(o);
		      else
		          this.initUI(o);
		},
		/**
		 * Pull in all the required js files.
		 * @method loadResources
		 * @param {Array} o Options containing compose package and full return url.
		 * @return {} none
		 **/
		loadResources: function(o)
		{
			//IE Bug fix for TinyMCE when pulling in the js file dynamically.
		   	window.skipTinyMCEInitPhase = true;
		    var require = ["layout", "element", "tabview", "menu","cookie","tinymce","sugarwidgets","sugarquickcompose","sugarquickcomposecss"];
			var loader = new YAHOO.util.YUILoader({
				    require : require,
				    loadOptional: true,
				    skin: { base: 'blank', defaultSkin: '' },
				    data: o,
				    onSuccess: this.initComposePackage,
				    allowRollup: true,
				    base: "include/javascript/yui/build/"
				});

				//TiinyMCE cannot be added into the sugar_grp_quickcomp file as it breaks the build, needs to be loaded
				//separately.
				loader.addModule({
				    name :"tinymce",
				    type : "js",
				    varName: "TinyMCE",
				    fullpath: "vendor/tinymce/tinymce/tinymce.min.js"
				});

				//Load the Sugar widgets with dependancies on the yui library.
				loader.addModule({
				    name :"sugarwidgets",
				    type : "js",
				    fullpath: "include/javascript/sugarwidgets/SugarYUIWidgets.js",
				    varName: "YAHOO.SUGAR",
				    requires: ["datatable", "dragdrop", "treeview", "tabview"]
				});

				//Load the main components for the quick create compose screen.
				loader.addModule({
				    name :"sugarquickcompose",
				    type : "js",
				    varName: "SUGAR.email2.complexLayout",
				    requires: ["layout", "sugarwidgets", "tinymce"],
				    fullpath: "cache/include/javascript/sugar_grp_quickcomp.js"
				});

				//Load the css needed for the quickCompose.
				loader.addModule({
				    name :"sugarquickcomposecss",
				    type : "css",
				    fullpath: "modules/Emails/EmailUI.css"
				});

				loader.insert();
	    }
	};
}();
})();
