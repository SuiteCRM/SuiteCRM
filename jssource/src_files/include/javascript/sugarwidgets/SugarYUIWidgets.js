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
 */


YAHOO.namespace("SUGAR");
(function() {
	var sw = YAHOO.SUGAR,
		Event = YAHOO.util.Event,
		Connect = YAHOO.util.Connect,
	    Dom = YAHOO.util.Dom;

/**
 * Message Box is a singleton widget designed to replace the browsers 'alert'
 * function, as well as provide capabilities for pop-over loading bars and
 * other small non-interactive pop-overs.
 * TODO:Still needs configurable buttons in the footer as well as
 * auto building of a loading bar.
 */
sw.MessageBox = {
	progressTemplate : "{body}<br><div class='sugar-progress-wrap'><div class='sugar-progress-bar'/></div>",
	promptTemplate : "{body}:<input id='sugar-message-prompt' class='sugar-message-prompt' name='sugar-message-prompt'></input>",
	show: function(config) {
		var myConf = sw.MessageBox.config = {
			type:'message',
			modal:true,
			width: 240,
			id:'sugarMsgWindow',
			close:true,
			title:"Alert",
			msg: " ",
			buttons: [ ]
		};

		if (config['type'] && config['type'] == "prompt") {
			myConf['buttons'] = [{
				text: SUGAR.language.get("app_strings", "LBL_EMAIL_CANCEL"), handler:YAHOO.SUGAR.MessageBox.hide
			},{
				text: SUGAR.language.get("app_strings", "LBL_EMAIL_OK"), handler:config['fn'] ?
								function(){
									var returnValue = config['fn'](YAHOO.util.Dom.get("sugar-message-prompt").value);
									if (typeof(returnValue) == "undefined" || returnValue) {
										YAHOO.SUGAR.MessageBox.hide();
									} // if
								}
								: YAHOO.SUGAR.MessageBox.hide, isDefault:true
			}];
		} else if ((config['type'] && config['type'] == "alert")) {
			myConf['buttons'] = [{
				text: 		SUGAR.language.get("app_strings", "LBL_EMAIL_OK"),
				handler:	config['fn'] ?
								function(){YAHOO.SUGAR.MessageBox.hide(); config['fn']();}
							:
								YAHOO.SUGAR.MessageBox.hide,
				isDefault:true
			}]
		} else if((config['type'] && config['type'] == "confirm")) {
			myConf['buttons'] = [{
				text: SUGAR.language.get("app_strings", "LBL_EMAIL_YES"), handler:config['fn'] ?
								function(){config['fn']('yes');YAHOO.SUGAR.MessageBox.hide();}
								: YAHOO.SUGAR.MessageBox.hide, isDefault:true
			},{
				text: SUGAR.language.get("app_strings", "LBL_EMAIL_NO"), handler:config['fn'] ?
								function(){config['fn']('no');YAHOO.SUGAR.MessageBox.hide();}
								: YAHOO.SUGAR.MessageBox.hide
			}];
		}
		else if((config['type'] && config['type'] == "plain")) {
			myConf['buttons'] = [];
		} // else if

		for (var i in config) {
			myConf[i] = config[i];
		}
		if (sw.MessageBox.panel) {
			sw.MessageBox.panel.destroy();
		}
		sw.MessageBox.panel = new YAHOO.widget.SimpleDialog(myConf.id, {
			width: myConf.width + 'px',
			close: myConf.close,
			modal: myConf.modal,
			visible: false,
			fixedcenter: true,
	        constraintoviewport: true,
	        draggable: true,
	        buttons: myConf.buttons
		});
		if (myConf.type == "progress") {
			sw.MessageBox.panel.setBody(sw.MessageBox.progressTemplate.replace(/\{body\}/gi, myConf.msg));
		} else if  (myConf.type == "prompt") {
			sw.MessageBox.panel.setBody(sw.MessageBox.promptTemplate.replace(/\{body\}/gi, myConf.msg));
		} else if (myConf.type == "confirm") {
			sw.MessageBox.panel.setBody(myConf.msg);
		} else {
			sw.MessageBox.panel.setBody(myConf.msg);
		}
		sw.MessageBox.panel.setHeader(myConf.title);

		if (myConf.beforeShow) {
			sw.MessageBox.panel.beforeShowEvent.subscribe(function() {myConf.beforeShow();});
		} // if
		if (myConf.beforeHide) {
			sw.MessageBox.panel.beforeHideEvent.subscribe(function() {myConf.beforeHide();});
		} // if
		sw.MessageBox.panel.render(document.body);
		sw.MessageBox.panel.show();
	},

	updateProgress: function(percent, message) {
		if (!sw.MessageBox.config.type == "progress") return;

		if (typeof message == "string") {
			sw.MessageBox.panel.setBody(sw.MessageBox.progressTemplate.replace(/\{body\}/gi, message));
		}

		var barEl = Dom.getElementsByClassName("sugar-progress-bar", null, YAHOO.SUGAR.MessageBox.panel.element)[0];
		if (percent > 100)
			percent = 100;
		else if (percent < 0)
			percent = 0;

		barEl.style.width = percent + "%";
	},

	hide: function() {
		if (sw.MessageBox.panel)
			sw.MessageBox.panel.hide();
	}
};

sw.Template = function(content) {
	this._setContent(content);
};

sw.Template.prototype = {
	regex : /\{([\w\.]*)\}/gim,

	append: function (target, args) {
		var tEl = Dom.get(target);
		if (tEl) tEl.innerHTML += this.exec(args);
		else if (typeof(console) != "undefined" && typeof(console.log) == "function")
            console.log("Warning, unable to find target:" + target);
	},
	exec : function (args) {
		var out = this.content;
		for (var i in this.vars) {
			var val = this._getValue(i, args);
			var reg = new RegExp("\\{" + i + "\\}", "g");
			out = out.replace(reg, val);
		}
		return out;
	},

	_setContent : function(content) {
		this.content = content;
		var lastIndex = -1;
		var result =  this.regex.exec(content);
		this.vars = { };
		while(result && result.index > lastIndex){
        	lastIndex = result.index;
        	this.vars[result[1]] = true;
        	result =  this.regex.exec(content);
        }
	},

  _getValue: function(v, scope) {
    return function(e) {
      if(e.indexOf('.') == -1) {
        return this[e];
      }
      var splits = e.split('.');
      var top = this;
      for(var i=0; i<splits.length; i++) {
		top = top[splits[i]];
      }
      return top;
    }.call(scope, v);
  }

};


/**
 * SelectionGrid is simply a YUI Data Table with row selection already enabled.
 */
sw.SelectionGrid = function(containerEl, columns, dataSource, config){
	sw.SelectionGrid.superclass.constructor.call(this, containerEl, columns, dataSource, config);
	// Subscribe to events for row selection
	this.subscribe("rowMouseoverEvent", this.onEventHighlightRow);
	this.subscribe("rowMouseoutEvent", this.onEventUnhighlightRow);
	if (config.forceMulti){
        this.subscribe("rowClickEvent", function(o){
            o.event.preventDefault();
            this.clearTextSelection();
            o.event = SUGAR.util.clone(o.event);
            o.event.ctrlKey = o.event.metaKey = true;
            this.onEventSelectRow(o);
        });
    } else {
        this.subscribe("rowClickEvent", this.onEventSelectRow);
    }

	// Programmatically select the first row
	this.selectRow(this.getTrEl(0));
	// Programmatically bring focus to the instance so arrow selection works immediately
	this.focus();
}

YAHOO.extend(sw.SelectionGrid, YAHOO.widget.ScrollingDataTable, {
	//Bugfix, the default getColumn will fail if a th element is passed in. http://yuilibrary.com/projects/yui2/ticket/2528034
	getColumn : function(column) {
   	    var oColumn = this._oColumnSet.getColumn(column);

   	    if(!oColumn) {
   	        // Validate TD element
   	        var elCell = this.getTdEl(column);
    	    if(elCell && (!column.tagName || column.tagName.toUpperCase() != "TH")) {
   	        	oColumn = this._oColumnSet.getColumn(elCell.cellIndex);
   	        }
   	        // Validate TH element
   	        else {
   	            elCell = this.getThEl(column);

   	            if(elCell) {
   	                // Find by TH el ID
   	                var allColumns = this._oColumnSet.flat;
   	                for(var i=0, len=allColumns.length; i<len; i++) {
   	                    if(allColumns[i].getThEl().id === elCell.id) {
   	                        oColumn = allColumns[i];
   	                    }
   	                }
   	            }
   	        }
   	    }
   	    if(!oColumn) {
   	        YAHOO.log("Could not get Column for column at " + column, "info", this.toString());
   	    }
   	    return oColumn;
   	}
});


/**
 * DragDropTable is a YUI Data Table with support for drag/drop row re-ordering.
 */
sw.DragDropTable = function(containerEl, columns, dataSource, config){
	var DDT = sw.DragDropTable;
	DDT.superclass.constructor.call(this, containerEl, columns, dataSource, config);
	this.DDGroup = config.group ? config.group : "defGroup";
	//Add table to the dragdrop table groups
	if (typeof DDT.groups[this.DDGroup] == "undefined")
		DDT.groups[this.DDGroup] = [];

	DDT.groups[this.DDGroup][DDT.groups[this.DDGroup].length] = this;
	this.tabledd = new YAHOO.util.DDTarget(containerEl);
}
sw.DragDropTable.groups = {
		defGroup: []
}

YAHOO.extend(sw.DragDropTable, YAHOO.widget.ScrollingDataTable, {
	_addTrEl : function (oRecord) {
		var elTr = sw.DragDropTable.superclass._addTrEl.call(this, oRecord);
    var data = oRecord.getData() || {};
    var isDisabled = data.disabled || false
		if (!isDisabled &&
      (
        !this.disableEmptyRows ||
        (
		      oRecord.getData()[this.getColumnSet().keys[0].key] != false &&
          oRecord.getData()[this.getColumnSet().keys[0].key] != ""
        )
		  )
    ) {
		    var _rowDD = new sw.RowDD(this, oRecord, elTr);
		}

    if (isDisabled && elTr.classList) {
      elTr.classList.add('disabled');
    }

    return elTr;
	},
	getGroup : function () {
		return sw.DragDropTable.groups[this.DDGroup];
	}
});

/**
 * subclass of DragDrop to allow rows to be picked up and dropped between other rows.
 */
sw.RowDD = function(oDataTable, oRecord, elTr) {
	if(oDataTable && oRecord && elTr) {
		//sw.RowDD.superclass.constructor.call(this, elTr);
		this.ddtable = oDataTable;
        this.table = oDataTable.getTableEl();
        this.row = oRecord;
        this.rowEl = elTr;
        this.newIndex = null;
        this.init(elTr);
        this.initFrame(); // Needed for DDProxy
        this.invalidHandleTypes = {};
    }
};


YAHOO.extend(sw.RowDD, YAHOO.util.DDProxy, {
//    _removeIdRegex : /(<.[^\/<]*)id\s*=\s*['|"]?\w*['|"]?([^>]*>)/gim,
    _removeIdRegex : new RegExp("(<.[^\\/<]*)id\\s*=\\s*['|\"]?\w*['|\"]?([^>]*>)", "gim"),

	_resizeProxy: function() {
        this.constructor.superclass._resizeProxy.apply(this, arguments);
        var dragEl = this.getDragEl(),
            el = this.getEl();

        Dom.setStyle(this.pointer, 'height', (this.rowEl.offsetHeight + 5) + 'px');
        Dom.setStyle(this.pointer, 'display', 'block');
        var xy = Dom.getXY(el);
        Dom.setXY(this.pointer, [xy[0], (xy[1] - 5)]);

        Dom.setStyle(dragEl, 'height', this.rowEl.offsetHeight + "px");
        Dom.setStyle(dragEl, 'width', (parseInt(Dom.getStyle(dragEl, 'width'),10) + 4) + 'px');
        Dom.setXY(this.dragEl, xy);
    },

    startDrag: function(x, y) {
	    var dragEl = this.getDragEl();
        var clickEl = this.getEl();
        Dom.setStyle(clickEl, "opacity", "0.25");
        var tableWrap = false;
		if (clickEl.tagName.toUpperCase() == "TR")
            tableWrap = true;
		dragEl.innerHTML = "<table>" + clickEl.innerHTML.replace(this._removeIdRegex, "$1$2") + "</table>";
    	//Dom.setStyle(dragEl, "color", Dom.getStyle(clickEl, "color"));
        Dom.addClass(dragEl, "yui-dt-liner");
        Dom.setStyle(dragEl, "height", (clickEl.clientHeight - 2) + "px");
        Dom.setStyle(dragEl, "backgroundColor", Dom.getStyle(clickEl, "backgroundColor"));
  	    Dom.setStyle(dragEl, "border", "2px solid gray");
		Dom.setStyle(dragEl, "display", "");

  	    this.newTable = this.ddtable;
    },

	 clickValidator: function(e) {
    	if (this.row.getData()[0] == " ")
    		return false;
        var target = Event.getTarget(e);
    	return ( this.isValidHandleChild(target) &&
    			(this.id == this.handleElId || this.DDM.handleWasClicked(target, this.id)) );
    },
    /**
     * This function checks that the target of the drag is a table row in this
     * DDGroup and simply moves the sourceEL to that location as a preview.
     */
    onDragOver: function(ev, id) {
    	var groupTables = this.ddtable.getGroup();
    	for(i in groupTables) {
    		var targetTable = groupTables[i];
    		if (!targetTable.getContainerEl)
    			continue;
    		//We got a table id
    		if (targetTable.getContainerEl().id == id) {
    			if (targetTable != this.newTable) { //  Moved from one table to another
    				this.newIndex = targetTable.getRecordSet().getLength() - 1;
    				var destEl = Dom.get(targetTable.getLastTrEl());
    				destEl.parentNode.insertBefore(this.getEl(), destEl);
    			}

    			this.newTable = targetTable
    			return true;
    		}
    	}

    	if (this.newTable && this.newTable.getRecord(id)) {  // Found the target row
    		var targetRow = this.newTable.getRecord(id);
    		var destEl = Dom.get(id);
    		destEl.parentNode.insertBefore(this.getEl(), destEl);
    		this.newIndex = this.newTable.getRecordIndex(targetRow);
		}
    },

    endDrag: function() {
    	//Ensure the element is back on the home table to be cleaned up.
    	if (this.newTable != null && this.newIndex != null) {
    		this.getEl().style.display = "none";
    		this.table.appendChild(this.getEl());
    		this.newTable.addRow(this.row.getData(), this.newIndex);
    		try{
                //This method works, but throws an error under IE8
                this.ddtable.deleteRow(this.row);
            }catch(e){
                if(typeof(console) != "undefined" && console.log)
                {
                    console.log(e);
                }
            }
    		this.ddtable.render();
    	}
    	this.newTable = this.newIndex = null

		var clickEl = this.getEl();
        Dom.setStyle(clickEl, "opacity", "");
    }
});
/**
 * A YUI panel that supports loading and re-loading it's contents from an Asynch request.
 */

sw.AsyncPanel = function (el, params) {
	if (params)
		sw.AsyncPanel.superclass.constructor.call(this, el, params);
	else
		sw.AsyncPanel.superclass.constructor.call(this, el);
}

YAHOO.extend(sw.AsyncPanel, YAHOO.widget.Panel, {
	loadingText : "Loading...",
	failureText : "Error loading content.",

	load : function(url, method, callback, postdata) {
		method = method ? method : "GET";
		this.setBody(this.loadingText);
		if (Connect.url) url = Connect.url + "&" +  url;
		this.callback = callback;
		Connect.asyncRequest(method, url, {success:this._updateContent, failure:this._loadFailed, scope:this}, postdata);
	},

	_updateContent : function (o) {
		//Under safari, the width of the panel may expand for no apparent reason, and under FF it will contract
		var w = this.cfg.config.width.value + "px";
		this.setBody(o.responseText);
		if (!SUGAR.isIE)
			this.body.style.width = w
		if (this.callback != null)
			this.callback(o);
	},

	_loadFailed : function(o) {
		this.setBody(this.failureText);
	}
});

sw.ClosableTab = function(el, parent, conf) {
	this.closeEvent = new YAHOO.util.CustomEvent("close", this);
	if (conf)
		sw.ClosableTab.superclass.constructor.call(this, el, conf);
	else
		sw.ClosableTab.superclass.constructor.call(this, el);

	this.setAttributeConfig("TabView", {
        value: parent
    });
	this.get("labelEl").parentNode.href = "javascript:void(0);";
}

YAHOO.extend(sw.ClosableTab, YAHOO.widget.Tab, {
	close : function () {
		this.closeEvent.fire();
		var parent = this.get("TabView");
		parent.removeTab(this);
	},

	initAttributes: function(attr) {
		sw.ClosableTab.superclass.initAttributes.call(this, attr);

		/**
		 * The message to display when closing the tab
         * @attribute closeMsg
         * @type String
		 */
		this.setAttributeConfig("closeMsg", {
			value: attr.closeMsg || ""
		});

		/**
         * The tab's label text (or innerHTML).
         * @attribute label
         * @type String
         */
        this.setAttributeConfig("label", {
            value: attr.label || this._getLabel(),
            method: function(value) {
                var labelEl = this.get("labelEl");
                if (!labelEl) { // create if needed
                    this.set(LABEL_EL, this._createLabelEl());
                }

                labelEl.innerHTML = value;

                var closeButton = document.createElement('a');
            	closeButton.href = "javascript:void(0);";
            	Dom.addClass(closeButton, "sugar-tab-close");
            	Event.addListener(closeButton, "click", function(e, tab){
	            		if (tab.get("closeMsg") != "")
	            		{
	            			if (confirm(tab.get("closeMsg"))) {
	        					tab.close();
	        				}
	            		}
	            		else {
	        				tab.close();
	        			}
            		},
            		this
            	);
            	labelEl.appendChild(closeButton);
            }
        });
	}
});



/**
 * The sugar Tree is a YUI tree with node construction based on AJAX data built in.
 */
sw.Tree = function (parentEl, baseRequestParams, rootParams) {
	this.baseRequestParams = baseRequestParams;
	sw.Tree.superclass.constructor.call(this, parentEl);
	if (rootParams) {
		if (typeof rootParams == "string")
			this.sendTreeNodeDataRequest(this.getRoot(), rootParams);
		else
			this.sendTreeNodeDataRequest(this.getRoot(), "");
	}
}

YAHOO.extend(sw.Tree, YAHOO.widget.TreeView, {
	sendTreeNodeDataRequest: function(parentNode, params){
		YAHOO.util.Connect.asyncRequest('POST', 'index.php', {
			success: this.handleTreeNodeDataRequest,
			argument: {
				parentNode: parentNode
			},
			scope: this
		}, this.baseRequestParams + params);
	},
	handleTreeNodeDataRequest : function(o) {
		var parentNode = o.argument.parentNode;
		//parent.tree.removeChildren(parentNode);
		var resp = YAHOO.lang.JSON.parse(o.responseText);
		if (resp.tree_data.nodes) {
			for (var i = 0; i < resp.tree_data.nodes.length; i++) {
				var newChild = this.buildTreeNodeRecursive(resp.tree_data.nodes[i], parentNode);
			}
		}
		parentNode.tree.draw();
	},

	buildTreeNodeRecursive : function(nodeData, parentNode) {
		nodeData.label = nodeData.text;
		var node = new YAHOO.widget.TextNode(nodeData, parentNode, nodeData.expanded);
		if (typeof(nodeData.children) == 'object') {
			for (var i = 0; i < nodeData.children.length; i++) {
				this.buildTreeNodeRecursive(nodeData.children[i], node);
			}
		}
		return node;
	}
});


/**
 * A 1/2 second fade-in animation.
 * @class TVSlideIn
 * @constructor
 * @param el {HTMLElement} the element to animate
 * @param callback {function} function to invoke when the animation is finished
 */
YAHOO.widget.TVSlideIn = function(el, callback) {
    /**
     * The element to animate
     * @property el
     * @type HTMLElement
     */
    this.el = el;

    /**
     * the callback to invoke when the animation is complete
     * @property callback
     * @type function
     */
    this.callback = callback;

    this.logger = new YAHOO.widget.LogWriter(this.toString());
};

YAHOO.widget.TVSlideIn.prototype = {
    /**
     * Performs the animation
     * @method animate
     */
    animate: function() {
        var tvanim = this;

        var s = this.el.style;
        s.height = "";
        s.display = "";
        s.overflow = "hidden";

        var th = this.el.clientHeight;
        s.height = "0px";

        var dur = 0.4;
        var a = new YAHOO.util.Anim(this.el, {height: {from: 0, to: th, unit:"px"}}, dur);
        a.onComplete.subscribe( function() { tvanim.onComplete(); } );
        a.animate();
    },

    /**
     * Clean up and invoke callback
     * @method onComplete
     */
    onComplete: function() {
    	this.el.style.overflow = "";
    	this.el.style.height = "";
        this.callback();
    },

    /**
     * toString
     * @method toString
     * @return {string} the string representation of the instance
     */
    toString: function() {
        return "TVSlideIn";
    }
};

/**
 * A 1/2 second fade out animation.
 * @class TVSlideOut
 * @constructor
 * @param el {HTMLElement} the element to animate
 * @param callback {Function} function to invoke when the animation is finished
 */
YAHOO.widget.TVSlideOut = function(el, callback) {
    /**
     * The element to animate
     * @property el
     * @type HTMLElement
     */
    this.el = el;

    /**
     * the callback to invoke when the animation is complete
     * @property callback
     * @type function
     */
    this.callback = callback;

    this.logger = new YAHOO.widget.LogWriter(this.toString());
};

YAHOO.widget.TVSlideOut.prototype = {
    /**
     * Performs the animation
     * @method animate
     */
    animate: function() {
        var tvanim = this;
        var dur = 0.4;
        var th = this.el.clientHeight;
        this.el.style.overflow = "hidden";
        var a = new YAHOO.util.Anim(this.el, {height: {from: th, to: 0, unit:"px"}}, dur);
        a.onComplete.subscribe( function() { tvanim.onComplete(); } );
        a.animate();
    },

    /**
     * Clean up and invoke callback
     * @method onComplete
     */
    onComplete: function() {
        var s = this.el.style;
        s.display = "none";
        this.el.style.overflow = "";
        this.el.style.height = "";
        this.callback();
    },

    /**
     * toString
     * @method toString
     * @return {string} the string representation of the instance
     */
    toString: function() {
        return "TVSlideOut";
    }
};

})();


/**
 * Problem: SODA tests rely on grabbing report field names by IDs. However,
 * these ID's don't always seem to be consistent as they are auto-generated by YUI.
 * Here I have replaced the constructor to set the id of the row element to the modulename
 * concatenated with the fieldname if the report flag has been set, otherwise proceed as normal.
 *
 * Peter D.
 */
(function() {
    var temp = YAHOO.widget.Record.prototype;
    YAHOO.widget.Record = function(oLiteral) {
        this._nCount = YAHOO.widget.Record._nCount;

        YAHOO.widget.Record._nCount++;
        this._oData = {};
        if (YAHOO.lang.isObject(oLiteral)) {
            for (var sKey in oLiteral) {
                if (YAHOO.lang.hasOwnProperty(oLiteral, sKey)) {
                    this._oData[sKey] = oLiteral[sKey];
                }
            }
        }

        if (SUGAR.reports && SUGAR.reports.overrideRecord) {
            this._sId = this._oData.module_name + "_" + this._oData.field_name;
        } else {
            this._sId = Dom.generateId(null, "yui-rec"); //"yui-rec" + this._nCount;
        }
    };
    YAHOO.widget.Record._nCount = 0;
    YAHOO.widget.Record.prototype = temp;
})();
