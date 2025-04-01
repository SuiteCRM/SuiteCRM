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


if(typeof('console') == 'undefined'){
	console = {
		log: function(message) {
	
		}
	}
}
var _debug_ = false;
var _write_ = function( message ){ if(_debug_){ console.log(message);} }



Studio2 = {

	init: function() {
		var Dom = YAHOO.util.Dom,
			DDTarget = YAHOO.util.DDTarget,
			DDM = YAHOO.utilDragDropMgr;
		
		Studio2.maxColumns = parseInt(document.getElementById('maxColumns').value);
		Studio2.setStartId(parseInt(document.getElementById('idCount').value));
		Studio2.setStartId(1000);
		Studio2.fieldwidth = parseInt(document.getElementById('fieldwidth').value);
		Studio2.panelNumber = parseInt(document.getElementById('nextPanelId').value);
		Studio2.isIE = SUGAR.isIE;
		Studio2.expandableFields = [];
        Studio2.scrollZones = {};

		//	Activate the layout panels
		var panels = document.getElementById('panels');
		Dom.setStyle(panels, "visibility", "hidden");
		//new Studio2.PanelDD(panels.id,'studio2.panels'); // add the main layout area into PANELS to allow for panel drags to empty areas
		new DDTarget(panels.id, 'studio2.panels');
		for (var i=0;i<panels.childNodes.length;i++) {
			var panel = panels.childNodes[i];
			if (panel.nodeName == 'DIV') { // a valid panel

				// add the panel into the ROWS drag-and-drop group to allow for drags to empty areas of the panels
				new DDTarget(panel.id,'studio2.rows');

				for (var j=0;j<panel.childNodes.length;j++) {
					var row = panel.childNodes[j];
					if(typeof(row.id)!='undefined' && row.id!=null)	

					if (row.nodeName == 'DIV' && row.className == 'le_row' ) { // a valid row

						// Convert all (empty) fields into variable width fields
						var leftSibling = null;
						var foundEmpty = 0;

						for (var k=0;k<row.childNodes.length;k++) {
							var field = row.childNodes[k];
							var ranOnce = false;
							if (field.nodeName == 'DIV') { // field

								for (var l=0;l<field.childNodes.length;l++) {
									var property = field.childNodes[l];

									if (!(property instanceof SVGElement) && property.className && (property.className.indexOf('field_name') != -1) && property.firstChild) {
										if (property.firstChild.nodeValue.indexOf('(empty)') != -1) {
											//register field to be expandable
											_write_("(empty) found");
											Studio2.setSpecial(field);
											Studio2.registerExpandableField( Studio2.prevField(field) || Studio2.nextField( field ) );
											break;
										}else if (property.firstChild.nodeValue.indexOf('(filler)') != -1){
											var sibling = Studio2.prevField( field ) || Studio2.nextField( field );
											Studio2.setSpecial( field );
											var swapFields = Studio2.nextField( field ) != null;
											Studio2.registerExpandableField( sibling);
											this.toggleFieldWidth (sibling );
											ranOnce = true;
											if( swapFields ){
												//swap (filler) with the sibling
												field = Studio2.nextField (sibling ) || Studio2.prevField( sibling );

												this.swapElements( sibling , field)

											}
											break;
										}

									}
								}
							}
							if( ranOnce ){break;}
						}

						Studio2.activateElement(row);
					}
				}
			}
		}
		Studio2.activateElement(panels);
        Dom.setStyle(panels, "visibility", '');

		// Activate the fields in the availablefields list
		var availablefields = document.getElementById('availablefields');
		Studio2.activateElement(availablefields);

		// Setup Delete area in the toolbox (the Trash icon)
		var d = document.getElementById('delete');
		Studio2.setSpecial(d);
		var delDD = new DDTarget('delete', 'studio2.panels');
		delDD.addToGroup('studio2.rows');
		delDD.addToGroup('studio2.fields');
		
		var fillerProxy = Studio2.newField();
		Studio2.setSpecial(fillerProxy);
		var hanger = document.getElementById('fillerproxy');
		hanger.parentNode.insertBefore(fillerProxy,hanger.nextSibling);
		Studio2.activateElement(fillerProxy);

		rowProxy = Studio2.newRow(true);
		Studio2.setSpecial(rowProxy);
		var hanger = document.getElementById('rowproxy');
		hanger.parentNode.insertBefore(rowProxy,hanger.nextSibling);
		Studio2.activateElement(rowProxy);

		var hanger = document.getElementById('panelproxy');
		if (hanger != null) // if no panelproxy then don't display a new panel option: needed for portal layouts which have only one panel
		{
			panelProxy = Studio2.newPanel();
			Studio2.setSpecial(panelProxy);
			hanger.parentNode.insertBefore(panelProxy,hanger.nextSibling);
			Studio2.activateElement(panelProxy);
		}
		
		Studio2.resizeDivs();

		ModuleBuilder.helpRegisterByID('layoutEditor','div');
		ModuleBuilder.helpRegisterByID('layoutEditorButtons','input');
		ModuleBuilder.helpSetup('layoutEditor','default');

        var elem = document.getElementById('prepareForSave').elements;
        if (elem != null) {
            var has_tab = false;

            for (var i = 0; i < elem.length; i++) {
                if (elem[i].name.match(/^tabDefs_.*_newTab$/)) {
                    if (elem[i].value == '1' && elem[i].name != 'tabDefs_' + Studio2.firstPanelId + '_newTab')
                        has_tab = true;
                }
            }
            if (has_tab == true) {
                document.getElementById('le_paneltype_select_' + Studio2.firstPanelIdCount).disabled = true;
            }
        }
	},
	
	resizeDivs : function () {
		var Dom = YAHOO.util.Dom;
		if (!Dom.get('panels'))
            return;
        var body = document.getElementById('mbtabs');
        var targetHeight =  body.clientHeight - (Dom.getY('toolbox') - Dom.getY(body)) - 32;
		if (Studio2.isIE) targetHeight -= 10;
		Dom.setStyle('panels', "height", targetHeight + "px");
		Dom.setStyle('panels', "width" , ((Studio2.fieldwidth * 2) + 112) + "px");
		Dom.setStyle('toolbox', "height", targetHeight + "px");
        Studio2.scrollZones = {
            panels: Studio2.getScrollZones('panels'),
            toolbox: Studio2.getScrollZones('toolbox')
        }
	},

	/**
	* SIGNATURE
	* 	array = Studio2.expandableFields
	* 	element = id of the element to unregister.
	* RETURN
	* 	element is removed from Studio2.expandableFields if found.
	*/

	unregisterExpandableField:function( field ){
		//_write_("received for unregister: "+field.id);
		if(field==null || typeof(field) == 'undefined'){ return; }
		if ( this.isExpandable(field) ) {
			if (this.getColumnWidth( field ) > 1) { this.reduceFieldWidth( field ); }
			 _write_("Unregistered:"+field.id);
			 field.removeChild( field.childNodes[1] );
			 field.removeAttribute("expandable");
			 field.removeAttribute("state");
		}
	},
	isExpandable:function( field ){
		return field.getAttribute("expandable")!=null && !this.isSpecial(field);//&& field.getAttribute("expandable") == "true";
	},
	swapStates:function( src, dest ){
		var old_src= {state:src.getAttribute("state"), img: src.childNodes[1].src};
		src.setAttribute("state", dest.getAttribute("state"));
		src.childNodes[1].src = dest.childNodes[1].src;
		dest.childNodes[1].src = old_src.img;
		dest.setAttribute("state", old_src.state);
	},

	getImageElement:function(default_toggle){
		var img = document.createElement('img');
		if(!default_toggle)
			img.src = 'index.php?entryPoint=getImage&themeName='+SUGAR.themes.theme_name+'&imageName=minus_inline.gif';
		else
			img.src = 'index.php?entryPoint=getImage&themeName='+SUGAR.themes.theme_name+'&imageName=plus_inline.gif';
		img.className = 'le_edit';
		img.style.paddingRight = 2;
		img.style.cssFloat = 'left';
		img.name = 'expandable_field_icon';
		return img;
	},


	toggleFieldWidth:function(id){

		var field = YAHOO.util.Dom.get(id);
		if ( typeof(field) == 'undefined' || field === null ) return; 
		var img = field.childNodes[1];

		if( field.getAttribute("state") && field.getAttribute("state")=='reduced' ){
			field.parentNode.removeChild( Studio2.nextField(field) || Studio2.prevField(field) );
			Studio2.setColumnWidth(id,2);
			img.src = 'index.php?entryPoint=getImage&themeName='+SUGAR.themes.theme_name+'&imageName=minus_inline.gif';
			this.setExpanded( field );


		}else if( field.getAttribute("state") && field.getAttribute("state")=='expanded' ){
			Studio2.setColumnWidth(id,1);
			var newField = Studio2.newField();
			Studio2.setSpecial(newField);
			Studio2.activateElement(newField);
			field.parentNode.appendChild(newField);
			this.setReduced( field );
			img.src='index.php?entryPoint=getImage&themeName='+SUGAR.themes.theme_name+'&imageName=plus_inline.gif';

		}
	},
	setExpanded: function( field ){
		field.setAttribute("expandable","true");
		field.setAttribute("state","expanded");
		_write_("Expanded: "+field.id);
	},
	setReduced: function( field ){
		field.setAttribute("expandable","true");
		field.setAttribute("state","reduced");
		_write_("Recued: "+field.id);
	},
	isExpanded: function (field ){
		return field.getAttribute('state') == 'expanded';
	},
	isReduced:function (field ){
		return field.getAttribute('state') == 'reduced';
	},
	registerExpandableField: function( field ) {//field = HTML element
		if( Studio2.maxColumns < 2 || field == null || typeof(field) == 'undefined' || this.isSpecial (field) ) { return; }
		if( !this.isExpandable( field ) )  {
			var next = this.nextField ( field ) ;
			var prev = this.prevField ( field ) ;
			var removeMe = next || prev ;
			if( this.isSpecial( next) || this.isSpecial( prev ) || this.isEmpty( next ) || this.isEmpty( prev ) || removeMe == null ){ //Always Expanded
				_write_("remove me is :"+removeMe);
				if (removeMe != null) { field.parentNode.removeChild(removeMe); }
				var img = this.getImageElement ( false );
				img.onclick = function () { Studio2.toggleFieldWidth ( field.id ) };
				field.insertBefore ( img, field.childNodes[1] );
				this.setColumnWidth( field.id, 2 );
				this.setExpanded( field );
				_write_("registered field");
			}
		}else{ _write_("Could not Register field:"+field.id); }
	},
	setStartId: function(id) {
		Studio2.idStack = [id];
	},

	nextId: function() {
		if (Studio2.idStack.length == 1) { // if down to our last id, allocate another
			Studio2.idStack[0]++;
			Studio2.idStack.push(Studio2.idStack[0]);
		}
		return Studio2.idStack.pop();
	},

	setNextId: function(id) {
		Studio2.idStack.push(id);
	},

	isSpecial: function(element) {
		if(element==null || typeof(element) == 'undefined'){return false;};
		return YAHOO.util.Dom.hasClass(element,'special');
	},

	setSpecial: function(el) {
		YAHOO.util.Dom.addClass(el, 'special');
	},

	unsetSpecial: function(el) {
		YAHOO.util.Dom.removeClass(el, 'special');
	},
	isEmpty: function( element ){
		if (element == null || typeof(element) == 'undefined') {return false;};
		return YAHOO.util.Dom.hasClass(element, 'le_field special');
	},
	count: function(element) {
		var count = 0;
		for (var j=0;j<element.childNodes.length;j++) {
			var child = element.childNodes[j];
			if (child.nodeName == 'DIV') { // a valid child
				count++;
			}
		}
		return count;
	},

	newField: function(){ // TODO: use properties to set field contents
		//This object must exists on the page
		var newField = document.createElement('div');
		newField.className ='le_field';
		newField.id = Studio2.nextId();
		newField.innerHTML = '<span>'+SUGAR.language.get('ModuleBuilder', 'LBL_FILLER')+'</span>' + // the displayed label
							 '<span class=\'field_name\'>(filler)</span>'; // the hidden field that identifies this as something to be saved by prepareForSave()
		return newField;
	},

	newRow: function(titleRequired) {
		var newRow = document.createElement('div');
		if (titleRequired) {
			var child = document.createElement('span');
	        child.className = 'panel_name';
	        child.appendChild(document.createTextNode(SUGAR.language.get('ModuleBuilder', 'LBL_NEW_ROW') ) );
	        newRow.appendChild(child);
		}
		newRow.className='le_row';
		newRow.id = Studio2.nextId();
		for(var i=0;i<Studio2.maxColumns;i++) {
			var newField = Studio2.newField();
			Studio2.setSpecial(newField);
			newRow.appendChild(newField);
		}
		return newRow;
	},

	newPanel: function() {
		var newPanel = document.createElement('div');
		newPanel.className='le_panel';
		newPanel.id = Studio2.nextId();
		// get the panelid for this panel - must be unique in the layout, even across saves
		// our dynamically assigned DOM ids won't work as any panel given one of these DOM ids could find itself in conflict with a panel created in a later session
		//var panelIdentifier = 'lbl_panel'+(Studio2.panelNumber ++) ;
		var panelNumber = (Studio2.panelNumber ++) ;
		var view = document.getElementById('prepareForSave').view.value;
		var panelLabel = 'lbl_' + view +  '_panel' + panelNumber;

		var div = document.createElement('div');
		div.id = 'le_panellabel_' + newPanel.id;

        var child = document.createElement('span');
        child.id = 'le_panelname_' + newPanel.id;
        child.className = 'panel_name';
        child.appendChild(document.createTextNode(SUGAR.language.get('ModuleBuilder', 'LBL_NEW_PANEL') ) );
        div.appendChild(child);
        var child = document.createElement('span');
        child.id = 'le_panelid_' + newPanel.id;
        child.className = 'panel_id';
        child.appendChild(document.createTextNode(panelLabel));
		div.appendChild(child);
		newPanel.appendChild(div);

		var img = document.createElement('button');
		img.className = 'le_edit suitepicon suitepicon-action-edit';
		img.style.cursor="pointer;";
		var editModule = document.getElementById('prepareForSave').view_module.value;
		var editString = 'module=ModuleBuilder&action=editProperty&view_module='+editModule+'&view='+view+'&id_label=le_panelname_'+newPanel.id+'&name_label=label_'+panelLabel+'&title_label='+SUGAR.language.get('ModuleBuilder', 'LBL_LABEL_TITLE') ;
		if (document.getElementById('prepareForSave').view_package)
		      editString += '&view_package='+document.getElementById('prepareForSave').view_package.value ;
		var view = document.prepareForSave.view.value;
		img.onclick = function() { var value_label = document.getElementById('le_panelname_'+newPanel.id).innerHTML;ModuleBuilder.asyncRequest( editString + '&value_label=' + value_label, ModuleBuilder.updateContent ); }
		newPanel.appendChild(img);
		return newPanel;
	},

	establishLocation: function(element) {
		var location = null;
		while(element.parentNode != 'body') {
			location = element.id;
			if ((location == 'panels') || (location == 'toolbox') || (location == 'delete')){
				break;
			}
			element = element.parentNode;
		}
		if (location == null) {
			alert(SUGAR.language.get('ModuleBuilder', 'LBL_BADLY_FORMED_DOCUMENT'));
			die();
		}
		return location;
	},

	reclaimIds: function(element) {
		// return the ids in this element to the available pool
		// do not reclaim field IDs as they never really disappear - they just move between toolbox and panel
		if (element.className.indexOf('le_field') == -1) {
			Studio2.setNextId(element.id);
			for (var i=0;i<element.childNodes.length;i++) {
				var child = element.childNodes[i];
				if (child.nodeName == 'DIV') { // a subelement
					Studio2.reclaimIds(child);
				}
			}
		}
	},

	reclaimFields: function(element) {
		if (element.className.indexOf('le_field') != -1) {
			if (! Studio2.isSpecial(element)) {
				var destination = document.getElementById('availablefields');
//				destination.appendChild(element.parentNode.removeChild(element));
				destination.appendChild(element);
				Studio2.resetFieldWidth(element);
			} else {
				element.parentNode.removeChild(element);
			}
		} else {
			for (var i=0;i<element.childNodes.length;i++) {
				var child = element.childNodes[i];
				if (child.nodeName == 'DIV') { // a subelement
					Studio2.reclaimFields(child);
				}
			}
		}
	},

	highlightElement: function(field) {
		YAHOO.util.Dom.setStyle(field,'visibility','hidden');
	},

	/* FIELD WIDTH FUNCTIONS */

	getSpacing: function(field) {
		var Field = new YAHOO.util.Element(field);
		var leftMargin = parseInt(Field.getStyle('margin-left'));
		var rightMargin = parseInt(Field.getStyle('margin-right'));
		var leftPadding = parseInt(Field.getStyle('padding-left'));
		var rightPadding = parseInt(Field.getStyle('padding-right'));
		if (Studio2.isIE) {
			return (leftMargin + rightMargin);
		} else {
			return (leftMargin + rightMargin + leftPadding + rightPadding + 2);
		}
	},

	resetFieldWidth: function(field) {
		YAHOO.util.Dom.setStyle(field,'width',Studio2.fieldwidth + 'px');
		//Dom.setStyle(field,'width',Studio2.fieldwidth + 'px' );
	},

	/* a hack function, purely because Firefox has a problem with field widths during the init function */
	/* so rather than relying on the style values we just set the width directly to the final value */
	adjustWidth: function(field,columns) {
		var newWidth = columns * (Studio2.fieldwidth + Studio2.getSpacing(field));
		YAHOO.util.Dom.setStyle(field,'width',newWidth + 'px' );
	},

	increaseFieldWidth: function(field) {
		var newWidth;
//		var currentWidth = parseInt(field.clientWidth);
		var currentWidth = Studio2.getFieldWidth(field);
		newWidth = currentWidth + Studio2.fieldwidth + Studio2.getSpacing(field);
//		field.style.width = newWidth+'px';
		YAHOO.util.Dom.setStyle(field,'width',newWidth + 'px' );
	},

	reduceFieldWidth: function(field) {
		var newWidth;
		var currentWidth = Studio2.getFieldWidth(field);
		newWidth = currentWidth - Studio2.fieldwidth - Studio2.getSpacing(field);
		YAHOO.util.Dom.setStyle(field,'width',newWidth + 'px' );
	},

	getFieldWidth: function(field) {
		var width = parseInt(YAHOO.util.Dom.getStyle(field, 'width')); // computed style value of the field width (or currentStyle in IE - same result)
		if (isNaN(width)) {
			width = Studio2.fieldwidth; // if field width is set to something like 'auto' we need to take a better guess
		}
		return width;
	},

	setFieldWidth: function(field,width) {
		YAHOO.util.Dom.setStyle(field,'width',width);
	},

	getColumnWidth: function(field) {
		return Math.floor(Studio2.getFieldWidth(field)/Studio2.fieldwidth);
	},

	setColumnWidth: function(field,columns) {
		var spacing = Studio2.getSpacing(field);
		var newWidth = columns * (Studio2.fieldwidth + spacing) - spacing;
		YAHOO.util.Dom.setStyle(field,'width',newWidth + 'px' );
	},

	firstField: function(row) {
		var firstfield = row.firstChild;
		while (firstfield.nodeName != 'DIV') {
			firstfield = firstfield.nextSibling;
		}
		return firstfield;
	},

	getColumn: function(field) {
		var firstfield = Studio2.firstField(field.parentNode);
		return Math.ceil((YAHOO.util.Dom.getX(field) - YAHOO.util.Dom.getX(firstfield) / Studio2.fieldwidth));	
	},

	getRow: function(field) {
		// find our parent row
		// find how many previous siblings we have that are also rows
		// our row is that + 1
		var row = field.parentNode;
		var count = 1;
		while ((row = row.previousSibling) !== null) {
			if (row.nodeName == 'DIV') {
				count++;
			}
		}
		return count;
	},

	prevField: function(field){
		var prev = field.previousSibling;
		while( (null !== prev) && (prev.nodeName != 'DIV')){
			prev = prev.previousSibling;
		}
		return prev;
	},
	nextField: function(field) {
		var next = field.nextSibling;
		while (typeof(next)!='undefined' && (next !== null) && (next.nodeName != 'DIV')) {
			next = next.nextSibling;
		}
		return next;
	},


	/* ELEMENT FUNCTIONS */

	// TODO: rewrite tidyPanels, tidyRows and tidyFields as a recursive tidy
	tidyPanels: function() {
		var panels = document.getElementById('panels');
		if (Studio2.count(panels) <= 0) {
			var newPanel = Studio2.newPanel();
			newPanel.appendChild(Studio2.newRow(false));
			panels.appendChild(newPanel);
			Studio2.activateElement(newPanel);
		}
	},

	tidyRows: function(panel) {
		if (Studio2.count(panel) <= 0) { // no rows left
			if (Studio2.count(panel.parentNode)>1) {
				Studio2.removeElement(panel);
				Studio2.tidyPanels();
			} else {
				// add a blank row back in
				var newRow = Studio2.newRow(false);
				panel.appendChild(newRow);
				Studio2.activateElement(newRow);
//				debugger;
			}
		}
	},

	tidyFields: function(row) {
		if (Studio2.count(row) <= 0) { // no fields left
			var panel = row.parentNode;
			Studio2.removeElement(row);
			Studio2.tidyRows(panel);
		}
	},

	removeElement: function(element) {
		Studio2.reclaimIds(element);
		Studio2.reclaimFields(element);
		if (element.className.indexOf('le_field') == -1) {
			// all fields have been moved to availablefields in Studio2.reclaimFields
			element.parentNode.removeChild(element);
		}
	},

	swapElements: function(el1,el2) {
		// TODO: record this swap in TRANSACTION
		var el1Width = Studio2.getFieldWidth(el1);
		var el2Width = Studio2.getFieldWidth(el2);
		YAHOO.util.DragDropMgr.swapNode(el1, el2);
		Studio2.setFieldWidth(el1,el2Width);
		Studio2.setFieldWidth(el2,el1Width);
	},

	activateElement: function(element) {
		if (!document.getElementById(element.id)) {
			document.body.appendChild(element);
		}
		if (element.className.indexOf('le_panel') != -1) {
			new Studio2.PanelDD(element.id,'studio2.panels');
			new YAHOO.util.DDTarget(element.id,'studio2.rows'); // add so a background for row moves
		}
		if (element.className.indexOf('le_row') != -1) {
			new Studio2.RowDD(element.id,'studio2.rows');
		}
		if (element.className.indexOf('le_field') != -1) {
			new Studio2.FieldDD(element,'studio2.fields');
		}
		for (var i=0;i<element.childNodes.length;i++) {
			var child = element.childNodes[i];
			if (child.nodeName == 'DIV') { // a valid child
				Studio2.activateElement(child);
			}
		}

	},

	/**
	 * A substitute for cloneNode that is Yahoo Drag-and-Drop compatible.
	 * Using document.cloneNode causes Yahoo DnD to fail as the ID is also cloned, leading to duplicate IDs in the document
	 * This substitute doesn't copy the ID
	 */

	copyElement: function(element) {
		var copy = document.createElement(element.tagName);
		if (element.attributes.length > 0) {
			var attrs = element.attributes;
			for(var i=0;i<attrs.length;i++) {
				if (attrs[i].name != 'id') {
					// check to see if this attribute is actually set in the document, or just a default - IE's attributes array contains both, and we only want the specified attributes
					var a = element.getAttributeNode(attrs[i].name);
					if (a && a.specified) {
						if (attrs[i].name == 'class') { // Needed for IE
							copy.className = attrs[i].value;
						}
						copy.setAttribute(attrs[i].name,attrs[i].value);
					}
				}
			}
		}

		Studio2.copyChildren(element, copy);
		copy.id = Studio2.nextId();
		return copy;
	},
	
	copyChildren : function(original, clone)
	{
		var children = original.childNodes
		for (var i in children)
		{
			if (children[i])
			{
				if (children[i].tagName && children[i].tagName.substr(0,1) != "#"){
					clone.appendChild(Studio2.copyElement(children[i]));
				}
				else if (children[i].nodeName && children[i].nodeName == "#text")
				{
					clone.innerHTML += children[i].data;
				}
			}
		}
	},

	setCopy: function(copy) {
		Studio2.copyId = copy.id;
	},

	copy: function() {
		if (Studio2.copyId != null) {
			return document.getElementById(Studio2.copyId);
		} else {
			return false;
		}
	},

	activateCopy:	function() {
		Studio2.activateElement(document.getElementById(Studio2.copyId));
	},

	removeCopy:	 function() {
		if (Studio2.copyId != null) {
			Studio2.removeElement(Studio2.copy());
		}
		Studio2.copyId = null;
	},

	// Copy all the slot content across to a temporary form table for submitting to the backend for the save
	// We could have made all the slots be hidden fields within a form, (ala the original Portal editor), but then we'd have to do a lot of work during
	// editing that really only needs to be done once per save

	prepareForSave: function() {
		// create a new saveForm
		var panels = document.getElementById('panels');
		var saveForm = document.getElementById('prepareForSave');
		if (! saveForm) {
			saveForm = document.createElement('form');
			saveForm.id = 'prepareForSave';
			YAHOO.util.Dom.setStyle(saveForm,"visibility", "hidden");
			panels.parentNode.appendChild(saveForm);
		}
		// remove any existing slot information, but importantly, preserve any non-slot stuff needed for form submittal
		var length = saveForm.childNodes.length;
		var index = 0;
		for( var i=0; i<length; i++) {
			if (saveForm.childNodes[index].nodeName != 'INPUT') {
				index++;
			} else {
				if (saveForm.childNodes[index].getAttribute('name').substr(0,4) == "slot") {
		  			saveForm.removeChild(saveForm.childNodes[index]);
		 	 	} else {
			  		index++;
			  	}
			}
		}

		//	convert to input name='slot-{panel}-{slot}-{type}' value={value}
		var panelid = 0;
		var panels = document.getElementById('panels');

		for( var i=0;i<panels.childNodes.length;i++) {
			var panel = panels.childNodes[i];

			if (panel.nodeName == 'DIV') { // a panel
				panelid++;
				var fieldid = -1;

				for (var j=0;j<panel.childNodes.length;j++) {
					var row = panel.childNodes[j];

					// save the panel name and label
					if (row.id && row.id.indexOf('le_panellabel') != -1) {
						// a panel label
						var inputField = document.createElement('input');
						inputField.setAttribute('type','hidden');
						inputField.setAttribute('name','panel-'+panelid+'-name');
                        inputField.setAttribute('value',document.getElementById('le_panelname_'+row.id.substr(14,row.id.length)).innerHTML);
						saveForm.appendChild(inputField);
						var inputField = document.createElement('input');
						inputField.setAttribute('type','hidden');
                        inputField.setAttribute('name','panel-'+panelid+'-label');
                        inputField.setAttribute('value',document.getElementById('le_panelid_'+row.id.substr(14,row.id.length)).innerHTML);
                        saveForm.appendChild(inputField);
					}

					// now for the rows
					if (row.nodeName == 'DIV') { // a row
						for (var k=0;k<row.childNodes.length;k++) {
							var field = row.childNodes[k];


							if (field.nodeName == 'DIV') { // a field
								fieldid++;
								for ( var l=0; l < field.childNodes.length; l++ ) {
									var property = field.childNodes[l];

									if (property.nodeName == 'SPAN') { // a property of a field
										if (property.attributes.length > 0) {
//										if (property.hasAttributes) {
											var type = property.className;
											if ((type.length>5) && (type.substr(0,5) == 'field') && (property.childNodes.length != 0)) {
												var value = property.firstChild.nodeValue;
												var inputField = document.createElement('input');
												inputField.setAttribute('type','hidden');
												inputField.setAttribute('name','slot-'+panelid+'-'+fieldid+'-'+type.substr(6,type.length));
												inputField.setAttribute('value',value);
												saveForm.appendChild(inputField);
											}
										}
									}
								}
								// check fieldwidth in the layout; if more than one, then add an (empty) for each (so the parser can keep track of the slots)
								var endId = fieldid+Studio2.getColumnWidth(field)-1;
								while (fieldid<endId) {
									fieldid++;
									var inputField = document.createElement('input');
									inputField.setAttribute('type','hidden');
									inputField.setAttribute('name','slot-'+panelid+'-'+fieldid+'-name');
									inputField.setAttribute('value','(empty)');
									saveForm.appendChild(inputField);

								}

							}
						}
					}
				}
			}
		}
	},

	handleSave: function() {
		ajaxStatus.showStatus(SUGAR.language.get('app_strings', 'LBL_SAVING'));
		ModuleBuilder.state.isDirty=false;
		this.prepareForSave();
		// set <input type='hidden' name='action' value='saveLayout'>
		var saveForm = document.forms['prepareForSave'];
		var inputField = document.createElement('input');
		inputField.setAttribute('type','hidden');
		inputField.setAttribute('name','action');
		inputField.setAttribute('value','saveLayout');
		saveForm.appendChild(inputField);
		ModuleBuilder.submitForm('prepareForSave');
		ajaxStatus.flashStatus('Save complete',5000);
	},

	handlePublish: function() {
		ajaxStatus.showStatus(SUGAR.language.get('app_strings', 'LBL_SAVING'));
		ModuleBuilder.state.isDirty=false;
		this.prepareForSave();
		// set <input type='hidden' name='action' value='saveAndPublishLayout'>
		var saveForm = document.forms['prepareForSave'];
		var inputField = document.createElement('input');
		inputField.setAttribute('type','hidden');
		inputField.setAttribute('name','action');
		inputField.setAttribute('value','saveAndPublishLayout');
		saveForm.appendChild(inputField);
		ModuleBuilder.submitForm('prepareForSave');
		ajaxStatus.flashStatus(SUGAR.language.get('ModuleBuilder','LBL_DEPLOYE_COMPLETE'),5000);
	},
	
	checkGridLayout : function(view)
	{
	    if (Studio2.countGridFields() == 0) {
		   ModuleBuilder.layoutValidation.popup() ;
		   return false;
		}
		if (view == "detailview")	
			return true;  
		
	    return Studio2.checkRequiredFields();
	},

	countGridFields : function() {
	    var count = 0;
	    var divs = document.getElementById( 'panels' ).getElementsByTagName( 'div' ) ;
	    for ( var j=0;j<divs.length;j++) {
	        if (divs[j].className == 'le_field')
			    count++;
	    }
	    return count;
	},  

    checkRequiredFields : function(){
		var Dom = YAHOO.util.Dom;
		var availablefields = Dom.get('availablefields');
		var fields = Dom.getElementsByClassName('field_name', '', 'availablefields');
		var missing = [ ];
		for(field in fields){
		    if (Studio2.requiredFields.indexOf(fields[field].innerHTML) != -1) {
				missing[missing.length] = fields[field].innerHTML;
			}
		}
		if (missing.length > 0)	
		{
		    var msg = SUGAR.language.get("ModuleBuilder", "ERROR_REQUIRED_FIELDS");
			for (var i = 0; i < missing.length; i++) {
			  msg += '"' + missing[i] + '"';
			  if (i != missing.length - 1)
			      msg += ",";
			}
	        return window.confirm(msg);
		}
				
	    return true;
	},
	
	checkCalcFields: function(view, error) {
		if (view == "DetailView")
           return true;
		
   		var Dom = YAHOO.util.Dom;
	    var panels = Dom.get('panels');
	    var fields = Dom.getElementsByClassName('field_name', 'span', 'panels');
	    var cfs = [ ];
	    for(i in fields){
	        if (Studio2.calcFieldList.indexOf(fields[i].innerHTML) != -1) {
	            cfs.push(fields[i].innerHTML);
	        }
	    }
	    if (cfs.length > 0) 
	    {
	        var msg = SUGAR.language.get("ModuleBuilder", error) + "\n";
	        for (var i = 0; i < cfs.length; i++) {
	          msg += '"' + cfs[i] + '"';
	          if (i != cfs.length - 1)
	              msg += ",";
	        }
	        return window.confirm(msg);
	    }
	    return true;
		
	},

    getScrollZones: function(parent){
        var Dom = YAHOO.util.Dom, TL, TR, BL, BR;
        //Height of the scroll zones
        var h = 20, el = Dom.get(parent);
        //Calculate the top area
        TL = Dom.getXY(el);
        BR = [TL[0] + el.clientWidth, TL[1] + h];
        var scrollUpBox = [TL, BR];
        //Calculate the bottom area.
        BR = [BR[0], TL[1] + el.clientHeight];
        TL = [TL[0], BR[1] - h];

        var scrollDownBox = [TL, BR];
        return  {
            up: scrollUpBox,
            down: scrollDownBox
        }
    },

    isWithinBox: function(xy, box)
    {
        return xy[0] > box[0][0] && xy[0] < box[1][0] && xy[1] > box[0][1] && xy[1] < box[1][1];
    },

    setScrollObj: function(o){
        Studio2.scrollObj = o;
        Studio2.scrollCheck = setInterval(function(){
            var o = Studio2.scrollObj;
            for(var i in Studio2.scrollZones)
            {
                var zone = Studio2.scrollZones[i];
                if (Studio2.isWithinBox([o.lastX, o.lastY], zone.up))
                {
                    document.getElementById(i).scrollTop -= 5;
                    YAHOO.util.DragDropMgr.refreshCache();
                    return;
                }
                else if (Studio2.isWithinBox([o.lastX, o.lastY], zone.down))
                {
                    document.getElementById(i).scrollTop += 5;
                    YAHOO.util.DragDropMgr.refreshCache();
                    return;
                }
            }
        }, 25);
    },

    clearScrollObj: function() {
        Studio2.scrollObj = false;
        if (Studio2.scrollCheck)
            clearInterval(Studio2.scrollCheck);
        Studio2.scrollCheck = false;
    },

    onDrag: function(e) {
       // Keep track of the direction of the drag for use during onDragOver
        var y = e.pageY;

        if (y < this.lastY) {
            this.goingUp = true;
        } else if (y > this.lastY) {
            this.goingUp = false;
        }

        this.lastY = e.pageY || e.clientY;
        this.lastX = e.pageX || e.clientX;
    }
};

Studio2.firstPanelId = "";
Studio2.firstPanelIdCount = 0;
