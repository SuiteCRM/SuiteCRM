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

 (function() {
	var sw = YAHOO.SUGAR,
		Event = YAHOO.util.Event,
		Connect = YAHOO.util.Connect,
	    Dom = YAHOO.util.Dom
	    SE = SUGAR.email2;

///////////////////////////////////////////////////////////////////////////////
////    ADDRESS BOOK
SE.addressBook = {
    _contactCache : new Array(), // cache of contacts
    _dd : new Array(), // filtered list, same format as _contactCache
    _ddLists : new Array(), // list of Lists
    _dd_mlUsed : new Array(), // contacts in mailing list edit view column1
    _dd_mlAvailable : new Array(), // contacts in mailing list edit view column2
    clickBubble : true, // hack to get around onclick event bubbling
	relatedBeanId : '',
	relatedBeanType : '',
	idx : 0,

    itemSpacing : 'white-space:nowrap; padding:2px;',
    reGUID : SE.reGUID,



    /**
    *  YUI bug fix 2527707.  Causes nested datatable's in <tables> to cause 404 errors whens earching.
    */
    initFixForDatatableSort: function () {
        //Workaround for YUI bug 2527707: http://yuilibrary.com/projects/yui2/ticket/913efafad48ce433199f3e72e4847b18, should be removed when YUI 2.8+ is used
        YAHOO.widget.DataTable.prototype.getColumn = function(column) {
            var oColumn = this._oColumnSet.getColumn(column);

            if(!oColumn) {
                // Validate TD element
                var elCell = column.nodeName.toLowerCase() != "th" ? this.getTdEl(column) : false;
                if(elCell) {
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

            return oColumn;
        };
    },

    cancelEdit : function() {
        if(this.editContactDialog)
            this.editContactDialog.hide();
        if(this.editMailingListDialog)
            this.editMailingListDialog.hide();
    },

    /**
     * Clears filter form
     */
    clear : function() {
        var t = document.getElementById('contactsFilter');
        t.value = '';
        this.filter(t);
    },

    /**
     * handle context-menu Compose-to call
     * @param string type 'contacts' or 'lists'
     */
    composeTo : function(type, waited) {
        var activePanel = SUGAR.email2.innerLayout.get("activeTab").get("id")
        if (activePanel.substring(0, 10) != "composeTab") {
            SE.composeLayout.c0_composeNewEmail();
            setTimeout("SE.addressBook.composeTo('" + type + "', true);");
	        SE.contextMenus.contactsContextMenu.hide();
            return;
        }
        var idx = activePanel.substring(10);
        var rows = [ ];
        var id = '';
        // determine if we have a selection to work with
        if(type == 'contacts') {
            var ids = SE.contactView.getSelectedRows();
            for (var i in ids) {
            	rows[i] = SE.contactView.getRecord(ids[i]);
            }
            removeHiddenNodes(rows, SE.contactView);
        }
		else { return; }

        if(rows.length > 0) {
            SE.composeLayout.handleDrop(
                (type == 'contacts') ? SE.contactView : SE.emailListsView,
                null, rows, 'addressTO' + idx );
        } else {
            alert(app_strings.LBL_EMAIL_MENU_MAKE_SELECTION);
        }
    },

    editContact : function() {
        SE.contextMenus.contactsContextMenu.hide();
        var element = SE.contactView.getSelectedNodes()[0];
        var elementId = "";
        if (element.className.indexOf('address-contact') > -1) {
            elementId = element.id;
        } else if (element.className.indexOf('address-exp-contact') > -1) {
            elementId = element.id.substring(2);
        }
    },


    /**
     * Filters contact entries based on user input
     */
    filter : function(inputEl) {
        var ret = new Object();
        var re = new RegExp(inputEl.value, "gi");

        for(var i in this._contactCache) {
            if(this._contactCache[i].name.match(re)) {
                ret[i] = this._contactCache[i];
            }
        }

        this.buildContactList(ret);
    },

    fullForm : function(id, module) {
        document.location = "index.php?return_module=Emails&return_action=index&module=" + module + "&action=EditView&record=" + id;
    },

    /**
     * returns a formatted email address from the addressBook cache
     */
    getFormattedAddress : function(id) {
        var o = this._contactCache[id];
        var primaryEmail = '';

        for(var i=0; i<o.email.length; i++) {
            var currentEmail = o.email[i].email_address;

            if(o.email[i].primary_address == 1) {
                primaryEmail = o.email[i].email_address;
            }
        }

        var finalEmail = (primaryEmail == "") ? currentEmail : primaryEmail;
        var name = new String(o.name);
        var finalName = name.replace(/(<([^>]+)>)/ig, "").replace(/&#039;/gi,'\'');
        var ret = finalName + " <" + finalEmail.replace(/&#039;/gi,'\'') + ">";

        return ret;
    },

    /**
     * Sets up async call to query for matching contacts, users, etc.
     */
    searchContacts : function() {
        var fn = document.getElementById('input_searchField').value;
        var pe = document.getElementById('input_searchPerson').value;

        var rb = document.getElementById('hasRelatedBean').checked;
        if (rb) {
			var idx = this.idx;
        	var relatedBeanId = document.getElementById('data_parent_id' + idx).value;
        	var relatedBeanType = document.getElementById('data_parent_type' + idx).value;
        	this.addressBookDataModel.params['related_bean_id'] = relatedBeanId;
        	this.addressBookDataModel.params['related_bean_type'] = relatedBeanType;
        } else {
        	this.addressBookDataModel.params['related_bean_id'] = '';
        }

        this.addressBookDataModel.params['search_field'] = fn;
        this.addressBookDataModel.params['person'] = pe;
        this.addressBookDataModel.params['emailUIAction'] = 'getAddressSearchResults';
        this.grid._oDataSource = this.addressBookDataModel;
        this.grid.getDataSource().sendRequest(SUGAR.util.paramsToUrl(this.addressBookDataModel.params),  this.grid.onDataReturnInitializeTable, this.grid);
    },

    /**
     * Clear Search Crieteria For Addressbook
     */
    clearAddressBookSearch : function() {
        document.getElementById('input_searchField').value = "";
        document.getElementById('input_searchPerson').selectedIndex = 0;
    },

    /**
     * Opens modal select window to add contacts to addressbook
     */
    selectContactsDialogue : function(destId) {
        if(!this.contactsDialogue) {
        	var dlg = this.contactsDialogue = new YAHOO.widget.Dialog("contactsDialogue", {
            	modal:true,
            	visible:false,
            	draggable: false,
            	constraintoviewport: true,
                width   : 980,
                buttons : [{text: app_strings.LBL_EMAIL_ADDRESS_BOOK_ADD, isDefault: true, handler: this.populateEmailAddressFieldsFromResultTable},
                           {text: app_strings.LBL_EMAIL_ADDRESS_BOOK_CLEAR, isDefault: true, handler: this.clearAllEmailAddressFieldsFromResultTable} ]
            });
        	dlg.setHeader(app_strings.LBL_EMAIL_ADDRESS_BOOK_SELECT_TITLE);

        	var body = SUGAR.util.getAndRemove("contactsDialogueHTML");
        	dlg.setBody(body.innerHTML);
        	dlg.renderEvent.subscribe(function() {
            	var iev = YAHOO.util.Dom.get("contactsDialogueBody");
            	if (iev && !SUGAR.isIE) {
            		this.body.style.width = "950px";
            	}
            }, dlg);


        	dlg.beforeRenderEvent.subscribe(function() {
        		var dd = new YAHOO.util.DDProxy(dlg.element);
        		dd.setHandleElId(dlg.header);
        		dd.on('endDragEvent', function() {
        			dlg.show();
        		});
        	}, dlg, true);
        	dlg.render();

        	var tp = new YAHOO.widget.TabView("contactsSearchTabs");

        	var tabContent = SUGAR.util.getAndRemove("searchForm");
        	tp.addTab(new YAHOO.widget.Tab({
				label: app_strings.LBL_EMAIL_ADDRESS_BOOK_TITLE,
				scroll : true,
				content : tabContent.innerHTML,
				id : "addressSearchTab",
				active : true
			}));

        	var addListenerFields = ['input_searchPerson','input_searchField' ]
        	YAHOO.util.Event.addListener(addListenerFields,"keydown", function(e){
        		if (e.keyCode == 13) {
        			YAHOO.util.Event.stopEvent(e);
        			SUGAR.email2.addressBook.searchContacts();
        		}
        	});

        	this.contactsDialogue.render();
        	dlg.center();
        }
        //Quick Compose does not have an innerLayout component and will always be referenced with ix 0.
        if (typeof(SUGAR.email2.innerLayout) == 'undefined')
            var idx = 0;
        else
        {
            var activePanel = SUGAR.email2.innerLayout.get("activeTab").get("id");
            var idx = activePanel.substring(10);
        }
        SE.addressBook.idx = idx;

		var relatedBeanId;
        if ((hasRelatedBeanId = document.getElementById('data_parent_id' + idx).value) != '') {
        	document.getElementById('relatedBeanColumn').style.display = '';
        	var relatedBeanName = document.getElementById('data_parent_name' + idx).value;
		   	var relatedBeanType = document.getElementById('data_parent_type' + idx).value;
		   	relatedBeanId = document.getElementById('data_parent_id' + idx).value;
		   	document.getElementById('relatedBeanInfo').innerHTML = ' ' + relatedBeanType + ' <b>' + relatedBeanName + '</b>';
		   	SE.addressBook.relatedBeanType = relatedBeanType;
	    } else {
	    	document.getElementById('relatedBeanColumn').style.display = 'none';
	    	document.getElementById('hasRelatedBean').checked = false;
	    }

	    if (!SE.addressBook.grid)
	    {
	    	if (hasRelatedBeanId) {
	    		document.getElementById('hasRelatedBean').checked = true;
	    	}
	        AddressSearchGridInit();
			SE.addressBook.relatedBeanId = relatedBeanId;
	    }
	    else
	    {
	    	if (typeof(relatedBeanId) != 'undefined' && relatedBeanId != SE.addressBook.relatedBeanId)
	    	{
	    		SE.addressBook.relatedBeanId = relatedBeanId;
	    		document.getElementById('hasRelatedBean').checked = true;
	    	}
	    	if (document.getElementById('hasRelatedBean').checked == true)
	    	{
	    		SE.addressBook.addressBookDataModel.params['related_bean_id'] = relatedBeanId;
	       		SE.addressBook.addressBookDataModel.params['related_bean_type'] = relatedBeanType;
	    	} else {
	    		SE.addressBook.addressBookDataModel.params['related_bean_id'] = '';
	       		SE.addressBook.addressBookDataModel.params['related_bean_type'] = '';
	    	}
	       	SE.addressBook.addressBookDataModel.params['search_field'] = document.getElementById('input_searchField').value;;
			SE.addressBook.addressBookDataModel.params['person'] = document.getElementById('input_searchPerson').value;
    		SE.addressBook.grid.getDataSource().sendRequest(SUGAR.util.paramsToUrl(SE.addressBook.addressBookDataModel.params),  SE.addressBook.grid.onDataReturnInitializeTable, SE.addressBook.grid);
	    }

	    //Remove any lingering rows in the result set table if the module was closed.
	    SE.addressBook.gridResults.deleteRows(0, SUGAR.email2.addressBook.gridResults.getRecordSet().getLength());
	    //Repopulate
	    SE.addressBook.populateResulstTableEmailAddresses();

        this.contactsDialogue.show();
    },
    /**
    *  Clear all email addresses from result table.
    *
    */
    clearAllEmailAddressFieldsFromResultTable: function () {
        SUGAR.email2.addressBook.gridResults.deleteRows(0, SUGAR.email2.addressBook.gridResults.getRecordSet().getLength());
        //Unhighlight any rows currently selected if the emails were cleared.
        SUGAR.email2.addressBook.grid.toggleSelectAll(false);
        SUGAR.email2.addressBook.grid.reSelectRowsOnRender();
    },
    /**
    *  Take all email address listed in the compose tab To|Cc|Bcc fields and re-populates the
    *  results table.  This function is called when the address book is displayed.
    */
    populateResulstTableEmailAddresses: function () {

        var idx = SE.addressBook.idx;
        var emailFields = ['to','cc','bcc'];

        for(var k=0;k<emailFields.length;k++)
        {
            var elKey = 'address' + emailFields[k].toUpperCase() + idx;
            var allEmails = document.getElementById(elKey).value;
            if(allEmails == '')
                continue;

            var formatedEmails = SE.composeLayout._getEmailArrayFromString(allEmails);

    		for (var i=0; i<formatedEmails.length; i++)
    		{
    		    var t_name = formatedEmails[i].name;
    		    var t_emailAddr = formatedEmails[i].email_address;
    		    var displayEmail = t_name + ' <' + t_emailAddr + '>';
    		    if(t_name == '')
    		        t_name = displayEmail = t_emailAddr;

    		    var addressType = SE.addressBook.translateAddresType(emailFields[k],true);
                SUGAR.email2.addressBook.gridResults.addRow({'type':addressType,'name':t_name,'email_address': t_emailAddr,
                    'display_email_address': displayEmail,'bean_id': -1,'idx' : SE.addressBook.idx});
    		}
        }
    },

    /**
    * Checks all entries in the result table against a particular email address, returning true
    * if the email address is found, false otherwise.
    */
    doesEmailAdddressExistInResultTable: function(emailAddress)
    {
        if(trim(emailAddress) == '')
            return false;

        var emailAddressFound = false;
        var contacts = SE.addressBook.gridResults.getRecordSet().getRecords();
        for (var i=0; i < contacts.length; i++)
        {
            var data = SE.addressBook.gridResults.getRecord(contacts[i]).getData();
            //If we are adding to cc or bcc fields, make them visible.
            if(data.email_address == emailAddress)
            {
                emailAddressFound = true;
                break;
            }
        }

        return emailAddressFound;
    },
    /**
    *  Takes all email addresses that the users wishes to add from the address book and populates the To
    *  fields on the compose tab.
    */
    populateEmailAddressFieldsFromResultTable: function()
    {
        //Clear the fields first, all email addresses are stored in the address book
        var idx = SE.addressBook.idx;
        var emailFields = ['to','cc','bcc'];
        for(var k=0;k<emailFields.length;k++)
        {
            var elKey = 'address' + emailFields[k].toUpperCase() + idx;
            document.getElementById(elKey).value = "";
        }

        var contacts = SE.addressBook.gridResults.getRecordSet().getRecords();
        for (var i=0; i < contacts.length; i++)
        {
            var data = SE.addressBook.gridResults.getRecord(contacts[i]).getData();

            var addressTypeKey = SE.addressBook.translateAddresType(data.type,false);
            //If we are adding to cc or bcc fields, make them visible.
            if(addressTypeKey =='cc' || addressTypeKey =='bcc')
                SE.composeLayout.showHiddenAddress(addressTypeKey,data.idx);
            //Construct the target id
            var target_id = 'address' + addressTypeKey.toUpperCase() + data.idx

            var target = document.getElementById(target_id);
            target.value = SE.addressBook.smartAddEmailAddressToComposeField(target.value, data.display_email_address);
        }

        //Delete all rows from the result set table
        SUGAR.email2.addressBook.gridResults.deleteRows(0, SUGAR.email2.addressBook.gridResults.getRecordSet().getLength());

        //Hide the dialogue
        SE.addressBook.contactsDialogue.hide()
    },
    /**
    *  Insert contacts into the result table.
    */
    insertContactToResultTable : function(event,address_type) {

        var contactsDialogue = SE.addressBook.contactsDialogue;
        var contacts = SE.addressBook.grid.getSelectedRows();

        var rows = SUGAR.email2.addressBook.grid.getRecordSet().getRecords();
        for (var i = 0; i < rows.length; i++)
        {
			if (typeof(rows[i]) != "undefined" && rows[i].getData().checked )
			{
			    var recId = SE.addressBook.grid.getRecord(rows[i]).getId();
                SE.addressBook.insertContactRowToResultTable(recId,address_type);
                SUGAR.email2.addressBook.grid.selectRow(rows[i]);
                rows[i].setData("selected",true);
			}
        }
        var checkBoxes = SUGAR.email2.addressBook.grid.get("element").getElementsByTagName('input');
        for (var i = 0; i < checkBoxes.length; i++) {
            checkBoxes[i].checked = false;
        }
    },
    /**
    *
    */
    insertContactRowToResultTable : function(rowId, addressType) {
        var data = SE.addressBook.grid.getRecord(rowId).getData();
        if(SE.addressBook.doesGridResultsEntryExist(data.email) )
                return;
        var name = data.name.replace(/&#039;/gi,'\'').replace(/&quot;/gi,'"');
        var ea = name + ' <' + data.email.replace(/&#039;/gi,'\'') + '>';
        if(addressType == null)
            addressType = app_strings.LBL_EMAIL_ADDRESS_BOOK_ADD_TO.replace(/:$/,''); //Default to To when using the plus icon.
        SUGAR.email2.addressBook.gridResults.addRow({'type':addressType,'name':name,'email_address': data.email,'display_email_address': ea,'bean_id': data.bean_id,'idx' : SE.addressBook.idx});
    },
    /**
    * Remove a row from the gridsResult table.
    */
    removeRowFromGridResults : function(rowId,emailAddress)
    {
        var contacts = SE.addressBook.gridResults.getRecordSet().getRecords();
        for (var i=0; i < contacts.length; i++)
        {
            var rec = SE.addressBook.gridResults.getRecord(contacts[i]);
            var data = rec.getData();
            if(data.email_address == emailAddress)
            {
                SUGAR.email2.addressBook.gridResults.deleteRow(rec.getId());
                break;
            }
        }

       SUGAR.email2.addressBook.toggleSearchRowIcon(rowId,true);
    },
    /**
    * Translates between the addressType To|Cc|Bcc labels/keys.
    */
    translateAddresType: function(addressType,fromKey)
    {
        var displayTo = app_strings.LBL_EMAIL_ADDRESS_BOOK_ADD_TO.replace(/:$/,'');
        var displayCc = app_strings.LBL_EMAIL_ADDRESS_BOOK_ADD_CC.replace(/:$/,'');
        var displayBcc = app_strings.LBL_EMAIL_ADDRESS_BOOK_ADD_BCC.replace(/:$/,'');
        var mappingObject = {};

        if(fromKey)
            mappingObject = {'to':displayTo, 'cc':displayCc, 'bcc':displayBcc};
        else
        {
            mappingObject[displayTo] = 'to'; //Cant use object literal with variable variable.
            mappingObject[displayCc] = 'cc';
            mappingObject[displayBcc] = 'bcc';
        }

        return typeof(mappingObject[addressType]) != 'undefined' ? mappingObject[addressType] : '';

    },
    /**
    *
    */
    toggleSearchRowIcon : function(rowId,show)
    {
        if(show)
        {
            var idToShow = rowId + '_add_img';
            var idToHide = rowId + '_rm_img';
        }
        else
        {
            var idToShow = rowId + '_rm_img';
            var idToHide = rowId + '_add_img';
        }


        Dom.addClass(idToHide, "yui-hidden");
        Dom.removeClass(idToShow, "yui-hidden");
    },
    /**
    * Determine if an entry has already been added to the grid results table to prevent duplicates.
    */
    doesGridResultsEntryExist: function(emailAddrs)
    {

        var contactExists = false;
        var contacts = SE.addressBook.gridResults.getRecordSet().getRecords();
        for (var i=0; i < contacts.length; i++)
        {
            var data = SE.addressBook.gridResults.getRecord(contacts[i]).getData();
            if(data.email_address == emailAddrs)
            {
                contactExists = true;
                break;
            }
        }
        return contactExists;
    },

    /**
     * adds an email address to a string, but first checks if it exists
     * @param string concat The string we are appending email addresses to
     * @param string addr Email address to add
     * @return string
     */
    smartAddEmailAddressToComposeField : function(concat, addr) {
        var re = new RegExp(addr);

        if(!concat.match(re)) {
            if(concat != "") {
                concat += "; " + addr;
            } else {
                concat = addr;
            }
        }

        return concat;
    }
};
////    END ADDRESS BOOK
///////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////
////    AUTOCOMPLETE
/**
 * Auto-complete object
 */
SE.autoComplete = {
    config : {
        delimChar : [";", ","],
        useShadow :    false,
        useIFrame : false,
        typeAhead : true,
        prehighlightClassName : "yui-ac-prehighlight",
        queryDelay : 0
    },
    instances : new Array(),

    /**
     * Parses an addressBook entry looking for primary address.  If not found, it will return the last found address.
     * @param object Contact from AddressBook
     * @return string
     */
    getPrimaryAddress : function(contact) {
        var address = app_strings.LBL_EMAIL_ADDRESS_BOOK_NOT_FOUND;

        for(var eIndex in contact.email) {
            address = contact.email[eIndex].email_address;
            if(contact.email[eIndex].primary_address == 1) {
                return contact.email[eIndex].email_address;
            }
        }
        return address;
    },


    /**
     * initializes autocomplete widgets for a given compose view
     * @param int idx
     */
    init : function(idx) {
        var ds = new YAHOO.widget.DS_JSArray(this.returnDataSource(SE.addressBook._contactCache), {
            "queryMatchContains" : false,
            "queryMatchSubset" : true
        });

        this.instances[idx] = {
            to : null,
            cc : null,
            bcc : null
        };


        // instantiate the autoComplete widgets
        this.instances[idx]['to'] = new YAHOO.widget.AutoComplete('addressTO'+idx, "addressToAC"+idx, ds, this.config);
        this.instances[idx]['cc'] = new YAHOO.widget.AutoComplete('addressCC'+idx, "addressCcAC"+idx, ds, this.config);
        this.instances[idx]['bcc'] = new YAHOO.widget.AutoComplete('addressBCC'+idx, "addressBccAC"+idx, ds, this.config);

        // enable hiding of interfering textareas
        this.instances[idx]['to'].containerExpandEvent.subscribe(SE.autoComplete.toggleTextareaHide);
        this.instances[idx]['cc'].containerExpandEvent.subscribe(SE.autoComplete.toggleTextareaHide);
        this.instances[idx]['bcc'].containerExpandEvent.subscribe(SE.autoComplete.toggleTextareaHide);

        // enable reshowing of hidden textareas
        this.instances[idx]['to'].containerCollapseEvent.subscribe(SE.autoComplete.toggleTextareaShow);
        this.instances[idx]['cc'].containerCollapseEvent.subscribe(SE.autoComplete.toggleTextareaShow);
        this.instances[idx]['bcc'].containerCollapseEvent.subscribe(SE.autoComplete.toggleTextareaShow);

        // enable refreshes of contact lists
        this.instances[idx]['to'].textboxFocusEvent.subscribe(SE.autoComplete.refreshDataSource);
        this.instances[idx]['cc'].textboxFocusEvent.subscribe(SE.autoComplete.refreshDataSource);
        this.instances[idx]['bcc'].textboxFocusEvent.subscribe(SE.autoComplete.refreshDataSource);
    },

    refreshDataSource : function(sType, aArgs) {
        var textBoxId = aArgs[0].getInputEl().id; // "addressTo0"
        var idx;
        var refresh = SE.autoComplete.returnDataSource(SE.addressBook._contactCache);

        if(textBoxId.indexOf("addressTO") > -1 || textBoxId.indexOf("addressCC") > -1) {
            idx = textBoxId.substr(9);
        } else {
            idx = textBoxId.substr(10);
        }

        SE.autoComplete.instances[idx]['to'].dataSource.data = refresh;
        SE.autoComplete.instances[idx]['cc'].dataSource.data = refresh;
        SE.autoComplete.instances[idx]['bcc'].dataSource.data = refresh;
    },

    /**
     * Parses AddressBook entries to return an appropriate DataSource array for YUI.autoComplete
     */
    returnDataSource : function(contacts) {
        var ret = new Array();
        for(var id in contacts) {
            if (contacts[id].name) {
	            var primary = this.getPrimaryAddress(contacts[id]);

	            ret[ret.length] = contacts[id].name.replace(/<[\/]*b>/gi, '') + " <" + primary + ">";
	            //ret[ret.length] = contacts[id].name + " <" + primary + ">";

	            for(var emailIndex in contacts[id].email) {
	                ret[ret.length] = contacts[id].email[emailIndex].email_address;
	            }
            }
        }

        return ret;
    },

    /**
     * Hides address textareas to prevent autocomplete dropdown from being obscured
     */
    toggleTextareaHide : function(sType, aArgs) {
        var textBoxId = aArgs[0]._oTextbox.id; // "addressTo0"
        var type = "";
        var idx = -1;

        if(textBoxId.indexOf("addressTO") > -1) {
            type = "to";
        } else if(textBoxId.indexOf("addressCC") > -1) {
            type = "cc";
        }
        idx = textBoxId.substr(9);

        // follow through if not BCC
        if(type != "") {
            var cc = document.getElementById("addressCC" + idx);
            var bcc = document.getElementById("addressBCC" + idx);

            switch(type) {
                case "to":
                    cc.style.visibility = 'hidden';
                case "cc":
                    bcc.style.visibility = 'hidden';
                break;
            }
        }
    },

    /**
     * Redisplays the textareas after an address is committed.
     */
    toggleTextareaShow : function(sType, aArgs) {
        var textBoxId = aArgs[0]._oTextbox.id; // "addressTo0"
        var type = "";
        var idx = -1;

        if(textBoxId.indexOf("addressTO") > -1) {
            type = "to";
        } else if(textBoxId.indexOf("addressCC") > -1) {
            type = "cc";
        }
        idx = textBoxId.substr(9);

        // follow through if not BCC
        if(type != "") {
            document.getElementById("addressCC" + idx).style.visibility = 'visible';
            document.getElementById("addressBCC" + idx).style.visibility = 'visible';
        }
    }
};

////    END AUTOCOMPLETE
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
////    COMPOSE & SEND
/**
 * expands the options sidebar
 */
SE.composeLayout = {
    currentInstanceId : 0,
    ccHidden : true,
    bccHidden : true,
    outboundAccountErrors : null,
    loadedTinyInstances : {}, //Tracks which tinyMCE editors have initalized with html content.
    subjectMaxlen : 255,

    showAddressDetails : function(e) {
    	var linkElement = document.getElementById("More"+e.id);
    	var spanElement = document.getElementById("Detail"+e.id);
    	var emailAddressList = e.value;
    	if(e.value.length > 96)
    	{
        	var resultArray = SE.composeLayout._getEmailArrayFromString(emailAddressList);
            var displayArray = [];
    		for (var i=0; i<resultArray.length; i++)
    		{
    		    var t_name = resultArray[i].name;
    		    var t_emailAddr = resultArray[i].email_address;
    		    if(t_name == '')
    		       displayArray.push('<br/>&lt;' + t_emailAddr + '&gt;');
    		    else
    		       displayArray.push(t_name + '<br/>&lt;' + t_emailAddr + '&gt;');
    		}

            var result = displayArray.join('<br/>');
        	// Display
            linkElement.style.display = "inline";
            linkElement.style.height="10px";
            linkElement.style.overflow="visible";
            spanElement.innerHTML = result;
    	}
    	else
    		linkElement.style.display = "none";

	},

   /**
    *  Given a string of email address, return an array containing the name portion (if available)
    *  and email portion.
    */
    _getEmailArrayFromString : function (emailAddressList){

        var reg = /@.*?;/g;
        while ((results = reg.exec(emailAddressList)) != null)
        {
            orignial = results[0];
            parsedResult = results[0].replace(';', ':::::');
            emailAddressList = emailAddressList.replace (orignial, parsedResult);
        }

        reg = /@.*?,/g;
        while ((results = reg.exec(emailAddressList)) != null)
        {
            orignial = results[0];
            parsedResult = results[0].replace(',', ':::::');
            emailAddressList = emailAddressList.replace (orignial, parsedResult);
        }
        //Administrator <johndoe@som.com>  ;1@somwhe.com;2@somwherecomplex.com,3@somwherecomplex.com;4@somwherecomplex.com,5@somwherecomplex.com,
        var emailArr = emailAddressList.split(":::::");
        var resultsArray = [];
        var newArr = [];
        for (var i=0; i<emailArr.length; i++)
        {
            var rposition = emailArr[i].indexOf('<');
            var lposition = emailArr[i].indexOf('>');

            if(trim(emailArr[i]) != '')
            {
                if(rposition != -1 && lposition != -1)
                {
                    var t_name = emailArr[i].substr(0, rposition-1);
                    var t_emailAddr = emailArr[i].substr(rposition+1, (lposition-1 - rposition) );
                    resultsArray.push({'name':t_name, 'email_address': t_emailAddr});
                }
                else
                {
                    resultsArray.push({'name':'', 'email_address': emailArr[i]});
                }
            }
        }
        return resultsArray;
    },
    ///////////////////////////////////////////////////////////////////////////
    ////    COMPOSE FLOW
    /**
     * Prepare bucket DIV and yui-ext tab panels
     */
    _0_yui : function() {
        var idx = this.currentInstanceId;

        var composeTab = new YAHOO.SUGAR.ClosableTab({
        		label: mod_strings.LNK_NEW_SEND_EMAIL,
				scroll : true,
				content : "<div id='htmleditordiv" + idx + "'/>",
				id : "composeTab" + idx,
				closeMsg: app_strings.LBL_EMAIL_CONFIRM_CLOSE,
				active : true
        }, SE.innerLayout);
        SE.innerLayout.addTab(composeTab);

        // get template engine with template
        if (!SE.composeLayout.composeTemplate) {
        	SE.composeLayout.composeTemplate = new YAHOO.SUGAR.Template(SE.templates['compose']);
        }

        // create Tab inner layout
        var composePanel =  this.getComposeLayout();
        composePanel.getUnitByPosition("right").collapse();
        composePanel.autoSize();

    },
	/**
     * Generate the quick compose layout
	 * @method getQuickComposeLayout
	 * @param {Pannel} parentPanel Parent pannel
	 * @param {Object} o Options
	 * @return {} none
	 **/
    getQuickComposeLayout : function (parentPanel,o) {
    	 var idx = SE.composeLayout.currentInstanceId;

    	 //Before rendering the parent pannel we need to initalize the grid layout
    	 parentPanel.beforeRenderEvent.subscribe(function() {

    	 	YAHOO.util.Event.onAvailable('htmleditordiv' + idx, function() {
    	 		SE.composeLayout._createComposeLayout(idx);
    	 		SE.composeLayout[idx].set('height', 550);
	        	SE.composeLayout[idx].render();
           });
        });

     	 //Wait until the Compose Layout has rendered, then add the
     	 //options tab and perform the tiny initialization.
         parentPanel.renderEvent.subscribe(function() {

    	 	YAHOO.util.Event.onAvailable('htmleditordiv' + idx, function() {
     		SE.composeLayout._initComposeOptionTabs(idx);
     		SE.composeLayout[idx].getUnitByPosition("right").collapse();
     		//Initialize tinyMCE
            SE.composeLayout._1_tiny(false);

     		//Init templates and address book
     		SE.composeLayout._2_final();

            SE.composeLayout.quickCreateComposePackage(o);

    	 	});
    	 });

	    //Check if we have the div override for the shortcut bar
        if(typeof o.menu_id != 'undefined') {
		   parentPanel.render(o.menu_id);
	    } else {
		   parentPanel.render(document.body);
	    }

        return SE.composeLayout[idx];
    },
    /**
     * Fill in all fields into the quick compose layout.
	 * @method quickCreateComposePackage
	 * @param {Object} o Options
	 * @return {} none
	 **/
    quickCreateComposePackage: function(o)
    {
        //If we have a compose package fill in defaults.
        if (typeof(o.composePackage) != 'undefined')
        {
            composePackage = o.composePackage; //Set the compose data object
            //Hijack this method called by composePackage as it's not need for quick creates.
            SE.composeLayout.c0_composeNewEmail = function(){};
            SE.composeLayout.composePackage(); //Fill in defaults.
        }
    },
    getComposeLayout : function() {
        var idx = SE.composeLayout.currentInstanceId;

       	this._createComposeLayout(idx);
        SE.composeLayout[idx].render();
        this._initComposeOptionTabs(idx);

        return SE.composeLayout[idx];
        },

        /**
        *	Create the layout manager for the compose window.
        */
        _createComposeLayout : function(idx)
        {
        	SE.composeLayout[idx] = new YAHOO.widget.Layout('htmleditordiv' + idx, {
        	parent: SE.complexLayout,
        	border:true,
            hideOnLayout: true,
            height: 600,
			units: [{
					position: "center",
	                animate: false,
	                scroll: false,
	                split:true,
	                body:
	                	SE.composeLayout.composeTemplate.exec({
	                        'app_strings':app_strings,
	                        'mod_strings':mod_strings,
	                        'linkbeans_options' : linkBeans,
	                        'idx' : SE.composeLayout.currentInstanceId
	                	})
	            },{
	            	position: "right",
				    scroll:true,
				    collapse: true,
				    collapsed: true,
				    resize: true,
				    border:true,
				    animate: false,
				    width:'230',
				    body: "<div class='composeRightTabs' id='composeRightTabs" + idx + "'/>",
				    titlebar: true,
				    split: true,
				    header: app_strings.LBL_EMAIL_OPTIONS
	            }]
	        });
        },

        /**
        *  Create compose tab which will populate the 'right' container in the compose window.
        */
        _initComposeOptionTabs : function(idx)
        {
	        var cTabs = new YAHOO.widget.TabView("composeRightTabs" + idx);
	        var tab = new YAHOO.widget.Tab({
				label: app_strings.LBL_EMAIL_ATTACHMENT,
				scroll : true,
				content : SUGAR.util.getAndRemove("divAttachments" + idx).innerHTML,
				id : "divAttachments" + idx,
				active : true
			});

	        tab.layout = SE.composeLayout[idx];

     	   tab.on("activeChange", function(o){
        		if (o.newValue) {
        			this.layout.getUnitByPosition("right").set("header", app_strings.LBL_EMAIL_ATTACHMENT);
        		}
       		});

        	cTabs.addTab(tab);

	        tab = new YAHOO.widget.Tab({
				label: app_strings.LBL_EMAIL_OPTIONS,
				scroll : true,
				content : SUGAR.util.getAndRemove("divOptions" + idx).innerHTML,
				id : "divOptions" + idx,
				active : false
			});

	        tab.layout = SE.composeLayout[idx];
	        tab.on("activeChange", function(o){
	        	if (o.newValue) {
	        		this.layout.getUnitByPosition("right").set("header", app_strings.LBL_EMAIL_OPTIONS);
	        	}
	        });
        	cTabs.addTab(tab);

	        SE.composeLayout[idx].autoSize = function() {
	        	var pEl = this.get("element").parentNode.parentNode.parentNode;
	        	this.set("height", pEl.clientHeight-30);
	        	this.render();
	        }

        	SE.composeLayout[idx].rightTabs = cTabs;
    },
    isParentTypeValid : function(idx) {
		var parentTypeValue = document.getElementById('data_parent_type' + idx).value;
		var parentNameValue = document.getElementById('data_parent_name' + idx).value;
		if (trim(parentTypeValue) == ""){
			alert(mod_strings.LBL_ERROR_SELECT_MODULE);
			return false;
		} // if
		return true;
    },

    isParentTypeAndNameValid : function(idx) {
		var parentTypeValue = document.getElementById('data_parent_type' + idx).value;
		var parentNameValue = document.getElementById('data_parent_name' + idx).value;
		var parentIdValue = document.getElementById('data_parent_id' + idx).value;
		if ((trim(parentTypeValue) != "" && trim(parentNameValue) == "") ||
			(trim(parentTypeValue) != "" && trim(parentNameValue) != "" && parentIdValue == "")){
				alert(mod_strings.LBL_ERROR_SELECT_MODULE_SELECT);
			return false;
		} // if
		return true;
    },

    callopenpopupForEmail2 : function(idx,options) {

        var formName = 'emailCompose' + idx;

        if(typeof(options) != 'undefined' && typeof(options.form_name) != 'undefined')
            formName = options.form_name;

		var parentTypeValue = document.getElementById('data_parent_type' + idx).value;
		var parentNameValue = document.getElementById('data_parent_name' + idx).value;
		if (!SE.composeLayout.isParentTypeValid(idx)) {
			return;
		} // if
		open_popup(document.getElementById('data_parent_type' + idx).value,600,600,'&tree=ProductsProd',true,false,
		{
			call_back_function:"SE.composeLayout.popupAddEmail",
			form_name:formName,
			field_to_name_array:{
				id:'data_parent_id' + idx,
				name:'data_parent_name' + idx,
				email1:'email1'}
		});
	},

	popupAddEmail : function(o)
	{
		var nameKey = "data_parent_name" + SE.composeLayout.currentInstanceId;
		var data = o.name_to_value_array;
		if (typeof (data[nameKey]) != "undefined" && data[nameKey] != ""
			&& typeof (data["email1"]) != "undefined" && data["email1"] != "" && data["email1"] != "undefined")
        {
        	var target = Dom.get("addressTO" + SE.composeLayout.currentInstanceId);
        	target.value = SE.addressBook.smartAddEmailAddressToComposeField(target.value, data[nameKey] + "<" + data.email1 + ">");
        }
		set_return(o);
	},
    /**
     * Prepare TinyMCE
     */
    _1_tiny : function(isReplyForward) {
        var idx = SE.composeLayout.currentInstanceId;
        var elId = SE.tinyInstances.currentHtmleditor = 'htmleditor' + idx;
        SE.tinyInstances[elId] = { };
        SE.tinyInstances[elId].ready = false;

        if (!SUGAR.util.isTouchScreen()) {
            var t = tinymce.get(elId);
        }
        if(typeof(t) == 'undefined')  {
            if (!SUGAR.util.isTouchScreen()) {
                tinymce.init({
                    selector: '#' + elId,
                    convert_urls: false,
                    theme_advanced_toolbar_align: tinyConfig.theme_advanced_toolbar_align,
                    valid_children: tinyConfig.valid_children,
                    width: tinyConfig.width,
                    theme: tinyConfig.theme,
                    theme_advanced_toolbar_location: tinyConfig.theme_advanced_toolbar_location,
                    theme_advanced_buttons1: tinyConfig.theme_advanced_buttons1,
                    theme_advanced_buttons2: tinyConfig.theme_advanced_buttons2,
                    theme_advanced_buttons3: tinyConfig.theme_advanced_buttons3,
                    plugins: tinyConfig.plugins,
                    language: tinyConfig.language,
                    extended_valid_elements: tinyConfig.extended_valid_elements,
                    strict_loading_mode: true,
                    force_br_newlines: true,
                    forced_root_block: '',
                    license_key: tinyConfig.license_key
                });
            }
            YAHOO.util.Event.onAvailable(elId + "_parent", function() {
                SE.composeLayout.resizeEditorSetSignature(idx,!isReplyForward);
                }, this);
        }
    },

    resizeEditorSetSignature : function(idx,setSignature)
    {
    	var instance = SE.util.getTiny(SE.tinyInstances.currentHtmleditor);

        if(typeof(instance) == 'undefined' || (typeof(SE.composeLayout.loadedTinyInstances[idx]) != 'undefined' && SE.composeLayout.loadedTinyInstances[idx] == false)) {
            setTimeout("SE.composeLayout.resizeEditorSetSignature(" + idx + ",'"+setSignature+"');",500);
		    return;
		}

        SE.composeLayout.resizeEditor(idx);
        if(setSignature) {
            setTimeout("SUGAR.email2.composeLayout.setSignature("+idx+");",250);
        }

    },

    resizeEditor : function(idx)
    {
    	var cof = Dom.get('composeOverFrame' + idx);
        var head = Dom.get('composeHeaderTable' + idx);
        var targetHeight = cof.clientHeight - head.clientHeight;
    	var instance = SE.util.getTiny('htmleditor' + idx);

        try {
    	var parentEl = Dom.get(instance.editorId + '_parent');
    	var toolbar = Dom.getElementsByClassName("mceFirst", "tr", parentEl)[0];
    	var contentEl  = instance.contentAreaContainer;
        var iFrame = contentEl.firstChild;
        var tinMceToolbarOffset = 18;
        iFrame.style.height = (targetHeight - toolbar.offsetHeight - tinMceToolbarOffset)  + "px";

        } catch(e) {
            setTimeout("SE.composeLayout.resizeEditor("+idx+");",1000);
        }
    },

    /**
     * Initializes d&d, auto-complete, email templates
     */
    _2_final : function() {
        var idx = SE.composeLayout.currentInstanceId;

        if(this.emailTemplates) {
            this.setComposeOptions(idx);
        } else {
            //populate email template cache
            AjaxObject.target = '';
            AjaxObject.startRequest(callbackComposeCache, urlStandard + "&emailUIAction=fillComposeCache");
        }

        // handle drop targets for addressBook
       var to =  new YAHOO.util.DDTarget('addressTO' +idx, 'addressBookDD', {notifyDrop:this.handleDrop});
       var cc =  new YAHOO.util.DDTarget('addressCC' +idx, 'addressBookDD', {notifyDrop:this.handleDrop});
       var bcc = new YAHOO.util.DDTarget('addressBCC'+idx, 'addressBookDD', {notifyDrop:this.handleDrop});
       to.notifyDrop = cc.notifyDrop = bcc.notifyDrop = this.handleDrop;

        // auto-complete setup
        SE.autoComplete.init(idx);

        // set focus on to:
        document.getElementById("addressTO" + idx).focus();
    },

	/**
     * hide tinyMCE tool bar if send email as plaintext is checked
     */
    renderTinyMCEToolBar : function (idx, hide) {
    	if (hide) {
    		document.getElementById('htmleditor' + idx + '_toolbar1').style.display = 'none';
    	} else {
    		document.getElementById('htmleditor' + idx + '_toolbar1').style.display = '';
    	}
    },

    c1_composeEmail : function(isReplyForward, retry) {
        if (!retry) {
            this._0_yui();
        }
        if  (!SUGAR.util.isTouchScreen() && (typeof(tinyMCE) == 'undefined' || typeof(tinymce.settings) == 'undefined')){
            setTimeout("SE.composeLayout.c1_composeEmail(" + isReplyForward + ", true);", 500);
        } else {
	        this._1_tiny(isReplyForward);
	        this._2_final();

	        if(isReplyForward) {
	            this.replyForwardEmailStage2();
	        }
        }
    },

    /**
     * takes draft info and prepopulates
     */
    c0_composeDraft : function() {
        this.getNewInstanceId();
        inCompose = true;
        document.getElementById('_blank').innerHTML = '';
        var idx = SE.composeLayout.currentInstanceId;
		SE.composeLayout.draftObject = new Object();
		SE.composeLayout.draftObject.id = idx;
		SE.composeLayout.draftObject.isDraft = true;
        SE.composeLayout.currentInstanceId = idx;
        SE.tinyInstances.currentHtmleditor = 'htmleditor' + SE.composeLayout.currentInstanceId;
        SE.tinyInstances[SE.tinyInstances.currentHtmleditor] = new Object();
        SE.tinyInstances[SE.tinyInstances.currentHtmleditor].ready = false;

        SE.composeLayout._0_yui();
        SE.composeLayout._1_tiny(true);

        // final touches
        SE.composeLayout._2_final();

        /* Draft-specific final processing. Need a delay to allow Tiny to render before calling setText() */
        setTimeout("AjaxObject.handleReplyForwardForDraft(SE.o);", 1000);
    },

    /**
     * Strip & Prep editor hidden fields
     */
    c0_composeNewEmail : function() {
        this.getNewInstanceId();
        this.c1_composeEmail(false);
    },

    /**
     * Sends async request to get the compose view.
     * Requests come from "reply" or "forwards"
     */
    c0_replyForwardEmail : function(ieId, uid, mbox, type) {
        SE.composeLayout.replyForwardObj = new Object();
        SE.composeLayout.replyForwardObj.ieId = ieId;
        SE.composeLayout.replyForwardObj.uid = uid;
        SE.composeLayout.replyForwardObj.mbox = mbox;
        SE.composeLayout.replyForwardObj.type = type;

        if(mbox == 'sugar::Emails') {
            SE.composeLayout.replyForwardObj.sugarEmail = true;
        }

        SE.composeLayout.getNewInstanceId();
        SE.composeLayout.c1_composeEmail(true);
    },
    ////    END COMPOSE FLOW
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Called when a contact, email, or mailinglist is dropped
     * into one of the compose fields.
     */
    handleDrop : function (source, event, data, target) {
        var nodes;
        if (!target) {
            target = event.getTarget();
            if (data.single) {
                data.nodes = [data.nodes];
            }
            nodes = data.nodes;
        } else {
            target = document.getElementById(target);
            nodes = data;
        }

        if (target.id.indexOf('address') > -1) {
            // dropped onto email to/cc/bcc field
            for(var i in nodes) {
            	var node = nodes[i].getData();
            	var email = "";
                if (node[1].indexOf('contact') > -1) {
                    email = SE.addressBook.getFormattedAddress(node[0]);
                } else if (node[1].indexOf('address-email') > -1){
                    email = node[3].replace(/&nbsp;/gi, '');
                    email = email.replace('&lt;', '<').replace('&gt;', '>');
                    var tr = source.getTrEl(nodes[i]);
                    while (tr && !Dom.hasClass(tr, "address-contact")) {
                    	tr = source.getPreviousTrEl(tr);
                    }
                    var CID = source.getRecord(tr).getData()[0];
                    var o = SE.addressBook._contactCache[CID];
                    var name = new String(o.name);
                    var finalName = name.replace(/(<([^>]+)>)/ig, "");
                    email = finalName + email;
                }
                target.value = SE.addressBook.smartAddEmailAddressToComposeField(target.value, email);
            }
        }
    },


    /////////////////////////////////////////////////////////////////////////////
    ///    EMAIL TEMPLATE CODE
    applyEmailTemplate : function (idx, id) {

        //bug #20680
        var box_title = mod_strings.LBL_EMAILTEMPLATE_MESSAGE_SHOW_TITLE;
		var box_msg = mod_strings.LBL_EMAILTEMPLATE_MESSAGE_SHOW_MSG;
		var box_none_msg = mod_strings.LBL_EMAILTEMPLATE_MESSAGE_CLEAR_MSG;

		//bug #6224
		var to_addr = document.getElementById('addressTO'+idx);
		if (to_addr.value.search(/[^;,]{6,}[;,][^;,]{6,}/) != -1)
		{
			box_title = mod_strings.LBL_EMAILTEMPLATE_MESSAGE_WARNING_TITLE;
			box_msg = mod_strings.LBL_EMAILTEMPLATE_MESSAGE_MULTIPLE_RECIPIENTS + '<br /><br />' + box_msg;
		}

		// id is selected index of email template drop-down
		if(id == '' || id == "0") {
			YAHOO.SUGAR.MessageBox.show({
	           title:box_title,
	           msg: box_none_msg,
	           type: 'confirm',
	           fn: function(btn){
	           		if(btn=='no'){return;};
	           		SUGAR.email2.composeLayout.processNoneResult(idx, id);},
	           modal:true,
	           scope:this
	       });
	       return;
		}

		YAHOO.SUGAR.MessageBox.show({
           title:box_title,
           msg: box_msg,
           type: 'confirm',
           fn: function(btn){
           		if(btn=='no'){return;};
           		SUGAR.email2.composeLayout.processResult(idx, id);},
           modal:true,
           scope:this
       });
    },

    processNoneResult : function(idx, id) {
        var tiny = SE.util.getTiny('htmleditor' + idx);
        var tinyHTML = tiny.getContent();
        var openTag = '<div><span><span>';
        var htmllow = tinyHTML.toLowerCase();
        var start = htmllow.indexOf(openTag);
		if (start > -1) {
	        tinyHTML = tinyHTML.substr(start);
            tiny.setContent(tinyHTML);
		} else {
       	    tiny.setContent('');
		}
        //now that content is set, call method to set signature
        setTimeout("SUGAR.email2.composeLayout.setSignature("+idx+");",500);
    },

	processResult : function(idx , id){
		var post_data = {"module":"EmailTemplates","record":id};
		var global_rpcClient =  new SugarRPCClient();

		result = global_rpcClient.call_method('retrieve', post_data, true);
		if(!result['record']) return;
		json_objects['email_template_object'] = result['record'];
		this.appendEmailTemplateJSON();

        // get attachments if any
        AjaxObject.target = '';
        AjaxObject.startRequest(callbackLoadAttachments, urlStandard + "&emailUIAction=getTemplateAttachments&parent_id=" + id);
    },

    appendEmailTemplateJSON : function() {
        var idx = SE.composeLayout.currentInstanceId; // post increment

        // query based on template, contact_id0,related_to
        //jchi 09/10/2008 refix #7743
        if(json_objects['email_template_object']['fields']['subject'] != '' )
        {
            // cn: bug 7743, don't stomp populated Subject Line
            document.getElementById('emailSubject' + idx).value = decodeURI(encodeURI(json_objects['email_template_object']['fields']['subject']));
        }
        var text = '';
        if(json_objects['email_template_object']['fields']['text_only'] == 1)
        {
        	text = "<p>" + decodeURI(encodeURI(json_objects['email_template_object']['fields']['body'])).replace(/<BR>/ig, '</p><p>').replace(/<br>/gi, "</p><p>").replace(/&amp;/gi,'&').replace(/&lt;/gi,'<').replace(/&gt;/gi,'>').replace(/&#039;/gi,'\'').replace(/&quot;/gi,'"') + "</p>";
        	document.getElementById('setEditor' + idx).checked = true;
        	SUGAR.email2.composeLayout.renderTinyMCEToolBar(idx, 1);
        }
        else
        {
        	text = decodeURI(encodeURI(json_objects['email_template_object']['fields']['body_html'])).replace(/<BR>/ig, '\n').replace(/<br>/gi, "\n").replace(/&amp;/gi,'&').replace(/&lt;/gi,'<').replace(/&gt;/gi,'>').replace(/&#039;/gi,'\'').replace(/&quot;/gi,'"');
        	document.getElementById('setEditor' + idx).checked = false;
        	SUGAR.email2.composeLayout.renderTinyMCEToolBar(idx, 0);
        }


        var tiny = SE.util.getTiny('htmleditor' + idx);
        var tinyHTML = tiny.getContent();
        var openTag = '<div><span><span>';
        var closeTag = '</span></span></div>';
        var htmllow = tinyHTML.toLowerCase();
        var start = htmllow.indexOf(openTag);
		if (start > -1) {
	        var htmlPart2 = tinyHTML.substr(start);
	        tinyHTML = text + htmlPart2;
	        tiny.setContent(tinyHTML);
		} else {
        	tiny.setContent(text);
		}
		//now that content is set, call method to set signature
        setTimeout("SUGAR.email2.composeLayout.setSignature("+idx+");",500);
    },

    /**
     * Writes out the signature in the email editor
     */
    setSignature : function(idx)
    {
        if (!tinyMCE)
            return false;
        var hide = document.getElementById('setEditor' + idx).checked;
        SE.composeLayout.renderTinyMCEToolBar(idx,hide);
        //wait for signatures to load before trying to set them
        if (!SE.composeLayout.signatures) {
            setTimeout("SE.composeLayout.setSignature(" + idx + ");", 1000);
			return;
        }

        if(idx != null) {
            var sel = document.getElementById('signatures' + idx);
        } else {
            var sel = document.getElementById('signature_id');
            idx = SE.tinyInstances.currentHtmleditor;
        }

        //Ensure that the tinyMCE html has been rendered.
        if(typeof(SE.composeLayout.loadedTinyInstances[idx]) != 'undefined' && SE.composeLayout.loadedTinyInstances[idx] == false) {
            setTimeout("SE.composeLayout.setSignature(" + idx + ");",1000);
		    return;
		}

        var signature = '';

        try {
            signature = sel.options[sel.selectedIndex].value;
        } catch(e) {

        }

        // The tags are used for finding the signature
        var openTag = '<div id="signature-begin"><br />';
        var closeTag = '<span>&nbsp;</span></div>';

        // Get tinyMCE instance
        var t = tinymce.get('htmleditor' + idx);

        // IE 6 Hack
        if(typeof(t) != 'undefined')
        {
            t.contentDocument = t.contentWindow.document;
            var html = t.getContent();
        }
        else
        {
            var html = '';
        }

        var htmllow = html.toLowerCase();
        var start = htmllow.indexOf(openTag);
        // Start looking for the closeTag where the start tag was found
        var end = htmllow.indexOf(closeTag, start);
        if (end >= 0)
        {
            end += closeTag.length;
        }
        else
        {
            end = htmllow.length;
        }

        // selected "none" - remove signature from email
        if (signature == '')
        {
            if (start > -1)
            {
                var htmlPart1 = html.substr(0, start);
                var htmlPart2 = html.substr(end, html.length);

                html = htmlPart1 + htmlPart2;
                t.setContent(html);
            }
            SE.signatures.lastAttemptedLoad = '';
            return false;
        }

        if (!SE.signatures.lastAttemptedLoad) // lazy load place holder
        {
            SE.signatures.lastAttemptedLoad = '';
        }

        SE.signatures.lastAttemptedLoad = signature;

        if (typeof(SE.signatures[signature]) == 'undefined')
        {
            //lazy load
            SE.signatures.lastAttemptedLoad = ''; // reset this flag for recursion
            SE.signatures.targetInstance = (idx) ? idx : "";
            AjaxObject.target = '';
            AjaxObject.startRequest(callbackLoadSignature, urlStandard + "&emailUIAction=getSignature&id="+signature);
        }
        else
        {
            var newSignature = this.prepareSignature(SE.signatures[signature]);

            // clear out old signature
            if (SE.signatures.lastAttemptedLoad && start > -1)
            {
                var htmlPart1 = html.substr(0, start);
                var htmlPart2 = html.substr(end, html.length);

                html = htmlPart1 + htmlPart2;
            }

            // pre|append
			start = html.indexOf('<div><hr></div>');
            if (SE.userPrefs.signatures.signature_prepend == 'true' && start > -1) // Prepend
            {
				var htmlPart1 = html.substr(0, start);
				var htmlPart2 = html.substr(start, html.length);
                var newHtml = htmlPart1 + openTag + newSignature + closeTag + htmlPart2;
            }
            else if (SUGAR.email2.userPrefs.signatures.signature_prepend == 'true') // Prepend
            {
            	//bug 48285
                var newHtml = html;

                //remove custom spacing
                var spacing = '<span id="spacing"><br /><br /><br /></span>&nbsp;';
                var customSpacingStart = html.indexOf(spacing);

                if (customSpacingStart > -1)
                {
                    var part1 = newHtml.substr(0, customSpacingStart);
                    var part2 = newHtml.substr(customSpacingStart+spacing.length, newHtml.length);
                    newHtml = part1 + part2;
                }

                //append signature
                var bodyStartTag = '<body>';
                var body = newHtml.indexOf(bodyStartTag);

                if (body > -1)
                {
                    var part1 = newHtml.substr(0, body+bodyStartTag.length);
                    var part2 = newHtml.substr(body+bodyStartTag.length, newHtml.length);
                    newHtml = part1 + spacing + openTag + newSignature + closeTag + part2;
                }
                else
                {
                    newHtml = openTag + newSignature + closeTag + newHtml;
                }
                //end bug 48285
            }
            else // Append
            {
            	// On full compose mail has <body> element empty 
            	var bodyStringEmpty = htmllow.indexOf("<body>") > -1 && htmllow.replace(/\s/g, "").match(/<body>.+<\/body>/) == null;
            	// On quick compose a new document has html.length == 0
                if (htmllow.length == 0 || bodyStringEmpty)
                {
                    // Prepend <br /> to openTag because if it's an empty document
                    // or a document with nothing other than whitespace in <body></body>
                	// TinyMCE will pick the id of the div containing the signature
                    // when adding a new row and duplicate it so this might cause
                    // trouble when changing signatures
                    openTag = "<br />" + openTag;
                }

                var body = html.indexOf('</body>');
                if (body > -1)
                {
                    var part1 = html.substr(0, body);
                    var part2 = html.substr(body, html.length);
                    var newHtml = part1 + openTag + newSignature + closeTag + part2;
                }
                else
                {
                    var newHtml = html + openTag + newSignature + closeTag;
                }
            }
            t.setContent(newHtml);
        }
    },

    prepareSignature : function(str) {
        var signature = new String(str);

        signature = signature.replace(/&lt;/gi, '<');
        signature = signature.replace(/&gt;/gi, '>');

        return signature;
    },


    showAttachmentPanel : function(idx) {
    	var east = SE.composeLayout[idx].getUnitByPosition("right");
    	var tabs = SE.composeLayout[idx].rightTabs;
    	east.expand();
        tabs.set("activeTab", tabs.getTab(0));
    },

    /**
     * expands sidebar and displays options panel
     */
    showOptionsPanel : function(idx) {
    	var east = SE.composeLayout[idx].getUnitByPosition("right");
    	var tabs = SE.composeLayout[idx].rightTabs;
    	east.expand();
        tabs.set("activeTab", tabs.getTab(1));
    },

    /**
     * Selects the Contacts tab
     */
    showContactsPanel : function() {
        SE.complexLayout.regions.west.showPanel("contactsTab");
    },

    /**
     * Generates fields for Select Document
     */
    addDocumentField : function(idx) {
        var basket = document.getElementById('addedDocuments' + idx);
        if(basket) {
            var index = (basket.childNodes.length / 7) - 1;
            if(index < 0)
                index = 0;
        } else {
            index = 0;
        }

        var test = document.getElementById('documentId' + idx + index);

        while(test != null) {
            index++;
            test = document.getElementById('documentId' + idx + index);
        }

        var documentCup = document.createElement("div");
        documentCup.id = 'documentCup' + idx + index;
        documentCup.innerHTML = "<input type='hidden' name='document" + idx + index + "' id='document" + idx + index + "' />" +
                // document id field
                "<input type='hidden' name='documentId" + idx + index + "' id='documentId" + idx + index + "' />" +
                // document name field
                "<input value='' size='15' disabled='true' type='text' name='documentName" + idx + index + "' id='documentName" + idx + index + "' />" +
                // select button
                "<button class='button firstChild' type='button' name='documentSelect" + idx + index + "' id='documentSelect" + idx + index + "'" +
                    "onclick='SE.composeLayout.selectDocument(\"" + index + "\");' value='" + app_strings.LBL_EMAIL_SELECT + "'>" +
                "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=id-ff-select.png' ></button>" +
                // remove button
                "<button class='button lastChild' type='button' name='documentRemove" + idx + index + "' id='documentRemove" + idx + index + "'" +
                    "onclick='SE.composeLayout.deleteDocumentField(\"documentCup" + idx + index + "\");' value='" + app_strings.LBL_EMAIL_REMOVE + "'>" +
                 "<span class=\"suitepicon suitepicon-action-clear\"></span></button>" +
                "<br/>";

        basket.appendChild(documentCup);
        //basket.innerHTML += out;
        return index;
    },

    /**
     * Makes async call to save a draft of the email
     * @param int Instance index
     */
    saveDraft : function(tinyInstance) {
        SE.tinyInstances.currentHtmleditor = 'htmleditor' + tinyInstance;
        this.sendEmail(tinyInstance, true);
    },

    selectDocument : function(target) {
        URL="index.php?module=Emails&action=PopupDocuments&to_pdf=true&target=" + target;
        windowName = 'selectDocument';
        windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

        win = SUGAR.util.openWindow(URL, windowName, windowFeatures);
        if(window.focus) {
            // put the focus on the popup if the browser supports the focus() method
            win.focus();
        }
    },

    /**
     * Modal popup for file attachment dialogue
     */
    addFileField : function() {
    	if(!SE.addFileDialog){ // lazy initialize the dialog and only create it once
            SE.addFileDialog = new YAHOO.widget.Dialog("addFileDialog", {
            	modal:true,
            	visible:false,
            	fixedcenter:true,
            	constraintoviewport: true,
                scroll: true,
                keylisteners : new YAHOO.util.KeyListener(document, { keys:27 }, {
                	fn:function(){SE.addFileDialog.hide();}
                })
            });
            SE.addFileDialog.setHeader(app_strings.LBL_EMAIL_ATTACHMENTS);
            SE.addFileDialog.render();
           // SE.addFileDialog.addKeyListener(27, , SE.addFileDialog);
        }
    	Dom.removeClass("addFileDialog", "yui-hidden");

        SE.addFileDialog.show();
    },

    /**
     * Async upload of file to temp dir
     */
    uploadAttachment : function() {
        if(document.getElementById('email_attachment').value != "") {
            var formObject = document.getElementById('uploadAttachment');
            YAHOO.util.Connect.setForm(formObject, true, true);
            AjaxObject.target = '';
            AjaxObject.startRequest(callbackUploadAttachment, null);
        } else {
            alert(app_strings.LBL_EMAIL_ERROR_NO_FILE);
        }
    },

    /**
     * Adds a SugarDocument to an outbound email.  Action occurs in a popup window displaying a ListView from the Documents module
     * @param string target in focus compose layout
     */
    setDocument : function(idx, target, documentId, documentName, docRevId) {
        // fields are named/id'd [fieldName][instanceId][index]
        var addedDocs = document.getElementById("addedDocuments" + idx);
        var docId = document.getElementById('documentId' + idx + target);
        var docName = document.getElementById('documentName' + idx + target);
        var docRevisionId = document.getElementById('document' + idx + target);
        docId.value = documentId;
        docName.value = documentName;
        docRevisionId.value = docRevId;
    },

    /**
     * Removes the bucket div containing the document input fields
     */
    deleteDocumentField : function(documentCup) {
        var f0 = document.getElementById(documentCup);
        f0.parentNode.removeChild(f0);
    },

    /**
     * Removes a Template Attachment field
     * @param int
     * @param int
     */
    deleteTemplateAttachmentField : function(idx, index) {
        // create not-in-array values for removal filtering
        var r = document.getElementById("templateAttachmentsRemove" + idx).value;

        if(r != "") {
            r += "::";
        }

        r += document.getElementById('templateAttachmentId' + idx + index).value;
        document.getElementById("templateAttachmentsRemove" + idx).value = r;

        var target = 'templateAttachmentCup' + idx + index;
        d =  document.getElementById(target);
        d.parentNode.removeChild(d);
    },

    /**
     * Async removal of uploaded temp file
     * @param string index Should be a concatenation of idx and index
     * @param string
     */
    deleteUploadAttachment : function(index, file) {
        var d = document.getElementById('email_attachment_bucket' + index);
        d.parentNode.removeChild(d);

        // make async call to delete cached file
        AjaxObject.target = '';
        AjaxObject.startRequest('', urlStandard + "&emailUIAction=removeUploadedAttachment&file="+unescape(file));
    },

    /**
     * Attaches files coming from Email Templates
     */
    addTemplateAttachmentField : function(idx) {
        // expose title
        document.getElementById('templateAttachmentsTitle' + idx).style.display = 'block';

        var basket = document.getElementById('addedTemplateAttachments' + idx);

        if(basket) {
            var index = basket.childNodes.length;
            if(index < 0)
                index = 0;
        } else {
            index = 0;
        }

        var out = "<div id='templateAttachmentCup" + idx + index + "'>" +
				// remove button
				"<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=minus.gif' " +
					"style='cursor:pointer' align='absmiddle' onclick='SUGAR.email2.composeLayout.deleteTemplateAttachmentField(\"" +
					idx + "\",\"" + index + "\");'/>" +
				// file icon
				"<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=attachment.gif' " + "align='absmiddle' />" +
				// templateAttachment field
				"<input type='hidden' value='" + "' name='templateAttachment" + idx + index + "' id='templateAttachment" + idx + index + "' />" +
				// docId field
				"<input type='hidden' value='" + "' name='templateAttachmentId" + idx + index + "' id='templateAttachmentId" + idx + index + "' />" +
				// file name
				"<span id='templateAttachmentName"  + idx + index + "'" + ">&nbsp;</span>" +
				"<br id='br" + index + "></br>" +
				"<br id='brdoc" + index + "></br>" +
			"</div>";
		basket.innerHTML = basket.innerHTML + out;

        return index;
    },

    /**
     * Sends one email via async call
     * @param int idx Editor instance ID
     * @param bool isDraft
     */
    sendEmail : function(idx, isDraft) {

        //If the outbound account has an error message associate with it, alert the user and refuse to continue.
        var obAccountID = document.getElementById('addressFrom' + idx).value;

        if( typeof(SUGAR.email2.composeLayout.outboundAccountErrors[obAccountID]) != 'undefined' )
        {
            SUGAR.showMessageBox(app_strings.LBL_EMAIL_ERROR_DESC, SUGAR.email2.composeLayout.outboundAccountErrors[obAccountID], 'alert');
            return false;
        }


        var form = document.getElementById('emailCompose' + idx);
        var composeOptionsFormName = "composeOptionsForm" + idx;


        var t = SE.util.getTiny('htmleditor' + idx);
        if (t != null || typeof(t) != "undefined") {
            var html = t.getContent();
        } else {
            var html = "<p>" + document.getElementById('htmleditor' + idx).value + "</p>";
        }

 	    var subj = document.getElementById('emailSubject' + idx).value;
        var to = trim(document.getElementById('addressTO' + idx).value);
        var cc = trim(document.getElementById('addressCC' + idx).value);
        var bcc = trim(document.getElementById('addressBCC' + idx).value);
        var email_id = document.getElementById('email_id' + idx).value;
        var composeType = document.getElementById('composeType').value;
        var parent_type = document.getElementById("parent_type").value;
        var parent_id = document.getElementById("parent_id").value;

        var el_uid = document.getElementById("uid");
        var uid = (el_uid == null) ? '' : el_uid.value;

      	var el_ieId = document.getElementById("ieId");
        var ieId = (el_ieId == null) ? '' : el_ieId.value;

        var el_mbox = document.getElementById("mbox");
        var mbox = (el_mbox == null) ? '' : el_mbox.value;

        if (!isValidEmail(to) || !isValidEmail(cc) || !isValidEmail(bcc)) {
			alert(app_strings.LBL_EMAIL_COMPOSE_INVALID_ADDRESS);
        	return false;
        }

        if (!SE.composeLayout.isParentTypeAndNameValid(idx)) {
        	return;
        } // if
		var parentTypeValue = document.getElementById('data_parent_type' + idx).value;
		var parentIdValue = document.getElementById('data_parent_id' + idx).value;
        parent_id = parentIdValue;
        parent_type = parentTypeValue;

        var in_draft = (document.getElementById('type' + idx).value == 'draft') ? true : false;
        // baseline viability check

        if(to == "" && cc == '' && bcc == '' && !isDraft) {
            alert(app_strings.LBL_EMAIL_COMPOSE_ERR_NO_RECIPIENTS);
            return false;
        } else if(subj == '' && !isDraft) {
            if(!confirm(app_strings.LBL_EMAIL_COMPOSE_NO_SUBJECT)) {
                return false;
            } else {
                subj = app_strings.LBL_EMAIL_COMPOSE_NO_SUBJECT_LITERAL;
            }
        } else if(html == '' && !isDraft) {
            if(!confirm(app_strings.LBL_EMAIL_COMPOSE_NO_BODY)) {
                return false;
            }
        }

        SE.util.clearHiddenFieldValues('emailCompose' + idx);
		document.getElementById('data_parent_id' + idx).value = parentIdValue;
		var title = (isDraft) ? app_strings.LBL_EMAIL_SAVE_DRAFT : app_strings.LBL_EMAIL_SENDING_EMAIL;
        SUGAR.showMessageBox(title, app_strings.LBL_EMAIL_ONE_MOMENT);
        html = html.replace(/&lt;/ig, "sugarLessThan");
        html = html.replace(/&gt;/ig, "sugarGreaterThan");

        form.sendDescription.value = html;
        form.sendSubject.value = subj;
        form.sendTo.value = to;
        form.sendCc.value = cc;
        form.sendBcc.value = bcc;
        form.email_id.value = email_id;
        form.composeType.value = composeType;
        form.composeLayoutId.value = 'composeLayout' + idx;
        form.setEditor.value = (document.getElementById('setEditor' + idx).checked == false) ? 1 : 0;
        form.saveToSugar.value = 1;
        form.fromAccount.value = document.getElementById('addressFrom' + idx).value;
        form.parent_type.value = parent_type;
        form.parent_id.value = parent_id;
        form.uid.value = uid;
        form.ieId.value = ieId;
        form.mbox.value = mbox;

        // email attachments
        var addedFiles = document.getElementById('addedFiles' + idx);
        if(addedFiles) {
            for(i=0; i<addedFiles.childNodes.length; i++) {
                var bucket = addedFiles.childNodes[i];

                for(j=0; j<bucket.childNodes.length; j++) {
                    var node = bucket.childNodes[j];
                    var nName = new String(node.name);

                    if(node.type == 'hidden' && nName.match(/email_attachment/)) {
                        if(form.attachments.value != '') {
                            form.attachments.value += "::";
                        }
                        form.attachments.value += unescape(node.value);
                    }
                }
            }
        }

        // sugar documents
        var addedDocs = document.getElementById('addedDocuments' + idx);
        if(addedDocs) {
            for(i=0; i<addedDocs.childNodes.length; i++) {
                var cNode = addedDocs.childNodes[i];
                for(j=0; j<cNode.childNodes.length; j++) {
                    var node = cNode.childNodes[j];
                    var nName = new String(node.name);
                    if(node.type == 'hidden' && nName.match(/documentId/)) {
                        if(form.documents.value != '') {
                            form.documents.value += "::";
                        }
                        form.documents.value += node.value;
                    }
                }
            }
        }

        // template attachments
        var addedTemplateAttachments = document.getElementById('addedTemplateAttachments' + idx);
        if(addedTemplateAttachments) {
            for(i=0; i<addedTemplateAttachments.childNodes.length; i++) {
                var cNode = addedTemplateAttachments.childNodes[i];
                for(j=0; j<cNode.childNodes.length; j++) {
                    var node = cNode.childNodes[j];
                    var nName = new String(node.name);
                    if(node.type == 'hidden' && nName.match(/templateAttachmentId/)) {
                        if(form.templateAttachments.value != "") {
                            form.templateAttachments.value += "::";
                        }
                        form.templateAttachments.value += node.value;
                    }
                }
            }
        }

        // remove attachments
        form.templateAttachmentsRemove.value = document.getElementById("templateAttachmentsRemove" + idx).value;

        YAHOO.util.Connect.setForm(form);

        AjaxObject.target = 'frameFlex';

        // sending a draft email
        if(!isDraft && in_draft) {
            // remove row
            SE.listView.removeRowByUid(email_id);
        }

        var sendCallback = (isDraft) ? AjaxObject.composeLayout.callback.saveDraft : callbackSendEmail;
        var emailUiAction = (isDraft) ? "&emailUIAction=sendEmail&saveDraft=true" : "&emailUIAction=sendEmail";

        AjaxObject.startRequest(sendCallback, urlStandard + emailUiAction);
    },

    /**
     * Handles clicking the email address link from a given view
     */
    composePackage : function() {
        if(composePackage != null) {
            SE.composeLayout.c0_composeNewEmail();


            if(composePackage.to_email_addrs) {
                document.getElementById("addressTO" + SE.composeLayout.currentInstanceId).value = composePackage.to_email_addrs;
            } // if

            if (composePackage.cc_addrs) {
                document.getElementById("addressCC" + SE.composeLayout.currentInstanceId).value = composePackage.cc_addrs;
                SE.composeLayout.showHiddenAddress('cc', SE.composeLayout.currentInstanceId);
            }

            if (composePackage.subject != null && composePackage.subject.length > 0) {
            	document.getElementById("emailSubject" + SE.composeLayout.currentInstanceId).value = composePackage.subject;
            }

            //If no parent fields are set in the composePackage, ensure they are cleared.
            var parentFields = ['parent_type','parent_name','parent_id'];
            for(var i=0;i<parentFields.length;i++)
            {
                if ( typeof(composePackage[parentFields[i]]) == 'undefined' )
                    composePackage[parentFields[i]] = "";
            }

            document.getElementById("parent_type").value = composePackage.parent_type;
            document.getElementById('data_parent_type' + SE.composeLayout.currentInstanceId).value = composePackage.parent_type;
            document.getElementById("parent_id").value = composePackage.parent_id;
            document.getElementById('data_parent_id' + SE.composeLayout.currentInstanceId).value = composePackage.parent_id;
            document.getElementById('data_parent_name' + SE.composeLayout.currentInstanceId).value = composePackage.parent_name;

            if(composePackage.email_id != null && composePackage.email_id.length > 0) {
                document.getElementById("email_id" + SE.composeLayout.currentInstanceId).value = composePackage.email_id;
            } // if
            if (composePackage.body != null && composePackage.body.length > 0) {
		        var tiny = SE.util.getTiny('htmleditor' + SE.composeLayout.currentInstanceId);
		        SE.composeLayout.loadedTinyInstances[SE.composeLayout.currentInstanceId] = false;
        		setTimeout("SE.composeLayout.setContentOnThisTiny();", 3000);
            } // if
            if (composePackage.attachments != null) {
				SE.composeLayout.loadAttachments(composePackage.attachments);
            } // if

            if (composePackage.fromAccounts != null && composePackage.fromAccounts.status) {
				var addressFrom = document.getElementById('addressFrom' + SE.composeLayout.currentInstanceId);
		        SE.util.emptySelectOptions(addressFrom);
		        var fromAccountOpts = composePackage.fromAccounts.data;
		        for(i=0; i<fromAccountOpts.length; i++) {
		              var key = fromAccountOpts[i].value;
		              var display = fromAccountOpts[i].text;
		              var opt = new Option(display, key);
		              if (fromAccountOpts[i].selected) {
		              	opt.selected = true;
		              }
		              addressFrom.options.add(opt);
		        }

            } // if
        } // if
    },

    setContentOnThisTiny : function(recursive) {
    	var tiny = SE.util.getTiny('htmleditor' + SE.composeLayout.currentInstanceId);
        var tinyHTML = tiny.getContent();
        composePackage.body = decodeURI(encodeURI(composePackage.body));
        // cn: bug 14361 - text-only templates don't fill compose screen
        if(composePackage.body == '') {
            composePackage.body = decodeURI(encodeURI(composePackage.body)).replace(/<BR>/ig, '\n').replace(/<br>/gi, "\n").replace(/&amp;/gi,'&').replace(/&lt;/gi,'<').replace(/&gt;/gi,'>').replace(/&#039;/gi,'\'').replace(/&quot;/gi,'"');
        } // if
        //Flag determines if we should clear the tiny contents or just append
        if (typeof(composePackage.clearBody) != 'undefined' && composePackage.clearBody)
        {
            SE.composeLayout.tinyHTML = '';
        }
        else
        {

            //check to see if tiny is defined, and this is not a recursive call if not, then call self function one more time
            if(typeof tiny == 'undefined'  &&  typeof recursive == 'undefined'){
                //call this same function again, this time setting the recursive flag to true
                setTimeout("SE.composeLayout.setContentOnThisTiny(true);", 3000);
                return;
            }

            //bug 48179
            //check tinyHTML for closing tags
            var body = tinyHTML.lastIndexOf('</body>');
            spacing = '<span id="spacing"><br /><br /><br /></span>&nbsp;';

            if (body > -1)
            {
                var part1 = tinyHTML.substr(0, body);
                var part2 = tinyHTML.substr(body, tinyHTML.length);
                var newHtml = part1 + spacing + composePackage.body + part2;
            }
            else
            {
                var newHtml = tinyHTML + spacing + composePackage.body;
            }
            //end bug 48179

            SE.composeLayout.tinyHTML = newHtml;
        }

         tiny.setContent(SE.composeLayout.tinyHTML);
         //Indicate that the contents has been loaded successfully.
         SE.composeLayout.loadedTinyInstances[SE.composeLayout.currentInstanceId] = true;
    },
    /**
     * Confirms closure of a compose screen if "x" is clicked
     */
    confirmClose : function(panel) {
        if(confirm(app_strings.LBL_EMAIL_CONFIRM_CLOSE)) {
            SE.composeLayout.closeCompose(panel.id);
            return true;
        } else {
            return false;
        }
    },

    /**
     * forces close of a compose screen
     */
    forceCloseCompose : function(id) {
    	SE.composeLayout.closeCompose(id);

    	// handle flow back to originating view
        if(composePackage) {
            // check if it's a module we need to return to
            if(composePackage.return_module && composePackage.return_action && composePackage.return_id) {
                if(confirm(app_strings.LBL_EMAIL_RETURN_TO_VIEW)) {
                    var url = "index.php?module=" + composePackage.return_module + "&action=" + composePackage.return_action + "&record=" + composePackage.return_id;
                    window.location = url;
                }
            }
        }
    },

    /**
     * closes the editor that just sent email
     * @param string id ID of composeLayout tab
     */
    closeCompose : function(id) {
        // destroy tinyMCE instance
        var idx = id.substr(13, id.length);
        var instanceId = "htmleditor" + idx;
        tinymce.remove('#' + instanceId);

        // nullify DOM and namespace values.
        inCompose = false;
        SE.composeLayout[idx] = null;
        SE.tinyInstances[instanceId] = null;
        var tabsArray = SE.innerLayout.get("tabs");
        for (i = 0 ; i < tabsArray.length ; i++) {
        	if (tabsArray[i].get("id") == ('composeTab' + idx)) {
        		tabsArray[i].close();
        		break;
        	}
        }
        //SE.innerLayout.getTab(idx).close();
    },

    /**
    *  Enable the quick search for the compose relate field or search tab
    */
    enableQuickSearchRelate: function(idx,overides){

        if(typeof overides != 'undefined')
        {
            var newModuleID = overides['moduleSelectField']; //data_parent_type_search
            var newModule = document.getElementById(newModuleID).value;
            var formName = overides['formName'];
            var fieldName = overides['fieldName'];
            var fieldId = overides['fieldId'];
            var fullName = formName + "_" + fieldName;
            var postBlurFunction = null;
        }
        else
        {
            var newModule = document.getElementById('data_parent_type'+idx).value;
            var formName = 'emailCompose'+idx;
            var fieldName = 'data_parent_name'+idx;
            var fieldId = 'data_parent_id'+idx;
            var fullName = formName + "_" + fieldName;
            var postBlurFunction = "SE.composeLayout.qsAddAddress";
        }

        if(typeof sqs_objects == 'undefined')
            window['sqs_objects'] = new Array;

        window['sqs_objects'][fullName] = {
            form:formName,
			method:"query",
			modules:[newModule],
			group:"or",
            field_list:["name","id", "email1"],populate_list:[fieldName,fieldId],required_list:[fieldId],
            conditions:[{name:"name",op:"like_custom",end:"%",value:""}],
			post_onblur_function: postBlurFunction,
            order:"name","limit":"30","no_match_text":"No Match"};


        if(typeof QSProcessedFieldsArray != 'undefined')
        	QSProcessedFieldsArray[fullName] = false;
        if (typeof(QSFieldsArray) != 'undefined' && typeof(QSFieldsArray[fullName]) != 'undefined') {
        	QSFieldsArray[fullName].destroy();
        	delete QSFieldsArray[fullName];
        }
        if (Dom.get(fullName + "_results")) {
        	Dom.get(fullName + "_results").parentNode.removeChild(Dom.get(fullName + "_results"));
        }

        enableQS(false);
    },

	qsAddAddress : function(o) {
        if (o.name != "" && o.email1 != "")
        {
        	var target = Dom.get("addressTO" + SE.composeLayout.currentInstanceId);
        	target.value = SE.addressBook.smartAddEmailAddressToComposeField(target.value, o.name + "<" + o.email1 + ">");
        }
    },
    /**
     * Returns a new instance ID, 0-index
     */
    getNewInstanceId : function() {
        this.currentInstanceId = this.currentInstanceId + 1;
        return this.currentInstanceId;
    },

    /**
     * Takes an array of objects that contain the filename and GUID of a Note (attachment or Sugar Document) and applies the values to the compose screen.  Valid use-cases are applying an EmailTemplate or resuming a Draft Email.
     */
    loadAttachments : function(result) {
        var idx = SE.composeLayout.currentInstanceId;

        if(typeof(result) == 'object') {
        	//jchi #20680. Clean the former template attachments;
        	var basket = document.getElementById('addedTemplateAttachments' + idx);
			if(basket.innerHTML != ''){
				confirm(mod_strings.LBL_CHECK_ATTACHMENTS, mod_strings.LBL_HAS_ATTACHMENTS, function(btn){
					if (btn != 'yes'){
						basket.innerHTML = '';
					}
				});
			}
            for(i in result) {
                if(typeof result[i] == 'object') {
                    var index = SE.composeLayout.addTemplateAttachmentField(idx);
                    var bean = result[i];
                    document.getElementById('templateAttachmentId' + idx + index).value = bean['id'];
                    document.getElementById('templateAttachmentName' + idx + index).innerHTML += bean['filename'];
                }
            }
        }
    },

    /**
     * fills drop-down values for email templates and signatures
     */
    setComposeOptions : function(idx) {
        // send from accounts
        var addressFrom = document.getElementById('addressFrom' + idx);

        if (addressFrom.options.length <= 0) {
        	SE.util.emptySelectOptions(addressFrom);
	        var fromAccountOpts = SE.composeLayout.fromAccounts;
	        for (id = 0 ; id < fromAccountOpts.length ; id++) {
	              var key = fromAccountOpts[id].value;
	              var display = fromAccountOpts[id].text;
	              var is_default = false;
	              if(key == SUGAR.default_inbound_accnt_id)
	              	is_default = true;
	              var opt = new Option(display, key);
	              addressFrom.options.add(opt);
	              addressFrom.options[id].selected = is_default; //Safari bug new Option(x,y,true) does not work.
	        }
        }

        // email templates
        var et = document.getElementById('email_template' + idx);
        SE.util.emptySelectOptions(et);

        for(var key in this.emailTemplates) { // iterate through assoc array
            var display = this.emailTemplates[key];
            var opt = new Option(display, key);
            et.options.add(opt);
        }

        // signatures
        var sigs = document.getElementById('signatures' + idx);
        SE.util.emptySelectOptions(sigs);

        for(var key in this.signatures) { // iterate through assoc array
            var display = this.signatures[key];
            var opt = new Option(display, key);

            if(key == SE.userPrefs.signatures.signature_default) {
                opt.selected = true;
            }

            sigs.options.add(opt);
        }

        // html/plain email?
        var htmlEmail = document.getElementById('setEditor' + idx);
        if(SE.userPrefs.emailSettings.sendPlainText == 1) {
            htmlEmail.checked = true;
        } else {
        	htmlEmail.checked = false;
        }

        SE.tinyInstances[SE.tinyInstances.currentHtmleditor].ready = true;
    },

    /**
     * After compose screen is rendered, async call to get email body from Sugar
     */
    replyForwardEmailStage2 : function() {
        SE.util.clearHiddenFieldValues('emailUIForm');
        SUGAR.showMessageBox(app_strings.LBL_EMAIL_RETRIEVING_MESSAGE, app_strings.LBL_EMAIL_ONE_MOMENT);

        var ieId = SE.composeLayout.replyForwardObj.ieId;
        var uid = SE.composeLayout.replyForwardObj.uid;
        var mbox = SE.composeLayout.replyForwardObj.mbox;
        var type = SE.composeLayout.replyForwardObj.type;
        var idx = SE.composeLayout.currentInstanceId;

        var sugarEmail = (SE.composeLayout.replyForwardObj.sugarEmail) ? '&sugarEmail=true' : "";

        document.getElementById('emailSubject' + idx).value = type;
        document.getElementById('emailUIAction').value = 'composeEmail';
        document.getElementById('composeType').value = type;
        document.getElementById('ieId').value = ieId;
        document.getElementById('uid').value = uid;
        document.getElementById('mbox').value = mbox;
		document.getElementById('setEditor' + idx).checked = SE.userPrefs.emailSettings.sendPlainText == 1 ? true : false;
        var formObject = document.getElementById('emailUIForm');
        YAHOO.util.Connect.setForm(formObject);

        var sendType = type;
        AjaxObject.startRequest(callbackReplyForward, urlStandard + "&composeType=" + type + sugarEmail);
    },

    /**
     * Move email addresses from To field to BCC field
     */
    moveToBCC : function (addrType,idx) {

        var toVal = $.trim($("#addressTO"+idx).val());
        var BCCVal =$.trim($("#addressBCC"+idx).val());

        if (toVal.length != 0)
        {
            // get rid of first comma in BCC field and last comma in TO field
            // so we don't end up with double commas in BCC field
            BCCVal = BCCVal.replace(/^,/, '');
            toVal = toVal.replace(/\,$/, '');

            $("#addressBCC"+idx).val(toVal +","+BCCVal);
            $("#addressTO"+idx).val("");     // empty out the to field
        }
        // show the BCC field
        SE.composeLayout.showHiddenAddress('bcc', SE.composeLayout.currentInstanceId);
    },

    /**
    *  Show the hidden cc or bcc fields
    */
    showHiddenAddress: function(addrType,idx){

    	Dom.removeClass(addrType+"_tr"+idx, "yui-hidden");
    	Dom.addClass(addrType+"_span"+idx, "yui-hidden");
		Dom.addClass("bcc_cc_sep"+idx, "yui-hidden");
		this[addrType+'Hidden'+idx] = false;

		//After bcc or cc is added, move options below last addr field
		Dom.insertAfter("add_addr_options_tr"+idx, 'bcc_tr'+idx);

		//If both cc and bcc hidden, remove the empty row containing text.
		if( ( typeof(this['ccHidden'+idx]) != 'undefined' && typeof(this['bccHidden'+idx]) != 'undefined')
			   && ( this['ccHidden'+idx]  == false && this['bccHidden'+idx] == false) )
			Dom.addClass("add_addr_options_tr"+idx, "yui-hidden");

		SE.composeLayout.resizeEditor(idx);
    },
    /**
    *  Hide the cc and bcc fields if they were shown.
    */
    hideHiddenAddresses: function(idx){

        var addrTypes = ['cc','bcc'];
        for(var i = 0;i<addrTypes.length;i++)
        {
            Dom.addClass(addrTypes[i] + "_tr"+idx, "yui-hidden");
            Dom.removeClass(addrTypes[i] + "_span"+idx, "yui-hidden");
            this[addrTypes[i] + 'Hidden'+idx] = true
        }

        Dom.removeClass("bcc_cc_sep"+idx, "yui-hidden");
        Dom.removeClass("add_addr_options_tr"+idx, "yui-hidden");
        Dom.insertBefore("add_addr_options_tr"+idx, 'bcc_tr'+idx);
    }
};

////    END SE.composeLayout
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////    SE.util
SE.util = {
    /**
     * Cleans serialized UID lists of duplicates
     * @param string
     * @return string
     */
    cleanUids : function(str) {
        var seen = new Object();
        var clean = "";
        var arr = new String(str).split(",");

        for(var i=0; i<arr.length; i++) {
            if(seen[arr[i]]) {
                continue;
            }

            clean += (clean != "") ? "," : "";
            clean += arr[i];
            seen[arr[i]] = true;
        }

        return clean;
    },

    /**
     * Clears hidden field values
     * @param string id ID of form element to clear
     */
    clearHiddenFieldValues : function(id) {
        var form = document.getElementById(id);

        for(i=0; i<form.elements.length; i++) {
            if(form.elements[i].type == 'hidden') {
                var e = form.elements[i];
                if(e.name != 'action' && e.name != 'module' && e.name != 'to_pdf') {
                    e.value = '';
                }
            }
        }
    },

    /**
     * Reduces a SELECT drop-down to 0 items to prepare for new ones
     */
    emptySelectOptions : function(el) {
        if(el) {
            for(i=el.childNodes.length - 1; i >= 0; i--) {
                if(el.childNodes[i]) {
                    el.removeChild(el.childNodes[i]);
                }
            }
        }
    },

    /**
     * Returns the MBOX path in the manner php_imap expects:
     * ie: INBOX.DEBUG.test
     * @param string str Current serialized value, Home.personal.test.INBOX.DEBUG.test
     */
    generateMboxPath : function(str) {
        var ex = str.split("::");

        /* we have a serialized MBOX path */
        if(ex.length > 1) {
            var start = false;
            var ret = '';
            for(var i=0; i<ex.length; i++) {
                if(ex[i] == 'INBOX') {
                    start = true;
                }

                if(start == true) {
                    if(ret != "") {
                        ret += ".";
                    }
                    ret += ex[i];
                }
            }
        } else {
            /* we have a Sugar folder GUID - do nothing */
            return str;
        }

        return ret;
    },

    /**
     * returns a SUGAR GUID by navigating the DOM tree a few moves backwards
     * @param HTMLElement el
     * @return string GUID of found element or empty on failure
     */
    getGuidFromElement : function(el) {
        var GUID = '';
        var iterations = 4;
        var passedEl = el;

        // upwards
        for(var i=0; i<iterations; i++) {
            if(el) {
                if(el.id.match(SE.reGUID)) {
                    return el.id;
                } else {
                    el = el.parentNode;
                }
            }
        }

        return GUID;
    },

    /**
     * Returns the ID value for the current in-focus, active panel (in the innerLayout, not complexLayout)
     * @return string
     */
    getPanelId : function() {
        return SE.innerLayout.get("activeTab").id ? SE.innerLayout.get("activeTab").id : "Preview";
    },

    /**
     * wrapper to handle weirdness with IE
     * @param string instanceId
     * @return tinyMCE Controller object
     */
    getTiny : function(instanceId) {
        if(instanceId == '') {
            return null;
        }

        var t = tinymce.get(instanceId);

        if(this.isIe()) {
            this.sleep(200);
            YAHOO.util.Event.onContentReady(instanceId, function(t) { return t; });
        }
        return t;
    },

    /**
     * Simple check for MSIE browser
     * @return bool
     */
    isIe : function() {
        var nav = new String(navigator.appVersion);
        if(nav.match(/MSIE/)) {
            return true;
        }
        return false;
    },

    /**
     * Recursively removes an element from the DOM
     * @param HTMLElement
     */
    removeElementRecursive : function(el) {
        this.emptySelectOptions(el);
    },

    /**
     * Fakes a sleep
     * @param int
     */
    sleep : function(secs) {
        setTimeout("void(0);", secs);
    },

    /**
     * Converts a <select> element to an Ext.form.combobox
     */
     convertSelect : function(select) {
       alert('in convertSelect');
       if (typeof(select) == "string") {
           select = document.getElementById(select);
       }
     },

     findChildNode : function (parent, property, value) {
    	 for (i in parent.children) {
    		 var child = parent.children[i];
    		 if (child.data[property] && child.data[property] == value || child[property] && child[property] == value)
    			 return child;
    		 var searchChild = SE.util.findChildNode(child, property, value);
    		 if (searchChild)
    			 return searchChild;
    	 }
    	 return false;
     },

     cascadeNodes : function (parent, fn, scope, args) {
    	 for (i in parent.children) {
    		 var child = parent.children[i];
    		 var s = scope ? scope : child;
    		 var a = args ? args : child;
        	 fn.call(s, a);
    		 SE.util.cascadeNodes(child, fn, scope, args);
    	 }
     }
};


////    END UTIL
///////////////////////////////////////////////////////////////////////////////


})();
