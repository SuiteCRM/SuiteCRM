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


/******************************************************************************
 * Initialize Email 2.0 Application
 */

//Override Sugar Languge so quick creates work properly


var getUserEditViewUserId = function () {
  var uid = null;
  if (
    typeof editViewSnapshots !== 'undefined' &&
    typeof editViewSnapshots.EditView !== 'undefined'
  ) {
    uid = editViewSnapshots.EditView
      .split('moduleUsersrecord')[1]
      .split('actionpageEditView')[0];
  }

  return uid;
};

function email2init() {

  //Init Tiny MCE
  // var tinyConfig = "code,bold,italic,underline,strikethrough,separator,justifyleft,justifycenter,justifyright,justifyfull," +
  //             "separator,bullist,numlist,outdent,indent,separator,forecolor,backcolor,fontselect,fontsizeselect";
  if (!SUGAR.util.isTouchScreen()) {
    tinymce.init({
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
      elements: tinyConfig.elements,
      language: tinyConfig.language,
      extended_valid_elements: tinyConfig.extended_valid_elements,
      mode: tinyConfig.mode,
      strict_loading_mode: true,
      force_br_newlines: true,
      forced_root_block: '',
      directionality: (typeof(rtl) == "undefined") ? "ltr" : "rtl",
      license_key: tinyConfig.license_key
    });
  }

  // initialze message overlay
  SUGAR.email2.e2overlay = new YAHOO.widget.Dialog("SUGAR.email2.e2overlay", {
      //iframe        : true,
      modal: false,
      autoTabs: true,
      width: 300,
      height: 120,
      shadow: true
    }
  );
  // Hide Sugar menu
  if (SUGAR.themes.tempHideLeftCol)
    SUGAR.themes.tempHideLeftCol();

  // add key listener for kb shortcust - disable backspace nav in mozilla/ie
//	YAHOO.util.Event.addListener(window.document, 'keypress', SUGAR.email2.keys.overall);

  // set defaults for YAHOO.util.DragDropManager
  YAHOO.util.DDM.mode = 0; // point mode, default is point (0)

  SUGAR.email2.nextYear = new Date();
  SUGAR.email2.nextYear.setDate(SUGAR.email2.nextYear.getDate() + 360);


  // initialize and display UI framework (complexLayout.js)
  complexLayoutInit();


  var user = getUserEditViewUserId();

  // initialize and display grid (grid.js)
  gridInit(user);

  // initialize treeview for folders
  //onloadTreeinit();
  SUGAR.email2.folders.rebuildFolders(true, user);


  //Setup the Message Box overlay
  /*Ext.MessageBox.maxWidth = 350;
   Ext.MessageBox.minProgressWidth = 350;

   ///////////////////////////////////////////////////////////////////////////
   ////	CONTEXT MENUS
   // detailView array
   SUGAR.email2.contextMenus.detailViewContextMenus = new Object();
   */
  var SEC = SUGAR.email2.contextMenus;

  //Grid menu
  var emailMenu = SEC.emailListContextMenu = new YAHOO.widget.ContextMenu("emailContextMenu", {
    trigger: SUGAR.email2.grid.get("element"),
    lazyload: true
  });
  emailMenu.subscribe("beforeShow", function () {
    var oTarget = this.contextEventTarget;
    if (typeof(oTarget) == "undefined")
      return;
    var grid = SUGAR.email2.grid;
    var selectedRows = grid.getSelectedRows();
    var multipleSelected = (selectedRows.length > 1) ? true : false;
    if (!multipleSelected) {
      grid.unselectAllRows();
      grid.selectRow(oTarget);
      SUGAR.email2.contextMenus.showEmailsListMenu(grid, grid.getRecord(oTarget));
    }
    else if (multipleSelected) {
      SUGAR.email2.contextMenus.showEmailsListMenu(grid, grid.getRecord(oTarget));
    }
  });

  //When we need to access menu items later we can only do so by indexes so we create a mapping to allow
  //us to access individual elements easier by name rather than by index
  emailMenu.itemsMapping = {
    'viewRelationships': 0, 'openMultiple': 1, 'archive': 2, 'reply': 3, 'replyAll': 4, 'forward': 5,
    'delete': 6, 'print': 7, 'mark': 8, 'assignTo': 9, 'relateTo': 10
  };
  emailMenu.addItems([
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_relate.gif'/>" + app_strings.LBL_EMAIL_VIEW_RELATIONSHIPS,
      id: 'showDetailView',
      onclick: {fn: SEC.showDetailView}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=open_multiple.gif'/>" + app_strings.LBL_EMAIL_OPEN_ALL,
      onclick: {fn: SEC.openMultiple}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_archive.gif'/>" + app_strings.LBL_EMAIL_ARCHIVE_TO_SUITE,
      onclick: {fn: SEC.archiveToSugar}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_reply.gif'/>" + app_strings.LBL_EMAIL_REPLY,
      id: 'reply',
      onclick: {fn: SEC.replyForwardEmailContext}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_replyall.gif'/>" + app_strings.LBL_EMAIL_REPLY_ALL,
      id: 'replyAll',
      onclick: {fn: SEC.replyForwardEmailContext}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_forward.gif'/>" + app_strings.LBL_EMAIL_FORWARD,
      id: 'forward',
      onclick: {fn: SEC.replyForwardEmailContext}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_delete.gif'/>" + app_strings.LBL_EMAIL_DELETE,
      id: 'delete',
      onclick: {fn: SEC.markDeleted}
    },
    {
      text: "<img src='themes/default/images/Print_Email.gif'/>" + app_strings.LBL_EMAIL_PRINT,
      id: 'print',
      onclick: {fn: SEC.viewPrintable}
    },
    // Mark... submenu
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_mark.gif'/>" + app_strings.LBL_EMAIL_MARK,
      submenu: {
        id: "markEmailMenu",
        itemdata: [
          {
            text: app_strings.LBL_EMAIL_MARK + " " + app_strings.LBL_EMAIL_MARK_UNREAD,
            onclick: {fn: SEC.markUnread}
          },
          {
            text: app_strings.LBL_EMAIL_MARK + " " + app_strings.LBL_EMAIL_MARK_READ,
            onclick: {fn: SEC.markRead}
          },
          {
            text: app_strings.LBL_EMAIL_MARK + " " + app_strings.LBL_EMAIL_MARK_FLAGGED,
            onclick: {fn: SEC.markFlagged}
          },
          {
            text: app_strings.LBL_EMAIL_MARK + " " + app_strings.LBL_EMAIL_MARK_UNFLAGGED,
            onclick: {fn: SEC.markUnflagged}
          }
        ]
      }
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_assign.gif'/>" + app_strings.LBL_EMAIL_ASSIGN_TO,
      id: 'assignTo',
      onclick: {fn: SEC.assignEmailsTo}
    },
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_relate.gif'/>" + app_strings.LBL_EMAIL_RELATE_TO,
      id: 'relateTo',
      onclick: {fn: SEC.relateTo}
    }
  ]);
  SEC.emailListContextMenu.render();

  //Handle the Tree folder menu trigger ourselves
  YAHOO.util.Event.addListener(YAHOO.util.Dom.get("emailtree"), "contextmenu", SUGAR.email2.folders.handleRightClick)


  //Folder Menu
  SEC.frameFoldersContextMenu = new YAHOO.widget.ContextMenu("folderContextMenu", {
    trigger: "",
    lazyload: true
  });
  SEC.frameFoldersContextMenu.addItems([
    {
      text: "<img src='index.php?entryPoint=getImage&themeName=" + SUGAR.themes.theme_name + "&imageName=icon_email_check.gif'/>" + app_strings.LBL_EMAIL_CHECK,
      //helptext: "<i>" + app_strings.LBL_EMAIL_MENU_HELP_ADD_FOLDER + "</i>",
      onclick: {
        fn: function () {
          var node = SUGAR.email2.clickedFolderNode;
          if (node.data.ieId) {
            SUGAR.email2.folders.startEmailCheckOneAccount(node.data.ieId, false)
          }
          ;
        }
      }
    },
    {
      text: app_strings.LBL_EMAIL_MENU_SYNCHRONIZE,
      //helptext: "<i>" + app_strings.LBL_EMAIL_MENU_HELP_ADD_FOLDER + "</i>",
      onclick: {
        fn: function () {
          var node = SUGAR.email2.clickedFolderNode;
          if (node.data.ieId) {
            SUGAR.email2.folders.startEmailCheckOneAccount(node.data.ieId, true)
          }
          ;
        }
      }
    },
    {
      text: app_strings.LBL_EMAIL_MENU_ADD_FOLDER,
      //helptext: "<i>" + app_strings.LBL_EMAIL_MENU_HELP_ADD_FOLDER + "</i>",
      onclick: {fn: SUGAR.email2.folders.folderAdd}
    },
    {
      text: app_strings.LBL_EMAIL_MENU_DELETE_FOLDER,
      //helptext: "<i>" + app_strings.LBL_EMAIL_MENU_HELP_DELETE_FOLDER + "</i>",
      onclick: {fn: SUGAR.email2.folders.folderDelete}
    },
    {
      text: app_strings.LBL_EMAIL_MENU_RENAME_FOLDER,
      //helptext: "<i>" + app_strings.LBL_EMAIL_MENU_HELP_RENAME_FOLDER + "</i>",
      onclick: {fn: SUGAR.email2.folders.folderRename}
    },
    {
      text: app_strings.LBL_EMAIL_MENU_EMPTY_TRASH,
      //helptext: "<i>" + app_strings.LBL_EMAIL_MENU_HELP_EMPTY_TRASH + "</i>",
      onclick: {fn: SUGAR.email2.folders.emptyTrash}
    },
    {
      text: app_strings.LBL_EMAIL_MENU_CLEAR_CACHE,
      onclick: {
        fn: function () {
          var node = SUGAR.email2.clickedFolderNode;
          if (node.data.ieId) {
            SUGAR.email2.folders.clearCacheFiles(node.data.ieId)
          }
          ;
        }
      }
    }
  ]);
  SEC.frameFoldersContextMenu.render();

  SEC.initContactsMenu = function () {
    // contacts
    SEC.contactsContextMenu = new YAHOO.widget.ContextMenu("contactsMenu", {
      trigger: "contacts",
      lazyload: true
    });
    SEC.contactsContextMenu.addItems([
      {
        text: app_strings.LBL_EMAIL_MENU_REMOVE,
        onclick: {fn: SUGAR.email2.addressBook.removeContact}
      },
      {
        text: app_strings.LBL_EMAIL_MENU_COMPOSE,
        onclick: {
          fn: function () {
            SUGAR.email2.addressBook.composeTo('contacts')
          }
        }
      }
    ]);
    SEC.contactsContextMenu.subscribe("beforeShow", function () {
      var oTarget = this.contextEventTarget, grid = SUGAR.email2.contactView;
      if (oTarget && !grid.isSelected(oTarget)) {
        grid.unselectAllRows();
        grid.selectRow(oTarget);
      }
    });
    SEC.contactsContextMenu.render();
  }


  // set auto-check timer
  SUGAR.email2.folders.startCheckTimer();
  // check if we're coming from an email-link click
  setTimeout("SUGAR.email2.composeLayout.composePackage()", 2000);

  YAHOO.util.Event.on(window, 'resize', SUGAR.email2.autoSetLayout);

  //Init fix for YUI 2.7.0 datatable sort.
  SUGAR.email2.addressBook.initFixForDatatableSort();
}

function createTreePanel(treeData, params) {
  var tree = new YAHOO.widget.TreeView(params.id);
  var root = tree.getRoot();

  //if (treeData.nodes && treeData[0].id == "Home")
  //	treeData = treeData[0];
  return tree;
}

function addChildNodes(parentNode, parentData) {
  var Ck = YAHOO.util.Cookie;
  var nextyear = SUGAR.email2.nextYear;
  var nodes = parentData.nodes || parentData.children;
  for (i in nodes) {
    if (typeof(nodes[i]) == 'object') {
      if (nodes[i].data) {
        // See comment about href below.
        // nodes[i].data.href = '#';
        var node = new YAHOO.widget.TextNode(nodes[i].data, parentNode);
        node.action = nodes[i].data.action;
      } else {
        if (nodes[i].id == SUGAR.language.get('app_strings', 'LBL_EMAIL_HOME_FOLDER')) {
          addChildNodes(parentNode, nodes[i]);
          return;
        }
        nodes[i].expanded = Ck.getSub("EmailTreeLayout", nodes[i].id + "") == "true";
        Ck.setSub("EmailTreeLayout", nodes[i].id + "", nodes[i].expanded ? true : false, {expires: SUGAR.email2.nextYear});
        if (nodes[i].cls) {
          nodes[i].className = nodes[i].cls;
        }
        // Previously, span was added in the label so it was rendering in the tree.
        // Default behavior is to wrap in span if no href property, and since this href
        // doesn't do anything, remove it so that it will be wrapped in spans.
        // nodes[i].href = "#";

        // URL Decode the text, so it shows properly
        nodes[i].text = unescape(nodes[i].text);

        if (nodes[i].text) nodes[i].label = nodes[i].text;
        //Override YUI child node creation
        if (nodes[i].children) {
          nodes[i].nodes = nodes[i].children;
          nodes[i].children = [];
        }
        var node = new YAHOO.widget.TextNode(nodes[i], parentNode);
      }

      if (typeof(nodes[i].nodes) == 'object') {
        addChildNodes(node, nodes[i]);
      }
    }
  }
}

/**
 * Custom TreeView initialization sequence to setup DragDrop targets for every tree node
 */
function email2treeinit(tree, treedata, treediv, params) {
  //ensure the tree data is not corrupt
  if (!treedata) {
    return;
  }
  if (SUGAR.email2.tree) {
    SUGAR.email2.tree.destroy();
    SUGAR.email2.tree = null;
  }

  var tree = SUGAR.email2.tree = createTreePanel({nodes: {}}, {
    id: 'emailtree'
  });

  tree.subscribe("clickEvent", SUGAR.email2.folders.handleClick);
  tree.subscribe("collapseComplete", function (node) {
    YAHOO.util.Cookie.setSub("EmailTreeLayout", node.data.id + "", false, {expires: SUGAR.email2.nextYear});
  });
  tree.subscribe("expandComplete", function (node) {
    YAHOO.util.Cookie.setSub("EmailTreeLayout", node.data.id + "", true, {expires: SUGAR.email2.nextYear});
    for (var i in node.children) {
      SE.accounts.setupDDTarget(node.children[i]);
    }
  });
  tree.setCollapseAnim("TVSlideOut");
  tree.setExpandAnim("TVSlideIn");
  var root = tree.root;
  while (root.hasChildren()) {
    var node = root.children[0];
    node.destroy();
    tree.removeNode(root.children[0], false);
  }
  addChildNodes(root, treedata);
  tree.render();
  SUGAR.email2.accounts.renderTree();
}

SUGAR.email2.folders.folderDD = function (id, sGroup, config) {
  SUGAR.email2.folders.folderDD.superclass.constructor.call(this, id, sGroup, config);
};


YAHOO.extend(SUGAR.email2.folders.folderDD, YAHOO.util.DDProxy, {
  startDrag: function (x, y) {
    var Dom = YAHOO.util.Dom;
    this.dragNode = SUGAR.email2.tree.getNodeByElement(this.getEl());

    this.dragId = "";
    var dragEl = this.getDragEl();
    var clickEl = this.getEl();
    Dom.setStyle(clickEl, "color", "#AAA");
    Dom.setStyle(clickEl, "opacity", "0.25");
    dragEl.innerHTML = clickEl.innerHTML;

    Dom.addClass(dragEl, "ygtvcell");
    Dom.addClass(dragEl, "ygtvcontent");
    Dom.addClass(dragEl, "folderDragProxy");
    Dom.setStyle(dragEl, "height", (clickEl.clientHeight - 5) + "px");
    Dom.setStyle(dragEl, "width", (clickEl.clientWidth - 5) + "px");
    Dom.setStyle(dragEl, "backgroundColor", "#FFF");
    Dom.setStyle(dragEl, "opacity", "0.5");
    Dom.setStyle(dragEl, "border", "1px solid #AAA");
  },

  onDragOver: function (ev, id) {
    var Dom = YAHOO.util.Dom;
    if (id != this.dragId) {
      var node = SUGAR.email2.tree.getNodeByElement(YAHOO.util.Dom.get(id));
      if (node.data.cls != "sugarFolder") {
        SUGAR.email2.folders.unhighliteAll();
        return;
      }
      this.dragId = id;
      this.targetNode = node;
      SUGAR.email2.folders.unhighliteAll();
      node.highlight();
    }
  },

  onDragOut: function (e, id) {
    if (this.targetNode) {
      SUGAR.email2.folders.unhighliteAll();
      this.targetNode = false;
      this.dragId = false;
    }
  },

  endDrag: function () {
    YAHOO.util.Dom.setStyle(this.getEl(), "opacity", "1.0");
    if (this.targetNode) {
      SUGAR.email2.folders.moveFolder(this.dragNode.data.id, this.targetNode.data.id);
    }
  }
});