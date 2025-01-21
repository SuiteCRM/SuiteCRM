/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

$(document).ready(function() {
  removeMenu();
  removeDisabledObjects(menu);
  createMenu();

  // Restore default menu configuration
  $("#restore-menu").on("click", function() {
    if (confirm(SUGAR.language.languages.Administration.LBL_STIC_MENU_RESTORE_CONFIRM) == false) {
      return;
    }

    // Define data to be sent in the request
    var dataToSend = {
      manageMode: "restore"
    };

    // Perform AJAX request to restore default menu
    $.ajax({
      url: location.href.slice(0, location.href.indexOf(location.search)),
      type: "POST",
      data: {
        module: "Administration",
        action: "SticAdvancedMenuController",
        ...dataToSend
      },
      success: function(response) {
        window.location.reload(true);
      },
      error: function(xhr, status, error) {
        console.error("Request error:", status, error);
      }
    });
  });

  // Save current menu configuration
  $("#save-menu").on("click", function() {
    function getNodeData(id) {
      var tree = $("#stic-menu-manager").jstree(true);
      var node = tree.get_node(id);
      var data = {
        id: node.id,
        text: node.text
      };

      // Add URL if exists
      if (node.a_attr["data-url"]) {
        data.url = node.a_attr["data-url"];
      }

      // Process children recursively
      if (node.children && node.children.length) {
        data.children = node.children.map(getNodeData);
      }

      return data;
    }

    // Get all root level nodes
    var rootNodes = $("#stic-menu-manager").jstree(true).get_node("#").children;
    var $cleanMenu = rootNodes.map(getNodeData);


    // Define data to be sent in the request
    var dataToSend = {
      menuJson: JSON.stringify($cleanMenu),
      sticAdvancedMenuIcons: document.getElementById("stic_advanced_menu_icons").checked ? "1" : "0",
      sticAdvancedMenuAll: document.getElementById("stic_advanced_menu_all").checked ? "1" : "0",
      manageMode: "save",
      manageLang: document.getElementById("grouptab_lang").value
    };

    // Perform AJAX request to save menu configuration
    $.ajax({
      url: location.href.slice(0, location.href.indexOf(location.search)),
      type: "POST",
      data: {
        module: "Administration",
        action: "SticAdvancedMenuController",
        ...dataToSend
      },
      success: function(response) {
        console.log("Response received:", response);
        location.reload(true);
      },
      error: function(xhr, status, error) {
        console.error("Request error:", status, error);
      }
    });
  });
  // Event handler for copied nodes from hidden-modules
  $("#stic-menu-manager").on("copy_node.jstree", function(e, data) {
    var original_node = data.original;
    var new_node = data.node;

    // Preserve original ID and text
    var tree = $("#stic-menu-manager").jstree(true);
    tree.set_id(new_node, original_node.id);

    handleTreeChanges();
  });

  // Tree modification event handlers
  $("#stic-menu-manager").on("rename_node.jstree", handleTreeChanges);
  $("#stic-menu-manager").on("move_node.jstree", handleTreeChanges);
  $("#stic-menu-manager").on("delete_node.jstree", handleTreeChanges);
  $("#stic-menu-manager").on("create_node.jstree", handleTreeChanges);

  // Options change handlers
  $("#stic_advanced_menu_icons").on("change", handleTreeChanges);
  $("#stic_advanced_menu_all").on("change", handleTreeChanges);

  // Auto-hide saved notification
  setTimeout(() => {
    $("#saved-notice").fadeOut(3000);
  }, 3000);

  /**
 * Handles node deletion from the menu tree.
 * Moves deleted nodes and their children to hidden-modules tree.
 */
  $("#stic-menu-manager").on("delete_node.jstree", function(e, data) {
    function getNodeDataArray(node) {
      var nodeDataArray = [];

      function addNodeToArray(n) {
        var nodeData = {
          id: n.id,
          text: n.text,
          parent: n.parent
        };

        if (n.data) {
          nodeData.data = n.data;
        }
        if (n.original) {
          nodeData.original = n.original;
        }

        nodeDataArray.push(nodeData);

        // Process node children
        if (n.children && n.children.length > 0) {
          n.children.forEach(function(childId) {
            var childNode = data.instance.get_node(childId);
            addNodeToArray(childNode);
          });
        }
      }

      addNodeToArray(node);
      return nodeDataArray;
    }

    var deletedNodesArray = getNodeDataArray(data.node);

    // Transfer nodes to hidden-modules
    deletedNodesArray.forEach(element => {
      if (element.id != "SinergiaDA") {
        addNewItemToHiddenModules({
          id: element.id,
          text: element.text
        });
      } else {
        setTimeout(() => {  addSdaToHidden(); }, 1000);
      }
    });

    console.log("Nodes deleted from stic-menu-manager:", deletedNodesArray);
  });
});

/**
 * Filters node properties to keep only essential data
 * @param {Object} node - Node to filter
 * @return {Object} Filtered node with only id, children, text and url
 */
function filterNodes(node) {
  if (typeof node === "object" && node !== null) {
    const keys = Object.keys(node);
    for (const key of keys) {
      if (key !== "id" && key !== "children" && key !== "text" && key !== "url") {
        delete node[key];
      } else if (key === "children") {
        node[key] = node[key].map(filterNodes);
        if (node[key].length === 0) {
          delete node[key];
        }
      }
    }
  }
  return node;
}
/**
 * Updates save button appearance when tree changes
 * @param {Event} e - Event object
 * @param {Object} data - Event data
 */
function handleTreeChanges(e, data) {
  console.log("Tree has been modified:", data);
  $("#save-menu .glyphicon")
    .addClass("glyphicon-save")
    .removeClass("glyphicon-ok")
    .addClass("text-danger")
    .removeClass("text-success");
}

/**
 * Creates and configures the jsTree menu
 */
function createMenu() {
  $("#stic-menu-manager").jstree({
    core: {
      data: menu[0].map(function processNode(node) {
        if (node.url) {
          node.a_attr = { title: node.url, "data-url": node.url };
        }
        if (node.children && node.children.length) {
          node.children = node.children.map(processNode);
        }
        return node;
      }),
      check_callback: function(operation, node, parent, position, more) {
        if (operation === "copy_node" || operation === "move_node") {
          node.url = node.original.url;
        }
        return true;
      },
      themes: {
        icons: false,
        dots: true
      }
    },
    plugins: ["dnd", "wholerow", "contextmenu", "types"],
    types: {
      default: {
        valid_children: ["default"]
      }
    },
    dnd: {
      copy: false
    },
    contextmenu: {
      items: function($node) {
        var tree = $("#stic-menu-manager").jstree(true);
        return {
          Create: {
            separator_before: false,
            separator_after: true,
            label:
              "<i class='glyphicon glyphicon-plus'></i>" +
              SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_CREATE,
            action: function(obj) {
              $node = tree.create_node($node, {
                id: "LBL_GROUPTAB_" + Math.floor(Date.now() / 1000),
                text: newNodeString,
                url: "",
                original: {
                  url: ""
                }
              });
              tree.edit($node);
            }
          },
          Rename: {
            separator_before: false,
            separator_after: true,
            label:
              "<i class='glyphicon glyphicon-pencil'></i>" +
              SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_RENAME,
            action: function(obj) {
              tree.edit($node);
            },
            _disabled: function(data) {
              return !($node.id.startsWith("LBL_") || $node.id === "SinergiaDA");
            }
          },
          Duplicate: {
            separator_before: false,
            separator_after: false,
            label:
              "<i class='glyphicon glyphicon-duplicate'></i>" +
              SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_DUPLICATE,
            action: function(obj) {
              var nodeData = tree.get_json($node, { no_state: true, no_id: false, no_children: false, no_data: false });

              function generateCustomId(baseId) {
                var randomSuffix = Math.floor(Date.now() / 1000);
                return baseId + "_" + randomSuffix;
              }

              function duplicateNodeRecursively(node) {
                var nodeCopy = $.extend(true, {}, node);

                nodeCopy.id = generateCustomId(node.id.replace(/_\d+$/, '_'));
                nodeCopy.text = node.text || "_";
                nodeCopy.url = node.url || (node.original && node.original.url) || "";
                nodeCopy.original = {
                  ...nodeCopy,
                  url: nodeCopy.url
                };

                if (nodeCopy.children && nodeCopy.children.length > 0) {
                  nodeCopy.children = nodeCopy.children.map(duplicateNodeRecursively);
                }

                return nodeCopy;
              }

              var duplicatedNode = duplicateNodeRecursively(nodeData);
              var siblings = tree.get_children_dom(tree.get_parent($node));
              var currentIndex = siblings.index(tree.get_node($node, true));
              var newNodeId = tree.create_node(tree.get_parent($node), duplicatedNode, currentIndex + 1);

              if (newNodeId) {
                tree.deselect_all();
                tree.select_node(newNodeId);
                handleTreeChanges();
              }
            }
          },
          EditURL: {
            separator_before: false,
            separator_after: true,
            label:
              "<i class='glyphicon glyphicon-link'></i>" +
              SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_EDITURL,
            action: function(obj) {
              var editUrlPrompt = function() {
                var urlOld = tree.get_node($node).a_attr["data-url"] || "";
                var url = prompt(
                  SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_EDITURL_PROMPT + ":",
                  urlOld
                );
                if (url !== null) {
                  var node = tree.get_node($node);
                  if (url === "") {
                    delete node.a_attr["data-url"];
                    delete node.a_attr["title"];
                    $("#" + node.id + " a").removeAttr("title").removeAttr("data-url");
                  } else if (isValidUrl(url)) {
                    node.url = url;
                    node.a_attr = { title: node.url, "data-url": node.url };
                    $("#" + node.id + " a:first").attr("title", url).attr("data-url", url);
                  } else {
                    alert(SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_EDITURL_PROMPT_VALIDATE);
                    editUrlPrompt();
                  }
                }
              };
              editUrlPrompt();
            },
            _disabled: function(data) {
              return !$node.id.startsWith("LBL_");
            }
          },
          Delete: {
            separator_before: false,
            separator_after: false,
            label:
              "<i class='glyphicon glyphicon-remove'></i>" +
              SUGAR.language.languages.Administration.LBL_STIC_MENU_COMMAND_REMOVE,
            action: function(obj) {
              if (tree.is_selected($node)) {
                tree.delete_node(tree.get_selected());
              } else {
                tree.delete_node($node);
              }
            }
          }
        };
      }
    }
  });

  // Create jsTree for hidden modules
  $("#hidden-modules").jstree({
      core: {
        data: allModules[0],
        themes: {
          icons: false
        },
        check_callback: function(operation, node, node_parent, node_position, more) {
          if (operation === "move_node") {
            // Prevent any movement inside the tree
            return false;
          }
          // Allow other operations (such as copying out)
          return true;
        }
      },
      plugins: ["dnd", "wholerow", "search", "unique"],
      dnd: {
        is_draggable: function(nodes) {
          // Allow to drag nodes out, but not inside the tree
          return true;
        },
        copy: false, // Allow to copy nodes instead of moving them
        always_copy: false // Always copy, never move
      }
    })
    .on("ready.jstree", function() {
      addSdaToHidden();
    });
}

/**
 * Adds SinergiaDA to the hidden module if it doesn't exist in the main tree.
 * 
 */
function addSdaToHidden() {
  // Check if SinergiaDA exists in the main tree
  var mainTree = $("#stic-menu-manager").jstree(true);
  var sinergiaDAExists = mainTree && mainTree.get_node("SinergiaDA");

  // If it does not exist in the main tree, we add it to Hidden-Modules
  if (!sinergiaDAExists) {
    addNewItemToHiddenModules({
      id: "SinergiaDA",
      text: "SinergiaDA",
      url: "index.php?module=Home&action=sdaRedirect",
      a_attr: {
        href: "index.php?module=Home&action=sdaRedirect",
        "data-url": "index.php?module=Home&action=sdaRedirect",
        id: "SinergiaDA_anchor",
        title: "index.php?module=Home&action=sdaRedirect"
      }
    });
  }
}

/**
 * Remove existing jsTree menu
 */
function removeMenu() {
  var tree = $("#stic-menu-manager").jstree(true);
  if (tree) {
    tree.destroy();
  }
}

/**
 * Updates node display with text and URL properties
 * @param {string} nodeId - ID of the node to update
 */
function updateNodeDisplay(nodeId) {
  var tree = $("#stic-menu-manager").jstree(true);
  var nodeObj = tree.get_node(nodeId);
  if (nodeObj) {
    var displayText = nodeObj.text ? nodeObj.text.split("|")[0] : "Untitled Node";
    var url = nodeObj.text ? nodeObj.text.split("|")[1] : null;

    if (url) {
      nodeObj.url = url;
      $("#" + nodeObj.id).attr("title", url);
    }
    tree.rename_node(nodeId, displayText);
  }
}

/**
 * Validates if a string is a valid URL
 * @param {string} string - String to validate
 * @returns {boolean} True if valid URL, false otherwise
 */
function isValidUrl(string) {
  try {
    new URL(string);
    return true;
  } catch (_) {
    return false;
  }
}

/**
 * Recursively removes disabled objects from array
 * @param {Array} arr - Array to process
 * @returns {Array} Processed array with disabled items removed
 */
function removeDisabledObjects(arr) {
  for (let i = arr.length - 1; i >= 0; i--) {
    let element = arr[i];

    if (Array.isArray(element)) {
      removeDisabledObjects(element);
      if (element.length === 0) {
        arr.splice(i, 1);
      }
    } else if (typeof element === "object" && element !== null) {
      if (element.hasOwnProperty("disabled") && element.disabled === true) {
        arr.splice(i, 1);
        continue;
      }
      for (let prop in element) {
        if (Array.isArray(element[prop])) {
          removeDisabledObjects(element[prop]);
          if (element[prop].length === 0) {
            delete element[prop];
          }
        } else if (typeof element[prop] === "object" && element[prop] !== null) {
          if (removeDisabledObjects([element[prop]]).length === 0) {
            delete element[prop];
          }
        }
      }
    }
  }
  return arr;
}
/**
 * Creates a new main node in the jsTree
 */
function newMainNode() {
  var tree = $("#stic-menu-manager").jstree(true);
  var text = newNodeString;
  var newNode = {
    id: "LBL_GROUPTAB_" + Math.floor(Date.now() / 1000),
    text: text
  };

  tree.create_node("#", newNode, "last");
}

/**
 * Expands all nodes in the jsTree
 */
function expandAll() {
  var $tree = $("#stic-menu-manager");
  if ($tree.length) {
    try {
      $tree.jstree("open_all");
      console.log("Tree fully expanded");
    } catch (error) {
      console.error("Error expanding the tree:", error);
    }
  } else {
    console.warn("jsTree not found");
  }
}

/**
 * Collapses all nodes in the jsTree
 */
function collapseAll() {
  var $tree = $("#stic-menu-manager");
  if ($tree.length) {
    try {
      $tree.jstree("close_all");
      console.log("Tree fully collapsed");
    } catch (error) {
      console.error("Error collapsing the tree:", error);
    }
  } else {
    console.warn("jsTree not found");
  }
}
/**
 * Adds a new item to the hidden modules tree
 * @param {Object} nodeData - Node data object
 * @param {string} nodeData.id - Node identifier
 * @param {string} nodeData.text - Node display text
 * @param {string} [nodeData.url] - Optional node URL
 * @returns {string|null} New node ID or null if creation fails
 */
function addNewItemToHiddenModules(nodeData) {
  if (!nodeData.id || !nodeData.text) {
    console.error("Required properties missing");
    return null;
  }

  var hiddenModulesTree = $.jstree.reference("#hidden-modules");
  if (!hiddenModulesTree) {
    console.error("Hidden modules tree not found");
    return null;
  }

  if (hiddenModulesTree.get_node(nodeData.id)) {
    return null;
  }

  var newNode = hiddenModulesTree.create_node("#", {
    id: nodeData.id,
    text: nodeData.text,
    url: nodeData.url,
    data: nodeData,
    original: nodeData,
    a_attr: nodeData.a_attr
  });

  if (newNode) {
    // Apply highlight effect
    setTimeout(function() {
      var $node = $("#" + newNode);
      $node.css({
        transition: "background-color 0.5s ease",
        "background-color": "#B5BC31"
      });
      setTimeout(function() {
        $node.css({
          transition: "background-color 1s ease",
          "background-color": ""
        });
      }, 300);
    }, 100);

    sortHiddenModulesAlphabetically();
    return newNode;
  }

  console.error("Node creation failed");
  return null;
}
/**
 * Sorts nodes in hidden modules tree alphabetically
 */
function sortHiddenModulesAlphabetically() {
  var hiddenModulesTree = $.jstree.reference("#hidden-modules");
  if (!hiddenModulesTree) {
    console.error("Hidden modules tree not found");
    return;
  }

  var topLevelNodes = hiddenModulesTree.get_node("#").children;

  topLevelNodes.sort(function(a, b) {
    var nodeA = hiddenModulesTree.get_node(a);
    var nodeB = hiddenModulesTree.get_node(b);
    var textA = nodeA.text.trim().toLowerCase();
    var textB = nodeB.text.trim().toLowerCase();
    return textA.localeCompare(textB);
  });

  topLevelNodes.forEach((node, index) => {
    hiddenModulesTree.move_node(node, "#", index);
  });

  hiddenModulesTree.redraw(true);
}

/**
 * Updates language parameter in URL
 * @param {HTMLSelectElement} sel - Language dropdown element
 */
function tabLanguageChange(sel) {
  var partURL = window.location.href;
  if (partURL.search(/&lang=\w*&/i) != -1) {
    partURL = partURL.replace(/&lang=\w*&/i, "&lang=" + sel.value + "&");
  } else if (partURL.search(/&lang=\w*/i) != -1) {
    partURL = partURL.replace(/&lang=\w*/i, "&lang=" + sel.value);
  } else {
    partURL = window.location.href + "&lang=" + sel.value;
  }
  window.location.href = partURL;
}
