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
  // Initialize SmartMenus plugin with specific offsets
  $("#stic-menu").smartmenus({
    subMenusSubOffsetX: 1,
    subMenusSubOffsetY: -8,
    showTimeout: 100
  });

  // Update the menu when the first item is clicked (presumably the menu toggle)
  $("#stic-menu > li:first-child > a").on("click", function(e) {
    e.preventDefault();
    updateActionMenu();
  });

  // Update the menu on initial page load
  updateActionMenu();
});

/**
 * Builds the action menu by cloning elements from the sidebar
 * @returns {jQuery} A new unordered list containing the action menu items
 */
function buildActionMenu() {
  var $newActionMenu = $("<ul>");

  // Iterate through each action menu link in the sidebar
  $("#actionMenuSidebar ul li.actionmenulinks").each(function() {
    var $originalLink = $(this).find("a");
    var $icon = $originalLink.find(".side-bar-action-icon span").clone();
    var linkText = $originalLink.find(".actionmenulink").text();

    // Create a new link with the same attributes as the original
    var $newLink = $("<a>")
      .attr("href", $originalLink.attr("href"))
      .attr("data-action-name", $originalLink.data("action-name"))
      .addClass("link-cloned");

    // Append the icon and text to the new link
    $newLink.append($icon).append(linkText);

    // Wrap the new link in a list item and add it to the menu
    var $newListItem = $("<li>").append($newLink);
    $newActionMenu.append($newListItem);
  });

  return $newActionMenu;
}

/**
 * Updates the action menu in the main navigation
 * This function rebuilds the action menu and inserts it into the appropriate area
 */
function updateActionMenu() {
  var $newActionMenu = buildActionMenu();
  var $actionsArea = $("#stic-menu #actions-area");

  // Insert the new action menu at the beginning of the actions area, checking that the action is not being repeated
  if ($("#stic-menu #actions-area .link-cloned").length == 0) {
    $actionsArea.prepend($newActionMenu.children());
  }
  // If there are no action items, reduce the opacity of the entire section
  if ($actionsArea.find("a").length == 0) {
    $actionsArea.closest("li").css("opacity", 0.2);
  }
}