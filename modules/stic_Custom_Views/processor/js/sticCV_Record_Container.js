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

/**
 * This file contains logic and functions needed to manage custom views behaviour
 *
 */

var sticCV_Record_Container = class sticCV_Record_Container {
  constructor(customView, name) {
    this.customView = customView;

    this.name = name;

    this.container = null;
    this.header = null;
    this.content = null;
  }

  exists() {
    return (this.container?.exists()??false) && (this.header?.exists()??false) && (this.content?.exists()??false);
  }

  show(show = true, automatically = false) {
    if (automatically) {
      return this.applyAction({ action: "visible_auto", value: show, element_section: "container" });
    } else {
      return this.applyAction({ action: "visible", value: show, element_section: "container" });
    }
  }
  hide(automatically = false) {
    return this.show(false, automatically);
  }

  style(style) {
    return this.applyAction({ action: "css_style", value: style, element_section: "container" });
  }

  color(color = "") {
    return this.applyAction({ action: "color", value: color, element_section: "container" });
  }
  background(color = "") {
    return this.applyAction({ action: "background", value: color, element_section: "container" });
  }

  applyAction(action) {
    switch (action.element_section) {
      case "tab_header":
      case "panel_header":
      case "field_label":
      case "header":
        return this.header ? this.header.applyAction(action) : null;
      case "tab_content":
      case "panel_content":
      case "field_input":
      case "content":
        return this.content ? this.content.applyAction(action) : null;
      case "tab":
      case "panel":
      case "field":
      case "container":
        this.container && this.container.applyAction(action);
        this.header && this.header.applyAction(action);
        this.content && this.content.applyAction(action);
        return this;
    }
    return false;
  }
};
