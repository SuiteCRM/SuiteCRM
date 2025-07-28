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
var sticCV_Element_Div = class sticCV_Element_Div {
  constructor(customView, $element) {
    this.customView = customView;
    this.$element = $element;
  }

  exists() {
    return (this.$element?.length??0) > 0;
  }

  show(show = true) {
    return this.applyAction({ action: "visible", value: show });
  }
  hide() {
    return this.show(false);
  }

  color(color = "") {
    return this.applyAction({ action: "color", value: color });
  }
  background(color = "") {
    return this.applyAction({ action: "background", value: color });
  }

  bold(bold = true) {
    return this.applyAction({ action: "bold", value: bold });
  }
  italic(italic = true) {
    return this.applyAction({ action: "italic", value: italic });
  }
  underline(underline = true) {
    return this.applyAction({ action: "underline", value: underline });
  }

  style(style) {
    return this.applyAction({ action: "style", value: style });
  }

  frame(frame = true) {
    return this.applyAction({ action: "frame", value: frame });
  }

  applyAction(action) {
    switch (action.action) {
      case "visible":
        sticCVUtils.show(this.$element, this.customView, action.value);
        return this;
      case "visible_auto":
        sticCVUtils.show_auto(this.$element, this.customView, action.value);
        return this;
      case "color":
        sticCVUtils.color(this.$element, this.customView, action.value);
        return this;
      case "background":
        sticCVUtils.background(this.$element, this.customView, action.value);
        return this;
      case "bold":
        sticCVUtils.bold(this.$element, this.customView, action.value);
        return this;
      case "italic":
        sticCVUtils.italic(this.$element, this.customView, action.value);
        return this;
      case "underline":
        sticCVUtils.underline(this.$element, this.customView, action.value);
        return this;
      case "css_style":
        return this.applyAction({ action: "style", value: JSON.parse(action.value) });
      case "style":
        sticCVUtils.style(this.$element, this.customView, action.value);
        return this;
      case "frame":
        sticCVUtils.frame(this.$element, this.customView, action.value);
        return this;
    }
    return false;
  }

  onChange(callback) {
    return sticCVUtils.onChange(this.$element, callback);
  }
  change() {
    return sticCVUtils.change(this.$element, callback);
  }
};
