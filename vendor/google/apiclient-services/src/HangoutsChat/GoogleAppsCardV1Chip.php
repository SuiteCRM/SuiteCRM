<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\HangoutsChat;

class GoogleAppsCardV1Chip extends \Google\Model
{
  /**
   * @var string
   */
  public $altText;
  /**
   * @var bool
   */
  public $disabled;
  /**
   * @var bool
   */
  public $enabled;
  protected $iconType = GoogleAppsCardV1Icon::class;
  protected $iconDataType = '';
  /**
   * @var string
   */
  public $label;
  protected $onClickType = GoogleAppsCardV1OnClick::class;
  protected $onClickDataType = '';

  /**
   * @param string
   */
  public function setAltText($altText)
  {
    $this->altText = $altText;
  }
  /**
   * @return string
   */
  public function getAltText()
  {
    return $this->altText;
  }
  /**
   * @param bool
   */
  public function setDisabled($disabled)
  {
    $this->disabled = $disabled;
  }
  /**
   * @return bool
   */
  public function getDisabled()
  {
    return $this->disabled;
  }
  /**
   * @param bool
   */
  public function setEnabled($enabled)
  {
    $this->enabled = $enabled;
  }
  /**
   * @return bool
   */
  public function getEnabled()
  {
    return $this->enabled;
  }
  /**
   * @param GoogleAppsCardV1Icon
   */
  public function setIcon(GoogleAppsCardV1Icon $icon)
  {
    $this->icon = $icon;
  }
  /**
   * @return GoogleAppsCardV1Icon
   */
  public function getIcon()
  {
    return $this->icon;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
  }
  /**
   * @param GoogleAppsCardV1OnClick
   */
  public function setOnClick(GoogleAppsCardV1OnClick $onClick)
  {
    $this->onClick = $onClick;
  }
  /**
   * @return GoogleAppsCardV1OnClick
   */
  public function getOnClick()
  {
    return $this->onClick;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCardV1Chip::class, 'Google_Service_HangoutsChat_GoogleAppsCardV1Chip');
