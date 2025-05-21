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

class GoogleAppsCardV1CollapseControl extends \Google\Model
{
  protected $collapseButtonType = GoogleAppsCardV1Button::class;
  protected $collapseButtonDataType = '';
  protected $expandButtonType = GoogleAppsCardV1Button::class;
  protected $expandButtonDataType = '';
  /**
   * @var string
   */
  public $horizontalAlignment;

  /**
   * @param GoogleAppsCardV1Button
   */
  public function setCollapseButton(GoogleAppsCardV1Button $collapseButton)
  {
    $this->collapseButton = $collapseButton;
  }
  /**
   * @return GoogleAppsCardV1Button
   */
  public function getCollapseButton()
  {
    return $this->collapseButton;
  }
  /**
   * @param GoogleAppsCardV1Button
   */
  public function setExpandButton(GoogleAppsCardV1Button $expandButton)
  {
    $this->expandButton = $expandButton;
  }
  /**
   * @return GoogleAppsCardV1Button
   */
  public function getExpandButton()
  {
    return $this->expandButton;
  }
  /**
   * @param string
   */
  public function setHorizontalAlignment($horizontalAlignment)
  {
    $this->horizontalAlignment = $horizontalAlignment;
  }
  /**
   * @return string
   */
  public function getHorizontalAlignment()
  {
    return $this->horizontalAlignment;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCardV1CollapseControl::class, 'Google_Service_HangoutsChat_GoogleAppsCardV1CollapseControl');
