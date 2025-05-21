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

namespace Google\Service\ShoppingContent;

class TextWithTooltip extends \Google\Model
{
  /**
   * @var string
   */
  public $simpleTooltipValue;
  /**
   * @var string
   */
  public $simpleValue;
  /**
   * @var string
   */
  public $tooltipIconStyle;

  /**
   * @param string
   */
  public function setSimpleTooltipValue($simpleTooltipValue)
  {
    $this->simpleTooltipValue = $simpleTooltipValue;
  }
  /**
   * @return string
   */
  public function getSimpleTooltipValue()
  {
    return $this->simpleTooltipValue;
  }
  /**
   * @param string
   */
  public function setSimpleValue($simpleValue)
  {
    $this->simpleValue = $simpleValue;
  }
  /**
   * @return string
   */
  public function getSimpleValue()
  {
    return $this->simpleValue;
  }
  /**
   * @param string
   */
  public function setTooltipIconStyle($tooltipIconStyle)
  {
    $this->tooltipIconStyle = $tooltipIconStyle;
  }
  /**
   * @return string
   */
  public function getTooltipIconStyle()
  {
    return $this->tooltipIconStyle;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TextWithTooltip::class, 'Google_Service_ShoppingContent_TextWithTooltip');
