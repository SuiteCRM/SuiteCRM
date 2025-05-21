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

class Callout extends \Google\Model
{
  protected $fullMessageType = TextWithTooltip::class;
  protected $fullMessageDataType = '';
  /**
   * @var string
   */
  public $styleHint;

  /**
   * @param TextWithTooltip
   */
  public function setFullMessage(TextWithTooltip $fullMessage)
  {
    $this->fullMessage = $fullMessage;
  }
  /**
   * @return TextWithTooltip
   */
  public function getFullMessage()
  {
    return $this->fullMessage;
  }
  /**
   * @param string
   */
  public function setStyleHint($styleHint)
  {
    $this->styleHint = $styleHint;
  }
  /**
   * @return string
   */
  public function getStyleHint()
  {
    return $this->styleHint;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Callout::class, 'Google_Service_ShoppingContent_Callout');
