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

class InputFieldTextInput extends \Google\Model
{
  protected $additionalInfoType = TextWithTooltip::class;
  protected $additionalInfoDataType = '';
  /**
   * @var string
   */
  public $ariaLabel;
  /**
   * @var string
   */
  public $formatInfo;
  /**
   * @var string
   */
  public $type;

  /**
   * @param TextWithTooltip
   */
  public function setAdditionalInfo(TextWithTooltip $additionalInfo)
  {
    $this->additionalInfo = $additionalInfo;
  }
  /**
   * @return TextWithTooltip
   */
  public function getAdditionalInfo()
  {
    return $this->additionalInfo;
  }
  /**
   * @param string
   */
  public function setAriaLabel($ariaLabel)
  {
    $this->ariaLabel = $ariaLabel;
  }
  /**
   * @return string
   */
  public function getAriaLabel()
  {
    return $this->ariaLabel;
  }
  /**
   * @param string
   */
  public function setFormatInfo($formatInfo)
  {
    $this->formatInfo = $formatInfo;
  }
  /**
   * @return string
   */
  public function getFormatInfo()
  {
    return $this->formatInfo;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InputFieldTextInput::class, 'Google_Service_ShoppingContent_InputFieldTextInput');
