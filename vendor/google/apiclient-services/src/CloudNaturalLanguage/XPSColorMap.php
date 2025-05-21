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

namespace Google\Service\CloudNaturalLanguage;

class XPSColorMap extends \Google\Model
{
  /**
   * @var string
   */
  public $annotationSpecIdToken;
  protected $colorType = Color::class;
  protected $colorDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $intColorType = XPSColorMapIntColor::class;
  protected $intColorDataType = '';

  /**
   * @param string
   */
  public function setAnnotationSpecIdToken($annotationSpecIdToken)
  {
    $this->annotationSpecIdToken = $annotationSpecIdToken;
  }
  /**
   * @return string
   */
  public function getAnnotationSpecIdToken()
  {
    return $this->annotationSpecIdToken;
  }
  /**
   * @param Color
   */
  public function setColor(Color $color)
  {
    $this->color = $color;
  }
  /**
   * @return Color
   */
  public function getColor()
  {
    return $this->color;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param XPSColorMapIntColor
   */
  public function setIntColor(XPSColorMapIntColor $intColor)
  {
    $this->intColor = $intColor;
  }
  /**
   * @return XPSColorMapIntColor
   */
  public function getIntColor()
  {
    return $this->intColor;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSColorMap::class, 'Google_Service_CloudNaturalLanguage_XPSColorMap');
