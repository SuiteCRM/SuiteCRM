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

class XPSColorMapIntColor extends \Google\Model
{
  /**
   * @var int
   */
  public $blue;
  /**
   * @var int
   */
  public $green;
  /**
   * @var int
   */
  public $red;

  /**
   * @param int
   */
  public function setBlue($blue)
  {
    $this->blue = $blue;
  }
  /**
   * @return int
   */
  public function getBlue()
  {
    return $this->blue;
  }
  /**
   * @param int
   */
  public function setGreen($green)
  {
    $this->green = $green;
  }
  /**
   * @return int
   */
  public function getGreen()
  {
    return $this->green;
  }
  /**
   * @param int
   */
  public function setRed($red)
  {
    $this->red = $red;
  }
  /**
   * @return int
   */
  public function getRed()
  {
    return $this->red;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSColorMapIntColor::class, 'Google_Service_CloudNaturalLanguage_XPSColorMapIntColor');
