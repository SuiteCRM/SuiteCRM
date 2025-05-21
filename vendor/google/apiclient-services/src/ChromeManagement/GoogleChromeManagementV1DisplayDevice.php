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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementV1DisplayDevice extends \Google\Model
{
  /**
   * @var int
   */
  public $displayHeightMm;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var int
   */
  public $displayWidthMm;
  /**
   * @var bool
   */
  public $internal;
  /**
   * @var int
   */
  public $manufactureYear;
  /**
   * @var string
   */
  public $manufacturerId;
  /**
   * @var int
   */
  public $modelId;

  /**
   * @param int
   */
  public function setDisplayHeightMm($displayHeightMm)
  {
    $this->displayHeightMm = $displayHeightMm;
  }
  /**
   * @return int
   */
  public function getDisplayHeightMm()
  {
    return $this->displayHeightMm;
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
   * @param int
   */
  public function setDisplayWidthMm($displayWidthMm)
  {
    $this->displayWidthMm = $displayWidthMm;
  }
  /**
   * @return int
   */
  public function getDisplayWidthMm()
  {
    return $this->displayWidthMm;
  }
  /**
   * @param bool
   */
  public function setInternal($internal)
  {
    $this->internal = $internal;
  }
  /**
   * @return bool
   */
  public function getInternal()
  {
    return $this->internal;
  }
  /**
   * @param int
   */
  public function setManufactureYear($manufactureYear)
  {
    $this->manufactureYear = $manufactureYear;
  }
  /**
   * @return int
   */
  public function getManufactureYear()
  {
    return $this->manufactureYear;
  }
  /**
   * @param string
   */
  public function setManufacturerId($manufacturerId)
  {
    $this->manufacturerId = $manufacturerId;
  }
  /**
   * @return string
   */
  public function getManufacturerId()
  {
    return $this->manufacturerId;
  }
  /**
   * @param int
   */
  public function setModelId($modelId)
  {
    $this->modelId = $modelId;
  }
  /**
   * @return int
   */
  public function getModelId()
  {
    return $this->modelId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementV1DisplayDevice::class, 'Google_Service_ChromeManagement_GoogleChromeManagementV1DisplayDevice');
