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

namespace Google\Service\Testing;

class PerAndroidVersionInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $deviceCapacity;
  protected $directAccessVersionInfoType = DirectAccessVersionInfo::class;
  protected $directAccessVersionInfoDataType = '';
  /**
   * @var string
   */
  public $interactiveDeviceAvailabilityEstimate;
  /**
   * @var string
   */
  public $versionId;

  /**
   * @param string
   */
  public function setDeviceCapacity($deviceCapacity)
  {
    $this->deviceCapacity = $deviceCapacity;
  }
  /**
   * @return string
   */
  public function getDeviceCapacity()
  {
    return $this->deviceCapacity;
  }
  /**
   * @param DirectAccessVersionInfo
   */
  public function setDirectAccessVersionInfo(DirectAccessVersionInfo $directAccessVersionInfo)
  {
    $this->directAccessVersionInfo = $directAccessVersionInfo;
  }
  /**
   * @return DirectAccessVersionInfo
   */
  public function getDirectAccessVersionInfo()
  {
    return $this->directAccessVersionInfo;
  }
  /**
   * @param string
   */
  public function setInteractiveDeviceAvailabilityEstimate($interactiveDeviceAvailabilityEstimate)
  {
    $this->interactiveDeviceAvailabilityEstimate = $interactiveDeviceAvailabilityEstimate;
  }
  /**
   * @return string
   */
  public function getInteractiveDeviceAvailabilityEstimate()
  {
    return $this->interactiveDeviceAvailabilityEstimate;
  }
  /**
   * @param string
   */
  public function setVersionId($versionId)
  {
    $this->versionId = $versionId;
  }
  /**
   * @return string
   */
  public function getVersionId()
  {
    return $this->versionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PerAndroidVersionInfo::class, 'Google_Service_Testing_PerAndroidVersionInfo');
