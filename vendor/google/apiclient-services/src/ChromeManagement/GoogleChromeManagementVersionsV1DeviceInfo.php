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

class GoogleChromeManagementVersionsV1DeviceInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $affiliatedDeviceId;
  /**
   * @var string
   */
  public $deviceType;
  /**
   * @var string
   */
  public $hostname;
  /**
   * @var string
   */
  public $machine;

  /**
   * @param string
   */
  public function setAffiliatedDeviceId($affiliatedDeviceId)
  {
    $this->affiliatedDeviceId = $affiliatedDeviceId;
  }
  /**
   * @return string
   */
  public function getAffiliatedDeviceId()
  {
    return $this->affiliatedDeviceId;
  }
  /**
   * @param string
   */
  public function setDeviceType($deviceType)
  {
    $this->deviceType = $deviceType;
  }
  /**
   * @return string
   */
  public function getDeviceType()
  {
    return $this->deviceType;
  }
  /**
   * @param string
   */
  public function setHostname($hostname)
  {
    $this->hostname = $hostname;
  }
  /**
   * @return string
   */
  public function getHostname()
  {
    return $this->hostname;
  }
  /**
   * @param string
   */
  public function setMachine($machine)
  {
    $this->machine = $machine;
  }
  /**
   * @return string
   */
  public function getMachine()
  {
    return $this->machine;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementVersionsV1DeviceInfo::class, 'Google_Service_ChromeManagement_GoogleChromeManagementVersionsV1DeviceInfo');
