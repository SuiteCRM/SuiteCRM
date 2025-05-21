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

namespace Google\Service\AndroidManagement;

class DeviceRadioState extends \Google\Model
{
  /**
   * @var string
   */
  public $airplaneModeState;
  /**
   * @var string
   */
  public $cellularTwoGState;
  /**
   * @var string
   */
  public $minimumWifiSecurityLevel;
  /**
   * @var string
   */
  public $ultraWidebandState;
  /**
   * @var string
   */
  public $wifiState;

  /**
   * @param string
   */
  public function setAirplaneModeState($airplaneModeState)
  {
    $this->airplaneModeState = $airplaneModeState;
  }
  /**
   * @return string
   */
  public function getAirplaneModeState()
  {
    return $this->airplaneModeState;
  }
  /**
   * @param string
   */
  public function setCellularTwoGState($cellularTwoGState)
  {
    $this->cellularTwoGState = $cellularTwoGState;
  }
  /**
   * @return string
   */
  public function getCellularTwoGState()
  {
    return $this->cellularTwoGState;
  }
  /**
   * @param string
   */
  public function setMinimumWifiSecurityLevel($minimumWifiSecurityLevel)
  {
    $this->minimumWifiSecurityLevel = $minimumWifiSecurityLevel;
  }
  /**
   * @return string
   */
  public function getMinimumWifiSecurityLevel()
  {
    return $this->minimumWifiSecurityLevel;
  }
  /**
   * @param string
   */
  public function setUltraWidebandState($ultraWidebandState)
  {
    $this->ultraWidebandState = $ultraWidebandState;
  }
  /**
   * @return string
   */
  public function getUltraWidebandState()
  {
    return $this->ultraWidebandState;
  }
  /**
   * @param string
   */
  public function setWifiState($wifiState)
  {
    $this->wifiState = $wifiState;
  }
  /**
   * @return string
   */
  public function getWifiState()
  {
    return $this->wifiState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeviceRadioState::class, 'Google_Service_AndroidManagement_DeviceRadioState');
