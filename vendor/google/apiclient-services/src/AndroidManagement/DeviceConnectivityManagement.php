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

class DeviceConnectivityManagement extends \Google\Model
{
  /**
   * @var string
   */
  public $configureWifi;
  /**
   * @var string
   */
  public $tetheringSettings;
  /**
   * @var string
   */
  public $usbDataAccess;
  /**
   * @var string
   */
  public $wifiDirectSettings;
  protected $wifiRoamingPolicyType = WifiRoamingPolicy::class;
  protected $wifiRoamingPolicyDataType = '';
  protected $wifiSsidPolicyType = WifiSsidPolicy::class;
  protected $wifiSsidPolicyDataType = '';

  /**
   * @param string
   */
  public function setConfigureWifi($configureWifi)
  {
    $this->configureWifi = $configureWifi;
  }
  /**
   * @return string
   */
  public function getConfigureWifi()
  {
    return $this->configureWifi;
  }
  /**
   * @param string
   */
  public function setTetheringSettings($tetheringSettings)
  {
    $this->tetheringSettings = $tetheringSettings;
  }
  /**
   * @return string
   */
  public function getTetheringSettings()
  {
    return $this->tetheringSettings;
  }
  /**
   * @param string
   */
  public function setUsbDataAccess($usbDataAccess)
  {
    $this->usbDataAccess = $usbDataAccess;
  }
  /**
   * @return string
   */
  public function getUsbDataAccess()
  {
    return $this->usbDataAccess;
  }
  /**
   * @param string
   */
  public function setWifiDirectSettings($wifiDirectSettings)
  {
    $this->wifiDirectSettings = $wifiDirectSettings;
  }
  /**
   * @return string
   */
  public function getWifiDirectSettings()
  {
    return $this->wifiDirectSettings;
  }
  /**
   * @param WifiRoamingPolicy
   */
  public function setWifiRoamingPolicy(WifiRoamingPolicy $wifiRoamingPolicy)
  {
    $this->wifiRoamingPolicy = $wifiRoamingPolicy;
  }
  /**
   * @return WifiRoamingPolicy
   */
  public function getWifiRoamingPolicy()
  {
    return $this->wifiRoamingPolicy;
  }
  /**
   * @param WifiSsidPolicy
   */
  public function setWifiSsidPolicy(WifiSsidPolicy $wifiSsidPolicy)
  {
    $this->wifiSsidPolicy = $wifiSsidPolicy;
  }
  /**
   * @return WifiSsidPolicy
   */
  public function getWifiSsidPolicy()
  {
    return $this->wifiSsidPolicy;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeviceConnectivityManagement::class, 'Google_Service_AndroidManagement_DeviceConnectivityManagement');
