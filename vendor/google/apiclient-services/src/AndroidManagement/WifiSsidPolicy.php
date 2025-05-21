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

class WifiSsidPolicy extends \Google\Collection
{
  protected $collection_key = 'wifiSsids';
  /**
   * @var string
   */
  public $wifiSsidPolicyType;
  protected $wifiSsidsType = WifiSsid::class;
  protected $wifiSsidsDataType = 'array';

  /**
   * @param string
   */
  public function setWifiSsidPolicyType($wifiSsidPolicyType)
  {
    $this->wifiSsidPolicyType = $wifiSsidPolicyType;
  }
  /**
   * @return string
   */
  public function getWifiSsidPolicyType()
  {
    return $this->wifiSsidPolicyType;
  }
  /**
   * @param WifiSsid[]
   */
  public function setWifiSsids($wifiSsids)
  {
    $this->wifiSsids = $wifiSsids;
  }
  /**
   * @return WifiSsid[]
   */
  public function getWifiSsids()
  {
    return $this->wifiSsids;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WifiSsidPolicy::class, 'Google_Service_AndroidManagement_WifiSsidPolicy');
