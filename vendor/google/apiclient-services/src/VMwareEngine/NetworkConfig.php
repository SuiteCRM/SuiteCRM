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

namespace Google\Service\VMwareEngine;

class NetworkConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $dnsServerIp;
  /**
   * @var string
   */
  public $managementCidr;
  /**
   * @var int
   */
  public $managementIpAddressLayoutVersion;
  /**
   * @var string
   */
  public $vmwareEngineNetwork;
  /**
   * @var string
   */
  public $vmwareEngineNetworkCanonical;

  /**
   * @param string
   */
  public function setDnsServerIp($dnsServerIp)
  {
    $this->dnsServerIp = $dnsServerIp;
  }
  /**
   * @return string
   */
  public function getDnsServerIp()
  {
    return $this->dnsServerIp;
  }
  /**
   * @param string
   */
  public function setManagementCidr($managementCidr)
  {
    $this->managementCidr = $managementCidr;
  }
  /**
   * @return string
   */
  public function getManagementCidr()
  {
    return $this->managementCidr;
  }
  /**
   * @param int
   */
  public function setManagementIpAddressLayoutVersion($managementIpAddressLayoutVersion)
  {
    $this->managementIpAddressLayoutVersion = $managementIpAddressLayoutVersion;
  }
  /**
   * @return int
   */
  public function getManagementIpAddressLayoutVersion()
  {
    return $this->managementIpAddressLayoutVersion;
  }
  /**
   * @param string
   */
  public function setVmwareEngineNetwork($vmwareEngineNetwork)
  {
    $this->vmwareEngineNetwork = $vmwareEngineNetwork;
  }
  /**
   * @return string
   */
  public function getVmwareEngineNetwork()
  {
    return $this->vmwareEngineNetwork;
  }
  /**
   * @param string
   */
  public function setVmwareEngineNetworkCanonical($vmwareEngineNetworkCanonical)
  {
    $this->vmwareEngineNetworkCanonical = $vmwareEngineNetworkCanonical;
  }
  /**
   * @return string
   */
  public function getVmwareEngineNetworkCanonical()
  {
    return $this->vmwareEngineNetworkCanonical;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkConfig::class, 'Google_Service_VMwareEngine_NetworkConfig');
