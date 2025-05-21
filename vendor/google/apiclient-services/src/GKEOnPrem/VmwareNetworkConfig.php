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

namespace Google\Service\GKEOnPrem;

class VmwareNetworkConfig extends \Google\Collection
{
  protected $collection_key = 'serviceAddressCidrBlocks';
  protected $controlPlaneV2ConfigType = VmwareControlPlaneV2Config::class;
  protected $controlPlaneV2ConfigDataType = '';
  protected $dhcpIpConfigType = VmwareDhcpIpConfig::class;
  protected $dhcpIpConfigDataType = '';
  protected $hostConfigType = VmwareHostConfig::class;
  protected $hostConfigDataType = '';
  /**
   * @var string[]
   */
  public $podAddressCidrBlocks;
  /**
   * @var string[]
   */
  public $serviceAddressCidrBlocks;
  protected $staticIpConfigType = VmwareStaticIpConfig::class;
  protected $staticIpConfigDataType = '';
  /**
   * @var string
   */
  public $vcenterNetwork;

  /**
   * @param VmwareControlPlaneV2Config
   */
  public function setControlPlaneV2Config(VmwareControlPlaneV2Config $controlPlaneV2Config)
  {
    $this->controlPlaneV2Config = $controlPlaneV2Config;
  }
  /**
   * @return VmwareControlPlaneV2Config
   */
  public function getControlPlaneV2Config()
  {
    return $this->controlPlaneV2Config;
  }
  /**
   * @param VmwareDhcpIpConfig
   */
  public function setDhcpIpConfig(VmwareDhcpIpConfig $dhcpIpConfig)
  {
    $this->dhcpIpConfig = $dhcpIpConfig;
  }
  /**
   * @return VmwareDhcpIpConfig
   */
  public function getDhcpIpConfig()
  {
    return $this->dhcpIpConfig;
  }
  /**
   * @param VmwareHostConfig
   */
  public function setHostConfig(VmwareHostConfig $hostConfig)
  {
    $this->hostConfig = $hostConfig;
  }
  /**
   * @return VmwareHostConfig
   */
  public function getHostConfig()
  {
    return $this->hostConfig;
  }
  /**
   * @param string[]
   */
  public function setPodAddressCidrBlocks($podAddressCidrBlocks)
  {
    $this->podAddressCidrBlocks = $podAddressCidrBlocks;
  }
  /**
   * @return string[]
   */
  public function getPodAddressCidrBlocks()
  {
    return $this->podAddressCidrBlocks;
  }
  /**
   * @param string[]
   */
  public function setServiceAddressCidrBlocks($serviceAddressCidrBlocks)
  {
    $this->serviceAddressCidrBlocks = $serviceAddressCidrBlocks;
  }
  /**
   * @return string[]
   */
  public function getServiceAddressCidrBlocks()
  {
    return $this->serviceAddressCidrBlocks;
  }
  /**
   * @param VmwareStaticIpConfig
   */
  public function setStaticIpConfig(VmwareStaticIpConfig $staticIpConfig)
  {
    $this->staticIpConfig = $staticIpConfig;
  }
  /**
   * @return VmwareStaticIpConfig
   */
  public function getStaticIpConfig()
  {
    return $this->staticIpConfig;
  }
  /**
   * @param string
   */
  public function setVcenterNetwork($vcenterNetwork)
  {
    $this->vcenterNetwork = $vcenterNetwork;
  }
  /**
   * @return string
   */
  public function getVcenterNetwork()
  {
    return $this->vcenterNetwork;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareNetworkConfig::class, 'Google_Service_GKEOnPrem_VmwareNetworkConfig');
