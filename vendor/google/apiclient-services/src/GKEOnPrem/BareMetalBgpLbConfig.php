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

class BareMetalBgpLbConfig extends \Google\Collection
{
  protected $collection_key = 'bgpPeerConfigs';
  protected $addressPoolsType = BareMetalLoadBalancerAddressPool::class;
  protected $addressPoolsDataType = 'array';
  /**
   * @var string
   */
  public $asn;
  protected $bgpPeerConfigsType = BareMetalBgpPeerConfig::class;
  protected $bgpPeerConfigsDataType = 'array';
  protected $loadBalancerNodePoolConfigType = BareMetalLoadBalancerNodePoolConfig::class;
  protected $loadBalancerNodePoolConfigDataType = '';

  /**
   * @param BareMetalLoadBalancerAddressPool[]
   */
  public function setAddressPools($addressPools)
  {
    $this->addressPools = $addressPools;
  }
  /**
   * @return BareMetalLoadBalancerAddressPool[]
   */
  public function getAddressPools()
  {
    return $this->addressPools;
  }
  /**
   * @param string
   */
  public function setAsn($asn)
  {
    $this->asn = $asn;
  }
  /**
   * @return string
   */
  public function getAsn()
  {
    return $this->asn;
  }
  /**
   * @param BareMetalBgpPeerConfig[]
   */
  public function setBgpPeerConfigs($bgpPeerConfigs)
  {
    $this->bgpPeerConfigs = $bgpPeerConfigs;
  }
  /**
   * @return BareMetalBgpPeerConfig[]
   */
  public function getBgpPeerConfigs()
  {
    return $this->bgpPeerConfigs;
  }
  /**
   * @param BareMetalLoadBalancerNodePoolConfig
   */
  public function setLoadBalancerNodePoolConfig(BareMetalLoadBalancerNodePoolConfig $loadBalancerNodePoolConfig)
  {
    $this->loadBalancerNodePoolConfig = $loadBalancerNodePoolConfig;
  }
  /**
   * @return BareMetalLoadBalancerNodePoolConfig
   */
  public function getLoadBalancerNodePoolConfig()
  {
    return $this->loadBalancerNodePoolConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalBgpLbConfig::class, 'Google_Service_GKEOnPrem_BareMetalBgpLbConfig');
