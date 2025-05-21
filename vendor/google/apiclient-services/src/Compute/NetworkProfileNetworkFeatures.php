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

namespace Google\Service\Compute;

class NetworkProfileNetworkFeatures extends \Google\Collection
{
  protected $collection_key = 'subnetStackTypes';
  /**
   * @var string[]
   */
  public $addressPurposes;
  /**
   * @var string
   */
  public $allowAliasIpRanges;
  /**
   * @var string
   */
  public $allowAutoModeSubnet;
  /**
   * @var string
   */
  public $allowClassDFirewalls;
  /**
   * @var string
   */
  public $allowCloudNat;
  /**
   * @var string
   */
  public $allowCloudRouter;
  /**
   * @var string
   */
  public $allowExternalIpAccess;
  /**
   * @var string
   */
  public $allowInterconnect;
  /**
   * @var string
   */
  public $allowLoadBalancing;
  /**
   * @var string
   */
  public $allowMultiNicInSameNetwork;
  /**
   * @var string
   */
  public $allowPacketMirroring;
  /**
   * @var string
   */
  public $allowPrivateGoogleAccess;
  /**
   * @var string
   */
  public $allowPsc;
  /**
   * @var string
   */
  public $allowSameNetworkUnicast;
  /**
   * @var string
   */
  public $allowStaticRoutes;
  /**
   * @var string
   */
  public $allowSubInterfaces;
  /**
   * @var string
   */
  public $allowVpcPeering;
  /**
   * @var string
   */
  public $allowVpn;
  /**
   * @var string[]
   */
  public $interfaceTypes;
  /**
   * @var string[]
   */
  public $subnetPurposes;
  /**
   * @var string[]
   */
  public $subnetStackTypes;
  /**
   * @var string
   */
  public $unicast;

  /**
   * @param string[]
   */
  public function setAddressPurposes($addressPurposes)
  {
    $this->addressPurposes = $addressPurposes;
  }
  /**
   * @return string[]
   */
  public function getAddressPurposes()
  {
    return $this->addressPurposes;
  }
  /**
   * @param string
   */
  public function setAllowAliasIpRanges($allowAliasIpRanges)
  {
    $this->allowAliasIpRanges = $allowAliasIpRanges;
  }
  /**
   * @return string
   */
  public function getAllowAliasIpRanges()
  {
    return $this->allowAliasIpRanges;
  }
  /**
   * @param string
   */
  public function setAllowAutoModeSubnet($allowAutoModeSubnet)
  {
    $this->allowAutoModeSubnet = $allowAutoModeSubnet;
  }
  /**
   * @return string
   */
  public function getAllowAutoModeSubnet()
  {
    return $this->allowAutoModeSubnet;
  }
  /**
   * @param string
   */
  public function setAllowClassDFirewalls($allowClassDFirewalls)
  {
    $this->allowClassDFirewalls = $allowClassDFirewalls;
  }
  /**
   * @return string
   */
  public function getAllowClassDFirewalls()
  {
    return $this->allowClassDFirewalls;
  }
  /**
   * @param string
   */
  public function setAllowCloudNat($allowCloudNat)
  {
    $this->allowCloudNat = $allowCloudNat;
  }
  /**
   * @return string
   */
  public function getAllowCloudNat()
  {
    return $this->allowCloudNat;
  }
  /**
   * @param string
   */
  public function setAllowCloudRouter($allowCloudRouter)
  {
    $this->allowCloudRouter = $allowCloudRouter;
  }
  /**
   * @return string
   */
  public function getAllowCloudRouter()
  {
    return $this->allowCloudRouter;
  }
  /**
   * @param string
   */
  public function setAllowExternalIpAccess($allowExternalIpAccess)
  {
    $this->allowExternalIpAccess = $allowExternalIpAccess;
  }
  /**
   * @return string
   */
  public function getAllowExternalIpAccess()
  {
    return $this->allowExternalIpAccess;
  }
  /**
   * @param string
   */
  public function setAllowInterconnect($allowInterconnect)
  {
    $this->allowInterconnect = $allowInterconnect;
  }
  /**
   * @return string
   */
  public function getAllowInterconnect()
  {
    return $this->allowInterconnect;
  }
  /**
   * @param string
   */
  public function setAllowLoadBalancing($allowLoadBalancing)
  {
    $this->allowLoadBalancing = $allowLoadBalancing;
  }
  /**
   * @return string
   */
  public function getAllowLoadBalancing()
  {
    return $this->allowLoadBalancing;
  }
  /**
   * @param string
   */
  public function setAllowMultiNicInSameNetwork($allowMultiNicInSameNetwork)
  {
    $this->allowMultiNicInSameNetwork = $allowMultiNicInSameNetwork;
  }
  /**
   * @return string
   */
  public function getAllowMultiNicInSameNetwork()
  {
    return $this->allowMultiNicInSameNetwork;
  }
  /**
   * @param string
   */
  public function setAllowPacketMirroring($allowPacketMirroring)
  {
    $this->allowPacketMirroring = $allowPacketMirroring;
  }
  /**
   * @return string
   */
  public function getAllowPacketMirroring()
  {
    return $this->allowPacketMirroring;
  }
  /**
   * @param string
   */
  public function setAllowPrivateGoogleAccess($allowPrivateGoogleAccess)
  {
    $this->allowPrivateGoogleAccess = $allowPrivateGoogleAccess;
  }
  /**
   * @return string
   */
  public function getAllowPrivateGoogleAccess()
  {
    return $this->allowPrivateGoogleAccess;
  }
  /**
   * @param string
   */
  public function setAllowPsc($allowPsc)
  {
    $this->allowPsc = $allowPsc;
  }
  /**
   * @return string
   */
  public function getAllowPsc()
  {
    return $this->allowPsc;
  }
  /**
   * @param string
   */
  public function setAllowSameNetworkUnicast($allowSameNetworkUnicast)
  {
    $this->allowSameNetworkUnicast = $allowSameNetworkUnicast;
  }
  /**
   * @return string
   */
  public function getAllowSameNetworkUnicast()
  {
    return $this->allowSameNetworkUnicast;
  }
  /**
   * @param string
   */
  public function setAllowStaticRoutes($allowStaticRoutes)
  {
    $this->allowStaticRoutes = $allowStaticRoutes;
  }
  /**
   * @return string
   */
  public function getAllowStaticRoutes()
  {
    return $this->allowStaticRoutes;
  }
  /**
   * @param string
   */
  public function setAllowSubInterfaces($allowSubInterfaces)
  {
    $this->allowSubInterfaces = $allowSubInterfaces;
  }
  /**
   * @return string
   */
  public function getAllowSubInterfaces()
  {
    return $this->allowSubInterfaces;
  }
  /**
   * @param string
   */
  public function setAllowVpcPeering($allowVpcPeering)
  {
    $this->allowVpcPeering = $allowVpcPeering;
  }
  /**
   * @return string
   */
  public function getAllowVpcPeering()
  {
    return $this->allowVpcPeering;
  }
  /**
   * @param string
   */
  public function setAllowVpn($allowVpn)
  {
    $this->allowVpn = $allowVpn;
  }
  /**
   * @return string
   */
  public function getAllowVpn()
  {
    return $this->allowVpn;
  }
  /**
   * @param string[]
   */
  public function setInterfaceTypes($interfaceTypes)
  {
    $this->interfaceTypes = $interfaceTypes;
  }
  /**
   * @return string[]
   */
  public function getInterfaceTypes()
  {
    return $this->interfaceTypes;
  }
  /**
   * @param string[]
   */
  public function setSubnetPurposes($subnetPurposes)
  {
    $this->subnetPurposes = $subnetPurposes;
  }
  /**
   * @return string[]
   */
  public function getSubnetPurposes()
  {
    return $this->subnetPurposes;
  }
  /**
   * @param string[]
   */
  public function setSubnetStackTypes($subnetStackTypes)
  {
    $this->subnetStackTypes = $subnetStackTypes;
  }
  /**
   * @return string[]
   */
  public function getSubnetStackTypes()
  {
    return $this->subnetStackTypes;
  }
  /**
   * @param string
   */
  public function setUnicast($unicast)
  {
    $this->unicast = $unicast;
  }
  /**
   * @return string
   */
  public function getUnicast()
  {
    return $this->unicast;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkProfileNetworkFeatures::class, 'Google_Service_Compute_NetworkProfileNetworkFeatures');
