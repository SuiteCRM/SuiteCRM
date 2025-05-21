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

namespace Google\Service\Container;

class NetworkConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $datapathProvider;
  /**
   * @var bool
   */
  public $defaultEnablePrivateNodes;
  protected $defaultSnatStatusType = DefaultSnatStatus::class;
  protected $defaultSnatStatusDataType = '';
  /**
   * @var bool
   */
  public $disableL4LbFirewallReconciliation;
  protected $dnsConfigType = DNSConfig::class;
  protected $dnsConfigDataType = '';
  /**
   * @var bool
   */
  public $enableCiliumClusterwideNetworkPolicy;
  /**
   * @var bool
   */
  public $enableFqdnNetworkPolicy;
  /**
   * @var bool
   */
  public $enableIntraNodeVisibility;
  /**
   * @var bool
   */
  public $enableL4ilbSubsetting;
  /**
   * @var bool
   */
  public $enableMultiNetworking;
  protected $gatewayApiConfigType = GatewayAPIConfig::class;
  protected $gatewayApiConfigDataType = '';
  /**
   * @var string
   */
  public $inTransitEncryptionConfig;
  /**
   * @var string
   */
  public $network;
  protected $networkPerformanceConfigType = ClusterNetworkPerformanceConfig::class;
  protected $networkPerformanceConfigDataType = '';
  /**
   * @var string
   */
  public $privateIpv6GoogleAccess;
  protected $serviceExternalIpsConfigType = ServiceExternalIPsConfig::class;
  protected $serviceExternalIpsConfigDataType = '';
  /**
   * @var string
   */
  public $subnetwork;

  /**
   * @param string
   */
  public function setDatapathProvider($datapathProvider)
  {
    $this->datapathProvider = $datapathProvider;
  }
  /**
   * @return string
   */
  public function getDatapathProvider()
  {
    return $this->datapathProvider;
  }
  /**
   * @param bool
   */
  public function setDefaultEnablePrivateNodes($defaultEnablePrivateNodes)
  {
    $this->defaultEnablePrivateNodes = $defaultEnablePrivateNodes;
  }
  /**
   * @return bool
   */
  public function getDefaultEnablePrivateNodes()
  {
    return $this->defaultEnablePrivateNodes;
  }
  /**
   * @param DefaultSnatStatus
   */
  public function setDefaultSnatStatus(DefaultSnatStatus $defaultSnatStatus)
  {
    $this->defaultSnatStatus = $defaultSnatStatus;
  }
  /**
   * @return DefaultSnatStatus
   */
  public function getDefaultSnatStatus()
  {
    return $this->defaultSnatStatus;
  }
  /**
   * @param bool
   */
  public function setDisableL4LbFirewallReconciliation($disableL4LbFirewallReconciliation)
  {
    $this->disableL4LbFirewallReconciliation = $disableL4LbFirewallReconciliation;
  }
  /**
   * @return bool
   */
  public function getDisableL4LbFirewallReconciliation()
  {
    return $this->disableL4LbFirewallReconciliation;
  }
  /**
   * @param DNSConfig
   */
  public function setDnsConfig(DNSConfig $dnsConfig)
  {
    $this->dnsConfig = $dnsConfig;
  }
  /**
   * @return DNSConfig
   */
  public function getDnsConfig()
  {
    return $this->dnsConfig;
  }
  /**
   * @param bool
   */
  public function setEnableCiliumClusterwideNetworkPolicy($enableCiliumClusterwideNetworkPolicy)
  {
    $this->enableCiliumClusterwideNetworkPolicy = $enableCiliumClusterwideNetworkPolicy;
  }
  /**
   * @return bool
   */
  public function getEnableCiliumClusterwideNetworkPolicy()
  {
    return $this->enableCiliumClusterwideNetworkPolicy;
  }
  /**
   * @param bool
   */
  public function setEnableFqdnNetworkPolicy($enableFqdnNetworkPolicy)
  {
    $this->enableFqdnNetworkPolicy = $enableFqdnNetworkPolicy;
  }
  /**
   * @return bool
   */
  public function getEnableFqdnNetworkPolicy()
  {
    return $this->enableFqdnNetworkPolicy;
  }
  /**
   * @param bool
   */
  public function setEnableIntraNodeVisibility($enableIntraNodeVisibility)
  {
    $this->enableIntraNodeVisibility = $enableIntraNodeVisibility;
  }
  /**
   * @return bool
   */
  public function getEnableIntraNodeVisibility()
  {
    return $this->enableIntraNodeVisibility;
  }
  /**
   * @param bool
   */
  public function setEnableL4ilbSubsetting($enableL4ilbSubsetting)
  {
    $this->enableL4ilbSubsetting = $enableL4ilbSubsetting;
  }
  /**
   * @return bool
   */
  public function getEnableL4ilbSubsetting()
  {
    return $this->enableL4ilbSubsetting;
  }
  /**
   * @param bool
   */
  public function setEnableMultiNetworking($enableMultiNetworking)
  {
    $this->enableMultiNetworking = $enableMultiNetworking;
  }
  /**
   * @return bool
   */
  public function getEnableMultiNetworking()
  {
    return $this->enableMultiNetworking;
  }
  /**
   * @param GatewayAPIConfig
   */
  public function setGatewayApiConfig(GatewayAPIConfig $gatewayApiConfig)
  {
    $this->gatewayApiConfig = $gatewayApiConfig;
  }
  /**
   * @return GatewayAPIConfig
   */
  public function getGatewayApiConfig()
  {
    return $this->gatewayApiConfig;
  }
  /**
   * @param string
   */
  public function setInTransitEncryptionConfig($inTransitEncryptionConfig)
  {
    $this->inTransitEncryptionConfig = $inTransitEncryptionConfig;
  }
  /**
   * @return string
   */
  public function getInTransitEncryptionConfig()
  {
    return $this->inTransitEncryptionConfig;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param ClusterNetworkPerformanceConfig
   */
  public function setNetworkPerformanceConfig(ClusterNetworkPerformanceConfig $networkPerformanceConfig)
  {
    $this->networkPerformanceConfig = $networkPerformanceConfig;
  }
  /**
   * @return ClusterNetworkPerformanceConfig
   */
  public function getNetworkPerformanceConfig()
  {
    return $this->networkPerformanceConfig;
  }
  /**
   * @param string
   */
  public function setPrivateIpv6GoogleAccess($privateIpv6GoogleAccess)
  {
    $this->privateIpv6GoogleAccess = $privateIpv6GoogleAccess;
  }
  /**
   * @return string
   */
  public function getPrivateIpv6GoogleAccess()
  {
    return $this->privateIpv6GoogleAccess;
  }
  /**
   * @param ServiceExternalIPsConfig
   */
  public function setServiceExternalIpsConfig(ServiceExternalIPsConfig $serviceExternalIpsConfig)
  {
    $this->serviceExternalIpsConfig = $serviceExternalIpsConfig;
  }
  /**
   * @return ServiceExternalIPsConfig
   */
  public function getServiceExternalIpsConfig()
  {
    return $this->serviceExternalIpsConfig;
  }
  /**
   * @param string
   */
  public function setSubnetwork($subnetwork)
  {
    $this->subnetwork = $subnetwork;
  }
  /**
   * @return string
   */
  public function getSubnetwork()
  {
    return $this->subnetwork;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkConfig::class, 'Google_Service_Container_NetworkConfig');
