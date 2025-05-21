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

namespace Google\Service\NetworkServices;

class Gateway extends \Google\Collection
{
  protected $collection_key = 'ports';
  /**
   * @var string[]
   */
  public $addresses;
  /**
   * @var string[]
   */
  public $certificateUrls;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $envoyHeaders;
  /**
   * @var string
   */
  public $gatewaySecurityPolicy;
  /**
   * @var string
   */
  public $ipVersion;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var int[]
   */
  public $ports;
  /**
   * @var string
   */
  public $routingMode;
  /**
   * @var string
   */
  public $scope;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $serverTlsPolicy;
  /**
   * @var string
   */
  public $subnetwork;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string[]
   */
  public function setAddresses($addresses)
  {
    $this->addresses = $addresses;
  }
  /**
   * @return string[]
   */
  public function getAddresses()
  {
    return $this->addresses;
  }
  /**
   * @param string[]
   */
  public function setCertificateUrls($certificateUrls)
  {
    $this->certificateUrls = $certificateUrls;
  }
  /**
   * @return string[]
   */
  public function getCertificateUrls()
  {
    return $this->certificateUrls;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setEnvoyHeaders($envoyHeaders)
  {
    $this->envoyHeaders = $envoyHeaders;
  }
  /**
   * @return string
   */
  public function getEnvoyHeaders()
  {
    return $this->envoyHeaders;
  }
  /**
   * @param string
   */
  public function setGatewaySecurityPolicy($gatewaySecurityPolicy)
  {
    $this->gatewaySecurityPolicy = $gatewaySecurityPolicy;
  }
  /**
   * @return string
   */
  public function getGatewaySecurityPolicy()
  {
    return $this->gatewaySecurityPolicy;
  }
  /**
   * @param string
   */
  public function setIpVersion($ipVersion)
  {
    $this->ipVersion = $ipVersion;
  }
  /**
   * @return string
   */
  public function getIpVersion()
  {
    return $this->ipVersion;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
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
   * @param int[]
   */
  public function setPorts($ports)
  {
    $this->ports = $ports;
  }
  /**
   * @return int[]
   */
  public function getPorts()
  {
    return $this->ports;
  }
  /**
   * @param string
   */
  public function setRoutingMode($routingMode)
  {
    $this->routingMode = $routingMode;
  }
  /**
   * @return string
   */
  public function getRoutingMode()
  {
    return $this->routingMode;
  }
  /**
   * @param string
   */
  public function setScope($scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return string
   */
  public function getScope()
  {
    return $this->scope;
  }
  /**
   * @param string
   */
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
  }
  /**
   * @param string
   */
  public function setServerTlsPolicy($serverTlsPolicy)
  {
    $this->serverTlsPolicy = $serverTlsPolicy;
  }
  /**
   * @return string
   */
  public function getServerTlsPolicy()
  {
    return $this->serverTlsPolicy;
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
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Gateway::class, 'Google_Service_NetworkServices_Gateway');
