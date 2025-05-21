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

class NetworkPolicy extends \Google\Model
{
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
  public $edgeServicesCidr;
  protected $externalIpType = NetworkService::class;
  protected $externalIpDataType = '';
  protected $internetAccessType = NetworkService::class;
  protected $internetAccessDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;
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
  public function setEdgeServicesCidr($edgeServicesCidr)
  {
    $this->edgeServicesCidr = $edgeServicesCidr;
  }
  /**
   * @return string
   */
  public function getEdgeServicesCidr()
  {
    return $this->edgeServicesCidr;
  }
  /**
   * @param NetworkService
   */
  public function setExternalIp(NetworkService $externalIp)
  {
    $this->externalIp = $externalIp;
  }
  /**
   * @return NetworkService
   */
  public function getExternalIp()
  {
    return $this->externalIp;
  }
  /**
   * @param NetworkService
   */
  public function setInternetAccess(NetworkService $internetAccess)
  {
    $this->internetAccess = $internetAccess;
  }
  /**
   * @return NetworkService
   */
  public function getInternetAccess()
  {
    return $this->internetAccess;
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
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
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
class_alias(NetworkPolicy::class, 'Google_Service_VMwareEngine_NetworkPolicy');
