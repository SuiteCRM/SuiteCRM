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

class NetworkPeering extends \Google\Model
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
   * @var bool
   */
  public $exchangeSubnetRoutes;
  /**
   * @var bool
   */
  public $exportCustomRoutes;
  /**
   * @var bool
   */
  public $exportCustomRoutesWithPublicIp;
  /**
   * @var bool
   */
  public $importCustomRoutes;
  /**
   * @var bool
   */
  public $importCustomRoutesWithPublicIp;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $peerMtu;
  /**
   * @var string
   */
  public $peerNetwork;
  /**
   * @var string
   */
  public $peerNetworkType;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateDetails;
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
   * @param bool
   */
  public function setExchangeSubnetRoutes($exchangeSubnetRoutes)
  {
    $this->exchangeSubnetRoutes = $exchangeSubnetRoutes;
  }
  /**
   * @return bool
   */
  public function getExchangeSubnetRoutes()
  {
    return $this->exchangeSubnetRoutes;
  }
  /**
   * @param bool
   */
  public function setExportCustomRoutes($exportCustomRoutes)
  {
    $this->exportCustomRoutes = $exportCustomRoutes;
  }
  /**
   * @return bool
   */
  public function getExportCustomRoutes()
  {
    return $this->exportCustomRoutes;
  }
  /**
   * @param bool
   */
  public function setExportCustomRoutesWithPublicIp($exportCustomRoutesWithPublicIp)
  {
    $this->exportCustomRoutesWithPublicIp = $exportCustomRoutesWithPublicIp;
  }
  /**
   * @return bool
   */
  public function getExportCustomRoutesWithPublicIp()
  {
    return $this->exportCustomRoutesWithPublicIp;
  }
  /**
   * @param bool
   */
  public function setImportCustomRoutes($importCustomRoutes)
  {
    $this->importCustomRoutes = $importCustomRoutes;
  }
  /**
   * @return bool
   */
  public function getImportCustomRoutes()
  {
    return $this->importCustomRoutes;
  }
  /**
   * @param bool
   */
  public function setImportCustomRoutesWithPublicIp($importCustomRoutesWithPublicIp)
  {
    $this->importCustomRoutesWithPublicIp = $importCustomRoutesWithPublicIp;
  }
  /**
   * @return bool
   */
  public function getImportCustomRoutesWithPublicIp()
  {
    return $this->importCustomRoutesWithPublicIp;
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
   * @param int
   */
  public function setPeerMtu($peerMtu)
  {
    $this->peerMtu = $peerMtu;
  }
  /**
   * @return int
   */
  public function getPeerMtu()
  {
    return $this->peerMtu;
  }
  /**
   * @param string
   */
  public function setPeerNetwork($peerNetwork)
  {
    $this->peerNetwork = $peerNetwork;
  }
  /**
   * @return string
   */
  public function getPeerNetwork()
  {
    return $this->peerNetwork;
  }
  /**
   * @param string
   */
  public function setPeerNetworkType($peerNetworkType)
  {
    $this->peerNetworkType = $peerNetworkType;
  }
  /**
   * @return string
   */
  public function getPeerNetworkType()
  {
    return $this->peerNetworkType;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setStateDetails($stateDetails)
  {
    $this->stateDetails = $stateDetails;
  }
  /**
   * @return string
   */
  public function getStateDetails()
  {
    return $this->stateDetails;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NetworkPeering::class, 'Google_Service_VMwareEngine_NetworkPeering');
