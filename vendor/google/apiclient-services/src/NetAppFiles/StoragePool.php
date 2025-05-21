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

namespace Google\Service\NetAppFiles;

class StoragePool extends \Google\Model
{
  /**
   * @var string
   */
  public $activeDirectory;
  /**
   * @var bool
   */
  public $allowAutoTiering;
  /**
   * @var string
   */
  public $capacityGib;
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
  public $encryptionType;
  /**
   * @var bool
   */
  public $globalAccessAllowed;
  /**
   * @var string
   */
  public $kmsConfig;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var bool
   */
  public $ldapEnabled;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $psaRange;
  /**
   * @var string
   */
  public $replicaZone;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $serviceLevel;
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
  public $volumeCapacityGib;
  /**
   * @var int
   */
  public $volumeCount;
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setActiveDirectory($activeDirectory)
  {
    $this->activeDirectory = $activeDirectory;
  }
  /**
   * @return string
   */
  public function getActiveDirectory()
  {
    return $this->activeDirectory;
  }
  /**
   * @param bool
   */
  public function setAllowAutoTiering($allowAutoTiering)
  {
    $this->allowAutoTiering = $allowAutoTiering;
  }
  /**
   * @return bool
   */
  public function getAllowAutoTiering()
  {
    return $this->allowAutoTiering;
  }
  /**
   * @param string
   */
  public function setCapacityGib($capacityGib)
  {
    $this->capacityGib = $capacityGib;
  }
  /**
   * @return string
   */
  public function getCapacityGib()
  {
    return $this->capacityGib;
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
  public function setEncryptionType($encryptionType)
  {
    $this->encryptionType = $encryptionType;
  }
  /**
   * @return string
   */
  public function getEncryptionType()
  {
    return $this->encryptionType;
  }
  /**
   * @param bool
   */
  public function setGlobalAccessAllowed($globalAccessAllowed)
  {
    $this->globalAccessAllowed = $globalAccessAllowed;
  }
  /**
   * @return bool
   */
  public function getGlobalAccessAllowed()
  {
    return $this->globalAccessAllowed;
  }
  /**
   * @param string
   */
  public function setKmsConfig($kmsConfig)
  {
    $this->kmsConfig = $kmsConfig;
  }
  /**
   * @return string
   */
  public function getKmsConfig()
  {
    return $this->kmsConfig;
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
   * @param bool
   */
  public function setLdapEnabled($ldapEnabled)
  {
    $this->ldapEnabled = $ldapEnabled;
  }
  /**
   * @return bool
   */
  public function getLdapEnabled()
  {
    return $this->ldapEnabled;
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
   * @param string
   */
  public function setPsaRange($psaRange)
  {
    $this->psaRange = $psaRange;
  }
  /**
   * @return string
   */
  public function getPsaRange()
  {
    return $this->psaRange;
  }
  /**
   * @param string
   */
  public function setReplicaZone($replicaZone)
  {
    $this->replicaZone = $replicaZone;
  }
  /**
   * @return string
   */
  public function getReplicaZone()
  {
    return $this->replicaZone;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param string
   */
  public function setServiceLevel($serviceLevel)
  {
    $this->serviceLevel = $serviceLevel;
  }
  /**
   * @return string
   */
  public function getServiceLevel()
  {
    return $this->serviceLevel;
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
  public function setVolumeCapacityGib($volumeCapacityGib)
  {
    $this->volumeCapacityGib = $volumeCapacityGib;
  }
  /**
   * @return string
   */
  public function getVolumeCapacityGib()
  {
    return $this->volumeCapacityGib;
  }
  /**
   * @param int
   */
  public function setVolumeCount($volumeCount)
  {
    $this->volumeCount = $volumeCount;
  }
  /**
   * @return int
   */
  public function getVolumeCount()
  {
    return $this->volumeCount;
  }
  /**
   * @param string
   */
  public function setZone($zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return string
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StoragePool::class, 'Google_Service_NetAppFiles_StoragePool');
