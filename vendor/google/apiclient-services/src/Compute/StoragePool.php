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

class StoragePool extends \Google\Model
{
  /**
   * @var string
   */
  public $capacityProvisioningType;
  /**
   * @var string
   */
  public $creationTimestamp;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $labelFingerprint;
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
  public $performanceProvisioningType;
  /**
   * @var string
   */
  public $poolProvisionedCapacityGb;
  /**
   * @var string
   */
  public $poolProvisionedIops;
  /**
   * @var string
   */
  public $poolProvisionedThroughput;
  protected $resourceStatusType = StoragePoolResourceStatus::class;
  protected $resourceStatusDataType = '';
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $selfLinkWithId;
  /**
   * @var string
   */
  public $state;
  protected $statusType = StoragePoolResourceStatus::class;
  protected $statusDataType = '';
  /**
   * @var string
   */
  public $storagePoolType;
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setCapacityProvisioningType($capacityProvisioningType)
  {
    $this->capacityProvisioningType = $capacityProvisioningType;
  }
  /**
   * @return string
   */
  public function getCapacityProvisioningType()
  {
    return $this->capacityProvisioningType;
  }
  /**
   * @param string
   */
  public function setCreationTimestamp($creationTimestamp)
  {
    $this->creationTimestamp = $creationTimestamp;
  }
  /**
   * @return string
   */
  public function getCreationTimestamp()
  {
    return $this->creationTimestamp;
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
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param string
   */
  public function setLabelFingerprint($labelFingerprint)
  {
    $this->labelFingerprint = $labelFingerprint;
  }
  /**
   * @return string
   */
  public function getLabelFingerprint()
  {
    return $this->labelFingerprint;
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
  public function setPerformanceProvisioningType($performanceProvisioningType)
  {
    $this->performanceProvisioningType = $performanceProvisioningType;
  }
  /**
   * @return string
   */
  public function getPerformanceProvisioningType()
  {
    return $this->performanceProvisioningType;
  }
  /**
   * @param string
   */
  public function setPoolProvisionedCapacityGb($poolProvisionedCapacityGb)
  {
    $this->poolProvisionedCapacityGb = $poolProvisionedCapacityGb;
  }
  /**
   * @return string
   */
  public function getPoolProvisionedCapacityGb()
  {
    return $this->poolProvisionedCapacityGb;
  }
  /**
   * @param string
   */
  public function setPoolProvisionedIops($poolProvisionedIops)
  {
    $this->poolProvisionedIops = $poolProvisionedIops;
  }
  /**
   * @return string
   */
  public function getPoolProvisionedIops()
  {
    return $this->poolProvisionedIops;
  }
  /**
   * @param string
   */
  public function setPoolProvisionedThroughput($poolProvisionedThroughput)
  {
    $this->poolProvisionedThroughput = $poolProvisionedThroughput;
  }
  /**
   * @return string
   */
  public function getPoolProvisionedThroughput()
  {
    return $this->poolProvisionedThroughput;
  }
  /**
   * @param StoragePoolResourceStatus
   */
  public function setResourceStatus(StoragePoolResourceStatus $resourceStatus)
  {
    $this->resourceStatus = $resourceStatus;
  }
  /**
   * @return StoragePoolResourceStatus
   */
  public function getResourceStatus()
  {
    return $this->resourceStatus;
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
  public function setSelfLinkWithId($selfLinkWithId)
  {
    $this->selfLinkWithId = $selfLinkWithId;
  }
  /**
   * @return string
   */
  public function getSelfLinkWithId()
  {
    return $this->selfLinkWithId;
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
   * @param StoragePoolResourceStatus
   */
  public function setStatus(StoragePoolResourceStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return StoragePoolResourceStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setStoragePoolType($storagePoolType)
  {
    $this->storagePoolType = $storagePoolType;
  }
  /**
   * @return string
   */
  public function getStoragePoolType()
  {
    return $this->storagePoolType;
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
class_alias(StoragePool::class, 'Google_Service_Compute_StoragePool');
