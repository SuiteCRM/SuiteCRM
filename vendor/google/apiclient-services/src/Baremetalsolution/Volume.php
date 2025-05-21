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

namespace Google\Service\Baremetalsolution;

class Volume extends \Google\Collection
{
  protected $collection_key = 'instances';
  /**
   * @var bool
   */
  public $attached;
  /**
   * @var string
   */
  public $autoGrownSizeGib;
  /**
   * @var bool
   */
  public $bootVolume;
  /**
   * @var string
   */
  public $currentSizeGib;
  /**
   * @var string
   */
  public $emergencySizeGib;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string[]
   */
  public $instances;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $maxSizeGib;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $notes;
  /**
   * @var string
   */
  public $originallyRequestedSizeGib;
  /**
   * @var string
   */
  public $performanceTier;
  /**
   * @var string
   */
  public $pod;
  /**
   * @var string
   */
  public $protocol;
  /**
   * @var string
   */
  public $remainingSpaceGib;
  /**
   * @var string
   */
  public $requestedSizeGib;
  /**
   * @var string
   */
  public $snapshotAutoDeleteBehavior;
  /**
   * @var bool
   */
  public $snapshotEnabled;
  protected $snapshotReservationDetailType = SnapshotReservationDetail::class;
  protected $snapshotReservationDetailDataType = '';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $storageType;
  /**
   * @var string
   */
  public $workloadProfile;

  /**
   * @param bool
   */
  public function setAttached($attached)
  {
    $this->attached = $attached;
  }
  /**
   * @return bool
   */
  public function getAttached()
  {
    return $this->attached;
  }
  /**
   * @param string
   */
  public function setAutoGrownSizeGib($autoGrownSizeGib)
  {
    $this->autoGrownSizeGib = $autoGrownSizeGib;
  }
  /**
   * @return string
   */
  public function getAutoGrownSizeGib()
  {
    return $this->autoGrownSizeGib;
  }
  /**
   * @param bool
   */
  public function setBootVolume($bootVolume)
  {
    $this->bootVolume = $bootVolume;
  }
  /**
   * @return bool
   */
  public function getBootVolume()
  {
    return $this->bootVolume;
  }
  /**
   * @param string
   */
  public function setCurrentSizeGib($currentSizeGib)
  {
    $this->currentSizeGib = $currentSizeGib;
  }
  /**
   * @return string
   */
  public function getCurrentSizeGib()
  {
    return $this->currentSizeGib;
  }
  /**
   * @param string
   */
  public function setEmergencySizeGib($emergencySizeGib)
  {
    $this->emergencySizeGib = $emergencySizeGib;
  }
  /**
   * @return string
   */
  public function getEmergencySizeGib()
  {
    return $this->emergencySizeGib;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
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
   * @param string[]
   */
  public function setInstances($instances)
  {
    $this->instances = $instances;
  }
  /**
   * @return string[]
   */
  public function getInstances()
  {
    return $this->instances;
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
  public function setMaxSizeGib($maxSizeGib)
  {
    $this->maxSizeGib = $maxSizeGib;
  }
  /**
   * @return string
   */
  public function getMaxSizeGib()
  {
    return $this->maxSizeGib;
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
  public function setNotes($notes)
  {
    $this->notes = $notes;
  }
  /**
   * @return string
   */
  public function getNotes()
  {
    return $this->notes;
  }
  /**
   * @param string
   */
  public function setOriginallyRequestedSizeGib($originallyRequestedSizeGib)
  {
    $this->originallyRequestedSizeGib = $originallyRequestedSizeGib;
  }
  /**
   * @return string
   */
  public function getOriginallyRequestedSizeGib()
  {
    return $this->originallyRequestedSizeGib;
  }
  /**
   * @param string
   */
  public function setPerformanceTier($performanceTier)
  {
    $this->performanceTier = $performanceTier;
  }
  /**
   * @return string
   */
  public function getPerformanceTier()
  {
    return $this->performanceTier;
  }
  /**
   * @param string
   */
  public function setPod($pod)
  {
    $this->pod = $pod;
  }
  /**
   * @return string
   */
  public function getPod()
  {
    return $this->pod;
  }
  /**
   * @param string
   */
  public function setProtocol($protocol)
  {
    $this->protocol = $protocol;
  }
  /**
   * @return string
   */
  public function getProtocol()
  {
    return $this->protocol;
  }
  /**
   * @param string
   */
  public function setRemainingSpaceGib($remainingSpaceGib)
  {
    $this->remainingSpaceGib = $remainingSpaceGib;
  }
  /**
   * @return string
   */
  public function getRemainingSpaceGib()
  {
    return $this->remainingSpaceGib;
  }
  /**
   * @param string
   */
  public function setRequestedSizeGib($requestedSizeGib)
  {
    $this->requestedSizeGib = $requestedSizeGib;
  }
  /**
   * @return string
   */
  public function getRequestedSizeGib()
  {
    return $this->requestedSizeGib;
  }
  /**
   * @param string
   */
  public function setSnapshotAutoDeleteBehavior($snapshotAutoDeleteBehavior)
  {
    $this->snapshotAutoDeleteBehavior = $snapshotAutoDeleteBehavior;
  }
  /**
   * @return string
   */
  public function getSnapshotAutoDeleteBehavior()
  {
    return $this->snapshotAutoDeleteBehavior;
  }
  /**
   * @param bool
   */
  public function setSnapshotEnabled($snapshotEnabled)
  {
    $this->snapshotEnabled = $snapshotEnabled;
  }
  /**
   * @return bool
   */
  public function getSnapshotEnabled()
  {
    return $this->snapshotEnabled;
  }
  /**
   * @param SnapshotReservationDetail
   */
  public function setSnapshotReservationDetail(SnapshotReservationDetail $snapshotReservationDetail)
  {
    $this->snapshotReservationDetail = $snapshotReservationDetail;
  }
  /**
   * @return SnapshotReservationDetail
   */
  public function getSnapshotReservationDetail()
  {
    return $this->snapshotReservationDetail;
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
  public function setStorageType($storageType)
  {
    $this->storageType = $storageType;
  }
  /**
   * @return string
   */
  public function getStorageType()
  {
    return $this->storageType;
  }
  /**
   * @param string
   */
  public function setWorkloadProfile($workloadProfile)
  {
    $this->workloadProfile = $workloadProfile;
  }
  /**
   * @return string
   */
  public function getWorkloadProfile()
  {
    return $this->workloadProfile;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Volume::class, 'Google_Service_Baremetalsolution_Volume');
