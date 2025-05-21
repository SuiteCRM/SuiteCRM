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

class Replication extends \Google\Model
{
  /**
   * @var string
   */
  public $clusterLocation;
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
  public $destinationVolume;
  protected $destinationVolumeParametersType = DestinationVolumeParameters::class;
  protected $destinationVolumeParametersDataType = '';
  /**
   * @var bool
   */
  public $healthy;
  protected $hybridPeeringDetailsType = HybridPeeringDetails::class;
  protected $hybridPeeringDetailsDataType = '';
  /**
   * @var string
   */
  public $hybridReplicationType;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $mirrorState;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $replicationSchedule;
  /**
   * @var string
   */
  public $role;
  /**
   * @var string
   */
  public $sourceVolume;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateDetails;
  protected $transferStatsType = TransferStats::class;
  protected $transferStatsDataType = '';

  /**
   * @param string
   */
  public function setClusterLocation($clusterLocation)
  {
    $this->clusterLocation = $clusterLocation;
  }
  /**
   * @return string
   */
  public function getClusterLocation()
  {
    return $this->clusterLocation;
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
  public function setDestinationVolume($destinationVolume)
  {
    $this->destinationVolume = $destinationVolume;
  }
  /**
   * @return string
   */
  public function getDestinationVolume()
  {
    return $this->destinationVolume;
  }
  /**
   * @param DestinationVolumeParameters
   */
  public function setDestinationVolumeParameters(DestinationVolumeParameters $destinationVolumeParameters)
  {
    $this->destinationVolumeParameters = $destinationVolumeParameters;
  }
  /**
   * @return DestinationVolumeParameters
   */
  public function getDestinationVolumeParameters()
  {
    return $this->destinationVolumeParameters;
  }
  /**
   * @param bool
   */
  public function setHealthy($healthy)
  {
    $this->healthy = $healthy;
  }
  /**
   * @return bool
   */
  public function getHealthy()
  {
    return $this->healthy;
  }
  /**
   * @param HybridPeeringDetails
   */
  public function setHybridPeeringDetails(HybridPeeringDetails $hybridPeeringDetails)
  {
    $this->hybridPeeringDetails = $hybridPeeringDetails;
  }
  /**
   * @return HybridPeeringDetails
   */
  public function getHybridPeeringDetails()
  {
    return $this->hybridPeeringDetails;
  }
  /**
   * @param string
   */
  public function setHybridReplicationType($hybridReplicationType)
  {
    $this->hybridReplicationType = $hybridReplicationType;
  }
  /**
   * @return string
   */
  public function getHybridReplicationType()
  {
    return $this->hybridReplicationType;
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
  public function setMirrorState($mirrorState)
  {
    $this->mirrorState = $mirrorState;
  }
  /**
   * @return string
   */
  public function getMirrorState()
  {
    return $this->mirrorState;
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
  public function setReplicationSchedule($replicationSchedule)
  {
    $this->replicationSchedule = $replicationSchedule;
  }
  /**
   * @return string
   */
  public function getReplicationSchedule()
  {
    return $this->replicationSchedule;
  }
  /**
   * @param string
   */
  public function setRole($role)
  {
    $this->role = $role;
  }
  /**
   * @return string
   */
  public function getRole()
  {
    return $this->role;
  }
  /**
   * @param string
   */
  public function setSourceVolume($sourceVolume)
  {
    $this->sourceVolume = $sourceVolume;
  }
  /**
   * @return string
   */
  public function getSourceVolume()
  {
    return $this->sourceVolume;
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
   * @param TransferStats
   */
  public function setTransferStats(TransferStats $transferStats)
  {
    $this->transferStats = $transferStats;
  }
  /**
   * @return TransferStats
   */
  public function getTransferStats()
  {
    return $this->transferStats;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Replication::class, 'Google_Service_NetAppFiles_Replication');
