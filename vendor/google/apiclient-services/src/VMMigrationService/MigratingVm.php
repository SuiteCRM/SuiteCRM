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

namespace Google\Service\VMMigrationService;

class MigratingVm extends \Google\Collection
{
  protected $collection_key = 'recentCutoverJobs';
  protected $awsSourceVmDetailsType = AwsSourceVmDetails::class;
  protected $awsSourceVmDetailsDataType = '';
  protected $azureSourceVmDetailsType = AzureSourceVmDetails::class;
  protected $azureSourceVmDetailsDataType = '';
  protected $computeEngineDisksTargetDefaultsType = ComputeEngineDisksTargetDefaults::class;
  protected $computeEngineDisksTargetDefaultsDataType = '';
  protected $computeEngineTargetDefaultsType = ComputeEngineTargetDefaults::class;
  protected $computeEngineTargetDefaultsDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $currentSyncInfoType = ReplicationCycle::class;
  protected $currentSyncInfoDataType = '';
  protected $cutoverForecastType = CutoverForecast::class;
  protected $cutoverForecastDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  protected $errorType = Status::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $group;
  /**
   * @var string[]
   */
  public $labels;
  protected $lastReplicationCycleType = ReplicationCycle::class;
  protected $lastReplicationCycleDataType = '';
  protected $lastSyncType = ReplicationSync::class;
  protected $lastSyncDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $policyType = SchedulePolicy::class;
  protected $policyDataType = '';
  protected $recentCloneJobsType = CloneJob::class;
  protected $recentCloneJobsDataType = 'array';
  protected $recentCutoverJobsType = CutoverJob::class;
  protected $recentCutoverJobsDataType = 'array';
  /**
   * @var string
   */
  public $sourceVmId;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $stateTime;
  /**
   * @var string
   */
  public $updateTime;
  protected $vmwareSourceVmDetailsType = VmwareSourceVmDetails::class;
  protected $vmwareSourceVmDetailsDataType = '';

  /**
   * @param AwsSourceVmDetails
   */
  public function setAwsSourceVmDetails(AwsSourceVmDetails $awsSourceVmDetails)
  {
    $this->awsSourceVmDetails = $awsSourceVmDetails;
  }
  /**
   * @return AwsSourceVmDetails
   */
  public function getAwsSourceVmDetails()
  {
    return $this->awsSourceVmDetails;
  }
  /**
   * @param AzureSourceVmDetails
   */
  public function setAzureSourceVmDetails(AzureSourceVmDetails $azureSourceVmDetails)
  {
    $this->azureSourceVmDetails = $azureSourceVmDetails;
  }
  /**
   * @return AzureSourceVmDetails
   */
  public function getAzureSourceVmDetails()
  {
    return $this->azureSourceVmDetails;
  }
  /**
   * @param ComputeEngineDisksTargetDefaults
   */
  public function setComputeEngineDisksTargetDefaults(ComputeEngineDisksTargetDefaults $computeEngineDisksTargetDefaults)
  {
    $this->computeEngineDisksTargetDefaults = $computeEngineDisksTargetDefaults;
  }
  /**
   * @return ComputeEngineDisksTargetDefaults
   */
  public function getComputeEngineDisksTargetDefaults()
  {
    return $this->computeEngineDisksTargetDefaults;
  }
  /**
   * @param ComputeEngineTargetDefaults
   */
  public function setComputeEngineTargetDefaults(ComputeEngineTargetDefaults $computeEngineTargetDefaults)
  {
    $this->computeEngineTargetDefaults = $computeEngineTargetDefaults;
  }
  /**
   * @return ComputeEngineTargetDefaults
   */
  public function getComputeEngineTargetDefaults()
  {
    return $this->computeEngineTargetDefaults;
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
   * @param ReplicationCycle
   */
  public function setCurrentSyncInfo(ReplicationCycle $currentSyncInfo)
  {
    $this->currentSyncInfo = $currentSyncInfo;
  }
  /**
   * @return ReplicationCycle
   */
  public function getCurrentSyncInfo()
  {
    return $this->currentSyncInfo;
  }
  /**
   * @param CutoverForecast
   */
  public function setCutoverForecast(CutoverForecast $cutoverForecast)
  {
    $this->cutoverForecast = $cutoverForecast;
  }
  /**
   * @return CutoverForecast
   */
  public function getCutoverForecast()
  {
    return $this->cutoverForecast;
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
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string
   */
  public function setGroup($group)
  {
    $this->group = $group;
  }
  /**
   * @return string
   */
  public function getGroup()
  {
    return $this->group;
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
   * @param ReplicationCycle
   */
  public function setLastReplicationCycle(ReplicationCycle $lastReplicationCycle)
  {
    $this->lastReplicationCycle = $lastReplicationCycle;
  }
  /**
   * @return ReplicationCycle
   */
  public function getLastReplicationCycle()
  {
    return $this->lastReplicationCycle;
  }
  /**
   * @param ReplicationSync
   */
  public function setLastSync(ReplicationSync $lastSync)
  {
    $this->lastSync = $lastSync;
  }
  /**
   * @return ReplicationSync
   */
  public function getLastSync()
  {
    return $this->lastSync;
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
   * @param SchedulePolicy
   */
  public function setPolicy(SchedulePolicy $policy)
  {
    $this->policy = $policy;
  }
  /**
   * @return SchedulePolicy
   */
  public function getPolicy()
  {
    return $this->policy;
  }
  /**
   * @param CloneJob[]
   */
  public function setRecentCloneJobs($recentCloneJobs)
  {
    $this->recentCloneJobs = $recentCloneJobs;
  }
  /**
   * @return CloneJob[]
   */
  public function getRecentCloneJobs()
  {
    return $this->recentCloneJobs;
  }
  /**
   * @param CutoverJob[]
   */
  public function setRecentCutoverJobs($recentCutoverJobs)
  {
    $this->recentCutoverJobs = $recentCutoverJobs;
  }
  /**
   * @return CutoverJob[]
   */
  public function getRecentCutoverJobs()
  {
    return $this->recentCutoverJobs;
  }
  /**
   * @param string
   */
  public function setSourceVmId($sourceVmId)
  {
    $this->sourceVmId = $sourceVmId;
  }
  /**
   * @return string
   */
  public function getSourceVmId()
  {
    return $this->sourceVmId;
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
  public function setStateTime($stateTime)
  {
    $this->stateTime = $stateTime;
  }
  /**
   * @return string
   */
  public function getStateTime()
  {
    return $this->stateTime;
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
   * @param VmwareSourceVmDetails
   */
  public function setVmwareSourceVmDetails(VmwareSourceVmDetails $vmwareSourceVmDetails)
  {
    $this->vmwareSourceVmDetails = $vmwareSourceVmDetails;
  }
  /**
   * @return VmwareSourceVmDetails
   */
  public function getVmwareSourceVmDetails()
  {
    return $this->vmwareSourceVmDetails;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MigratingVm::class, 'Google_Service_VMMigrationService_MigratingVm');
