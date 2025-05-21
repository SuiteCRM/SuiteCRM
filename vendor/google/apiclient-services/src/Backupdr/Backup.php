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

namespace Google\Service\Backupdr;

class Backup extends \Google\Collection
{
  protected $collection_key = 'serviceLocks';
  protected $backupApplianceBackupPropertiesType = BackupApplianceBackupProperties::class;
  protected $backupApplianceBackupPropertiesDataType = '';
  protected $backupApplianceLocksType = BackupLock::class;
  protected $backupApplianceLocksDataType = 'array';
  /**
   * @var string
   */
  public $backupType;
  protected $computeInstanceBackupPropertiesType = ComputeInstanceBackupProperties::class;
  protected $computeInstanceBackupPropertiesDataType = '';
  /**
   * @var string
   */
  public $consistencyTime;
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
  public $enforcedRetentionEndTime;
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $expireTime;
  protected $gcpBackupPlanInfoType = GCPBackupPlanInfo::class;
  protected $gcpBackupPlanInfoDataType = '';
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
  public $resourceSizeBytes;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  protected $serviceLocksType = BackupLock::class;
  protected $serviceLocksDataType = 'array';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param BackupApplianceBackupProperties
   */
  public function setBackupApplianceBackupProperties(BackupApplianceBackupProperties $backupApplianceBackupProperties)
  {
    $this->backupApplianceBackupProperties = $backupApplianceBackupProperties;
  }
  /**
   * @return BackupApplianceBackupProperties
   */
  public function getBackupApplianceBackupProperties()
  {
    return $this->backupApplianceBackupProperties;
  }
  /**
   * @param BackupLock[]
   */
  public function setBackupApplianceLocks($backupApplianceLocks)
  {
    $this->backupApplianceLocks = $backupApplianceLocks;
  }
  /**
   * @return BackupLock[]
   */
  public function getBackupApplianceLocks()
  {
    return $this->backupApplianceLocks;
  }
  /**
   * @param string
   */
  public function setBackupType($backupType)
  {
    $this->backupType = $backupType;
  }
  /**
   * @return string
   */
  public function getBackupType()
  {
    return $this->backupType;
  }
  /**
   * @param ComputeInstanceBackupProperties
   */
  public function setComputeInstanceBackupProperties(ComputeInstanceBackupProperties $computeInstanceBackupProperties)
  {
    $this->computeInstanceBackupProperties = $computeInstanceBackupProperties;
  }
  /**
   * @return ComputeInstanceBackupProperties
   */
  public function getComputeInstanceBackupProperties()
  {
    return $this->computeInstanceBackupProperties;
  }
  /**
   * @param string
   */
  public function setConsistencyTime($consistencyTime)
  {
    $this->consistencyTime = $consistencyTime;
  }
  /**
   * @return string
   */
  public function getConsistencyTime()
  {
    return $this->consistencyTime;
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
  public function setEnforcedRetentionEndTime($enforcedRetentionEndTime)
  {
    $this->enforcedRetentionEndTime = $enforcedRetentionEndTime;
  }
  /**
   * @return string
   */
  public function getEnforcedRetentionEndTime()
  {
    return $this->enforcedRetentionEndTime;
  }
  /**
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
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
   * @param GCPBackupPlanInfo
   */
  public function setGcpBackupPlanInfo(GCPBackupPlanInfo $gcpBackupPlanInfo)
  {
    $this->gcpBackupPlanInfo = $gcpBackupPlanInfo;
  }
  /**
   * @return GCPBackupPlanInfo
   */
  public function getGcpBackupPlanInfo()
  {
    return $this->gcpBackupPlanInfo;
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
  public function setResourceSizeBytes($resourceSizeBytes)
  {
    $this->resourceSizeBytes = $resourceSizeBytes;
  }
  /**
   * @return string
   */
  public function getResourceSizeBytes()
  {
    return $this->resourceSizeBytes;
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
   * @param BackupLock[]
   */
  public function setServiceLocks($serviceLocks)
  {
    $this->serviceLocks = $serviceLocks;
  }
  /**
   * @return BackupLock[]
   */
  public function getServiceLocks()
  {
    return $this->serviceLocks;
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
class_alias(Backup::class, 'Google_Service_Backupdr_Backup');
