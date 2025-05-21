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

namespace Google\Service\Spanner;

class Backup extends \Google\Collection
{
  protected $collection_key = 'referencingDatabases';
  /**
   * @var string[]
   */
  public $backupSchedules;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $databaseDialect;
  protected $encryptionInfoType = EncryptionInfo::class;
  protected $encryptionInfoDataType = '';
  protected $encryptionInformationType = EncryptionInfo::class;
  protected $encryptionInformationDataType = 'array';
  /**
   * @var string
   */
  public $exclusiveSizeBytes;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $freeableSizeBytes;
  /**
   * @var string
   */
  public $incrementalBackupChainId;
  protected $instancePartitionsType = BackupInstancePartition::class;
  protected $instancePartitionsDataType = 'array';
  /**
   * @var string
   */
  public $maxExpireTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $oldestVersionTime;
  /**
   * @var string[]
   */
  public $referencingBackups;
  /**
   * @var string[]
   */
  public $referencingDatabases;
  /**
   * @var string
   */
  public $sizeBytes;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $versionTime;

  /**
   * @param string[]
   */
  public function setBackupSchedules($backupSchedules)
  {
    $this->backupSchedules = $backupSchedules;
  }
  /**
   * @return string[]
   */
  public function getBackupSchedules()
  {
    return $this->backupSchedules;
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
  public function setDatabase($database)
  {
    $this->database = $database;
  }
  /**
   * @return string
   */
  public function getDatabase()
  {
    return $this->database;
  }
  /**
   * @param string
   */
  public function setDatabaseDialect($databaseDialect)
  {
    $this->databaseDialect = $databaseDialect;
  }
  /**
   * @return string
   */
  public function getDatabaseDialect()
  {
    return $this->databaseDialect;
  }
  /**
   * @param EncryptionInfo
   */
  public function setEncryptionInfo(EncryptionInfo $encryptionInfo)
  {
    $this->encryptionInfo = $encryptionInfo;
  }
  /**
   * @return EncryptionInfo
   */
  public function getEncryptionInfo()
  {
    return $this->encryptionInfo;
  }
  /**
   * @param EncryptionInfo[]
   */
  public function setEncryptionInformation($encryptionInformation)
  {
    $this->encryptionInformation = $encryptionInformation;
  }
  /**
   * @return EncryptionInfo[]
   */
  public function getEncryptionInformation()
  {
    return $this->encryptionInformation;
  }
  /**
   * @param string
   */
  public function setExclusiveSizeBytes($exclusiveSizeBytes)
  {
    $this->exclusiveSizeBytes = $exclusiveSizeBytes;
  }
  /**
   * @return string
   */
  public function getExclusiveSizeBytes()
  {
    return $this->exclusiveSizeBytes;
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
  public function setFreeableSizeBytes($freeableSizeBytes)
  {
    $this->freeableSizeBytes = $freeableSizeBytes;
  }
  /**
   * @return string
   */
  public function getFreeableSizeBytes()
  {
    return $this->freeableSizeBytes;
  }
  /**
   * @param string
   */
  public function setIncrementalBackupChainId($incrementalBackupChainId)
  {
    $this->incrementalBackupChainId = $incrementalBackupChainId;
  }
  /**
   * @return string
   */
  public function getIncrementalBackupChainId()
  {
    return $this->incrementalBackupChainId;
  }
  /**
   * @param BackupInstancePartition[]
   */
  public function setInstancePartitions($instancePartitions)
  {
    $this->instancePartitions = $instancePartitions;
  }
  /**
   * @return BackupInstancePartition[]
   */
  public function getInstancePartitions()
  {
    return $this->instancePartitions;
  }
  /**
   * @param string
   */
  public function setMaxExpireTime($maxExpireTime)
  {
    $this->maxExpireTime = $maxExpireTime;
  }
  /**
   * @return string
   */
  public function getMaxExpireTime()
  {
    return $this->maxExpireTime;
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
  public function setOldestVersionTime($oldestVersionTime)
  {
    $this->oldestVersionTime = $oldestVersionTime;
  }
  /**
   * @return string
   */
  public function getOldestVersionTime()
  {
    return $this->oldestVersionTime;
  }
  /**
   * @param string[]
   */
  public function setReferencingBackups($referencingBackups)
  {
    $this->referencingBackups = $referencingBackups;
  }
  /**
   * @return string[]
   */
  public function getReferencingBackups()
  {
    return $this->referencingBackups;
  }
  /**
   * @param string[]
   */
  public function setReferencingDatabases($referencingDatabases)
  {
    $this->referencingDatabases = $referencingDatabases;
  }
  /**
   * @return string[]
   */
  public function getReferencingDatabases()
  {
    return $this->referencingDatabases;
  }
  /**
   * @param string
   */
  public function setSizeBytes($sizeBytes)
  {
    $this->sizeBytes = $sizeBytes;
  }
  /**
   * @return string
   */
  public function getSizeBytes()
  {
    return $this->sizeBytes;
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
  public function setVersionTime($versionTime)
  {
    $this->versionTime = $versionTime;
  }
  /**
   * @return string
   */
  public function getVersionTime()
  {
    return $this->versionTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Backup::class, 'Google_Service_Spanner_Backup');
