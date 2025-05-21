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

namespace Google\Service\CloudRedis;

class Backup extends \Google\Collection
{
  protected $collection_key = 'backupFiles';
  protected $backupFilesType = BackupFile::class;
  protected $backupFilesDataType = 'array';
  /**
   * @var string
   */
  public $backupType;
  /**
   * @var string
   */
  public $cluster;
  /**
   * @var string
   */
  public $clusterUid;
  /**
   * @var string
   */
  public $createTime;
  protected $encryptionInfoType = EncryptionInfo::class;
  protected $encryptionInfoDataType = '';
  /**
   * @var string
   */
  public $engineVersion;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nodeType;
  /**
   * @var int
   */
  public $replicaCount;
  /**
   * @var int
   */
  public $shardCount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $totalSizeBytes;
  /**
   * @var string
   */
  public $uid;

  /**
   * @param BackupFile[]
   */
  public function setBackupFiles($backupFiles)
  {
    $this->backupFiles = $backupFiles;
  }
  /**
   * @return BackupFile[]
   */
  public function getBackupFiles()
  {
    return $this->backupFiles;
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
   * @param string
   */
  public function setCluster($cluster)
  {
    $this->cluster = $cluster;
  }
  /**
   * @return string
   */
  public function getCluster()
  {
    return $this->cluster;
  }
  /**
   * @param string
   */
  public function setClusterUid($clusterUid)
  {
    $this->clusterUid = $clusterUid;
  }
  /**
   * @return string
   */
  public function getClusterUid()
  {
    return $this->clusterUid;
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
   * @param string
   */
  public function setEngineVersion($engineVersion)
  {
    $this->engineVersion = $engineVersion;
  }
  /**
   * @return string
   */
  public function getEngineVersion()
  {
    return $this->engineVersion;
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
  public function setNodeType($nodeType)
  {
    $this->nodeType = $nodeType;
  }
  /**
   * @return string
   */
  public function getNodeType()
  {
    return $this->nodeType;
  }
  /**
   * @param int
   */
  public function setReplicaCount($replicaCount)
  {
    $this->replicaCount = $replicaCount;
  }
  /**
   * @return int
   */
  public function getReplicaCount()
  {
    return $this->replicaCount;
  }
  /**
   * @param int
   */
  public function setShardCount($shardCount)
  {
    $this->shardCount = $shardCount;
  }
  /**
   * @return int
   */
  public function getShardCount()
  {
    return $this->shardCount;
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
  public function setTotalSizeBytes($totalSizeBytes)
  {
    $this->totalSizeBytes = $totalSizeBytes;
  }
  /**
   * @return string
   */
  public function getTotalSizeBytes()
  {
    return $this->totalSizeBytes;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Backup::class, 'Google_Service_CloudRedis_Backup');
