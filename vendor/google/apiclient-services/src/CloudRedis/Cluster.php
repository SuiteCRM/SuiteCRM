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

class Cluster extends \Google\Collection
{
  protected $collection_key = 'pscServiceAttachments';
  /**
   * @var bool
   */
  public $asyncClusterEndpointsDeletionEnabled;
  /**
   * @var string
   */
  public $authorizationMode;
  protected $automatedBackupConfigType = AutomatedBackupConfig::class;
  protected $automatedBackupConfigDataType = '';
  /**
   * @var string
   */
  public $backupCollection;
  protected $clusterEndpointsType = ClusterEndpoint::class;
  protected $clusterEndpointsDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  protected $crossClusterReplicationConfigType = CrossClusterReplicationConfig::class;
  protected $crossClusterReplicationConfigDataType = '';
  /**
   * @var bool
   */
  public $deletionProtectionEnabled;
  protected $discoveryEndpointsType = DiscoveryEndpoint::class;
  protected $discoveryEndpointsDataType = 'array';
  protected $encryptionInfoType = EncryptionInfo::class;
  protected $encryptionInfoDataType = '';
  protected $gcsSourceType = GcsBackupSource::class;
  protected $gcsSourceDataType = '';
  /**
   * @var string
   */
  public $kmsKey;
  protected $maintenancePolicyType = ClusterMaintenancePolicy::class;
  protected $maintenancePolicyDataType = '';
  protected $maintenanceScheduleType = ClusterMaintenanceSchedule::class;
  protected $maintenanceScheduleDataType = '';
  protected $managedBackupSourceType = ManagedBackupSource::class;
  protected $managedBackupSourceDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nodeType;
  protected $persistenceConfigType = ClusterPersistenceConfig::class;
  protected $persistenceConfigDataType = '';
  public $preciseSizeGb;
  protected $pscConfigsType = PscConfig::class;
  protected $pscConfigsDataType = 'array';
  protected $pscConnectionsType = PscConnection::class;
  protected $pscConnectionsDataType = 'array';
  protected $pscServiceAttachmentsType = PscServiceAttachment::class;
  protected $pscServiceAttachmentsDataType = 'array';
  /**
   * @var string[]
   */
  public $redisConfigs;
  /**
   * @var int
   */
  public $replicaCount;
  /**
   * @var int
   */
  public $shardCount;
  /**
   * @var int
   */
  public $sizeGb;
  /**
   * @var string
   */
  public $state;
  protected $stateInfoType = StateInfo::class;
  protected $stateInfoDataType = '';
  /**
   * @var string
   */
  public $transitEncryptionMode;
  /**
   * @var string
   */
  public $uid;
  protected $zoneDistributionConfigType = ZoneDistributionConfig::class;
  protected $zoneDistributionConfigDataType = '';

  /**
   * @param bool
   */
  public function setAsyncClusterEndpointsDeletionEnabled($asyncClusterEndpointsDeletionEnabled)
  {
    $this->asyncClusterEndpointsDeletionEnabled = $asyncClusterEndpointsDeletionEnabled;
  }
  /**
   * @return bool
   */
  public function getAsyncClusterEndpointsDeletionEnabled()
  {
    return $this->asyncClusterEndpointsDeletionEnabled;
  }
  /**
   * @param string
   */
  public function setAuthorizationMode($authorizationMode)
  {
    $this->authorizationMode = $authorizationMode;
  }
  /**
   * @return string
   */
  public function getAuthorizationMode()
  {
    return $this->authorizationMode;
  }
  /**
   * @param AutomatedBackupConfig
   */
  public function setAutomatedBackupConfig(AutomatedBackupConfig $automatedBackupConfig)
  {
    $this->automatedBackupConfig = $automatedBackupConfig;
  }
  /**
   * @return AutomatedBackupConfig
   */
  public function getAutomatedBackupConfig()
  {
    return $this->automatedBackupConfig;
  }
  /**
   * @param string
   */
  public function setBackupCollection($backupCollection)
  {
    $this->backupCollection = $backupCollection;
  }
  /**
   * @return string
   */
  public function getBackupCollection()
  {
    return $this->backupCollection;
  }
  /**
   * @param ClusterEndpoint[]
   */
  public function setClusterEndpoints($clusterEndpoints)
  {
    $this->clusterEndpoints = $clusterEndpoints;
  }
  /**
   * @return ClusterEndpoint[]
   */
  public function getClusterEndpoints()
  {
    return $this->clusterEndpoints;
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
   * @param CrossClusterReplicationConfig
   */
  public function setCrossClusterReplicationConfig(CrossClusterReplicationConfig $crossClusterReplicationConfig)
  {
    $this->crossClusterReplicationConfig = $crossClusterReplicationConfig;
  }
  /**
   * @return CrossClusterReplicationConfig
   */
  public function getCrossClusterReplicationConfig()
  {
    return $this->crossClusterReplicationConfig;
  }
  /**
   * @param bool
   */
  public function setDeletionProtectionEnabled($deletionProtectionEnabled)
  {
    $this->deletionProtectionEnabled = $deletionProtectionEnabled;
  }
  /**
   * @return bool
   */
  public function getDeletionProtectionEnabled()
  {
    return $this->deletionProtectionEnabled;
  }
  /**
   * @param DiscoveryEndpoint[]
   */
  public function setDiscoveryEndpoints($discoveryEndpoints)
  {
    $this->discoveryEndpoints = $discoveryEndpoints;
  }
  /**
   * @return DiscoveryEndpoint[]
   */
  public function getDiscoveryEndpoints()
  {
    return $this->discoveryEndpoints;
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
   * @param GcsBackupSource
   */
  public function setGcsSource(GcsBackupSource $gcsSource)
  {
    $this->gcsSource = $gcsSource;
  }
  /**
   * @return GcsBackupSource
   */
  public function getGcsSource()
  {
    return $this->gcsSource;
  }
  /**
   * @param string
   */
  public function setKmsKey($kmsKey)
  {
    $this->kmsKey = $kmsKey;
  }
  /**
   * @return string
   */
  public function getKmsKey()
  {
    return $this->kmsKey;
  }
  /**
   * @param ClusterMaintenancePolicy
   */
  public function setMaintenancePolicy(ClusterMaintenancePolicy $maintenancePolicy)
  {
    $this->maintenancePolicy = $maintenancePolicy;
  }
  /**
   * @return ClusterMaintenancePolicy
   */
  public function getMaintenancePolicy()
  {
    return $this->maintenancePolicy;
  }
  /**
   * @param ClusterMaintenanceSchedule
   */
  public function setMaintenanceSchedule(ClusterMaintenanceSchedule $maintenanceSchedule)
  {
    $this->maintenanceSchedule = $maintenanceSchedule;
  }
  /**
   * @return ClusterMaintenanceSchedule
   */
  public function getMaintenanceSchedule()
  {
    return $this->maintenanceSchedule;
  }
  /**
   * @param ManagedBackupSource
   */
  public function setManagedBackupSource(ManagedBackupSource $managedBackupSource)
  {
    $this->managedBackupSource = $managedBackupSource;
  }
  /**
   * @return ManagedBackupSource
   */
  public function getManagedBackupSource()
  {
    return $this->managedBackupSource;
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
   * @param ClusterPersistenceConfig
   */
  public function setPersistenceConfig(ClusterPersistenceConfig $persistenceConfig)
  {
    $this->persistenceConfig = $persistenceConfig;
  }
  /**
   * @return ClusterPersistenceConfig
   */
  public function getPersistenceConfig()
  {
    return $this->persistenceConfig;
  }
  public function setPreciseSizeGb($preciseSizeGb)
  {
    $this->preciseSizeGb = $preciseSizeGb;
  }
  public function getPreciseSizeGb()
  {
    return $this->preciseSizeGb;
  }
  /**
   * @param PscConfig[]
   */
  public function setPscConfigs($pscConfigs)
  {
    $this->pscConfigs = $pscConfigs;
  }
  /**
   * @return PscConfig[]
   */
  public function getPscConfigs()
  {
    return $this->pscConfigs;
  }
  /**
   * @param PscConnection[]
   */
  public function setPscConnections($pscConnections)
  {
    $this->pscConnections = $pscConnections;
  }
  /**
   * @return PscConnection[]
   */
  public function getPscConnections()
  {
    return $this->pscConnections;
  }
  /**
   * @param PscServiceAttachment[]
   */
  public function setPscServiceAttachments($pscServiceAttachments)
  {
    $this->pscServiceAttachments = $pscServiceAttachments;
  }
  /**
   * @return PscServiceAttachment[]
   */
  public function getPscServiceAttachments()
  {
    return $this->pscServiceAttachments;
  }
  /**
   * @param string[]
   */
  public function setRedisConfigs($redisConfigs)
  {
    $this->redisConfigs = $redisConfigs;
  }
  /**
   * @return string[]
   */
  public function getRedisConfigs()
  {
    return $this->redisConfigs;
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
   * @param int
   */
  public function setSizeGb($sizeGb)
  {
    $this->sizeGb = $sizeGb;
  }
  /**
   * @return int
   */
  public function getSizeGb()
  {
    return $this->sizeGb;
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
   * @param StateInfo
   */
  public function setStateInfo(StateInfo $stateInfo)
  {
    $this->stateInfo = $stateInfo;
  }
  /**
   * @return StateInfo
   */
  public function getStateInfo()
  {
    return $this->stateInfo;
  }
  /**
   * @param string
   */
  public function setTransitEncryptionMode($transitEncryptionMode)
  {
    $this->transitEncryptionMode = $transitEncryptionMode;
  }
  /**
   * @return string
   */
  public function getTransitEncryptionMode()
  {
    return $this->transitEncryptionMode;
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
   * @param ZoneDistributionConfig
   */
  public function setZoneDistributionConfig(ZoneDistributionConfig $zoneDistributionConfig)
  {
    $this->zoneDistributionConfig = $zoneDistributionConfig;
  }
  /**
   * @return ZoneDistributionConfig
   */
  public function getZoneDistributionConfig()
  {
    return $this->zoneDistributionConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Cluster::class, 'Google_Service_CloudRedis_Cluster');
