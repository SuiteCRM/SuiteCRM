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

class Volume extends \Google\Collection
{
  protected $collection_key = 'smbSettings';
  /**
   * @var string
   */
  public $activeDirectory;
  protected $backupConfigType = BackupConfig::class;
  protected $backupConfigDataType = '';
  /**
   * @var string
   */
  public $capacityGib;
  /**
   * @var string
   */
  public $coldTierSizeGib;
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
  protected $exportPolicyType = ExportPolicy::class;
  protected $exportPolicyDataType = '';
  /**
   * @var bool
   */
  public $hasReplication;
  protected $hybridReplicationParametersType = HybridReplicationParameters::class;
  protected $hybridReplicationParametersDataType = '';
  /**
   * @var bool
   */
  public $kerberosEnabled;
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
  public $largeCapacity;
  /**
   * @var bool
   */
  public $ldapEnabled;
  protected $mountOptionsType = MountOption::class;
  protected $mountOptionsDataType = 'array';
  /**
   * @var bool
   */
  public $multipleEndpoints;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string[]
   */
  public $protocols;
  /**
   * @var string
   */
  public $psaRange;
  /**
   * @var string
   */
  public $replicaZone;
  protected $restoreParametersType = RestoreParameters::class;
  protected $restoreParametersDataType = '';
  /**
   * @var string[]
   */
  public $restrictedActions;
  /**
   * @var string
   */
  public $securityStyle;
  /**
   * @var string
   */
  public $serviceLevel;
  /**
   * @var string
   */
  public $shareName;
  /**
   * @var string[]
   */
  public $smbSettings;
  public $snapReserve;
  /**
   * @var bool
   */
  public $snapshotDirectory;
  protected $snapshotPolicyType = SnapshotPolicy::class;
  protected $snapshotPolicyDataType = '';
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
  public $storagePool;
  protected $tieringPolicyType = TieringPolicy::class;
  protected $tieringPolicyDataType = '';
  /**
   * @var string
   */
  public $unixPermissions;
  /**
   * @var string
   */
  public $usedGib;
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
   * @param BackupConfig
   */
  public function setBackupConfig(BackupConfig $backupConfig)
  {
    $this->backupConfig = $backupConfig;
  }
  /**
   * @return BackupConfig
   */
  public function getBackupConfig()
  {
    return $this->backupConfig;
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
  public function setColdTierSizeGib($coldTierSizeGib)
  {
    $this->coldTierSizeGib = $coldTierSizeGib;
  }
  /**
   * @return string
   */
  public function getColdTierSizeGib()
  {
    return $this->coldTierSizeGib;
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
   * @param ExportPolicy
   */
  public function setExportPolicy(ExportPolicy $exportPolicy)
  {
    $this->exportPolicy = $exportPolicy;
  }
  /**
   * @return ExportPolicy
   */
  public function getExportPolicy()
  {
    return $this->exportPolicy;
  }
  /**
   * @param bool
   */
  public function setHasReplication($hasReplication)
  {
    $this->hasReplication = $hasReplication;
  }
  /**
   * @return bool
   */
  public function getHasReplication()
  {
    return $this->hasReplication;
  }
  /**
   * @param HybridReplicationParameters
   */
  public function setHybridReplicationParameters(HybridReplicationParameters $hybridReplicationParameters)
  {
    $this->hybridReplicationParameters = $hybridReplicationParameters;
  }
  /**
   * @return HybridReplicationParameters
   */
  public function getHybridReplicationParameters()
  {
    return $this->hybridReplicationParameters;
  }
  /**
   * @param bool
   */
  public function setKerberosEnabled($kerberosEnabled)
  {
    $this->kerberosEnabled = $kerberosEnabled;
  }
  /**
   * @return bool
   */
  public function getKerberosEnabled()
  {
    return $this->kerberosEnabled;
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
  public function setLargeCapacity($largeCapacity)
  {
    $this->largeCapacity = $largeCapacity;
  }
  /**
   * @return bool
   */
  public function getLargeCapacity()
  {
    return $this->largeCapacity;
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
   * @param MountOption[]
   */
  public function setMountOptions($mountOptions)
  {
    $this->mountOptions = $mountOptions;
  }
  /**
   * @return MountOption[]
   */
  public function getMountOptions()
  {
    return $this->mountOptions;
  }
  /**
   * @param bool
   */
  public function setMultipleEndpoints($multipleEndpoints)
  {
    $this->multipleEndpoints = $multipleEndpoints;
  }
  /**
   * @return bool
   */
  public function getMultipleEndpoints()
  {
    return $this->multipleEndpoints;
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
   * @param string[]
   */
  public function setProtocols($protocols)
  {
    $this->protocols = $protocols;
  }
  /**
   * @return string[]
   */
  public function getProtocols()
  {
    return $this->protocols;
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
   * @param RestoreParameters
   */
  public function setRestoreParameters(RestoreParameters $restoreParameters)
  {
    $this->restoreParameters = $restoreParameters;
  }
  /**
   * @return RestoreParameters
   */
  public function getRestoreParameters()
  {
    return $this->restoreParameters;
  }
  /**
   * @param string[]
   */
  public function setRestrictedActions($restrictedActions)
  {
    $this->restrictedActions = $restrictedActions;
  }
  /**
   * @return string[]
   */
  public function getRestrictedActions()
  {
    return $this->restrictedActions;
  }
  /**
   * @param string
   */
  public function setSecurityStyle($securityStyle)
  {
    $this->securityStyle = $securityStyle;
  }
  /**
   * @return string
   */
  public function getSecurityStyle()
  {
    return $this->securityStyle;
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
  public function setShareName($shareName)
  {
    $this->shareName = $shareName;
  }
  /**
   * @return string
   */
  public function getShareName()
  {
    return $this->shareName;
  }
  /**
   * @param string[]
   */
  public function setSmbSettings($smbSettings)
  {
    $this->smbSettings = $smbSettings;
  }
  /**
   * @return string[]
   */
  public function getSmbSettings()
  {
    return $this->smbSettings;
  }
  public function setSnapReserve($snapReserve)
  {
    $this->snapReserve = $snapReserve;
  }
  public function getSnapReserve()
  {
    return $this->snapReserve;
  }
  /**
   * @param bool
   */
  public function setSnapshotDirectory($snapshotDirectory)
  {
    $this->snapshotDirectory = $snapshotDirectory;
  }
  /**
   * @return bool
   */
  public function getSnapshotDirectory()
  {
    return $this->snapshotDirectory;
  }
  /**
   * @param SnapshotPolicy
   */
  public function setSnapshotPolicy(SnapshotPolicy $snapshotPolicy)
  {
    $this->snapshotPolicy = $snapshotPolicy;
  }
  /**
   * @return SnapshotPolicy
   */
  public function getSnapshotPolicy()
  {
    return $this->snapshotPolicy;
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
  public function setStoragePool($storagePool)
  {
    $this->storagePool = $storagePool;
  }
  /**
   * @return string
   */
  public function getStoragePool()
  {
    return $this->storagePool;
  }
  /**
   * @param TieringPolicy
   */
  public function setTieringPolicy(TieringPolicy $tieringPolicy)
  {
    $this->tieringPolicy = $tieringPolicy;
  }
  /**
   * @return TieringPolicy
   */
  public function getTieringPolicy()
  {
    return $this->tieringPolicy;
  }
  /**
   * @param string
   */
  public function setUnixPermissions($unixPermissions)
  {
    $this->unixPermissions = $unixPermissions;
  }
  /**
   * @return string
   */
  public function getUnixPermissions()
  {
    return $this->unixPermissions;
  }
  /**
   * @param string
   */
  public function setUsedGib($usedGib)
  {
    $this->usedGib = $usedGib;
  }
  /**
   * @return string
   */
  public function getUsedGib()
  {
    return $this->usedGib;
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
class_alias(Volume::class, 'Google_Service_NetAppFiles_Volume');
