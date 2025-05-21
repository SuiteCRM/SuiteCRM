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

namespace Google\Service\OracleDatabase;

class AutonomousDatabaseProperties extends \Google\Collection
{
  protected $collection_key = 'supportedCloneRegions';
  public $actualUsedDataStorageSizeTb;
  public $allocatedStorageSizeTb;
  protected $apexDetailsType = AutonomousDatabaseApex::class;
  protected $apexDetailsDataType = '';
  /**
   * @var bool
   */
  public $arePrimaryAllowlistedIpsUsed;
  /**
   * @var string
   */
  public $autonomousContainerDatabaseId;
  /**
   * @var string[]
   */
  public $availableUpgradeVersions;
  /**
   * @var int
   */
  public $backupRetentionPeriodDays;
  /**
   * @var string
   */
  public $characterSet;
  /**
   * @var float
   */
  public $computeCount;
  protected $connectionStringsType = AutonomousDatabaseConnectionStrings::class;
  protected $connectionStringsDataType = '';
  protected $connectionUrlsType = AutonomousDatabaseConnectionUrls::class;
  protected $connectionUrlsDataType = '';
  /**
   * @var int
   */
  public $cpuCoreCount;
  protected $customerContactsType = CustomerContact::class;
  protected $customerContactsDataType = 'array';
  /**
   * @var string
   */
  public $dataSafeState;
  /**
   * @var int
   */
  public $dataStorageSizeGb;
  /**
   * @var int
   */
  public $dataStorageSizeTb;
  /**
   * @var string
   */
  public $databaseManagementState;
  /**
   * @var string
   */
  public $dbEdition;
  /**
   * @var string
   */
  public $dbVersion;
  /**
   * @var string
   */
  public $dbWorkload;
  /**
   * @var string
   */
  public $failedDataRecoveryDuration;
  /**
   * @var bool
   */
  public $isAutoScalingEnabled;
  /**
   * @var bool
   */
  public $isLocalDataGuardEnabled;
  /**
   * @var bool
   */
  public $isStorageAutoScalingEnabled;
  /**
   * @var string
   */
  public $licenseType;
  /**
   * @var string
   */
  public $lifecycleDetails;
  /**
   * @var int
   */
  public $localAdgAutoFailoverMaxDataLossLimit;
  /**
   * @var string
   */
  public $localDisasterRecoveryType;
  protected $localStandbyDbType = AutonomousDatabaseStandbySummary::class;
  protected $localStandbyDbDataType = '';
  /**
   * @var string
   */
  public $maintenanceBeginTime;
  /**
   * @var string
   */
  public $maintenanceEndTime;
  /**
   * @var string
   */
  public $maintenanceScheduleType;
  /**
   * @var int
   */
  public $memoryPerOracleComputeUnitGbs;
  /**
   * @var int
   */
  public $memoryTableGbs;
  /**
   * @var bool
   */
  public $mtlsConnectionRequired;
  /**
   * @var string
   */
  public $nCharacterSet;
  /**
   * @var string
   */
  public $nextLongTermBackupTime;
  /**
   * @var string
   */
  public $ociUrl;
  /**
   * @var string
   */
  public $ocid;
  /**
   * @var string
   */
  public $openMode;
  /**
   * @var string
   */
  public $operationsInsightsState;
  /**
   * @var string[]
   */
  public $peerDbIds;
  /**
   * @var string
   */
  public $permissionLevel;
  /**
   * @var string
   */
  public $privateEndpoint;
  /**
   * @var string
   */
  public $privateEndpointIp;
  /**
   * @var string
   */
  public $privateEndpointLabel;
  /**
   * @var string
   */
  public $refreshableMode;
  /**
   * @var string
   */
  public $refreshableState;
  /**
   * @var string
   */
  public $role;
  protected $scheduledOperationDetailsType = ScheduledOperationDetails::class;
  protected $scheduledOperationDetailsDataType = 'array';
  /**
   * @var string
   */
  public $secretId;
  /**
   * @var string
   */
  public $sqlWebDeveloperUrl;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string[]
   */
  public $supportedCloneRegions;
  /**
   * @var float
   */
  public $totalAutoBackupStorageSizeGbs;
  /**
   * @var int
   */
  public $usedDataStorageSizeTbs;
  /**
   * @var string
   */
  public $vaultId;

  public function setActualUsedDataStorageSizeTb($actualUsedDataStorageSizeTb)
  {
    $this->actualUsedDataStorageSizeTb = $actualUsedDataStorageSizeTb;
  }
  public function getActualUsedDataStorageSizeTb()
  {
    return $this->actualUsedDataStorageSizeTb;
  }
  public function setAllocatedStorageSizeTb($allocatedStorageSizeTb)
  {
    $this->allocatedStorageSizeTb = $allocatedStorageSizeTb;
  }
  public function getAllocatedStorageSizeTb()
  {
    return $this->allocatedStorageSizeTb;
  }
  /**
   * @param AutonomousDatabaseApex
   */
  public function setApexDetails(AutonomousDatabaseApex $apexDetails)
  {
    $this->apexDetails = $apexDetails;
  }
  /**
   * @return AutonomousDatabaseApex
   */
  public function getApexDetails()
  {
    return $this->apexDetails;
  }
  /**
   * @param bool
   */
  public function setArePrimaryAllowlistedIpsUsed($arePrimaryAllowlistedIpsUsed)
  {
    $this->arePrimaryAllowlistedIpsUsed = $arePrimaryAllowlistedIpsUsed;
  }
  /**
   * @return bool
   */
  public function getArePrimaryAllowlistedIpsUsed()
  {
    return $this->arePrimaryAllowlistedIpsUsed;
  }
  /**
   * @param string
   */
  public function setAutonomousContainerDatabaseId($autonomousContainerDatabaseId)
  {
    $this->autonomousContainerDatabaseId = $autonomousContainerDatabaseId;
  }
  /**
   * @return string
   */
  public function getAutonomousContainerDatabaseId()
  {
    return $this->autonomousContainerDatabaseId;
  }
  /**
   * @param string[]
   */
  public function setAvailableUpgradeVersions($availableUpgradeVersions)
  {
    $this->availableUpgradeVersions = $availableUpgradeVersions;
  }
  /**
   * @return string[]
   */
  public function getAvailableUpgradeVersions()
  {
    return $this->availableUpgradeVersions;
  }
  /**
   * @param int
   */
  public function setBackupRetentionPeriodDays($backupRetentionPeriodDays)
  {
    $this->backupRetentionPeriodDays = $backupRetentionPeriodDays;
  }
  /**
   * @return int
   */
  public function getBackupRetentionPeriodDays()
  {
    return $this->backupRetentionPeriodDays;
  }
  /**
   * @param string
   */
  public function setCharacterSet($characterSet)
  {
    $this->characterSet = $characterSet;
  }
  /**
   * @return string
   */
  public function getCharacterSet()
  {
    return $this->characterSet;
  }
  /**
   * @param float
   */
  public function setComputeCount($computeCount)
  {
    $this->computeCount = $computeCount;
  }
  /**
   * @return float
   */
  public function getComputeCount()
  {
    return $this->computeCount;
  }
  /**
   * @param AutonomousDatabaseConnectionStrings
   */
  public function setConnectionStrings(AutonomousDatabaseConnectionStrings $connectionStrings)
  {
    $this->connectionStrings = $connectionStrings;
  }
  /**
   * @return AutonomousDatabaseConnectionStrings
   */
  public function getConnectionStrings()
  {
    return $this->connectionStrings;
  }
  /**
   * @param AutonomousDatabaseConnectionUrls
   */
  public function setConnectionUrls(AutonomousDatabaseConnectionUrls $connectionUrls)
  {
    $this->connectionUrls = $connectionUrls;
  }
  /**
   * @return AutonomousDatabaseConnectionUrls
   */
  public function getConnectionUrls()
  {
    return $this->connectionUrls;
  }
  /**
   * @param int
   */
  public function setCpuCoreCount($cpuCoreCount)
  {
    $this->cpuCoreCount = $cpuCoreCount;
  }
  /**
   * @return int
   */
  public function getCpuCoreCount()
  {
    return $this->cpuCoreCount;
  }
  /**
   * @param CustomerContact[]
   */
  public function setCustomerContacts($customerContacts)
  {
    $this->customerContacts = $customerContacts;
  }
  /**
   * @return CustomerContact[]
   */
  public function getCustomerContacts()
  {
    return $this->customerContacts;
  }
  /**
   * @param string
   */
  public function setDataSafeState($dataSafeState)
  {
    $this->dataSafeState = $dataSafeState;
  }
  /**
   * @return string
   */
  public function getDataSafeState()
  {
    return $this->dataSafeState;
  }
  /**
   * @param int
   */
  public function setDataStorageSizeGb($dataStorageSizeGb)
  {
    $this->dataStorageSizeGb = $dataStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getDataStorageSizeGb()
  {
    return $this->dataStorageSizeGb;
  }
  /**
   * @param int
   */
  public function setDataStorageSizeTb($dataStorageSizeTb)
  {
    $this->dataStorageSizeTb = $dataStorageSizeTb;
  }
  /**
   * @return int
   */
  public function getDataStorageSizeTb()
  {
    return $this->dataStorageSizeTb;
  }
  /**
   * @param string
   */
  public function setDatabaseManagementState($databaseManagementState)
  {
    $this->databaseManagementState = $databaseManagementState;
  }
  /**
   * @return string
   */
  public function getDatabaseManagementState()
  {
    return $this->databaseManagementState;
  }
  /**
   * @param string
   */
  public function setDbEdition($dbEdition)
  {
    $this->dbEdition = $dbEdition;
  }
  /**
   * @return string
   */
  public function getDbEdition()
  {
    return $this->dbEdition;
  }
  /**
   * @param string
   */
  public function setDbVersion($dbVersion)
  {
    $this->dbVersion = $dbVersion;
  }
  /**
   * @return string
   */
  public function getDbVersion()
  {
    return $this->dbVersion;
  }
  /**
   * @param string
   */
  public function setDbWorkload($dbWorkload)
  {
    $this->dbWorkload = $dbWorkload;
  }
  /**
   * @return string
   */
  public function getDbWorkload()
  {
    return $this->dbWorkload;
  }
  /**
   * @param string
   */
  public function setFailedDataRecoveryDuration($failedDataRecoveryDuration)
  {
    $this->failedDataRecoveryDuration = $failedDataRecoveryDuration;
  }
  /**
   * @return string
   */
  public function getFailedDataRecoveryDuration()
  {
    return $this->failedDataRecoveryDuration;
  }
  /**
   * @param bool
   */
  public function setIsAutoScalingEnabled($isAutoScalingEnabled)
  {
    $this->isAutoScalingEnabled = $isAutoScalingEnabled;
  }
  /**
   * @return bool
   */
  public function getIsAutoScalingEnabled()
  {
    return $this->isAutoScalingEnabled;
  }
  /**
   * @param bool
   */
  public function setIsLocalDataGuardEnabled($isLocalDataGuardEnabled)
  {
    $this->isLocalDataGuardEnabled = $isLocalDataGuardEnabled;
  }
  /**
   * @return bool
   */
  public function getIsLocalDataGuardEnabled()
  {
    return $this->isLocalDataGuardEnabled;
  }
  /**
   * @param bool
   */
  public function setIsStorageAutoScalingEnabled($isStorageAutoScalingEnabled)
  {
    $this->isStorageAutoScalingEnabled = $isStorageAutoScalingEnabled;
  }
  /**
   * @return bool
   */
  public function getIsStorageAutoScalingEnabled()
  {
    return $this->isStorageAutoScalingEnabled;
  }
  /**
   * @param string
   */
  public function setLicenseType($licenseType)
  {
    $this->licenseType = $licenseType;
  }
  /**
   * @return string
   */
  public function getLicenseType()
  {
    return $this->licenseType;
  }
  /**
   * @param string
   */
  public function setLifecycleDetails($lifecycleDetails)
  {
    $this->lifecycleDetails = $lifecycleDetails;
  }
  /**
   * @return string
   */
  public function getLifecycleDetails()
  {
    return $this->lifecycleDetails;
  }
  /**
   * @param int
   */
  public function setLocalAdgAutoFailoverMaxDataLossLimit($localAdgAutoFailoverMaxDataLossLimit)
  {
    $this->localAdgAutoFailoverMaxDataLossLimit = $localAdgAutoFailoverMaxDataLossLimit;
  }
  /**
   * @return int
   */
  public function getLocalAdgAutoFailoverMaxDataLossLimit()
  {
    return $this->localAdgAutoFailoverMaxDataLossLimit;
  }
  /**
   * @param string
   */
  public function setLocalDisasterRecoveryType($localDisasterRecoveryType)
  {
    $this->localDisasterRecoveryType = $localDisasterRecoveryType;
  }
  /**
   * @return string
   */
  public function getLocalDisasterRecoveryType()
  {
    return $this->localDisasterRecoveryType;
  }
  /**
   * @param AutonomousDatabaseStandbySummary
   */
  public function setLocalStandbyDb(AutonomousDatabaseStandbySummary $localStandbyDb)
  {
    $this->localStandbyDb = $localStandbyDb;
  }
  /**
   * @return AutonomousDatabaseStandbySummary
   */
  public function getLocalStandbyDb()
  {
    return $this->localStandbyDb;
  }
  /**
   * @param string
   */
  public function setMaintenanceBeginTime($maintenanceBeginTime)
  {
    $this->maintenanceBeginTime = $maintenanceBeginTime;
  }
  /**
   * @return string
   */
  public function getMaintenanceBeginTime()
  {
    return $this->maintenanceBeginTime;
  }
  /**
   * @param string
   */
  public function setMaintenanceEndTime($maintenanceEndTime)
  {
    $this->maintenanceEndTime = $maintenanceEndTime;
  }
  /**
   * @return string
   */
  public function getMaintenanceEndTime()
  {
    return $this->maintenanceEndTime;
  }
  /**
   * @param string
   */
  public function setMaintenanceScheduleType($maintenanceScheduleType)
  {
    $this->maintenanceScheduleType = $maintenanceScheduleType;
  }
  /**
   * @return string
   */
  public function getMaintenanceScheduleType()
  {
    return $this->maintenanceScheduleType;
  }
  /**
   * @param int
   */
  public function setMemoryPerOracleComputeUnitGbs($memoryPerOracleComputeUnitGbs)
  {
    $this->memoryPerOracleComputeUnitGbs = $memoryPerOracleComputeUnitGbs;
  }
  /**
   * @return int
   */
  public function getMemoryPerOracleComputeUnitGbs()
  {
    return $this->memoryPerOracleComputeUnitGbs;
  }
  /**
   * @param int
   */
  public function setMemoryTableGbs($memoryTableGbs)
  {
    $this->memoryTableGbs = $memoryTableGbs;
  }
  /**
   * @return int
   */
  public function getMemoryTableGbs()
  {
    return $this->memoryTableGbs;
  }
  /**
   * @param bool
   */
  public function setMtlsConnectionRequired($mtlsConnectionRequired)
  {
    $this->mtlsConnectionRequired = $mtlsConnectionRequired;
  }
  /**
   * @return bool
   */
  public function getMtlsConnectionRequired()
  {
    return $this->mtlsConnectionRequired;
  }
  /**
   * @param string
   */
  public function setNCharacterSet($nCharacterSet)
  {
    $this->nCharacterSet = $nCharacterSet;
  }
  /**
   * @return string
   */
  public function getNCharacterSet()
  {
    return $this->nCharacterSet;
  }
  /**
   * @param string
   */
  public function setNextLongTermBackupTime($nextLongTermBackupTime)
  {
    $this->nextLongTermBackupTime = $nextLongTermBackupTime;
  }
  /**
   * @return string
   */
  public function getNextLongTermBackupTime()
  {
    return $this->nextLongTermBackupTime;
  }
  /**
   * @param string
   */
  public function setOciUrl($ociUrl)
  {
    $this->ociUrl = $ociUrl;
  }
  /**
   * @return string
   */
  public function getOciUrl()
  {
    return $this->ociUrl;
  }
  /**
   * @param string
   */
  public function setOcid($ocid)
  {
    $this->ocid = $ocid;
  }
  /**
   * @return string
   */
  public function getOcid()
  {
    return $this->ocid;
  }
  /**
   * @param string
   */
  public function setOpenMode($openMode)
  {
    $this->openMode = $openMode;
  }
  /**
   * @return string
   */
  public function getOpenMode()
  {
    return $this->openMode;
  }
  /**
   * @param string
   */
  public function setOperationsInsightsState($operationsInsightsState)
  {
    $this->operationsInsightsState = $operationsInsightsState;
  }
  /**
   * @return string
   */
  public function getOperationsInsightsState()
  {
    return $this->operationsInsightsState;
  }
  /**
   * @param string[]
   */
  public function setPeerDbIds($peerDbIds)
  {
    $this->peerDbIds = $peerDbIds;
  }
  /**
   * @return string[]
   */
  public function getPeerDbIds()
  {
    return $this->peerDbIds;
  }
  /**
   * @param string
   */
  public function setPermissionLevel($permissionLevel)
  {
    $this->permissionLevel = $permissionLevel;
  }
  /**
   * @return string
   */
  public function getPermissionLevel()
  {
    return $this->permissionLevel;
  }
  /**
   * @param string
   */
  public function setPrivateEndpoint($privateEndpoint)
  {
    $this->privateEndpoint = $privateEndpoint;
  }
  /**
   * @return string
   */
  public function getPrivateEndpoint()
  {
    return $this->privateEndpoint;
  }
  /**
   * @param string
   */
  public function setPrivateEndpointIp($privateEndpointIp)
  {
    $this->privateEndpointIp = $privateEndpointIp;
  }
  /**
   * @return string
   */
  public function getPrivateEndpointIp()
  {
    return $this->privateEndpointIp;
  }
  /**
   * @param string
   */
  public function setPrivateEndpointLabel($privateEndpointLabel)
  {
    $this->privateEndpointLabel = $privateEndpointLabel;
  }
  /**
   * @return string
   */
  public function getPrivateEndpointLabel()
  {
    return $this->privateEndpointLabel;
  }
  /**
   * @param string
   */
  public function setRefreshableMode($refreshableMode)
  {
    $this->refreshableMode = $refreshableMode;
  }
  /**
   * @return string
   */
  public function getRefreshableMode()
  {
    return $this->refreshableMode;
  }
  /**
   * @param string
   */
  public function setRefreshableState($refreshableState)
  {
    $this->refreshableState = $refreshableState;
  }
  /**
   * @return string
   */
  public function getRefreshableState()
  {
    return $this->refreshableState;
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
   * @param ScheduledOperationDetails[]
   */
  public function setScheduledOperationDetails($scheduledOperationDetails)
  {
    $this->scheduledOperationDetails = $scheduledOperationDetails;
  }
  /**
   * @return ScheduledOperationDetails[]
   */
  public function getScheduledOperationDetails()
  {
    return $this->scheduledOperationDetails;
  }
  /**
   * @param string
   */
  public function setSecretId($secretId)
  {
    $this->secretId = $secretId;
  }
  /**
   * @return string
   */
  public function getSecretId()
  {
    return $this->secretId;
  }
  /**
   * @param string
   */
  public function setSqlWebDeveloperUrl($sqlWebDeveloperUrl)
  {
    $this->sqlWebDeveloperUrl = $sqlWebDeveloperUrl;
  }
  /**
   * @return string
   */
  public function getSqlWebDeveloperUrl()
  {
    return $this->sqlWebDeveloperUrl;
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
   * @param string[]
   */
  public function setSupportedCloneRegions($supportedCloneRegions)
  {
    $this->supportedCloneRegions = $supportedCloneRegions;
  }
  /**
   * @return string[]
   */
  public function getSupportedCloneRegions()
  {
    return $this->supportedCloneRegions;
  }
  /**
   * @param float
   */
  public function setTotalAutoBackupStorageSizeGbs($totalAutoBackupStorageSizeGbs)
  {
    $this->totalAutoBackupStorageSizeGbs = $totalAutoBackupStorageSizeGbs;
  }
  /**
   * @return float
   */
  public function getTotalAutoBackupStorageSizeGbs()
  {
    return $this->totalAutoBackupStorageSizeGbs;
  }
  /**
   * @param int
   */
  public function setUsedDataStorageSizeTbs($usedDataStorageSizeTbs)
  {
    $this->usedDataStorageSizeTbs = $usedDataStorageSizeTbs;
  }
  /**
   * @return int
   */
  public function getUsedDataStorageSizeTbs()
  {
    return $this->usedDataStorageSizeTbs;
  }
  /**
   * @param string
   */
  public function setVaultId($vaultId)
  {
    $this->vaultId = $vaultId;
  }
  /**
   * @return string
   */
  public function getVaultId()
  {
    return $this->vaultId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutonomousDatabaseProperties::class, 'Google_Service_OracleDatabase_AutonomousDatabaseProperties');
