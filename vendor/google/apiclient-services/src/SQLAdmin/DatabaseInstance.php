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

namespace Google\Service\SQLAdmin;

class DatabaseInstance extends \Google\Collection
{
  protected $collection_key = 'upgradableDatabaseVersions';
  /**
   * @var string[]
   */
  public $availableMaintenanceVersions;
  /**
   * @var string
   */
  public $backendType;
  /**
   * @var string
   */
  public $connectionName;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $currentDiskSize;
  /**
   * @var string
   */
  public $databaseInstalledVersion;
  /**
   * @var string
   */
  public $databaseVersion;
  protected $diskEncryptionConfigurationType = DiskEncryptionConfiguration::class;
  protected $diskEncryptionConfigurationDataType = '';
  protected $diskEncryptionStatusType = DiskEncryptionStatus::class;
  protected $diskEncryptionStatusDataType = '';
  /**
   * @var string
   */
  public $dnsName;
  /**
   * @var string
   */
  public $etag;
  protected $failoverReplicaType = DatabaseInstanceFailoverReplica::class;
  protected $failoverReplicaDataType = '';
  /**
   * @var string
   */
  public $gceZone;
  protected $geminiConfigType = GeminiInstanceConfig::class;
  protected $geminiConfigDataType = '';
  /**
   * @var bool
   */
  public $includeReplicasForMajorVersionUpgrade;
  /**
   * @var string
   */
  public $instanceType;
  protected $ipAddressesType = IpMapping::class;
  protected $ipAddressesDataType = 'array';
  /**
   * @var string
   */
  public $ipv6Address;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $maintenanceVersion;
  /**
   * @var string
   */
  public $masterInstanceName;
  /**
   * @var string
   */
  public $maxDiskSize;
  /**
   * @var string
   */
  public $name;
  protected $onPremisesConfigurationType = OnPremisesConfiguration::class;
  protected $onPremisesConfigurationDataType = '';
  protected $outOfDiskReportType = SqlOutOfDiskReport::class;
  protected $outOfDiskReportDataType = '';
  /**
   * @var string
   */
  public $primaryDnsName;
  /**
   * @var string
   */
  public $project;
  /**
   * @var string
   */
  public $pscServiceAttachmentLink;
  /**
   * @var string
   */
  public $region;
  protected $replicaConfigurationType = ReplicaConfiguration::class;
  protected $replicaConfigurationDataType = '';
  /**
   * @var string[]
   */
  public $replicaNames;
  protected $replicationClusterType = ReplicationCluster::class;
  protected $replicationClusterDataType = '';
  /**
   * @var string
   */
  public $rootPassword;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  protected $scheduledMaintenanceType = SqlScheduledMaintenance::class;
  protected $scheduledMaintenanceDataType = '';
  /**
   * @var string
   */
  public $secondaryGceZone;
  /**
   * @var string
   */
  public $selfLink;
  protected $serverCaCertType = SslCert::class;
  protected $serverCaCertDataType = '';
  /**
   * @var string
   */
  public $serviceAccountEmailAddress;
  protected $settingsType = Settings::class;
  protected $settingsDataType = '';
  /**
   * @var string
   */
  public $sqlNetworkArchitecture;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string[]
   */
  public $suspensionReason;
  /**
   * @var bool
   */
  public $switchTransactionLogsToCloudStorageEnabled;
  /**
   * @var string[]
   */
  public $tags;
  protected $upgradableDatabaseVersionsType = AvailableDatabaseVersion::class;
  protected $upgradableDatabaseVersionsDataType = 'array';
  /**
   * @var string
   */
  public $writeEndpoint;

  /**
   * @param string[]
   */
  public function setAvailableMaintenanceVersions($availableMaintenanceVersions)
  {
    $this->availableMaintenanceVersions = $availableMaintenanceVersions;
  }
  /**
   * @return string[]
   */
  public function getAvailableMaintenanceVersions()
  {
    return $this->availableMaintenanceVersions;
  }
  /**
   * @param string
   */
  public function setBackendType($backendType)
  {
    $this->backendType = $backendType;
  }
  /**
   * @return string
   */
  public function getBackendType()
  {
    return $this->backendType;
  }
  /**
   * @param string
   */
  public function setConnectionName($connectionName)
  {
    $this->connectionName = $connectionName;
  }
  /**
   * @return string
   */
  public function getConnectionName()
  {
    return $this->connectionName;
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
  public function setCurrentDiskSize($currentDiskSize)
  {
    $this->currentDiskSize = $currentDiskSize;
  }
  /**
   * @return string
   */
  public function getCurrentDiskSize()
  {
    return $this->currentDiskSize;
  }
  /**
   * @param string
   */
  public function setDatabaseInstalledVersion($databaseInstalledVersion)
  {
    $this->databaseInstalledVersion = $databaseInstalledVersion;
  }
  /**
   * @return string
   */
  public function getDatabaseInstalledVersion()
  {
    return $this->databaseInstalledVersion;
  }
  /**
   * @param string
   */
  public function setDatabaseVersion($databaseVersion)
  {
    $this->databaseVersion = $databaseVersion;
  }
  /**
   * @return string
   */
  public function getDatabaseVersion()
  {
    return $this->databaseVersion;
  }
  /**
   * @param DiskEncryptionConfiguration
   */
  public function setDiskEncryptionConfiguration(DiskEncryptionConfiguration $diskEncryptionConfiguration)
  {
    $this->diskEncryptionConfiguration = $diskEncryptionConfiguration;
  }
  /**
   * @return DiskEncryptionConfiguration
   */
  public function getDiskEncryptionConfiguration()
  {
    return $this->diskEncryptionConfiguration;
  }
  /**
   * @param DiskEncryptionStatus
   */
  public function setDiskEncryptionStatus(DiskEncryptionStatus $diskEncryptionStatus)
  {
    $this->diskEncryptionStatus = $diskEncryptionStatus;
  }
  /**
   * @return DiskEncryptionStatus
   */
  public function getDiskEncryptionStatus()
  {
    return $this->diskEncryptionStatus;
  }
  /**
   * @param string
   */
  public function setDnsName($dnsName)
  {
    $this->dnsName = $dnsName;
  }
  /**
   * @return string
   */
  public function getDnsName()
  {
    return $this->dnsName;
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
   * @param DatabaseInstanceFailoverReplica
   */
  public function setFailoverReplica(DatabaseInstanceFailoverReplica $failoverReplica)
  {
    $this->failoverReplica = $failoverReplica;
  }
  /**
   * @return DatabaseInstanceFailoverReplica
   */
  public function getFailoverReplica()
  {
    return $this->failoverReplica;
  }
  /**
   * @param string
   */
  public function setGceZone($gceZone)
  {
    $this->gceZone = $gceZone;
  }
  /**
   * @return string
   */
  public function getGceZone()
  {
    return $this->gceZone;
  }
  /**
   * @param GeminiInstanceConfig
   */
  public function setGeminiConfig(GeminiInstanceConfig $geminiConfig)
  {
    $this->geminiConfig = $geminiConfig;
  }
  /**
   * @return GeminiInstanceConfig
   */
  public function getGeminiConfig()
  {
    return $this->geminiConfig;
  }
  /**
   * @param bool
   */
  public function setIncludeReplicasForMajorVersionUpgrade($includeReplicasForMajorVersionUpgrade)
  {
    $this->includeReplicasForMajorVersionUpgrade = $includeReplicasForMajorVersionUpgrade;
  }
  /**
   * @return bool
   */
  public function getIncludeReplicasForMajorVersionUpgrade()
  {
    return $this->includeReplicasForMajorVersionUpgrade;
  }
  /**
   * @param string
   */
  public function setInstanceType($instanceType)
  {
    $this->instanceType = $instanceType;
  }
  /**
   * @return string
   */
  public function getInstanceType()
  {
    return $this->instanceType;
  }
  /**
   * @param IpMapping[]
   */
  public function setIpAddresses($ipAddresses)
  {
    $this->ipAddresses = $ipAddresses;
  }
  /**
   * @return IpMapping[]
   */
  public function getIpAddresses()
  {
    return $this->ipAddresses;
  }
  /**
   * @param string
   */
  public function setIpv6Address($ipv6Address)
  {
    $this->ipv6Address = $ipv6Address;
  }
  /**
   * @return string
   */
  public function getIpv6Address()
  {
    return $this->ipv6Address;
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
  public function setMaintenanceVersion($maintenanceVersion)
  {
    $this->maintenanceVersion = $maintenanceVersion;
  }
  /**
   * @return string
   */
  public function getMaintenanceVersion()
  {
    return $this->maintenanceVersion;
  }
  /**
   * @param string
   */
  public function setMasterInstanceName($masterInstanceName)
  {
    $this->masterInstanceName = $masterInstanceName;
  }
  /**
   * @return string
   */
  public function getMasterInstanceName()
  {
    return $this->masterInstanceName;
  }
  /**
   * @param string
   */
  public function setMaxDiskSize($maxDiskSize)
  {
    $this->maxDiskSize = $maxDiskSize;
  }
  /**
   * @return string
   */
  public function getMaxDiskSize()
  {
    return $this->maxDiskSize;
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
   * @param OnPremisesConfiguration
   */
  public function setOnPremisesConfiguration(OnPremisesConfiguration $onPremisesConfiguration)
  {
    $this->onPremisesConfiguration = $onPremisesConfiguration;
  }
  /**
   * @return OnPremisesConfiguration
   */
  public function getOnPremisesConfiguration()
  {
    return $this->onPremisesConfiguration;
  }
  /**
   * @param SqlOutOfDiskReport
   */
  public function setOutOfDiskReport(SqlOutOfDiskReport $outOfDiskReport)
  {
    $this->outOfDiskReport = $outOfDiskReport;
  }
  /**
   * @return SqlOutOfDiskReport
   */
  public function getOutOfDiskReport()
  {
    return $this->outOfDiskReport;
  }
  /**
   * @param string
   */
  public function setPrimaryDnsName($primaryDnsName)
  {
    $this->primaryDnsName = $primaryDnsName;
  }
  /**
   * @return string
   */
  public function getPrimaryDnsName()
  {
    return $this->primaryDnsName;
  }
  /**
   * @param string
   */
  public function setProject($project)
  {
    $this->project = $project;
  }
  /**
   * @return string
   */
  public function getProject()
  {
    return $this->project;
  }
  /**
   * @param string
   */
  public function setPscServiceAttachmentLink($pscServiceAttachmentLink)
  {
    $this->pscServiceAttachmentLink = $pscServiceAttachmentLink;
  }
  /**
   * @return string
   */
  public function getPscServiceAttachmentLink()
  {
    return $this->pscServiceAttachmentLink;
  }
  /**
   * @param string
   */
  public function setRegion($region)
  {
    $this->region = $region;
  }
  /**
   * @return string
   */
  public function getRegion()
  {
    return $this->region;
  }
  /**
   * @param ReplicaConfiguration
   */
  public function setReplicaConfiguration(ReplicaConfiguration $replicaConfiguration)
  {
    $this->replicaConfiguration = $replicaConfiguration;
  }
  /**
   * @return ReplicaConfiguration
   */
  public function getReplicaConfiguration()
  {
    return $this->replicaConfiguration;
  }
  /**
   * @param string[]
   */
  public function setReplicaNames($replicaNames)
  {
    $this->replicaNames = $replicaNames;
  }
  /**
   * @return string[]
   */
  public function getReplicaNames()
  {
    return $this->replicaNames;
  }
  /**
   * @param ReplicationCluster
   */
  public function setReplicationCluster(ReplicationCluster $replicationCluster)
  {
    $this->replicationCluster = $replicationCluster;
  }
  /**
   * @return ReplicationCluster
   */
  public function getReplicationCluster()
  {
    return $this->replicationCluster;
  }
  /**
   * @param string
   */
  public function setRootPassword($rootPassword)
  {
    $this->rootPassword = $rootPassword;
  }
  /**
   * @return string
   */
  public function getRootPassword()
  {
    return $this->rootPassword;
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
   * @param SqlScheduledMaintenance
   */
  public function setScheduledMaintenance(SqlScheduledMaintenance $scheduledMaintenance)
  {
    $this->scheduledMaintenance = $scheduledMaintenance;
  }
  /**
   * @return SqlScheduledMaintenance
   */
  public function getScheduledMaintenance()
  {
    return $this->scheduledMaintenance;
  }
  /**
   * @param string
   */
  public function setSecondaryGceZone($secondaryGceZone)
  {
    $this->secondaryGceZone = $secondaryGceZone;
  }
  /**
   * @return string
   */
  public function getSecondaryGceZone()
  {
    return $this->secondaryGceZone;
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
   * @param SslCert
   */
  public function setServerCaCert(SslCert $serverCaCert)
  {
    $this->serverCaCert = $serverCaCert;
  }
  /**
   * @return SslCert
   */
  public function getServerCaCert()
  {
    return $this->serverCaCert;
  }
  /**
   * @param string
   */
  public function setServiceAccountEmailAddress($serviceAccountEmailAddress)
  {
    $this->serviceAccountEmailAddress = $serviceAccountEmailAddress;
  }
  /**
   * @return string
   */
  public function getServiceAccountEmailAddress()
  {
    return $this->serviceAccountEmailAddress;
  }
  /**
   * @param Settings
   */
  public function setSettings(Settings $settings)
  {
    $this->settings = $settings;
  }
  /**
   * @return Settings
   */
  public function getSettings()
  {
    return $this->settings;
  }
  /**
   * @param string
   */
  public function setSqlNetworkArchitecture($sqlNetworkArchitecture)
  {
    $this->sqlNetworkArchitecture = $sqlNetworkArchitecture;
  }
  /**
   * @return string
   */
  public function getSqlNetworkArchitecture()
  {
    return $this->sqlNetworkArchitecture;
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
  public function setSuspensionReason($suspensionReason)
  {
    $this->suspensionReason = $suspensionReason;
  }
  /**
   * @return string[]
   */
  public function getSuspensionReason()
  {
    return $this->suspensionReason;
  }
  /**
   * @param bool
   */
  public function setSwitchTransactionLogsToCloudStorageEnabled($switchTransactionLogsToCloudStorageEnabled)
  {
    $this->switchTransactionLogsToCloudStorageEnabled = $switchTransactionLogsToCloudStorageEnabled;
  }
  /**
   * @return bool
   */
  public function getSwitchTransactionLogsToCloudStorageEnabled()
  {
    return $this->switchTransactionLogsToCloudStorageEnabled;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param AvailableDatabaseVersion[]
   */
  public function setUpgradableDatabaseVersions($upgradableDatabaseVersions)
  {
    $this->upgradableDatabaseVersions = $upgradableDatabaseVersions;
  }
  /**
   * @return AvailableDatabaseVersion[]
   */
  public function getUpgradableDatabaseVersions()
  {
    return $this->upgradableDatabaseVersions;
  }
  /**
   * @param string
   */
  public function setWriteEndpoint($writeEndpoint)
  {
    $this->writeEndpoint = $writeEndpoint;
  }
  /**
   * @return string
   */
  public function getWriteEndpoint()
  {
    return $this->writeEndpoint;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseInstance::class, 'Google_Service_SQLAdmin_DatabaseInstance');
