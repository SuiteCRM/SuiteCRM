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

class CloudVmClusterProperties extends \Google\Collection
{
  protected $collection_key = 'sshPublicKeys';
  /**
   * @var string
   */
  public $clusterName;
  /**
   * @var string
   */
  public $compartmentId;
  /**
   * @var int
   */
  public $cpuCoreCount;
  public $dataStorageSizeTb;
  /**
   * @var int
   */
  public $dbNodeStorageSizeGb;
  /**
   * @var string[]
   */
  public $dbServerOcids;
  protected $diagnosticsDataCollectionOptionsType = DataCollectionOptions::class;
  protected $diagnosticsDataCollectionOptionsDataType = '';
  /**
   * @var string
   */
  public $diskRedundancy;
  /**
   * @var string
   */
  public $dnsListenerIp;
  /**
   * @var string
   */
  public $domain;
  /**
   * @var string
   */
  public $giVersion;
  /**
   * @var string
   */
  public $hostname;
  /**
   * @var string
   */
  public $hostnamePrefix;
  /**
   * @var string
   */
  public $licenseType;
  /**
   * @var bool
   */
  public $localBackupEnabled;
  /**
   * @var int
   */
  public $memorySizeGb;
  /**
   * @var int
   */
  public $nodeCount;
  /**
   * @var string
   */
  public $ociUrl;
  /**
   * @var string
   */
  public $ocid;
  /**
   * @var float
   */
  public $ocpuCount;
  /**
   * @var string
   */
  public $scanDns;
  /**
   * @var string
   */
  public $scanDnsRecordId;
  /**
   * @var string[]
   */
  public $scanIpIds;
  /**
   * @var int
   */
  public $scanListenerPortTcp;
  /**
   * @var int
   */
  public $scanListenerPortTcpSsl;
  /**
   * @var string
   */
  public $shape;
  /**
   * @var bool
   */
  public $sparseDiskgroupEnabled;
  /**
   * @var string[]
   */
  public $sshPublicKeys;
  /**
   * @var string
   */
  public $state;
  /**
   * @var int
   */
  public $storageSizeGb;
  /**
   * @var string
   */
  public $systemVersion;
  protected $timeZoneType = TimeZone::class;
  protected $timeZoneDataType = '';

  /**
   * @param string
   */
  public function setClusterName($clusterName)
  {
    $this->clusterName = $clusterName;
  }
  /**
   * @return string
   */
  public function getClusterName()
  {
    return $this->clusterName;
  }
  /**
   * @param string
   */
  public function setCompartmentId($compartmentId)
  {
    $this->compartmentId = $compartmentId;
  }
  /**
   * @return string
   */
  public function getCompartmentId()
  {
    return $this->compartmentId;
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
  public function setDataStorageSizeTb($dataStorageSizeTb)
  {
    $this->dataStorageSizeTb = $dataStorageSizeTb;
  }
  public function getDataStorageSizeTb()
  {
    return $this->dataStorageSizeTb;
  }
  /**
   * @param int
   */
  public function setDbNodeStorageSizeGb($dbNodeStorageSizeGb)
  {
    $this->dbNodeStorageSizeGb = $dbNodeStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getDbNodeStorageSizeGb()
  {
    return $this->dbNodeStorageSizeGb;
  }
  /**
   * @param string[]
   */
  public function setDbServerOcids($dbServerOcids)
  {
    $this->dbServerOcids = $dbServerOcids;
  }
  /**
   * @return string[]
   */
  public function getDbServerOcids()
  {
    return $this->dbServerOcids;
  }
  /**
   * @param DataCollectionOptions
   */
  public function setDiagnosticsDataCollectionOptions(DataCollectionOptions $diagnosticsDataCollectionOptions)
  {
    $this->diagnosticsDataCollectionOptions = $diagnosticsDataCollectionOptions;
  }
  /**
   * @return DataCollectionOptions
   */
  public function getDiagnosticsDataCollectionOptions()
  {
    return $this->diagnosticsDataCollectionOptions;
  }
  /**
   * @param string
   */
  public function setDiskRedundancy($diskRedundancy)
  {
    $this->diskRedundancy = $diskRedundancy;
  }
  /**
   * @return string
   */
  public function getDiskRedundancy()
  {
    return $this->diskRedundancy;
  }
  /**
   * @param string
   */
  public function setDnsListenerIp($dnsListenerIp)
  {
    $this->dnsListenerIp = $dnsListenerIp;
  }
  /**
   * @return string
   */
  public function getDnsListenerIp()
  {
    return $this->dnsListenerIp;
  }
  /**
   * @param string
   */
  public function setDomain($domain)
  {
    $this->domain = $domain;
  }
  /**
   * @return string
   */
  public function getDomain()
  {
    return $this->domain;
  }
  /**
   * @param string
   */
  public function setGiVersion($giVersion)
  {
    $this->giVersion = $giVersion;
  }
  /**
   * @return string
   */
  public function getGiVersion()
  {
    return $this->giVersion;
  }
  /**
   * @param string
   */
  public function setHostname($hostname)
  {
    $this->hostname = $hostname;
  }
  /**
   * @return string
   */
  public function getHostname()
  {
    return $this->hostname;
  }
  /**
   * @param string
   */
  public function setHostnamePrefix($hostnamePrefix)
  {
    $this->hostnamePrefix = $hostnamePrefix;
  }
  /**
   * @return string
   */
  public function getHostnamePrefix()
  {
    return $this->hostnamePrefix;
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
   * @param bool
   */
  public function setLocalBackupEnabled($localBackupEnabled)
  {
    $this->localBackupEnabled = $localBackupEnabled;
  }
  /**
   * @return bool
   */
  public function getLocalBackupEnabled()
  {
    return $this->localBackupEnabled;
  }
  /**
   * @param int
   */
  public function setMemorySizeGb($memorySizeGb)
  {
    $this->memorySizeGb = $memorySizeGb;
  }
  /**
   * @return int
   */
  public function getMemorySizeGb()
  {
    return $this->memorySizeGb;
  }
  /**
   * @param int
   */
  public function setNodeCount($nodeCount)
  {
    $this->nodeCount = $nodeCount;
  }
  /**
   * @return int
   */
  public function getNodeCount()
  {
    return $this->nodeCount;
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
   * @param float
   */
  public function setOcpuCount($ocpuCount)
  {
    $this->ocpuCount = $ocpuCount;
  }
  /**
   * @return float
   */
  public function getOcpuCount()
  {
    return $this->ocpuCount;
  }
  /**
   * @param string
   */
  public function setScanDns($scanDns)
  {
    $this->scanDns = $scanDns;
  }
  /**
   * @return string
   */
  public function getScanDns()
  {
    return $this->scanDns;
  }
  /**
   * @param string
   */
  public function setScanDnsRecordId($scanDnsRecordId)
  {
    $this->scanDnsRecordId = $scanDnsRecordId;
  }
  /**
   * @return string
   */
  public function getScanDnsRecordId()
  {
    return $this->scanDnsRecordId;
  }
  /**
   * @param string[]
   */
  public function setScanIpIds($scanIpIds)
  {
    $this->scanIpIds = $scanIpIds;
  }
  /**
   * @return string[]
   */
  public function getScanIpIds()
  {
    return $this->scanIpIds;
  }
  /**
   * @param int
   */
  public function setScanListenerPortTcp($scanListenerPortTcp)
  {
    $this->scanListenerPortTcp = $scanListenerPortTcp;
  }
  /**
   * @return int
   */
  public function getScanListenerPortTcp()
  {
    return $this->scanListenerPortTcp;
  }
  /**
   * @param int
   */
  public function setScanListenerPortTcpSsl($scanListenerPortTcpSsl)
  {
    $this->scanListenerPortTcpSsl = $scanListenerPortTcpSsl;
  }
  /**
   * @return int
   */
  public function getScanListenerPortTcpSsl()
  {
    return $this->scanListenerPortTcpSsl;
  }
  /**
   * @param string
   */
  public function setShape($shape)
  {
    $this->shape = $shape;
  }
  /**
   * @return string
   */
  public function getShape()
  {
    return $this->shape;
  }
  /**
   * @param bool
   */
  public function setSparseDiskgroupEnabled($sparseDiskgroupEnabled)
  {
    $this->sparseDiskgroupEnabled = $sparseDiskgroupEnabled;
  }
  /**
   * @return bool
   */
  public function getSparseDiskgroupEnabled()
  {
    return $this->sparseDiskgroupEnabled;
  }
  /**
   * @param string[]
   */
  public function setSshPublicKeys($sshPublicKeys)
  {
    $this->sshPublicKeys = $sshPublicKeys;
  }
  /**
   * @return string[]
   */
  public function getSshPublicKeys()
  {
    return $this->sshPublicKeys;
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
   * @param int
   */
  public function setStorageSizeGb($storageSizeGb)
  {
    $this->storageSizeGb = $storageSizeGb;
  }
  /**
   * @return int
   */
  public function getStorageSizeGb()
  {
    return $this->storageSizeGb;
  }
  /**
   * @param string
   */
  public function setSystemVersion($systemVersion)
  {
    $this->systemVersion = $systemVersion;
  }
  /**
   * @return string
   */
  public function getSystemVersion()
  {
    return $this->systemVersion;
  }
  /**
   * @param TimeZone
   */
  public function setTimeZone(TimeZone $timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return TimeZone
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudVmClusterProperties::class, 'Google_Service_OracleDatabase_CloudVmClusterProperties');
