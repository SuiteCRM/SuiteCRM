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

class CloudExadataInfrastructureProperties extends \Google\Collection
{
  protected $collection_key = 'customerContacts';
  /**
   * @var int
   */
  public $activatedStorageCount;
  /**
   * @var int
   */
  public $additionalStorageCount;
  /**
   * @var int
   */
  public $availableStorageSizeGb;
  /**
   * @var int
   */
  public $computeCount;
  /**
   * @var int
   */
  public $cpuCount;
  protected $customerContactsType = CustomerContact::class;
  protected $customerContactsDataType = 'array';
  public $dataStorageSizeTb;
  /**
   * @var int
   */
  public $dbNodeStorageSizeGb;
  /**
   * @var string
   */
  public $dbServerVersion;
  protected $maintenanceWindowType = MaintenanceWindow::class;
  protected $maintenanceWindowDataType = '';
  /**
   * @var int
   */
  public $maxCpuCount;
  public $maxDataStorageTb;
  /**
   * @var int
   */
  public $maxDbNodeStorageSizeGb;
  /**
   * @var int
   */
  public $maxMemoryGb;
  /**
   * @var int
   */
  public $memorySizeGb;
  /**
   * @var string
   */
  public $monthlyDbServerVersion;
  /**
   * @var string
   */
  public $monthlyStorageServerVersion;
  /**
   * @var string
   */
  public $nextMaintenanceRunId;
  /**
   * @var string
   */
  public $nextMaintenanceRunTime;
  /**
   * @var string
   */
  public $nextSecurityMaintenanceRunTime;
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
  public $shape;
  /**
   * @var string
   */
  public $state;
  /**
   * @var int
   */
  public $storageCount;
  /**
   * @var string
   */
  public $storageServerVersion;
  /**
   * @var int
   */
  public $totalStorageSizeGb;

  /**
   * @param int
   */
  public function setActivatedStorageCount($activatedStorageCount)
  {
    $this->activatedStorageCount = $activatedStorageCount;
  }
  /**
   * @return int
   */
  public function getActivatedStorageCount()
  {
    return $this->activatedStorageCount;
  }
  /**
   * @param int
   */
  public function setAdditionalStorageCount($additionalStorageCount)
  {
    $this->additionalStorageCount = $additionalStorageCount;
  }
  /**
   * @return int
   */
  public function getAdditionalStorageCount()
  {
    return $this->additionalStorageCount;
  }
  /**
   * @param int
   */
  public function setAvailableStorageSizeGb($availableStorageSizeGb)
  {
    $this->availableStorageSizeGb = $availableStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getAvailableStorageSizeGb()
  {
    return $this->availableStorageSizeGb;
  }
  /**
   * @param int
   */
  public function setComputeCount($computeCount)
  {
    $this->computeCount = $computeCount;
  }
  /**
   * @return int
   */
  public function getComputeCount()
  {
    return $this->computeCount;
  }
  /**
   * @param int
   */
  public function setCpuCount($cpuCount)
  {
    $this->cpuCount = $cpuCount;
  }
  /**
   * @return int
   */
  public function getCpuCount()
  {
    return $this->cpuCount;
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
   * @param string
   */
  public function setDbServerVersion($dbServerVersion)
  {
    $this->dbServerVersion = $dbServerVersion;
  }
  /**
   * @return string
   */
  public function getDbServerVersion()
  {
    return $this->dbServerVersion;
  }
  /**
   * @param MaintenanceWindow
   */
  public function setMaintenanceWindow(MaintenanceWindow $maintenanceWindow)
  {
    $this->maintenanceWindow = $maintenanceWindow;
  }
  /**
   * @return MaintenanceWindow
   */
  public function getMaintenanceWindow()
  {
    return $this->maintenanceWindow;
  }
  /**
   * @param int
   */
  public function setMaxCpuCount($maxCpuCount)
  {
    $this->maxCpuCount = $maxCpuCount;
  }
  /**
   * @return int
   */
  public function getMaxCpuCount()
  {
    return $this->maxCpuCount;
  }
  public function setMaxDataStorageTb($maxDataStorageTb)
  {
    $this->maxDataStorageTb = $maxDataStorageTb;
  }
  public function getMaxDataStorageTb()
  {
    return $this->maxDataStorageTb;
  }
  /**
   * @param int
   */
  public function setMaxDbNodeStorageSizeGb($maxDbNodeStorageSizeGb)
  {
    $this->maxDbNodeStorageSizeGb = $maxDbNodeStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getMaxDbNodeStorageSizeGb()
  {
    return $this->maxDbNodeStorageSizeGb;
  }
  /**
   * @param int
   */
  public function setMaxMemoryGb($maxMemoryGb)
  {
    $this->maxMemoryGb = $maxMemoryGb;
  }
  /**
   * @return int
   */
  public function getMaxMemoryGb()
  {
    return $this->maxMemoryGb;
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
   * @param string
   */
  public function setMonthlyDbServerVersion($monthlyDbServerVersion)
  {
    $this->monthlyDbServerVersion = $monthlyDbServerVersion;
  }
  /**
   * @return string
   */
  public function getMonthlyDbServerVersion()
  {
    return $this->monthlyDbServerVersion;
  }
  /**
   * @param string
   */
  public function setMonthlyStorageServerVersion($monthlyStorageServerVersion)
  {
    $this->monthlyStorageServerVersion = $monthlyStorageServerVersion;
  }
  /**
   * @return string
   */
  public function getMonthlyStorageServerVersion()
  {
    return $this->monthlyStorageServerVersion;
  }
  /**
   * @param string
   */
  public function setNextMaintenanceRunId($nextMaintenanceRunId)
  {
    $this->nextMaintenanceRunId = $nextMaintenanceRunId;
  }
  /**
   * @return string
   */
  public function getNextMaintenanceRunId()
  {
    return $this->nextMaintenanceRunId;
  }
  /**
   * @param string
   */
  public function setNextMaintenanceRunTime($nextMaintenanceRunTime)
  {
    $this->nextMaintenanceRunTime = $nextMaintenanceRunTime;
  }
  /**
   * @return string
   */
  public function getNextMaintenanceRunTime()
  {
    return $this->nextMaintenanceRunTime;
  }
  /**
   * @param string
   */
  public function setNextSecurityMaintenanceRunTime($nextSecurityMaintenanceRunTime)
  {
    $this->nextSecurityMaintenanceRunTime = $nextSecurityMaintenanceRunTime;
  }
  /**
   * @return string
   */
  public function getNextSecurityMaintenanceRunTime()
  {
    return $this->nextSecurityMaintenanceRunTime;
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
  public function setStorageCount($storageCount)
  {
    $this->storageCount = $storageCount;
  }
  /**
   * @return int
   */
  public function getStorageCount()
  {
    return $this->storageCount;
  }
  /**
   * @param string
   */
  public function setStorageServerVersion($storageServerVersion)
  {
    $this->storageServerVersion = $storageServerVersion;
  }
  /**
   * @return string
   */
  public function getStorageServerVersion()
  {
    return $this->storageServerVersion;
  }
  /**
   * @param int
   */
  public function setTotalStorageSizeGb($totalStorageSizeGb)
  {
    $this->totalStorageSizeGb = $totalStorageSizeGb;
  }
  /**
   * @return int
   */
  public function getTotalStorageSizeGb()
  {
    return $this->totalStorageSizeGb;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudExadataInfrastructureProperties::class, 'Google_Service_OracleDatabase_CloudExadataInfrastructureProperties');
