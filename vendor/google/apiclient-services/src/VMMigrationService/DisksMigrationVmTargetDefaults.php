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

class DisksMigrationVmTargetDefaults extends \Google\Collection
{
  protected $collection_key = 'networkTags';
  /**
   * @var string[]
   */
  public $additionalLicenses;
  protected $bootDiskDefaultsType = BootDiskDefaults::class;
  protected $bootDiskDefaultsDataType = '';
  protected $computeSchedulingType = ComputeScheduling::class;
  protected $computeSchedulingDataType = '';
  /**
   * @var bool
   */
  public $enableIntegrityMonitoring;
  /**
   * @var bool
   */
  public $enableVtpm;
  protected $encryptionType = Encryption::class;
  protected $encryptionDataType = '';
  /**
   * @var string
   */
  public $hostname;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var string
   */
  public $machineTypeSeries;
  /**
   * @var string[]
   */
  public $metadata;
  protected $networkInterfacesType = NetworkInterface::class;
  protected $networkInterfacesDataType = 'array';
  /**
   * @var string[]
   */
  public $networkTags;
  /**
   * @var bool
   */
  public $secureBoot;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $vmName;

  /**
   * @param string[]
   */
  public function setAdditionalLicenses($additionalLicenses)
  {
    $this->additionalLicenses = $additionalLicenses;
  }
  /**
   * @return string[]
   */
  public function getAdditionalLicenses()
  {
    return $this->additionalLicenses;
  }
  /**
   * @param BootDiskDefaults
   */
  public function setBootDiskDefaults(BootDiskDefaults $bootDiskDefaults)
  {
    $this->bootDiskDefaults = $bootDiskDefaults;
  }
  /**
   * @return BootDiskDefaults
   */
  public function getBootDiskDefaults()
  {
    return $this->bootDiskDefaults;
  }
  /**
   * @param ComputeScheduling
   */
  public function setComputeScheduling(ComputeScheduling $computeScheduling)
  {
    $this->computeScheduling = $computeScheduling;
  }
  /**
   * @return ComputeScheduling
   */
  public function getComputeScheduling()
  {
    return $this->computeScheduling;
  }
  /**
   * @param bool
   */
  public function setEnableIntegrityMonitoring($enableIntegrityMonitoring)
  {
    $this->enableIntegrityMonitoring = $enableIntegrityMonitoring;
  }
  /**
   * @return bool
   */
  public function getEnableIntegrityMonitoring()
  {
    return $this->enableIntegrityMonitoring;
  }
  /**
   * @param bool
   */
  public function setEnableVtpm($enableVtpm)
  {
    $this->enableVtpm = $enableVtpm;
  }
  /**
   * @return bool
   */
  public function getEnableVtpm()
  {
    return $this->enableVtpm;
  }
  /**
   * @param Encryption
   */
  public function setEncryption(Encryption $encryption)
  {
    $this->encryption = $encryption;
  }
  /**
   * @return Encryption
   */
  public function getEncryption()
  {
    return $this->encryption;
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
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param string
   */
  public function setMachineTypeSeries($machineTypeSeries)
  {
    $this->machineTypeSeries = $machineTypeSeries;
  }
  /**
   * @return string
   */
  public function getMachineTypeSeries()
  {
    return $this->machineTypeSeries;
  }
  /**
   * @param string[]
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return string[]
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param NetworkInterface[]
   */
  public function setNetworkInterfaces($networkInterfaces)
  {
    $this->networkInterfaces = $networkInterfaces;
  }
  /**
   * @return NetworkInterface[]
   */
  public function getNetworkInterfaces()
  {
    return $this->networkInterfaces;
  }
  /**
   * @param string[]
   */
  public function setNetworkTags($networkTags)
  {
    $this->networkTags = $networkTags;
  }
  /**
   * @return string[]
   */
  public function getNetworkTags()
  {
    return $this->networkTags;
  }
  /**
   * @param bool
   */
  public function setSecureBoot($secureBoot)
  {
    $this->secureBoot = $secureBoot;
  }
  /**
   * @return bool
   */
  public function getSecureBoot()
  {
    return $this->secureBoot;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param string
   */
  public function setVmName($vmName)
  {
    $this->vmName = $vmName;
  }
  /**
   * @return string
   */
  public function getVmName()
  {
    return $this->vmName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DisksMigrationVmTargetDefaults::class, 'Google_Service_VMMigrationService_DisksMigrationVmTargetDefaults');
