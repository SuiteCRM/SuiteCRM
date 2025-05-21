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

class AzureVmDetails extends \Google\Collection
{
  protected $collection_key = 'disks';
  /**
   * @var string
   */
  public $architecture;
  /**
   * @var string
   */
  public $bootOption;
  /**
   * @var string
   */
  public $committedStorageMb;
  /**
   * @var string
   */
  public $computerName;
  /**
   * @var int
   */
  public $cpuCount;
  /**
   * @var int
   */
  public $diskCount;
  protected $disksType = Disk::class;
  protected $disksDataType = 'array';
  /**
   * @var int
   */
  public $memoryMb;
  protected $osDescriptionType = OSDescription::class;
  protected $osDescriptionDataType = '';
  protected $osDiskType = OSDisk::class;
  protected $osDiskDataType = '';
  /**
   * @var string
   */
  public $powerState;
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $vmId;
  /**
   * @var string
   */
  public $vmSize;

  /**
   * @param string
   */
  public function setArchitecture($architecture)
  {
    $this->architecture = $architecture;
  }
  /**
   * @return string
   */
  public function getArchitecture()
  {
    return $this->architecture;
  }
  /**
   * @param string
   */
  public function setBootOption($bootOption)
  {
    $this->bootOption = $bootOption;
  }
  /**
   * @return string
   */
  public function getBootOption()
  {
    return $this->bootOption;
  }
  /**
   * @param string
   */
  public function setCommittedStorageMb($committedStorageMb)
  {
    $this->committedStorageMb = $committedStorageMb;
  }
  /**
   * @return string
   */
  public function getCommittedStorageMb()
  {
    return $this->committedStorageMb;
  }
  /**
   * @param string
   */
  public function setComputerName($computerName)
  {
    $this->computerName = $computerName;
  }
  /**
   * @return string
   */
  public function getComputerName()
  {
    return $this->computerName;
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
   * @param int
   */
  public function setDiskCount($diskCount)
  {
    $this->diskCount = $diskCount;
  }
  /**
   * @return int
   */
  public function getDiskCount()
  {
    return $this->diskCount;
  }
  /**
   * @param Disk[]
   */
  public function setDisks($disks)
  {
    $this->disks = $disks;
  }
  /**
   * @return Disk[]
   */
  public function getDisks()
  {
    return $this->disks;
  }
  /**
   * @param int
   */
  public function setMemoryMb($memoryMb)
  {
    $this->memoryMb = $memoryMb;
  }
  /**
   * @return int
   */
  public function getMemoryMb()
  {
    return $this->memoryMb;
  }
  /**
   * @param OSDescription
   */
  public function setOsDescription(OSDescription $osDescription)
  {
    $this->osDescription = $osDescription;
  }
  /**
   * @return OSDescription
   */
  public function getOsDescription()
  {
    return $this->osDescription;
  }
  /**
   * @param OSDisk
   */
  public function setOsDisk(OSDisk $osDisk)
  {
    $this->osDisk = $osDisk;
  }
  /**
   * @return OSDisk
   */
  public function getOsDisk()
  {
    return $this->osDisk;
  }
  /**
   * @param string
   */
  public function setPowerState($powerState)
  {
    $this->powerState = $powerState;
  }
  /**
   * @return string
   */
  public function getPowerState()
  {
    return $this->powerState;
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
   * @param string
   */
  public function setVmId($vmId)
  {
    $this->vmId = $vmId;
  }
  /**
   * @return string
   */
  public function getVmId()
  {
    return $this->vmId;
  }
  /**
   * @param string
   */
  public function setVmSize($vmSize)
  {
    $this->vmSize = $vmSize;
  }
  /**
   * @return string
   */
  public function getVmSize()
  {
    return $this->vmSize;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AzureVmDetails::class, 'Google_Service_VMMigrationService_AzureVmDetails');
