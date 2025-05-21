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

namespace Google\Service\MigrationCenterAPI;

class DiskEntry extends \Google\Model
{
  /**
   * @var string
   */
  public $capacityBytes;
  /**
   * @var string
   */
  public $diskLabel;
  /**
   * @var string
   */
  public $diskLabelType;
  /**
   * @var string
   */
  public $freeBytes;
  /**
   * @var string
   */
  public $hwAddress;
  /**
   * @var string
   */
  public $interfaceType;
  protected $partitionsType = DiskPartitionList::class;
  protected $partitionsDataType = '';
  protected $vmwareType = VmwareDiskConfig::class;
  protected $vmwareDataType = '';

  /**
   * @param string
   */
  public function setCapacityBytes($capacityBytes)
  {
    $this->capacityBytes = $capacityBytes;
  }
  /**
   * @return string
   */
  public function getCapacityBytes()
  {
    return $this->capacityBytes;
  }
  /**
   * @param string
   */
  public function setDiskLabel($diskLabel)
  {
    $this->diskLabel = $diskLabel;
  }
  /**
   * @return string
   */
  public function getDiskLabel()
  {
    return $this->diskLabel;
  }
  /**
   * @param string
   */
  public function setDiskLabelType($diskLabelType)
  {
    $this->diskLabelType = $diskLabelType;
  }
  /**
   * @return string
   */
  public function getDiskLabelType()
  {
    return $this->diskLabelType;
  }
  /**
   * @param string
   */
  public function setFreeBytes($freeBytes)
  {
    $this->freeBytes = $freeBytes;
  }
  /**
   * @return string
   */
  public function getFreeBytes()
  {
    return $this->freeBytes;
  }
  /**
   * @param string
   */
  public function setHwAddress($hwAddress)
  {
    $this->hwAddress = $hwAddress;
  }
  /**
   * @return string
   */
  public function getHwAddress()
  {
    return $this->hwAddress;
  }
  /**
   * @param string
   */
  public function setInterfaceType($interfaceType)
  {
    $this->interfaceType = $interfaceType;
  }
  /**
   * @return string
   */
  public function getInterfaceType()
  {
    return $this->interfaceType;
  }
  /**
   * @param DiskPartitionList
   */
  public function setPartitions(DiskPartitionList $partitions)
  {
    $this->partitions = $partitions;
  }
  /**
   * @return DiskPartitionList
   */
  public function getPartitions()
  {
    return $this->partitions;
  }
  /**
   * @param VmwareDiskConfig
   */
  public function setVmware(VmwareDiskConfig $vmware)
  {
    $this->vmware = $vmware;
  }
  /**
   * @return VmwareDiskConfig
   */
  public function getVmware()
  {
    return $this->vmware;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DiskEntry::class, 'Google_Service_MigrationCenterAPI_DiskEntry');
