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

class DiskPartition extends \Google\Model
{
  /**
   * @var string
   */
  public $capacityBytes;
  /**
   * @var string
   */
  public $fileSystem;
  /**
   * @var string
   */
  public $freeBytes;
  /**
   * @var string
   */
  public $mountPoint;
  protected $subPartitionsType = DiskPartitionList::class;
  protected $subPartitionsDataType = '';
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $uuid;

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
  public function setFileSystem($fileSystem)
  {
    $this->fileSystem = $fileSystem;
  }
  /**
   * @return string
   */
  public function getFileSystem()
  {
    return $this->fileSystem;
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
  public function setMountPoint($mountPoint)
  {
    $this->mountPoint = $mountPoint;
  }
  /**
   * @return string
   */
  public function getMountPoint()
  {
    return $this->mountPoint;
  }
  /**
   * @param DiskPartitionList
   */
  public function setSubPartitions(DiskPartitionList $subPartitions)
  {
    $this->subPartitions = $subPartitions;
  }
  /**
   * @return DiskPartitionList
   */
  public function getSubPartitions()
  {
    return $this->subPartitions;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setUuid($uuid)
  {
    $this->uuid = $uuid;
  }
  /**
   * @return string
   */
  public function getUuid()
  {
    return $this->uuid;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DiskPartition::class, 'Google_Service_MigrationCenterAPI_DiskPartition');
