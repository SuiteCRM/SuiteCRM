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

class ComputeEngineShapeDescriptor extends \Google\Collection
{
  protected $collection_key = 'storage';
  /**
   * @var int
   */
  public $logicalCoreCount;
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var int
   */
  public $memoryMb;
  /**
   * @var int
   */
  public $physicalCoreCount;
  /**
   * @var string
   */
  public $series;
  protected $storageType = ComputeStorageDescriptor::class;
  protected $storageDataType = 'array';

  /**
   * @param int
   */
  public function setLogicalCoreCount($logicalCoreCount)
  {
    $this->logicalCoreCount = $logicalCoreCount;
  }
  /**
   * @return int
   */
  public function getLogicalCoreCount()
  {
    return $this->logicalCoreCount;
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
   * @param int
   */
  public function setPhysicalCoreCount($physicalCoreCount)
  {
    $this->physicalCoreCount = $physicalCoreCount;
  }
  /**
   * @return int
   */
  public function getPhysicalCoreCount()
  {
    return $this->physicalCoreCount;
  }
  /**
   * @param string
   */
  public function setSeries($series)
  {
    $this->series = $series;
  }
  /**
   * @return string
   */
  public function getSeries()
  {
    return $this->series;
  }
  /**
   * @param ComputeStorageDescriptor[]
   */
  public function setStorage($storage)
  {
    $this->storage = $storage;
  }
  /**
   * @return ComputeStorageDescriptor[]
   */
  public function getStorage()
  {
    return $this->storage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ComputeEngineShapeDescriptor::class, 'Google_Service_MigrationCenterAPI_ComputeEngineShapeDescriptor');
