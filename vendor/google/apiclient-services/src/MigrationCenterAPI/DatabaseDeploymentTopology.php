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

class DatabaseDeploymentTopology extends \Google\Collection
{
  protected $collection_key = 'instances';
  /**
   * @var int
   */
  public $coreCount;
  /**
   * @var int
   */
  public $coreLimit;
  /**
   * @var string
   */
  public $diskAllocatedBytes;
  /**
   * @var string
   */
  public $diskUsedBytes;
  protected $instancesType = DatabaseInstance::class;
  protected $instancesDataType = 'array';
  /**
   * @var string
   */
  public $memoryBytes;
  /**
   * @var string
   */
  public $memoryLimitBytes;
  /**
   * @var int
   */
  public $physicalCoreCount;
  /**
   * @var int
   */
  public $physicalCoreLimit;

  /**
   * @param int
   */
  public function setCoreCount($coreCount)
  {
    $this->coreCount = $coreCount;
  }
  /**
   * @return int
   */
  public function getCoreCount()
  {
    return $this->coreCount;
  }
  /**
   * @param int
   */
  public function setCoreLimit($coreLimit)
  {
    $this->coreLimit = $coreLimit;
  }
  /**
   * @return int
   */
  public function getCoreLimit()
  {
    return $this->coreLimit;
  }
  /**
   * @param string
   */
  public function setDiskAllocatedBytes($diskAllocatedBytes)
  {
    $this->diskAllocatedBytes = $diskAllocatedBytes;
  }
  /**
   * @return string
   */
  public function getDiskAllocatedBytes()
  {
    return $this->diskAllocatedBytes;
  }
  /**
   * @param string
   */
  public function setDiskUsedBytes($diskUsedBytes)
  {
    $this->diskUsedBytes = $diskUsedBytes;
  }
  /**
   * @return string
   */
  public function getDiskUsedBytes()
  {
    return $this->diskUsedBytes;
  }
  /**
   * @param DatabaseInstance[]
   */
  public function setInstances($instances)
  {
    $this->instances = $instances;
  }
  /**
   * @return DatabaseInstance[]
   */
  public function getInstances()
  {
    return $this->instances;
  }
  /**
   * @param string
   */
  public function setMemoryBytes($memoryBytes)
  {
    $this->memoryBytes = $memoryBytes;
  }
  /**
   * @return string
   */
  public function getMemoryBytes()
  {
    return $this->memoryBytes;
  }
  /**
   * @param string
   */
  public function setMemoryLimitBytes($memoryLimitBytes)
  {
    $this->memoryLimitBytes = $memoryLimitBytes;
  }
  /**
   * @return string
   */
  public function getMemoryLimitBytes()
  {
    return $this->memoryLimitBytes;
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
   * @param int
   */
  public function setPhysicalCoreLimit($physicalCoreLimit)
  {
    $this->physicalCoreLimit = $physicalCoreLimit;
  }
  /**
   * @return int
   */
  public function getPhysicalCoreLimit()
  {
    return $this->physicalCoreLimit;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseDeploymentTopology::class, 'Google_Service_MigrationCenterAPI_DatabaseDeploymentTopology');
