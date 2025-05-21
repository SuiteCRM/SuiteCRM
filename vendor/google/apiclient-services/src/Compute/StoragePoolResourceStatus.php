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

namespace Google\Service\Compute;

class StoragePoolResourceStatus extends \Google\Model
{
  /**
   * @var string
   */
  public $diskCount;
  /**
   * @var string
   */
  public $lastResizeTimestamp;
  /**
   * @var string
   */
  public $maxTotalProvisionedDiskCapacityGb;
  /**
   * @var string
   */
  public $poolUsedCapacityBytes;
  /**
   * @var string
   */
  public $poolUsedIops;
  /**
   * @var string
   */
  public $poolUsedThroughput;
  /**
   * @var string
   */
  public $poolUserWrittenBytes;
  /**
   * @var string
   */
  public $totalProvisionedDiskCapacityGb;
  /**
   * @var string
   */
  public $totalProvisionedDiskIops;
  /**
   * @var string
   */
  public $totalProvisionedDiskThroughput;

  /**
   * @param string
   */
  public function setDiskCount($diskCount)
  {
    $this->diskCount = $diskCount;
  }
  /**
   * @return string
   */
  public function getDiskCount()
  {
    return $this->diskCount;
  }
  /**
   * @param string
   */
  public function setLastResizeTimestamp($lastResizeTimestamp)
  {
    $this->lastResizeTimestamp = $lastResizeTimestamp;
  }
  /**
   * @return string
   */
  public function getLastResizeTimestamp()
  {
    return $this->lastResizeTimestamp;
  }
  /**
   * @param string
   */
  public function setMaxTotalProvisionedDiskCapacityGb($maxTotalProvisionedDiskCapacityGb)
  {
    $this->maxTotalProvisionedDiskCapacityGb = $maxTotalProvisionedDiskCapacityGb;
  }
  /**
   * @return string
   */
  public function getMaxTotalProvisionedDiskCapacityGb()
  {
    return $this->maxTotalProvisionedDiskCapacityGb;
  }
  /**
   * @param string
   */
  public function setPoolUsedCapacityBytes($poolUsedCapacityBytes)
  {
    $this->poolUsedCapacityBytes = $poolUsedCapacityBytes;
  }
  /**
   * @return string
   */
  public function getPoolUsedCapacityBytes()
  {
    return $this->poolUsedCapacityBytes;
  }
  /**
   * @param string
   */
  public function setPoolUsedIops($poolUsedIops)
  {
    $this->poolUsedIops = $poolUsedIops;
  }
  /**
   * @return string
   */
  public function getPoolUsedIops()
  {
    return $this->poolUsedIops;
  }
  /**
   * @param string
   */
  public function setPoolUsedThroughput($poolUsedThroughput)
  {
    $this->poolUsedThroughput = $poolUsedThroughput;
  }
  /**
   * @return string
   */
  public function getPoolUsedThroughput()
  {
    return $this->poolUsedThroughput;
  }
  /**
   * @param string
   */
  public function setPoolUserWrittenBytes($poolUserWrittenBytes)
  {
    $this->poolUserWrittenBytes = $poolUserWrittenBytes;
  }
  /**
   * @return string
   */
  public function getPoolUserWrittenBytes()
  {
    return $this->poolUserWrittenBytes;
  }
  /**
   * @param string
   */
  public function setTotalProvisionedDiskCapacityGb($totalProvisionedDiskCapacityGb)
  {
    $this->totalProvisionedDiskCapacityGb = $totalProvisionedDiskCapacityGb;
  }
  /**
   * @return string
   */
  public function getTotalProvisionedDiskCapacityGb()
  {
    return $this->totalProvisionedDiskCapacityGb;
  }
  /**
   * @param string
   */
  public function setTotalProvisionedDiskIops($totalProvisionedDiskIops)
  {
    $this->totalProvisionedDiskIops = $totalProvisionedDiskIops;
  }
  /**
   * @return string
   */
  public function getTotalProvisionedDiskIops()
  {
    return $this->totalProvisionedDiskIops;
  }
  /**
   * @param string
   */
  public function setTotalProvisionedDiskThroughput($totalProvisionedDiskThroughput)
  {
    $this->totalProvisionedDiskThroughput = $totalProvisionedDiskThroughput;
  }
  /**
   * @return string
   */
  public function getTotalProvisionedDiskThroughput()
  {
    return $this->totalProvisionedDiskThroughput;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StoragePoolResourceStatus::class, 'Google_Service_Compute_StoragePoolResourceStatus');
