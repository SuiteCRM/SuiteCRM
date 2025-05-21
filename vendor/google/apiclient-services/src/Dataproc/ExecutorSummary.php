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

namespace Google\Service\Dataproc;

class ExecutorSummary extends \Google\Collection
{
  protected $collection_key = 'excludedInStages';
  /**
   * @var int
   */
  public $activeTasks;
  /**
   * @var string
   */
  public $addTime;
  /**
   * @var string[]
   */
  public $attributes;
  /**
   * @var int
   */
  public $completedTasks;
  /**
   * @var string
   */
  public $diskUsed;
  /**
   * @var string[]
   */
  public $excludedInStages;
  /**
   * @var string
   */
  public $executorId;
  /**
   * @var string[]
   */
  public $executorLogs;
  /**
   * @var int
   */
  public $failedTasks;
  /**
   * @var string
   */
  public $hostPort;
  /**
   * @var bool
   */
  public $isActive;
  /**
   * @var bool
   */
  public $isExcluded;
  /**
   * @var string
   */
  public $maxMemory;
  /**
   * @var int
   */
  public $maxTasks;
  protected $memoryMetricsType = MemoryMetrics::class;
  protected $memoryMetricsDataType = '';
  /**
   * @var string
   */
  public $memoryUsed;
  protected $peakMemoryMetricsType = ExecutorMetrics::class;
  protected $peakMemoryMetricsDataType = '';
  /**
   * @var int
   */
  public $rddBlocks;
  /**
   * @var string
   */
  public $removeReason;
  /**
   * @var string
   */
  public $removeTime;
  /**
   * @var int
   */
  public $resourceProfileId;
  protected $resourcesType = ResourceInformation::class;
  protected $resourcesDataType = 'map';
  /**
   * @var int
   */
  public $totalCores;
  /**
   * @var string
   */
  public $totalDurationMillis;
  /**
   * @var string
   */
  public $totalGcTimeMillis;
  /**
   * @var string
   */
  public $totalInputBytes;
  /**
   * @var string
   */
  public $totalShuffleRead;
  /**
   * @var string
   */
  public $totalShuffleWrite;
  /**
   * @var int
   */
  public $totalTasks;

  /**
   * @param int
   */
  public function setActiveTasks($activeTasks)
  {
    $this->activeTasks = $activeTasks;
  }
  /**
   * @return int
   */
  public function getActiveTasks()
  {
    return $this->activeTasks;
  }
  /**
   * @param string
   */
  public function setAddTime($addTime)
  {
    $this->addTime = $addTime;
  }
  /**
   * @return string
   */
  public function getAddTime()
  {
    return $this->addTime;
  }
  /**
   * @param string[]
   */
  public function setAttributes($attributes)
  {
    $this->attributes = $attributes;
  }
  /**
   * @return string[]
   */
  public function getAttributes()
  {
    return $this->attributes;
  }
  /**
   * @param int
   */
  public function setCompletedTasks($completedTasks)
  {
    $this->completedTasks = $completedTasks;
  }
  /**
   * @return int
   */
  public function getCompletedTasks()
  {
    return $this->completedTasks;
  }
  /**
   * @param string
   */
  public function setDiskUsed($diskUsed)
  {
    $this->diskUsed = $diskUsed;
  }
  /**
   * @return string
   */
  public function getDiskUsed()
  {
    return $this->diskUsed;
  }
  /**
   * @param string[]
   */
  public function setExcludedInStages($excludedInStages)
  {
    $this->excludedInStages = $excludedInStages;
  }
  /**
   * @return string[]
   */
  public function getExcludedInStages()
  {
    return $this->excludedInStages;
  }
  /**
   * @param string
   */
  public function setExecutorId($executorId)
  {
    $this->executorId = $executorId;
  }
  /**
   * @return string
   */
  public function getExecutorId()
  {
    return $this->executorId;
  }
  /**
   * @param string[]
   */
  public function setExecutorLogs($executorLogs)
  {
    $this->executorLogs = $executorLogs;
  }
  /**
   * @return string[]
   */
  public function getExecutorLogs()
  {
    return $this->executorLogs;
  }
  /**
   * @param int
   */
  public function setFailedTasks($failedTasks)
  {
    $this->failedTasks = $failedTasks;
  }
  /**
   * @return int
   */
  public function getFailedTasks()
  {
    return $this->failedTasks;
  }
  /**
   * @param string
   */
  public function setHostPort($hostPort)
  {
    $this->hostPort = $hostPort;
  }
  /**
   * @return string
   */
  public function getHostPort()
  {
    return $this->hostPort;
  }
  /**
   * @param bool
   */
  public function setIsActive($isActive)
  {
    $this->isActive = $isActive;
  }
  /**
   * @return bool
   */
  public function getIsActive()
  {
    return $this->isActive;
  }
  /**
   * @param bool
   */
  public function setIsExcluded($isExcluded)
  {
    $this->isExcluded = $isExcluded;
  }
  /**
   * @return bool
   */
  public function getIsExcluded()
  {
    return $this->isExcluded;
  }
  /**
   * @param string
   */
  public function setMaxMemory($maxMemory)
  {
    $this->maxMemory = $maxMemory;
  }
  /**
   * @return string
   */
  public function getMaxMemory()
  {
    return $this->maxMemory;
  }
  /**
   * @param int
   */
  public function setMaxTasks($maxTasks)
  {
    $this->maxTasks = $maxTasks;
  }
  /**
   * @return int
   */
  public function getMaxTasks()
  {
    return $this->maxTasks;
  }
  /**
   * @param MemoryMetrics
   */
  public function setMemoryMetrics(MemoryMetrics $memoryMetrics)
  {
    $this->memoryMetrics = $memoryMetrics;
  }
  /**
   * @return MemoryMetrics
   */
  public function getMemoryMetrics()
  {
    return $this->memoryMetrics;
  }
  /**
   * @param string
   */
  public function setMemoryUsed($memoryUsed)
  {
    $this->memoryUsed = $memoryUsed;
  }
  /**
   * @return string
   */
  public function getMemoryUsed()
  {
    return $this->memoryUsed;
  }
  /**
   * @param ExecutorMetrics
   */
  public function setPeakMemoryMetrics(ExecutorMetrics $peakMemoryMetrics)
  {
    $this->peakMemoryMetrics = $peakMemoryMetrics;
  }
  /**
   * @return ExecutorMetrics
   */
  public function getPeakMemoryMetrics()
  {
    return $this->peakMemoryMetrics;
  }
  /**
   * @param int
   */
  public function setRddBlocks($rddBlocks)
  {
    $this->rddBlocks = $rddBlocks;
  }
  /**
   * @return int
   */
  public function getRddBlocks()
  {
    return $this->rddBlocks;
  }
  /**
   * @param string
   */
  public function setRemoveReason($removeReason)
  {
    $this->removeReason = $removeReason;
  }
  /**
   * @return string
   */
  public function getRemoveReason()
  {
    return $this->removeReason;
  }
  /**
   * @param string
   */
  public function setRemoveTime($removeTime)
  {
    $this->removeTime = $removeTime;
  }
  /**
   * @return string
   */
  public function getRemoveTime()
  {
    return $this->removeTime;
  }
  /**
   * @param int
   */
  public function setResourceProfileId($resourceProfileId)
  {
    $this->resourceProfileId = $resourceProfileId;
  }
  /**
   * @return int
   */
  public function getResourceProfileId()
  {
    return $this->resourceProfileId;
  }
  /**
   * @param ResourceInformation[]
   */
  public function setResources($resources)
  {
    $this->resources = $resources;
  }
  /**
   * @return ResourceInformation[]
   */
  public function getResources()
  {
    return $this->resources;
  }
  /**
   * @param int
   */
  public function setTotalCores($totalCores)
  {
    $this->totalCores = $totalCores;
  }
  /**
   * @return int
   */
  public function getTotalCores()
  {
    return $this->totalCores;
  }
  /**
   * @param string
   */
  public function setTotalDurationMillis($totalDurationMillis)
  {
    $this->totalDurationMillis = $totalDurationMillis;
  }
  /**
   * @return string
   */
  public function getTotalDurationMillis()
  {
    return $this->totalDurationMillis;
  }
  /**
   * @param string
   */
  public function setTotalGcTimeMillis($totalGcTimeMillis)
  {
    $this->totalGcTimeMillis = $totalGcTimeMillis;
  }
  /**
   * @return string
   */
  public function getTotalGcTimeMillis()
  {
    return $this->totalGcTimeMillis;
  }
  /**
   * @param string
   */
  public function setTotalInputBytes($totalInputBytes)
  {
    $this->totalInputBytes = $totalInputBytes;
  }
  /**
   * @return string
   */
  public function getTotalInputBytes()
  {
    return $this->totalInputBytes;
  }
  /**
   * @param string
   */
  public function setTotalShuffleRead($totalShuffleRead)
  {
    $this->totalShuffleRead = $totalShuffleRead;
  }
  /**
   * @return string
   */
  public function getTotalShuffleRead()
  {
    return $this->totalShuffleRead;
  }
  /**
   * @param string
   */
  public function setTotalShuffleWrite($totalShuffleWrite)
  {
    $this->totalShuffleWrite = $totalShuffleWrite;
  }
  /**
   * @return string
   */
  public function getTotalShuffleWrite()
  {
    return $this->totalShuffleWrite;
  }
  /**
   * @param int
   */
  public function setTotalTasks($totalTasks)
  {
    $this->totalTasks = $totalTasks;
  }
  /**
   * @return int
   */
  public function getTotalTasks()
  {
    return $this->totalTasks;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExecutorSummary::class, 'Google_Service_Dataproc_ExecutorSummary');
