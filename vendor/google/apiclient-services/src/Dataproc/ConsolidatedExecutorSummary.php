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

class ConsolidatedExecutorSummary extends \Google\Model
{
  /**
   * @var int
   */
  public $activeTasks;
  /**
   * @var int
   */
  public $completedTasks;
  /**
   * @var int
   */
  public $count;
  /**
   * @var string
   */
  public $diskUsed;
  /**
   * @var int
   */
  public $failedTasks;
  /**
   * @var int
   */
  public $isExcluded;
  /**
   * @var string
   */
  public $maxMemory;
  protected $memoryMetricsType = MemoryMetrics::class;
  protected $memoryMetricsDataType = '';
  /**
   * @var string
   */
  public $memoryUsed;
  /**
   * @var int
   */
  public $rddBlocks;
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
   * @param int
   */
  public function setCount($count)
  {
    $this->count = $count;
  }
  /**
   * @return int
   */
  public function getCount()
  {
    return $this->count;
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
   * @param int
   */
  public function setIsExcluded($isExcluded)
  {
    $this->isExcluded = $isExcluded;
  }
  /**
   * @return int
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
class_alias(ConsolidatedExecutorSummary::class, 'Google_Service_Dataproc_ConsolidatedExecutorSummary');
