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

class StageMetrics extends \Google\Model
{
  /**
   * @var string
   */
  public $diskBytesSpilled;
  /**
   * @var string
   */
  public $executorCpuTimeNanos;
  /**
   * @var string
   */
  public $executorDeserializeCpuTimeNanos;
  /**
   * @var string
   */
  public $executorDeserializeTimeMillis;
  /**
   * @var string
   */
  public $executorRunTimeMillis;
  /**
   * @var string
   */
  public $jvmGcTimeMillis;
  /**
   * @var string
   */
  public $memoryBytesSpilled;
  /**
   * @var string
   */
  public $peakExecutionMemoryBytes;
  /**
   * @var string
   */
  public $resultSerializationTimeMillis;
  /**
   * @var string
   */
  public $resultSize;
  protected $stageInputMetricsType = StageInputMetrics::class;
  protected $stageInputMetricsDataType = '';
  protected $stageOutputMetricsType = StageOutputMetrics::class;
  protected $stageOutputMetricsDataType = '';
  protected $stageShuffleReadMetricsType = StageShuffleReadMetrics::class;
  protected $stageShuffleReadMetricsDataType = '';
  protected $stageShuffleWriteMetricsType = StageShuffleWriteMetrics::class;
  protected $stageShuffleWriteMetricsDataType = '';

  /**
   * @param string
   */
  public function setDiskBytesSpilled($diskBytesSpilled)
  {
    $this->diskBytesSpilled = $diskBytesSpilled;
  }
  /**
   * @return string
   */
  public function getDiskBytesSpilled()
  {
    return $this->diskBytesSpilled;
  }
  /**
   * @param string
   */
  public function setExecutorCpuTimeNanos($executorCpuTimeNanos)
  {
    $this->executorCpuTimeNanos = $executorCpuTimeNanos;
  }
  /**
   * @return string
   */
  public function getExecutorCpuTimeNanos()
  {
    return $this->executorCpuTimeNanos;
  }
  /**
   * @param string
   */
  public function setExecutorDeserializeCpuTimeNanos($executorDeserializeCpuTimeNanos)
  {
    $this->executorDeserializeCpuTimeNanos = $executorDeserializeCpuTimeNanos;
  }
  /**
   * @return string
   */
  public function getExecutorDeserializeCpuTimeNanos()
  {
    return $this->executorDeserializeCpuTimeNanos;
  }
  /**
   * @param string
   */
  public function setExecutorDeserializeTimeMillis($executorDeserializeTimeMillis)
  {
    $this->executorDeserializeTimeMillis = $executorDeserializeTimeMillis;
  }
  /**
   * @return string
   */
  public function getExecutorDeserializeTimeMillis()
  {
    return $this->executorDeserializeTimeMillis;
  }
  /**
   * @param string
   */
  public function setExecutorRunTimeMillis($executorRunTimeMillis)
  {
    $this->executorRunTimeMillis = $executorRunTimeMillis;
  }
  /**
   * @return string
   */
  public function getExecutorRunTimeMillis()
  {
    return $this->executorRunTimeMillis;
  }
  /**
   * @param string
   */
  public function setJvmGcTimeMillis($jvmGcTimeMillis)
  {
    $this->jvmGcTimeMillis = $jvmGcTimeMillis;
  }
  /**
   * @return string
   */
  public function getJvmGcTimeMillis()
  {
    return $this->jvmGcTimeMillis;
  }
  /**
   * @param string
   */
  public function setMemoryBytesSpilled($memoryBytesSpilled)
  {
    $this->memoryBytesSpilled = $memoryBytesSpilled;
  }
  /**
   * @return string
   */
  public function getMemoryBytesSpilled()
  {
    return $this->memoryBytesSpilled;
  }
  /**
   * @param string
   */
  public function setPeakExecutionMemoryBytes($peakExecutionMemoryBytes)
  {
    $this->peakExecutionMemoryBytes = $peakExecutionMemoryBytes;
  }
  /**
   * @return string
   */
  public function getPeakExecutionMemoryBytes()
  {
    return $this->peakExecutionMemoryBytes;
  }
  /**
   * @param string
   */
  public function setResultSerializationTimeMillis($resultSerializationTimeMillis)
  {
    $this->resultSerializationTimeMillis = $resultSerializationTimeMillis;
  }
  /**
   * @return string
   */
  public function getResultSerializationTimeMillis()
  {
    return $this->resultSerializationTimeMillis;
  }
  /**
   * @param string
   */
  public function setResultSize($resultSize)
  {
    $this->resultSize = $resultSize;
  }
  /**
   * @return string
   */
  public function getResultSize()
  {
    return $this->resultSize;
  }
  /**
   * @param StageInputMetrics
   */
  public function setStageInputMetrics(StageInputMetrics $stageInputMetrics)
  {
    $this->stageInputMetrics = $stageInputMetrics;
  }
  /**
   * @return StageInputMetrics
   */
  public function getStageInputMetrics()
  {
    return $this->stageInputMetrics;
  }
  /**
   * @param StageOutputMetrics
   */
  public function setStageOutputMetrics(StageOutputMetrics $stageOutputMetrics)
  {
    $this->stageOutputMetrics = $stageOutputMetrics;
  }
  /**
   * @return StageOutputMetrics
   */
  public function getStageOutputMetrics()
  {
    return $this->stageOutputMetrics;
  }
  /**
   * @param StageShuffleReadMetrics
   */
  public function setStageShuffleReadMetrics(StageShuffleReadMetrics $stageShuffleReadMetrics)
  {
    $this->stageShuffleReadMetrics = $stageShuffleReadMetrics;
  }
  /**
   * @return StageShuffleReadMetrics
   */
  public function getStageShuffleReadMetrics()
  {
    return $this->stageShuffleReadMetrics;
  }
  /**
   * @param StageShuffleWriteMetrics
   */
  public function setStageShuffleWriteMetrics(StageShuffleWriteMetrics $stageShuffleWriteMetrics)
  {
    $this->stageShuffleWriteMetrics = $stageShuffleWriteMetrics;
  }
  /**
   * @return StageShuffleWriteMetrics
   */
  public function getStageShuffleWriteMetrics()
  {
    return $this->stageShuffleWriteMetrics;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StageMetrics::class, 'Google_Service_Dataproc_StageMetrics');
