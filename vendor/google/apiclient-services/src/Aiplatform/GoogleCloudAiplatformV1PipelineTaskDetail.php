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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1PipelineTaskDetail extends \Google\Collection
{
  protected $collection_key = 'pipelineTaskStatus';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $endTime;
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  protected $executionType = GoogleCloudAiplatformV1Execution::class;
  protected $executionDataType = '';
  protected $executorDetailType = GoogleCloudAiplatformV1PipelineTaskExecutorDetail::class;
  protected $executorDetailDataType = '';
  protected $inputsType = GoogleCloudAiplatformV1PipelineTaskDetailArtifactList::class;
  protected $inputsDataType = 'map';
  protected $outputsType = GoogleCloudAiplatformV1PipelineTaskDetailArtifactList::class;
  protected $outputsDataType = 'map';
  /**
   * @var string
   */
  public $parentTaskId;
  protected $pipelineTaskStatusType = GoogleCloudAiplatformV1PipelineTaskDetailPipelineTaskStatus::class;
  protected $pipelineTaskStatusDataType = 'array';
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $taskId;
  /**
   * @var string
   */
  public $taskName;

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param GoogleRpcStatus
   */
  public function setError(GoogleRpcStatus $error)
  {
    $this->error = $error;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param GoogleCloudAiplatformV1Execution
   */
  public function setExecution(GoogleCloudAiplatformV1Execution $execution)
  {
    $this->execution = $execution;
  }
  /**
   * @return GoogleCloudAiplatformV1Execution
   */
  public function getExecution()
  {
    return $this->execution;
  }
  /**
   * @param GoogleCloudAiplatformV1PipelineTaskExecutorDetail
   */
  public function setExecutorDetail(GoogleCloudAiplatformV1PipelineTaskExecutorDetail $executorDetail)
  {
    $this->executorDetail = $executorDetail;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineTaskExecutorDetail
   */
  public function getExecutorDetail()
  {
    return $this->executorDetail;
  }
  /**
   * @param GoogleCloudAiplatformV1PipelineTaskDetailArtifactList[]
   */
  public function setInputs($inputs)
  {
    $this->inputs = $inputs;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineTaskDetailArtifactList[]
   */
  public function getInputs()
  {
    return $this->inputs;
  }
  /**
   * @param GoogleCloudAiplatformV1PipelineTaskDetailArtifactList[]
   */
  public function setOutputs($outputs)
  {
    $this->outputs = $outputs;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineTaskDetailArtifactList[]
   */
  public function getOutputs()
  {
    return $this->outputs;
  }
  /**
   * @param string
   */
  public function setParentTaskId($parentTaskId)
  {
    $this->parentTaskId = $parentTaskId;
  }
  /**
   * @return string
   */
  public function getParentTaskId()
  {
    return $this->parentTaskId;
  }
  /**
   * @param GoogleCloudAiplatformV1PipelineTaskDetailPipelineTaskStatus[]
   */
  public function setPipelineTaskStatus($pipelineTaskStatus)
  {
    $this->pipelineTaskStatus = $pipelineTaskStatus;
  }
  /**
   * @return GoogleCloudAiplatformV1PipelineTaskDetailPipelineTaskStatus[]
   */
  public function getPipelineTaskStatus()
  {
    return $this->pipelineTaskStatus;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setTaskId($taskId)
  {
    $this->taskId = $taskId;
  }
  /**
   * @return string
   */
  public function getTaskId()
  {
    return $this->taskId;
  }
  /**
   * @param string
   */
  public function setTaskName($taskName)
  {
    $this->taskName = $taskName;
  }
  /**
   * @return string
   */
  public function getTaskName()
  {
    return $this->taskName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PipelineTaskDetail::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PipelineTaskDetail');
