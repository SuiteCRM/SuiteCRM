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

class GoogleCloudAiplatformV1Schedule extends \Google\Model
{
  /**
   * @var bool
   */
  public $allowQueueing;
  /**
   * @var bool
   */
  public $catchUp;
  protected $createNotebookExecutionJobRequestType = GoogleCloudAiplatformV1CreateNotebookExecutionJobRequest::class;
  protected $createNotebookExecutionJobRequestDataType = '';
  protected $createPipelineJobRequestType = GoogleCloudAiplatformV1CreatePipelineJobRequest::class;
  protected $createPipelineJobRequestDataType = '';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $cron;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $lastPauseTime;
  /**
   * @var string
   */
  public $lastResumeTime;
  protected $lastScheduledRunResponseType = GoogleCloudAiplatformV1ScheduleRunResponse::class;
  protected $lastScheduledRunResponseDataType = '';
  /**
   * @var string
   */
  public $maxConcurrentRunCount;
  /**
   * @var string
   */
  public $maxRunCount;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nextRunTime;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $startedRunCount;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param bool
   */
  public function setAllowQueueing($allowQueueing)
  {
    $this->allowQueueing = $allowQueueing;
  }
  /**
   * @return bool
   */
  public function getAllowQueueing()
  {
    return $this->allowQueueing;
  }
  /**
   * @param bool
   */
  public function setCatchUp($catchUp)
  {
    $this->catchUp = $catchUp;
  }
  /**
   * @return bool
   */
  public function getCatchUp()
  {
    return $this->catchUp;
  }
  /**
   * @param GoogleCloudAiplatformV1CreateNotebookExecutionJobRequest
   */
  public function setCreateNotebookExecutionJobRequest(GoogleCloudAiplatformV1CreateNotebookExecutionJobRequest $createNotebookExecutionJobRequest)
  {
    $this->createNotebookExecutionJobRequest = $createNotebookExecutionJobRequest;
  }
  /**
   * @return GoogleCloudAiplatformV1CreateNotebookExecutionJobRequest
   */
  public function getCreateNotebookExecutionJobRequest()
  {
    return $this->createNotebookExecutionJobRequest;
  }
  /**
   * @param GoogleCloudAiplatformV1CreatePipelineJobRequest
   */
  public function setCreatePipelineJobRequest(GoogleCloudAiplatformV1CreatePipelineJobRequest $createPipelineJobRequest)
  {
    $this->createPipelineJobRequest = $createPipelineJobRequest;
  }
  /**
   * @return GoogleCloudAiplatformV1CreatePipelineJobRequest
   */
  public function getCreatePipelineJobRequest()
  {
    return $this->createPipelineJobRequest;
  }
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
  public function setCron($cron)
  {
    $this->cron = $cron;
  }
  /**
   * @return string
   */
  public function getCron()
  {
    return $this->cron;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
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
   * @param string
   */
  public function setLastPauseTime($lastPauseTime)
  {
    $this->lastPauseTime = $lastPauseTime;
  }
  /**
   * @return string
   */
  public function getLastPauseTime()
  {
    return $this->lastPauseTime;
  }
  /**
   * @param string
   */
  public function setLastResumeTime($lastResumeTime)
  {
    $this->lastResumeTime = $lastResumeTime;
  }
  /**
   * @return string
   */
  public function getLastResumeTime()
  {
    return $this->lastResumeTime;
  }
  /**
   * @param GoogleCloudAiplatformV1ScheduleRunResponse
   */
  public function setLastScheduledRunResponse(GoogleCloudAiplatformV1ScheduleRunResponse $lastScheduledRunResponse)
  {
    $this->lastScheduledRunResponse = $lastScheduledRunResponse;
  }
  /**
   * @return GoogleCloudAiplatformV1ScheduleRunResponse
   */
  public function getLastScheduledRunResponse()
  {
    return $this->lastScheduledRunResponse;
  }
  /**
   * @param string
   */
  public function setMaxConcurrentRunCount($maxConcurrentRunCount)
  {
    $this->maxConcurrentRunCount = $maxConcurrentRunCount;
  }
  /**
   * @return string
   */
  public function getMaxConcurrentRunCount()
  {
    return $this->maxConcurrentRunCount;
  }
  /**
   * @param string
   */
  public function setMaxRunCount($maxRunCount)
  {
    $this->maxRunCount = $maxRunCount;
  }
  /**
   * @return string
   */
  public function getMaxRunCount()
  {
    return $this->maxRunCount;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setNextRunTime($nextRunTime)
  {
    $this->nextRunTime = $nextRunTime;
  }
  /**
   * @return string
   */
  public function getNextRunTime()
  {
    return $this->nextRunTime;
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
  public function setStartedRunCount($startedRunCount)
  {
    $this->startedRunCount = $startedRunCount;
  }
  /**
   * @return string
   */
  public function getStartedRunCount()
  {
    return $this->startedRunCount;
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
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Schedule::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Schedule');
