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

namespace Google\Service\WorkflowExecutions;

class Execution extends \Google\Model
{
  /**
   * @var string
   */
  public $argument;
  /**
   * @var string
   */
  public $callLogLevel;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var bool
   */
  public $disableConcurrencyQuotaOverflowBuffering;
  /**
   * @var string
   */
  public $duration;
  /**
   * @var string
   */
  public $endTime;
  protected $errorType = Error::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $executionHistoryLevel;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $result;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $state;
  protected $stateErrorType = StateError::class;
  protected $stateErrorDataType = '';
  protected $statusType = Status::class;
  protected $statusDataType = '';
  /**
   * @var string
   */
  public $workflowRevisionId;

  /**
   * @param string
   */
  public function setArgument($argument)
  {
    $this->argument = $argument;
  }
  /**
   * @return string
   */
  public function getArgument()
  {
    return $this->argument;
  }
  /**
   * @param string
   */
  public function setCallLogLevel($callLogLevel)
  {
    $this->callLogLevel = $callLogLevel;
  }
  /**
   * @return string
   */
  public function getCallLogLevel()
  {
    return $this->callLogLevel;
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
   * @param bool
   */
  public function setDisableConcurrencyQuotaOverflowBuffering($disableConcurrencyQuotaOverflowBuffering)
  {
    $this->disableConcurrencyQuotaOverflowBuffering = $disableConcurrencyQuotaOverflowBuffering;
  }
  /**
   * @return bool
   */
  public function getDisableConcurrencyQuotaOverflowBuffering()
  {
    return $this->disableConcurrencyQuotaOverflowBuffering;
  }
  /**
   * @param string
   */
  public function setDuration($duration)
  {
    $this->duration = $duration;
  }
  /**
   * @return string
   */
  public function getDuration()
  {
    return $this->duration;
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
   * @param Error
   */
  public function setError(Error $error)
  {
    $this->error = $error;
  }
  /**
   * @return Error
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string
   */
  public function setExecutionHistoryLevel($executionHistoryLevel)
  {
    $this->executionHistoryLevel = $executionHistoryLevel;
  }
  /**
   * @return string
   */
  public function getExecutionHistoryLevel()
  {
    return $this->executionHistoryLevel;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setResult($result)
  {
    $this->result = $result;
  }
  /**
   * @return string
   */
  public function getResult()
  {
    return $this->result;
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
   * @param StateError
   */
  public function setStateError(StateError $stateError)
  {
    $this->stateError = $stateError;
  }
  /**
   * @return StateError
   */
  public function getStateError()
  {
    return $this->stateError;
  }
  /**
   * @param Status
   */
  public function setStatus(Status $status)
  {
    $this->status = $status;
  }
  /**
   * @return Status
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setWorkflowRevisionId($workflowRevisionId)
  {
    $this->workflowRevisionId = $workflowRevisionId;
  }
  /**
   * @return string
   */
  public function getWorkflowRevisionId()
  {
    return $this->workflowRevisionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Execution::class, 'Google_Service_WorkflowExecutions_Execution');
