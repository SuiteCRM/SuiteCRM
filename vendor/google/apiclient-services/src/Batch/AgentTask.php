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

namespace Google\Service\Batch;

class AgentTask extends \Google\Model
{
  protected $agentTaskSpecType = AgentTaskSpec::class;
  protected $agentTaskSpecDataType = '';
  /**
   * @var string
   */
  public $intendedState;
  /**
   * @var string
   */
  public $reachedBarrier;
  protected $specType = TaskSpec::class;
  protected $specDataType = '';
  protected $statusType = TaskStatus::class;
  protected $statusDataType = '';
  /**
   * @var string
   */
  public $task;
  /**
   * @var string
   */
  public $taskSource;

  /**
   * @param AgentTaskSpec
   */
  public function setAgentTaskSpec(AgentTaskSpec $agentTaskSpec)
  {
    $this->agentTaskSpec = $agentTaskSpec;
  }
  /**
   * @return AgentTaskSpec
   */
  public function getAgentTaskSpec()
  {
    return $this->agentTaskSpec;
  }
  /**
   * @param string
   */
  public function setIntendedState($intendedState)
  {
    $this->intendedState = $intendedState;
  }
  /**
   * @return string
   */
  public function getIntendedState()
  {
    return $this->intendedState;
  }
  /**
   * @param string
   */
  public function setReachedBarrier($reachedBarrier)
  {
    $this->reachedBarrier = $reachedBarrier;
  }
  /**
   * @return string
   */
  public function getReachedBarrier()
  {
    return $this->reachedBarrier;
  }
  /**
   * @param TaskSpec
   */
  public function setSpec(TaskSpec $spec)
  {
    $this->spec = $spec;
  }
  /**
   * @return TaskSpec
   */
  public function getSpec()
  {
    return $this->spec;
  }
  /**
   * @param TaskStatus
   */
  public function setStatus(TaskStatus $status)
  {
    $this->status = $status;
  }
  /**
   * @return TaskStatus
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setTask($task)
  {
    $this->task = $task;
  }
  /**
   * @return string
   */
  public function getTask()
  {
    return $this->task;
  }
  /**
   * @param string
   */
  public function setTaskSource($taskSource)
  {
    $this->taskSource = $taskSource;
  }
  /**
   * @return string
   */
  public function getTaskSource()
  {
    return $this->taskSource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AgentTask::class, 'Google_Service_Batch_AgentTask');
