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

class StatusEvent extends \Google\Model
{
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $eventTime;
  protected $taskExecutionType = TaskExecution::class;
  protected $taskExecutionDataType = '';
  /**
   * @var string
   */
  public $taskState;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setEventTime($eventTime)
  {
    $this->eventTime = $eventTime;
  }
  /**
   * @return string
   */
  public function getEventTime()
  {
    return $this->eventTime;
  }
  /**
   * @param TaskExecution
   */
  public function setTaskExecution(TaskExecution $taskExecution)
  {
    $this->taskExecution = $taskExecution;
  }
  /**
   * @return TaskExecution
   */
  public function getTaskExecution()
  {
    return $this->taskExecution;
  }
  /**
   * @param string
   */
  public function setTaskState($taskState)
  {
    $this->taskState = $taskState;
  }
  /**
   * @return string
   */
  public function getTaskState()
  {
    return $this->taskState;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StatusEvent::class, 'Google_Service_Batch_StatusEvent');
