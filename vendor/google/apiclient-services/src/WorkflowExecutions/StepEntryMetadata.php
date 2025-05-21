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

class StepEntryMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $expectedIteration;
  /**
   * @var string
   */
  public $progressNumber;
  /**
   * @var string
   */
  public $progressType;
  /**
   * @var string
   */
  public $threadId;

  /**
   * @param string
   */
  public function setExpectedIteration($expectedIteration)
  {
    $this->expectedIteration = $expectedIteration;
  }
  /**
   * @return string
   */
  public function getExpectedIteration()
  {
    return $this->expectedIteration;
  }
  /**
   * @param string
   */
  public function setProgressNumber($progressNumber)
  {
    $this->progressNumber = $progressNumber;
  }
  /**
   * @return string
   */
  public function getProgressNumber()
  {
    return $this->progressNumber;
  }
  /**
   * @param string
   */
  public function setProgressType($progressType)
  {
    $this->progressType = $progressType;
  }
  /**
   * @return string
   */
  public function getProgressType()
  {
    return $this->progressType;
  }
  /**
   * @param string
   */
  public function setThreadId($threadId)
  {
    $this->threadId = $threadId;
  }
  /**
   * @return string
   */
  public function getThreadId()
  {
    return $this->threadId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StepEntryMetadata::class, 'Google_Service_WorkflowExecutions_StepEntryMetadata');
