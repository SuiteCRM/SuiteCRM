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

namespace Google\Service\Dataform;

class ScheduledExecutionRecord extends \Google\Model
{
  protected $errorStatusType = Status::class;
  protected $errorStatusDataType = '';
  /**
   * @var string
   */
  public $executionTime;
  /**
   * @var string
   */
  public $workflowInvocation;

  /**
   * @param Status
   */
  public function setErrorStatus(Status $errorStatus)
  {
    $this->errorStatus = $errorStatus;
  }
  /**
   * @return Status
   */
  public function getErrorStatus()
  {
    return $this->errorStatus;
  }
  /**
   * @param string
   */
  public function setExecutionTime($executionTime)
  {
    $this->executionTime = $executionTime;
  }
  /**
   * @return string
   */
  public function getExecutionTime()
  {
    return $this->executionTime;
  }
  /**
   * @param string
   */
  public function setWorkflowInvocation($workflowInvocation)
  {
    $this->workflowInvocation = $workflowInvocation;
  }
  /**
   * @return string
   */
  public function getWorkflowInvocation()
  {
    return $this->workflowInvocation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ScheduledExecutionRecord::class, 'Google_Service_Dataform_ScheduledExecutionRecord');
