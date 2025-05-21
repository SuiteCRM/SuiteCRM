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

class ScheduledReleaseRecord extends \Google\Model
{
  /**
   * @var string
   */
  public $compilationResult;
  protected $errorStatusType = Status::class;
  protected $errorStatusDataType = '';
  /**
   * @var string
   */
  public $releaseTime;

  /**
   * @param string
   */
  public function setCompilationResult($compilationResult)
  {
    $this->compilationResult = $compilationResult;
  }
  /**
   * @return string
   */
  public function getCompilationResult()
  {
    return $this->compilationResult;
  }
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
  public function setReleaseTime($releaseTime)
  {
    $this->releaseTime = $releaseTime;
  }
  /**
   * @return string
   */
  public function getReleaseTime()
  {
    return $this->releaseTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ScheduledReleaseRecord::class, 'Google_Service_Dataform_ScheduledReleaseRecord');
