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

class JobsSummary extends \Google\Collection
{
  protected $collection_key = 'attempts';
  /**
   * @var int
   */
  public $activeJobs;
  /**
   * @var string
   */
  public $applicationId;
  protected $attemptsType = ApplicationAttemptInfo::class;
  protected $attemptsDataType = 'array';
  /**
   * @var int
   */
  public $completedJobs;
  /**
   * @var int
   */
  public $failedJobs;
  /**
   * @var string
   */
  public $schedulingMode;

  /**
   * @param int
   */
  public function setActiveJobs($activeJobs)
  {
    $this->activeJobs = $activeJobs;
  }
  /**
   * @return int
   */
  public function getActiveJobs()
  {
    return $this->activeJobs;
  }
  /**
   * @param string
   */
  public function setApplicationId($applicationId)
  {
    $this->applicationId = $applicationId;
  }
  /**
   * @return string
   */
  public function getApplicationId()
  {
    return $this->applicationId;
  }
  /**
   * @param ApplicationAttemptInfo[]
   */
  public function setAttempts($attempts)
  {
    $this->attempts = $attempts;
  }
  /**
   * @return ApplicationAttemptInfo[]
   */
  public function getAttempts()
  {
    return $this->attempts;
  }
  /**
   * @param int
   */
  public function setCompletedJobs($completedJobs)
  {
    $this->completedJobs = $completedJobs;
  }
  /**
   * @return int
   */
  public function getCompletedJobs()
  {
    return $this->completedJobs;
  }
  /**
   * @param int
   */
  public function setFailedJobs($failedJobs)
  {
    $this->failedJobs = $failedJobs;
  }
  /**
   * @return int
   */
  public function getFailedJobs()
  {
    return $this->failedJobs;
  }
  /**
   * @param string
   */
  public function setSchedulingMode($schedulingMode)
  {
    $this->schedulingMode = $schedulingMode;
  }
  /**
   * @return string
   */
  public function getSchedulingMode()
  {
    return $this->schedulingMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(JobsSummary::class, 'Google_Service_Dataproc_JobsSummary');
