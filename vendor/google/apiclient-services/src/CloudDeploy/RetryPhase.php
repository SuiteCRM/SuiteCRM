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

namespace Google\Service\CloudDeploy;

class RetryPhase extends \Google\Collection
{
  protected $collection_key = 'attempts';
  protected $attemptsType = RetryAttempt::class;
  protected $attemptsDataType = 'array';
  /**
   * @var string
   */
  public $backoffMode;
  /**
   * @var string
   */
  public $totalAttempts;

  /**
   * @param RetryAttempt[]
   */
  public function setAttempts($attempts)
  {
    $this->attempts = $attempts;
  }
  /**
   * @return RetryAttempt[]
   */
  public function getAttempts()
  {
    return $this->attempts;
  }
  /**
   * @param string
   */
  public function setBackoffMode($backoffMode)
  {
    $this->backoffMode = $backoffMode;
  }
  /**
   * @return string
   */
  public function getBackoffMode()
  {
    return $this->backoffMode;
  }
  /**
   * @param string
   */
  public function setTotalAttempts($totalAttempts)
  {
    $this->totalAttempts = $totalAttempts;
  }
  /**
   * @return string
   */
  public function getTotalAttempts()
  {
    return $this->totalAttempts;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RetryPhase::class, 'Google_Service_CloudDeploy_RetryPhase');
