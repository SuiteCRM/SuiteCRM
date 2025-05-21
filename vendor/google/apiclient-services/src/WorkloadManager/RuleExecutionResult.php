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

namespace Google\Service\WorkloadManager;

class RuleExecutionResult extends \Google\Model
{
  /**
   * @var string
   */
  public $message;
  /**
   * @var string
   */
  public $resultCount;
  /**
   * @var string
   */
  public $rule;
  /**
   * @var string
   */
  public $scannedResourceCount;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setMessage($message)
  {
    $this->message = $message;
  }
  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }
  /**
   * @param string
   */
  public function setResultCount($resultCount)
  {
    $this->resultCount = $resultCount;
  }
  /**
   * @return string
   */
  public function getResultCount()
  {
    return $this->resultCount;
  }
  /**
   * @param string
   */
  public function setRule($rule)
  {
    $this->rule = $rule;
  }
  /**
   * @return string
   */
  public function getRule()
  {
    return $this->rule;
  }
  /**
   * @param string
   */
  public function setScannedResourceCount($scannedResourceCount)
  {
    $this->scannedResourceCount = $scannedResourceCount;
  }
  /**
   * @return string
   */
  public function getScannedResourceCount()
  {
    return $this->scannedResourceCount;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(RuleExecutionResult::class, 'Google_Service_WorkloadManager_RuleExecutionResult');
