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

class PromoteReleaseOperation extends \Google\Model
{
  /**
   * @var string
   */
  public $phase;
  /**
   * @var string
   */
  public $rollout;
  /**
   * @var string
   */
  public $targetId;
  /**
   * @var string
   */
  public $wait;

  /**
   * @param string
   */
  public function setPhase($phase)
  {
    $this->phase = $phase;
  }
  /**
   * @return string
   */
  public function getPhase()
  {
    return $this->phase;
  }
  /**
   * @param string
   */
  public function setRollout($rollout)
  {
    $this->rollout = $rollout;
  }
  /**
   * @return string
   */
  public function getRollout()
  {
    return $this->rollout;
  }
  /**
   * @param string
   */
  public function setTargetId($targetId)
  {
    $this->targetId = $targetId;
  }
  /**
   * @return string
   */
  public function getTargetId()
  {
    return $this->targetId;
  }
  /**
   * @param string
   */
  public function setWait($wait)
  {
    $this->wait = $wait;
  }
  /**
   * @return string
   */
  public function getWait()
  {
    return $this->wait;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PromoteReleaseOperation::class, 'Google_Service_CloudDeploy_PromoteReleaseOperation');
