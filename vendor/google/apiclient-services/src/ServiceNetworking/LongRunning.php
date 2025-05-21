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

namespace Google\Service\ServiceNetworking;

class LongRunning extends \Google\Model
{
  /**
   * @var string
   */
  public $initialPollDelay;
  /**
   * @var string
   */
  public $maxPollDelay;
  /**
   * @var float
   */
  public $pollDelayMultiplier;
  /**
   * @var string
   */
  public $totalPollTimeout;

  /**
   * @param string
   */
  public function setInitialPollDelay($initialPollDelay)
  {
    $this->initialPollDelay = $initialPollDelay;
  }
  /**
   * @return string
   */
  public function getInitialPollDelay()
  {
    return $this->initialPollDelay;
  }
  /**
   * @param string
   */
  public function setMaxPollDelay($maxPollDelay)
  {
    $this->maxPollDelay = $maxPollDelay;
  }
  /**
   * @return string
   */
  public function getMaxPollDelay()
  {
    return $this->maxPollDelay;
  }
  /**
   * @param float
   */
  public function setPollDelayMultiplier($pollDelayMultiplier)
  {
    $this->pollDelayMultiplier = $pollDelayMultiplier;
  }
  /**
   * @return float
   */
  public function getPollDelayMultiplier()
  {
    return $this->pollDelayMultiplier;
  }
  /**
   * @param string
   */
  public function setTotalPollTimeout($totalPollTimeout)
  {
    $this->totalPollTimeout = $totalPollTimeout;
  }
  /**
   * @return string
   */
  public function getTotalPollTimeout()
  {
    return $this->totalPollTimeout;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LongRunning::class, 'Google_Service_ServiceNetworking_LongRunning');
