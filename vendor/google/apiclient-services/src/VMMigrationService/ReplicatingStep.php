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

namespace Google\Service\VMMigrationService;

class ReplicatingStep extends \Google\Model
{
  /**
   * @var string
   */
  public $lastThirtyMinutesAverageBytesPerSecond;
  /**
   * @var string
   */
  public $lastTwoMinutesAverageBytesPerSecond;
  /**
   * @var string
   */
  public $replicatedBytes;
  /**
   * @var string
   */
  public $totalBytes;

  /**
   * @param string
   */
  public function setLastThirtyMinutesAverageBytesPerSecond($lastThirtyMinutesAverageBytesPerSecond)
  {
    $this->lastThirtyMinutesAverageBytesPerSecond = $lastThirtyMinutesAverageBytesPerSecond;
  }
  /**
   * @return string
   */
  public function getLastThirtyMinutesAverageBytesPerSecond()
  {
    return $this->lastThirtyMinutesAverageBytesPerSecond;
  }
  /**
   * @param string
   */
  public function setLastTwoMinutesAverageBytesPerSecond($lastTwoMinutesAverageBytesPerSecond)
  {
    $this->lastTwoMinutesAverageBytesPerSecond = $lastTwoMinutesAverageBytesPerSecond;
  }
  /**
   * @return string
   */
  public function getLastTwoMinutesAverageBytesPerSecond()
  {
    return $this->lastTwoMinutesAverageBytesPerSecond;
  }
  /**
   * @param string
   */
  public function setReplicatedBytes($replicatedBytes)
  {
    $this->replicatedBytes = $replicatedBytes;
  }
  /**
   * @return string
   */
  public function getReplicatedBytes()
  {
    return $this->replicatedBytes;
  }
  /**
   * @param string
   */
  public function setTotalBytes($totalBytes)
  {
    $this->totalBytes = $totalBytes;
  }
  /**
   * @return string
   */
  public function getTotalBytes()
  {
    return $this->totalBytes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReplicatingStep::class, 'Google_Service_VMMigrationService_ReplicatingStep');
