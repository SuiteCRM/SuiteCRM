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

namespace Google\Service\DisplayVideo;

class TargetFrequency extends \Google\Model
{
  /**
   * @var string
   */
  public $targetCount;
  /**
   * @var string
   */
  public $timeUnit;
  /**
   * @var int
   */
  public $timeUnitCount;

  /**
   * @param string
   */
  public function setTargetCount($targetCount)
  {
    $this->targetCount = $targetCount;
  }
  /**
   * @return string
   */
  public function getTargetCount()
  {
    return $this->targetCount;
  }
  /**
   * @param string
   */
  public function setTimeUnit($timeUnit)
  {
    $this->timeUnit = $timeUnit;
  }
  /**
   * @return string
   */
  public function getTimeUnit()
  {
    return $this->timeUnit;
  }
  /**
   * @param int
   */
  public function setTimeUnitCount($timeUnitCount)
  {
    $this->timeUnitCount = $timeUnitCount;
  }
  /**
   * @return int
   */
  public function getTimeUnitCount()
  {
    return $this->timeUnitCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TargetFrequency::class, 'Google_Service_DisplayVideo_TargetFrequency');
