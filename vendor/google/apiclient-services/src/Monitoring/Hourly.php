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

namespace Google\Service\Monitoring;

class Hourly extends \Google\Model
{
  /**
   * @var int
   */
  public $minuteOffset;
  /**
   * @var int
   */
  public $periodicity;

  /**
   * @param int
   */
  public function setMinuteOffset($minuteOffset)
  {
    $this->minuteOffset = $minuteOffset;
  }
  /**
   * @return int
   */
  public function getMinuteOffset()
  {
    return $this->minuteOffset;
  }
  /**
   * @param int
   */
  public function setPeriodicity($periodicity)
  {
    $this->periodicity = $periodicity;
  }
  /**
   * @return int
   */
  public function getPeriodicity()
  {
    return $this->periodicity;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Hourly::class, 'Google_Service_Monitoring_Hourly');
