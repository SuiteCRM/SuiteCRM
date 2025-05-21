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

namespace Google\Service\OracleDatabase;

class ScheduledOperationDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $dayOfWeek;
  protected $startTimeType = TimeOfDay::class;
  protected $startTimeDataType = '';
  protected $stopTimeType = TimeOfDay::class;
  protected $stopTimeDataType = '';

  /**
   * @param string
   */
  public function setDayOfWeek($dayOfWeek)
  {
    $this->dayOfWeek = $dayOfWeek;
  }
  /**
   * @return string
   */
  public function getDayOfWeek()
  {
    return $this->dayOfWeek;
  }
  /**
   * @param TimeOfDay
   */
  public function setStartTime(TimeOfDay $startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return TimeOfDay
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param TimeOfDay
   */
  public function setStopTime(TimeOfDay $stopTime)
  {
    $this->stopTime = $stopTime;
  }
  /**
   * @return TimeOfDay
   */
  public function getStopTime()
  {
    return $this->stopTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ScheduledOperationDetails::class, 'Google_Service_OracleDatabase_ScheduledOperationDetails');
