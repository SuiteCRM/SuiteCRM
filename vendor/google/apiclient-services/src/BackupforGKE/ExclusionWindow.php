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

namespace Google\Service\BackupforGKE;

class ExclusionWindow extends \Google\Model
{
  /**
   * @var bool
   */
  public $daily;
  protected $daysOfWeekType = DayOfWeekList::class;
  protected $daysOfWeekDataType = '';
  /**
   * @var string
   */
  public $duration;
  protected $singleOccurrenceDateType = Date::class;
  protected $singleOccurrenceDateDataType = '';
  protected $startTimeType = TimeOfDay::class;
  protected $startTimeDataType = '';

  /**
   * @param bool
   */
  public function setDaily($daily)
  {
    $this->daily = $daily;
  }
  /**
   * @return bool
   */
  public function getDaily()
  {
    return $this->daily;
  }
  /**
   * @param DayOfWeekList
   */
  public function setDaysOfWeek(DayOfWeekList $daysOfWeek)
  {
    $this->daysOfWeek = $daysOfWeek;
  }
  /**
   * @return DayOfWeekList
   */
  public function getDaysOfWeek()
  {
    return $this->daysOfWeek;
  }
  /**
   * @param string
   */
  public function setDuration($duration)
  {
    $this->duration = $duration;
  }
  /**
   * @return string
   */
  public function getDuration()
  {
    return $this->duration;
  }
  /**
   * @param Date
   */
  public function setSingleOccurrenceDate(Date $singleOccurrenceDate)
  {
    $this->singleOccurrenceDate = $singleOccurrenceDate;
  }
  /**
   * @return Date
   */
  public function getSingleOccurrenceDate()
  {
    return $this->singleOccurrenceDate;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExclusionWindow::class, 'Google_Service_BackupforGKE_ExclusionWindow');
