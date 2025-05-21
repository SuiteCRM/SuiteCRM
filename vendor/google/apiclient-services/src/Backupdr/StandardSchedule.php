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

namespace Google\Service\Backupdr;

class StandardSchedule extends \Google\Collection
{
  protected $collection_key = 'months';
  protected $backupWindowType = BackupWindow::class;
  protected $backupWindowDataType = '';
  /**
   * @var int[]
   */
  public $daysOfMonth;
  /**
   * @var string[]
   */
  public $daysOfWeek;
  /**
   * @var int
   */
  public $hourlyFrequency;
  /**
   * @var string[]
   */
  public $months;
  /**
   * @var string
   */
  public $recurrenceType;
  /**
   * @var string
   */
  public $timeZone;
  protected $weekDayOfMonthType = WeekDayOfMonth::class;
  protected $weekDayOfMonthDataType = '';

  /**
   * @param BackupWindow
   */
  public function setBackupWindow(BackupWindow $backupWindow)
  {
    $this->backupWindow = $backupWindow;
  }
  /**
   * @return BackupWindow
   */
  public function getBackupWindow()
  {
    return $this->backupWindow;
  }
  /**
   * @param int[]
   */
  public function setDaysOfMonth($daysOfMonth)
  {
    $this->daysOfMonth = $daysOfMonth;
  }
  /**
   * @return int[]
   */
  public function getDaysOfMonth()
  {
    return $this->daysOfMonth;
  }
  /**
   * @param string[]
   */
  public function setDaysOfWeek($daysOfWeek)
  {
    $this->daysOfWeek = $daysOfWeek;
  }
  /**
   * @return string[]
   */
  public function getDaysOfWeek()
  {
    return $this->daysOfWeek;
  }
  /**
   * @param int
   */
  public function setHourlyFrequency($hourlyFrequency)
  {
    $this->hourlyFrequency = $hourlyFrequency;
  }
  /**
   * @return int
   */
  public function getHourlyFrequency()
  {
    return $this->hourlyFrequency;
  }
  /**
   * @param string[]
   */
  public function setMonths($months)
  {
    $this->months = $months;
  }
  /**
   * @return string[]
   */
  public function getMonths()
  {
    return $this->months;
  }
  /**
   * @param string
   */
  public function setRecurrenceType($recurrenceType)
  {
    $this->recurrenceType = $recurrenceType;
  }
  /**
   * @return string
   */
  public function getRecurrenceType()
  {
    return $this->recurrenceType;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
  /**
   * @param WeekDayOfMonth
   */
  public function setWeekDayOfMonth(WeekDayOfMonth $weekDayOfMonth)
  {
    $this->weekDayOfMonth = $weekDayOfMonth;
  }
  /**
   * @return WeekDayOfMonth
   */
  public function getWeekDayOfMonth()
  {
    return $this->weekDayOfMonth;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StandardSchedule::class, 'Google_Service_Backupdr_StandardSchedule');
