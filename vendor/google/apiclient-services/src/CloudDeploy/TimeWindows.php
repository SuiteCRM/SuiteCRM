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

class TimeWindows extends \Google\Collection
{
  protected $collection_key = 'weeklyWindows';
  protected $oneTimeWindowsType = OneTimeWindow::class;
  protected $oneTimeWindowsDataType = 'array';
  /**
   * @var string
   */
  public $timeZone;
  protected $weeklyWindowsType = WeeklyWindow::class;
  protected $weeklyWindowsDataType = 'array';

  /**
   * @param OneTimeWindow[]
   */
  public function setOneTimeWindows($oneTimeWindows)
  {
    $this->oneTimeWindows = $oneTimeWindows;
  }
  /**
   * @return OneTimeWindow[]
   */
  public function getOneTimeWindows()
  {
    return $this->oneTimeWindows;
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
   * @param WeeklyWindow[]
   */
  public function setWeeklyWindows($weeklyWindows)
  {
    $this->weeklyWindows = $weeklyWindows;
  }
  /**
   * @return WeeklyWindow[]
   */
  public function getWeeklyWindows()
  {
    return $this->weeklyWindows;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TimeWindows::class, 'Google_Service_CloudDeploy_TimeWindows');
