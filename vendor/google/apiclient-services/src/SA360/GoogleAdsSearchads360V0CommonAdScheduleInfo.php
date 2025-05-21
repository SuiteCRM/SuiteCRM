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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0CommonAdScheduleInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $dayOfWeek;
  /**
   * @var int
   */
  public $endHour;
  /**
   * @var string
   */
  public $endMinute;
  /**
   * @var int
   */
  public $startHour;
  /**
   * @var string
   */
  public $startMinute;

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
   * @param int
   */
  public function setEndHour($endHour)
  {
    $this->endHour = $endHour;
  }
  /**
   * @return int
   */
  public function getEndHour()
  {
    return $this->endHour;
  }
  /**
   * @param string
   */
  public function setEndMinute($endMinute)
  {
    $this->endMinute = $endMinute;
  }
  /**
   * @return string
   */
  public function getEndMinute()
  {
    return $this->endMinute;
  }
  /**
   * @param int
   */
  public function setStartHour($startHour)
  {
    $this->startHour = $startHour;
  }
  /**
   * @return int
   */
  public function getStartHour()
  {
    return $this->startHour;
  }
  /**
   * @param string
   */
  public function setStartMinute($startMinute)
  {
    $this->startMinute = $startMinute;
  }
  /**
   * @return string
   */
  public function getStartMinute()
  {
    return $this->startMinute;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonAdScheduleInfo::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonAdScheduleInfo');
