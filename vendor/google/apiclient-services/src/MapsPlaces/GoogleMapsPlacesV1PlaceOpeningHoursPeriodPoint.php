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

namespace Google\Service\MapsPlaces;

class GoogleMapsPlacesV1PlaceOpeningHoursPeriodPoint extends \Google\Model
{
  protected $dateType = GoogleTypeDate::class;
  protected $dateDataType = '';
  /**
   * @var int
   */
  public $day;
  /**
   * @var int
   */
  public $hour;
  /**
   * @var int
   */
  public $minute;
  /**
   * @var bool
   */
  public $truncated;

  /**
   * @param GoogleTypeDate
   */
  public function setDate(GoogleTypeDate $date)
  {
    $this->date = $date;
  }
  /**
   * @return GoogleTypeDate
   */
  public function getDate()
  {
    return $this->date;
  }
  /**
   * @param int
   */
  public function setDay($day)
  {
    $this->day = $day;
  }
  /**
   * @return int
   */
  public function getDay()
  {
    return $this->day;
  }
  /**
   * @param int
   */
  public function setHour($hour)
  {
    $this->hour = $hour;
  }
  /**
   * @return int
   */
  public function getHour()
  {
    return $this->hour;
  }
  /**
   * @param int
   */
  public function setMinute($minute)
  {
    $this->minute = $minute;
  }
  /**
   * @return int
   */
  public function getMinute()
  {
    return $this->minute;
  }
  /**
   * @param bool
   */
  public function setTruncated($truncated)
  {
    $this->truncated = $truncated;
  }
  /**
   * @return bool
   */
  public function getTruncated()
  {
    return $this->truncated;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1PlaceOpeningHoursPeriodPoint::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1PlaceOpeningHoursPeriodPoint');
