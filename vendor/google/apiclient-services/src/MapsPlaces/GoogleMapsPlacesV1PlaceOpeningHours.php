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

class GoogleMapsPlacesV1PlaceOpeningHours extends \Google\Collection
{
  protected $collection_key = 'weekdayDescriptions';
  /**
   * @var string
   */
  public $nextCloseTime;
  /**
   * @var string
   */
  public $nextOpenTime;
  /**
   * @var bool
   */
  public $openNow;
  protected $periodsType = GoogleMapsPlacesV1PlaceOpeningHoursPeriod::class;
  protected $periodsDataType = 'array';
  /**
   * @var string
   */
  public $secondaryHoursType;
  protected $specialDaysType = GoogleMapsPlacesV1PlaceOpeningHoursSpecialDay::class;
  protected $specialDaysDataType = 'array';
  /**
   * @var string[]
   */
  public $weekdayDescriptions;

  /**
   * @param string
   */
  public function setNextCloseTime($nextCloseTime)
  {
    $this->nextCloseTime = $nextCloseTime;
  }
  /**
   * @return string
   */
  public function getNextCloseTime()
  {
    return $this->nextCloseTime;
  }
  /**
   * @param string
   */
  public function setNextOpenTime($nextOpenTime)
  {
    $this->nextOpenTime = $nextOpenTime;
  }
  /**
   * @return string
   */
  public function getNextOpenTime()
  {
    return $this->nextOpenTime;
  }
  /**
   * @param bool
   */
  public function setOpenNow($openNow)
  {
    $this->openNow = $openNow;
  }
  /**
   * @return bool
   */
  public function getOpenNow()
  {
    return $this->openNow;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceOpeningHoursPeriod[]
   */
  public function setPeriods($periods)
  {
    $this->periods = $periods;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceOpeningHoursPeriod[]
   */
  public function getPeriods()
  {
    return $this->periods;
  }
  /**
   * @param string
   */
  public function setSecondaryHoursType($secondaryHoursType)
  {
    $this->secondaryHoursType = $secondaryHoursType;
  }
  /**
   * @return string
   */
  public function getSecondaryHoursType()
  {
    return $this->secondaryHoursType;
  }
  /**
   * @param GoogleMapsPlacesV1PlaceOpeningHoursSpecialDay[]
   */
  public function setSpecialDays($specialDays)
  {
    $this->specialDays = $specialDays;
  }
  /**
   * @return GoogleMapsPlacesV1PlaceOpeningHoursSpecialDay[]
   */
  public function getSpecialDays()
  {
    return $this->specialDays;
  }
  /**
   * @param string[]
   */
  public function setWeekdayDescriptions($weekdayDescriptions)
  {
    $this->weekdayDescriptions = $weekdayDescriptions;
  }
  /**
   * @return string[]
   */
  public function getWeekdayDescriptions()
  {
    return $this->weekdayDescriptions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsPlacesV1PlaceOpeningHours::class, 'Google_Service_MapsPlaces_GoogleMapsPlacesV1PlaceOpeningHours');
