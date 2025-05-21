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

namespace Google\Service\AirQuality;

class LookupHistoryRequest extends \Google\Collection
{
  protected $collection_key = 'extraComputations';
  protected $customLocalAqisType = CustomLocalAqi::class;
  protected $customLocalAqisDataType = 'array';
  /**
   * @var string
   */
  public $dateTime;
  /**
   * @var string[]
   */
  public $extraComputations;
  /**
   * @var int
   */
  public $hours;
  /**
   * @var string
   */
  public $languageCode;
  protected $locationType = LatLng::class;
  protected $locationDataType = '';
  /**
   * @var int
   */
  public $pageSize;
  /**
   * @var string
   */
  public $pageToken;
  protected $periodType = Interval::class;
  protected $periodDataType = '';
  /**
   * @var string
   */
  public $uaqiColorPalette;
  /**
   * @var bool
   */
  public $universalAqi;

  /**
   * @param CustomLocalAqi[]
   */
  public function setCustomLocalAqis($customLocalAqis)
  {
    $this->customLocalAqis = $customLocalAqis;
  }
  /**
   * @return CustomLocalAqi[]
   */
  public function getCustomLocalAqis()
  {
    return $this->customLocalAqis;
  }
  /**
   * @param string
   */
  public function setDateTime($dateTime)
  {
    $this->dateTime = $dateTime;
  }
  /**
   * @return string
   */
  public function getDateTime()
  {
    return $this->dateTime;
  }
  /**
   * @param string[]
   */
  public function setExtraComputations($extraComputations)
  {
    $this->extraComputations = $extraComputations;
  }
  /**
   * @return string[]
   */
  public function getExtraComputations()
  {
    return $this->extraComputations;
  }
  /**
   * @param int
   */
  public function setHours($hours)
  {
    $this->hours = $hours;
  }
  /**
   * @return int
   */
  public function getHours()
  {
    return $this->hours;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param LatLng
   */
  public function setLocation(LatLng $location)
  {
    $this->location = $location;
  }
  /**
   * @return LatLng
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param int
   */
  public function setPageSize($pageSize)
  {
    $this->pageSize = $pageSize;
  }
  /**
   * @return int
   */
  public function getPageSize()
  {
    return $this->pageSize;
  }
  /**
   * @param string
   */
  public function setPageToken($pageToken)
  {
    $this->pageToken = $pageToken;
  }
  /**
   * @return string
   */
  public function getPageToken()
  {
    return $this->pageToken;
  }
  /**
   * @param Interval
   */
  public function setPeriod(Interval $period)
  {
    $this->period = $period;
  }
  /**
   * @return Interval
   */
  public function getPeriod()
  {
    return $this->period;
  }
  /**
   * @param string
   */
  public function setUaqiColorPalette($uaqiColorPalette)
  {
    $this->uaqiColorPalette = $uaqiColorPalette;
  }
  /**
   * @return string
   */
  public function getUaqiColorPalette()
  {
    return $this->uaqiColorPalette;
  }
  /**
   * @param bool
   */
  public function setUniversalAqi($universalAqi)
  {
    $this->universalAqi = $universalAqi;
  }
  /**
   * @return bool
   */
  public function getUniversalAqi()
  {
    return $this->universalAqi;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LookupHistoryRequest::class, 'Google_Service_AirQuality_LookupHistoryRequest');
