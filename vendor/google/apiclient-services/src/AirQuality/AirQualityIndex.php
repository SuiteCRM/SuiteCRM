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

class AirQualityIndex extends \Google\Model
{
  /**
   * @var int
   */
  public $aqi;
  /**
   * @var string
   */
  public $aqiDisplay;
  /**
   * @var string
   */
  public $category;
  /**
   * @var string
   */
  public $code;
  protected $colorType = Color::class;
  protected $colorDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $dominantPollutant;

  /**
   * @param int
   */
  public function setAqi($aqi)
  {
    $this->aqi = $aqi;
  }
  /**
   * @return int
   */
  public function getAqi()
  {
    return $this->aqi;
  }
  /**
   * @param string
   */
  public function setAqiDisplay($aqiDisplay)
  {
    $this->aqiDisplay = $aqiDisplay;
  }
  /**
   * @return string
   */
  public function getAqiDisplay()
  {
    return $this->aqiDisplay;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param Color
   */
  public function setColor(Color $color)
  {
    $this->color = $color;
  }
  /**
   * @return Color
   */
  public function getColor()
  {
    return $this->color;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setDominantPollutant($dominantPollutant)
  {
    $this->dominantPollutant = $dominantPollutant;
  }
  /**
   * @return string
   */
  public function getDominantPollutant()
  {
    return $this->dominantPollutant;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AirQualityIndex::class, 'Google_Service_AirQuality_AirQualityIndex');
