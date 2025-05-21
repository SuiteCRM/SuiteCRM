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

class HourlyForecast extends \Google\Collection
{
  protected $collection_key = 'pollutants';
  /**
   * @var string
   */
  public $dateTime;
  protected $healthRecommendationsType = HealthRecommendations::class;
  protected $healthRecommendationsDataType = '';
  protected $indexesType = AirQualityIndex::class;
  protected $indexesDataType = 'array';
  protected $pollutantsType = Pollutant::class;
  protected $pollutantsDataType = 'array';

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
   * @param HealthRecommendations
   */
  public function setHealthRecommendations(HealthRecommendations $healthRecommendations)
  {
    $this->healthRecommendations = $healthRecommendations;
  }
  /**
   * @return HealthRecommendations
   */
  public function getHealthRecommendations()
  {
    return $this->healthRecommendations;
  }
  /**
   * @param AirQualityIndex[]
   */
  public function setIndexes($indexes)
  {
    $this->indexes = $indexes;
  }
  /**
   * @return AirQualityIndex[]
   */
  public function getIndexes()
  {
    return $this->indexes;
  }
  /**
   * @param Pollutant[]
   */
  public function setPollutants($pollutants)
  {
    $this->pollutants = $pollutants;
  }
  /**
   * @return Pollutant[]
   */
  public function getPollutants()
  {
    return $this->pollutants;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HourlyForecast::class, 'Google_Service_AirQuality_HourlyForecast');
