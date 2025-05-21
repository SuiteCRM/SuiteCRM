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

namespace Google\Service\BusinessProfilePerformance;

class DailyMetricTimeSeries extends \Google\Model
{
  /**
   * @var string
   */
  public $dailyMetric;
  protected $dailySubEntityTypeType = DailySubEntityType::class;
  protected $dailySubEntityTypeDataType = '';
  protected $timeSeriesType = TimeSeries::class;
  protected $timeSeriesDataType = '';

  /**
   * @param string
   */
  public function setDailyMetric($dailyMetric)
  {
    $this->dailyMetric = $dailyMetric;
  }
  /**
   * @return string
   */
  public function getDailyMetric()
  {
    return $this->dailyMetric;
  }
  /**
   * @param DailySubEntityType
   */
  public function setDailySubEntityType(DailySubEntityType $dailySubEntityType)
  {
    $this->dailySubEntityType = $dailySubEntityType;
  }
  /**
   * @return DailySubEntityType
   */
  public function getDailySubEntityType()
  {
    return $this->dailySubEntityType;
  }
  /**
   * @param TimeSeries
   */
  public function setTimeSeries(TimeSeries $timeSeries)
  {
    $this->timeSeries = $timeSeries;
  }
  /**
   * @return TimeSeries
   */
  public function getTimeSeries()
  {
    return $this->timeSeries;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DailyMetricTimeSeries::class, 'Google_Service_BusinessProfilePerformance_DailyMetricTimeSeries');
