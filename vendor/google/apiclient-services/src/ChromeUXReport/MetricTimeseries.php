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

namespace Google\Service\ChromeUXReport;

class MetricTimeseries extends \Google\Collection
{
  protected $collection_key = 'histogramTimeseries';
  protected $fractionTimeseriesType = FractionTimeseries::class;
  protected $fractionTimeseriesDataType = 'map';
  protected $histogramTimeseriesType = TimeseriesBin::class;
  protected $histogramTimeseriesDataType = 'array';
  protected $percentilesTimeseriesType = TimeseriesPercentiles::class;
  protected $percentilesTimeseriesDataType = '';

  /**
   * @param FractionTimeseries[]
   */
  public function setFractionTimeseries($fractionTimeseries)
  {
    $this->fractionTimeseries = $fractionTimeseries;
  }
  /**
   * @return FractionTimeseries[]
   */
  public function getFractionTimeseries()
  {
    return $this->fractionTimeseries;
  }
  /**
   * @param TimeseriesBin[]
   */
  public function setHistogramTimeseries($histogramTimeseries)
  {
    $this->histogramTimeseries = $histogramTimeseries;
  }
  /**
   * @return TimeseriesBin[]
   */
  public function getHistogramTimeseries()
  {
    return $this->histogramTimeseries;
  }
  /**
   * @param TimeseriesPercentiles
   */
  public function setPercentilesTimeseries(TimeseriesPercentiles $percentilesTimeseries)
  {
    $this->percentilesTimeseries = $percentilesTimeseries;
  }
  /**
   * @return TimeseriesPercentiles
   */
  public function getPercentilesTimeseries()
  {
    return $this->percentilesTimeseries;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MetricTimeseries::class, 'Google_Service_ChromeUXReport_MetricTimeseries');
