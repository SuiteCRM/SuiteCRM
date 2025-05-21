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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaObtainCrawlRateResponse extends \Google\Model
{
  protected $dedicatedCrawlRateTimeSeriesType = GoogleCloudDiscoveryengineV1alphaDedicatedCrawlRateTimeSeries::class;
  protected $dedicatedCrawlRateTimeSeriesDataType = '';
  protected $errorType = GoogleRpcStatus::class;
  protected $errorDataType = '';
  protected $organicCrawlRateTimeSeriesType = GoogleCloudDiscoveryengineV1alphaOrganicCrawlRateTimeSeries::class;
  protected $organicCrawlRateTimeSeriesDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param GoogleCloudDiscoveryengineV1alphaDedicatedCrawlRateTimeSeries
   */
  public function setDedicatedCrawlRateTimeSeries(GoogleCloudDiscoveryengineV1alphaDedicatedCrawlRateTimeSeries $dedicatedCrawlRateTimeSeries)
  {
    $this->dedicatedCrawlRateTimeSeries = $dedicatedCrawlRateTimeSeries;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaDedicatedCrawlRateTimeSeries
   */
  public function getDedicatedCrawlRateTimeSeries()
  {
    return $this->dedicatedCrawlRateTimeSeries;
  }
  /**
   * @param GoogleRpcStatus
   */
  public function setError(GoogleRpcStatus $error)
  {
    $this->error = $error;
  }
  /**
   * @return GoogleRpcStatus
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaOrganicCrawlRateTimeSeries
   */
  public function setOrganicCrawlRateTimeSeries(GoogleCloudDiscoveryengineV1alphaOrganicCrawlRateTimeSeries $organicCrawlRateTimeSeries)
  {
    $this->organicCrawlRateTimeSeries = $organicCrawlRateTimeSeries;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaOrganicCrawlRateTimeSeries
   */
  public function getOrganicCrawlRateTimeSeries()
  {
    return $this->organicCrawlRateTimeSeries;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaObtainCrawlRateResponse::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaObtainCrawlRateResponse');
