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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1ExportTensorboardTimeSeriesDataResponse extends \Google\Collection
{
  protected $collection_key = 'timeSeriesDataPoints';
  /**
   * @var string
   */
  public $nextPageToken;
  protected $timeSeriesDataPointsType = GoogleCloudAiplatformV1TimeSeriesDataPoint::class;
  protected $timeSeriesDataPointsDataType = 'array';

  /**
   * @param string
   */
  public function setNextPageToken($nextPageToken)
  {
    $this->nextPageToken = $nextPageToken;
  }
  /**
   * @return string
   */
  public function getNextPageToken()
  {
    return $this->nextPageToken;
  }
  /**
   * @param GoogleCloudAiplatformV1TimeSeriesDataPoint[]
   */
  public function setTimeSeriesDataPoints($timeSeriesDataPoints)
  {
    $this->timeSeriesDataPoints = $timeSeriesDataPoints;
  }
  /**
   * @return GoogleCloudAiplatformV1TimeSeriesDataPoint[]
   */
  public function getTimeSeriesDataPoints()
  {
    return $this->timeSeriesDataPoints;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ExportTensorboardTimeSeriesDataResponse::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ExportTensorboardTimeSeriesDataResponse');
