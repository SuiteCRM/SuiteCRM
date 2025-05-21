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

class GoogleCloudAiplatformV1WriteTensorboardRunDataRequest extends \Google\Collection
{
  protected $collection_key = 'timeSeriesData';
  /**
   * @var string
   */
  public $tensorboardRun;
  protected $timeSeriesDataType = GoogleCloudAiplatformV1TimeSeriesData::class;
  protected $timeSeriesDataDataType = 'array';

  /**
   * @param string
   */
  public function setTensorboardRun($tensorboardRun)
  {
    $this->tensorboardRun = $tensorboardRun;
  }
  /**
   * @return string
   */
  public function getTensorboardRun()
  {
    return $this->tensorboardRun;
  }
  /**
   * @param GoogleCloudAiplatformV1TimeSeriesData[]
   */
  public function setTimeSeriesData($timeSeriesData)
  {
    $this->timeSeriesData = $timeSeriesData;
  }
  /**
   * @return GoogleCloudAiplatformV1TimeSeriesData[]
   */
  public function getTimeSeriesData()
  {
    return $this->timeSeriesData;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1WriteTensorboardRunDataRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1WriteTensorboardRunDataRequest');
